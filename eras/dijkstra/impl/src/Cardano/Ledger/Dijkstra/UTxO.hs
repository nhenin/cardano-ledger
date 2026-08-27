{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.UTxO (
  DijkstraEraUTxO (..),
  dijkstraConsumed,
  getConsumedAssets,
  getConsumedCapacityDeposits,
  getProducedAssets,
  getProducedCapacityDeposits,
  getDijkstraScriptsNeeded,
  getDijkstraScriptsProvided,
  scriptsProvidedDijkstraStAnnTx,
  batchNonDistinctRefScriptsSize,
) where

import Cardano.Ledger.Alonzo.Plutus.Context (CollectError)
import Cardano.Ledger.Alonzo.UTxO (
  AlonzoEraUTxO (..),
  AlonzoScriptsNeeded (..),
  getAlonzoScriptsHashesNeeded,
  zipAsIxItem,
 )
import Cardano.Ledger.Babbage.UTxO (
  getBabbageScriptsProvided,
  getBabbageSpendingDatum,
  getBabbageSupplementalDataHashes,
 )
import Cardano.Ledger.BaseTypes (inject)
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Conway.TxBody (conwayProposalsDeposits)
import Cardano.Ledger.Conway.UTxO (
  getConwayMinFeeTxUtxo,
  getConwayScriptsNeeded,
  getConwayWitsVKeyNeeded,
  txNonDistinctRefScriptsSize,
 )
import Cardano.Ledger.Credential (Credential, credScriptHash)
import Cardano.Ledger.Dijkstra.Assets (Assets (..))
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Dijkstra.Scripts (DijkstraEraScript (..), pattern GuardingPurpose)
import Cardano.Ledger.Dijkstra.State
import Cardano.Ledger.Dijkstra.Tx (DijkstraStAnnTx (..))
import Cardano.Ledger.Dijkstra.TxOut (DijkstraEraTxOut (..))
import Cardano.Ledger.Dijkstra.TxOut.CapacityDeposit (CapacityDeposit (..))
import Cardano.Ledger.Mary.UTxO (burnedMultiAssets)
import Cardano.Ledger.Mary.Value (MaryValue (..), filterMultiAsset)
import Cardano.Ledger.Plutus (Language, PlutusWithContext)
import Data.Foldable (Foldable (..))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Monoid (Sum (..))
import qualified Data.OMap.Strict as OMap
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import Lens.Micro (SimpleGetter, to, (^.))
import Lens.Micro.Extras (view)

class AlonzoEraUTxO era => DijkstraEraUTxO era where
  subTransactionsStAnnTx :: StAnnTx TopTx era -> [StAnnTx SubTx era]
  plutusLegacyModeStAnnTxG :: SimpleGetter (StAnnTx TopTx era) Bool
  scriptsHashesNeededStAnnTx :: StAnnTx SubTx era -> Set ScriptHash

-- | Unlike `shelleyConsumed`, this function does not need access to `Accounts` to produce accurate
-- information about refunds, hence is this simplification. Note that using `shelleyConsumed` in
-- Dijkstra era onwards will produce the same result as this one.
dijkstraConsumed ::
  EraUTxO era =>
  PParams era ->
  UTxO era ->
  TxBody l era ->
  Value era
dijkstraConsumed pp = getConsumedValue pp (const Nothing)

-- | The application side of what a transaction consumes: the assets of the
-- inputs it spends, plus refunds, withdrawals and positively minted
-- quantities. Capacity deposits are deliberately absent — they are
-- operational funding, accounted by 'getConsumedCapacityDeposits'.
getConsumedAssets ::
  forall era l.
  ( DijkstraEraTxBody era
  , EraUTxO era
  , Value era ~ Assets
  , STxLevel l era ~ STxBothLevels l era
  ) =>
  PParams era ->
  (Credential Staking -> Maybe Coin) ->
  UTxO era ->
  TxBody l era ->
  Assets
getConsumedAssets pp lookupStakingDeposit utxo txBody =
  withBothTxLevels
    txBody
    ( \topTxBody ->
        txBodyConsumedAssets pp lookupStakingDeposit utxo topTxBody
          <> foldMap'
            (txBodyConsumedAssets pp lookupStakingDeposit utxo . view bodyTxL)
            (topTxBody ^. subTransactionsTxBodyL)
    )
    (txBodyConsumedAssets pp lookupStakingDeposit utxo)

-- | The application side one body consumes. The Mary accounting, inlined
-- because 'getConsumedMaryValue' requires 'Value era ~ MaryValue'.
txBodyConsumedAssets ::
  ( DijkstraEraTxBody era
  , EraUTxO era
  , Value era ~ Assets
  ) =>
  PParams era ->
  (Credential Staking -> Maybe Coin) ->
  UTxO era ->
  TxBody l era ->
  Assets
txBodyConsumedAssets pp lookupStakingDeposit utxo txBody =
  sumUTxO (txInsFilter utxo (txBody ^. inputsTxBodyL))
    <> inject
      ( getTotalRefundsTxBody pp lookupStakingDeposit txBody
          <> fold (unWithdrawals (txBody ^. withdrawalsTxBodyL))
      )
    <> Assets (MaryValue mempty (filterMultiAsset (\_ _ -> (> 0)) (txBody ^. mintTxBodyL)))

-- | The operational side of what a transaction consumes: the capacity
-- deposits released by every input the batch spends — own inputs and each
-- sub-transaction's.
getConsumedCapacityDeposits ::
  forall era l.
  ( DijkstraEraTxOut era
  , DijkstraEraTxBody era
  , EraTx era
  , STxLevel l era ~ STxBothLevels l era
  ) =>
  UTxO era ->
  TxBody l era ->
  CapacityDeposit
getConsumedCapacityDeposits utxo txBody =
  withBothTxLevels
    txBody
    ( \topTxBody ->
        txBodyReleasedCapacityDeposits utxo topTxBody
          <> foldMap'
            (txBodyReleasedCapacityDeposits utxo . view bodyTxL)
            (topTxBody ^. subTransactionsTxBodyL)
    )
    (txBodyReleasedCapacityDeposits utxo)

-- | The capacity deposits released by the inputs one body consumes. Uses
-- the same input set as the assets side.
txBodyReleasedCapacityDeposits ::
  (DijkstraEraTxOut era, EraTxBody era) =>
  UTxO era ->
  TxBody l era ->
  CapacityDeposit
txBodyReleasedCapacityDeposits utxo txBody =
  foldMap'
    (^. capacityDepositTxOutL)
    (unUTxO (txInsFilter utxo (txBody ^. inputsTxBodyL)))

-- | The application side of what a transaction produces: output assets,
-- fee, donations, governance and certificate deposits, burned quantities.
-- Capacity deposits are deliberately absent — they are operational funding,
-- accounted by 'getProducedCapacityDeposits'.
getProducedAssets ::
  forall era.
  ( DijkstraEraTxBody era
  , EraUTxO era
  , Value era ~ Assets
  ) =>
  PParams era ->
  (KeyHash StakePool -> Bool) ->
  TxBody TopTx era ->
  Assets
getProducedAssets pp isRegPoolId topTxBody =
  txBodyProducedAssetsWithoutCerts pp topTxBody
    <> foldMap'
      (txBodyProducedAssetsWithoutCerts pp . (^. bodyTxL))
      (topTxBody ^. subTransactionsTxBodyL)
    <> inject (topTxBody ^. feeTxBodyL)
    <> inject (getTotalDepositsTxCerts pp isRegPoolId (batchTxCerts topTxBody))

-- | The application side one body produces, certificates excluded (they are
-- processed separately, threading account state through the whole batch).
txBodyProducedAssetsWithoutCerts ::
  ( DijkstraEraTxBody era
  , EraUTxO era
  , Value era ~ Assets
  ) =>
  PParams era ->
  TxBody l era ->
  Assets
txBodyProducedAssetsWithoutCerts pp txBody =
  sumAllValue (txBody ^. outputsTxBodyL)
    <> inject (txBody ^. treasuryDonationTxBodyL)
    <> inject (conwayProposalsDeposits pp txBody)
    <> Assets (burnedMultiAssets txBody)
    <> inject (fold (unDirectDeposits (txBody ^. directDepositsTxBodyL)))

-- | Every certificate of the batch: the top-level body's own, and those of
-- each sub-transaction.
batchTxCerts ::
  (DijkstraEraTxBody era, EraTx era) =>
  TxBody TopTx era ->
  StrictSeq (TxCert era)
batchTxCerts topTxBody =
  foldMap' (^. bodyTxL . certsTxBodyL) (topTxBody ^. subTransactionsTxBodyL)
    <> (topTxBody ^. certsTxBodyL)

-- | The operational side of what a transaction produces: the capacity
-- deposits locked by every output the batch creates — own outputs and each
-- sub-transaction's.
getProducedCapacityDeposits ::
  forall era.
  ( DijkstraEraTxOut era
  , DijkstraEraTxBody era
  , EraTx era
  ) =>
  TxBody TopTx era ->
  CapacityDeposit
getProducedCapacityDeposits topTxBody =
  txBodyLockedCapacityDeposits topTxBody
    <> foldMap'
      (txBodyLockedCapacityDeposits . (^. bodyTxL))
      (topTxBody ^. subTransactionsTxBodyL)

-- | The capacity deposits locked by the outputs one body creates.
txBodyLockedCapacityDeposits ::
  (DijkstraEraTxOut era, EraTxBody era) =>
  TxBody l era ->
  CapacityDeposit
txBodyLockedCapacityDeposits txBody =
  foldMap' (^. capacityDepositTxOutL) (txBody ^. outputsTxBodyL)

instance EraUTxO DijkstraEra where
  type ScriptsNeeded DijkstraEra = AlonzoScriptsNeeded DijkstraEra

  -- The era-generic API forces one lumped value; the lump happens here and
  -- only here, visibly: application assets plus injected capacity deposits.
  getConsumedValue pp lookupStakingDeposit utxo txBody =
    getConsumedAssets pp lookupStakingDeposit utxo txBody
      <> inject (unCapacityDeposit (getConsumedCapacityDeposits utxo txBody))

  getProducedValue pp isRegPoolId txBody =
    getProducedAssets pp isRegPoolId txBody
      <> inject (unCapacityDeposit (getProducedCapacityDeposits txBody))

  getScriptsProvided = getDijkstraScriptsProvided

  getScriptsNeeded = getDijkstraScriptsNeeded

  getScriptsHashesNeeded = getAlonzoScriptsHashesNeeded

  getWitsVKeyNeeded _ = getConwayWitsVKeyNeeded

  getMinFeeTxUtxo = getConwayMinFeeTxUtxo

-- | Like 'getBabbageScriptsProvided', but for 'TopTx' also aggregates
-- scripts from all subtransactions.
getDijkstraScriptsProvided ::
  ( EraTx era
  , DijkstraEraTxBody era
  , STxLevel l era ~ STxBothLevels l era
  ) =>
  UTxO era ->
  Tx l era ->
  ScriptsProvided era
getDijkstraScriptsProvided utxo tx =
  withBothTxLevels
    tx
    ( \topTx ->
        ScriptsProvided $
          Map.unions $
            unScriptsProvided (getBabbageScriptsProvided utxo topTx)
              : [ unScriptsProvided (getBabbageScriptsProvided utxo subTx)
                | subTx <- OMap.elems (topTx ^. bodyTxL . subTransactionsTxBodyL)
                ]
    )
    (getBabbageScriptsProvided utxo)

getDijkstraScriptsNeeded ::
  (DijkstraEraTxBody era, DijkstraEraScript era) =>
  UTxO era -> TxBody l era -> AlonzoScriptsNeeded era
getDijkstraScriptsNeeded utxo txb =
  getConwayScriptsNeeded utxo txb
    <> guardingScriptsNeeded
  where
    guardingScriptsNeeded = AlonzoScriptsNeeded $
      catMaybes $
        zipAsIxItem (txb ^. guardsTxBodyL) $
          \(AsIxItem idx cred) -> (\sh -> (GuardingPurpose (AsIxItem idx sh), sh)) <$> credScriptHash cred

instance AlonzoEraUTxO DijkstraEra where
  getSupplementalDataHashes = getBabbageSupplementalDataHashes

  getSpendingDatum = getBabbageSpendingDatum

  scriptsProvidedStAnnTx = scriptsProvidedDijkstraStAnnTx

  scriptsNeededStAnnTx = scriptsNeededDijkstraStAnnTx

  plutusScriptsWithContextStAnnTx = plutusScriptsWithContextDijkstraStAnnTx

  plutusLanguagesUsedStAnnTx = plutusLanguagesUsedDijkstraStAnnTx

scriptsProvidedDijkstraStAnnTx ::
  ( EraTxLevel era
  , STxLevel l era ~ STxBothLevels l era
  , STxLevel SubTx era ~ STxBothLevels SubTx era
  , STxLevel TopTx era ~ STxBothLevels TopTx era
  ) =>
  DijkstraStAnnTx l era -> ScriptsProvided era
scriptsProvidedDijkstraStAnnTx stAnnTx =
  withBothTxLevels
    stAnnTx
    (\DijkstraStAnnTopTx {dsattScriptsProvided} -> dsattScriptsProvided)
    (\DijkstraStAnnSubTx {dsastScriptsProvided} -> dsastScriptsProvided)

scriptsNeededDijkstraStAnnTx ::
  ( EraTxLevel era
  , STxLevel l era ~ STxBothLevels l era
  , STxLevel SubTx era ~ STxBothLevels SubTx era
  , STxLevel TopTx era ~ STxBothLevels TopTx era
  ) =>
  DijkstraStAnnTx l era -> ScriptsNeeded era
scriptsNeededDijkstraStAnnTx stAnnTx =
  withBothTxLevels
    stAnnTx
    (\DijkstraStAnnTopTx {dsattScriptsNeeded} -> dsattScriptsNeeded)
    (\DijkstraStAnnSubTx {dsastScriptsNeeded} -> dsastScriptsNeeded)

plutusScriptsWithContextDijkstraStAnnTx ::
  ( EraTxLevel era
  , STxLevel l era ~ STxBothLevels l era
  , STxLevel SubTx era ~ STxBothLevels SubTx era
  , STxLevel TopTx era ~ STxBothLevels TopTx era
  ) =>
  DijkstraStAnnTx l era ->
  Either (NonEmpty (CollectError era)) [PlutusWithContext]
plutusScriptsWithContextDijkstraStAnnTx stAnnTx =
  withBothTxLevels
    stAnnTx
    (\DijkstraStAnnTopTx {dsattPlutusScriptsWithContext} -> dsattPlutusScriptsWithContext)
    (\DijkstraStAnnSubTx {dsastPlutusScriptsWithContext} -> dsastPlutusScriptsWithContext)

plutusLanguagesUsedDijkstraStAnnTx ::
  ( EraTxLevel era
  , STxLevel l era ~ STxBothLevels l era
  , STxLevel SubTx era ~ STxBothLevels SubTx era
  , STxLevel TopTx era ~ STxBothLevels TopTx era
  ) =>
  DijkstraStAnnTx l era -> Set Language
plutusLanguagesUsedDijkstraStAnnTx stAnnTx =
  withBothTxLevels
    stAnnTx
    (\DijkstraStAnnTopTx {dsattPlutusLanguagesUsed} -> dsattPlutusLanguagesUsed)
    (\DijkstraStAnnSubTx {dsastPlutusLanguagesUsed} -> dsastPlutusLanguagesUsed)

instance DijkstraEraUTxO DijkstraEra where
  subTransactionsStAnnTx = subTransactionsDijkstraStAnnTx
  plutusLegacyModeStAnnTxG = to (\DijkstraStAnnTopTx {dsattPlutusLegacyMode} -> dsattPlutusLegacyMode)
  scriptsHashesNeededStAnnTx = dsastScriptsHashesNeeded

subTransactionsDijkstraStAnnTx ::
  DijkstraStAnnTx TopTx era -> [DijkstraStAnnTx SubTx era]
subTransactionsDijkstraStAnnTx DijkstraStAnnTopTx {dsattSubTransactions} = dsattSubTransactions

-- | Total size of reference scripts across a top-level transaction and all its subtransactions.
batchNonDistinctRefScriptsSize ::
  ( EraTx era
  , DijkstraEraTxBody era
  ) =>
  UTxO era ->
  Tx TopTx era ->
  Int
batchNonDistinctRefScriptsSize utxo tx =
  txNonDistinctRefScriptsSize utxo tx
    + getSum
      ( foldMap'
          (Sum . txNonDistinctRefScriptsSize utxo)
          (tx ^. bodyTxL . subTransactionsTxBodyL)
      )

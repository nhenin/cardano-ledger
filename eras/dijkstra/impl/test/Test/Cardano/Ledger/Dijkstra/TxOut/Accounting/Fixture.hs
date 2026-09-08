{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Dijkstra.TxOut.Accounting.Fixture (
  pparams,
  tokens,
  ordinaryInputs,
  ordinaryBody,
  subBody,
  batchBody,
  collateralBody,
  collateralInputs,
  collateralState,
  applicationOnlyCollateralBody,
  incompleteNativeReturnBody,
  implicitTopBody,
  implicitSubBody,
  implicitCollateralBody,
  implicitCollateralState,
  emptyState,
) where

import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.BaseTypes (Network (Testnet), StrictMaybe (SJust))
import Cardano.Ledger.Coin (Coin (..), CompactForm (CompactCoin))
import Cardano.Ledger.Credential (Credential (KeyHashObj), StakeReference (StakeRefNull))
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.Core (
  CoinPerByte (..),
  PParams,
  TxBody,
  TxLevel (SubTx, TopTx),
  TxOut,
  capacityDepositFormTxOutL,
  collateralInputsTxBodyL,
  collateralReturnTxBodyL,
  emptyPParams,
  feeTxBodyL,
  inputsTxBodyL,
  mkBasicTx,
  mkBasicTxBody,
  mkBasicTxOut,
  outputValueTxOutL,
  outputsTxBodyL,
  ppCoinsPerUTxOByteL,
  subTransactionsTxBodyL,
  totalCollateralTxBodyL,
 )
import Cardano.Ledger.Dijkstra.TxOut (CapacityDepositForm (..))
import Cardano.Ledger.Dijkstra.TxOut.ApplicationAssets (ApplicationAssets (..))
import Cardano.Ledger.Dijkstra.TxOut.CapacityDeposit (CapacityDeposit (..))
import Cardano.Ledger.Dijkstra.TxOut.Value (OutputValue (..))
import Cardano.Ledger.Hashes (ScriptHash (..))
import Cardano.Ledger.Mary.AssetName (AssetName (..))
import Cardano.Ledger.Mary.MultiAsset (MultiAsset (..))
import Cardano.Ledger.Mary.PolicyID (PolicyID (..))
import Cardano.Ledger.Shelley.LedgerState (UTxOState (..))
import Cardano.Ledger.State (UTxO (..), txouts)
import Cardano.Ledger.TxIn (TxIn)
import Data.Default (def)
import qualified Data.Map.Strict as Map
import qualified Data.OMap.Strict as OMap
import Lens.Micro ((&), (.~))
import Test.Cardano.Ledger.Shelley.Examples (mkKeyHash)

pparams :: PParams DijkstraEra
pparams = emptyPParams & ppCoinsPerUTxOByteL .~ CoinPerByte (CompactCoin 1)

tokens :: MultiAsset
tokens = nativeTokens 10

nativeTokens :: Integer -> MultiAsset
nativeTokens quantity =
  MultiAsset $
    Map.singleton
      (PolicyID (ScriptHash "00000000000000000000000000000000000000000000000000000000"))
      (Map.singleton (AssetName "returned-token") quantity)

address :: Addr
address = Addr Testnet (KeyHashObj (mkKeyHash 1)) StakeRefNull

-- These small explicit amounts isolate accounting from tariff validation.
-- The capacity rule and its exact-allocation cases have their own tests.
explicitOutput :: Integer -> Integer -> MultiAsset -> TxOut DijkstraEra
explicitOutput application deposit native =
  mkBasicTxOut address mempty
    & outputValueTxOutL
      .~ OutputValue (CapacityDeposit (Coin deposit)) (ApplicationAssets (Coin application) native)
    & capacityDepositFormTxOutL .~ ExplicitCapacityDeposit

implicitOutput :: Integer -> TxOut DijkstraEra
implicitOutput total =
  explicitOutput total 0 mempty
    & capacityDepositFormTxOutL .~ ImplicitCapacityDeposit

ordinaryInputs :: UTxO DijkstraEra
ordinaryInputs = txouts $ mkBasicTxBody @DijkstraEra @TopTx & outputsTxBodyL .~ [explicitOutput 7 3 tokens]

ordinaryBody :: TxBody TopTx DijkstraEra
ordinaryBody =
  mkBasicTxBody
    & inputsTxBodyL .~ Map.keysSet (unUTxO ordinaryInputs)
    & outputsTxBodyL .~ [explicitOutput 4 2 tokens]
    & feeTxBodyL .~ Coin 4

subBody :: TxBody SubTx DijkstraEra
subBody = mkBasicTxBody & outputsTxBodyL .~ [explicitOutput 7 3 tokens]

batchBody :: TxBody TopTx DijkstraEra
batchBody = ordinaryBody & subTransactionsTxBodyL .~ OMap.singleton (mkBasicTx subBody)

collateralInputs :: Map.Map TxIn (TxOut DijkstraEra)
collateralInputs = unUTxO ordinaryInputs

collateralBody :: TxBody TopTx DijkstraEra
collateralBody =
  mkBasicTxBody
    & collateralInputsTxBodyL .~ Map.keysSet collateralInputs
    & collateralReturnTxBodyL .~ SJust (explicitOutput 4 2 tokens)
    & totalCollateralTxBodyL .~ SJust (Coin 4)

applicationOnlyCollateralBody :: TxBody TopTx DijkstraEra
applicationOnlyCollateralBody = collateralBody & totalCollateralTxBodyL .~ SJust (Coin 3)

incompleteNativeReturnBody :: TxBody TopTx DijkstraEra
incompleteNativeReturnBody =
  collateralBody & collateralReturnTxBodyL .~ SJust (explicitOutput 4 2 (nativeTokens 9))

emptyState :: UTxOState DijkstraEra
emptyState = def

collateralState :: UTxOState DijkstraEra
collateralState = emptyState {utxosUtxo = ordinaryInputs, utxosFees = Coin 1}

implicitTopBody :: TxBody TopTx DijkstraEra
implicitTopBody = mkBasicTxBody & outputsTxBodyL .~ [implicitOutput 1000000]

implicitSubBody :: TxBody SubTx DijkstraEra
implicitSubBody = mkBasicTxBody & outputsTxBodyL .~ [implicitOutput 1000000]

implicitCollateralState :: UTxOState DijkstraEra
implicitCollateralState =
  emptyState
    { utxosUtxo =
        txouts $ mkBasicTxBody @DijkstraEra @TopTx & outputsTxBodyL .~ [explicitOutput 9999950 50 mempty]
    , utxosFees = Coin 1
    }

implicitCollateralBody :: TxBody TopTx DijkstraEra
implicitCollateralBody =
  mkBasicTxBody
    & collateralInputsTxBodyL .~ Map.keysSet (unUTxO (utxosUtxo implicitCollateralState))
    & collateralReturnTxBodyL .~ SJust (implicitOutput 1000000)

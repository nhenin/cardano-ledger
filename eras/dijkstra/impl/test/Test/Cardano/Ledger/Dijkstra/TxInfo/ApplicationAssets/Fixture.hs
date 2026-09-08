{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Dijkstra.TxInfo.ApplicationAssets.Fixture (
  Projection (..),
  creationContext,
  spendingContext,
  referenceContext,
  subCreationContext,
  project,
  projectWithoutParameters,
  expectedApplicationValue,
  originalTxId,
) where

import Cardano.Ledger.Alonzo.Plutus.Context (
  EraPlutusContext (..),
  LedgerTxInfo (..),
  mkPlutusTxInfoFromResult,
 )
import Cardano.Ledger.Alonzo.Scripts (AsPurpose (..))
import Cardano.Ledger.BaseTypes (Globals (..), ProtVer (..))
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Scripts (DijkstraPlutusPurpose (..))
import Cardano.Ledger.Dijkstra.TxInfo (DijkstraContextError)
import Cardano.Ledger.Plutus (Language (..), SLanguage (..), transSafeHash)
import qualified Cardano.Ledger.Plutus.Value.Translation as PlutusValue
import Cardano.Ledger.State (UTxO (..))
import Cardano.Ledger.TxIn (TxId (..), TxIn (..))
import Data.ByteString (ByteString)
import qualified Data.Map.Strict as Map
import GHC.Exts (fromList)
import Lens.Micro ((&), (.~), (^.))
import qualified PlutusLedgerApi.V1 as PV1
import qualified PlutusLedgerApi.V2 as PV2
import qualified PlutusLedgerApi.V3 as PV3
import qualified PlutusLedgerApi.V4 as PV4
import Test.Cardano.Ledger.Core.Utils (testGlobals)

-- Keep the observation independent of the output allocator: compare the actual
-- script-facing Value, including every native asset, across all four APIs.
data Projection = Projection
  { projectedInputs :: [PV1.Value]
  , projectedReferenceInputs :: [PV1.Value]
  , projectedOutputs :: [PV1.Value]
  , projectedTxId :: ByteString
  }
  deriving (Eq, Show)

creationContext :: TxOut DijkstraEra -> LedgerTxInfo DijkstraEra
creationContext out =
  context mempty $
    mkBasicTx @DijkstraEra @TopTx (mkBasicTxBody & outputsTxBodyL .~ fromList [out])

subCreationContext :: TxOut DijkstraEra -> LedgerTxInfo DijkstraEra
subCreationContext out =
  context mempty $
    mkBasicTx @DijkstraEra @SubTx (mkBasicTxBody & outputsTxBodyL .~ fromList [out])

spendingContext :: TxOut DijkstraEra -> LedgerTxInfo DijkstraEra
spendingContext out =
  context (UTxO (Map.singleton (outputReference out) out)) $
    mkBasicTx @DijkstraEra @TopTx (mkBasicTxBody & inputsTxBodyL .~ fromList [outputReference out])

referenceContext :: TxOut DijkstraEra -> LedgerTxInfo DijkstraEra
referenceContext out =
  context (UTxO (Map.singleton (outputReference out) out)) $
    mkBasicTx @DijkstraEra @TopTx
      (mkBasicTxBody & referenceInputsTxBodyL .~ fromList [outputReference out])

outputReference :: TxOut DijkstraEra -> TxIn
outputReference out =
  TxIn (txIdTxBody (mkBasicTxBody @DijkstraEra @TopTx & outputsTxBodyL .~ fromList [out])) minBound

context :: UTxO DijkstraEra -> Tx l DijkstraEra -> LedgerTxInfo DijkstraEra
context utxo tx =
  LedgerTxInfo
    { ltiProtVer = ProtVer (eraProtVerLow @DijkstraEra) 0
    , ltiEpochInfo = epochInfo testGlobals
    , ltiSystemStart = systemStart testGlobals
    , ltiUTxO = utxo
    , ltiTx = tx
    , ltiMemoizedSubTransactions = mempty
    }

expectedApplicationValue :: TxOut DijkstraEra -> PV1.Value
expectedApplicationValue out = PlutusValue.fromLedgerMaryValue (out ^. valueTxOutL)

originalTxId :: LedgerTxInfo DijkstraEra -> ByteString
originalTxId LedgerTxInfo {ltiTx = tx} =
  PV1.fromBuiltin . transSafeHash . unTxId . txIdTxBody $ tx ^. bodyTxL

project ::
  Language ->
  PParams DijkstraEra ->
  LedgerTxInfo DijkstraEra ->
  Either (DijkstraContextError DijkstraEra) Projection
project lang pp = projectResult lang . mkTxInfoResultWithPParams pp

projectWithoutParameters ::
  Language -> LedgerTxInfo DijkstraEra -> Either (DijkstraContextError DijkstraEra) Projection
projectWithoutParameters lang = projectResult lang . mkTxInfoResult

projectResult ::
  Language -> TxInfoResult DijkstraEra -> Either (DijkstraContextError DijkstraEra) Projection
projectResult lang result = case lang of
  PlutusV1 -> do
    info <- mkPlutusTxInfoFromResult (DijkstraSpending AsPurpose) $ lookupTxInfoResult SPlutusV1 result
    pure $
      Projection
        (map (PV1.txOutValue . PV1.txInInfoResolved) (PV1.txInfoInputs info))
        []
        (map PV1.txOutValue (PV1.txInfoOutputs info))
        (PV1.fromBuiltin (PV1.getTxId (PV1.txInfoId info)))
  PlutusV2 -> do
    info <- mkPlutusTxInfoFromResult (DijkstraSpending AsPurpose) $ lookupTxInfoResult SPlutusV2 result
    pure $
      Projection
        (map (PV2.txOutValue . PV2.txInInfoResolved) (PV2.txInfoInputs info))
        (map (PV2.txOutValue . PV2.txInInfoResolved) (PV2.txInfoReferenceInputs info))
        (map PV2.txOutValue (PV2.txInfoOutputs info))
        (PV1.fromBuiltin (PV1.getTxId (PV2.txInfoId info)))
  PlutusV3 -> do
    info <- mkPlutusTxInfoFromResult (DijkstraSpending AsPurpose) $ lookupTxInfoResult SPlutusV3 result
    pure $
      Projection
        (map (PV2.txOutValue . PV3.txInInfoResolved) (PV3.txInfoInputs info))
        (map (PV2.txOutValue . PV3.txInInfoResolved) (PV3.txInfoReferenceInputs info))
        (map PV2.txOutValue (PV3.txInfoOutputs info))
        (PV1.fromBuiltin (PV3.getTxId (PV3.txInfoId info)))
  PlutusV4 -> do
    info <- mkPlutusTxInfoFromResult (DijkstraSpending AsPurpose) $ lookupTxInfoResult SPlutusV4 result
    pure $
      Projection
        (map (PV4.txOutValue . PV4.txInInfoResolved) (PV4.txInfoInputs info))
        (map (PV4.txOutValue . PV4.txInInfoResolved) (PV4.txInfoReferenceInputs info))
        (map PV4.txOutValue (PV4.txInfoOutputs info))
        (PV1.fromBuiltin (PV4.getTxId (PV4.txInfoId info)))

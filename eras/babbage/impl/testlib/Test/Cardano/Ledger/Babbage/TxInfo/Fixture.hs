{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}

module Test.Cardano.Ledger.Babbage.TxInfo.Fixture (
  zeroCapacityPricePParams,
  metadataTxInfo,
  metadataTxInfoResult,
  prepareMetadataUTxO,
) where

import Cardano.Ledger.Alonzo.Plutus.Context (
  ContextError,
  EraPlutusContext (lookupTxInfoResult, mkTxInfoResultWithPParams),
  EraPlutusTxInfo,
  LedgerTxInfo,
  PlutusTxInfo,
  PlutusTxInfoResult,
  mkPlutusTxInfoFromResult,
 )
import Cardano.Ledger.Alonzo.Scripts (AsPurpose (..), pattern SpendingPurpose)
import Cardano.Ledger.Core (EraPParams, PParams, TxOut, emptyPParams)
import Cardano.Ledger.Plutus.Language (SLanguage)
import Cardano.Ledger.State (UTxO (..))
import qualified Data.Map.Strict as Map

-- | The existing default parameter record has a zero capacity price. These
-- fixtures test metadata, so their small coin amounts remain application ADA.
zeroCapacityPricePParams :: EraPParams era => PParams era
zeroCapacityPricePParams = emptyPParams

metadataTxInfoResult ::
  (EraPParams era, EraPlutusTxInfo l era) =>
  SLanguage l ->
  LedgerTxInfo era ->
  PlutusTxInfoResult l era
metadataTxInfoResult slang =
  lookupTxInfoResult slang . mkTxInfoResultWithPParams zeroCapacityPricePParams

metadataTxInfo ::
  (EraPParams era, EraPlutusTxInfo l era) =>
  SLanguage l ->
  LedgerTxInfo era ->
  Either (ContextError era) (PlutusTxInfo l)
metadataTxInfo slang lti =
  mkPlutusTxInfoFromResult (SpendingPurpose AsPurpose) $ metadataTxInfoResult slang lti

prepareMetadataUTxO :: (TxOut era -> TxOut era) -> UTxO era -> UTxO era
prepareMetadataUTxO prepareOutput = UTxO . Map.map prepareOutput . unUTxO

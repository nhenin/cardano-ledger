{-# LANGUAGE FlexibleContexts #-}

module Test.Cardano.Ledger.Dijkstra.TxInfo.Fixture (
  metadataTxInfo,
  metadataOutputWithFreeCapacity,
) where

import Cardano.Ledger.Alonzo.Plutus.Context (EraPlutusTxInfo, LedgerTxInfo, PlutusTxInfoResult)
import Cardano.Ledger.Core (EraPParams, TxOut)
import Cardano.Ledger.Dijkstra.TxOut (DijkstraEraTxOut)
import Cardano.Ledger.Dijkstra.TxOut.Translation (fundCapacityDeposit)
import Cardano.Ledger.Plutus.Language (SLanguage)
import Test.Cardano.Ledger.Babbage.TxInfo.Fixture (metadataTxInfoResult, zeroCapacityPricePParams)

-- | Address and transaction metadata fixtures have a known zero price. Their
-- implicit outputs can therefore be projected without reducing application ADA.
metadataTxInfo ::
  (EraPParams era, EraPlutusTxInfo l era) =>
  SLanguage l ->
  LedgerTxInfo era ->
  PlutusTxInfoResult l era
metadataTxInfo = metadataTxInfoResult

-- | Prepare the independent output/input oracle at the metadata context's
-- zero price. The parameter-free translator receives an explicit allocation.
metadataOutputWithFreeCapacity :: DijkstraEraTxOut era => TxOut era -> TxOut era
metadataOutputWithFreeCapacity = fundCapacityDeposit zeroCapacityPricePParams

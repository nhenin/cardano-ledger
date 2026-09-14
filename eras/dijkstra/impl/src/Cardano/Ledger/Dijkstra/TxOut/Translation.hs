{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

-- | Convert complete Conway outputs using standard or injected deposit recovery.
module Cardano.Ledger.Dijkstra.TxOut.Translation (
  -- * Conway to Dijkstra output translation
  fromConway,
  fromConway',
) where

import Cardano.Ledger.Alonzo.Core (AlonzoEraTxOut (datumTxOutF))
import Cardano.Ledger.Babbage.TxOut (BabbageEraTxOut (referenceScriptTxOutL))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Core (
  EraScript (upgradeScript),
  EraTxOut (..),
  PParams,
  RecoverCapacityDeposit,
 )
import Cardano.Ledger.Dijkstra.TxOut (DijkstraTxOut (DijkstraTxOut))
import Cardano.Ledger.Dijkstra.TxOut.Value (OutputValue)
import Cardano.Ledger.Dijkstra.TxOut.Value.Translation (
  AllocationError,
  recoverOutputAllocation,
  requiredCapacityDeposit,
 )
import Cardano.Ledger.Plutus.Data (translateDatum)
import Lens.Micro.Extras (view)

-- | Recover a Conway output's allocation using the supplied source parameters.
-- Recovery assumes this price matches the price when the output was created.
fromConway ::
  PParams ConwayEra ->
  TxOut ConwayEra ->
  Either AllocationError DijkstraTxOut
fromConway = fromConway' . requiredCapacityDeposit

-- | Convert with an injected deposit-recovery policy. Return allocation failures
-- before constructing the Dijkstra output.
fromConway' ::
  RecoverCapacityDeposit ConwayEra ->
  TxOut ConwayEra ->
  Either AllocationError DijkstraTxOut
fromConway' recoverCapacityDeposit =
  fmap . flip upgradeDijkstraTxOut
    <*> recoverOutputAllocation recoverCapacityDeposit

-- Private helpers

-- | Upgrade the structural fields using an allocation already supplied by the
-- caller. Deposit recovery and allocation failures belong to the source-output mapper.
upgradeDijkstraTxOut :: OutputValue -> TxOut ConwayEra -> DijkstraTxOut
upgradeDijkstraTxOut outputValue =
  DijkstraTxOut
    <$> view addrTxOutL
    <*> pure outputValue
    <*> (translateDatum . view datumTxOutF)
    <*> (fmap upgradeScript . view referenceScriptTxOutL)

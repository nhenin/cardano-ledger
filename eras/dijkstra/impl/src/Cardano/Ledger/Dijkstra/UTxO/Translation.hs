-- | Parameterized allocation at the two ledger-state boundaries: migration of
-- the historical UTxO and insertion of outputs from validated transactions.
module Cardano.Ledger.Dijkstra.UTxO.Translation (
  fundCapacityDeposits,
  translateUTxO,
  translateUTxOWithReport,
) where

import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Core (PParams)
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Dijkstra.TxOut (DijkstraEraTxOut)
import Cardano.Ledger.Dijkstra.TxOut.Translation (
  CapacityDepositAllocationError,
  fundCapacityDeposit,
  translateTxOutWithReport,
 )
import Cardano.Ledger.State (UTxO (..))
import Cardano.Ledger.TxIn (TxIn)
import qualified Data.Map.Strict as Map

-- | Only implicit outputs are allocated. Explicit historical allocations are
-- retained even when current protocol parameters have changed.
fundCapacityDeposits :: DijkstraEraTxOut era => PParams era -> UTxO era -> UTxO era
fundCapacityDeposits pp = UTxO . Map.map (fundCapacityDeposit pp) . unUTxO

translateUTxOWithReport ::
  PParams DijkstraEra ->
  UTxO ConwayEra ->
  (UTxO DijkstraEra, Map.Map TxIn CapacityDepositAllocationError)
translateUTxOWithReport pp (UTxO utxo) =
  (UTxO (Map.map fst translated), Map.mapMaybe snd translated)
  where
    translated = Map.map (translateTxOutWithReport pp) utxo

-- | Total era migration. Underfunded historical outputs and absent exact
-- fixpoints have deterministic, funds-preserving fallbacks. The reporting
-- variant exposes these exceptions to the newly-created-output exact rule.
translateUTxO :: PParams DijkstraEra -> UTxO ConwayEra -> UTxO DijkstraEra
translateUTxO pp = fst . translateUTxOWithReport pp

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Transitional monetary component for the Dijkstra output split.
--
-- The construction container holds a capacity deposit and application assets.
-- Dijkstra stores those two components directly in its compact output variants.
--
-- 'OutputValue' is the construction allocation for @TxOut DijkstraEra@; the
-- era's 'ApplicationAssets' remain independently accessible as its value.
-- Allocation, output encoding, historical-output translation and script-facing
-- projections must be specified at their integration boundaries. Collapsing the
-- components into a @MaryValue@ would lose the split.
module Cardano.Ledger.Dijkstra.TxOut.Value (
  OutputValue (..),
  outputCoins,
) where

import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Dijkstra.TxOut.ApplicationAssets (ApplicationAssets, applicationCoins)
import Cardano.Ledger.Dijkstra.TxOut.CapacityDeposit (CapacityDeposit, unCapacityDeposit)
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)

-- | Explicit allocation of the monetary components of an output.
-- Neither component is inferred from the other. Their constructors retain the
-- underlying quantities; this is not a proof of output validity.
--
-- There is deliberately no generic value-arithmetic instance: this type describes
-- an output allocation, not a transaction balance or a signed difference.
data OutputValue = OutputValue
  { capacityDeposit :: !CapacityDeposit
  , applicationAssets :: !ApplicationAssets
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData, NoThunks)

-- | Total ADA accounted for by both components, counted exactly once.
-- Native assets remain in 'applicationAssets'. This is a read-only projection:
-- setting a total would require an explicit rule for choosing its allocation.
outputCoins :: OutputValue -> Coin
outputCoins (OutputValue deposit assets) =
  unCapacityDeposit deposit <> applicationCoins assets

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | ADA allocated to backing persistent UTxO state.
module Cardano.Ledger.Dijkstra.TxOut.CapacityDeposit (
  CapacityDeposit (..),
) where

import Cardano.Ledger.Coin (Coin)
import Control.DeepSeq (NFData)
import NoThunks.Class (NoThunks)

-- | An explicitly supplied allocation, distinct from application ADA and from
-- the amount required by a pricing rule. This type does not calculate that
-- requirement or decide whether surplus backing is permitted.
--
-- A change to @coinsPerUTxOByte@ would expose additional deposit-management
-- work: the amount allocated under earlier parameters may differ from the
-- current requirement. An increase could make an old output's total ADA
-- insufficient if its backing were recalculated at the new price. Existing
-- minimum-ADA rules check newly produced outputs; they do not retroactively
-- invalidate existing UTxO entries when the parameter changes.
--
-- The split design must distinguish historical allocation, current required
-- backing and the amount released on spending, including how a transaction
-- funds any difference for its new outputs. This friction is not yet exercised
-- by the current model refactor, which changes neither parameters nor rules.
-- No repricing, additional-funding or release policy is selected here.
--
-- Construction does not validate the amount. Zero is an amount, not a marker
-- requesting implicit allocation or identifying a historical output.
newtype CapacityDeposit = CapacityDeposit {unCapacityDeposit :: Coin}
  deriving stock (Eq, Show)
  deriving newtype (NFData, NoThunks)

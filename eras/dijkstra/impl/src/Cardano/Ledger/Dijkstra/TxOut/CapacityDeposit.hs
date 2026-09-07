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
-- Construction does not validate the amount. Zero is an amount, not a marker
-- requesting implicit allocation or identifying a historical output.
newtype CapacityDeposit = CapacityDeposit {unCapacityDeposit :: Coin}
  deriving stock (Eq, Show)
  deriving newtype (NFData, NoThunks)

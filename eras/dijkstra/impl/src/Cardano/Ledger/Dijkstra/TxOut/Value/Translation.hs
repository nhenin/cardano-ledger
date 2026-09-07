-- | Explicit mapping between the historical output-value representation and
-- the split output model. Allocation belongs to the caller; this boundary does
-- not choose a capacity price, migrate an era or define a script representation.
module Cardano.Ledger.Dijkstra.TxOut.Value.Translation (
  AllocationError (..),
  fromMaryValue,
  toMaryValue,
) where

import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Dijkstra.TxOut.ApplicationAssets (ApplicationAssets (..))
import Cardano.Ledger.Dijkstra.TxOut.CapacityDeposit (CapacityDeposit (..))
import Cardano.Ledger.Dijkstra.TxOut.Value (OutputValue (..), outputCoins)
import Cardano.Ledger.Mary.Value (MaryValue (..))

-- | Failure to fund the caller's capacity allocation from the source output.
data AllocationError
  = NegativeCapacityDeposit !CapacityDeposit
  | -- | Available output ADA and the requested deposit, respectively.
    CapacityDepositExceedsOutputCoins !Coin !CapacityDeposit
  deriving (Eq, Show)

-- | Interpret the source 'MaryValue' as a historical output whose 'Coin' is
-- its total ADA. Allocate the explicitly supplied deposit from that total and
-- place the remaining ADA and all native assets in 'ApplicationAssets'.
--
-- Reject a negative deposit or one exceeding the source total. These are
-- allocation checks, not full output validation: native quantities, raw zero
-- entries, empty policies and quantity bounds are not checked or normalized.
-- A zero deposit is an explicit allocation, not a request to infer one.
fromMaryValue :: CapacityDeposit -> MaryValue -> Either AllocationError OutputValue
fromMaryValue deposit@(CapacityDeposit (Coin requested)) (MaryValue total@(Coin available) native)
  | requested < 0 = Left (NegativeCapacityDeposit deposit)
  | requested > available =
      Left (CapacityDepositExceedsOutputCoins total deposit)
  | otherwise =
      Right $
        OutputValue deposit (ApplicationAssets (Coin (available - requested)) native)

-- | Flatten the allocation to total ADA and the unchanged native-asset map.
-- This loses the distinction between capacity and application ADA. Reconstructing
-- the same allocation with 'fromMaryValue' requires retaining its deposit
-- separately, and requires that the allocation passes that function's checks.
-- This projection does not validate the supplied 'OutputValue'.
toMaryValue :: OutputValue -> MaryValue
toMaryValue value =
  MaryValue (outputCoins value) (nativeAssets (applicationAssets value))

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Mapping between Mary-valued outputs and the split output model, using
-- either the era's minimum-coin policy or an explicitly supplied allocation.
module Cardano.Ledger.Dijkstra.TxOut.Value.Translation (
  AllocationError (..),
  fromMaryOutputValue,
  requiredCapacityDeposit,
  fromMaryValue,
  toMaryValue,
) where

import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core (EraTxOut (..), PParams, Value)
import Cardano.Ledger.Dijkstra.TxOut.ApplicationAssets (ApplicationAssets (..), nativeAssets)
import Cardano.Ledger.Dijkstra.TxOut.CapacityDeposit (CapacityDeposit (..))
import Cardano.Ledger.Dijkstra.TxOut.Value (OutputValue (..), outputCoins)
import Cardano.Ledger.Mary.Value (MaryValue (..))
import Lens.Micro ((^.))

-- | Failure to fund the caller's capacity allocation from the source output.
data AllocationError
  = NegativeCapacityDeposit !CapacityDeposit
  | -- | Available output ADA and the requested deposit, respectively.
    CapacityDepositExceedsOutputCoins !Coin !CapacityDeposit
  deriving (Eq, Show)

-- | Allocate the era's minimum coin requirement to the capacity deposit.
-- The policy uses the supplied protocol parameters and the complete source
-- output; its remaining ADA and native assets belong to application assets.
-- This projection does not change the source output or recover a past deposit.
fromMaryOutputValue ::
  (EraTxOut era, Value era ~ MaryValue) =>
  PParams era ->
  TxOut era ->
  Either AllocationError OutputValue
fromMaryOutputValue protocolParameters txOut =
  fromMaryValue (requiredCapacityDeposit protocolParameters txOut) (txOut ^. valueTxOutL)

-- | Capacity required by the era's policy for the complete output.
requiredCapacityDeposit :: EraTxOut era => PParams era -> TxOut era -> CapacityDeposit
requiredCapacityDeposit protocolParameters txOut =
  CapacityDeposit (getMinCoinTxOut protocolParameters txOut)

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
        OutputValue deposit (ApplicationAssets (MaryValue (Coin (available - requested)) native))

-- | Flatten the allocation to total ADA and the unchanged native-asset map.
-- This loses the distinction between capacity and application ADA. Reconstructing
-- the same allocation with 'fromMaryValue' requires retaining its deposit
-- separately, and requires that the allocation passes that function's checks.
-- This projection does not validate the supplied 'OutputValue'.
toMaryValue :: OutputValue -> MaryValue
toMaryValue value =
  MaryValue (outputCoins value) (nativeAssets (applicationAssets value))

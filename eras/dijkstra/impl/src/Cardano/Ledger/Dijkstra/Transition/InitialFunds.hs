-- | Split initial ADA into the smallest sufficient capacity deposit and the
-- remaining application assets, using the size of the resulting Dijkstra output.
module Cardano.Ledger.Dijkstra.Transition.InitialFunds (
  -- * Allocate initial funds and report funding failures
  allocateInitialFunds,
  InitialFundsAllocationError (..),
) where

import Cardano.Ledger.Address (Addr)
import Cardano.Ledger.Coin (Coin (..), integerToWord64)
import Cardano.Ledger.Core (EraTxOut (mkBasicTxOut), PParams)
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Dijkstra.TxOut.CapacityDeposit (CapacityDeposit (..))
import Cardano.Ledger.Dijkstra.TxOut.LedgerInstances ()
import Cardano.Ledger.Dijkstra.TxOut.Value (OutputValue (..))
import Cardano.Ledger.Dijkstra.TxOut.Value.Translation (requiredCapacityDeposit)
import Cardano.Ledger.Val (inject)
import Data.Maybe (listToMaybe, mapMaybe)
import qualified Data.Set as Set

data InitialFundsAllocationError
  = -- | Initial ADA must fit the unsigned 64-bit coin representation.
    InitialFundsOutsideCoinRange !Coin
  | -- | No split of the available ADA can fund the resulting output's capacity.
    InsufficientInitialFunds !Coin
  deriving (Eq, Show)

-- | Preserve the total initial ADA and reserve the least deposit that covers
-- the final basic output's minimum coin requirement. Exact equality is not
-- required: moving ADA between the two components can change their CBOR sizes.
-- Reject unrepresentable amounts and amounts that cannot fund any allocation.
allocateInitialFunds ::
  PParams DijkstraEra ->
  Addr ->
  Coin ->
  Either InitialFundsAllocationError OutputValue
allocateInitialFunds protocolParameters address total@(Coin available) =
  case integerToWord64 available of
    Nothing -> Left (InitialFundsOutsideCoinRange total)
    Just _ ->
      maybe (Left (InsufficientInitialFunds total)) Right $
        listToMaybe $
          mapMaybe
            (fundCapacityDeposit protocolParameters address total)
            (constantSizeDepositRanges available)

-- Private helpers

-- The requirement is constant within this range. Its first sufficient deposit
-- is either the requirement itself or the range's lower bound.
fundCapacityDeposit ::
  PParams DijkstraEra -> Addr -> Coin -> (Integer, Integer) -> Maybe OutputValue
fundCapacityDeposit protocolParameters address total (lower, upper) =
  let CapacityDeposit (Coin required) =
        requiredCapacityDeposit protocolParameters $
          mkBasicTxOut address (allocateCapacityDeposit total lower)
      deposit = max lower required
   in if deposit <= upper
        then Just (allocateCapacityDeposit total deposit)
        else Nothing

-- Called only with a representable total and a deposit between zero and total.
allocateCapacityDeposit :: Coin -> Integer -> OutputValue
allocateCapacityDeposit (Coin total) deposit =
  OutputValue (CapacityDeposit (Coin deposit)) (inject (Coin (total - deposit)))

-- A basic output contains two unsigned CBOR coins and a fixed address, with no
-- datum, reference script or native assets. Coin widths change at these four
-- boundaries. Application ADA crosses them in reverse as the deposit grows.
-- The resulting (at most nine) ranges cover every split, in ascending order.
constantSizeDepositRanges :: Integer -> [(Integer, Integer)]
constantSizeDepositRanges total =
  let coinWidthBoundaries = [24, 256, 65536, 4294967296]
      splitBoundaries = coinWidthBoundaries <> map (total + 1 -) coinWidthBoundaries
      boundaries =
        Set.toAscList $
          Set.fromList $
            0 : total + 1 : filter (\boundary -> 0 < boundary && boundary <= total) splitBoundaries
   in zip boundaries (map (subtract 1) (drop 1 boundaries))

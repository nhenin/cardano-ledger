{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Contextful allocation of legacy outputs. Structural 'upgradeTxOut' and
-- decoding never allocate ADA: neither operation has the current parameters.
-- These functions act on the output entering ledger state, never signed bytes.
module Cardano.Ledger.Dijkstra.TxOut.Translation (
  CapacityDepositAllocationError (..),
  allocateCapacityDeposit,
  fundCapacityDeposit,
  fundCapacityDepositWithReport,
  translateTxOut,
  translateTxOutWithReport,
) where

import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Core (EraTxOut (..), PParams, TxOut)
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Dijkstra.TxOut (CapacityDepositForm (..), DijkstraEraTxOut (..))
import Cardano.Ledger.Dijkstra.TxOut.ApplicationAssets (ApplicationAssets (..))
import Cardano.Ledger.Dijkstra.TxOut.CapacityDeposit (CapacityDeposit (..))
import Cardano.Ledger.Dijkstra.TxOut.Value (OutputValue (..), outputCoins)
import Control.DeepSeq (NFData)
import Data.List (find)
import Data.Maybe (listToMaybe)
import qualified Data.Set as Set
import Data.Word (Word64)
import GHC.Generics (Generic)
import Lens.Micro ((&), (.~), (^.))

-- | A new implicit output must admit an exact allocation. Historical stock
-- cannot be rejected at an era boundary; the reporting migration API retains
-- these exceptions while preserving its funds.
data CapacityDepositAllocationError
  = CapacityDepositInsufficient !Coin !CapacityDeposit
  | CapacityDepositNoExactAllocation !Coin
  | CapacityDepositInvalidTotal !Coin
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

instance EncCBOR CapacityDepositAllocationError where
  encCBOR =
    encode . \case
      CapacityDepositInsufficient available required ->
        Sum CapacityDepositInsufficient 0 !> To available !> To required
      CapacityDepositNoExactAllocation total -> Sum CapacityDepositNoExactAllocation 1 !> To total
      CapacityDepositInvalidTotal total -> Sum CapacityDepositInvalidTotal 2 !> To total

instance DecCBOR CapacityDepositAllocationError where
  decCBOR = decode . Summands "CapacityDepositAllocationError" $ \case
    0 -> SumD CapacityDepositInsufficient <! From <! From
    1 -> SumD CapacityDepositNoExactAllocation <! From
    2 -> SumD CapacityDepositInvalidTotal <! From
    tag -> Invalid tag

-- | Find the smallest exact allocation, preserving total ADA and every native
-- asset quantity. An explicit output already states its allocation and is
-- returned unchanged; its exact tariff is checked by the output rule.
allocateCapacityDeposit ::
  DijkstraEraTxOut era =>
  PParams era ->
  TxOut era ->
  Either CapacityDepositAllocationError (TxOut era)
allocateCapacityDeposit pp txOut
  | total < 0 || total > toInteger (maxBound :: Word64) =
      Left $ CapacityDepositInvalidTotal (Coin total)
  | txOut ^. capacityDepositFormTxOutL == ExplicitCapacityDeposit = Right txOut
  | Just deposit <- find isExact candidates = Right $ allocate deposit
  | null candidates =
      Left $ CapacityDepositInsufficient (Coin total) (getCapacityDepositRequirement pp (allocate total))
  | otherwise = Left $ CapacityDepositNoExactAllocation (Coin total)
  where
    total = unCoin $ outputCoins (txOut ^. outputValueTxOutL)
    allocate = allocateFromTotal txOut total
    intervals = allocationIntervals total
    priced =
      [ (lo, hi, unCoin (unCapacityDeposit (getCapacityDepositRequirement pp (allocate lo))))
      | (lo, hi) <- intervals
      ]
    candidates = [max lo required | (lo, hi, required) <- priced, max lo required <= hi]
    isExact deposit = CapacityDeposit (Coin deposit) == getCapacityDepositRequirement pp (allocate deposit)

-- | The uint encoding widths are 1, 2, 3, 5 and 9 bytes. The tariff is constant
-- between each change in the width of the deposit @d@ or application ADA
-- @total-d@. Checking these finitely many intervals is exhaustive; an arbitrary
-- iteration limit can miss a fixpoint or silently return one side of a cycle.
allocationIntervals :: Integer -> [(Integer, Integer)]
allocationIntervals total = zip starts (map (subtract 1) (drop 1 starts))
  where
    thresholds = [24, 256, 65536, 4294967296]
    starts =
      Set.toAscList . Set.fromList $
        0
          : (total + 1)
          : filter (\n -> n > 0 && n <= total) (thresholds ++ map (\n -> total - n + 1) thresholds)

allocateFromTotal :: DijkstraEraTxOut era => TxOut era -> Integer -> Integer -> TxOut era
allocateFromTotal txOut total deposit =
  txOut
    & outputValueTxOutL
      .~ OutputValue
        (CapacityDeposit (Coin deposit))
        ((applicationAssets (txOut ^. outputValueTxOutL)) {applicationCoins = Coin (total - deposit)})
    & capacityDepositFormTxOutL .~ ExplicitCapacityDeposit

-- | Total migration with an explicit exception report. On a width-boundary
-- cycle, choose the smallest conservative allocation that covers its own
-- tariff. If no allocation is affordable, put all available ADA in capacity.
-- Historical outputs in either case are exceptions to the new exact rule;
-- this operation never creates ADA, deletes assets, or reprices explicit stock.
fundCapacityDepositWithReport ::
  DijkstraEraTxOut era =>
  PParams era ->
  TxOut era ->
  (TxOut era, Maybe CapacityDepositAllocationError)
fundCapacityDepositWithReport pp txOut =
  case allocateCapacityDeposit pp txOut of
    Right allocated -> (allocated, Nothing)
    Left err@(CapacityDepositInvalidTotal _) -> (txOut, Just err)
    Left err -> (allocate fallback, Just err)
  where
    total = unCoin $ outputCoins (txOut ^. outputValueTxOutL)
    allocate = allocateFromTotal txOut total
    conservative =
      [ deposit
      | (lo, hi) <- allocationIntervals total
      , let deposit = max lo (unCoin (unCapacityDeposit (getCapacityDepositRequirement pp (allocate lo))))
      , deposit <= hi
      ]
    fallback = maybe total id (listToMaybe conservative)

-- | Allocate outputs entering the UTxO. New outputs have already passed the
-- strict allocation rule. The total fallback also makes this usable for
-- historical migration; callers auditing migration should use the report API.
fundCapacityDeposit :: DijkstraEraTxOut era => PParams era -> TxOut era -> TxOut era
fundCapacityDeposit pp = fst . fundCapacityDepositWithReport pp

translateTxOutWithReport ::
  PParams DijkstraEra ->
  TxOut ConwayEra ->
  (TxOut DijkstraEra, Maybe CapacityDepositAllocationError)
translateTxOutWithReport pp = fundCapacityDepositWithReport pp . upgradeTxOut @DijkstraEra

-- | Structural upgrade followed by parameterized allocation. Historical
-- exceptions preserve funds; use 'translateTxOutWithReport' to inspect them.
translateTxOut :: PParams DijkstraEra -> TxOut ConwayEra -> TxOut DijkstraEra
translateTxOut pp = fst . translateTxOutWithReport pp

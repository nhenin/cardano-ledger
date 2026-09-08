{-# LANGUAGE OverloadedStrings #-}

module Test.Cardano.Ledger.Dijkstra.TxOut.Value.Translation.Fixture (
  NativeEntries,
  AllocationCase (..),
  RejectionCase (..),
  nativeScenarios,
  successfulAllocations,
  allocationsWithSameMaryValue,
  allocationsWithRetainedDeposit,
  negativeRequestedDeposit,
  unfundedAllocations,
) where

import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Dijkstra.TxOut.ApplicationAssets (ApplicationAssets (..))
import Cardano.Ledger.Dijkstra.TxOut.CapacityDeposit (CapacityDeposit (..))
import Cardano.Ledger.Dijkstra.TxOut.Value (OutputValue (..))
import Cardano.Ledger.Dijkstra.TxOut.Value.Translation (AllocationError (..))
import Cardano.Ledger.Hashes (ScriptHash (..))
import Cardano.Ledger.Mary.AssetName (AssetName (..))
import Cardano.Ledger.Mary.MultiAsset (MultiAsset (..))
import Cardano.Ledger.Mary.PolicyID (PolicyID (..))
import Cardano.Ledger.Mary.Value (MaryValue (..))
import qualified Data.Map.Strict as Map

type NativeEntries = Map.Map PolicyID (Map.Map AssetName Integer)

data AllocationCase = AllocationCase
  { allocationSource :: MaryValue
  , allocationDeposit :: CapacityDeposit
  , remainingApplicationCoins :: Coin
  }

data RejectionCase = RejectionCase
  { rejectedSource :: MaryValue
  , rejectedDeposit :: CapacityDeposit
  , expectedRejection :: AllocationError
  }

successfulAllocations :: NativeEntries -> [(String, AllocationCase)]
successfulAllocations native =
  [ ("zero total ADA", allocationCase native 0 0 0)
  , ("zero capacity deposit", allocationCase native 13 0 13)
  , ("partial capacity deposit", allocationCase native 13 4 9)
  , ("all ADA allocated to the capacity deposit", allocationCase native 13 13 0)
  ]

allocationCase :: NativeEntries -> Integer -> Integer -> Integer -> AllocationCase
allocationCase native total requested remaining =
  AllocationCase
    { allocationSource = MaryValue (Coin total) (MultiAsset native)
    , allocationDeposit = CapacityDeposit (Coin requested)
    , remainingApplicationCoins = Coin remaining
    }

allocationsWithSameMaryValue :: NativeEntries -> (OutputValue, OutputValue)
allocationsWithSameMaryValue native =
  ( OutputValue (CapacityDeposit (Coin 4)) (ApplicationAssets (Coin 9) (MultiAsset native))
  , OutputValue (CapacityDeposit (Coin 9)) (ApplicationAssets (Coin 4) (MultiAsset native))
  )

allocationsWithRetainedDeposit :: NativeEntries -> [(String, OutputValue)]
allocationsWithRetainedDeposit native =
  let (first, second) = allocationsWithSameMaryValue native
   in [("the allocation with deposit 4", first), ("the allocation with deposit 9", second)]

negativeRequestedDeposit :: RejectionCase
negativeRequestedDeposit =
  let deposit = CapacityDeposit (Coin (-1))
   in RejectionCase
        { rejectedSource = MaryValue (Coin 10) mempty
        , rejectedDeposit = deposit
        , expectedRejection = NegativeCapacityDeposit deposit
        }

unfundedAllocations :: [(String, RejectionCase)]
unfundedAllocations =
  [ ("a deposit above the available ADA", unfundedAllocation 10 11)
  , ("a negative available ADA balance", unfundedAllocation (-1) 0)
  ]

unfundedAllocation :: Integer -> Integer -> RejectionCase
unfundedAllocation available requested =
  let availableCoins = Coin available
      deposit = CapacityDeposit (Coin requested)
   in RejectionCase
        { rejectedSource = MaryValue availableCoins mempty
        , rejectedDeposit = deposit
        , expectedRejection = CapacityDepositExceedsOutputCoins availableCoins deposit
        }

-- Translation accepts unchecked native maps, including negative and zero
-- quantities and empty policies. These fixtures preserve those exact entries.
nativeScenarios :: [(String, NativeEntries)]
nativeScenarios =
  [ ("ADA only", Map.empty)
  ,
    ( "unchecked native quantities and empty policies"
    , Map.fromList
        [
          ( PolicyID (ScriptHash "00000000000000000000000000000000000000000000000000000000")
          , Map.fromList [(AssetName "token", 100), (AssetName "negative", -3), (AssetName "zero", 0)]
          )
        , (PolicyID (ScriptHash "01010101010101010101010101010101010101010101010101010101"), Map.empty)
        ]
    )
  ]

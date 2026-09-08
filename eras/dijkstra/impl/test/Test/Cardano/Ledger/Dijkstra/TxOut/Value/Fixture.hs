{-# LANGUAGE OverloadedStrings #-}

module Test.Cardano.Ledger.Dijkstra.TxOut.Value.Fixture (
  nativeAssetScenarios,
  coinAllocationCases,
  equalTotalAllocations,
) where

import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Dijkstra.TxOut.ApplicationAssets (ApplicationAssets (..))
import Cardano.Ledger.Dijkstra.TxOut.CapacityDeposit (CapacityDeposit (..))
import Cardano.Ledger.Dijkstra.TxOut.Value (OutputValue (..))
import Cardano.Ledger.Hashes (ScriptHash (..))
import Cardano.Ledger.Mary.AssetName (AssetName (..))
import Cardano.Ledger.Mary.MultiAsset (MultiAsset (..))
import Cardano.Ledger.Mary.PolicyID (PolicyID (..))
import qualified Data.Map.Strict as Map

-- Raw zeros and empty policies deliberately remain unchecked. Native quantities
-- never contribute to the ADA total.
nativeAssetScenarios :: [(String, MultiAsset)]
nativeAssetScenarios =
  [ ("ADA only", mempty)
  ,
    ( "native assets with raw zero and empty entries"
    , MultiAsset $
        Map.fromList
          [
            ( PolicyID (ScriptHash "00000000000000000000000000000000000000000000000000000000")
            , Map.fromList [(AssetName "token", 100), (AssetName "zero", 0)]
            )
          , (PolicyID (ScriptHash "01010101010101010101010101010101010101010101010101010101"), Map.empty)
          ]
    )
  ]

coinAllocationCases :: MultiAsset -> [(String, OutputValue, Coin)]
coinAllocationCases native =
  [ ("counts two zero allocations as zero", outputWithAllocatedCoins native 0 0, Coin 0)
  , ("counts application coins with a zero deposit", outputWithAllocatedCoins native 0 5, Coin 5)
  , ("counts the deposit with zero application coins", outputWithAllocatedCoins native 2 0, Coin 2)
  , ("counts a nonzero deposit and application coins once", outputWithAllocatedCoins native 2 5, Coin 7)
  ]

equalTotalAllocations :: MultiAsset -> (OutputValue, OutputValue)
equalTotalAllocations native =
  (outputWithAllocatedCoins native 2 5, outputWithAllocatedCoins native 3 4)

outputWithAllocatedCoins :: MultiAsset -> Integer -> Integer -> OutputValue
outputWithAllocatedCoins native deposit coins =
  OutputValue (CapacityDeposit (Coin deposit)) (ApplicationAssets (Coin coins) native)

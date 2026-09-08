module Test.Cardano.Ledger.Dijkstra.TxOut.ValueSpec (spec) where

import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Dijkstra.TxOut.Value (OutputValue, outputCoins)
import Test.Cardano.Ledger.Common
import qualified Test.Cardano.Ledger.Dijkstra.TxOut.Value.Fixture as Fixture

spec :: Spec
spec = describe "OutputValue allocation" $
  forM_ Fixture.nativeAssetScenarios $ \(scenario, native) -> describe scenario $ do
    describe "Total ADA" $
      forM_ (Fixture.coinAllocationCases native) $ \(allocationName, value, expected) ->
        it allocationName $ countsBothCoinAllocationsOnce (value, expected)

    it "preserves total ADA when coins move between allocations" $
      allocationsHaveEqualTotalCoins (Fixture.equalTotalAllocations native)
    it "distinguishes allocations with equal total ADA and native assets" $
      equalTotalAllocationsRemainDistinct (Fixture.equalTotalAllocations native)

countsBothCoinAllocationsOnce :: (OutputValue, Coin) -> Expectation
countsBothCoinAllocationsOnce allocationCase = do
  -- Setup
  let (value, expected) = allocationCase
  -- Exercise
  let actual = outputCoins value
  -- Verify
  actual `shouldBe` expected

allocationsHaveEqualTotalCoins :: (OutputValue, OutputValue) -> Expectation
allocationsHaveEqualTotalCoins allocations = do
  -- Setup
  let (first, second) = allocations
  -- Exercise
  let firstTotal = outputCoins first
      secondTotal = outputCoins second
  -- Verify
  firstTotal `shouldBe` secondTotal

equalTotalAllocationsRemainDistinct :: (OutputValue, OutputValue) -> Expectation
equalTotalAllocationsRemainDistinct allocations = do
  -- Setup
  let (first, second) = allocations
  -- Exercise
  let allocationsAreEqual = first == second
  -- Verify
  allocationsAreEqual `shouldBe` False

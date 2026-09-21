module Test.Cardano.Ledger.Dijkstra.TxOut.ValueSpec (spec) where

import Cardano.Ledger.Dijkstra.TxOut.Value (outputCoins)
import Test.Cardano.Ledger.Common
import qualified Test.Cardano.Ledger.Dijkstra.TxOut.Value.Fixture as Fixture

spec :: Spec
spec = describe "OutputValue allocation" $
  forM_ Fixture.nativeAssetScenarios $ \(scenarioName, nativeAssets) -> describe scenarioName $ do
    describe "Total ADA" $
      forM_ (Fixture.coinAllocationCases nativeAssets) $ \(allocationName, outputValue, expectedCoins) ->
        it allocationName $
          outputCoins outputValue `shouldBe` expectedCoins

    let (firstAllocation, secondAllocation) = Fixture.equalTotalAllocations nativeAssets

    it "preserves total ADA when coins move between allocations" $
      outputCoins firstAllocation `shouldBe` outputCoins secondAllocation

    it "distinguishes allocations with equal total ADA and native assets" $
      firstAllocation `shouldNotBe` secondAllocation

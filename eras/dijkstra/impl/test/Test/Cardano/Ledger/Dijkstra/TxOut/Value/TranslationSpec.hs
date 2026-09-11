module Test.Cardano.Ledger.Dijkstra.TxOut.Value.TranslationSpec (spec) where

import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core (coinTxOutL)
import Cardano.Ledger.Dijkstra.TxOut.ApplicationAssets (applicationCoins, nativeAssets)
import Cardano.Ledger.Dijkstra.TxOut.Value (OutputValue (..), outputCoins)
import Cardano.Ledger.Dijkstra.TxOut.Value.Translation (
  AllocationError (..),
  fromMaryOutputValue,
  fromMaryValue,
  toMaryValue,
 )
import Cardano.Ledger.Mary.MultiAsset (MultiAsset (..))
import Cardano.Ledger.Mary.Value (MaryValue (..))
import Lens.Micro ((^.))
import Test.Cardano.Ledger.Common
import qualified Test.Cardano.Ledger.Dijkstra.TxOut.Value.Translation.Fixture as Fixture

spec :: Spec
spec = describe "OutputValue translation" $ do
  forM_ Fixture.nativeScenarios $ \(scenarioName, nativeEntries) -> describe scenarioName $ do
    forM_ (Fixture.successfulAllocations nativeEntries) $ \(allocationName, allocationCase) ->
      describe allocationName $ do
        let sourceMaryValue = Fixture.allocationSource allocationCase
            requestedDeposit = Fixture.allocationDeposit allocationCase
            allocationResult = fromMaryValue requestedDeposit sourceMaryValue

        it "retains the requested capacity deposit" $
          capacityDeposit <$> allocationResult `shouldBe` Right requestedDeposit

        it "assigns the remaining ADA to application assets" $
          (applicationCoins . applicationAssets <$> allocationResult)
            `shouldBe` Right (Fixture.remainingApplicationCoins allocationCase)

        it "preserves total output ADA" $
          outputCoins <$> allocationResult `shouldBe` Right (maryValueCoins sourceMaryValue)

        it "retains exact native entries in application assets" $
          (nativeAssetEntries . nativeAssets . applicationAssets <$> allocationResult)
            `shouldBe` Right (maryValueNativeEntries sourceMaryValue)

        it "preserves total ADA when flattened" $
          (maryValueCoins . toMaryValue <$> allocationResult)
            `shouldBe` Right (maryValueCoins sourceMaryValue)

        it "retains exact native entries when flattened" $
          (maryValueNativeEntries . toMaryValue <$> allocationResult)
            `shouldBe` Right (maryValueNativeEntries sourceMaryValue)

    let (firstAllocation, secondAllocation) = Fixture.allocationsWithSameMaryValue nativeEntries

    it "distinguishes allocations with the same flattened value" $
      firstAllocation `shouldNotBe` secondAllocation

    it "flattens different allocations to the same MaryValue" $
      toMaryValue firstAllocation `shouldBe` toMaryValue secondAllocation

    forM_ (Fixture.allocationsWithRetainedDeposit nativeEntries) $ \(allocationName, outputValue) ->
      it ("round-trips " <> allocationName <> " when its deposit is retained separately") $
        fromMaryValue (capacityDeposit outputValue) (toMaryValue outputValue) `shouldBe` Right outputValue

  it "rejects a negative requested deposit" $ do
    let rejectionCase = Fixture.negativeRequestedDeposit
    fromMaryValue (Fixture.rejectedDeposit rejectionCase) (Fixture.rejectedSource rejectionCase)
      `shouldBe` Left (Fixture.expectedRejection rejectionCase)

  forM_ Fixture.unfundedAllocations $ \(scenarioName, rejectionCase) ->
    it ("reports available and requested ADA for " <> scenarioName) $
      fromMaryValue (Fixture.rejectedDeposit rejectionCase) (Fixture.rejectedSource rejectionCase)
        `shouldBe` Left (Fixture.expectedRejection rejectionCase)

  describe "output policy" $ do
    let protocolParameters = Fixture.pricedPParams 4310
        allocationResult = fromMaryOutputValue protocolParameters Fixture.fundedOutput

    forM_ Fixture.outputSizeScenarios $ \(scenarioName, txOut) ->
      it ("prices the complete encoded output with " <> scenarioName) $
        (capacityDeposit <$> fromMaryOutputValue protocolParameters txOut)
          `shouldBe` Right (Fixture.capacityDepositAtPrice 4310 txOut)

    it "uses the supplied coins-per-byte price" $
      (capacityDeposit <$> fromMaryOutputValue (Fixture.pricedPParams 8620) Fixture.fundedOutput)
        `shouldBe` Right (Fixture.capacityDepositAtPrice 8620 Fixture.fundedOutput)

    it "assigns ADA beyond the required deposit to application assets" $
      (applicationCoins . applicationAssets <$> allocationResult)
        `shouldBe` Right Fixture.fundedApplicationCoins

    it "preserves the total output ADA after allocating its required deposit" $
      outputCoins <$> allocationResult `shouldBe` Right (maryValueCoins Fixture.fundedMaryValue)

    it "retains exact native entries when allocating the required deposit" $
      (nativeAssetEntries . nativeAssets . applicationAssets <$> allocationResult)
        `shouldBe` Right (maryValueNativeEntries Fixture.fundedMaryValue)

    it "accepts an output whose total ADA exactly funds its deposit" $
      ( applicationCoins . applicationAssets
          <$> fromMaryOutputValue protocolParameters Fixture.exactlyFundedOutput
      )
        `shouldBe` Right (Coin 0)

    it "reports available ADA and the required deposit for an underfunded output" $ do
      let txOut = Fixture.underfundedOutput
      fromMaryOutputValue protocolParameters txOut
        `shouldBe` Left
          ( CapacityDepositExceedsOutputCoins
              (txOut ^. coinTxOutL)
              (Fixture.capacityDepositAtPrice 4310 txOut)
          )

    it "leaves all ADA available to the application when the capacity price is zero" $
      ( applicationCoins . applicationAssets
          <$> fromMaryOutputValue (Fixture.pricedPParams 0) Fixture.fundedOutput
      )
        `shouldBe` Right (maryValueCoins Fixture.fundedMaryValue)

    it "also derives the deposit from a Conway output and its era policy" $
      (capacityDeposit <$> fromMaryOutputValue (Fixture.pricedPParams 4310) Fixture.conwayOutput)
        `shouldBe` Right (Fixture.capacityDepositAtPrice 4310 Fixture.conwayOutput)

maryValueCoins :: MaryValue -> Coin
maryValueCoins (MaryValue coins _) = coins

maryValueNativeEntries :: MaryValue -> Fixture.NativeEntries
maryValueNativeEntries (MaryValue _ assets) = nativeAssetEntries assets

-- Raw Map equality observes zeros and empty policies that pointwise value
-- equality can ignore.
nativeAssetEntries :: MultiAsset -> Fixture.NativeEntries
nativeAssetEntries (MultiAsset entries) = entries

module Test.Cardano.Ledger.Dijkstra.TxOut.Value.TranslationSpec (spec) where

import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Dijkstra.TxOut.ApplicationAssets (ApplicationAssets (..))
import Cardano.Ledger.Dijkstra.TxOut.Value (OutputValue (..), outputCoins)
import Cardano.Ledger.Dijkstra.TxOut.Value.Translation (fromMaryValue, toMaryValue)
import Cardano.Ledger.Mary.MultiAsset (MultiAsset (..))
import Cardano.Ledger.Mary.Value (MaryValue (..))
import Test.Cardano.Ledger.Common
import qualified Test.Cardano.Ledger.Dijkstra.TxOut.Value.Translation.Fixture as Fixture

spec :: Spec
spec = describe "OutputValue translation" $ do
  forM_ Fixture.nativeScenarios $ \(scenario, native) -> describe scenario $ do
    forM_ (Fixture.successfulAllocations native) $ \(allocation, fixture) ->
      describe allocation $ do
        it "retains the requested capacity deposit" $ retainsRequestedDeposit fixture
        it "assigns the remaining ADA to application assets" $ assignsRemainingApplicationCoins fixture
        it "preserves total output ADA" $ preservesOutputCoins fixture
        it "retains exact native entries in application assets" $ retainsApplicationNativeEntries fixture
        it "preserves total ADA when flattened" $ preservesFlattenedCoins fixture
        it "retains exact native entries when flattened" $ retainsFlattenedNativeEntries fixture

    it "distinguishes allocations with the same flattened value" $
      distinguishesAllocations (Fixture.allocationsWithSameMaryValue native)
    it "flattens different allocations to the same MaryValue" $
      flattensAllocationsToSameValue (Fixture.allocationsWithSameMaryValue native)
    forM_ (Fixture.allocationsWithRetainedDeposit native) $ \(allocation, value) ->
      it ("round-trips " <> allocation <> " when its deposit is retained separately") $
        roundTripsWithRetainedDeposit value

  it "rejects a negative requested deposit" $
    reportsAllocationRejection Fixture.negativeRequestedDeposit
  forM_ Fixture.unfundedAllocations $ \(scenario, fixture) ->
    it ("reports available and requested ADA for " <> scenario) $
      reportsAllocationRejection fixture

retainsRequestedDeposit :: Fixture.AllocationCase -> Expectation
retainsRequestedDeposit fixture = do
  -- Setup
  let source = Fixture.allocationSource fixture
      deposit = Fixture.allocationDeposit fixture
  -- Exercise
  let actual = capacityDeposit <$> fromMaryValue deposit source
  -- Verify
  actual `shouldBe` Right deposit

assignsRemainingApplicationCoins :: Fixture.AllocationCase -> Expectation
assignsRemainingApplicationCoins fixture = do
  -- Setup
  let source = Fixture.allocationSource fixture
      deposit = Fixture.allocationDeposit fixture
      expected = Fixture.remainingApplicationCoins fixture
  -- Exercise
  let actual = applicationCoins . applicationAssets <$> fromMaryValue deposit source
  -- Verify
  actual `shouldBe` Right expected

preservesOutputCoins :: Fixture.AllocationCase -> Expectation
preservesOutputCoins fixture = do
  -- Setup
  let source = Fixture.allocationSource fixture
      deposit = Fixture.allocationDeposit fixture
      expected = maryCoins source
  -- Exercise
  let actual = outputCoins <$> fromMaryValue deposit source
  -- Verify
  actual `shouldBe` Right expected

retainsApplicationNativeEntries :: Fixture.AllocationCase -> Expectation
retainsApplicationNativeEntries fixture = do
  -- Setup
  let source = Fixture.allocationSource fixture
      deposit = Fixture.allocationDeposit fixture
      expected = maryNativeEntries source
  -- Exercise
  let actual = nativeEntries . nativeAssets . applicationAssets <$> fromMaryValue deposit source
  -- Verify
  actual `shouldBe` Right expected

preservesFlattenedCoins :: Fixture.AllocationCase -> Expectation
preservesFlattenedCoins fixture = do
  -- Setup
  let source = Fixture.allocationSource fixture
      deposit = Fixture.allocationDeposit fixture
      expected = maryCoins source
  -- Exercise
  let actual = maryCoins . toMaryValue <$> fromMaryValue deposit source
  -- Verify
  actual `shouldBe` Right expected

retainsFlattenedNativeEntries :: Fixture.AllocationCase -> Expectation
retainsFlattenedNativeEntries fixture = do
  -- Setup
  let source = Fixture.allocationSource fixture
      deposit = Fixture.allocationDeposit fixture
      expected = maryNativeEntries source
  -- Exercise
  let actual = maryNativeEntries . toMaryValue <$> fromMaryValue deposit source
  -- Verify
  actual `shouldBe` Right expected

distinguishesAllocations :: (OutputValue, OutputValue) -> Expectation
distinguishesAllocations allocations = do
  -- Setup
  let (first, second) = allocations
  -- Exercise
  let allocationsDiffer = first /= second
  -- Verify
  allocationsDiffer `shouldBe` True

flattensAllocationsToSameValue :: (OutputValue, OutputValue) -> Expectation
flattensAllocationsToSameValue allocations = do
  -- Setup
  let (first, second) = allocations
  -- Exercise
  let firstFlattened = toMaryValue first
      secondFlattened = toMaryValue second
  -- Verify
  firstFlattened `shouldBe` secondFlattened

roundTripsWithRetainedDeposit :: OutputValue -> Expectation
roundTripsWithRetainedDeposit value = do
  -- Setup
  let retainedDeposit = capacityDeposit value
  -- Exercise
  let actual = fromMaryValue retainedDeposit (toMaryValue value)
  -- Verify
  actual `shouldBe` Right value

reportsAllocationRejection :: Fixture.RejectionCase -> Expectation
reportsAllocationRejection fixture = do
  -- Setup
  let source = Fixture.rejectedSource fixture
      deposit = Fixture.rejectedDeposit fixture
      expected = Fixture.expectedRejection fixture
  -- Exercise
  let actual = fromMaryValue deposit source
  -- Verify
  actual `shouldBe` Left expected

maryCoins :: MaryValue -> Coin
maryCoins (MaryValue coins _) = coins

maryNativeEntries :: MaryValue -> Fixture.NativeEntries
maryNativeEntries (MaryValue _ assets) = nativeEntries assets

-- Raw Map equality observes zeros and empty policies that pointwise value
-- equality can ignore.
nativeEntries :: MultiAsset -> Fixture.NativeEntries
nativeEntries (MultiAsset entries) = entries

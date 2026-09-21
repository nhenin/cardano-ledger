module Test.Cardano.Ledger.Dijkstra.Transition.InitialFundsSpec (spec) where

import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Dijkstra.Transition.InitialFunds (
  InitialFundsAllocationError (..),
  allocateInitialFunds,
 )
import Cardano.Ledger.Dijkstra.TxOut.ApplicationAssets (applicationCoins)
import Cardano.Ledger.Dijkstra.TxOut.CapacityDeposit (CapacityDeposit (..))
import Cardano.Ledger.Dijkstra.TxOut.Value (OutputValue (..), outputCoins)
import Test.Cardano.Ledger.Common
import qualified Test.Cardano.Ledger.Dijkstra.Transition.InitialFunds.Fixture as Fixture

spec :: Spec
spec = describe "Dijkstra initial funds allocation" $ do
  forM_ Fixture.fundedCases $ \(scenarioName, funding) -> describe scenarioName $ do
    let allocationResult = allocateFunds funding

    it "preserves the total initial ADA" $
      outputCoins <$> allocationResult `shouldBe` Right (Fixture.amount funding)

    it "covers the minimum coin of the final output" $
      (\allocation -> capacityDeposit allocation >= Fixture.minimumDeposit funding allocation)
        <$> allocationResult
          `shouldBe` Right True

  describe "smallest sufficient deposit, checked by exhaustive search" $
    forM_ Fixture.smallFundingCases $ \(scenarioName, funding) ->
      it scenarioName $
        capacityDeposit
          <$> allocateFunds funding
            `shouldBe` maybe
              (Left (InsufficientInitialFunds (Fixture.amount funding)))
              Right
              (Fixture.firstSufficientDeposit funding)

  it "accepts one lovelace of surplus when exact equality has no solution" $
    capacityDeposit
      <$> allocateFunds Fixture.oneLovelaceSurplus
        `shouldBe` Right (CapacityDeposit (Coin 228))

  it "leaves the remaining 255 coins with the application" $
    (applicationCoins . applicationAssets <$> allocateFunds Fixture.oneLovelaceSurplus)
      `shouldBe` Right (Coin 255)

  it "finds the first sufficient deposit after application coin encoding shrinks" $
    capacityDeposit
      <$> allocateFunds Fixture.applicationEncodingBoundary
        `shouldBe` Right (CapacityDeposit (Coin 995611))

  it "prices the final output below its chosen deposit at the encoding boundary" $
    Fixture.minimumDeposit Fixture.applicationEncodingBoundary
      <$> allocateFunds Fixture.applicationEncodingBoundary
        `shouldBe` Right (CapacityDeposit (Coin 995610))

  it "finds the first sufficient deposit when application coins fall below 2^32" $
    capacityDeposit
      <$> allocateFunds Fixture.applicationCoinsAt32BitBoundary
        `shouldBe` Right (CapacityDeposit (Coin 1004231))

  it "accepts funding even when zero-deposit and all-deposit estimates are too high" $
    capacityDeposit
      <$> allocateFunds Fixture.fundingBelowEndpointEstimates
        `shouldBe` Right (CapacityDeposit (Coin 65436))

  forM_ Fixture.zeroPriceFunds $ \(scenarioName, funding) -> describe ("zero price with " <> scenarioName) $ do
    it "requires no capacity deposit" $
      capacityDeposit <$> allocateFunds funding `shouldBe` Right (CapacityDeposit (Coin 0))

    it "keeps all initial ADA in application assets" $
      (applicationCoins . applicationAssets <$> allocateFunds funding)
        `shouldBe` Right (Fixture.amount funding)

  forM_ Fixture.underfundedCases $ \(scenarioName, funding) ->
    it ("rejects " <> scenarioName) $
      allocateFunds funding `shouldBe` Left (InsufficientInitialFunds (Fixture.amount funding))

  forM_ Fixture.outsideCoinRange $ \(scenarioName, funding) ->
    it ("rejects " <> scenarioName <> " before constructing an output") $
      allocateFunds funding `shouldBe` Left (InitialFundsOutsideCoinRange (Fixture.amount funding))

-- Private helpers

allocateFunds :: Fixture.FundingCase -> Either InitialFundsAllocationError OutputValue
allocateFunds funding =
  allocateInitialFunds
    (Fixture.protocolParameters funding)
    (Fixture.address funding)
    (Fixture.amount funding)

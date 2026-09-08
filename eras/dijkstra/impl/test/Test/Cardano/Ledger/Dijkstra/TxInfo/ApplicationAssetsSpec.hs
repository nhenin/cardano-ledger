module Test.Cardano.Ledger.Dijkstra.TxInfo.ApplicationAssetsSpec (spec) where

import Cardano.Ledger.Dijkstra.TxInfo (DijkstraContextError (..))
import Cardano.Ledger.Dijkstra.TxOut.Translation (allocateCapacityDeposit)
import Cardano.Ledger.Plutus (Language (..), TxOutSource (..))
import Test.Cardano.Ledger.Common
import qualified Test.Cardano.Ledger.Dijkstra.TxInfo.ApplicationAssets.Fixture as Fixture
import qualified Test.Cardano.Ledger.Dijkstra.TxOut.Translation.Fixture as Output

spec :: Spec
spec = describe "Dijkstra Plutus application assets" $ do
  mapM_ languageSpec [PlutusV1, PlutusV2, PlutusV3, PlutusV4]
  it "projects application assets in a V4 sub-transaction" subTransactionProjectsApplicationAssets
  it "preserves the original V4 sub-transaction ID" subTransactionPreservesId
  mapM_ referenceSpec [PlutusV2, PlutusV3, PlutusV4]

languageSpec :: Language -> Spec
languageSpec lang = describe (show lang) $ do
  it "exposes the same application assets at implicit creation and stored spending" $
    implicitCreationMatchesSpending lang
  it "exposes the same application assets at explicit creation and spending" $
    explicitCreationMatchesSpending lang
  it "allocates an implicit resolved input using current parameters" $
    implicitInputUsesParameters lang
  it "uses the current capacity price when projecting implicit outputs" $
    implicitOutputUsesCurrentPrice lang
  it "keeps stored explicit application assets when the capacity price changes" $
    explicitInputRetainsAllocation lang
  it "preserves the signed body's transaction ID" $ projectionPreservesId lang
  it "rejects an implicit output whose capacity cannot be allocated" $
    invalidImplicitOutputIsRejected lang
  it "rejects price-free projection of an implicit output" $
    implicitOutputRequiresParameters lang
  it "projects an explicit output without needing protocol parameters" $
    explicitOutputNeedsNoParameters lang

implicitCreationMatchesSpending :: Language -> Expectation
implicitCreationMatchesSpending lang = do
  -- Setup
  allocated <- expectRight $ allocateCapacityDeposit Output.pricedPParams Output.implicitOutput
  let expected = [Fixture.expectedApplicationValue allocated]
  -- Exercise
  creation <-
    expectRight $
      Fixture.project lang Output.pricedPParams (Fixture.creationContext Output.implicitOutput)
  spending <-
    expectRight $ Fixture.project lang Output.pricedPParams (Fixture.spendingContext allocated)
  -- Verify
  (Fixture.projectedOutputs creation, Fixture.projectedInputs spending)
    `shouldBe` (expected, expected)

explicitCreationMatchesSpending :: Language -> Expectation
explicitCreationMatchesSpending lang = do
  -- Setup
  let explicit = Output.explicitOutput
      expected = [Fixture.expectedApplicationValue explicit]
  -- Exercise
  creation <-
    expectRight $ Fixture.project lang Output.pricedPParams (Fixture.creationContext explicit)
  spending <-
    expectRight $ Fixture.project lang Output.pricedPParams (Fixture.spendingContext explicit)
  -- Verify
  (Fixture.projectedOutputs creation, Fixture.projectedInputs spending)
    `shouldBe` (expected, expected)

implicitInputUsesParameters :: Language -> Expectation
implicitInputUsesParameters lang = do
  -- Setup
  allocated <- expectRight $ allocateCapacityDeposit Output.pricedPParams Output.implicitOutput
  -- Exercise
  spending <-
    expectRight $
      Fixture.project lang Output.pricedPParams (Fixture.spendingContext Output.implicitOutput)
  -- Verify
  Fixture.projectedInputs spending `shouldBe` [Fixture.expectedApplicationValue allocated]

implicitOutputUsesCurrentPrice :: Language -> Expectation
implicitOutputUsesCurrentPrice lang = do
  -- Setup
  cheap <- expectRight $ allocateCapacityDeposit Output.unitPricePParams Output.implicitOutput
  expensive <- expectRight $ allocateCapacityDeposit Output.pricedPParams Output.implicitOutput
  let source = Fixture.creationContext Output.implicitOutput
  -- Exercise
  cheapProjection <- expectRight $ Fixture.project lang Output.unitPricePParams source
  expensiveProjection <- expectRight $ Fixture.project lang Output.pricedPParams source
  -- Verify
  (Fixture.projectedOutputs cheapProjection, Fixture.projectedOutputs expensiveProjection)
    `shouldBe` ([Fixture.expectedApplicationValue cheap], [Fixture.expectedApplicationValue expensive])

explicitInputRetainsAllocation :: Language -> Expectation
explicitInputRetainsAllocation lang = do
  -- Setup
  allocated <- expectRight $ allocateCapacityDeposit Output.pricedPParams Output.implicitOutput
  -- Exercise
  spending <-
    expectRight $ Fixture.project lang Output.unitPricePParams (Fixture.spendingContext allocated)
  -- Verify
  Fixture.projectedInputs spending `shouldBe` [Fixture.expectedApplicationValue allocated]

projectionPreservesId :: Language -> Expectation
projectionPreservesId lang = do
  -- Setup
  let source = Fixture.creationContext Output.implicitOutput
  -- Exercise
  projected <- expectRight $ Fixture.project lang Output.pricedPParams source
  -- Verify
  Fixture.projectedTxId projected `shouldBe` Fixture.originalTxId source

invalidImplicitOutputIsRejected :: Language -> Expectation
invalidImplicitOutputIsRejected lang = do
  -- Setup
  allocationError <-
    expectLeft $ allocateCapacityDeposit Output.pricedPParams Output.underfundedOutput
  let source = Fixture.creationContext Output.underfundedOutput
  -- Exercise
  let projected = Fixture.project lang Output.pricedPParams source
  -- Verify
  projected
    `shouldBeLeft` CannotAllocateOutputCapacityDeposit (TxOutFromOutput minBound) allocationError

implicitOutputRequiresParameters :: Language -> Expectation
implicitOutputRequiresParameters lang = do
  -- Setup
  let source = Fixture.creationContext Output.implicitOutput
  -- Exercise
  let projected = Fixture.projectWithoutParameters lang source
  -- Verify
  projected `shouldBeLeft` MissingCapacityDepositParameters (TxOutFromOutput minBound)

explicitOutputNeedsNoParameters :: Language -> Expectation
explicitOutputNeedsNoParameters lang = do
  -- Setup
  let source = Fixture.creationContext Output.explicitOutput
  -- Exercise
  projected <- expectRight $ Fixture.projectWithoutParameters lang source
  -- Verify
  Fixture.projectedOutputs projected
    `shouldBe` [Fixture.expectedApplicationValue Output.explicitOutput]

subTransactionProjectsApplicationAssets :: Expectation
subTransactionProjectsApplicationAssets = do
  -- Setup
  allocated <- expectRight $ allocateCapacityDeposit Output.pricedPParams Output.implicitOutput
  let source = Fixture.subCreationContext Output.implicitOutput
  -- Exercise
  projected <- expectRight $ Fixture.project PlutusV4 Output.pricedPParams source
  -- Verify
  Fixture.projectedOutputs projected `shouldBe` [Fixture.expectedApplicationValue allocated]

subTransactionPreservesId :: Expectation
subTransactionPreservesId = do
  -- Setup
  let source = Fixture.subCreationContext Output.implicitOutput
  -- Exercise
  projected <- expectRight $ Fixture.project PlutusV4 Output.pricedPParams source
  -- Verify
  Fixture.projectedTxId projected `shouldBe` Fixture.originalTxId source

referenceSpec :: Language -> Spec
referenceSpec lang = it (show lang ++ " exposes application assets in a reference input") $ do
  -- Setup
  allocated <- expectRight $ allocateCapacityDeposit Output.pricedPParams Output.implicitOutput
  -- Exercise
  projected <-
    expectRight $ Fixture.project lang Output.pricedPParams (Fixture.referenceContext allocated)
  -- Verify
  Fixture.projectedReferenceInputs projected `shouldBe` [Fixture.expectedApplicationValue allocated]

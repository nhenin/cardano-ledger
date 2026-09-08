{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Dijkstra.TxOut.TranslationSpec (spec) where

import Cardano.Ledger.Binary (serialize)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core (eraProtVerLow, txIdTxBody)
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Dijkstra.TxOut (CapacityDepositForm (..), DijkstraEraTxOut (..))
import Cardano.Ledger.Dijkstra.TxOut.ApplicationAssets (ApplicationAssets (..))
import Cardano.Ledger.Dijkstra.TxOut.CapacityDeposit (CapacityDeposit (..))
import Cardano.Ledger.Dijkstra.TxOut.Translation (
  CapacityDepositAllocationError (..),
  allocateCapacityDeposit,
  fundCapacityDeposit,
  fundCapacityDepositWithReport,
 )
import Cardano.Ledger.Dijkstra.TxOut.Value (OutputValue (..), outputCoins)
import Cardano.Ledger.Dijkstra.UTxO.Translation (fundCapacityDeposits)
import Cardano.Ledger.State (UTxO (..), txouts)
import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map
import Lens.Micro ((&), (.~), (^.))
import Test.Cardano.Ledger.Common hiding (output)
import qualified Test.Cardano.Ledger.Dijkstra.TxOut.Translation.Fixture as Fixture

spec :: Spec
spec = describe "Dijkstra output allocation boundary" $ do
  it "decodes Conway wire bytes as an implicit output" legacyWireDecodesImplicit
  it "preserves total ADA during allocation" allocationPreservesTotalAda
  it "preserves native assets during allocation" allocationPreservesNativeAssets
  it "stores an allocated legacy output in explicit form" allocationStoresExplicitForm
  it "funds an implicit output at the exact measured requirement" allocationMeetsExactRequirement
  it "leaves an explicit allocation unchanged" explicitAllocationIsUnchanged
  it "preserves all ADA of an underfunded historical output" historicalUnderfundingPreservesAda
  it "reports underfunded historical allocation" historicalUnderfundingIsReported
  it "rejects an underfunded newly created implicit output" strictAllocationRejectsUnderfunding
  it "terminates and preserves an empty historical output" zeroHistoricalOutputTerminates
  it "rejects a new implicit output with no exact fixed point" strictAllocationRejectsWidthCycle
  it "reports a historical encoding-width cycle" historicalWidthCycleIsReported
  it
    "preserves total ADA through the historical width-cycle fallback"
    historicalWidthCyclePreservesAda
  it "conservatively funds the historical width-cycle fallback" historicalWidthCycleCoversRequirement
  it "rejects the width-cycle output in output validation" validationRejectsWidthCycle
  it "rejects an explicit zero deposit at a positive price" validationRejectsExplicitZero
  it "terminates with an exact zero deposit at a zero price" zeroPriceAllocationTerminates
  it
    "preserves the translated transaction body's original hash at UTxO entry"
    utxoEntryPreservesBodyHash
  it "preserves the translated transaction body's signed bytes" translatedBodyRetainsSignedBytes

legacyWireDecodesImplicit :: Expectation
legacyWireDecodesImplicit = do
  -- Setup
  let encodedConwayOutput = Fixture.decodedConwayOutput
  -- Exercise
  decoded <- expectRight encodedConwayOutput
  -- Verify
  decoded ^. capacityDepositFormTxOutL `shouldBe` ImplicitCapacityDeposit

allocationPreservesTotalAda :: Expectation
allocationPreservesTotalAda = do
  -- Setup
  source <- expectRight Fixture.decodedConwayOutput
  -- Exercise
  allocated <- expectRight $ allocateCapacityDeposit Fixture.pricedPParams source
  -- Verify
  outputCoins (allocated ^. outputValueTxOutL) `shouldBe` outputCoins (source ^. outputValueTxOutL)

allocationPreservesNativeAssets :: Expectation
allocationPreservesNativeAssets = do
  -- Setup
  source <- expectRight Fixture.decodedConwayOutput
  -- Exercise
  allocated <- expectRight $ allocateCapacityDeposit Fixture.pricedPParams source
  -- Verify
  nativeAssets (applicationAssets (allocated ^. outputValueTxOutL))
    `shouldBe` nativeAssets (applicationAssets (source ^. outputValueTxOutL))

allocationStoresExplicitForm :: Expectation
allocationStoresExplicitForm = do
  -- Setup
  let source = Fixture.implicitOutput
  -- Exercise
  allocated <- expectRight $ allocateCapacityDeposit Fixture.pricedPParams source
  -- Verify
  allocated ^. capacityDepositFormTxOutL `shouldBe` ExplicitCapacityDeposit

allocationMeetsExactRequirement :: Expectation
allocationMeetsExactRequirement = do
  -- Setup
  let pp = Fixture.pricedPParams
  -- Exercise
  allocated <- expectRight $ allocateCapacityDeposit pp Fixture.implicitOutput
  -- Verify
  allocated ^. capacityDepositTxOutL `shouldBe` getCapacityDepositRequirement pp allocated

explicitAllocationIsUnchanged :: Expectation
explicitAllocationIsUnchanged = do
  -- Setup
  let source = Fixture.explicitOutput
  -- Exercise
  let allocated = fundCapacityDeposit Fixture.pricedPParams source
  -- Verify
  allocated `shouldBe` source

historicalUnderfundingPreservesAda :: Expectation
historicalUnderfundingPreservesAda = do
  -- Setup
  let source = Fixture.underfundedOutput
  -- Exercise
  let (allocated, _) = fundCapacityDepositWithReport Fixture.pricedPParams source
  -- Verify
  outputCoins (allocated ^. outputValueTxOutL) `shouldBe` outputCoins (source ^. outputValueTxOutL)

historicalUnderfundingIsReported :: Expectation
historicalUnderfundingIsReported = do
  -- Setup
  let source = Fixture.underfundedOutput
  -- Exercise
  let (_, report) = fundCapacityDepositWithReport Fixture.pricedPParams source
  -- Verify
  case report of
    Just (CapacityDepositInsufficient (Coin 1) _) -> pure ()
    other -> expectationFailure $ "Expected an underfunded migration report, got " ++ show other

strictAllocationRejectsUnderfunding :: Expectation
strictAllocationRejectsUnderfunding = do
  -- Setup
  let source = Fixture.underfundedOutput
  -- Exercise
  let result = allocateCapacityDeposit Fixture.pricedPParams source
  -- Verify
  case result of
    Left (CapacityDepositInsufficient (Coin 1) _) -> pure ()
    other -> expectationFailure $ "Expected strict allocation to reject underfunding, got " ++ show other

zeroHistoricalOutputTerminates :: Expectation
zeroHistoricalOutputTerminates = do
  -- Setup
  let source = Fixture.zeroCoinOutput
  -- Exercise
  let (allocated, _) = fundCapacityDepositWithReport Fixture.pricedPParams source
  -- Verify
  allocated
    ^. outputValueTxOutL
      `shouldBe` OutputValue (CapacityDeposit (Coin 0)) (ApplicationAssets (Coin 0) mempty)

strictAllocationRejectsWidthCycle :: Expectation
strictAllocationRejectsWidthCycle = do
  -- Setup
  source <- expectJust Fixture.noExactImplicitOutput
  -- Exercise
  let actual = allocateCapacityDeposit Fixture.unitPricePParams source
  -- Verify
  actual
    `shouldBe` Left (CapacityDepositNoExactAllocation (outputCoins (source ^. outputValueTxOutL)))

historicalWidthCycleIsReported :: Expectation
historicalWidthCycleIsReported = do
  -- Setup
  source <- expectJust Fixture.noExactImplicitOutput
  -- Exercise
  let (_, report) = fundCapacityDepositWithReport Fixture.unitPricePParams source
  -- Verify
  report
    `shouldBe` Just (CapacityDepositNoExactAllocation (outputCoins (source ^. outputValueTxOutL)))

historicalWidthCyclePreservesAda :: Expectation
historicalWidthCyclePreservesAda = do
  -- Setup
  source <- expectJust Fixture.noExactImplicitOutput
  -- Exercise
  let (allocated, _) = fundCapacityDepositWithReport Fixture.unitPricePParams source
  -- Verify
  outputCoins (allocated ^. outputValueTxOutL) `shouldBe` outputCoins (source ^. outputValueTxOutL)

historicalWidthCycleCoversRequirement :: Expectation
historicalWidthCycleCoversRequirement = do
  -- Setup
  source <- expectJust Fixture.noExactImplicitOutput
  -- Exercise
  let (allocated, _) = fundCapacityDepositWithReport Fixture.unitPricePParams source
  -- Verify
  allocated
    ^. capacityDepositTxOutL
      `shouldSatisfy` (>= getCapacityDepositRequirement Fixture.unitPricePParams allocated)

validationRejectsWidthCycle :: Expectation
validationRejectsWidthCycle = do
  -- Setup
  source <- expectJust Fixture.noExactImplicitOutput
  -- Exercise
  let result = Fixture.validateOutput Fixture.unitPricePParams source
  -- Verify
  failures <- expectLeft result
  Foldable.toList failures `shouldContain` [Fixture.AllocationFailure]

validationRejectsExplicitZero :: Expectation
validationRejectsExplicitZero = do
  -- Setup
  let source = Fixture.explicitOutput & capacityDepositTxOutL .~ CapacityDeposit (Coin 0)
  -- Exercise
  let result = Fixture.validateOutput Fixture.pricedPParams source
  -- Verify
  failures <- expectLeft result
  Foldable.toList failures `shouldContain` [Fixture.ExplicitMismatch]

zeroPriceAllocationTerminates :: Expectation
zeroPriceAllocationTerminates = do
  -- Setup
  let source = Fixture.implicitOutput
  -- Exercise
  allocated <- expectRight $ allocateCapacityDeposit Fixture.zeroPricePParams source
  -- Verify
  Fixture.validateOutput Fixture.zeroPricePParams allocated `shouldBe` Right ()

utxoEntryPreservesBodyHash :: Expectation
utxoEntryPreservesBodyHash = do
  -- Setup
  translatedBody <- expectRight Fixture.decodedConwayBody
  let originalId = txIdTxBody Fixture.conwayBody
  -- Exercise
  let UTxO storedOutputs = fundCapacityDeposits Fixture.pricedPParams (txouts translatedBody)
  all
    (\output -> output ^. capacityDepositFormTxOutL == ExplicitCapacityDeposit)
    (Map.elems storedOutputs)
    `shouldBe` True
  -- Verify
  txIdTxBody translatedBody `shouldBe` originalId

translatedBodyRetainsSignedBytes :: Expectation
translatedBodyRetainsSignedBytes = do
  -- Setup
  translatedBody <- expectRight Fixture.decodedConwayBody
  -- Exercise
  let afterDecode = serialize (eraProtVerLow @DijkstraEra) translatedBody
  -- Verify
  afterDecode `shouldBe` Fixture.conwayBodyBytes

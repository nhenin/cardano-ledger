{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Dijkstra.TxOut.AccountingSpec (spec) where

import Cardano.Ledger.Coin (Coin (..), DeltaCoin (..))
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.Core (capacityDepositTxOutL, potValueTxOutF)
import Cardano.Ledger.Dijkstra.Rules (
  DijkstraUtxoPredFailure (..),
  collateralPotBalance,
  updateDijkstraUTxOAndInstantStake,
  updateDijkstraUTxOStatePhase2Invalid,
  validateTotalCollateral,
 )
import Cardano.Ledger.Dijkstra.TxOut.CapacityDeposit (CapacityDeposit (..))
import Cardano.Ledger.Dijkstra.UTxO (dijkstraConsumed, localProducedValue)
import Cardano.Ledger.Mary.Value (MaryValue (..))
import Cardano.Ledger.Shelley.LedgerState (UTxOState (..))
import Cardano.Ledger.State (EraUTxO (getProducedValue), UTxO (..))
import Data.Foldable (foldMap')
import Data.Functor.Identity (runIdentity)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import Lens.Micro ((^.))
import Test.Cardano.Ledger.Common
import qualified Test.Cardano.Ledger.Dijkstra.TxOut.Accounting.Fixture as Fixture
import Validation (validationToEither)

spec :: Spec
spec = describe "Dijkstra capacity accounting" $ do
  it "consumes the input application value and its capacity deposit" consumesInputPots
  it "produces output capacity alongside application value and fees" producesOutputPots
  it "counts capacity in a subtransaction's local produced value" producesSubtransactionPots
  it "includes subtransaction output capacity in the batch balance" producesBatchPots
  it "computes collateral from input and return total ADA" balancesCollateralPots
  it "accepts returned native assets with the complete collateral amount" acceptsReturnedNativeAssets
  it
    "rejects a totalCollateral field that counts only application ADA"
    rejectsApplicationOnlyCollateral
  it "rejects collateral that fails to return all native assets" rejectsNativeAssetLoss
  it "credits released capacity to the phase-2-invalid fee pot" creditsInvalidFees
  it "keeps the collateral return's capacity and native assets in the UTxO" preservesCollateralReturn
  it "removes the consumed collateral inputs" removesCollateralInputs
  it "allocates implicit top-level outputs when they enter the UTxO" fundsTopLevelOutputs
  it "allocates implicit subtransaction outputs when they enter the UTxO" fundsSubtransactionOutputs
  it
    "preserves the total fee credit when allocating an implicit collateral return"
    creditsImplicitReturnFees
  it "stores implicit collateral returns with a funded capacity deposit" fundsCollateralReturn

consumesInputPots :: Expectation
consumesInputPots = do
  -- Setup
  let inputs = Fixture.ordinaryInputs
      body = Fixture.ordinaryBody
  -- Exercise
  let actual = dijkstraConsumed Fixture.pparams inputs body
  -- Verify
  actual `shouldBe` MaryValue (Coin 10) Fixture.tokens

producesOutputPots :: Expectation
producesOutputPots = do
  -- Setup
  let body = Fixture.ordinaryBody
  -- Exercise
  let actual = getProducedValue Fixture.pparams (const False) body
  -- Verify
  actual `shouldBe` MaryValue (Coin 10) Fixture.tokens

producesSubtransactionPots :: Expectation
producesSubtransactionPots = do
  -- Setup
  let body = Fixture.subBody
  -- Exercise
  let actual = localProducedValue Fixture.pparams body
  -- Verify
  actual `shouldBe` MaryValue (Coin 10) Fixture.tokens

producesBatchPots :: Expectation
producesBatchPots = do
  -- Setup
  let body = Fixture.batchBody
  -- Exercise
  let actual = getProducedValue Fixture.pparams (const False) body
  -- Verify
  actual `shouldBe` MaryValue (Coin 20) (Fixture.tokens <> Fixture.tokens)

balancesCollateralPots :: Expectation
balancesCollateralPots = do
  -- Setup
  let body = Fixture.collateralBody
      inputs = Fixture.collateralInputs
  -- Exercise
  let actual = collateralPotBalance body inputs
  -- Verify
  actual `shouldBe` DeltaCoin 4

acceptsReturnedNativeAssets :: Expectation
acceptsReturnedNativeAssets = do
  -- Setup
  let body = Fixture.collateralBody
  -- Exercise
  let actual = validateTotalCollateral @DijkstraEra @"UTXO" Fixture.pparams body Fixture.collateralInputs
  -- Verify
  validationToEither actual `shouldBe` Right ()

rejectsApplicationOnlyCollateral :: Expectation
rejectsApplicationOnlyCollateral = do
  -- Setup
  let body = Fixture.applicationOnlyCollateralBody
  -- Exercise
  let actual = validateTotalCollateral @DijkstraEra @"UTXO" Fixture.pparams body Fixture.collateralInputs
  -- Verify
  validationToEither actual
    `shouldBe` Left (IncorrectTotalCollateralField (DeltaCoin 4) (Coin 3) :| [])

rejectsNativeAssetLoss :: Expectation
rejectsNativeAssetLoss = do
  -- Setup
  let body = Fixture.incompleteNativeReturnBody
  -- Exercise
  let actual = validateTotalCollateral @DijkstraEra @"UTXO" Fixture.pparams body Fixture.collateralInputs
  -- Verify
  validationToEither actual
    `shouldBe` Left (CollateralContainsNonADA (MaryValue (Coin 7) Fixture.tokens) :| [])

creditsInvalidFees :: Expectation
creditsInvalidFees = do
  -- Setup
  let state = Fixture.collateralState
  -- Exercise
  let actual = updateDijkstraUTxOStatePhase2Invalid Fixture.pparams Fixture.collateralBody state
  -- Verify
  utxosFees actual `shouldBe` Coin 5

preservesCollateralReturn :: Expectation
preservesCollateralReturn = do
  -- Setup
  let state = Fixture.collateralState
  -- Exercise
  let actual = updateDijkstraUTxOStatePhase2Invalid Fixture.pparams Fixture.collateralBody state
      returned = foldMap' (^. potValueTxOutF) (unUTxO (utxosUtxo actual))
  -- Verify
  returned `shouldBe` MaryValue (Coin 6) Fixture.tokens

removesCollateralInputs :: Expectation
removesCollateralInputs = do
  -- Setup
  let state = Fixture.collateralState
  -- Exercise
  let actual = updateDijkstraUTxOStatePhase2Invalid Fixture.pparams Fixture.collateralBody state
      remainingInputs = Map.keys (Map.intersection (unUTxO (utxosUtxo actual)) Fixture.collateralInputs)
  -- Verify
  remainingInputs `shouldBe` mempty

fundsTopLevelOutputs :: Expectation
fundsTopLevelOutputs = do
  -- Setup
  let body = Fixture.implicitTopBody
  -- Exercise
  let actual =
        runIdentity $
          updateDijkstraUTxOAndInstantStake Fixture.pparams body (\_ _ -> pure ()) Fixture.emptyState
      deposit = foldMap' (^. capacityDepositTxOutL) (unUTxO (utxosUtxo actual))
  -- Verify
  deposit `shouldSatisfy` (> CapacityDeposit (Coin 0))

fundsSubtransactionOutputs :: Expectation
fundsSubtransactionOutputs = do
  -- Setup
  let body = Fixture.implicitSubBody
  -- Exercise
  let actual =
        runIdentity $
          updateDijkstraUTxOAndInstantStake Fixture.pparams body (\_ _ -> pure ()) Fixture.emptyState
      deposit = foldMap' (^. capacityDepositTxOutL) (unUTxO (utxosUtxo actual))
  -- Verify
  deposit `shouldSatisfy` (> CapacityDeposit (Coin 0))

creditsImplicitReturnFees :: Expectation
creditsImplicitReturnFees = do
  -- Setup
  let body = Fixture.implicitCollateralBody
      state = Fixture.implicitCollateralState
  -- Exercise
  let actual = updateDijkstraUTxOStatePhase2Invalid Fixture.pparams body state
  -- Verify
  utxosFees actual `shouldBe` Coin 9000001

fundsCollateralReturn :: Expectation
fundsCollateralReturn = do
  -- Setup
  let body = Fixture.implicitCollateralBody
      state = Fixture.implicitCollateralState
  -- Exercise
  let actual = updateDijkstraUTxOStatePhase2Invalid Fixture.pparams body state
      deposit = foldMap' (^. capacityDepositTxOutL) (unUTxO (utxosUtxo actual))
  -- Verify
  deposit `shouldSatisfy` (> CapacityDeposit (Coin 0))

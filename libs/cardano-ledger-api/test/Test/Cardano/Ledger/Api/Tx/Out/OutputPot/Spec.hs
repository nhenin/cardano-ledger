{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Api.Tx.Out.OutputPot.Spec (
  currentSpec,
  mixedAssetsSpec,
  collateralReturnSpec,
) where

import Cardano.Ledger.Api.Tx.Out (potCoinsTxOutF, potValueTxOutF)
import Cardano.Ledger.Babbage.Collateral (collAdaBalance)
import Cardano.Ledger.Babbage.Core hiding (potCoinsTxOutF, potValueTxOutF)
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Mary.Value (MaryValue)
import Cardano.Ledger.State (sumAllCoin, sumAllValue, sumCoinUTxO, sumUTxO)
import qualified Cardano.Ledger.Val as Val
import Lens.Micro
import qualified Test.Cardano.Ledger.Api.Tx.Out.OutputPot.Fixture as Fixture
import Test.Cardano.Ledger.Common

-- These laws describe the seven current, unsplit output representations.
-- A future split representation needs separate tests for its accounting override.
currentSpec :: forall era. (EraTxOut era, Arbitrary (TxOut era)) => Spec
currentSpec = describe "Current unsplit OutputPot projections" $ do
  prop "preserve the editable value representation" $
    forAll (Fixture.currentOutput @era) potRepresentationMatchesHoldings
  prop "preserve the editable value" $
    forAll (Fixture.currentOutput @era) potValueMatchesHoldings
  prop "preserve the editable ADA amount" $
    forAll (Fixture.currentOutput @era) potCoinsMatchHoldings
  prop "preserve the editable compact ADA amount" $
    forAll (Fixture.currentOutput @era) compactPotCoinsMatchHoldings
  prop "project the ADA amount from the OutputPot value" $
    forAll (Fixture.currentOutput @era) potCoinsAgreeWithValue
  prop "classify ADA-only outputs from the OutputPot value" $
    forAll (Fixture.currentOutput @era) adaOnlyClassificationAgreesWithPotValue

  describe "After replacing holdings through the plain setter route" $
    holdingsReplacementSpec (Fixture.outputWithPlainHoldingsReplacement @era)
  describe "After replacing holdings through the compact setter route" $
    holdingsReplacementSpec (Fixture.outputWithCompactHoldingsReplacement @era)

  describe "Bounded output collections" $ do
    prop "preserve the sum of editable values in a list" $
      forAll (Fixture.boundedOutputCollection @era) listValueMatchesCombinedHoldings
    prop "preserve the sum of editable ADA amounts in a list" $
      forAll (Fixture.boundedOutputCollection @era) listCoinsMatchCombinedHoldings
    prop "preserve the sum of editable values in a UTxO" $
      forAll (Fixture.boundedOutputCollection @era) utxoValueMatchesCombinedHoldings
    prop "preserve the sum of editable ADA amounts in a UTxO" $
      forAll (Fixture.boundedOutputCollection @era) utxoCoinsMatchCombinedHoldings

  describe "Empty output collections" $ do
    it "sum list values to zero" $
      listValueMatchesCombinedHoldings (Fixture.emptyOutputCollection @era)
    it "sum list ADA amounts to zero" $
      listCoinsMatchCombinedHoldings (Fixture.emptyOutputCollection @era)
    it "sum UTxO values to zero" $
      utxoValueMatchesCombinedHoldings (Fixture.emptyOutputCollection @era)
    it "sum UTxO ADA amounts to zero" $
      utxoCoinsMatchCombinedHoldings (Fixture.emptyOutputCollection @era)

holdingsReplacementSpec :: EraTxOut era => Gen (Fixture.HoldingsReplacement era) -> Spec
holdingsReplacementSpec replacements = do
  prop "project the replacement value" $
    forAll replacements potValueMatchesReplacement
  prop "project the replacement ADA amount" $
    forAll replacements potCoinsMatchReplacement
  prop "project the replacement compact ADA amount" $
    forAll replacements compactPotCoinsMatchReplacement

mixedAssetsSpec :: forall era. (EraTxOut era, Value era ~ MaryValue) => Spec
mixedAssetsSpec = describe "Outputs with ADA and shared native assets" $ do
  it "sum list values including every native quantity" $
    listValueMatchesCombinedHoldings (Fixture.outputsWithSharedNativeAssets @era)
  it "sum list ADA amounts independently of native quantities" $
    listCoinsMatchCombinedHoldings (Fixture.outputsWithSharedNativeAssets @era)
  it "sum UTxO values including every native quantity" $
    utxoValueMatchesCombinedHoldings (Fixture.outputsWithSharedNativeAssets @era)
  it "sum UTxO ADA amounts independently of native quantities" $
    utxoCoinsMatchCombinedHoldings (Fixture.outputsWithSharedNativeAssets @era)

collateralReturnSpec :: forall era. BabbageEraTxBody era => Spec
collateralReturnSpec = describe "Collateral ADA balance" $ do
  it "retains all collateral input ADA when no return is present" $
    collateralBalanceMatchesRemainingCoins (Fixture.collateralWithoutReturn @era)
  it "subtracts a nonzero return once from all collateral input ADA" $
    collateralBalanceMatchesRemainingCoins (Fixture.collateralWithNonzeroReturn @era)

potRepresentationMatchesHoldings :: EraTxOut era => TxOut era -> Expectation
potRepresentationMatchesHoldings txOut = do
  -- Setup
  let expected = txOut ^. valueEitherTxOutL
  -- Exercise
  let actual = txOut ^. potValueEitherTxOutF
  -- Verify
  actual `shouldBe` expected

potValueMatchesHoldings :: EraTxOut era => TxOut era -> Expectation
potValueMatchesHoldings txOut = do
  -- Setup
  let expected = txOut ^. valueTxOutL
  -- Exercise
  let actual = txOut ^. potValueTxOutF
  -- Verify
  actual `shouldBe` expected

potCoinsMatchHoldings :: EraTxOut era => TxOut era -> Expectation
potCoinsMatchHoldings txOut = do
  -- Setup
  let expected = txOut ^. coinTxOutL
  -- Exercise
  let actual = txOut ^. potCoinsTxOutF
  -- Verify
  actual `shouldBe` expected

compactPotCoinsMatchHoldings :: EraTxOut era => TxOut era -> Expectation
compactPotCoinsMatchHoldings txOut = do
  -- Setup
  let expected = txOut ^. compactCoinTxOutL
  -- Exercise
  let actual = txOut ^. compactPotCoinsTxOutF
  -- Verify
  actual `shouldBe` expected

potCoinsAgreeWithValue :: EraTxOut era => TxOut era -> Expectation
potCoinsAgreeWithValue txOut = do
  -- Setup
  let expected = Val.coin (txOut ^. potValueTxOutF)
  -- Exercise
  let actual = txOut ^. potCoinsTxOutF
  -- Verify
  actual `shouldBe` expected

adaOnlyClassificationAgreesWithPotValue :: EraTxOut era => TxOut era -> Expectation
adaOnlyClassificationAgreesWithPotValue txOut = do
  -- Setup
  let expected = Val.isAdaOnly (txOut ^. potValueTxOutF)
  -- Exercise
  let actual = txOut ^. isAdaOnlyTxOutF
  -- Verify
  actual `shouldBe` expected

potValueMatchesReplacement :: EraTxOut era => Fixture.HoldingsReplacement era -> Expectation
potValueMatchesReplacement replacement = do
  -- Setup
  let txOut = Fixture.replacedOutput replacement
      expected = Fixture.replacementValue replacement
  -- Exercise
  let actual = txOut ^. potValueTxOutF
  -- Verify
  actual `shouldBe` expected

potCoinsMatchReplacement :: EraTxOut era => Fixture.HoldingsReplacement era -> Expectation
potCoinsMatchReplacement replacement = do
  -- Setup
  let txOut = Fixture.replacedOutput replacement
      expected = Val.coin (Fixture.replacementValue replacement)
  -- Exercise
  let actual = txOut ^. potCoinsTxOutF
  -- Verify
  actual `shouldBe` expected

compactPotCoinsMatchReplacement ::
  EraTxOut era => Fixture.HoldingsReplacement era -> Expectation
compactPotCoinsMatchReplacement replacement = do
  -- Setup
  let txOut = Fixture.replacedOutput replacement
      expected = Val.coin (Fixture.replacementValue replacement)
  -- Exercise
  let actual = fromCompact (txOut ^. compactPotCoinsTxOutF)
  -- Verify
  actual `shouldBe` expected

listValueMatchesCombinedHoldings :: EraTxOut era => Fixture.OutputCollection era -> Expectation
listValueMatchesCombinedHoldings collection = do
  -- Setup
  let outputs = Fixture.collectionOutputs collection
      expected = Fixture.combinedValue collection
  -- Exercise
  let actual = sumAllValue outputs
  -- Verify
  actual `shouldBe` expected

listCoinsMatchCombinedHoldings :: EraTxOut era => Fixture.OutputCollection era -> Expectation
listCoinsMatchCombinedHoldings collection = do
  -- Setup
  let outputs = Fixture.collectionOutputs collection
      expected = Fixture.combinedCoins collection
  -- Exercise
  let actual = sumAllCoin outputs
  -- Verify
  actual `shouldBe` expected

utxoValueMatchesCombinedHoldings :: EraTxOut era => Fixture.OutputCollection era -> Expectation
utxoValueMatchesCombinedHoldings collection = do
  -- Setup
  let utxo = Fixture.collectionUTxO collection
      expected = Fixture.combinedValue collection
  -- Exercise
  let actual = sumUTxO utxo
  -- Verify
  actual `shouldBe` expected

utxoCoinsMatchCombinedHoldings :: EraTxOut era => Fixture.OutputCollection era -> Expectation
utxoCoinsMatchCombinedHoldings collection = do
  -- Setup
  let utxo = Fixture.collectionUTxO collection
      expected = Fixture.combinedCoins collection
  -- Exercise
  let actual = sumCoinUTxO utxo
  -- Verify
  actual `shouldBe` expected

collateralBalanceMatchesRemainingCoins ::
  BabbageEraTxBody era => Fixture.CollateralBalanceCase era -> Expectation
collateralBalanceMatchesRemainingCoins balanceCase = do
  -- Setup
  let txBody = Fixture.collateralBody balanceCase
      inputs = Fixture.collateralInputs balanceCase
      expected = Fixture.remainingCollateralCoins balanceCase
  -- Exercise
  let actual = collAdaBalance txBody inputs
  -- Verify
  actual `shouldBe` expected

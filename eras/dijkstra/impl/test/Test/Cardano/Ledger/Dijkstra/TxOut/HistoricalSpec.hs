{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Dijkstra.TxOut.HistoricalSpec (spec) where

import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Core
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.State (sumAllCoin, sumAllValue)
import Lens.Micro
import Test.Cardano.Ledger.Common hiding (output)
import Test.Cardano.Ledger.Conway.Arbitrary ()
import qualified Test.Cardano.Ledger.Dijkstra.TxOut.Historical.Fixture as Fixture

spec :: Spec
spec = describe "Historical output pot accounting" $ do
  describe "Shelley" $ unchangedOutputPot @ShelleyEra
  describe "Allegra" $ unchangedOutputPot @AllegraEra
  describe "Mary" $ unchangedOutputPot @MaryEra
  describe "Alonzo" $ unchangedOutputPot @AlonzoEra
  describe "Babbage" $ unchangedOutputPot @BabbageEra
  describe "Conway" $ unchangedOutputPot @ConwayEra

unchangedOutputPot :: forall era. (EraTxOut era, Arbitrary (TxOut era)) => Spec
unchangedOutputPot = do
  prop "retains the historical value projection" $ forAll (arbitrary @(TxOut era)) $ \output -> do
    -- Setup
    let expected = output ^. valueTxOutL
    -- Exercise
    let actual = output ^. potValueTxOutF
    -- Verify
    actual `shouldBe` expected
  prop "retains the historical coin projection" $ forAll (arbitrary @(TxOut era)) $ \output -> do
    -- Setup
    let expected = output ^. coinTxOutL
    -- Exercise
    let actual = output ^. potCoinsTxOutF
    -- Verify
    actual `shouldBe` expected
  prop "retains the historical compact coin projection" $ forAll (arbitrary @(TxOut era)) $ \output -> do
    -- Setup
    let expected = output ^. compactCoinTxOutL
    -- Exercise
    let actual = output ^. compactPotCoinsTxOutF
    -- Verify
    actual `shouldBe` expected
  prop "retains the historical sum of values" $ forAll (Fixture.boundedOutputs @era) $ \outputs -> do
    -- Setup
    let expected = foldMap (^. valueTxOutL) outputs
    -- Exercise
    let actual = sumAllValue outputs
    -- Verify
    actual `shouldBe` expected
  prop "retains the historical sum of coins" $ forAll (Fixture.boundedOutputs @era) $ \outputs -> do
    -- Setup
    let expected = foldMap (^. coinTxOutL) outputs
    -- Exercise
    let actual = sumAllCoin outputs
    -- Verify
    actual `shouldBe` expected

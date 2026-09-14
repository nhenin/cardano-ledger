{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Dijkstra.TxOut.ApplicationAssetsSpec (spec) where

import Cardano.Ledger.Binary (decodeFull, serialize)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Compactible (Compactible (..), toCompactPartial)
import Cardano.Ledger.Core (eraProtVerLow)
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Dijkstra.TxOut.ApplicationAssets (
  ApplicationAssets,
  applicationCoins,
  nativeAssets,
 )
import Cardano.Ledger.Mary.MultiAsset (MultiAsset (..))
import Cardano.Ledger.Mary.Value (MaryValue (..))
import qualified Cardano.Ledger.Val as Val
import Data.Aeson (toJSON)
import Test.Cardano.Ledger.Common
import qualified Test.Cardano.Ledger.Dijkstra.TxOut.ApplicationAssets.Fixture as Fixture

spec :: Spec
spec = describe "ApplicationAssets" $ do
  describe "application balances" $ do
    prop "adding an empty balance preserves the assets" $
      forAll Fixture.genApplicationBalance $ \assets ->
        assets <> mempty `shouldBe` assets

    prop "addition is associative" $
      forAll Fixture.genBalanceTriple $ \(first, second, third) ->
        (first <> second) <> third `shouldBe` first <> (second <> third)

    prop "addition is commutative" $
      forAll Fixture.genBalancePair $ \(first, second) ->
        first <> second `shouldBe` second <> first

    prop "adding the inverse cancels the balance" $
      forAll Fixture.genApplicationBalance $ \assets ->
        assets <> Val.invert assets `shouldBe` mempty

    prop "scaling distributes over addition" $
      forAll Fixture.genScaledBalances $ \(factor, first, second) ->
        Val.scale factor (first <> second)
          `shouldBe` (Val.scale factor first <> Val.scale factor second)

    it "subtraction retains negative ADA and native balances" $
      (Fixture.assetsWithCoinsAndToken 5 2 Val.<-> Fixture.assetsWithCoinsAndToken 8 5)
        `shouldBe` Fixture.assetsWithCoinsAndToken (-3) (-3)

  describe "application ADA" $ do
    prop "injected coins are entirely application ADA" $
      forAll Fixture.genApplicationCoins $ \coins ->
        Val.coin (Val.inject coins :: ApplicationAssets) `shouldBe` coins

    prop "ADA-only assets are recovered by injecting their coins" $
      forAll Fixture.genAdaOnlyAssets $ \assets ->
        Val.inject (Val.coin assets) `shouldBe` assets

    prop "coin modification applies to application ADA" $
      forAll Fixture.genApplicationBalance $ \assets ->
        applicationCoins (Val.modifyCoin (<> Coin 7) assets)
          `shouldBe` (applicationCoins assets <> Coin 7)

    prop "coin modification preserves native assets" $
      forAll Fixture.genApplicationBalance $ \assets ->
        nativeAssets (Val.modifyCoin (const (Coin 0)) assets) `shouldBe` nativeAssets assets

    it "an output with native assets is not ADA-only" $
      Val.isAdaOnly (Fixture.assetsWithCoinsAndToken 0 1) `shouldBe` False

  describe "pointwise comparison" $ do
    it "a positive native balance exceeds a missing asset's zero quantity" $
      Val.pointwise (<=) (Fixture.assetsWithCoinsAndToken 0 1) mempty `shouldBe` False

    it "a missing asset's zero quantity is below a positive native balance" $
      Val.pointwise (<=) mempty (Fixture.assetsWithCoinsAndToken 0 1) `shouldBe` True

  describe "native-asset normalization" $ do
    prop "cancellation removes native entries and their empty policies" $
      forAll Fixture.genApplicationBalance $ \assets ->
        let MultiAsset remainingEntries = nativeAssets (assets <> Val.invert assets)
         in remainingEntries `shouldBe` mempty

    prop "zero scaling removes native entries and their empty policies" $
      forAll Fixture.genApplicationBalance $ \assets ->
        let MultiAsset remainingEntries = nativeAssets (Val.scale (0 :: Integer) assets)
         in remainingEntries `shouldBe` mempty

    it "scaling prunes raw zero quantities and empty policies" $ do
      let MultiAsset remainingEntries = nativeAssets (Val.scale (1 :: Integer) Fixture.rawZeroAssets)
      remainingEntries `shouldBe` mempty

  describe "compact application assets" $ do
    prop "supported output quantities survive compaction" $
      forAll Fixture.genOutputAssets $ \assets ->
        fromCompact <$> toCompact assets `shouldBe` Just assets

    prop "compact coin access reads application ADA" $
      forAll Fixture.genOutputAssets $ \assets ->
        fromCompact (Val.coinCompact (toCompactPartial assets)) `shouldBe` applicationCoins assets

    prop "compact injection creates only application ADA" $
      forAll Fixture.genOutputAssets $ \assets ->
        fromCompact (Val.injectCompact (toCompactPartial (applicationCoins assets)))
          `shouldBe` (Val.inject (applicationCoins assets) :: ApplicationAssets)

    prop "compact coin modification changes application ADA" $
      forAll Fixture.genOutputAssets $ \assets ->
        let changedAssets = Val.modifyCompactCoin (const (toCompactPartial (Coin 7))) (toCompactPartial assets)
         in applicationCoins (fromCompact changedAssets) `shouldBe` Coin 7

    prop "compact coin modification preserves native assets" $
      forAll Fixture.genOutputAssets $ \assets ->
        let changedAssets = Val.modifyCompactCoin (const (toCompactPartial (Coin 7))) (toCompactPartial assets)
         in nativeAssets (fromCompact changedAssets) `shouldBe` nativeAssets assets

    forM_ Fixture.nonCompactableAssets $ \(scenarioName, assets) ->
      it ("rejects " <> scenarioName) $
        toCompact assets `shouldBe` Nothing

    it "retains ADA and native quantities at the Word64 upper bound" $
      fromCompact <$> toCompact Fixture.maximumOutputAssets `shouldBe` Just Fixture.maximumOutputAssets

  describe "quantity serialization" $ do
    let protocolVersion = eraProtVerLow @DijkstraEra

    prop "supported output quantities survive CBOR encoding" $
      forAll Fixture.genOutputAssets $ \assets ->
        decodeFull @ApplicationAssets protocolVersion (serialize protocolVersion assets)
          `shouldBe` Right assets

    prop "compact output quantities survive CBOR encoding" $
      forAll Fixture.genOutputAssets $ \assets ->
        let compactAssets = toCompactPartial assets
         in decodeFull @(CompactForm ApplicationAssets)
              protocolVersion
              (serialize protocolVersion compactAssets)
              `shouldBe` Right compactAssets

    prop "CBOR preserves the existing ADA and native-asset quantity format" $
      forAll Fixture.genOutputAssets $ \assets ->
        serialize protocolVersion assets
          `shouldBe` serialize protocolVersion (MaryValue (applicationCoins assets) (nativeAssets assets))

    prop "JSON preserves the existing ADA and native-asset quantity fields" $
      forAll Fixture.genApplicationBalance $ \assets ->
        toJSON assets `shouldBe` toJSON (MaryValue (applicationCoins assets) (nativeAssets assets))

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Mary.MultiAssetSpec (spec) where

import Cardano.Ledger.BaseTypes (natVersion)
import Cardano.Ledger.Binary (DecoderError, decodeFull, serialize)
import Cardano.Ledger.Hashes (ScriptHash (..))
import Cardano.Ledger.Mary.AssetName (AssetName (..))
import Cardano.Ledger.Mary.MultiAsset (MultiAsset (..), multiAssetFromList)
import Cardano.Ledger.Mary.PolicyID (PolicyID (..))
import qualified Data.ByteString.Lazy as BSL
import Data.CanonicalMaps (canonicalInsert)
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import qualified Test.Cardano.Base.QuickCheck as BaseQC
import Test.Cardano.Data (expectValidMap)
import Test.Cardano.Ledger.Binary.RoundTrip (
  roundTripCborExpectation,
  roundTripCborFailureExpectation,
  roundTripCborRangeExpectation,
  roundTripCborRangeFailureExpectation,
 )
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Mary.Arbitrary (
  genMultiAssetCompletelyEmpty,
  genMultiAssetNestedEmpty,
  genMultiAssetZero,
 )

spec :: Spec
spec = do
  describe "MultiAsset" $ do
    prop "Canonical construction agrees" $
      BaseQC.withNumTests 10000 propCanonicalConstructionAgrees
    describe "Raw representation" $ do
      it "Encoding preserves explicit zero entries and empty policies" $
        forM_ [minBound .. maxBound] $ \version ->
          serialize version (MultiAsset rawMap) `shouldBe` serialize version rawMap
      it "Pre-Conway decoding prunes zeros and empty policies" $
        forM_ [natVersion @4 .. natVersion @8] $ \version ->
          -- MultiAsset equality treats absent and zero quantities alike. Compare
          -- the actual maps to check that decoding really removes these entries.
          case decodeFull @MultiAsset version (serialize version (MultiAsset rawMap)) of
            Left err -> expectationFailure $ "Failed to decode raw MultiAsset: " ++ show err
            Right (MultiAsset decoded) -> decoded `shouldBe` prunedMap
  describe "CBOR roundtrip" $ do
    describe "MultiAsset" $ do
      prop "Non-zero-valued MultiAsset succeeds for all eras" $
        roundTripCborExpectation @MultiAsset
      prop "Zero-valued MultiAsset fails for Conway" $
        forAll genMultiAssetZero $
          roundTripCborRangeFailureExpectation (natVersion @9) maxBound
      prop "MultiAsset with empty nested asset maps fails for Conway and Dijkstra" $
        forAll genMultiAssetNestedEmpty $
          roundTripCborRangeFailureExpectation (natVersion @9) maxBound
      prop "Completely empty MultiAsset succeeds for Conway (but fails for Dijkstra)" $
        forAll genMultiAssetCompletelyEmpty $
          roundTripCborRangeExpectation (natVersion @4) (natVersion @11)
      prop "Completely empty MultiAsset fails for Dijkstra" $
        forAll genMultiAssetCompletelyEmpty $
          roundTripCborRangeFailureExpectation (natVersion @12) maxBound
      -- Pre-Conway decoding accepts these representations and prunes them.
      prop "All MultiAsset types succeed for pre-Conway eras" $
        forAll (oneof [genMultiAssetCompletelyEmpty, genMultiAssetZero, genMultiAssetNestedEmpty]) $ \ma -> do
          forM_ [natVersion @4 .. natVersion @8] $ \version -> do
            let serialized :: BSL.ByteString
                serialized = serialize @MultiAsset version ma
            case decodeFull version serialized :: Either DecoderError MultiAsset of
              Right _ -> pure ()
              Left _ ->
                expectationFailure $
                  mconcat
                    [ "Should have deserialized successfully: <version: "
                    , show version
                    , "> "
                    , show ma
                    ]
      it "Signed quantities roundtrip at both Int64 bounds" $
        forM_ [toInteger (minBound :: Int64), toInteger (maxBound :: Int64)] $ \quantity ->
          roundTripCborExpectation $ singletonMultiAsset quantity
      it "Quantities outside the signed Int64 bounds are rejected" $
        forM_ [toInteger (minBound :: Int64) - 1, toInteger (maxBound :: Int64) + 1] $ \quantity ->
          roundTripCborFailureExpectation $ singletonMultiAsset quantity

singletonMultiAsset :: Integer -> MultiAsset
singletonMultiAsset quantity =
  MultiAsset $ Map.singleton firstPolicy $ Map.singleton (AssetName "bounds") quantity

firstPolicy :: PolicyID
firstPolicy = PolicyID (ScriptHash "000102030405060708090a0b0c0d0e0f101112131415161718191a1b")

-- Construct these maps directly: canonical constructors would remove the very
-- zero quantities and empty policies whose encoded representation we test.
rawMap :: Map.Map PolicyID (Map.Map AssetName Integer)
rawMap =
  Map.fromList
    [
      ( firstPolicy
      , Map.fromList [(AssetName "positive", 3), (AssetName "negative", -2), (AssetName "zero", 0)]
      )
    , (PolicyID (ScriptHash "01010101010101010101010101010101010101010101010101010101"), Map.empty)
    ,
      ( PolicyID (ScriptHash "02020202020202020202020202020202020202020202020202020202")
      , Map.singleton (AssetName "zero") 0
      )
    ]

prunedMap :: Map.Map PolicyID (Map.Map AssetName Integer)
prunedMap =
  Map.singleton firstPolicy $ Map.fromList [(AssetName "positive", 3), (AssetName "negative", -2)]

propCanonicalConstructionAgrees ::
  [(PolicyID, AssetName, Integer)] ->
  [(PolicyID, AssetName, Integer)] ->
  Property
propCanonicalConstructionAgrees xs ys = property $ do
  let ma1@(MultiAsset a1) = multiAssetFromList xs
      ma2@(MultiAsset a2) = multiAssetFromList ys
  expectValidMap a1
  expectValidMap a2
  let mb1@(MultiAsset b1) =
        mconcat
          [ MultiAsset $
              canonicalInsert const pid (canonicalInsert const an i mempty) mempty
          | (pid, an, i) <- xs
          ]
      mb2@(MultiAsset b2) =
        mconcat
          [ MultiAsset $
              canonicalInsert const pid (canonicalInsert const an i mempty) mempty
          | (pid, an, i) <- ys
          ]
  expectValidMap b1
  expectValidMap b2
  ma1 `shouldBe` mb1
  ma2 `shouldBe` mb2
  ma1 <> ma2 `shouldBe` mb1 <> mb2
  ma1 <> mb2 `shouldBe` mb1 <> ma2

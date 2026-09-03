{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Mary.ValueSpec (spec) where

import Cardano.Ledger.BaseTypes (natVersion)
import Cardano.Ledger.Binary (DecoderError, decodeFull, serialize)
import Cardano.Ledger.Coin (Coin (Coin))
import Cardano.Ledger.Compactible (fromCompact, toCompact)
import Cardano.Ledger.Core (eraProtVerLow)
import Cardano.Ledger.Hashes (ScriptHash (..))
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Mary.AssetName (AssetName (..))
import Cardano.Ledger.Mary.MultiAsset (MultiAsset (..))
import Cardano.Ledger.Mary.PolicyID (PolicyID (..))
import Cardano.Ledger.Mary.Value (MaryValue (..))
import Control.Exception (AssertionFailed (AssertionFailed), evaluate)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Word (Word64)
import Test.Cardano.Ledger.Binary.RoundTrip (
  roundTripCborExpectation,
  roundTripCborFailureExpectation,
  roundTripCborRangeExpectation,
  roundTripCborRangeFailureExpectation,
 )
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Mary.Arbitrary (
  genMaryValue,
  genMultiAsset,
  genMultiAssetCompletelyEmpty,
  genMultiAssetNestedEmpty,
  genMultiAssetToFail,
  genMultiAssetZero,
  genNegativeInt,
 )

spec :: Spec
spec = do
  describe "CBOR roundtrip" $ do
    describe "Coin" $ do
      prop "Non-negative Coin succeeds for all eras" $
        \(NonNegative i) -> roundTripCborExpectation (Coin i)
      prop "Negative Coin fails to deserialise for all eras" $
        \(Negative i) -> roundTripCborRangeFailureExpectation (natVersion @0) maxBound (Coin i)
    describe "MaryValue" $ do
      prop "Positive MaryValue succeeds for all eras" $ \(mv :: MaryValue) ->
        roundTripCborExpectation mv
      prop "Negative MaryValue fails for all eras" $
        forAll
          (genMaryValue (genMultiAsset (toInteger <$> genNegativeInt)))
          roundTripCborFailureExpectation
      prop "Zero MaryValue fails Conway onwards" $
        forAll (genMaryValue genMultiAssetZero) $
          roundTripCborRangeFailureExpectation (natVersion @9) maxBound
      prop "MaryValue with empty nested asset maps fails Conway onwards" $
        forAll (genMaryValue genMultiAssetNestedEmpty) $
          roundTripCborRangeFailureExpectation (natVersion @9) maxBound
      prop "MaryValue with completely empty MultiAsset succeeds for Conway" $
        forAll (genMaryValue genMultiAssetCompletelyEmpty) $
          roundTripCborRangeExpectation (natVersion @4) (natVersion @11)
      prop "MaryValue with completely empty MultiAsset fails for Dijkstra" $ \(Positive c) ->
        forM_ [natVersion @12 .. maxBound] $ \version -> do
          let serialized :: BSL.ByteString
              serialized = serialize @(Int, Map.Map () ()) version (c, Map.empty)
          case decodeFull version serialized :: Either DecoderError MaryValue of
            Left _ -> pure ()
            Right (m :: MaryValue) ->
              expectationFailure $
                mconcat
                  [ "Should not have deserialized: <version: "
                  , show version
                  , "> "
                  , show m
                  ]
      it "MaryValue quantities use unsigned rather than signed 64-bit bounds" $ do
        let pid = PolicyID (ScriptHash "000102030405060708090a0b0c0d0e0f101112131415161718191a1b")
            ma =
              MultiAsset $ Map.singleton pid $ Map.singleton (AssetName "bounds") (toInteger (maxBound :: Word64))
        -- The same native-asset map is decoded with an unsigned amount decoder
        -- inside MaryValue, but with the signed mint decoder on its own.
        roundTripCborExpectation (MaryValue (Coin 0) ma)
        roundTripCborFailureExpectation ma
      it "Too many assets should fail" $
        property $
          forAll
            (genMaryValue (genMultiAssetToFail True))
            ( roundTripCborRangeFailureExpectation @MaryValue
                (eraProtVerLow @MaryEra)
                maxBound
            )
  describe "MaryValue compacting" $ do
    prop "Canonical generator" $
      \(ma :: MaryValue) ->
        fromCompact (fromJust (toCompact ma)) `shouldBe` ma
    prop "Failing generator" $
      forAll (genMaryValue (genMultiAssetToFail True)) $
        \ma ->
          evaluate (fromCompact (fromJust (toCompact ma)))
            `shouldThrow` (\(AssertionFailed errorMsg) -> take 16 errorMsg == "Assertion failed")

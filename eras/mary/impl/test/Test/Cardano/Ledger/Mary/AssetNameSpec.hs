{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Mary.AssetNameSpec (spec) where

import Cardano.Ledger.Binary (decodeFull, serialize)
import Cardano.Ledger.Mary.AssetName (AssetName (..), assetNameToTextAsHex)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as BS16
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Short as SBS
import Data.String (IsString (..))
import Test.Cardano.Ledger.Binary.RoundTrip (roundTripCborFailureExpectation)
import Test.Cardano.Ledger.Common

spec :: Spec
spec = describe "AssetName" $ do
  it "CBOR preserves an empty asset name" $
    expectCborBytes (AssetName mempty) BS.empty
  it "CBOR preserves all 32 bytes at the size limit" $ do
    let assetName = "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"
        bytes = BS.pack [0 .. 31]
    assetNameBytes assetName `shouldBe` SBS.toShort bytes
    assetNameToTextAsHex assetName
      `shouldBe` "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"
    expectCborBytes assetName bytes
  it "CBOR rejects asset names longer than 32 bytes" $
    roundTripCborFailureExpectation $
      AssetName $
        SBS.toShort $
          BS.replicate 33 0

expectCborBytes :: AssetName -> BS.ByteString -> Expectation
expectCborBytes assetName bytes =
  forM_ [minBound .. maxBound] $ \version -> do
    serialize version assetName `shouldBe` serialize version bytes
    decodeFull @AssetName version (serialize version bytes) `shouldBe` Right assetName

-- Test literals denote hexadecimal bytes, independently of the production type.
instance IsString AssetName where
  fromString = AssetName . either error SBS.toShort . BS16.decode . BS8.pack

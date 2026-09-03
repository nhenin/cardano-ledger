{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Mary.PolicyIDSpec (spec) where

import Cardano.Ledger.Binary (decodeFull, serialize)
import Cardano.Ledger.Hashes (ScriptHash (..))
import Cardano.Ledger.Mary.PolicyID (PolicyID (..))
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import Test.Cardano.Ledger.Common

spec :: Spec
spec = describe "PolicyID" $ do
  it "CBOR preserves the 28 script-hash bytes without an extra wrapper" $ do
    let scriptHash = ScriptHash "000102030405060708090a0b0c0d0e0f101112131415161718191a1b"
        pid = PolicyID scriptHash
        bytes = BS.pack [0 .. 27]
    policyID pid `shouldBe` scriptHash
    forM_ [minBound .. maxBound] $ \version -> do
      serialize version pid `shouldBe` serialize version scriptHash
      serialize version pid `shouldBe` serialize version bytes
      decodeFull @PolicyID version (serialize version bytes) `shouldBe` Right pid
  it "CBOR rejects script hashes shorter or longer than 28 bytes" $
    forM_ [27, 29] $ \byteCount ->
      forM_ [minBound .. maxBound] $ \version ->
        decodeFull @PolicyID version (serialize version (BS.replicate byteCount 0))
          `shouldSatisfy` isLeft

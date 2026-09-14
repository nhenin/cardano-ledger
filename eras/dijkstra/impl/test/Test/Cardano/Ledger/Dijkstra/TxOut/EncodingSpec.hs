{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Dijkstra.TxOut.EncodingSpec (spec) where

import Cardano.Ledger.Binary (
  DecShareCBOR (decShareCBOR),
  decNoShareCBOR,
  decodeFull,
  decodeFullDecoder,
  encodeMemPack,
  serialize,
 )
import Cardano.Ledger.Core (EraTxOut (valueTxOutL))
import Cardano.Ledger.Dijkstra.TxOut (DijkstraTxOut, capacityDepositTxOutF)
import Data.Aeson (toJSON, withObject, (.:))
import Data.Aeson.Types (parseMaybe)
import Data.Either (isLeft)
import Lens.Micro ((^.))
import Test.Cardano.Ledger.Common
import qualified Test.Cardano.Ledger.Dijkstra.TxOut.Encoding.Fixture as Fixture

spec :: Spec
spec = describe "DijkstraTxOut encoding" $
  forM_ Fixture.outputCases $ \(name, txOut) -> describe name $ do
    let version = Fixture.protocolVersion

    it "preserves both allocations through CBOR" $
      decodeFull @DijkstraTxOut version (serialize version txOut) `shouldBe` Right txOut

    it "preserves both allocations and compact storage through MemPack" $ do
      let bytes = serialize version (encodeMemPack txOut)
          decoded = decodeFullDecoder version "DijkstraTxOut" (decNoShareCBOR @DijkstraTxOut) bytes
      decoded `shouldBe` Right txOut

    it "preserves both allocations when decoding CBOR with shared credentials" $ do
      let decoded =
            decodeFullDecoder
              version
              "DijkstraTxOut"
              (decShareCBOR @DijkstraTxOut (Fixture.sharedCredentials txOut))
              (serialize version txOut)
      decoded `shouldBe` Right txOut

    it "rejects a CBOR map without the capacity-deposit field" $
      decodeFull @DijkstraTxOut version (Fixture.withoutCapacityDeposit txOut)
        `shouldSatisfy` isLeft

    it "rejects legacy MemPack storage without an allocation envelope" $
      decodeFullDecoder
        version
        "DijkstraTxOut"
        (decNoShareCBOR @DijkstraTxOut)
        (Fixture.legacyMemPackBytes txOut)
        `shouldSatisfy` isLeft

    it "exposes the capacity deposit separately in JSON" $
      parseMaybe (withObject "DijkstraTxOut" (.: "capacityDeposit")) (toJSON txOut)
        `shouldBe` Just (toJSON (txOut ^. capacityDepositTxOutF))

    it "exposes application assets separately in JSON" $
      parseMaybe (withObject "DijkstraTxOut" (.: "applicationAssets")) (toJSON txOut)
        `shouldBe` Just (toJSON (txOut ^. valueTxOutL))

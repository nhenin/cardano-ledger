{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Dijkstra.TxOut.Compatibility.Spec (spec) where

import Cardano.Ledger.Alonzo.Core (AlonzoEraTxOut (..))
import Cardano.Ledger.Babbage.TxOut (
  BabbageEraTxOut (..),
  BabbageTxOut,
  addrEitherBabbageTxOutL,
  babbageMinUTxOValue,
  dataHashBabbageTxOutL,
  datumBabbageTxOutL,
  referenceScriptBabbageTxOutL,
  valueEitherBabbageTxOutL,
 )
import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import Cardano.Ledger.Binary (
  decNoShareCBOR,
  decodeFull,
  decodeFullDecoder,
  encodeMemPack,
  mkSized,
  serialize,
 )
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Conway.TxBody (upgradeBabbageTxOut)
import Cardano.Ledger.Core (EraTxOut (..), coinTxOutL, eraProtVerLow)
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.TxOut (DijkstraTxOut, toBabbageTxOut)
import Cardano.Ledger.Mary.Value (MaryValue)
import Cardano.Ledger.Val (coin)
import Lens.Micro ((&), (.~), (^.))
import Test.Cardano.Ledger.Common
import qualified Test.Cardano.Ledger.Dijkstra.TxOut.Compatibility.Fixture as Fixture

spec :: Spec
spec = describe "DijkstraTxOut compatibility" $ do
  let version = eraProtVerLow @DijkstraEra
  forM_ Fixture.outputCases $ \(name, fixture) -> describe name $ do
    let legacyOutput = Fixture.legacyOutput fixture
        txOut = Fixture.dijkstraOutput fixture

    it "reads the complete value" $
      txOut ^. valueTxOutL `shouldBe` legacyOutputValue legacyOutput

    it "reads the complete ADA amount" $
      txOut ^. coinTxOutL `shouldBe` coin (legacyOutputValue legacyOutput)

    it "preserves the compact or unpacked address view" $
      txOut ^. addrEitherTxOutL `shouldBe` legacyOutput ^. addrEitherBabbageTxOutL

    it "preserves datum-hash access, excluding inline datum hashes" $
      txOut ^. dataHashTxOutL `shouldBe` legacyOutput ^. dataHashBabbageTxOutL

    it "preserves the complete datum" $
      txOut ^. datumTxOutL `shouldBe` legacyOutput ^. datumBabbageTxOutL

    it "replaces value without changing the other fields" $
      toBabbageTxOut (txOut & valueTxOutL .~ Fixture.replacementValue)
        `shouldBe` (legacyOutput & valueEitherBabbageTxOutL .~ Left Fixture.replacementValue)

    it "replaces address without changing the other fields" $
      toBabbageTxOut (txOut & addrTxOutL .~ Fixture.replacementAddress)
        `shouldBe` (legacyOutput & addrEitherBabbageTxOutL .~ Left Fixture.replacementAddress)

    it "replaces datum without changing the other fields" $
      toBabbageTxOut (txOut & datumTxOutL .~ Fixture.replacementDatum)
        `shouldBe` (legacyOutput & datumBabbageTxOutL .~ Fixture.replacementDatum)

    it "removes the reference script without changing the other fields" $
      toBabbageTxOut (txOut & referenceScriptTxOutL .~ SNothing)
        `shouldBe` (legacyOutput & referenceScriptBabbageTxOutL .~ SNothing)

    it "emits identical CBOR bytes" $
      serialize version txOut `shouldBe` serialize version legacyOutput

    it "emits identical MemPack bytes inside CBOR" $
      serialize version (encodeMemPack txOut) `shouldBe` serialize version (encodeMemPack legacyOutput)

    it "reads the previous MemPack representation without normalizing storage" $ do
      let bytes = serialize version (encodeMemPack legacyOutput)
          decoded = decodeFullDecoder version "DijkstraTxOut" (decNoShareCBOR @DijkstraTxOut) bytes
      fmap toBabbageTxOut decoded `shouldBe` Right legacyOutput

    it "preserves the minimum ADA requirement" $
      getMinCoinSizedTxOut Fixture.pricedPParams (mkSized version txOut)
        `shouldBe` babbageMinUTxOValue Fixture.pricedPParams (mkSized version legacyOutput)

  forM_ Fixture.decodingCases $ \(name, fixture) ->
    it ("decodes " <> name) $ do
      let decoded = decodeFull @DijkstraTxOut version (Fixture.encodedOutput fixture)
      fmap toBabbageTxOut decoded `shouldBe` Right (Fixture.decodedOutput fixture)

  forM_ Fixture.basicOutputCases $ \(name, fixture) ->
    it ("constructs a basic output for " <> name) $ do
      let txOut = mkBasicTxOut @DijkstraEra (Fixture.basicAddress fixture) (Fixture.basicValue fixture)
      toBabbageTxOut txOut `shouldBe` Fixture.expectedBasicOutput fixture

  describe "structural comparison" $ do
    it "preserves equality across storage variants" $ do
      let expected =
            [ Fixture.legacyOutput first == Fixture.legacyOutput second
            | (first, second) <- Fixture.outputPairs
            ]
          actual =
            [ Fixture.dijkstraOutput first == Fixture.dijkstraOutput second
            | (first, second) <- Fixture.outputPairs
            ]
      actual `shouldBe` expected

    it "preserves ordering across storage variants" $ do
      let expected =
            [ compare (Fixture.legacyOutput first) (Fixture.legacyOutput second)
            | (first, second) <- Fixture.outputPairs
            ]
          actual =
            [ compare (Fixture.dijkstraOutput first) (Fixture.dijkstraOutput second)
            | (first, second) <- Fixture.outputPairs
            ]
      actual `shouldBe` expected

  forM_ Fixture.conwayOutputs $ \(name, legacyOutput) ->
    it ("preserves the Conway upgrade for " <> name) $
      toBabbageTxOut (upgradeTxOut @DijkstraEra legacyOutput)
        `shouldBe` upgradeBabbageTxOut @DijkstraEra legacyOutput

legacyOutputValue :: BabbageTxOut DijkstraEra -> MaryValue
legacyOutputValue legacyOutput = either id fromCompact (legacyOutput ^. valueEitherBabbageTxOutL)

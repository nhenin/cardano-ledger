{-# LANGUAGE TypeApplications #-}

-- | Properties of 'Assets', the application half of the Dijkstra output
-- split: the lawful bridge with the merged 'MaryValue' representation, the
-- wire-format compatibility the newtype promises, and the hand-written
-- compact and state-serialisation instances (the ones GND could not
-- derive).
module Test.Cardano.Ledger.Dijkstra.AssetsSpec (spec) where

import Cardano.Ledger.Compactible (Compactible (..))
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.Assets (Assets (..))
import Cardano.Ledger.Dijkstra.Core (eraProtVerLow)
import Cardano.Ledger.Mary.Value (MaryValue, MaryValueRepresentation (..))
import Cardano.Ledger.Val (coin)
import Data.MemPack (packByteString, unpackError)
import Test.Cardano.Ledger.Binary.RoundTrip (cborTrip, embedTripExpectation)
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Dijkstra.Arbitrary ()

spec :: Spec
spec = describe "Assets" $ do
  prop "toMaryRepresentation . fromMaryRepresentation is the identity" $
    forAll (arbitrary @MaryValue) $ \maryValue ->
      toMaryRepresentation (fromMaryRepresentation @Assets maryValue) === maryValue

  prop "fromMaryRepresentation . toMaryRepresentation is the identity" $
    forAll (arbitrary @Assets) $ \assets ->
      fromMaryRepresentation (toMaryRepresentation assets) === assets

  prop "the bridge only changes the type: the ada coin reads the same" $
    forAll (arbitrary @Assets) $ \assets ->
      coin (toMaryRepresentation assets) === coin assets

  prop "Assets keep MaryValue's wire format" $
    forAll (arbitrary @Assets) $ \assets ->
      embedTripExpectation
        (eraProtVerLow @DijkstraEra)
        (eraProtVerLow @DijkstraEra)
        (cborTrip @Assets @MaryValue)
        (\decodedMaryValue _ -> decodedMaryValue `shouldBe` toMaryRepresentation assets)
        assets

  prop "compact round-trip" $
    forAll (arbitrary @Assets) $ \assets ->
      fmap fromCompact (toCompact assets) === Just assets

  prop "MemPack round-trip on the compact form" $
    forAll (arbitrary @(CompactForm Assets)) $ \compactAssets ->
      unpackError (packByteString compactAssets) === compactAssets

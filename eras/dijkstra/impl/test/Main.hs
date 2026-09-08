{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Cardano.Ledger.Block (Block (Block))
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.Rules ()
import Cardano.Ledger.Plutus (SLanguage (..))
import Cardano.Protocol.Crypto (StandardCrypto)
import qualified Cardano.Protocol.Leios.BlockHeader as Leios
import qualified Test.Cardano.Base.QuickCheck as BaseQC
import qualified Test.Cardano.Ledger.Babbage.TxInfoSpec as BabbageTxInfo
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Conway.Binary.RoundTrip (roundTripConwayCommonSpec)
import Test.Cardano.Ledger.Core.Binary.RoundTrip (
  roundTripAnnEraExpectation,
  roundTripEraExpectation,
 )
import Test.Cardano.Ledger.Dijkstra.Arbitrary (genSmallDijkstraTxsBlockBody)
import Test.Cardano.Ledger.Dijkstra.Binary.Annotator ()
import qualified Test.Cardano.Ledger.Dijkstra.Binary.CddlSpec as Cddl
import qualified Test.Cardano.Ledger.Dijkstra.Binary.Golden as GoldenBinary
import Test.Cardano.Ledger.Dijkstra.Binary.RoundTrip ()
import qualified Test.Cardano.Ledger.Dijkstra.GenesisSpec as GenesisSpec
import qualified Test.Cardano.Ledger.Dijkstra.GoldenSpec as GoldenSpec
import qualified Test.Cardano.Ledger.Dijkstra.Imp as Imp
import Test.Cardano.Ledger.Dijkstra.ImpTest ()
import qualified Test.Cardano.Ledger.Dijkstra.Plutus.PlutusSpec as PlutusSpec
import qualified Test.Cardano.Ledger.Dijkstra.TxInfo.ApplicationAssetsSpec as ApplicationAssetsTxInfoSpec
import Test.Cardano.Ledger.Dijkstra.TxInfo.Fixture (metadataOutputWithFreeCapacity)
import qualified Test.Cardano.Ledger.Dijkstra.TxInfoSpec as DijkstraTxInfoSpec
import qualified Test.Cardano.Ledger.Dijkstra.TxOut.AccountingSpec as OutputAccountingSpec
import qualified Test.Cardano.Ledger.Dijkstra.TxOut.HistoricalSpec as HistoricalOutputSpec
import qualified Test.Cardano.Ledger.Dijkstra.TxOut.TranslationSpec as OutputTranslationSpec
import qualified Test.Cardano.Ledger.Dijkstra.TxOut.Value.TranslationSpec as OutputValueTranslationSpec
import qualified Test.Cardano.Ledger.Dijkstra.TxOut.ValueSpec as OutputValueSpec
import qualified Test.Cardano.Ledger.Dijkstra.TxOutSpec as OutputSpec
import qualified Test.Cardano.Ledger.Dijkstra.UTxO.TranslationSpec as UTxOTranslationSpec
import Test.Cardano.Ledger.Era
import Test.Cardano.Ledger.Shelley.JSON (roundTripJsonShelleyEraSpec)

instance EraSpec DijkstraEra where
  eraImpSpec = Imp.spec

main :: IO ()
main =
  ledgerEraTestMain @DijkstraEra $ do
    ApplicationAssetsTxInfoSpec.spec
    OutputValueSpec.spec
    OutputValueTranslationSpec.spec
    OutputAccountingSpec.spec
    HistoricalOutputSpec.spec
    OutputTranslationSpec.spec
    OutputSpec.spec
    UTxOTranslationSpec.spec
    describe "RoundTrip" $ do
      roundTripConwayCommonSpec @DijkstraEra
      prop "Block (Leios.Header)" $
        BaseQC.withNumTests 25 $
          forAll (Block <$> arbitrary <*> genSmallDijkstraTxsBlockBody) $ \block ->
            conjoin
              [ roundTripEraExpectation @DijkstraEra @(Block (Leios.Header StandardCrypto) DijkstraEra) block
              , roundTripAnnEraExpectation @DijkstraEra @(Block (Leios.Header StandardCrypto) DijkstraEra) block
              ]
    Cddl.spec
    GenesisSpec.spec
    GoldenSpec.spec
    roundTripJsonShelleyEraSpec @DijkstraEra
    describe "TxInfo" $ do
      BabbageTxInfo.specWithOutputPreparation @DijkstraEra metadataOutputWithFreeCapacity
      BabbageTxInfo.txInfoSpecWithOutputPreparation @DijkstraEra metadataOutputWithFreeCapacity SPlutusV3
      BabbageTxInfo.txInfoSpecWithOutputPreparation @DijkstraEra metadataOutputWithFreeCapacity SPlutusV4
      DijkstraTxInfoSpec.spec @DijkstraEra
    GoldenBinary.spec @DijkstraEra
    PlutusSpec.spec

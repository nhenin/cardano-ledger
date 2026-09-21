{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Dijkstra.TxOut.Compatibility.Fixture (
  OutputCase (..),
  BasicOutputCase (..),
  DecodingCase (..),
  outputCases,
  outputPairs,
  decodingCases,
  conwayOutputs,
  basicOutputCases,
  replacementValue,
  replacementAddress,
  replacementDatum,
  replacementScript,
  pricedPParams,
) where

import Cardano.Ledger.Address (Addr (..), compactAddr)
import Cardano.Ledger.Alonzo.TxBody (encodeAddress28, encodeDataHash32)
import Cardano.Ledger.Babbage.PParams (ppCoinsPerUTxOByteL)
import Cardano.Ledger.Babbage.TxOut (BabbageTxOut (..))
import Cardano.Ledger.BaseTypes (Network (..), StrictMaybe (..))
import Cardano.Ledger.Binary (
  encCBOR,
  encodeBreak,
  encodeListLenIndef,
  serialize,
 )
import Cardano.Ledger.Coin (Coin (..), CoinPerByte (..), CompactForm (..))
import Cardano.Ledger.Compactible (toCompactPartial)
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Core (Era, PParams, Script, Value, emptyPParams, eraProtVerLow)
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.TxOut (DijkstraTxOut, fromBabbageTxOut)
import Cardano.Ledger.Mary.AssetName (AssetName (..))
import Cardano.Ledger.Mary.MultiAsset (MultiAsset (..))
import Cardano.Ledger.Mary.PolicyID (PolicyID (..))
import Cardano.Ledger.Mary.Value (MaryValue (..))
import Cardano.Ledger.Plutus (Language (..))
import Cardano.Ledger.Plutus.Data (Datum (..), dataToBinaryData, hashData)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import Lens.Micro ((&), (.~))
import Test.Cardano.Ledger.Alonzo.Arbitrary (alwaysSucceeds)
import Test.Cardano.Ledger.Alonzo.Examples (exampleDatum)
import Test.Cardano.Ledger.Shelley.Examples (exampleByronAddress, mkKeyHash, mkScriptHash)

data OutputCase = OutputCase
  { legacyOutput :: BabbageTxOut DijkstraEra
  , dijkstraOutput :: DijkstraTxOut
  }

data BasicOutputCase = BasicOutputCase
  { basicAddress :: Addr
  , basicValue :: MaryValue
  , expectedBasicOutput :: BabbageTxOut DijkstraEra
  }

data DecodingCase = DecodingCase
  { encodedOutput :: LBS.ByteString
  , decodedOutput :: BabbageTxOut DijkstraEra
  }

outputCases :: [(String, OutputCase)]
outputCases =
  [ (name, OutputCase old (fromBabbageTxOut old))
  | (name, old) <- legacyOutputs replacementScript
  ]

outputPairs :: [(OutputCase, OutputCase)]
outputPairs = [(first, second) | (_, first) <- outputCases, (_, second) <- outputCases]

-- Construct the six storage variants explicitly. The noncanonical compact ADA
-- cases additionally distinguish structural equality from equality of fields.
legacyOutputs ::
  forall era. (Era era, Value era ~ MaryValue) => Script era -> [(String, BabbageTxOut era)]
legacyOutputs script =
  [ ("compact multiasset value", TxOutCompact' address assets)
  , ("compact multiasset value with datum hash", TxOutCompactDH' address assets datumHash)
  , ("compact inline datum", TxOutCompactDatum address assets binaryDatum)
  , ("compact reference script", TxOutCompactRefScript address assets NoDatum script)
  , ("optimized ADA value", TxOut_AddrHash28_AdaOnly staking address28 coins)
  ,
    ( "optimized ADA value with datum hash"
    , TxOut_AddrHash28_AdaOnly_DataHash32 staking address28 coins hash32
    )
  , ("noncanonical compact ADA value", TxOutCompact' address compactAda)
  , ("noncanonical compact ADA value with datum hash", TxOutCompactDH' address compactAda datumHash)
  ,
    ( "reference script with inline datum"
    , TxOutCompactRefScript address assets (Datum binaryDatum) script
    )
  , ("bootstrap address", BabbageTxOut exampleByronAddress adaValue NoDatum SNothing)
  ]
  where
    address = compactAddr baseAddress
    assets = toCompactPartial (MaryValue (Coin 42) (nativeAssets 7))
    compactAda = toCompactPartial adaValue
    staking = KeyHashObj (mkKeyHash 2)
    address28 = encodeAddress28 Testnet (KeyHashObj (mkKeyHash 1))
    coins = CompactCoin 42
    datumHash = hashData (exampleDatum @era)
    hash32 = encodeDataHash32 datumHash
    binaryDatum = dataToBinaryData (exampleDatum @era)

conwayOutputs :: [(String, BabbageTxOut ConwayEra)]
conwayOutputs = legacyOutputs (alwaysSucceeds @'PlutusV1 @ConwayEra 0)

basicOutputCases :: [(String, BasicOutputCase)]
basicOutputCases =
  [ ("base address with ADA", basicOutput baseAddress adaValue)
  , ("base address with native assets", basicOutput baseAddress replacementValue)
  , ("enterprise address", basicOutput replacementAddress replacementValue)
  , ("bootstrap address", basicOutput exampleByronAddress adaValue)
  ]
  where
    basicOutput address value =
      BasicOutputCase address value (BabbageTxOut address value NoDatum SNothing)

decodingCases :: [(String, DecodingCase)]
decodingCases =
  [ (name, DecodingCase (serialize version old) (canonicalOutput old))
  | (name, fixture) <- outputCases
  , let old = legacyOutput fixture
  ]
    <> [ ("indefinite two-field legacy list", indefiniteLegacyOutput NoDatum)
       , ("indefinite three-field legacy list", indefiniteLegacyOutput datumHash)
       ]
  where
    version = eraProtVerLow @DijkstraEra
    datumHash = DatumHash (hashData (exampleDatum @DijkstraEra))
    indefiniteLegacyOutput datum =
      DecodingCase
        ( serialize version $
            encodeListLenIndef
              <> encCBOR baseAddress
              <> encCBOR adaValue
              <> ( case datum of
                     DatumHash dh -> encCBOR dh
                     _ -> mempty
                 )
              <> encodeBreak
        )
        (BabbageTxOut baseAddress adaValue datum SNothing)

-- Wire decoding selects a canonical storage variant, including for an output
-- whose original in-memory constructor was noncanonical.
canonicalOutput :: BabbageTxOut DijkstraEra -> BabbageTxOut DijkstraEra
canonicalOutput (BabbageTxOut address value datum script) =
  BabbageTxOut address value datum script

baseAddress :: Addr
baseAddress = Addr Testnet (KeyHashObj (mkKeyHash 1)) (StakeRefBase (KeyHashObj (mkKeyHash 2)))

adaValue :: MaryValue
adaValue = MaryValue (Coin 42) mempty

replacementValue :: MaryValue
replacementValue = MaryValue (Coin 81) (nativeAssets 11)

nativeAssets :: Integer -> MultiAsset
nativeAssets quantity =
  MultiAsset $ Map.singleton (PolicyID (mkScriptHash 3)) (Map.singleton (AssetName "token") quantity)

replacementAddress :: Addr
replacementAddress = Addr Mainnet (KeyHashObj (mkKeyHash 4)) StakeRefNull

replacementDatum :: Datum DijkstraEra
replacementDatum = Datum (dataToBinaryData exampleDatum)

replacementScript :: Script DijkstraEra
replacementScript = alwaysSucceeds @'PlutusV1 0

pricedPParams :: PParams DijkstraEra
pricedPParams = emptyPParams & ppCoinsPerUTxOByteL .~ CoinPerByte (CompactCoin 4310)

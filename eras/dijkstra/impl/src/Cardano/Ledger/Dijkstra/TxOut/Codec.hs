{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Dijkstra output encoding preserves the explicitly supplied allocation.
module Cardano.Ledger.Dijkstra.TxOut.Codec (
  encodeDijkstraTxOut,
  decodeDijkstraTxOut,
) where

import Cardano.Base.Typeable (TypeName (TypeName))
import Cardano.Ledger.Address (Addr)
import Cardano.Ledger.Binary (
  DecCBOR (decCBOR),
  Decoder,
  Encoding,
  decodeFullAnnotator,
  decodeNestedCborBytes,
  decodeSparseKeyed,
  encodeNestedCbor,
  getDecoderVersion,
 )
import Cardano.Ledger.Binary.Coders (
  Encode (..),
  encode,
  encodeKeyedStrictMaybeWith,
  (!>),
 )
import Cardano.Ledger.Core (Script)
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Dijkstra.Scripts ()
import Cardano.Ledger.Dijkstra.TxOut.ApplicationAssets (ApplicationAssets)
import Cardano.Ledger.Dijkstra.TxOut.CapacityDeposit (CapacityDeposit)
import Cardano.Ledger.Dijkstra.TxOut.Value (OutputValue (..))
import Cardano.Ledger.Plutus (Datum (..))
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe.Strict (StrictMaybe (..))

encodeDijkstraTxOut ::
  Addr ->
  OutputValue ->
  Datum DijkstraEra ->
  StrictMaybe (Script DijkstraEra) ->
  Encoding
encodeDijkstraTxOut addr (OutputValue deposit assets) datum script =
  encode $
    Keyed (,,,,)
      !> Key 0 (To addr)
      !> Key 1 (To assets)
      !> Omit (== NoDatum) (Key 2 (To datum))
      !> encodeKeyedStrictMaybeWith 3 encodeNestedCbor script
      !> Key 4 (To deposit)

data DecodingDijkstraTxOut = DecodingDijkstraTxOut
  { decodingAddress :: !(StrictMaybe Addr)
  , decodingApplicationAssets :: !(StrictMaybe ApplicationAssets)
  , decodingDatum :: !(Datum DijkstraEra)
  , decodingReferenceScript :: !(StrictMaybe (Script DijkstraEra))
  , decodingCapacityDeposit :: !(StrictMaybe CapacityDeposit)
  }

-- | Require both monetary components. Legacy list encodings and maps without
-- an explicit capacity deposit cannot describe a Dijkstra output allocation.
decodeDijkstraTxOut ::
  forall s.
  Decoder s (Addr, OutputValue, Datum DijkstraEra, StrictMaybe (Script DijkstraEra))
decodeDijkstraTxOut = do
  txOut <- decodeSparseKeyed TypeName requiredFields initial decoderByKey
  case txOut of
    DecodingDijkstraTxOut (SJust addr) (SJust assets) datum script (SJust deposit) ->
      pure (addr, OutputValue deposit assets, datum, script)
    _ -> fail "DijkstraTxOut: missing mandatory output fields"
  where
    initial = DecodingDijkstraTxOut SNothing SNothing NoDatum SNothing SNothing
    requiredFields =
      [ (0, "address")
      , (1, "applicationAssets")
      , (4, "capacityDeposit")
      ]
    decoderByKey :: DecodingDijkstraTxOut -> Word -> Maybe (Decoder s DecodingDijkstraTxOut)
    decoderByKey txOut = \case
      0 -> Just $ do
        addr <- decCBOR
        pure txOut {decodingAddress = SJust addr}
      1 -> Just $ do
        assets <- decCBOR
        pure txOut {decodingApplicationAssets = SJust assets}
      2 -> Just $ do
        datum <- decCBOR
        pure txOut {decodingDatum = datum}
      3 -> Just $ do
        script <- decodeReferenceScript
        pure txOut {decodingReferenceScript = SJust script}
      4 -> Just $ do
        deposit <- decCBOR
        pure txOut {decodingCapacityDeposit = SJust deposit}
      _ -> Nothing

decodeReferenceScript :: Decoder s (Script DijkstraEra)
decodeReferenceScript = do
  version <- getDecoderVersion
  bytes <- decodeNestedCborBytes
  case decodeFullAnnotator version "Script" decCBOR (LBS.fromStrict bytes) of
    Left err -> fail $ "Script: " <> show err
    Right script -> pure script

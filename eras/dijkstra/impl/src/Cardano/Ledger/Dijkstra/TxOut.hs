{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | The Dijkstra output: the first era representation that separates
-- application assets from the operational funding of UTxO capacity.
--
-- Up to Conway, one 'Value' blends what the application controls with the
-- ada satisfying the minimum-ada requirement. 'DijkstraTxOut' keeps the
-- familiar structure — address, value, datum, reference script — and adds a
-- fifth, separate concern: the capacity deposit. Wherever an output is read,
-- the two concerns are never mixed: 'assetsTxOutL' sees application assets
-- alone (ada there may be as small as the application chooses, down to
-- zero), and 'capacityDepositTxOutL' sees the operational funding alone.
--
-- Naming convention: every name reachable from a Dijkstra output says which
-- side of the split it touches. The application side is @assets@
-- ('assetsTxOutL', 'assetsAdaTxOutL', the 'Cardano.Ledger.Dijkstra.Assets.Assets'
-- type); the operational side is @capacityDeposit@
-- (the 'Cardano.Ledger.Dijkstra.TxOut.CapacityDeposit.CapacityDeposit' type,
-- 'capacityDepositTxOutL', 'requiredCapacityDepositTxOut',
-- 'setCapacityDepositTxOut',
-- 'Cardano.Ledger.Dijkstra.TxOut.Translation.fundCapacityDeposit'); their sum is
-- @totalAda@ ('totalAdaTxOut'), the only unqualified ada. The generic-era
-- names — 'valueTxOutL', 'coinTxOutL', 'getMinCoinTxOut' — are
-- merged-world compatibility bridges: on this era they read the
-- application side only, and Dijkstra-specific code must use the explicit
-- names instead.
--
-- Wire format, two lanes. The native, split form is the Babbage output map
-- extended with key 4,
--
-- @
--   { 0 : address, 1 : assets, ? 2 : datum_option, ? 3 : script_ref, 4 : capacity_deposit }
-- @
--
-- validated as /exactly/ the required tariff
-- ('Cardano.Ledger.Dijkstra.Rules.CapacityDeposit.validateOutputCapacityDeposit').
-- The merged legacy forms — the Babbage map without key 4 and the Alonzo
-- array — are also accepted and decode with capacity deposit 0, the
-- implicit marker: the previous eras' floor applies, and the ledger derives
-- the split as the output enters the UTxO
-- ('Cardano.Ledger.Dijkstra.UTxO.Translation.fundCapacityDeposits').
-- Accepting them is what keeps a transaction signed before the era
-- boundary valid after it: signed bytes are reinterpreted, never
-- rewritten. Either way the STORED output is always split and exact — at
-- the era boundary the whole set is translated, at entry each created
-- output is restructured.
--
-- Pricing policy: @M(o)@ prices the canonical re-serialisation of the
-- output ('measureTxOut', and the 'Sized' wrapper in the transaction body
-- is built the same way), not the raw wire bytes — the tariff is for the
-- stored form, which is canonical.
module Cardano.Ledger.Dijkstra.TxOut (
  DijkstraTxOut (..),
  DijkstraEraTxOut (..),
  mkDijkstraTxOut,
  assetsTxOutL,
  assetsAdaTxOutL,
  measureTxOut,
  requiredCapacityDepositTxOut,
  totalAdaTxOut,
) where

import Cardano.Ledger.Address (Addr, CompactAddr, compactAddr, decompactAddr)
import Cardano.Ledger.Alonzo.Core (AlonzoEraTxOut (..))
import Cardano.Ledger.Babbage.Core (
  CoinPerByte (..),
  ppCoinsPerUTxOByteL,
 )
import Cardano.Ledger.Babbage.TxOut (
  BabbageEraTxOut (..),
 )
import qualified Cardano.Ledger.Babbage.TxOut as Babbage
import Cardano.Ledger.BaseTypes (
  KeyValuePairs (..),
  StrictMaybe (..),
  ToKeyValuePairs (..),
 )
import Cardano.Ledger.Binary (
  Annotator,
  DecCBOR (..),
  DecShareCBOR (..),
  Decoder,
  DecoderError (DecoderErrorCustom),
  EncCBOR (..),
  Interns,
  Sized,
  TokenType (..),
  cborError,
  decodeBreakOr,
  decodeFullAnnotator,
  decodeListLenOrIndef,
  decodeMemPack,
  decodeNestedCborBytes,
  encodeNestedCbor,
  getDecoderVersion,
  mkSized,
  peekTokenType,
  sizedSize,
 )
import Cardano.Ledger.Binary.Coders (
  Decode (..),
  Encode (..),
  Field,
  decode,
  encode,
  encodeKeyedStrictMaybeWith,
  field,
  invalidField,
  ofield,
  (!>),
 )
import Cardano.Ledger.Coin (Coin (..), CompactForm (..))
import Cardano.Ledger.Compactible (fromCompact, toCompactPartial)
import Cardano.Ledger.Core (
  Era,
  EraScript,
  EraTxOut (..),
  PParams,
  Script,
  Value,
  coinTxOutL,
  eraProtVerLow,
  upgradeScript,
 )
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Dijkstra.Assets (Assets (..), CompactForm (CompactAssets))
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Dijkstra.Scripts ()
import Cardano.Ledger.Dijkstra.TxOut.CapacityDeposit (
  CapacityDeposit (..),
  compactCapacityDepositOrError,
  compactImplicitCapacityDeposit,
  requiredCapacityDeposit,
 )
import Cardano.Ledger.Hashes (DataHash)
import Cardano.Ledger.Keys (KeyRole (Staking))
import Cardano.Ledger.Plutus.Data (
  Datum (..),
  binaryDataToData,
  dataToBinaryData,
  translateDatum,
 )
import Cardano.Ledger.Val (Val (..))
import Control.DeepSeq (NFData (..), rwhnf)
import Data.Aeson (ToJSON (..), (.=))
import qualified Data.ByteString.Lazy as LBS
import Data.MemPack (MemPack (..), packTagM, packedTagByteCount, unknownTagM)
import qualified Data.Text as T
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Lens.Micro (Lens', lens, to, (^.))
import NoThunks.Class (NoThunks)

-- | An output with five concerns, each its own field. Storage uses the
-- compact forms for address and value, like previous eras do internally; the
-- specialised memory-saving constructors of 'BabbageTxOut' are a later
-- optimisation, not part of the representation question.
data DijkstraTxOut era = DijkstraTxOut
  { dtoCompactAddr :: !CompactAddr
  , dtoAssets :: !(CompactForm (Value era))
  -- ^ What the application controls, and nothing else.
  , dtoCapacityDeposit :: !(CompactForm CapacityDeposit)
  -- ^ The operational funding of the capacity this output occupies. Fixed
  -- when the output is created; released when it is consumed.
  , dtoDatum :: !(Datum era)
  , dtoRefScript :: !(StrictMaybe (Script era))
  }
  deriving (Generic)

deriving stock instance
  (Era era, Eq (Script era), Eq (CompactForm (Value era))) =>
  Eq (DijkstraTxOut era)

deriving stock instance
  (Era era, Ord (Script era), Ord (CompactForm (Value era))) =>
  Ord (DijkstraTxOut era)

deriving stock instance
  (Era era, Show (Script era), Show (CompactForm (Value era))) =>
  Show (DijkstraTxOut era)

-- | Already in NF: every field is strict and fully evaluated compact data.
instance NFData (DijkstraTxOut era) where
  rnf = rwhnf

instance
  (Era era, NoThunks (Script era), NoThunks (CompactForm (Value era))) =>
  NoThunks (DijkstraTxOut era)

-- | Build an output from the decompacted views.
mkDijkstraTxOut ::
  (Val (Value era), HasCallStack) =>
  Addr ->
  Value era ->
  CapacityDeposit ->
  Datum era ->
  StrictMaybe (Script era) ->
  DijkstraTxOut era
mkDijkstraTxOut addr assets deposit datum refScript =
  DijkstraTxOut
    { dtoCompactAddr = compactAddr addr
    , dtoAssets = toCompactPartial assets
    , dtoCapacityDeposit = compactCapacityDepositOrError deposit
    , dtoDatum = datum
    , dtoRefScript = refScript
    }

-- | Outputs that record the operational funding of the UTxO capacity they
-- occupy in a field of their own, next to — never inside — the application
-- value.
class BabbageEraTxOut era => DijkstraEraTxOut era where
  -- | The operational concern alone: reading or writing it can never
  -- observe or disturb the application assets.
  capacityDepositTxOutL :: Lens' (TxOut era) CapacityDeposit

  -- | 'capacityDepositTxOutL' without the compact round-trip.
  compactCapacityDepositTxOutL :: Lens' (TxOut era) (CompactForm CapacityDeposit)

  -- | Fund the output's capacity deposit at exactly the required tariff
  -- @M(o)@ — the constructive dual of the exact deposit rule
  -- ('Cardano.Ledger.Dijkstra.Rules.CapacityDeposit.validateOutputCapacityDeposit').
  -- The application assets are untouched: for a transaction builder the
  -- deposit is additional ada the transaction must provide, not ada moved
  -- out of the output. Contrast with
  -- 'Cardano.Ledger.Dijkstra.TxOut.Translation.fundCapacityDeposit', the migration
  -- variant that moves ada and preserves the output's total.
  setCapacityDepositTxOut :: PParams era -> TxOut era -> TxOut era

instance DijkstraEraTxOut DijkstraEra where
  capacityDepositTxOutL =
    lens
      (fromCompact . dtoCapacityDeposit)
      (\txOut d -> txOut {dtoCapacityDeposit = compactCapacityDepositOrError d})
  {-# INLINE capacityDepositTxOutL #-}

  compactCapacityDepositTxOutL =
    lens dtoCapacityDeposit (\txOut d -> txOut {dtoCapacityDeposit = d})
  {-# INLINE compactCapacityDepositTxOutL #-}

  setCapacityDepositTxOut pparams =
    setCapacityDepositLoop 8 (pparams ^. ppCoinsPerUTxOByteL)

-- | The application assets of the output. This is the split-world name for
-- the legacy 'valueTxOutL', whose merged-world name no longer says which
-- side it reads; Dijkstra-specific code uses this one.
assetsTxOutL :: DijkstraEraTxOut era => Lens' (TxOut era) (Value era)
assetsTxOutL = valueTxOutL
{-# INLINE assetsTxOutL #-}

-- | The ada held as an ordinary application asset. This is the split-world
-- name for the legacy 'coinTxOutL': NOT the output's total ada
-- ('totalAdaTxOut') and NOT the operational funding
-- ('capacityDepositTxOutL').
assetsAdaTxOutL :: DijkstraEraTxOut era => Lens' (TxOut era) Coin
assetsAdaTxOutL = coinTxOutL
{-# INLINE assetsAdaTxOutL #-}

-- | The unchanged requirement @M(o)@ of one output: measure its serialised
-- bytes and price them. The formula's single source is
-- 'Cardano.Ledger.Dijkstra.TxOut.CapacityDeposit.requiredCapacityDeposit'; the
-- 'EraTxOut' instance's 'getMinCoinSizedTxOut' delegates here.
requiredCapacityDepositTxOut ::
  CoinPerByte -> Sized (DijkstraTxOut era) -> CapacityDeposit
requiredCapacityDepositTxOut coinsPerUTxOByte sizedTxOut =
  requiredCapacityDeposit coinsPerUTxOByte (sizedSize sizedTxOut)

-- | Measure an output exactly as validation will: its serialized bytes at
-- this era's protocol version.
measureTxOut ::
  forall era.
  (EraScript era, Val (Value era)) =>
  DijkstraTxOut era ->
  Sized (DijkstraTxOut era)
measureTxOut = mkSized (eraProtVerLow @era)

-- | Application ada plus capacity deposit: the ada an output accounts for
-- in total. The migration split preserves it exactly.
totalAdaTxOut :: Val (Value era) => DijkstraTxOut era -> Coin
totalAdaTxOut txOut =
  Coin $
    unCoin (unCapacityDeposit (fromCompact (dtoCapacityDeposit txOut)))
      + unCoin (coin (fromCompact (dtoAssets txOut)))

setCapacityDepositLoop ::
  Int ->
  CoinPerByte ->
  DijkstraTxOut DijkstraEra ->
  DijkstraTxOut DijkstraEra
setCapacityDepositLoop attemptsLeft coinsPerUTxOByte txOut
  | attemptsLeft <= 0 = txOut
  | otherwise =
      setCapacityDepositStep
        attemptsLeft
        coinsPerUTxOByte
        (requiredCapacityDepositTxOut coinsPerUTxOByte (measureTxOut txOut))
        txOut

setCapacityDepositStep ::
  Int ->
  CoinPerByte ->
  CapacityDeposit ->
  DijkstraTxOut DijkstraEra ->
  DijkstraTxOut DijkstraEra
setCapacityDepositStep attemptsLeft coinsPerUTxOByte requiredDeposit txOut
  | requiredDeposit == fromCompact (dtoCapacityDeposit txOut) = txOut
  | otherwise =
      setCapacityDepositLoop
        (attemptsLeft - 1)
        coinsPerUTxOByte
        txOut {dtoCapacityDeposit = compactCapacityDepositOrError requiredDeposit}

instance
  (Era era, Val (Value era), ToJSON (Script era)) =>
  ToKeyValuePairs (DijkstraTxOut era)
  where
  toKeyValuePairs (DijkstraTxOut cAddr cVal cDep datum mRefScript) =
    [ "address" .= decompactAddr cAddr
    , "value" .= fromCompact cVal
    , "capacityDeposit" .= fromCompact cDep
    , "datum" .= datum
    , "referenceScript" .= mRefScript
    ]

deriving via
  KeyValuePairs (DijkstraTxOut era)
  instance
    (Era era, Val (Value era), ToJSON (Script era)) => ToJSON (DijkstraTxOut era)

-- | The Babbage output map extended with the mandatory key 4.
instance (EraScript era, Val (Value era)) => EncCBOR (DijkstraTxOut era) where
  encCBOR (DijkstraTxOut cAddr cVal cDep datum script) =
    encode $
      Keyed (\_ _ _ _ _ -> ())
        !> Key 0 (To cAddr)
        !> Key 1 (To (fromCompact cVal))
        !> Omit (== NoDatum) (Key 2 (To datum))
        !> encodeKeyedStrictMaybeWith 3 encodeNestedCbor script
        !> Key 4 (To cDep)

-- | Accumulator for the sparse map decoder.
data DecodingTxOut era = DecodingTxOut
  { decodingTxOutAddr :: !(StrictMaybe CompactAddr)
  , decodingTxOutVal :: !(StrictMaybe (CompactForm (Value era)))
  , decodingTxOutDeposit :: !(StrictMaybe (CompactForm CapacityDeposit))
  , decodingTxOutDatum :: !(Datum era)
  , decodingTxOutScript :: !(StrictMaybe (Script era))
  }

instance (EraScript era, Val (Value era)) => DecCBOR (DijkstraTxOut era) where
  -- The native form is the key-4 map. The merged legacy forms — the Babbage
  -- map without key 4 and the Alonzo array — decode with capacity deposit 0,
  -- a dead value under the exact deposit rule, so it unambiguously marks the
  -- output as implicit: the ledger derives its deposit when the output
  -- enters the UTxO ("Cardano.Ledger.Dijkstra.UTxO.Translation"). Accepting
  -- the merged forms is what keeps a transaction signed in the previous era
  -- valid across the boundary: signed bytes can never be rewritten, only
  -- reinterpreted.
  decCBOR =
    peekTokenType >>= \case
      TypeMapLen -> decodeMapTxOut
      TypeMapLen64 -> decodeMapTxOut
      TypeMapLenIndef -> decodeMapTxOut
      _ -> decodeMergedArrayTxOut
  {-# INLINEABLE decCBOR #-}

decodeMapTxOut ::
  (EraScript era, Val (Value era)) => Decoder s (DijkstraTxOut era)
decodeMapTxOut = do
  partialTxOut <-
    decode $
      SparseKeyed
        "DijkstraTxOut"
        emptyDecodingTxOut
        decodingTxOutField
        requiredDecodingTxOutFields
  case partialTxOut of
    DecodingTxOut (SJust cAddr) (SJust cVal) deposit datum script ->
      pure $! DijkstraTxOut cAddr cVal (statedOrImplicitDeposit deposit) datum script
    _ -> fail "DijkstraTxOut: missing required field"
{-# INLINEABLE decodeMapTxOut #-}

-- | An absent key 4 is the merged legacy map: the deposit is implicit.
statedOrImplicitDeposit ::
  StrictMaybe (CompactForm CapacityDeposit) -> CompactForm CapacityDeposit
statedOrImplicitDeposit SNothing = compactImplicitCapacityDeposit
statedOrImplicitDeposit (SJust deposit) = deposit

-- | The Alonzo array form @[address, value, ? datum_hash]@ — a merged
-- legacy output: all its ada in the assets, deposit implicit, no reference
-- script.
decodeMergedArrayTxOut ::
  Val (Value era) => Decoder s (DijkstraTxOut era)
decodeMergedArrayTxOut =
  decodeListLenOrIndef >>= \case
    Nothing -> do
      cAddr <- decCBOR
      cVal <- decCBOR
      decodeBreakOr >>= \case
        True -> pure $! implicitMergedTxOut cAddr cVal NoDatum
        False -> do
          dataHash <- decCBOR
          decodeBreakOr >>= \case
            True -> pure $! implicitMergedTxOut cAddr cVal (DatumHash dataHash)
            False -> cborError $ DecoderErrorCustom "DijkstraTxOut" "Excess terms in merged TxOut"
    Just 2 -> do
      cAddr <- decCBOR
      cVal <- decCBOR
      pure $! implicitMergedTxOut cAddr cVal NoDatum
    Just 3 -> do
      cAddr <- decCBOR
      cVal <- decCBOR
      dataHash <- decCBOR
      pure $! implicitMergedTxOut cAddr cVal (DatumHash dataHash)
    Just _ -> cborError $ DecoderErrorCustom "DijkstraTxOut" "Wrong number of terms in merged TxOut"
{-# INLINEABLE decodeMergedArrayTxOut #-}

implicitMergedTxOut ::
  CompactAddr -> CompactForm (Value era) -> Datum era -> DijkstraTxOut era
implicitMergedTxOut cAddr cVal datum =
  DijkstraTxOut cAddr cVal compactImplicitCapacityDeposit datum SNothing

emptyDecodingTxOut :: DecodingTxOut era
emptyDecodingTxOut = DecodingTxOut SNothing SNothing SNothing NoDatum SNothing

decodingTxOutField ::
  (EraScript era, Val (Value era)) => Word -> Field (DecodingTxOut era)
decodingTxOutField 0 =
  field (\x txOut -> txOut {decodingTxOutAddr = SJust x}) From
decodingTxOutField 1 =
  field (\x txOut -> txOut {decodingTxOutVal = SJust x}) From
decodingTxOutField 2 =
  field (\x txOut -> txOut {decodingTxOutDatum = x}) From
decodingTxOutField 3 =
  ofield
    (\x txOut -> txOut {decodingTxOutScript = x})
    (D $ decodeCIC "Script")
decodingTxOutField 4 =
  field (\x txOut -> txOut {decodingTxOutDeposit = SJust x}) From
decodingTxOutField n = invalidField n
{-# INLINE decodingTxOutField #-}

requiredDecodingTxOutFields :: [(Word, String)]
requiredDecodingTxOutFields =
  [ (0, "address")
  , (1, "value")
  ]

decodeCIC :: DecCBOR (Annotator b) => T.Text -> Decoder s b
decodeCIC typeLabel = do
  version <- getDecoderVersion
  nestedBytes <- decodeNestedCborBytes
  case decodeFullAnnotator version typeLabel decCBOR (LBS.fromStrict nestedBytes) of
    Left decoderError -> fail $ T.unpack typeLabel <> ": " <> show decoderError
    Right decoded -> pure decoded
{-# INLINEABLE decodeCIC #-}

instance
  ( EraScript era
  , Val (Value era)
  , MemPack (Script era)
  , MemPack (CompactForm (Value era))
  ) =>
  DecShareCBOR (DijkstraTxOut era)
  where
  type Share (DijkstraTxOut era) = Interns (Credential Staking)

  -- State serialization stores outputs as a MemPack bytestring, the wire
  -- format as the key-4 map. The address is kept compact, so there is no
  -- decompacted credential to intern.
  decShareCBOR _ =
    peekTokenType >>= \case
      TypeBytes -> decodeMemPack
      TypeBytesIndef -> decodeMemPack
      _ -> decCBOR
  {-# INLINEABLE decShareCBOR #-}

instance
  ( Era era
  , MemPack (Script era)
  , MemPack (CompactForm (Value era))
  ) =>
  MemPack (DijkstraTxOut era)
  where
  packedByteCount (DijkstraTxOut cAddr cVal cDep datum script) =
    packedByteCount cAddr
      + packedByteCount cVal
      + packedByteCount cDep
      + packedByteCount datum
      + packedTagByteCount
      + case script of
        SNothing -> 0
        SJust s -> packedByteCount s
  {-# INLINE packedByteCount #-}
  packM (DijkstraTxOut cAddr cVal cDep datum script) = do
    packM cAddr
    packM cVal
    packM cDep
    packM datum
    case script of
      SNothing -> packTagM 0
      SJust s -> packTagM 1 >> packM s
  {-# INLINE packM #-}
  unpackM = do
    cAddr <- unpackM
    cVal <- unpackM
    cDep <- unpackM
    datum <- unpackM
    script <-
      unpackM >>= \case
        0 -> pure SNothing
        1 -> SJust <$> unpackM
        n -> unknownTagM @(DijkstraTxOut era) n
    pure $! DijkstraTxOut cAddr cVal cDep datum script
  {-# INLINE unpackM #-}

instance EraTxOut DijkstraEra where
  type TxOut DijkstraEra = DijkstraTxOut DijkstraEra

  mkBasicTxOut addr assets = mkDijkstraTxOut addr assets mempty NoDatum SNothing

  -- Structural upgrade only: this signature provides no PParams, so the
  -- deposit cannot be computed here. Era translation immediately funds it
  -- with 'Cardano.Ledger.Dijkstra.TxOut.Translation.fundCapacityDeposit'; an
  -- upgraded output whose deposit was never
  -- funded must not enter the UTxO.
  upgradeTxOut txOut =
    DijkstraTxOut
      { dtoCompactAddr = either compactAddr id (txOut ^. Babbage.addrEitherBabbageTxOutL)
      , -- The explicit semantic conversion of the migration: a merged
        -- pre-Dijkstra value becomes pure application assets (its deposit is
        -- funded right after by the migration).
        dtoAssets =
          either
            (toCompactPartial . Assets)
            CompactAssets
            (txOut ^. Babbage.valueEitherBabbageTxOutL)
      , dtoCapacityDeposit = compactImplicitCapacityDeposit
      , dtoDatum = translateDatum (txOut ^. Babbage.datumBabbageTxOutL)
      , dtoRefScript = upgradeScript <$> (txOut ^. Babbage.referenceScriptBabbageTxOutL)
      }

  addrEitherTxOutL =
    lens
      (Right . dtoCompactAddr)
      ( \txOut -> \case
          Left addr -> txOut {dtoCompactAddr = compactAddr addr}
          Right cAddr -> txOut {dtoCompactAddr = cAddr}
      )
  {-# INLINE addrEitherTxOutL #-}

  valueEitherTxOutL =
    lens
      (Right . dtoAssets)
      ( \txOut -> \case
          Left val -> txOut {dtoAssets = toCompactPartial val}
          Right cVal -> txOut {dtoAssets = cVal}
      )
  {-# INLINE valueEitherTxOutL #-}

  -- The requirement M(o) is unchanged; how it is funded is what changed.
  -- This generic hook is a merged-world compatibility bridge, hence the
  -- bare Coin; the typed requirement is 'requiredCapacityDepositTxOut'.
  getMinCoinSizedTxOut pparams =
    unCapacityDeposit . requiredCapacityDepositTxOut (pparams ^. ppCoinsPerUTxOByteL)

instance AlonzoEraTxOut DijkstraEra where
  -- Mirrors Babbage's semantics exactly: an inline datum has NO data hash
  -- here. Returning its hash would widen the allowed supplemental-datum set
  -- and loosen `NotAllowedSupplementalDatums` relative to Babbage/Conway.
  dataHashTxOutL =
    lens
      (dataHashOfDatum . dtoDatum)
      ( \txOut -> \case
          SNothing -> txOut {dtoDatum = NoDatum}
          SJust dh -> txOut {dtoDatum = DatumHash dh}
      )
  {-# INLINE dataHashTxOutL #-}

  datumTxOutF = to dtoDatum
  {-# INLINE datumTxOutF #-}

-- | The data hash an output states, if any. An inline datum states none —
-- its hash must not enter the allowed supplemental-datum set (same rule as
-- Babbage's 'getDataHashBabbageTxOut').
dataHashOfDatum :: Datum era -> StrictMaybe DataHash
dataHashOfDatum NoDatum = SNothing
dataHashOfDatum (DatumHash dataHash) = SJust dataHash
dataHashOfDatum (Datum _) = SNothing

instance BabbageEraTxOut DijkstraEra where
  dataTxOutL =
    lens
      ( \txOut -> case dtoDatum txOut of
          Datum binaryData -> SJust (binaryDataToData binaryData)
          _ -> SNothing
      )
      ( \txOut -> \case
          SNothing -> txOut {dtoDatum = NoDatum}
          SJust d -> txOut {dtoDatum = Datum (dataToBinaryData d)}
      )
  {-# INLINE dataTxOutL #-}

  datumTxOutL = lens dtoDatum (\txOut datum -> txOut {dtoDatum = datum})
  {-# INLINE datumTxOutL #-}

  referenceScriptTxOutL =
    lens dtoRefScript (\txOut script -> txOut {dtoRefScript = script})
  {-# INLINE referenceScriptTxOutL #-}

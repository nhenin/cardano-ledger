{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Dijkstra owns its output allocation. Application assets and capacity
-- deposits are distinct fields of 'OutputValue'; generic value/coin lenses
-- access the application side, while the output-pot getters count both.
--
-- Native output maps carry key 4. Legacy maps and arrays omit the deposit and
-- select an implicit allocation lane. Presence is recorded separately from
-- the amount: an explicitly stated zero deposit remains explicit. Decoding
-- does not allocate ADA or rewrite the containing transaction's signed bytes.
module Cardano.Ledger.Dijkstra.TxOut (
  DijkstraTxOut (..),
  CapacityDepositForm (..),
  DijkstraEraTxOut (..),
  mkDijkstraTxOut,
  potValueTxOutF,
  potCoinsTxOutF,
  measureTxOut,
  requiredCapacityDepositTxOut,
) where

import Cardano.Base.Typeable (TypeName (TypeName))
import Cardano.Ledger.Address (
  Addr,
  CompactAddr,
  compactAddr,
  decompactAddr,
  fromCborBackwardsBothAddr,
  fromCborBothAddr,
 )
import Cardano.Ledger.Alonzo.Core (AlonzoEraTxOut (..))
import Cardano.Ledger.Babbage.Core (BabbageEraPParams, CoinPerByte, ppCoinsPerUTxOByteL)
import Cardano.Ledger.Babbage.TxOut (BabbageEraTxOut (..))
import qualified Cardano.Ledger.Babbage.TxOut as Babbage
import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import Cardano.Ledger.Binary (
  Annotator,
  DecCBOR (..),
  DecShareCBOR (..),
  Decoder,
  EncCBOR (..),
  Interns,
  Sized,
  TokenType (..),
  decodeBreakOr,
  decodeFullAnnotator,
  decodeListLenOrIndef,
  decodeMemPack,
  decodeNestedCborBytes,
  decodeSparseKeyed,
  encodeListLen,
  encodeNestedCbor,
  getDecoderVersion,
  mkSized,
  peekTokenType,
  sizedSize,
  sizedValue,
 )
import Cardano.Ledger.Binary.Coders (
  Encode (..),
  encode,
  encodeKeyedStrictMaybeWith,
  (!>),
 )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Compactible (Compactible (..), toCompactPartial)
import Cardano.Ledger.Core (Era, EraScript (..), EraTxOut (..), PParams, eraProtVerLow)
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Dijkstra.PParams ()
import Cardano.Ledger.Dijkstra.Scripts ()
import Cardano.Ledger.Dijkstra.TxOut.ApplicationAssets (
  ApplicationAssets,
  CompactForm (CompactApplicationAssets),
  fromMaryRepresentation,
 )
import Cardano.Ledger.Dijkstra.TxOut.CapacityDeposit (
  CapacityDeposit (..),
  depositFieldGrowthAllowance,
  requiredCapacityDeposit,
 )
import Cardano.Ledger.Dijkstra.TxOut.Value (
  CompactForm (..),
  OutputValue,
  compactOutputCoins,
 )
import qualified Cardano.Ledger.Dijkstra.TxOut.Value.Translation as OutputValue
import Cardano.Ledger.Hashes (DataHash)
import Cardano.Ledger.Keys (KeyRole (Staking))
import Cardano.Ledger.Mary.Value (MaryValue)
import Cardano.Ledger.Plutus.Data (
  Datum (..),
  binaryDataToData,
  dataToBinaryData,
  translateDatum,
 )
import Control.DeepSeq (NFData (..), rwhnf)
import Data.Aeson (ToJSON (..), object, (.=))
import qualified Data.ByteString.Lazy as LBS
import Data.MemPack (MemPack (..), packTagM, packedTagByteCount, unknownTagM, unpackTagM)
import GHC.Generics (Generic)
import Lens.Micro (Lens', lens, to, (^.))
import NoThunks.Class (NoThunks)

-- | Whether capacity was stated, independently of its numeric amount.
data CapacityDepositForm = ExplicitCapacityDeposit | ImplicitCapacityDeposit
  deriving (Eq, Ord, Show, Generic)

instance NFData CapacityDepositForm

instance NoThunks CapacityDepositForm

-- | Compact allocation storage belongs to Dijkstra, without nesting another
-- era's output. Implicit form is transient: entry translation allocates it.
data DijkstraTxOut era = DijkstraTxOut
  { dtoCompactAddr :: !CompactAddr
  , dtoOutputValue :: !(CompactForm OutputValue)
  , dtoCapacityDepositForm :: !CapacityDepositForm
  , dtoDatum :: !(Datum era)
  , dtoRefScript :: !(StrictMaybe (Script era))
  }
  deriving (Generic)

deriving instance Eq (Script era) => Eq (DijkstraTxOut era)

deriving instance Ord (Script era) => Ord (DijkstraTxOut era)

deriving instance Show (Script era) => Show (DijkstraTxOut era)

-- Datum's strict payloads are hashes or compact bytes; forcing its constructor
-- evaluates that field completely. Scripts retain their own NFData contract.
instance NFData (Script era) => NFData (DijkstraTxOut era) where
  rnf out =
    rnf (dtoCompactAddr out) `seq`
      rnf (dtoOutputValue out) `seq`
        rnf (dtoCapacityDepositForm out) `seq`
          rwhnf (dtoDatum out) `seq`
            rnf (dtoRefScript out)

instance (Era era, NoThunks (Script era)) => NoThunks (DijkstraTxOut era)

class (BabbageEraTxOut era, BabbageEraPParams era) => DijkstraEraTxOut era where
  compactOutputValueTxOutL :: Lens' (TxOut era) (CompactForm OutputValue)

  -- | An implicit output must have zero allocated capacity. Component lenses
  -- preserve this marker; callers completing an explicit allocation must set
  -- 'ExplicitCapacityDeposit' before serialization.
  capacityDepositFormTxOutL :: Lens' (TxOut era) CapacityDepositForm

  -- | Raw tariff for the canonical explicit storage form. Unlike the
  -- compatibility minimum-coin helper, this prices capacity independently of
  -- the application's ADA minimum and adds no implicit growth allowance.
  getCapacityDepositRequirement :: PParams era -> TxOut era -> CapacityDeposit

  -- | Replacing an allocation preserves its wire-form classification.
  outputValueTxOutL :: Lens' (TxOut era) OutputValue
  outputValueTxOutL =
    compactOutputValueTxOutL . lens fromCompact (\_ -> toCompactPartial)

  -- | Component setters leave the other allocation and presence unchanged.
  capacityDepositTxOutL :: Lens' (TxOut era) CapacityDeposit
  capacityDepositTxOutL =
    compactOutputValueTxOutL
      . lens
        (fromCompact . compactCapacityDeposit)
        (\allocation deposit -> allocation {compactCapacityDeposit = toCompactPartial deposit})

  applicationAssetsTxOutL :: Lens' (TxOut era) ApplicationAssets
  applicationAssetsTxOutL =
    compactOutputValueTxOutL
      . lens
        (fromCompact . compactApplicationAssets)
        (\allocation assets -> allocation {compactApplicationAssets = toCompactPartial assets})

instance DijkstraEraTxOut DijkstraEra where
  getCapacityDepositRequirement pparams =
    requiredCapacityDepositTxOut (pparams ^. ppCoinsPerUTxOByteL) . measureTxOut
  compactOutputValueTxOutL = lens dtoOutputValue (\out allocation -> out {dtoOutputValue = allocation})
  capacityDepositFormTxOutL =
    lens dtoCapacityDepositForm (\out form -> out {dtoCapacityDepositForm = form})

-- | Construct a native output with an explicitly supplied allocation.
mkDijkstraTxOut :: Addr -> OutputValue -> Datum era -> StrictMaybe (Script era) -> DijkstraTxOut era
mkDijkstraTxOut addr allocation datum script =
  DijkstraTxOut (compactAddr addr) (toCompactPartial allocation) ExplicitCapacityDeposit datum script

-- | Total ADA without expanding the native-asset representation.
potCoinsDijkstraTxOut :: DijkstraTxOut era -> Coin
potCoinsDijkstraTxOut = compactOutputCoins . dtoOutputValue

-- | The tariff prices the canonical stored representation, including the
-- explicit deposit key. This does not mutate the transaction's signed form.
measureTxOut :: forall era. EraScript era => DijkstraTxOut era -> Sized (DijkstraTxOut era)
measureTxOut out = mkSized (eraProtVerLow @era) out {dtoCapacityDepositForm = ExplicitCapacityDeposit}

requiredCapacityDepositTxOut :: CoinPerByte -> Sized (DijkstraTxOut era) -> CapacityDeposit
requiredCapacityDepositTxOut price = requiredCapacityDeposit price . sizedSize

instance (Era era, ToJSON (Script era)) => ToJSON (DijkstraTxOut era) where
  toJSON out =
    object
      [ "address" .= decompactAddr (dtoCompactAddr out)
      , "outputValue" .= fromCompact (dtoOutputValue out)
      , "capacityDepositForm" .= show (dtoCapacityDepositForm out)
      , "datum" .= dtoDatum out
      , "referenceScript" .= dtoRefScript out
      ]

instance EraScript era => EncCBOR (DijkstraTxOut era) where
  encCBOR out
    | dtoCapacityDepositForm out == ImplicitCapacityDeposit
        && fromCompact (compactCapacityDeposit (dtoOutputValue out)) /= CapacityDeposit (Coin 0) =
        error "DijkstraTxOut: implicit output carries an allocated capacity deposit"
    | otherwise =
        case (dtoCapacityDepositForm out, dtoDatum out, dtoRefScript out) of
          (ImplicitCapacityDeposit, NoDatum, SNothing) ->
            encodeListLen 2
              <> encCBOR (dtoCompactAddr out)
              <> encCBOR (compactApplicationAssets (dtoOutputValue out))
          (ImplicitCapacityDeposit, DatumHash dataHash, SNothing) ->
            encodeListLen 3
              <> encCBOR (dtoCompactAddr out)
              <> encCBOR (compactApplicationAssets (dtoOutputValue out))
              <> encCBOR dataHash
          _ -> encodeMap
    where
      -- Preserve the historical canonical choice of merged arrays or maps.
      -- Native allocations always use the explicit key-4 map.
      encodeMap =
        encode $
          Keyed (\_ _ _ _ _ -> ())
            !> Key 0 (To (dtoCompactAddr out))
            !> Key 1 (To (compactApplicationAssets (dtoOutputValue out)))
            !> Omit (== NoDatum) (Key 2 (To (dtoDatum out)))
            !> encodeKeyedStrictMaybeWith 3 encodeNestedCbor (dtoRefScript out)
            !> Omit
              (const (dtoCapacityDepositForm out == ImplicitCapacityDeposit))
              (Key 4 (To (compactCapacityDeposit (dtoOutputValue out))))

data DecodingTxOut era = DecodingTxOut
  { decodingAddress :: !(StrictMaybe CompactAddr)
  , decodingAssets :: !(CompactForm ApplicationAssets)
  , decodingDeposit :: !(StrictMaybe (CompactForm CapacityDeposit))
  , decodingDatum :: !(Datum era)
  , decodingScript :: !(StrictMaybe (Script era))
  }
  deriving (Generic)

instance EraScript era => DecCBOR (DijkstraTxOut era) where
  decCBOR = decodeDijkstraTxOut fromCborBothAddr

decodeDijkstraTxOut ::
  forall era s.
  EraScript era =>
  (forall s'. Decoder s' (Addr, CompactAddr)) ->
  Decoder s (DijkstraTxOut era)
decodeDijkstraTxOut decodeAddress =
  peekTokenType >>= \case
    TypeMapLen -> decodeMap
    TypeMapLen64 -> decodeMap
    TypeMapLenIndef -> decodeMap
    _ -> decodeArray
  where
    decodeMap = do
      partial <- decodeSparseKeyed TypeName [(0, "address"), (1, "applicationAssets")] initial decodeField
      case partial of
        DecodingTxOut (SJust addr) assets deposit datum script ->
          pure $ case deposit of
            SNothing -> implicitOutput addr assets datum script
            SJust stated -> DijkstraTxOut addr (CompactOutputValue stated assets) ExplicitCapacityDeposit datum script
        _ -> fail "DijkstraTxOut: missing address"
    initial =
      DecodingTxOut
        SNothing
        (CompactApplicationAssets (toCompactPartial (mempty :: MaryValue)))
        SNothing
        NoDatum
        SNothing
    decodeField :: DecodingTxOut era -> Word -> Maybe (Decoder s (DecodingTxOut era))
    decodeField partial = \case
      0 -> Just $ do
        addr <- snd <$> decodeAddress
        pure partial {decodingAddress = SJust addr}
      1 -> Just $ do
        assets <- decCBOR
        pure partial {decodingAssets = assets}
      2 -> Just $ do
        datum <- decCBOR
        pure partial {decodingDatum = datum}
      3 -> Just $ do
        script <- decodeReferenceScript
        pure partial {decodingScript = SJust script}
      4 -> Just $ do
        deposit <- decCBOR
        pure partial {decodingDeposit = SJust deposit}
      _ -> Nothing
    decodeArray = do
      lengthOrIndefinite <- decodeListLenOrIndef
      addr <- snd <$> decodeAddress
      assets <- decCBOR
      case lengthOrIndefinite of
        Just 2 -> pure $ implicitOutput addr assets NoDatum SNothing
        Just 3 -> do
          dataHash <- decCBOR
          pure $ implicitOutput addr assets (DatumHash dataHash) SNothing
        Nothing ->
          decodeBreakOr >>= \case
            True -> pure $ implicitOutput addr assets NoDatum SNothing
            False -> do
              dataHash <- decCBOR
              decodeBreakOr >>= \case
                True -> pure $ implicitOutput addr assets (DatumHash dataHash) SNothing
                False -> fail "DijkstraTxOut: excess terms in merged output"
        Just _ -> fail "DijkstraTxOut: wrong number of terms in merged output"

implicitOutput ::
  CompactAddr ->
  CompactForm ApplicationAssets ->
  Datum era ->
  StrictMaybe (Script era) ->
  DijkstraTxOut era
implicitOutput addr assets datum script =
  DijkstraTxOut
    addr
    (CompactOutputValue (toCompactPartial (CapacityDeposit (Coin 0))) assets)
    ImplicitCapacityDeposit
    datum
    script

decodeReferenceScript :: DecCBOR (Annotator script) => Decoder s script
decodeReferenceScript = do
  version <- getDecoderVersion
  bytes <- decodeNestedCborBytes
  case decodeFullAnnotator version "Script" decCBOR (LBS.fromStrict bytes) of
    Left decoderError -> fail ("Script: " <> show decoderError)
    Right script -> pure script

instance (EraScript era, MemPack (Script era)) => DecShareCBOR (DijkstraTxOut era) where
  type Share (DijkstraTxOut era) = Interns (Credential Staking)

  -- Compact addresses have no unpacked credentials to intern. State storage
  -- uses the MemPack-bytes path in addition to historical CBOR snapshots.
  decShareCBOR _ =
    peekTokenType >>= \case
      TypeBytes -> decodeMemPack
      TypeBytesIndef -> decodeMemPack
      _ -> decodeDijkstraTxOut fromCborBackwardsBothAddr

instance (Era era, MemPack (Script era)) => MemPack (DijkstraTxOut era) where
  packedByteCount out =
    packedByteCount (dtoCompactAddr out)
      + packedByteCount (dtoOutputValue out)
      + packedTagByteCount
      + packedByteCount (dtoDatum out)
      + packedTagByteCount
      + case dtoRefScript out of
        SNothing -> 0
        SJust script -> packedByteCount script
  packM out = do
    packM (dtoCompactAddr out)
    packM (dtoOutputValue out)
    packTagM $ case dtoCapacityDepositForm out of
      ExplicitCapacityDeposit -> 0
      ImplicitCapacityDeposit -> 1
    packM (dtoDatum out)
    case dtoRefScript out of
      SNothing -> packTagM 0
      SJust script -> packTagM 1 >> packM script
  unpackM = do
    addr <- unpackM
    allocation <- unpackM
    form <-
      unpackTagM >>= \case
        0 -> pure ExplicitCapacityDeposit
        1 -> pure ImplicitCapacityDeposit
        tag -> unknownTagM @(DijkstraTxOut era) tag
    datum <- unpackM
    script <-
      unpackTagM >>= \case
        0 -> pure SNothing
        1 -> SJust <$> unpackM
        tag -> unknownTagM @(DijkstraTxOut era) tag
    pure $ DijkstraTxOut addr allocation form datum script

instance EraTxOut DijkstraEra where
  type TxOut DijkstraEra = DijkstraTxOut DijkstraEra

  mkBasicTxOut addr assets =
    implicitOutput
      (compactAddr addr)
      (toCompactPartial (fromMaryRepresentation assets))
      NoDatum
      SNothing

  -- Structural conversion only. Parameterized era/entry translation allocates
  -- capacity before this transient implicit output enters the stored UTxO.
  upgradeTxOut out =
    implicitOutput
      (either compactAddr id (out ^. Babbage.addrEitherBabbageTxOutL))
      (CompactApplicationAssets (either toCompactPartial id (out ^. Babbage.valueEitherBabbageTxOutL)))
      (translateDatum (out ^. Babbage.datumBabbageTxOutL))
      (upgradeScript <$> (out ^. Babbage.referenceScriptBabbageTxOutL))

  addrEitherTxOutL =
    lens (Right . dtoCompactAddr) (\out addr -> out {dtoCompactAddr = either compactAddr id addr})

  -- The generic mutable value is application assets, never the total pot.
  valueEitherTxOutL =
    lens
      ( \out -> case compactApplicationAssets (dtoOutputValue out) of
          CompactApplicationAssets assets -> Right assets
      )
      ( \out assets ->
          out
            { dtoOutputValue =
                (dtoOutputValue out)
                  { compactApplicationAssets = CompactApplicationAssets (either toCompactPartial id assets)
                  }
            }
      )

  potValueTxOutF = to (OutputValue.toMaryValue . fromCompact . dtoOutputValue)
  potCoinsTxOutF = to potCoinsDijkstraTxOut
  compactPotCoinsTxOutF = to (toCompactPartial . potCoinsDijkstraTxOut)

  -- The generic minimum applies to the application coin lens. Explicit outputs
  -- may carry zero application ADA: capacity has its own requirement and rule.
  -- Implicit outputs still fund the combined floor and deposit-field growth.
  --
  -- Consequently ensureMinCoinTxOut preserves an explicit allocation, while
  -- setMinCoinTxOut sets only its application ADA to zero. Neither helper funds,
  -- validates or reprices its capacity deposit. After an application edit that
  -- changes output size, the caller must separately check the capacity tariff.
  getMinCoinSizedTxOut pparams = getMinCoinTxOut pparams . sizedValue
  getMinCoinTxOut pparams out =
    case dtoCapacityDepositForm out of
      ExplicitCapacityDeposit -> Coin 0
      ImplicitCapacityDeposit ->
        unCapacityDeposit (getCapacityDepositRequirement pparams out)
          <> depositFieldGrowthAllowance (pparams ^. ppCoinsPerUTxOByteL)

instance AlonzoEraTxOut DijkstraEra where
  dataHashTxOutL =
    lens
      (dataHashOfDatum . dtoDatum)
      ( \out -> \case
          SNothing -> out {dtoDatum = NoDatum}
          SJust dataHash -> out {dtoDatum = DatumHash dataHash}
      )
  datumTxOutF = to dtoDatum

-- Inline data does not count as a stated hash, matching Babbage.
dataHashOfDatum :: Datum era -> StrictMaybe DataHash
dataHashOfDatum (DatumHash dataHash) = SJust dataHash
dataHashOfDatum _ = SNothing

instance BabbageEraTxOut DijkstraEra where
  dataTxOutL =
    lens
      ( \out -> case dtoDatum out of
          Datum binaryData -> SJust (binaryDataToData binaryData)
          _ -> SNothing
      )
      ( \out -> \case
          SNothing -> out {dtoDatum = NoDatum}
          SJust datum -> out {dtoDatum = Datum (dataToBinaryData datum)}
      )
  datumTxOutL = lens dtoDatum (\out datum -> out {dtoDatum = datum})
  referenceScriptTxOutL = lens dtoRefScript (\out script -> out {dtoRefScript = script})

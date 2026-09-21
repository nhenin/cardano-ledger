{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- | Represent Dijkstra outputs with separate capacity deposits and application assets.
-- Import "Cardano.Ledger.Dijkstra.TxOut.LedgerInstances" to use the Ledger output interfaces.
module Cardano.Ledger.Dijkstra.TxOut (
  -- * Dijkstra output representation and projections
  DijkstraTxOut (DijkstraTxOut),
  capacityDepositTxOutF,
  fromBabbageTxOut,
  toBabbageTxOut,
) where

import Cardano.Ledger.Address (Addr (..), CompactAddr, compactAddr, decompactAddr)
import Cardano.Ledger.Alonzo.TxBody (
  Addr28Extra,
  DataHash32,
  decodeAddress28,
  decodeDataHash32,
  encodeAddress28,
  encodeDataHash32,
 )
import qualified Cardano.Ledger.Babbage.TxOut as Babbage
import Cardano.Ledger.Binary (
  DecCBOR (..),
  DecShareCBOR (..),
  EncCBOR (..),
  Interns,
  TokenType (..),
  decodeMemPack,
  interns,
  peekTokenType,
 )
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Compactible (Compactible (..))
import Cardano.Ledger.Core (Script)
import Cardano.Ledger.Credential (Credential, StakeReference (StakeRefBase))
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Dijkstra.Scripts ()
import Cardano.Ledger.Dijkstra.TxOut.ApplicationAssets (ApplicationAssets, applicationCoins)
import Cardano.Ledger.Dijkstra.TxOut.CapacityDeposit (CapacityDeposit)
import Cardano.Ledger.Dijkstra.TxOut.Codec (decodeDijkstraTxOut, encodeDijkstraTxOut)
import Cardano.Ledger.Dijkstra.TxOut.Value (OutputValue (..))
import Cardano.Ledger.Dijkstra.TxOut.Value.Translation (AllocationError, fromMaryValue, toMaryValue)
import Cardano.Ledger.Hashes (DataHash, KeyRole (Staking))
import Cardano.Ledger.Plutus (BinaryData, Datum (..))
import qualified Cardano.Ledger.Val as Val
import Control.DeepSeq (NFData (rnf), rwhnf)
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Maybe (fromMaybe)
import Data.Maybe.Strict (StrictMaybe (..))
import Data.MemPack (MemPack (..), packTagM, packedTagByteCount, unknownTagM, unpackTagM)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Lens.Micro (SimpleGetter, to, (^.))
import NoThunks.Class (NoThunks)

-- | Store the supplied capacity deposit separately from application assets.
-- The existing compact alternatives retain address sharing and ADA-only storage.
-- The public pattern presents both monetary components as an 'OutputValue'.
data DijkstraTxOut
  = TxOutCompact'
      {-# UNPACK #-} !CompactAddr
      !CapacityDeposit
      !(CompactForm ApplicationAssets)
  | TxOutCompactDH'
      {-# UNPACK #-} !CompactAddr
      !CapacityDeposit
      !(CompactForm ApplicationAssets)
      !DataHash
  | TxOutCompactDatum
      {-# UNPACK #-} !CompactAddr
      !CapacityDeposit
      !(CompactForm ApplicationAssets)
      {-# UNPACK #-} !(BinaryData DijkstraEra) -- Inline data
  | TxOutCompactRefScript
      {-# UNPACK #-} !CompactAddr
      !CapacityDeposit
      !(CompactForm ApplicationAssets)
      !(Datum DijkstraEra)
      !(Script DijkstraEra)
  | TxOut_AddrHash28_AdaOnly
      !(Credential Staking)
      {-# UNPACK #-} !Addr28Extra
      !CapacityDeposit
      {-# UNPACK #-} !(CompactForm Coin) -- Application ADA
  | TxOut_AddrHash28_AdaOnly_DataHash32
      !(Credential Staking)
      {-# UNPACK #-} !Addr28Extra
      !CapacityDeposit
      {-# UNPACK #-} !(CompactForm Coin) -- Application ADA
      {-# UNPACK #-} !DataHash32
  deriving stock (Eq, Ord, Generic)

instance NFData DijkstraTxOut where
  rnf = rwhnf

instance NoThunks DijkstraTxOut

instance Show DijkstraTxOut where
  showsPrec precedence (DijkstraTxOut addr allocation datum script) =
    showParen (precedence > 10) $
      showString "DijkstraTxOut "
        . showsPrec 11 addr
        . showChar ' '
        . showsPrec 11 allocation
        . showChar ' '
        . showsPrec 11 datum
        . showChar ' '
        . showsPrec 11 script

instance ToJSON DijkstraTxOut where
  toJSON (DijkstraTxOut addr (OutputValue deposit assets) datum script) =
    object
      [ "address" .= addr
      , "capacityDeposit" .= deposit
      , "applicationAssets" .= assets
      , "datum" .= datum
      , "referenceScript" .= script
      ]

instance EncCBOR DijkstraTxOut where
  encCBOR (DijkstraTxOut addr allocation datum script) =
    encodeDijkstraTxOut addr allocation datum script

instance DecCBOR DijkstraTxOut where
  decCBOR = do
    (addr, allocation, datum, script) <- decodeDijkstraTxOut
    pure $ DijkstraTxOut addr allocation datum script

-- | Tag 6 distinguishes allocated outputs from the legacy tags 0 through 5.
-- The application payload retains its exact compact storage variant.
instance MemPack DijkstraTxOut where
  packedByteCount txOut =
    packedTagByteCount
      + packedByteCount (txOut ^. capacityDepositTxOutF)
      + packedTagByteCount
      + case txOut of
        TxOutCompact' address _ assets -> packedByteCount address + packedByteCount assets
        TxOutCompactDH' address _ assets datumHash ->
          packedByteCount address + packedByteCount assets + packedByteCount datumHash
        TxOutCompactDatum address _ assets datum ->
          packedByteCount address + packedByteCount assets + packedByteCount datum
        TxOutCompactRefScript address _ assets datum script ->
          packedByteCount address + packedByteCount assets + packedByteCount datum + packedByteCount script
        TxOut_AddrHash28_AdaOnly credential address _ coins ->
          packedByteCount credential + packedByteCount address + packedByteCount coins
        TxOut_AddrHash28_AdaOnly_DataHash32 credential address _ coins datumHash ->
          packedByteCount credential
            + packedByteCount address
            + packedByteCount coins
            + packedByteCount datumHash
  packM txOut = do
    packTagM 6
    packM (txOut ^. capacityDepositTxOutF)
    case txOut of
      TxOutCompact' address _ assets -> packTagM 0 >> packM address >> packM assets
      TxOutCompactDH' address _ assets datumHash ->
        packTagM 1 >> packM address >> packM assets >> packM datumHash
      TxOut_AddrHash28_AdaOnly credential address _ coins ->
        packTagM 2 >> packM credential >> packM address >> packM coins
      TxOut_AddrHash28_AdaOnly_DataHash32 credential address _ coins datumHash ->
        packTagM 3 >> packM credential >> packM address >> packM coins >> packM datumHash
      TxOutCompactDatum address _ assets datum ->
        packTagM 4 >> packM address >> packM assets >> packM datum
      TxOutCompactRefScript address _ assets datum script ->
        packTagM 5 >> packM address >> packM assets >> packM datum >> packM script
  unpackM =
    unpackTagM >>= \case
      6 -> do
        deposit <- unpackM
        unpackTagM >>= \case
          0 -> TxOutCompact' <$> unpackM <*> pure deposit <*> unpackM
          1 -> TxOutCompactDH' <$> unpackM <*> pure deposit <*> unpackM <*> unpackM
          2 -> TxOut_AddrHash28_AdaOnly <$> unpackM <*> unpackM <*> pure deposit <*> unpackM
          3 ->
            TxOut_AddrHash28_AdaOnly_DataHash32 <$> unpackM <*> unpackM <*> pure deposit <*> unpackM <*> unpackM
          4 -> TxOutCompactDatum <$> unpackM <*> pure deposit <*> unpackM <*> unpackM
          5 -> TxOutCompactRefScript <$> unpackM <*> pure deposit <*> unpackM <*> unpackM <*> unpackM
          tag -> unknownTagM @DijkstraTxOut tag
      tag -> unknownTagM @DijkstraTxOut tag

instance DecShareCBOR DijkstraTxOut where
  type Share DijkstraTxOut = Interns (Credential Staking)
  decShareCBOR credentials = do
    txOut <-
      peekTokenType >>= \case
        TypeBytes -> decodeMemPack
        TypeBytesIndef -> decodeMemPack
        _ -> decCBOR
    pure $! internDijkstraTxOut (interns credentials) txOut

-- | Construct and inspect an output with its explicitly supplied allocation.
-- Construction preserves both components without calculating a deposit.
pattern DijkstraTxOut ::
  HasCallStack =>
  Addr -> OutputValue -> Datum DijkstraEra -> StrictMaybe (Script DijkstraEra) -> DijkstraTxOut
pattern DijkstraTxOut addr allocation datum script <-
  (viewDijkstraTxOut -> (addr, allocation, datum, script))
  where
    DijkstraTxOut addr allocation datum script =
      compactDijkstraTxOut addr allocation datum script

{-# COMPLETE DijkstraTxOut #-}

-- | Read the allocation stored in the output, independently of pricing rules.
capacityDepositTxOutF :: SimpleGetter DijkstraTxOut CapacityDeposit
capacityDepositTxOutF = to $ \case
  TxOutCompact' _ deposit _ -> deposit
  TxOutCompactDH' _ deposit _ _ -> deposit
  TxOutCompactDatum _ deposit _ _ -> deposit
  TxOutCompactRefScript _ deposit _ _ _ -> deposit
  TxOut_AddrHash28_AdaOnly _ _ deposit _ -> deposit
  TxOut_AddrHash28_AdaOnly_DataHash32 _ _ deposit _ _ -> deposit
{-# INLINE capacityDepositTxOutF #-}

-- | Recover the supplied deposit from a Babbage-shaped output's total value.
-- Preserve its quantities and fields; Dijkstra selects the compact storage variant.
fromBabbageTxOut ::
  CapacityDeposit -> Babbage.BabbageTxOut DijkstraEra -> Either AllocationError DijkstraTxOut
fromBabbageTxOut deposit (Babbage.BabbageTxOut address value datum script) =
  (\allocation -> DijkstraTxOut address allocation datum script)
    <$> fromMaryValue deposit value
{-# INLINE fromBabbageTxOut #-}

-- | Merge the deposit and application assets into a Babbage-shaped total value.
-- Preserve its fields; recovering the same allocation requires supplying
-- the retained deposit to 'fromBabbageTxOut'.
toBabbageTxOut :: DijkstraTxOut -> Babbage.BabbageTxOut DijkstraEra
toBabbageTxOut (DijkstraTxOut address allocation datum script) =
  Babbage.BabbageTxOut address (toMaryValue allocation) datum script
{-# INLINE toBabbageTxOut #-}

-- Private helpers

internDijkstraTxOut :: (Credential Staking -> Credential Staking) -> DijkstraTxOut -> DijkstraTxOut
internDijkstraTxOut internCredential = \case
  TxOut_AddrHash28_AdaOnly credential address deposit coins ->
    TxOut_AddrHash28_AdaOnly (internCredential credential) address deposit coins
  TxOut_AddrHash28_AdaOnly_DataHash32 credential address deposit coins datumHash ->
    TxOut_AddrHash28_AdaOnly_DataHash32 (internCredential credential) address deposit coins datumHash
  txOut -> txOut

-- | Select the compact storage variant without changing the supplied allocation.
compactDijkstraTxOut ::
  HasCallStack =>
  Addr -> OutputValue -> Datum DijkstraEra -> StrictMaybe (Script DijkstraEra) -> DijkstraTxOut
compactDijkstraTxOut address (OutputValue deposit assets) NoDatum SNothing
  | Val.isAdaOnly assets
  , Just compactCoins <- toCompact (applicationCoins assets)
  , Addr network paymentCredential (StakeRefBase stakingCredential) <- address =
      TxOut_AddrHash28_AdaOnly
        stakingCredential
        (encodeAddress28 network paymentCredential)
        deposit
        compactCoins
compactDijkstraTxOut address (OutputValue deposit assets) (DatumHash datumHash) SNothing
  | Val.isAdaOnly assets
  , Just compactCoins <- toCompact (applicationCoins assets)
  , Addr network paymentCredential (StakeRefBase stakingCredential) <- address =
      TxOut_AddrHash28_AdaOnly_DataHash32
        stakingCredential
        (encodeAddress28 network paymentCredential)
        deposit
        compactCoins
        (encodeDataHash32 datumHash)
compactDijkstraTxOut address (OutputValue deposit assets) datum script =
  let compactAddress = compactAddr address
      compactAssets =
        fromMaybe (error ("Illegal ApplicationAssets in DijkstraTxOut: " <> show assets)) $
          toCompact assets
   in case (datum, script) of
        (NoDatum, SNothing) -> TxOutCompact' compactAddress deposit compactAssets
        (DatumHash datumHash, SNothing) -> TxOutCompactDH' compactAddress deposit compactAssets datumHash
        (Datum binaryData, SNothing) -> TxOutCompactDatum compactAddress deposit compactAssets binaryData
        (_, SJust referenceScript) ->
          TxOutCompactRefScript compactAddress deposit compactAssets datum referenceScript

viewDijkstraTxOut ::
  DijkstraTxOut ->
  (Addr, OutputValue, Datum DijkstraEra, StrictMaybe (Script DijkstraEra))
viewDijkstraTxOut = \case
  TxOutCompact' address deposit assets ->
    (decompactAddr address, OutputValue deposit (fromCompact assets), NoDatum, SNothing)
  TxOutCompactDH' address deposit assets datumHash ->
    (decompactAddr address, OutputValue deposit (fromCompact assets), DatumHash datumHash, SNothing)
  TxOutCompactDatum address deposit assets binaryData ->
    (decompactAddr address, OutputValue deposit (fromCompact assets), Datum binaryData, SNothing)
  TxOutCompactRefScript address deposit assets datum script ->
    (decompactAddr address, OutputValue deposit (fromCompact assets), datum, SJust script)
  TxOut_AddrHash28_AdaOnly credential address deposit coins ->
    ( decodeAddress28 credential address
    , OutputValue deposit (Val.inject (fromCompact coins))
    , NoDatum
    , SNothing
    )
  TxOut_AddrHash28_AdaOnly_DataHash32 credential address deposit coins datumHash ->
    ( decodeAddress28 credential address
    , OutputValue deposit (Val.inject (fromCompact coins))
    , DatumHash (decodeDataHash32 datumHash)
    , SNothing
    )

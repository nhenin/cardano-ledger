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

import Cardano.Ledger.Address (Addr, CompactAddr)
import Cardano.Ledger.Alonzo.TxBody (Addr28Extra, DataHash32)
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
import Cardano.Ledger.Compactible (CompactForm)
import Cardano.Ledger.Core (Script)
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Dijkstra.Scripts ()
import Cardano.Ledger.Dijkstra.TxOut.ApplicationAssets (ApplicationAssets)
import Cardano.Ledger.Dijkstra.TxOut.CapacityDeposit (CapacityDeposit)
import Cardano.Ledger.Dijkstra.TxOut.Codec (decodeDijkstraTxOut, encodeDijkstraTxOut)
import Cardano.Ledger.Dijkstra.TxOut.Value (OutputValue (..))
import Cardano.Ledger.Hashes (DataHash, KeyRole (Staking))
import Cardano.Ledger.Plutus (BinaryData, Datum (..))
import Control.DeepSeq (NFData (rnf), rwhnf)
import Data.Aeson (ToJSON (..), object, (.=))
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
      + packedByteCount (toBabbageTxOut txOut)
  packM txOut = do
    packTagM 6
    packM (txOut ^. capacityDepositTxOutF)
    packM (toBabbageTxOut txOut)
  unpackM =
    unpackTagM >>= \case
      6 -> fromBabbageTxOut <$> unpackM <*> unpackM
      tag -> unknownTagM @DijkstraTxOut tag

instance DecShareCBOR DijkstraTxOut where
  type Share DijkstraTxOut = Interns (Credential Staking)
  decShareCBOR credentials = do
    txOut <-
      peekTokenType >>= \case
        TypeBytes -> decodeMemPack
        TypeBytesIndef -> decodeMemPack
        _ -> decCBOR
    pure $!
      fromBabbageTxOut
        (txOut ^. capacityDepositTxOutF)
        (Babbage.internBabbageTxOut (interns credentials) (toBabbageTxOut txOut))

-- | Construct and inspect an output with its explicitly supplied allocation.
-- Construction preserves both components without calculating a deposit.
pattern DijkstraTxOut ::
  HasCallStack =>
  Addr -> OutputValue -> Datum DijkstraEra -> StrictMaybe (Script DijkstraEra) -> DijkstraTxOut
pattern DijkstraTxOut addr allocation datum script <-
  (viewDijkstraTxOut -> (addr, allocation, datum, script))
  where
    DijkstraTxOut addr (OutputValue deposit assets) datum script =
      fromBabbageTxOut deposit (Babbage.BabbageTxOut addr assets datum script)

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

-- | Attach an explicit deposit to a Babbage-shaped application projection.
-- Copy compact storage without normalizing assets or addresses. Since its value
-- is 'ApplicationAssets', the input does not represent an unsplit legacy output.
fromBabbageTxOut :: CapacityDeposit -> Babbage.BabbageTxOut DijkstraEra -> DijkstraTxOut
fromBabbageTxOut deposit = \case
  Babbage.TxOutCompact' addr assets -> TxOutCompact' addr deposit assets
  Babbage.TxOutCompactDH' addr assets datumHash -> TxOutCompactDH' addr deposit assets datumHash
  Babbage.TxOutCompactDatum addr assets datum -> TxOutCompactDatum addr deposit assets datum
  Babbage.TxOutCompactRefScript addr assets datum script -> TxOutCompactRefScript addr deposit assets datum script
  Babbage.TxOut_AddrHash28_AdaOnly credential address coin ->
    TxOut_AddrHash28_AdaOnly credential address deposit coin
  Babbage.TxOut_AddrHash28_AdaOnly_DataHash32 credential address coin datumHash ->
    TxOut_AddrHash28_AdaOnly_DataHash32 credential address deposit coin datumHash
{-# INLINE fromBabbageTxOut #-}

-- | Project application assets and the other output fields into Babbage-shaped
-- storage. This omits the capacity deposit and is not a whole-output encoding.
toBabbageTxOut :: DijkstraTxOut -> Babbage.BabbageTxOut DijkstraEra
toBabbageTxOut = \case
  TxOutCompact' addr _ assets -> Babbage.TxOutCompact' addr assets
  TxOutCompactDH' addr _ assets datumHash -> Babbage.TxOutCompactDH' addr assets datumHash
  TxOutCompactDatum addr _ assets datum -> Babbage.TxOutCompactDatum addr assets datum
  TxOutCompactRefScript addr _ assets datum script -> Babbage.TxOutCompactRefScript addr assets datum script
  TxOut_AddrHash28_AdaOnly credential address _ coin ->
    Babbage.TxOut_AddrHash28_AdaOnly credential address coin
  TxOut_AddrHash28_AdaOnly_DataHash32 credential address _ coin datumHash ->
    Babbage.TxOut_AddrHash28_AdaOnly_DataHash32 credential address coin datumHash
{-# INLINE toBabbageTxOut #-}

-- Private helpers

viewDijkstraTxOut ::
  DijkstraTxOut ->
  (Addr, OutputValue, Datum DijkstraEra, StrictMaybe (Script DijkstraEra))
viewDijkstraTxOut txOut =
  case toBabbageTxOut txOut of
    Babbage.BabbageTxOut addr assets datum script ->
      (addr, OutputValue (txOut ^. capacityDepositTxOutF) assets, datum, script)

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.TxOut (
  DijkstraTxOut (DijkstraTxOut),
  fromBabbageTxOut,
  toBabbageTxOut,
) where

import Cardano.Ledger.Address (Addr, CompactAddr)
import Cardano.Ledger.Alonzo.Core (AlonzoEraTxOut (..))
import Cardano.Ledger.Alonzo.TxBody (Addr28Extra, DataHash32)
import Cardano.Ledger.Babbage.TxOut (BabbageEraTxOut (..))
import qualified Cardano.Ledger.Babbage.TxOut as Babbage
import Cardano.Ledger.Binary (DecCBOR (..), DecShareCBOR (..), EncCBOR (..), Interns)
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Compactible (CompactForm)
import Cardano.Ledger.Conway.TxBody (upgradeBabbageTxOut)
import Cardano.Ledger.Core (EraTxOut (..), Script, Value)
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Dijkstra.Scripts ()
import Cardano.Ledger.Hashes (DataHash, KeyRole (Staking))
import Cardano.Ledger.Plutus (BinaryData, Datum (..))
import Control.DeepSeq (NFData (rnf), rwhnf)
import Data.Aeson (ToJSON (..))
import Data.Maybe.Strict (StrictMaybe (..))
import Data.MemPack (MemPack (..))
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Lens.Micro (Lens', lens, to)
import NoThunks.Class (NoThunks)

-- | Dijkstra owns its output representation. The value still contains the
-- complete holdings; capacity and application allocations are not split yet.
--
-- Keep the existing compact alternatives and their order in this first step:
-- this preserves address sharing, equality and ordering as well as the compact
-- ADA-only storage. The public pattern exposes the four logical components.
data DijkstraTxOut
  = TxOutCompact'
      {-# UNPACK #-} !CompactAddr
      !(CompactForm (Value DijkstraEra))
  | TxOutCompactDH'
      {-# UNPACK #-} !CompactAddr
      !(CompactForm (Value DijkstraEra))
      !DataHash
  | TxOutCompactDatum
      {-# UNPACK #-} !CompactAddr
      !(CompactForm (Value DijkstraEra))
      {-# UNPACK #-} !(BinaryData DijkstraEra) -- Inline data
  | TxOutCompactRefScript
      {-# UNPACK #-} !CompactAddr
      !(CompactForm (Value DijkstraEra))
      !(Datum DijkstraEra)
      !(Script DijkstraEra)
  | TxOut_AddrHash28_AdaOnly
      !(Credential Staking)
      {-# UNPACK #-} !Addr28Extra
      {-# UNPACK #-} !(CompactForm Coin) -- Ada value
  | TxOut_AddrHash28_AdaOnly_DataHash32
      !(Credential Staking)
      {-# UNPACK #-} !Addr28Extra
      {-# UNPACK #-} !(CompactForm Coin) -- Ada value
      {-# UNPACK #-} !DataHash32
  deriving stock (Eq, Ord, Generic)

instance NFData DijkstraTxOut where
  rnf = rwhnf

instance NoThunks DijkstraTxOut

instance Show DijkstraTxOut where
  show = show . toBabbageTxOut

instance ToJSON DijkstraTxOut where
  toJSON = toJSON . toBabbageTxOut
  toEncoding = toEncoding . toBabbageTxOut

-- | Preserve the current wire format and its accepted legacy output forms.
instance EncCBOR DijkstraTxOut where
  encCBOR = encCBOR . toBabbageTxOut

instance DecCBOR DijkstraTxOut where
  decCBOR = fromBabbageTxOut <$> decCBOR

instance MemPack DijkstraTxOut where
  packedByteCount = packedByteCount . toBabbageTxOut
  packM = packM . toBabbageTxOut
  unpackM = fromBabbageTxOut <$> unpackM

instance DecShareCBOR DijkstraTxOut where
  type Share DijkstraTxOut = Interns (Credential Staking)
  decShareCBOR credentials = do
    old <- decShareCBOR credentials
    pure $! fromBabbageTxOut old

-- | Construct and inspect the same four components as the previous output.
-- The value is the complete output value, not an application-only projection.
pattern DijkstraTxOut ::
  HasCallStack =>
  Addr -> Value DijkstraEra -> Datum DijkstraEra -> StrictMaybe (Script DijkstraEra) -> DijkstraTxOut
pattern DijkstraTxOut addr value datum script <-
  (toBabbageTxOut -> Babbage.BabbageTxOut addr value datum script)
  where
    DijkstraTxOut addr value datum script =
      fromBabbageTxOut (Babbage.BabbageTxOut addr value datum script)

{-# COMPLETE DijkstraTxOut #-}

-- | Copy the representation without normalizing values or addresses. These
-- inverse mappings let Dijkstra reuse the existing codecs and field operations
-- while owning its storage. They do not allocate a capacity deposit.
fromBabbageTxOut :: Babbage.BabbageTxOut DijkstraEra -> DijkstraTxOut
fromBabbageTxOut = \case
  Babbage.TxOutCompact' addr value -> TxOutCompact' addr value
  Babbage.TxOutCompactDH' addr value datumHash -> TxOutCompactDH' addr value datumHash
  Babbage.TxOutCompactDatum addr value datum -> TxOutCompactDatum addr value datum
  Babbage.TxOutCompactRefScript addr value datum script -> TxOutCompactRefScript addr value datum script
  Babbage.TxOut_AddrHash28_AdaOnly credential address coin ->
    TxOut_AddrHash28_AdaOnly credential address coin
  Babbage.TxOut_AddrHash28_AdaOnly_DataHash32 credential address coin datumHash ->
    TxOut_AddrHash28_AdaOnly_DataHash32 credential address coin datumHash
{-# INLINE fromBabbageTxOut #-}

-- | Recover the previous representation exactly, including compact alternatives.
toBabbageTxOut :: DijkstraTxOut -> Babbage.BabbageTxOut DijkstraEra
toBabbageTxOut = \case
  TxOutCompact' addr value -> Babbage.TxOutCompact' addr value
  TxOutCompactDH' addr value datumHash -> Babbage.TxOutCompactDH' addr value datumHash
  TxOutCompactDatum addr value datum -> Babbage.TxOutCompactDatum addr value datum
  TxOutCompactRefScript addr value datum script -> Babbage.TxOutCompactRefScript addr value datum script
  TxOut_AddrHash28_AdaOnly credential address coin ->
    Babbage.TxOut_AddrHash28_AdaOnly credential address coin
  TxOut_AddrHash28_AdaOnly_DataHash32 credential address coin datumHash ->
    Babbage.TxOut_AddrHash28_AdaOnly_DataHash32 credential address coin datumHash
{-# INLINE toBabbageTxOut #-}

babbageTxOutL :: Lens' DijkstraTxOut (Babbage.BabbageTxOut DijkstraEra)
babbageTxOutL = lens toBabbageTxOut (const fromBabbageTxOut)
{-# INLINE babbageTxOutL #-}

instance EraTxOut DijkstraEra where
  type TxOut DijkstraEra = DijkstraTxOut

  mkBasicTxOut addr value = DijkstraTxOut addr value NoDatum SNothing

  upgradeTxOut = fromBabbageTxOut . upgradeBabbageTxOut

  addrEitherTxOutL = babbageTxOutL . Babbage.addrEitherBabbageTxOutL
  {-# INLINE addrEitherTxOutL #-}

  valueEitherTxOutL = babbageTxOutL . Babbage.valueEitherBabbageTxOutL
  {-# INLINE valueEitherTxOutL #-}

  getMinCoinSizedTxOut = Babbage.babbageMinUTxOValue

instance AlonzoEraTxOut DijkstraEra where
  dataHashTxOutL = babbageTxOutL . Babbage.dataHashBabbageTxOutL
  {-# INLINE dataHashTxOutL #-}

  datumTxOutF = to (Babbage.getDatumBabbageTxOut . toBabbageTxOut)
  {-# INLINE datumTxOutF #-}

instance BabbageEraTxOut DijkstraEra where
  dataTxOutL = babbageTxOutL . Babbage.dataBabbageTxOutL
  {-# INLINE dataTxOutL #-}

  datumTxOutL = babbageTxOutL . Babbage.datumBabbageTxOutL
  {-# INLINE datumTxOutL #-}

  referenceScriptTxOutL = babbageTxOutL . Babbage.referenceScriptBabbageTxOutL
  {-# INLINE referenceScriptTxOutL #-}

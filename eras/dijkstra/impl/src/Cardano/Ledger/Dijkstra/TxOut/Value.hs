{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | Transitional monetary component for the Dijkstra output split.
--
-- The split is present in this first model: the container holds a capacity
-- deposit and application assets. A later step can place those two components
-- directly in the output and remove this container.
--
-- Dijkstra outputs store this allocation in compact form. Collapsing the
-- components into a @MaryValue@ would lose the split.
module Cardano.Ledger.Dijkstra.TxOut.Value (
  OutputValue (..),
  CompactForm (..),
  outputCoins,
  compactOutputCoins,
) where

import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..), decodeRecordNamed, encodeListLen)
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Compactible (Compactible (..))
import Cardano.Ledger.Dijkstra.TxOut.ApplicationAssets (
  ApplicationAssets,
  applicationCoins,
  compactApplicationCoins,
 )
import Cardano.Ledger.Dijkstra.TxOut.CapacityDeposit (CapacityDeposit, unCapacityDeposit)
import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON (..), object, (.=))
import Data.MemPack (MemPack (..))
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)

-- | Explicit allocation of the monetary components of an output.
-- Neither component is inferred from the other. Their constructors retain the
-- underlying quantities; this is not a proof of output validity.
--
-- There is deliberately no generic value-arithmetic instance: this type describes
-- an output allocation, not a transaction balance or a signed difference.
data OutputValue = OutputValue
  { capacityDeposit :: !CapacityDeposit
  , applicationAssets :: !ApplicationAssets
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData, NoThunks)

-- | Total ADA accounted for by both components, counted exactly once.
-- Native assets remain in 'applicationAssets'. This is a read-only projection:
-- setting a total would require an explicit rule for choosing its allocation.
outputCoins :: OutputValue -> Coin
outputCoins (OutputValue deposit assets) =
  unCapacityDeposit deposit <> applicationCoins assets

instance ToJSON OutputValue where
  toJSON (OutputValue deposit assets) =
    object ["capacityDeposit" .= deposit, "applicationAssets" .= assets]

instance EncCBOR OutputValue where
  encCBOR (OutputValue deposit assets) =
    encodeListLen 2 <> encCBOR deposit <> encCBOR assets

instance DecCBOR OutputValue where
  decCBOR = decodeRecordNamed "OutputValue" (const 2) $ OutputValue <$> decCBOR <*> decCBOR

instance Compactible OutputValue where
  data CompactForm OutputValue = CompactOutputValue
    { compactCapacityDeposit :: !(CompactForm CapacityDeposit)
    , compactApplicationAssets :: !(CompactForm ApplicationAssets)
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (NFData, NoThunks)
  toCompact (OutputValue deposit assets) = CompactOutputValue <$> toCompact deposit <*> toCompact assets
  fromCompact (CompactOutputValue deposit assets) = OutputValue (fromCompact deposit) (fromCompact assets)

instance EncCBOR (CompactForm OutputValue) where
  encCBOR (CompactOutputValue deposit assets) =
    encodeListLen 2 <> encCBOR deposit <> encCBOR assets

instance DecCBOR (CompactForm OutputValue) where
  decCBOR = decodeRecordNamed "CompactOutputValue" (const 2) $ CompactOutputValue <$> decCBOR <*> decCBOR

instance MemPack (CompactForm OutputValue) where
  packedByteCount (CompactOutputValue deposit assets) = packedByteCount deposit + packedByteCount assets
  packM (CompactOutputValue deposit assets) = packM deposit >> packM assets
  unpackM = CompactOutputValue <$> unpackM <*> unpackM

-- | Total output ADA, without expanding the native-asset representation.
compactOutputCoins :: CompactForm OutputValue -> Coin
compactOutputCoins (CompactOutputValue deposit assets) =
  unCapacityDeposit (fromCompact deposit) <> fromCompact (compactApplicationCoins assets)

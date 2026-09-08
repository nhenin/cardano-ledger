{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | Quantities allocated to the application side of a split output.
module Cardano.Ledger.Dijkstra.TxOut.ApplicationAssets (
  ApplicationAssets (..),
  CompactForm (CompactApplicationAssets),
  fromMaryRepresentation,
  toMaryRepresentation,
  compactApplicationCoins,
) where

import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Compactible (Compactible (..))
import Cardano.Ledger.Mary.MultiAsset (MultiAsset)
import Cardano.Ledger.Mary.Value (MaryValue (..))
import Cardano.Ledger.Val (Val (coinCompact))
import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON (..), object, (.=))
import Data.MemPack (MemPack (..))
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)

-- | Application-controlled ADA and native assets, including ordinary payments.
-- The capacity deposit is accounted for separately.
--
-- The shape is ADA plus native assets, but this type owns the application role.
-- It does not wrap the historical, multipurpose MaryValue model. A mapper from
-- a historical output must apply an explicit allocation rule before constructing
-- application assets; copying its total ADA would not establish a split.
-- Construction does not validate quantities, bounds or transaction validity.
data ApplicationAssets = ApplicationAssets
  { applicationCoins :: !Coin
  -- ^ ADA allocated to the application, excluding the capacity deposit.
  , nativeAssets :: !MultiAsset
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData, NoThunks)

-- | Representation bridge only: the caller has already selected application
-- quantities. This does not allocate a historical output's capacity deposit.
fromMaryRepresentation :: MaryValue -> ApplicationAssets
fromMaryRepresentation (MaryValue coins assets) = ApplicationAssets coins assets

toMaryRepresentation :: ApplicationAssets -> MaryValue
toMaryRepresentation (ApplicationAssets coins assets) = MaryValue coins assets

instance EncCBOR ApplicationAssets where
  encCBOR = encCBOR . toMaryRepresentation

instance DecCBOR ApplicationAssets where
  decCBOR = fromMaryRepresentation <$> decCBOR

instance ToJSON ApplicationAssets where
  toJSON (ApplicationAssets coins assets) =
    object ["coins" .= coins, "nativeAssets" .= assets]

-- | Reuse the compact native-asset representation at the representation
-- boundary, while retaining the application's distinct domain type.
instance Compactible ApplicationAssets where
  newtype CompactForm ApplicationAssets = CompactApplicationAssets (CompactForm MaryValue)
    deriving newtype (Eq, Ord, Show, EncCBOR, DecCBOR, NFData, NoThunks)
  toCompact = fmap CompactApplicationAssets . toCompact . toMaryRepresentation
  fromCompact (CompactApplicationAssets assets) = fromMaryRepresentation (fromCompact assets)

instance MemPack (CompactForm ApplicationAssets) where
  packedByteCount (CompactApplicationAssets assets) = packedByteCount assets
  packM (CompactApplicationAssets assets) = packM assets
  unpackM = CompactApplicationAssets <$> unpackM

-- | Read application ADA without expanding the native-asset map.
compactApplicationCoins :: CompactForm ApplicationAssets -> CompactForm Coin
compactApplicationCoins (CompactApplicationAssets assets) = coinCompact assets

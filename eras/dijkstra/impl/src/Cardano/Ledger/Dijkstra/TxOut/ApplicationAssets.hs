{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

-- | Quantities allocated to the application side of a split output.
module Cardano.Ledger.Dijkstra.TxOut.ApplicationAssets (
  ApplicationAssets (..),
  applicationCoins,
  nativeAssets,
) where

import Cardano.Ledger.BaseTypes (Inject)
import Cardano.Ledger.Binary (DecCBOR, EncCBOR)
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Compactible (Compactible (..))
import Cardano.Ledger.Mary.MultiAsset (MultiAsset)
import Cardano.Ledger.Mary.Value (MaryValue (..))
import Cardano.Ledger.Val (Val)
import qualified Cardano.Ledger.Val as Val
import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON)
import Data.Group (Abelian, Group)
import Data.MemPack (MemPack (..))
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)

-- | Application-controlled ADA and native assets, including ordinary payments.
-- The capacity deposit is accounted for separately.
--
-- This distinct type reuses Mary quantities, arithmetic and encoding. Wrapping
-- a value does not calculate its allocation or validate its quantities.
--
-- As with MaryValue, 'Ord' is structural; use 'Val.pointwise' for quantities.
newtype ApplicationAssets = ApplicationAssets MaryValue
  deriving stock (Show, Generic)
  deriving newtype
    ( Eq
    , Ord
    , NFData
    , NoThunks
    , Semigroup
    , Monoid
    , Group
    , Abelian
    , Inject Coin
    , Val
    , EncCBOR
    , DecCBOR
    , ToJSON
    )

-- | ADA allocated to the application, excluding the capacity deposit.
applicationCoins :: ApplicationAssets -> Coin
applicationCoins (ApplicationAssets assets) = Val.coin assets

nativeAssets :: ApplicationAssets -> MultiAsset
nativeAssets (ApplicationAssets (MaryValue _ assets)) = assets

instance Compactible ApplicationAssets where
  newtype CompactForm ApplicationAssets = CompactApplicationAssets (CompactForm MaryValue)
    deriving stock (Eq, Ord, Show)
    deriving newtype (NoThunks, EncCBOR, DecCBOR)
  toCompact (ApplicationAssets assets) = CompactApplicationAssets <$> toCompact assets
  fromCompact (CompactApplicationAssets assets) = ApplicationAssets (fromCompact assets)

instance MemPack (CompactForm ApplicationAssets) where
  packedByteCount (CompactApplicationAssets assets) = packedByteCount assets
  packM (CompactApplicationAssets assets) = packM assets
  unpackM = CompactApplicationAssets <$> unpackM

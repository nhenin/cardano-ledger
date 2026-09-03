{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Transaction-declared native-asset creation and destruction.
-- 'Forging' describes an operation, not holdings or total supply.
-- 'MintedAssets' and 'BurnedAssets' expose its positive accounting magnitudes
-- separately, so consumers do not have to rediscover the sign convention.
module Cardano.Ledger.Mary.Forging (
  Forging (..),
  MintedAssets,
  unMintedAssets,
  BurnedAssets,
  unBurnedAssets,
  mintedAssets,
  burnedAssets,
) where

import Cardano.Ledger.Mary.MultiAsset (MultiAsset, filterMultiAsset, mapMaybeMultiAsset)
import Control.DeepSeq (NFData)
import Data.Group (Group)
import NoThunks.Class (NoThunks)

-- | Declared changes per policy and asset name: positive to mint, negative to
-- burn, zero for no change. Ada is not minted or burned through this field.
--
-- Construction neither validates the transaction nor authorizes its policies.
-- The raw map, including zero quantities and empty policies, is retained so
-- a typed view does not change transaction data or policy selection.
newtype Forging = Forging {unForging :: MultiAsset}
  deriving stock (Eq, Show)
  deriving newtype (NFData, NoThunks, Semigroup, Monoid, Group)

-- Use hidden positional constructors and ordinary accessor functions: an
-- exported record selector would also permit updates, bypassing the filters.

-- | Strictly positive minted quantities; may be empty.
newtype MintedAssets = MintedAssets MultiAsset
  deriving stock (Eq, Show)
  deriving newtype (NFData, NoThunks, Semigroup, Monoid)

-- | Read-only access to the quantities selected by 'mintedAssets'.
unMintedAssets :: MintedAssets -> MultiAsset
unMintedAssets (MintedAssets assets) = assets

-- | Positive burn magnitudes; may be empty.
newtype BurnedAssets = BurnedAssets MultiAsset
  deriving stock (Eq, Show)
  deriving newtype (NFData, NoThunks, Semigroup, Monoid)

-- | Read-only access to the positive burn magnitudes selected by 'burnedAssets'.
unBurnedAssets :: BurnedAssets -> MultiAsset
unBurnedAssets (BurnedAssets assets) = assets

-- | Select positive native-asset quantities; drop zeros and empty policies.
mintedAssets :: Forging -> MintedAssets
mintedAssets = MintedAssets . filterMultiAsset (\_ _ -> (> 0)) . unForging

-- | Select burns as positive magnitudes; drop zeros and empty policies.
burnedAssets :: Forging -> BurnedAssets
burnedAssets =
  BurnedAssets
    . mapMaybeMultiAsset (\_ _ quantity -> if quantity < 0 then Just (negate quantity) else Nothing)
    . unForging

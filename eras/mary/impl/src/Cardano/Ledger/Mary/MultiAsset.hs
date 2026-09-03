{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

-- | The Ledger's native-asset map, keyed by policy and asset name. It contains
-- no Ada component; this is a concrete representation, not a generic notion
-- of all assets. The raw constructor permits signed quantities and zero or
-- empty entries. Validation depends on context and protocol version, while
-- addition and inversion canonicalize the map.
-- Introduced in Mary, this representation is also shared by later eras.
module Cardano.Ledger.Mary.MultiAsset (
  MultiAsset (..),
  insertMultiAsset,
  multiAssetFromList,
  policies,
  mapMaybeMultiAsset,
  filterMultiAsset,
  pruneZeroMultiAsset,
  flattenMultiAsset,
  isMultiAssetSmallEnough,
  decodeMultiAsset,
) where

import Cardano.Ledger.Binary (
  DecCBOR (..),
  Decoder,
  EncCBOR,
  TokenType (..),
  decodeInteger,
  decodeMap,
  ifDecoderVersionAtLeast,
  peekTokenType,
 )
import Cardano.Ledger.Binary.Version (natVersion)
import Cardano.Ledger.Mary.AssetName (AssetName)
import Cardano.Ledger.Mary.PolicyID (PolicyID)
import Control.DeepSeq (NFData (..))
import Control.Monad (guard, unless, when)
import Data.Aeson (ToJSON)
import Data.CanonicalMaps (canonicalMap, canonicalMapUnion)
import qualified Data.CanonicalMaps as CM
import Data.Foldable (foldMap')
import Data.Group (Group (..))
import Data.Int (Int64)
import Data.Map (Map)
import Data.Map.Internal (link, link2)
import Data.Map.Strict (assocs)
import qualified Data.Map.Strict as Map
import Data.Monoid (Sum (..))
import Data.Set (Set)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)

-- | The MultiAssets map
--
-- Note that the `Ord` instance isn't semantically meaningful and is used only
-- to satisfy constraints on Haskell containers such as `Set` and `Map`.
-- Do not use it for any purpose that would directly affect chain behavior.
newtype MultiAsset = MultiAsset (Map PolicyID (Map AssetName Integer))
  deriving (Show, Ord, Generic, ToJSON, EncCBOR)

instance Eq MultiAsset where
  MultiAsset x == MultiAsset y = CM.pointwise (CM.pointwise (==)) x y

instance NFData MultiAsset where
  rnf (MultiAsset m) = rnf m

instance NoThunks MultiAsset

instance Semigroup MultiAsset where
  MultiAsset m1 <> MultiAsset m2 =
    MultiAsset (canonicalMapUnion (canonicalMapUnion (+)) m1 m2)

instance Monoid MultiAsset where
  mempty = MultiAsset mempty

instance Group MultiAsset where
  invert (MultiAsset m) =
    MultiAsset (canonicalMap (canonicalMap ((-1 :: Integer) *)) m)

instance DecCBOR MultiAsset where
  decCBOR = decodeMultiAsset decodeIntegerBounded64

-- | `MultiAsset` can be used in two different circumstances:
--
-- 1. In `MaryValue` while sending, where amounts must be positive.
-- 2. During minting, both negative and positive are allowed, but not zero.
--
-- In both cases MultiAsset cannot be too big for compact representation and it must not
-- contain empty Maps.
--
-- The supplied decoder selects the quantity bounds for the context. Before
-- protocol version 9, zero quantities and empty maps are pruned. From version
-- 9, zero quantities and empty inner maps are rejected; from version 12, an
-- empty outer map is also rejected.
decodeMultiAsset :: (forall t. Decoder t Integer) -> Decoder s MultiAsset
decodeMultiAsset decodeAmount = do
  ma <-
    ifDecoderVersionAtLeast
      (natVersion @12)
      decodeDijkstra
      $ ifDecoderVersionAtLeast
        (natVersion @9)
        decodeConway
        decodeWithPrunning
  ma <$ unless (isMultiAssetSmallEnough ma) (fail "MultiAsset is too big to compact")
  where
    decodeConway = MultiAsset <$> decodeMap decCBOR (decodeNonEmptyMap decodeNonZeroAmount)
    decodeDijkstra = MultiAsset <$> decodeNonEmptyMap (decodeNonEmptyMap decodeNonZeroAmount)
    decodeWithPrunning =
      pruneZeroMultiAsset . MultiAsset <$> decodeMap decCBOR (decodeMap decCBOR decodeAmount)
    decodeNonZeroAmount = do
      amount <- decodeAmount
      amount <$ when (amount == 0) (fail "MultiAsset cannot contain zeros")
    decodeNonEmptyMap valueDecoder = do
      m <- decodeMap decCBOR valueDecoder
      m <$ when (Map.null m) (fail "Empty Assets are not allowed")

-- Note: we do not use `decodeInt64` from the cborg library here because the
-- implementation contains "-- TODO FIXME: overflow"
decodeIntegerBounded64 :: Decoder s Integer
decodeIntegerBounded64 = do
  tt <- peekTokenType
  case tt of
    TypeUInt -> pure ()
    TypeUInt64 -> pure ()
    TypeNInt -> pure ()
    TypeNInt64 -> pure ()
    _ -> fail "expected major type 0 or 1 when decoding mint field"
  x <- decodeInteger
  if minval <= x && x <= maxval
    then pure x
    else
      fail $
        concat
          [ "overflow when decoding mint field. min value: "
          , show minval
          , " max value: "
          , show maxval
          , " got: "
          , show x
          ]
  where
    maxval = fromIntegral (maxBound :: Int64)
    minval = fromIntegral (minBound :: Int64)

-- | Unlike @Cardano.Ledger.Mary.Value.representationSize@, this function cheaply
-- checks whether any offset within a MultiAsset compact representation is likely to overflow Word16.
--
-- compact form inequality:
--   8n (Word64) + 2n (Word16) + 2n (Word16) + 28p (policy ids) + sum of lengths of unique asset names <= 65535
-- maximum size for the asset name is 32 bytes, so:
-- 8n + 2n + 2n + 28p + 32n <= 65535
-- where: n = total number of assets, p = number of unique policy ids
isMultiAssetSmallEnough :: MultiAsset -> Bool
isMultiAssetSmallEnough (MultiAsset ma) =
  44 * getSum (foldMap' (Sum . length) ma) + 28 * length ma <= 65535

-- | Extract the policy keys, including policies with only zeros or no assets.
--
-- For a canonical map, this is the support of the native-asset quantity in
-- the specification. This projection does not canonicalize a raw map.
policies :: MultiAsset -> Set PolicyID
policies (MultiAsset m) = Map.keysSet m

-- | insertMultiAsset comb policy asset n v,
--   if comb = \ old new -> old, the integer in the MultiAsset is prefered over n
--   if comb = \ old new -> new, then n is prefered over the integer in the MultiAsset
--   if (comb old new) == 0, then that value should not be stored in the MultiAsset
insertMultiAsset ::
  (Integer -> Integer -> Integer) ->
  PolicyID ->
  AssetName ->
  Integer ->
  MultiAsset ->
  MultiAsset
insertMultiAsset combine pid aid new (MultiAsset m1) =
  case Map.splitLookup pid m1 of
    (l1, Just m2, l2) ->
      case Map.splitLookup aid m2 of
        (v1, Just old, v2) ->
          if n == 0
            then
              let m3 = link2 v1 v2
               in if Map.null m3
                    then MultiAsset (link2 l1 l2)
                    else MultiAsset (link pid m3 l1 l2)
            else MultiAsset (link pid (link aid n v1 v2) l1 l2)
          where
            n = combine old new
        (_, Nothing, _) ->
          MultiAsset
            ( link
                pid
                ( if new == 0
                    then m2
                    else Map.insert aid new m2
                )
                l1
                l2
            )
    (l1, Nothing, l2) ->
      MultiAsset
        ( if new == 0
            then link2 l1 l2
            else link pid (Map.singleton aid new) l1 l2
        )

-- | Remove all assets with that have zero amount specified
pruneZeroMultiAsset :: MultiAsset -> MultiAsset
pruneZeroMultiAsset = filterMultiAsset (\_ _ -> (/= 0))

-- | Filter multi assets. Canonical form is preserved.
filterMultiAsset ::
  -- | Predicate that needs to return `True` whenever an asset should be retained.
  (PolicyID -> AssetName -> Integer -> Bool) ->
  MultiAsset ->
  MultiAsset
filterMultiAsset f (MultiAsset ma) =
  MultiAsset $ Map.mapMaybeWithKey modifyAsset ma
  where
    modifyAsset policyId assetMap = do
      let newAssetMap = Map.filterWithKey (f policyId) assetMap
      guard (not (null newAssetMap))
      Just newAssetMap

-- | Map a function over each multi asset value while optionally filtering values
-- out. Canonical form is preserved.
mapMaybeMultiAsset ::
  (PolicyID -> AssetName -> Integer -> Maybe Integer) ->
  MultiAsset ->
  MultiAsset
mapMaybeMultiAsset f (MultiAsset ma) =
  MultiAsset $ Map.mapMaybeWithKey modifyAsset ma
  where
    modifyAsset policyId assetMap = do
      let newAssetMap = Map.mapMaybeWithKey (modifyValue policyId) assetMap
      guard (not (null newAssetMap))
      Just newAssetMap
    modifyValue policyId assetName assetValue = do
      newAssetValue <- f policyId assetName assetValue
      guard (newAssetValue /= 0)
      Just newAssetValue

-- | Rather than using prune to remove 0 assets, when can avoid adding them in the
--   first place by using multiAssetFromList to construct a MultiAsset
multiAssetFromList :: [(PolicyID, AssetName, Integer)] -> MultiAsset
multiAssetFromList = foldr (\(p, n, i) ans -> insertMultiAsset (+) p n i ans) mempty

flattenMultiAsset :: MultiAsset -> [(PolicyID, AssetName, Integer)]
flattenMultiAsset (MultiAsset m) =
  [ (policyId, aname, amount)
  | (policyId, m2) <- assocs m
  , (aname, amount) <- assocs m2
  ]

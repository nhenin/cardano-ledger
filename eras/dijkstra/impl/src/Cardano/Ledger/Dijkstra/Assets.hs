{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The value type of the Dijkstra era: application assets, and nothing
-- else.
--
-- Up to Conway the era value is 'MaryValue' and blends two meanings: assets
-- an application controls, and the ada satisfying the minimum-ada
-- requirement. Dijkstra separates the second concern into the output's
-- capacity deposit ("Cardano.Ledger.Dijkstra.TxOut"), so what remains in the
-- value is purely application assets — ada included, as an ordinary asset.
--
-- 'Assets' makes that shift a type: it is representationally 'MaryValue'
-- (same arithmetic, same wire format), but the compiler now rejects any code
-- that treats a Dijkstra value as a pre-Dijkstra merged value, or vice
-- versa. Every conversion between the two semantics is an explicit 'Assets'
-- wrap or 'unAssets' unwrap.
module Cardano.Ledger.Dijkstra.Assets (
  Assets (..),
  CompactForm (..),
) where

import Cardano.Ledger.BaseTypes (Inject)
import Cardano.Ledger.Binary (DecCBOR, EncCBOR)
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Compactible (Compactible (..))
import Cardano.Ledger.Mary.Value (MaryValue, MaryValueRepresentation (..))
import Cardano.Ledger.Val (Val)
import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON)
import Data.Group (Abelian, Group)
import Data.MemPack (MemPack (..))
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)

-- | Application assets: representationally a 'MaryValue', semantically only
-- what the application controls. The operational funding of UTxO capacity
-- lives in the output's capacity deposit, never in here.
newtype Assets = Assets {unAssets :: MaryValue}
  deriving stock (Generic)
  deriving newtype
    ( Eq
    , Ord
    , Show
    , NFData
    , NoThunks
    , EncCBOR
    , DecCBOR
    , ToJSON
    , Semigroup
    , Monoid
    , Group
    , Abelian
    , Inject Coin
    )

instance Compactible Assets where
  newtype CompactForm Assets = CompactAssets (CompactForm MaryValue)
    deriving newtype (Eq, Ord, Show, NoThunks, EncCBOR, DecCBOR, NFData)
  toCompact (Assets v) = CompactAssets <$> toCompact v
  fromCompact (CompactAssets cv) = Assets (fromCompact cv)

instance MemPack (CompactForm Assets) where
  packedByteCount (CompactAssets cv) = packedByteCount cv
  {-# INLINE packedByteCount #-}
  packM (CompactAssets cv) = packM cv
  {-# INLINE packM #-}
  unpackM = CompactAssets <$> unpackM
  {-# INLINE unpackM #-}

deriving newtype instance Val Assets

-- | The explicit bridge with the pre-Dijkstra merged representation: shared
-- era-generic code builds and inspects values in the 'MaryValue' shape, and
-- both crossings are visible wraps.
instance MaryValueRepresentation Assets where
  fromMaryRepresentation = Assets
  toMaryRepresentation = unAssets

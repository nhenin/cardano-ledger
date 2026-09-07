{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Quantities allocated to the application side of a split output.
module Cardano.Ledger.Dijkstra.TxOut.ApplicationAssets (
  ApplicationAssets (..),
) where

import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Mary.MultiAsset (MultiAsset)
import Control.DeepSeq (NFData)
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
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData, NoThunks)

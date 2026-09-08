{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- | ADA allocated to backing persistent UTxO state.
module Cardano.Ledger.Dijkstra.TxOut.CapacityDeposit (
  CapacityDeposit (..),
  CompactForm (CompactCapacityDeposit),
  requiredCapacityDeposit,
  depositFieldGrowthAllowance,
) where

import Cardano.Ledger.Babbage.Core (CoinPerByte (..))
import Cardano.Ledger.Binary (DecCBOR, EncCBOR)
import Cardano.Ledger.Coin (Coin)
import qualified Cardano.Ledger.Coin as Coin
import Cardano.Ledger.Compactible (Compactible (..))
import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON)
import Data.Int (Int64)
import Data.MemPack (MemPack (..))
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)

-- | An explicitly supplied allocation, distinct from application ADA and from
-- the amount required by a pricing rule. 'requiredCapacityDeposit' computes
-- the tariff; the output validator decides which allocations satisfy it.
--
-- A change to @coinsPerUTxOByte@ would expose additional deposit-management
-- work: the amount allocated under earlier parameters may differ from the
-- current requirement. An increase could make an old output's total ADA
-- insufficient if its backing were recalculated at the new price. Existing
-- minimum-ADA rules check newly produced outputs; they do not retroactively
-- invalidate existing UTxO entries when the parameter changes.
--
-- The split design must distinguish historical allocation, current required
-- backing and the amount released on spending, including how a transaction
-- funds any difference for its new outputs. Repricing and release are separate
-- ledger operations, rather than implicit effects of reading or writing it.
--
-- Construction does not validate the amount. Zero is an amount, not a marker
-- requesting implicit allocation or identifying a historical output.
newtype CapacityDeposit = CapacityDeposit {unCapacityDeposit :: Coin}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (NFData, NoThunks, EncCBOR, DecCBOR, ToJSON, Semigroup, Monoid)

instance Compactible CapacityDeposit where
  newtype CompactForm CapacityDeposit = CompactCapacityDeposit (CompactForm Coin)
    deriving newtype (Eq, Ord, Show, EncCBOR, DecCBOR, NFData, NoThunks)
  toCompact (CapacityDeposit coins) = CompactCapacityDeposit <$> toCompact coins
  fromCompact (CompactCapacityDeposit coins) = CapacityDeposit (fromCompact coins)

instance MemPack (CompactForm CapacityDeposit) where
  packedByteCount (CompactCapacityDeposit coins) = packedByteCount coins
  packM (CompactCapacityDeposit coins) = packM coins
  unpackM = CompactCapacityDeposit <$> unpackM

-- | Price the canonical output size using the existing Babbage tariff.
requiredCapacityDeposit :: CoinPerByte -> Int64 -> CapacityDeposit
requiredCapacityDeposit (CoinPerByte (Coin.CompactCoin price)) outputSize =
  CapacityDeposit (Coin.Coin ((160 + fromIntegral outputSize) * fromIntegral price))

-- | Maximum growth from a canonical explicit zero placeholder to a Word64
-- deposit. The output's presence marker, not this amount, selects its lane.
depositFieldGrowthAllowance :: CoinPerByte -> Coin
depositFieldGrowthAllowance (CoinPerByte (Coin.CompactCoin price)) =
  Coin.Coin (8 * fromIntegral price)

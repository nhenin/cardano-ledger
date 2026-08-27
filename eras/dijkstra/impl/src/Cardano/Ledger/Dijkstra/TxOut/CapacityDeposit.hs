{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- | The operational half of the Dijkstra output split.
--
-- Up to Conway the ada funding an output's UTxO capacity hides inside the
-- era value, indistinguishable from application money. Dijkstra gives it a
-- field of its own in the output ("Cardano.Ledger.Dijkstra.TxOut") and this
-- type: representationally a 'Coin' (same arithmetic, same wire format),
-- but the compiler now rejects any code that treats operational funding as
-- application money, or vice versa. The application counterpart is
-- 'Cardano.Ledger.Dijkstra.Assets.Assets', which plays the same role
-- against 'Cardano.Ledger.Mary.Value.MaryValue'.
module Cardano.Ledger.Dijkstra.TxOut.CapacityDeposit (
  CapacityDeposit (..),
  CompactForm (..),
  compactCapacityDepositOrError,
  compactImplicitCapacityDeposit,
  depositFieldGrowthAllowance,
  implicitCapacityDeposit,
  isImplicitCapacityDeposit,
  requiredCapacityDeposit,
) where

import Cardano.Ledger.Babbage.Core (CoinPerByte (..))
import Cardano.Ledger.Binary (DecCBOR, EncCBOR)
import Cardano.Ledger.Coin (Coin (..), CompactForm (CompactCoin), compactCoinOrError)
import Cardano.Ledger.Compactible (Compactible (..))
import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON)
import Data.Int (Int64)
import Data.MemPack (MemPack (..))
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import NoThunks.Class (NoThunks)

-- | The operational funding of the UTxO capacity an output occupies:
-- semantically never application money. Fixed when the output is created;
-- released when it is consumed.
newtype CapacityDeposit = CapacityDeposit {unCapacityDeposit :: Coin}
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
    )

instance Compactible CapacityDeposit where
  newtype CompactForm CapacityDeposit = CompactCapacityDeposit (CompactForm Coin)
    deriving newtype (Eq, Ord, Show, NoThunks, EncCBOR, DecCBOR, NFData)
  toCompact (CapacityDeposit c) = CompactCapacityDeposit <$> toCompact c
  fromCompact (CompactCapacityDeposit compactCoin) = CapacityDeposit (fromCompact compactCoin)

instance MemPack (CompactForm CapacityDeposit) where
  packedByteCount (CompactCapacityDeposit compactCoin) = packedByteCount compactCoin
  {-# INLINE packedByteCount #-}
  packM (CompactCapacityDeposit compactCoin) = packM compactCoin
  {-# INLINE packM #-}
  unpackM = CompactCapacityDeposit <$> unpackM
  {-# INLINE unpackM #-}

-- | The implicit marker: a zero deposit means \"derive my deposit on UTxO
-- entry\". Dead as an explicit statement — @M(o) > 0@ always — so it is
-- unambiguous. The merged legacy wire forms decode to it.
implicitCapacityDeposit :: CapacityDeposit
implicitCapacityDeposit = CapacityDeposit (Coin 0)

compactImplicitCapacityDeposit :: CompactForm CapacityDeposit
compactImplicitCapacityDeposit = CompactCapacityDeposit (CompactCoin 0)

isImplicitCapacityDeposit :: CapacityDeposit -> Bool
isImplicitCapacityDeposit deposit = deposit == implicitCapacityDeposit

-- | The most the deposit field itself can grow when an implicit output is
-- restructured on UTxO entry: the 1-byte zero marker becomes a deposit of
-- at most 5 CBOR bytes — 4 extra bytes, priced at the current rate. The
-- implicit-lane floor charges this allowance on top of the merged form's
-- tariff, so a passing output always affords its restructured requirement.
depositFieldGrowthAllowance :: CoinPerByte -> Coin
depositFieldGrowthAllowance (CoinPerByte (CompactCoin lovelacePerByte)) =
  Coin (4 * fromIntegral lovelacePerByte)

-- | 'toCompact' for a deposit that is known to be a representable amount,
-- erroring out otherwise.
compactCapacityDepositOrError :: HasCallStack => CapacityDeposit -> CompactForm CapacityDeposit
compactCapacityDepositOrError =
  CompactCapacityDeposit . compactCoinOrError . unCapacityDeposit

-- | The tariff for occupying @serialisedSize@ bytes of UTxO state:
-- @(160 + serialisedSize) × coinsPerUTxOByte@, taking exactly the one
-- parameter it prices with. The constant 160 approximates the per-entry
-- overhead (TxIn plus map entry) the protocol has priced since Babbage.
-- This is the single source of the formula; measuring an output is
-- "Cardano.Ledger.Dijkstra.TxOut"'s job
-- ('Cardano.Ledger.Dijkstra.TxOut.requiredCapacityDepositTxOut').
requiredCapacityDeposit :: CoinPerByte -> Int64 -> CapacityDeposit
requiredCapacityDeposit (CoinPerByte (CompactCoin lovelacePerByte)) serialisedSize =
  CapacityDeposit (Coin (fromIntegral (160 + serialisedSize) * fromIntegral lovelacePerByte))

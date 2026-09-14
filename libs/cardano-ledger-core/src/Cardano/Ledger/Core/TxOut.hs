{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | Era-specific output types and their common operations.
module Cardano.Ledger.Core.TxOut (
  EraTxOut (..),
  RecoverCapacityDeposit,
  Value,
  bootAddrTxOutF,
  coinTxOutL,
  compactCoinTxOutL,
  isAdaOnlyTxOutF,
  mkCoinTxOut,
) where

import Cardano.Ledger.Address (
  Addr (..),
  BootstrapAddress,
  CompactAddr,
  compactAddr,
  decompactAddr,
  isBootstrapCompactAddr,
 )
import Cardano.Ledger.BaseTypes (ProtVer (..))
import Cardano.Ledger.Binary (
  DecCBOR,
  DecShareCBOR (Share),
  EncCBOR,
  Interns,
  Sized (sizedValue),
  mkSized,
 )
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Compactible (Compactible (..))
import Cardano.Ledger.Core.Era (PreviousEra)
import Cardano.Ledger.Core.PParams (EraPParams, PParams, ppProtocolVersionL)
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Hashes (KeyRole (Staking))
import Cardano.Ledger.TxOut.CapacityDeposit (CapacityDeposit)
import Cardano.Ledger.Val (Val (..), inject)
import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON)
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.MemPack (MemPack)
import GHC.Stack (HasCallStack)
import Lens.Micro
import NoThunks.Class (NoThunks)

-- | Recover the capacity-deposit portion of an output's merged value.
-- The function captures the source pricing policy and its protocol parameters,
-- assuming they still describe the output's original deposit requirement.
type RecoverCapacityDeposit era = TxOut era -> CapacityDeposit

-- | Abstract interface into specific fields of a `TxOut`
class
  ( Val (Value era)
  , ToJSON (TxOut era)
  , DecCBOR (Value era)
  , DecCBOR (CompactForm (Value era))
  , MemPack (CompactForm (Value era))
  , EncCBOR (Value era)
  , EncCBOR (TxOut era)
  , DecCBOR (TxOut era)
  , DecShareCBOR (TxOut era)
  , Share (TxOut era) ~ Interns (Credential Staking)
  , NoThunks (TxOut era)
  , NFData (TxOut era)
  , Show (TxOut era)
  , Eq (TxOut era)
  , Ord (TxOut era)
  , MemPack (TxOut era)
  , EraPParams era
  ) =>
  EraTxOut era
  where
  -- | The output of a UTxO for a particular era
  type TxOut era = (r :: Type) | r -> era

  -- | Monetary components supplied when constructing an output. An era can
  -- require an explicit allocation independently of its application value.
  type TxOutAllocation era :: Type

  type TxOutAllocation era = Value era

  {-# MINIMAL
    mkBasicTxOut
    , upgradeTxOut
    , valueEitherTxOutL
    , addrEitherTxOutL
    , (getMinCoinSizedTxOut | getMinCoinTxOut)
    #-}

  mkBasicTxOut :: HasCallStack => Addr -> TxOutAllocation era -> TxOut era

  -- | Every era, except Shelley, must be able to upgrade a `TxOut` from a previous era.
  -- The caller supplies the source era's protocol parameters.
  -- Eras that only change representation may ignore them.
  upgradeTxOut ::
    EraTxOut (PreviousEra era) =>
    PParams (PreviousEra era) ->
    TxOut (PreviousEra era) ->
    TxOut era

  valueTxOutL :: Lens' (TxOut era) (Value era)
  valueTxOutL =
    lens
      ( \txOut -> case txOut ^. valueEitherTxOutL of
          Left value -> value
          Right cValue -> fromCompact cValue
      )
      (\txOut value -> txOut & valueEitherTxOutL .~ Left value)
  {-# INLINE valueTxOutL #-}

  compactValueTxOutL :: HasCallStack => Lens' (TxOut era) (CompactForm (Value era))
  compactValueTxOutL =
    lens
      ( \txOut -> case txOut ^. valueEitherTxOutL of
          Left value -> toCompactPartial value
          Right cValue -> cValue
      )
      (\txOut cValue -> txOut & valueEitherTxOutL .~ Right cValue)
  {-# INLINE compactValueTxOutL #-}

  -- | Lens for getting and setting in TxOut either an address or its compact
  -- version by doing the least amount of work.
  valueEitherTxOutL :: Lens' (TxOut era) (Either (Value era) (CompactForm (Value era)))

  addrTxOutL :: Lens' (TxOut era) Addr
  addrTxOutL =
    lens
      ( \txOut -> case txOut ^. addrEitherTxOutL of
          Left addr -> addr
          Right cAddr -> decompactAddr cAddr
      )
      (\txOut addr -> txOut & addrEitherTxOutL .~ Left addr)
  {-# INLINE addrTxOutL #-}

  compactAddrTxOutL :: Lens' (TxOut era) CompactAddr
  compactAddrTxOutL =
    lens
      ( \txOut -> case txOut ^. addrEitherTxOutL of
          Left addr -> compactAddr addr
          Right cAddr -> cAddr
      )
      (\txOut cAddr -> txOut & addrEitherTxOutL .~ Right cAddr)
  {-# INLINE compactAddrTxOutL #-}

  -- | Lens for getting and setting in TxOut either an address or its compact
  -- version by doing the least amount of work.
  --
  -- The utility of this function comes from the fact that TxOut usually stores
  -- the address in either one of two forms: compacted or unpacked. In order to
  -- avoid extroneous conversions in `getTxOutAddr` and `getTxOutCompactAddr` we
  -- can define just this functionality. Also sometimes it is crucial to know at
  -- the callsite which form of address we have readily available without any
  -- conversions (eg. searching millions of TxOuts for a particular address)
  addrEitherTxOutL :: Lens' (TxOut era) (Either Addr CompactAddr)

  -- | Produce the minimum lovelace that a given transaction output must
  -- contain. Information about the size of the TxOut is required in some eras.
  -- Use `getMinCoinTxOut` if you don't have the size readily available to you.
  getMinCoinSizedTxOut :: PParams era -> Sized (TxOut era) -> Coin
  getMinCoinSizedTxOut pp = getMinCoinTxOut pp . sizedValue

  -- | Same as `getMinCoinSizedTxOut`, except information about the size of
  -- TxOut will be computed by serializing the TxOut. If the size turns out to
  -- be not needed, then serialization will have no overhead, since it is
  -- computed lazily.
  getMinCoinTxOut :: PParams era -> TxOut era -> Coin
  getMinCoinTxOut pp txOut =
    let ProtVer version _ = pp ^. ppProtocolVersionL
     in getMinCoinSizedTxOut pp (mkSized version txOut)

bootAddrTxOutF ::
  EraTxOut era => SimpleGetter (TxOut era) (Maybe BootstrapAddress)
bootAddrTxOutF = to $ \txOut ->
  case txOut ^. addrEitherTxOutL of
    Left (AddrBootstrap bootstrapAddr) -> Just bootstrapAddr
    Right cAddr
      | isBootstrapCompactAddr cAddr -> do
          AddrBootstrap bootstrapAddr <- Just (decompactAddr cAddr)
          Just bootstrapAddr
    _ -> Nothing
{-# INLINE bootAddrTxOutF #-}

coinTxOutL :: (HasCallStack, EraTxOut era) => Lens' (TxOut era) Coin
coinTxOutL =
  lens
    ( \txOut ->
        case txOut ^. valueEitherTxOutL of
          Left val -> coin val
          Right cVal -> fromCompact (coinCompact cVal)
    )
    ( \txOut c ->
        case txOut ^. valueEitherTxOutL of
          Left val -> txOut & valueTxOutL .~ modifyCoin (const c) val
          Right cVal ->
            txOut & compactValueTxOutL .~ modifyCompactCoin (const (toCompactPartial c)) cVal
    )
{-# INLINE coinTxOutL #-}

compactCoinTxOutL :: (HasCallStack, EraTxOut era) => Lens' (TxOut era) (CompactForm Coin)
compactCoinTxOutL =
  lens
    ( \txOut ->
        case txOut ^. valueEitherTxOutL of
          Left val -> toCompactPartial (coin val)
          Right cVal -> coinCompact cVal
    )
    ( \txOut cCoin ->
        case txOut ^. valueEitherTxOutL of
          Left val -> txOut & valueTxOutL .~ modifyCoin (const (fromCompact cCoin)) val
          Right cVal ->
            txOut & compactValueTxOutL .~ modifyCompactCoin (const cCoin) cVal
    )
{-# INLINE compactCoinTxOutL #-}

-- | This is a getter that implements an efficient way to check whether 'TxOut'
-- contains ADA only.
isAdaOnlyTxOutF :: EraTxOut era => SimpleGetter (TxOut era) Bool
isAdaOnlyTxOutF = to $ \txOut ->
  case txOut ^. valueEitherTxOutL of
    Left val -> isAdaOnly val
    Right cVal -> isAdaOnlyCompact cVal

toCompactPartial :: (HasCallStack, Val a) => a -> CompactForm a
toCompactPartial v =
  fromMaybe (error $ "Illegal value in TxOut: " <> show v) $ toCompact v

-- | Construct an ADA-only output when its construction allocation is its value.
-- Eras that require an additional allocation must supply it explicitly.
mkCoinTxOut ::
  (EraTxOut era, TxOutAllocation era ~ Value era) =>
  Addr -> Coin -> TxOut era
mkCoinTxOut addr = mkBasicTxOut addr . inject

-- | A value is something which quantifies a transaction output.
type family Value era :: Type

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The capacity-deposit rule, in one place: both the UTXO and the SUBUTXO
-- rules validate the outputs they create with
-- 'validateOutputCapacityDeposit', each injecting its own failure
-- constructors.
--
-- The rule has two lanes, keyed on the wire form of the output:
--
-- * __Explicit__ (a stated, non-zero deposit): the deposit must equal
--   /exactly/ the requirement @M(o)@ — the split-world semantics. Not a
--   floor: over-funding would park application ada in the operational field
--   and desynchronise the reserve identity @B(t) = \sum M(o)@, so it is
--   rejected like under-funding.
--
-- * __Implicit__ (deposit 0 — the merged legacy forms decode to it, and no
--   valid explicit output can state it since @M(o) > 0@): the previous
--   eras' floor applies — the output's total ada must cover @M(o)@ — and
--   the ledger derives the split as the output enters the UTxO
--   ("Cardano.Ledger.Dijkstra.UTxO.Translation"). This lane is what keeps a
--   transaction signed before the era boundary valid after it: signed
--   bytes can never be rewritten, only reinterpreted.
module Cardano.Ledger.Dijkstra.Rules.CapacityDeposit (
  validateOutputCapacityDeposit,
) where

import Cardano.Ledger.Babbage.Core (BabbageEraPParams, ppCoinsPerUTxOByteL)
import Cardano.Ledger.BaseTypes (Mismatch (..), Relation (RelEQ, RelGTEQ))
import Cardano.Ledger.Binary (Sized, sizedValue)
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Core (EraTxOut (..), PParams, TxOut, coinTxOutL)
import Cardano.Ledger.Dijkstra.TxOut (DijkstraEraTxOut (..))
import Cardano.Ledger.Dijkstra.TxOut.CapacityDeposit (
  CapacityDeposit (..),
  depositFieldGrowthAllowance,
  isImplicitCapacityDeposit,
 )
import Cardano.Ledger.Rules.ValidationMode (Test)
import Control.State.Transition.Extended (failureOnNonEmpty)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (mapMaybe)
import Lens.Micro ((^.))

-- | Validate every created output against the lane its wire form chose:
-- exact tariff for explicit deposits, the merged-world floor for implicit
-- ones.
validateOutputCapacityDeposit ::
  (DijkstraEraTxOut era, BabbageEraPParams era, Foldable f) =>
  (NonEmpty (TxOut era, Mismatch RelEQ CapacityDeposit) -> failure) ->
  (NonEmpty (TxOut era, Mismatch RelGTEQ Coin) -> failure) ->
  PParams era ->
  f (Sized (TxOut era)) ->
  Test failure
validateOutputCapacityDeposit mkIncorrectDepositFailure mkImplicitTooSmallFailure pparams createdOutputs =
  failureOnNonEmpty (misfundedExplicitOutputs pparams createdOutputs) mkIncorrectDepositFailure
    *> failureOnNonEmpty (underfundedImplicitOutputs pparams createdOutputs) mkImplicitTooSmallFailure

-- | The explicit created outputs whose capacity deposit differs from the
-- required @M(o)@, each paired with its supplied\/expected mismatch.
misfundedExplicitOutputs ::
  (DijkstraEraTxOut era, Foldable f) =>
  PParams era ->
  f (Sized (TxOut era)) ->
  [(TxOut era, Mismatch RelEQ CapacityDeposit)]
misfundedExplicitOutputs pparams createdOutputs =
  mapMaybe (misfundedExplicitOutput pparams) (toList createdOutputs)

-- | Judge one explicit created output against the tariff it must fund.
-- Implicit outputs are not this lane's concern.
misfundedExplicitOutput ::
  DijkstraEraTxOut era =>
  PParams era ->
  Sized (TxOut era) ->
  Maybe (TxOut era, Mismatch RelEQ CapacityDeposit)
misfundedExplicitOutput pparams sizedTxOut
  | isImplicitTxOut (sizedValue sizedTxOut) = Nothing
  | otherwise =
      depositMismatch
        (sizedValue sizedTxOut)
        (CapacityDeposit (getMinCoinSizedTxOut pparams sizedTxOut))

-- | The implicit created outputs whose total ada cannot cover the tariff
-- the entry-time restructuring will charge. The restructured form is
-- larger than the merged one — the 1-byte zero marker grows into a deposit
-- of up to 5 CBOR bytes — so the floor charges the merged form's tariff
-- plus that growth allowance: a passing output always affords its
-- restructured requirement.
underfundedImplicitOutputs ::
  (DijkstraEraTxOut era, BabbageEraPParams era, Foldable f) =>
  PParams era ->
  f (Sized (TxOut era)) ->
  [(TxOut era, Mismatch RelGTEQ Coin)]
underfundedImplicitOutputs pparams createdOutputs =
  mapMaybe (underfundedImplicitOutput pparams) (toList createdOutputs)

-- | Judge one implicit created output against the merged-world floor: its
-- total ada (all of it in the assets, since the deposit is 0) must cover
-- @M(o)@.
underfundedImplicitOutput ::
  (DijkstraEraTxOut era, BabbageEraPParams era) =>
  PParams era ->
  Sized (TxOut era) ->
  Maybe (TxOut era, Mismatch RelGTEQ Coin)
underfundedImplicitOutput pparams sizedTxOut
  | not (isImplicitTxOut (sizedValue sizedTxOut)) = Nothing
  | sizedValue sizedTxOut ^. coinTxOutL >= implicitFloor pparams sizedTxOut = Nothing
  | otherwise =
      Just
        ( sizedValue sizedTxOut
        , Mismatch
            { mismatchSupplied = sizedValue sizedTxOut ^. coinTxOutL
            , mismatchExpected = implicitFloor pparams sizedTxOut
            }
        )

-- | The merged form's tariff plus the deposit-field growth allowance.
implicitFloor ::
  (DijkstraEraTxOut era, BabbageEraPParams era) =>
  PParams era ->
  Sized (TxOut era) ->
  Coin
implicitFloor pparams sizedTxOut =
  getMinCoinSizedTxOut pparams sizedTxOut
    <> depositFieldGrowthAllowance (pparams ^. ppCoinsPerUTxOByteL)

-- | Deposit 0 marks the merged legacy forms: no valid explicit output can
-- state it, since @M(o) >= 160 * coinsPerUTxOByte > 0@.
isImplicitTxOut :: DijkstraEraTxOut era => TxOut era -> Bool
isImplicitTxOut txOut = isImplicitCapacityDeposit (txOut ^. capacityDepositTxOutL)

-- | 'Nothing' when the funded deposit is exactly the required one; the
-- supplied\/expected mismatch otherwise.
depositMismatch ::
  DijkstraEraTxOut era =>
  TxOut era ->
  CapacityDeposit ->
  Maybe (TxOut era, Mismatch RelEQ CapacityDeposit)
depositMismatch txOut requiredDeposit
  | txOut ^. capacityDepositTxOutL == requiredDeposit = Nothing
  | otherwise =
      Just
        ( txOut
        , Mismatch
            { mismatchSupplied = txOut ^. capacityDepositTxOutL
            , mismatchExpected = requiredDeposit
            }
        )

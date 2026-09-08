{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Capacity allocation is chosen by wire form, never by a zero amount.
-- Explicit outputs must state the exact tariff. Legacy implicit outputs must
-- afford the floor and admit an exact, total-preserving allocation before they
-- enter the UTxO; validation never changes their signed representation.
module Cardano.Ledger.Dijkstra.Rules.CapacityDeposit (
  validateOutputCapacityDeposit,
  implicitCapacityDepositFloor,
) where

import Cardano.Ledger.BaseTypes (Mismatch (..), Relation (RelEQ, RelGTEQ))
import Cardano.Ledger.Binary (Sized, sizedValue)
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Core (EraTxOut (..), PParams, TxOut)
import Cardano.Ledger.Dijkstra.TxOut (CapacityDepositForm (..), DijkstraEraTxOut (..))
import Cardano.Ledger.Dijkstra.TxOut.CapacityDeposit (CapacityDeposit)
import Cardano.Ledger.Dijkstra.TxOut.Translation (
  CapacityDepositAllocationError,
  allocateCapacityDeposit,
 )
import Cardano.Ledger.Dijkstra.TxOut.Value (outputCoins)
import Cardano.Ledger.Rules.ValidationMode (Test)
import Control.State.Transition.Extended (failureOnNonEmpty)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty)
import Lens.Micro ((^.))

validateOutputCapacityDeposit ::
  (DijkstraEraTxOut era, Foldable f) =>
  (NonEmpty (TxOut era, Mismatch RelEQ CapacityDeposit) -> failure) ->
  (NonEmpty (TxOut era, Mismatch RelGTEQ Coin) -> failure) ->
  (NonEmpty (TxOut era, CapacityDepositAllocationError) -> failure) ->
  PParams era ->
  f (Sized (TxOut era)) ->
  Test failure
validateOutputCapacityDeposit mkExplicitFailure mkImplicitFloorFailure mkAllocationFailure pp outputs =
  failureOnNonEmpty explicitFailures mkExplicitFailure
    *> failureOnNonEmpty implicitFloorFailures mkImplicitFloorFailure
    *> failureOnNonEmpty allocationFailures mkAllocationFailure
  where
    createdOutputs = toList outputs
    explicitFailures =
      [ (txOut, Mismatch supplied required)
      | sized <- createdOutputs
      , let txOut = sizedValue sized
      , txOut ^. capacityDepositFormTxOutL == ExplicitCapacityDeposit
      , let supplied = txOut ^. capacityDepositTxOutL
      , let required = getCapacityDepositRequirement pp txOut
      , supplied /= required
      ]
    implicitFloorFailures =
      [ (txOut, Mismatch supplied required)
      | sized <- createdOutputs
      , let txOut = sizedValue sized
      , txOut ^. capacityDepositFormTxOutL == ImplicitCapacityDeposit
      , let supplied = outputCoins (txOut ^. outputValueTxOutL)
      , let required = implicitCapacityDepositFloor pp sized
      , supplied < required
      ]
    allocationFailures =
      [ (txOut, allocationError)
      | sized <- createdOutputs
      , let txOut = sizedValue sized
      , Left allocationError <- [allocateCapacityDeposit pp txOut]
      ]

-- | The canonical explicit form initially has a one-byte zero deposit.
-- An unsigned compact coin can occupy nine CBOR bytes, so reserve eight bytes
-- of growth. The old four-byte allowance assumed deposits fit in Word32.
-- The exact allocation check still runs: this conservative floor cannot prove
-- that a fixed point exists when residual application ADA crosses a width band.
implicitCapacityDepositFloor ::
  DijkstraEraTxOut era => PParams era -> Sized (TxOut era) -> Coin
implicitCapacityDepositFloor pp = getMinCoinTxOut pp . sizedValue

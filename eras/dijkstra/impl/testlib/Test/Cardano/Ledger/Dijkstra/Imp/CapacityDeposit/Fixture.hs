{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}

module Test.Cardano.Ledger.Dijkstra.Imp.CapacityDeposit.Fixture (
  largeImplicitOutput,
  largeExplicitOutput,
  wrongExplicitOutput,
  singleOutputTx,
  implicitRejection,
  explicitRejection,
  preserveOutputAllocations,
) where

import Cardano.Ledger.BaseTypes (Mismatch (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Rules (DijkstraUtxoPredFailure (..))
import Cardano.Ledger.Dijkstra.TxOut (CapacityDepositForm (..))
import Cardano.Ledger.Dijkstra.TxOut.CapacityDeposit (CapacityDeposit (..))
import Cardano.Ledger.Dijkstra.TxOut.Translation (CapacityDepositAllocationError (..))
import Cardano.Ledger.Plutus (mkInlineDatum)
import Cardano.Ledger.Val (inject)
import qualified Data.ByteString as BS
import Data.List.NonEmpty (NonEmpty)
import Lens.Micro ((&), (.~), (^.))
import qualified PlutusLedgerApi.V1 as PV1
import Test.Cardano.Ledger.Dijkstra.ImpTest

-- | Same large datum and five-million total as the inherited historical test.
largeImplicitOutput :: DijkstraEraImp era => ImpTestM era (TxOut era)
largeImplicitOutput = largeOutput (Coin 5_000_000)

largeOutput :: DijkstraEraImp era => Coin -> ImpTestM era (TxOut era)
largeOutput amount = do
  addr <- freshKeyAddrNoPtr_
  pure $
    mkBasicTxOut addr (inject amount)
      & datumTxOutL .~ mkInlineDatum (PV1.B (BS.replicate 1500 0))

-- | Native explicit outputs keep the requested application amount. With it
-- fixed, the required deposit grows monotonically through finite uint widths.
largeExplicitOutput :: DijkstraEraImp era => Coin -> ImpTestM era (TxOut era)
largeExplicitOutput applicationAmount = do
  pp <- getsPParams id
  out <- largeOutput applicationAmount
  let
    fund candidate =
      let required = getCapacityDepositRequirement pp candidate
       in if candidate ^. capacityDepositTxOutL == required
            then candidate
            else fund (candidate & capacityDepositTxOutL .~ required)
  pure $ fund (out & capacityDepositFormTxOutL .~ ExplicitCapacityDeposit)

wrongExplicitOutput :: DijkstraEraImp era => ImpTestM era (TxOut era)
wrongExplicitOutput = do
  out <- largeExplicitOutput (Coin 1)
  pure $ out & capacityDepositTxOutL .~ CapacityDeposit (Coin 0)

singleOutputTx :: DijkstraEraImp era => TxOut era -> Tx TopTx era
singleOutputTx out = mkBasicTx (mkBasicTxBody & outputsTxBodyL .~ [out])

-- | Expected phase-1 failures: neither the legacy floor nor a complete
-- allocation can be funded from the five-million total. The final tariff is
-- measured with every available coin placed in capacity.
implicitRejection ::
  DijkstraEraImp era => PParams era -> TxOut era -> NonEmpty (DijkstraUtxoPredFailure era)
implicitRejection pp out =
  [ UnableToAllocateCapacityDepositUTxO
      [(out, CapacityDepositInsufficient total (getCapacityDepositRequirement pp allCapacity))]
  , ImplicitOutputTooSmallUTxO [(out, Mismatch total (getMinCoinTxOut pp out))]
  ]
  where
    total = out ^. potCoinsTxOutF
    allCapacity =
      out
        & coinTxOutL .~ Coin 0
        & capacityDepositTxOutL .~ CapacityDeposit total
        & capacityDepositFormTxOutL .~ ExplicitCapacityDeposit

explicitRejection ::
  DijkstraEraImp era => PParams era -> TxOut era -> DijkstraUtxoPredFailure era
explicitRejection pp out =
  IncorrectCapacityDepositUTxO
    [(out, Mismatch (out ^. capacityDepositTxOutL) (getCapacityDepositRequirement pp out))]

-- | Preserve the intended output lane before the normal pipeline computes
-- fees, script integrity and signatures. This never rewrites a signed body.
preserveOutputAllocations :: DijkstraEraImp era => Tx TopTx era -> ImpTestM era (Tx TopTx era)
preserveOutputAllocations = dijkstraFixupTxWithOutputFixup pure

{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Connect the Dijkstra output representation to the Ledger output interfaces.
module Cardano.Ledger.Dijkstra.TxOut.LedgerInstances () where

import Cardano.Ledger.Alonzo.Core (AlonzoEraTxOut (..))
import Cardano.Ledger.Babbage.TxOut (BabbageEraTxOut (..))
import qualified Cardano.Ledger.Babbage.TxOut as Babbage
import Cardano.Ledger.Core (EraTxOut (..))
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Dijkstra.TxOut (
  DijkstraTxOut (DijkstraTxOut),
  capacityDepositTxOutF,
  fromBabbageTxOut,
  toBabbageTxOut,
 )
import Cardano.Ledger.Dijkstra.TxOut.Translation (fromConway)
import Cardano.Ledger.Dijkstra.TxOut.Value (OutputValue)
import Cardano.Ledger.Dijkstra.TxOut.Value.Translation (AllocationError)
import Cardano.Ledger.Plutus (Datum (NoDatum))
import Data.Maybe.Strict (StrictMaybe (SNothing))
import Lens.Micro (Lens', lens, to, (^.))

-- * Ledger output interfaces

instance EraTxOut DijkstraEra where
  type TxOut DijkstraEra = DijkstraTxOut
  type TxOutAllocation DijkstraEra = OutputValue

  mkBasicTxOut addr allocation = DijkstraTxOut addr allocation NoDatum SNothing

  -- During development, migration assumes that valid source outputs can fund
  -- their allocation under the supplied source parameters. Fail explicitly if
  -- this assumption is violated instead of changing the requested allocation.
  upgradeTxOut sourcePParams =
    either failCapacityAllocation id . fromConway sourcePParams

  addrEitherTxOutL = babbageTxOutL . Babbage.addrEitherBabbageTxOutL
  {-# INLINE addrEitherTxOutL #-}

  valueEitherTxOutL = babbageTxOutL . Babbage.valueEitherBabbageTxOutL
  {-# INLINE valueEitherTxOutL #-}

  getMinCoinSizedTxOut = Babbage.babbageMinUTxOValue

instance AlonzoEraTxOut DijkstraEra where
  dataHashTxOutL = babbageTxOutL . Babbage.dataHashBabbageTxOutL
  {-# INLINE dataHashTxOutL #-}

  datumTxOutF = to (Babbage.getDatumBabbageTxOut . toBabbageTxOut)
  {-# INLINE datumTxOutF #-}

instance BabbageEraTxOut DijkstraEra where
  dataTxOutL = babbageTxOutL . Babbage.dataBabbageTxOutL
  {-# INLINE dataTxOutL #-}

  datumTxOutL = babbageTxOutL . Babbage.datumBabbageTxOutL
  {-# INLINE datumTxOutL #-}

  referenceScriptTxOutL = babbageTxOutL . Babbage.referenceScriptBabbageTxOutL
  {-# INLINE referenceScriptTxOutL #-}

-- Private helpers

failCapacityAllocation :: AllocationError -> DijkstraTxOut
failCapacityAllocation =
  error . ("Dijkstra.upgradeTxOut: capacity allocation invariant violated: " <>) . show

babbageTxOutL :: Lens' DijkstraTxOut (Babbage.BabbageTxOut DijkstraEra)
babbageTxOutL =
  lens toBabbageTxOut (\txOut -> fromBabbageTxOut (txOut ^. capacityDepositTxOutF))
{-# INLINE babbageTxOutL #-}

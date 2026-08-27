{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | The translation of a merged-world UTxO set into the Dijkstra split:
-- "Cardano.Ledger.Dijkstra.TxOut.Translation" lifted over the map, at its
-- two application points.
--
-- 1. __The era boundary__: 'translateUTxO' restructures every output of the
--    previous era's UTxO set ("Cardano.Ledger.Dijkstra.Translation").
--
-- 2. __UTxO entry__: 'fundCapacityDeposits' restructures the outputs a
--    transaction creates as they enter the map — an implicit (merged-form)
--    output gets its deposit derived, an explicit exact output passes
--    through unchanged — so the stored state is always split.
module Cardano.Ledger.Dijkstra.UTxO.Translation (
  fundCapacityDeposits,
  translateUTxO,
) where

import Cardano.Ledger.Babbage.Core (CoinPerByte)
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Core (EraScript, TxOut, Value)
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Dijkstra.TxOut (DijkstraTxOut)
import Cardano.Ledger.Dijkstra.TxOut.Translation (fundCapacityDeposit, translateTxOut)
import Cardano.Ledger.State (UTxO (..))
import Cardano.Ledger.Val (Val)
import qualified Data.Map.Strict as Map

-- | 'fundCapacityDeposit' over the outputs entering the UTxO map — the
-- entry-time application point of the translation.
fundCapacityDeposits ::
  (EraScript era, Val (Value era), TxOut era ~ DijkstraTxOut era) =>
  CoinPerByte ->
  UTxO era ->
  UTxO era
fundCapacityDeposits coinsPerUTxOByte =
  UTxO . Map.map (fundCapacityDeposit coinsPerUTxOByte) . unUTxO

-- | 'translateTxOut' over the whole UTxO set — the hard-fork translation
-- applied at the era boundary.
translateUTxO :: CoinPerByte -> UTxO ConwayEra -> UTxO DijkstraEra
translateUTxO coinsPerUTxOByte =
  UTxO . Map.map (translateTxOut coinsPerUTxOByte) . unUTxO

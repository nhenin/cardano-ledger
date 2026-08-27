{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

-- | The translation of one merged-world output into the Dijkstra split.
--
-- Translation RESTRUCTURES existing data — it moves ada between the two
-- fields of one output, from the application assets into the capacity
-- deposit — and never creates any: 'totalAdaTxOut' is preserved by every
-- function in this module. Because value preservation accounts totals, a
-- restructured output weighs exactly what its merged form weighed, and the
-- restructuring is invisible to the accounting.
--
-- "Cardano.Ledger.Dijkstra.UTxO.Translation" lifts this notion over the
-- UTxO set, at its two application points: the era boundary and UTxO entry.
module Cardano.Ledger.Dijkstra.TxOut.Translation (
  fundCapacityDeposit,
  translateTxOut,
) where

import Cardano.Ledger.Babbage.Core (CoinPerByte)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Compactible (fromCompact, toCompactPartial)
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Core (EraScript, TxOut, Value, upgradeTxOut)
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Dijkstra.TxOut (
  DijkstraTxOut (..),
  measureTxOut,
  requiredCapacityDepositTxOut,
  totalAdaTxOut,
 )
import Cardano.Ledger.Dijkstra.TxOut.CapacityDeposit (
  CapacityDeposit (..),
  compactCapacityDepositOrError,
 )
import Cardano.Ledger.Val (Val (..))

-- | Compute the unchanged requirement @M(o)@ at the given pricing and move
-- exactly that much ada from the value into the capacity deposit; total ada
-- in the output is preserved. Because @M(o)@ prices the serialised size of
-- the output it applies to, and moving ada can change the CBOR width of the
-- two coin fields, the split iterates (bounded, 8 attempts) rather than
-- solving directly. Anywhere both resulting coins land in one width band —
-- e.g. @[65536, 2^32-1]@ lovelace, 5 bytes each, which covers every
-- realistic output — the second attempt is already the fixpoint. A fixpoint
-- does not always exist, though: when the residual application ada sits
-- right at a width boundary the requirement can oscillate between two
-- sizes, and the loop then returns the last attempt, one width-step away
-- from exact. A legacy output whose total ada cannot cover the recomputed
-- requirement translates with everything it has in the deposit — the deposit
-- rule constrains newly created outputs, not the migrated stock.
--
-- An output whose deposit is already exactly @M(o)@ is returned unchanged:
-- the loop exits on equality before moving anything, which is what lets
-- UTxO entry apply this function to every created output uniformly.
fundCapacityDeposit ::
  (EraScript era, Val (Value era)) =>
  CoinPerByte ->
  DijkstraTxOut era ->
  DijkstraTxOut era
fundCapacityDeposit coinsPerUTxOByte txOut =
  fundCapacityDepositLoop 8 coinsPerUTxOByte (totalAdaTxOut txOut) txOut

-- | The era-boundary translation of one output: upgrade the merged
-- representation structurally (value becomes pure application assets,
-- deposit 0), then restructure the ada.
translateTxOut :: CoinPerByte -> TxOut ConwayEra -> DijkstraTxOut DijkstraEra
translateTxOut coinsPerUTxOByte conwayTxOut =
  fundCapacityDeposit coinsPerUTxOByte (upgradeTxOut @DijkstraEra conwayTxOut)

fundCapacityDepositLoop ::
  (EraScript era, Val (Value era)) =>
  Int ->
  CoinPerByte ->
  Coin ->
  DijkstraTxOut era ->
  DijkstraTxOut era
fundCapacityDepositLoop attemptsLeft coinsPerUTxOByte totalAda txOut
  | attemptsLeft <= 0 = txOut
  | otherwise =
      fundCapacityDepositStep
        attemptsLeft
        coinsPerUTxOByte
        totalAda
        ( min
            (requiredCapacityDepositTxOut coinsPerUTxOByte (measureTxOut txOut))
            (CapacityDeposit totalAda)
        )
        txOut

fundCapacityDepositStep ::
  (EraScript era, Val (Value era)) =>
  Int ->
  CoinPerByte ->
  Coin ->
  CapacityDeposit ->
  DijkstraTxOut era ->
  DijkstraTxOut era
fundCapacityDepositStep attemptsLeft coinsPerUTxOByte totalAda requiredDeposit txOut
  | requiredDeposit == fromCompact (dtoCapacityDeposit txOut) = txOut
  | otherwise =
      fundCapacityDepositLoop
        (attemptsLeft - 1)
        coinsPerUTxOByte
        totalAda
        (moveAdaToDeposit totalAda requiredDeposit txOut)

-- | Set the deposit and give the rest of the total ada to the assets: the
-- one place where ada crosses the assets\/deposit boundary.
moveAdaToDeposit ::
  Val (Value era) =>
  Coin ->
  CapacityDeposit ->
  DijkstraTxOut era ->
  DijkstraTxOut era
moveAdaToDeposit totalAda deposit txOut =
  txOut
    { dtoCapacityDeposit = compactCapacityDepositOrError deposit
    , dtoAssets =
        toCompactPartial $
          modifyCoin
            (const (Coin (unCoin totalAda - unCoin (unCapacityDeposit deposit))))
            (fromCompact (dtoAssets txOut))
    }

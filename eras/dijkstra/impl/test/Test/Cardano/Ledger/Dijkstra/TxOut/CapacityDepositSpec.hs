{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

-- | Properties of the capacity-deposit funding primitives, at mainnet
-- pricing and on real arbitrary outputs. The builder setter is the
-- constructive dual of the exact deposit rule; the migration split
-- conserves the output's total ada. Exact lovelace throughout.
module Test.Cardano.Ledger.Dijkstra.TxOut.CapacityDepositSpec (spec) where

import Cardano.Ledger.BaseTypes (inject)
import Cardano.Ledger.Coin (Coin (..), CompactForm (CompactCoin))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.Core (
  CoinPerByte (..),
  PParams,
  TxOut,
  coinTxOutL,
  emptyPParams,
  getMinCoinTxOut,
  mkBasicTxOut,
  ppCoinsPerUTxOByteL,
 )
import Cardano.Ledger.Dijkstra.TxOut (
  DijkstraEraTxOut (..),
  DijkstraTxOut,
  assetsAdaTxOutL,
  totalAdaTxOut,
 )
import Cardano.Ledger.Dijkstra.TxOut.Translation (fundCapacityDeposit, translateTxOut)
import Cardano.Ledger.Dijkstra.TxOut.CapacityDeposit (CapacityDeposit (..))
import Lens.Micro ((&), (.~), (^.))
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Cardano.Ledger.Core.KeyPair (mkAddr)
import Test.Cardano.Ledger.Dijkstra.Arbitrary ()
import Test.Cardano.Ledger.Shelley.Examples (examplePayKey, exampleStakeKey)

spec :: Spec
spec = describe "CapacityDeposit" $ do
  prop "setCapacityDepositTxOut funds exactly the required tariff" $
    forAll (arbitrary @(DijkstraTxOut DijkstraEra)) $ \txOut ->
      hasExactCapacityDeposit (setCapacityDepositTxOut pparams txOut)

  prop "setCapacityDepositTxOut changes nothing but the deposit" $
    forAll (arbitrary @(DijkstraTxOut DijkstraEra)) $ \txOut ->
      ( setCapacityDepositTxOut pparams txOut
          & capacityDepositTxOutL .~ (txOut ^. capacityDepositTxOutL)
      )
        === txOut

  prop "setCapacityDepositTxOut is idempotent" $
    forAll (arbitrary @(DijkstraTxOut DijkstraEra)) $ \txOut ->
      setCapacityDepositTxOut pparams (setCapacityDepositTxOut pparams txOut)
        === setCapacityDepositTxOut pparams txOut

  prop "fundCapacityDeposit conserves the output's total ada" $
    forAll (arbitrary @(DijkstraTxOut DijkstraEra)) $ \txOut ->
      totalAdaTxOut (fundCapacityDeposit coinsPerUTxOByte txOut)
        === totalAdaTxOut txOut

  prop "fundCapacityDeposit reaches the exact tariff away from coin-width boundaries" $
    forAll (arbitrary @(DijkstraTxOut DijkstraEra)) $ \txOut ->
      hasExactCapacityDeposit (fundCapacityDeposit coinsPerUTxOByte (withAdaOutsideBoundaryWindow txOut))

  prop "fundCapacityDeposit gives everything to the deposit when the tariff is unaffordable" $
    forAll (arbitrary @(DijkstraTxOut DijkstraEra)) $ \txOut ->
      holdsAllAdaInTheDeposit
        scarceTotalAda
        (fundCapacityDeposit coinsPerUTxOByte (withScarceAda txOut))

  prop "era migration (upgrade then fund) conserves the Conway output's ada" $
    forAll (arbitrary @(TxOut ConwayEra)) $ \conwayTxOut ->
      -- On the Conway side 'coinTxOutL' is the right name: the merged coin
      -- IS the pre-split total ada.
      totalAdaTxOut (translateTxOut coinsPerUTxOByte conwayTxOut) === conwayTxOut ^. coinTxOutL

  prop "era migration reaches the exact tariff away from coin-width boundaries" $
    forAll (arbitrary @(TxOut ConwayEra)) $ \conwayTxOut ->
      hasExactCapacityDeposit (translateOutsideBoundaryWindow conwayTxOut)

  it "the tariff formula is (160 + serialised size) * coinsPerUTxOByte" $ do
    -- Anchors the formula against hand-checked numbers on a deterministic
    -- output, independently of the code under test: 'anchorTxOut' with
    -- deposit 0 serialises to 69 bytes (map header 1, key 0 + 57-byte
    -- address as a CBOR byte string 1+59, key 1 + five-byte ada coin 1+5,
    -- key 4 + one-byte zero deposit 1+1), and funding it grows the deposit
    -- field from 1 byte to 5.
    getMinCoinTxOut pparams anchorTxOut `shouldBe` Coin ((160 + 69) * 4_310)
    fundedAnchorTxOut
      ^. capacityDepositTxOutL
        `shouldBe` CapacityDeposit (Coin ((160 + 73) * 4_310))

  it "fundCapacityDeposit stays within one width-step of the tariff at a coin-width boundary" $ do
    -- 'boundaryWindowTotalAda' fully affords the tariff, but the residual
    -- straddles the 3-byte/5-byte coin-width boundary at 65,536 lovelace,
    -- so no fixpoint exists and the bounded loop stops one width flip from
    -- exact. Conservation still holds; this pins the actual guarantee in
    -- the window the exactness properties exclude.
    totalAdaTxOut boundaryWindowTxOut `shouldBe` boundaryWindowTotalAda
    abs
      ( unCoin (unCapacityDeposit (boundaryWindowTxOut ^. capacityDepositTxOutL))
          - unCoin (getMinCoinTxOut pparams boundaryWindowTxOut)
      )
      `shouldSatisfy` (<= oneWidthFlip)

-- | Mainnet pricing: 4,310 lovelace per serialised byte.
coinsPerUTxOByte :: CoinPerByte
coinsPerUTxOByte = CoinPerByte (CompactCoin 4_310)

pparams :: PParams DijkstraEra
pparams = emptyPParams & ppCoinsPerUTxOByteL .~ coinsPerUTxOByte

-- | The exact deposit rule, as a property of one output: its capacity
-- deposit is precisely its own current requirement.
hasExactCapacityDeposit :: DijkstraTxOut DijkstraEra -> Property
hasExactCapacityDeposit txOut =
  txOut ^. capacityDepositTxOutL === CapacityDeposit (getMinCoinTxOut pparams txOut)

-- | The grandfathering outcome: all the ada the output had sits in the
-- deposit, the application side is empty.
holdsAllAdaInTheDeposit :: Coin -> DijkstraTxOut DijkstraEra -> Property
holdsAllAdaInTheDeposit expectedTotalAda txOut =
  conjoin
    [ txOut ^. capacityDepositTxOutL === CapacityDeposit expectedTotalAda
    , txOut ^. assetsAdaTxOutL === Coin 0
    ]

-- | Pin the output's total ada at ten ada, deposit zero. Ten ada covers any
-- tariff a generated output can require (roughly 1 to 4.3 ada), and the
-- residual after funding — between roughly 5.7 and 9 ada — stays far from
-- every CBOR coin-width boundary, so the funding loop provably converges
-- (see the width-boundary caveat on 'fundCapacityDeposit', and the
-- dedicated boundary case above for what happens otherwise).
withAdaOutsideBoundaryWindow :: DijkstraTxOut DijkstraEra -> DijkstraTxOut DijkstraEra
withAdaOutsideBoundaryWindow txOut =
  txOut
    & assetsAdaTxOutL .~ Coin 10_000_000
    & capacityDepositTxOutL .~ mempty

-- | Total ada far below any tariff: @M(o) >= 160 * 4_310@ lovelace.
scarceTotalAda :: Coin
scarceTotalAda = Coin 1_000

withScarceAda :: DijkstraTxOut DijkstraEra -> DijkstraTxOut DijkstraEra
withScarceAda txOut =
  txOut
    & assetsAdaTxOutL .~ scarceTotalAda
    & capacityDepositTxOutL .~ mempty

-- | 'translateTxOut' with the Conway ada pinned at ten ada first, for the
-- same convergence reason as 'withAdaOutsideBoundaryWindow'.
translateOutsideBoundaryWindow :: TxOut ConwayEra -> DijkstraTxOut DijkstraEra
translateOutsideBoundaryWindow conwayTxOut =
  translateTxOut coinsPerUTxOByte (conwayTxOut & coinTxOutL .~ Coin 10_000_000)

-- | A deterministic pure-ada output: fixed example address, no datum, no
-- script, deposit zero. The anchor for the hand-computed formula cases.
simpleTxOut :: Coin -> DijkstraTxOut DijkstraEra
simpleTxOut adaAmount =
  mkBasicTxOut @DijkstraEra (mkAddr examplePayKey exampleStakeKey) (inject adaAmount)

anchorTxOut :: DijkstraTxOut DijkstraEra
anchorTxOut = simpleTxOut (Coin 10_000_000)

fundedAnchorTxOut :: DijkstraTxOut DijkstraEra
fundedAnchorTxOut = fundCapacityDeposit coinsPerUTxOByte anchorTxOut

-- | The tariff of the anchor output in its converged, both-coins-five-bytes
-- form.
anchorTariff :: CapacityDeposit
anchorTariff = fundedAnchorTxOut ^. capacityDepositTxOutL

boundaryWindowTotalAda :: Coin
boundaryWindowTotalAda = Coin (unCoin (unCapacityDeposit anchorTariff) + 60_000)

boundaryWindowTxOut :: DijkstraTxOut DijkstraEra
boundaryWindowTxOut =
  fundCapacityDeposit coinsPerUTxOByte (simpleTxOut boundaryWindowTotalAda)

-- | One CBOR coin-width flip at the 65,536 boundary: 2 bytes of serialised
-- size, priced at 4,310 lovelace each.
oneWidthFlip :: Integer
oneWidthFlip = 2 * 4_310

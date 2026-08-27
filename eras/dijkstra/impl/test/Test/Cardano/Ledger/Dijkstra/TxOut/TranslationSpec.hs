{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

-- | Properties of the output translation: the cross-era wire
-- reinterpretation, the entry-time pass-through on explicit outputs, and
-- the two validation lanes it induces.
module Test.Cardano.Ledger.Dijkstra.TxOut.TranslationSpec (spec) where

import Cardano.Ledger.BaseTypes (Mismatch (..), Relation (RelEQ, RelGTEQ))
import Cardano.Ledger.Binary (mkSized)
import Cardano.Ledger.Coin (Coin (..), CompactForm (CompactCoin))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Core (emptyPParams)
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.Core (
  CoinPerByte (..),
  PParams,
  TxOut,
  eraProtVerHigh,
  eraProtVerLow,
  getMinCoinTxOut,
  ppCoinsPerUTxOByteL,
  upgradeTxOut,
 )
import Cardano.Ledger.Dijkstra.Rules.CapacityDeposit (validateOutputCapacityDeposit)
import Cardano.Ledger.Dijkstra.TxOut (
  DijkstraEraTxOut (..),
  DijkstraTxOut,
  assetsAdaTxOutL,
 )
import Cardano.Ledger.Dijkstra.TxOut.CapacityDeposit (CapacityDeposit, depositFieldGrowthAllowance)
import Cardano.Ledger.Dijkstra.TxOut.Translation (fundCapacityDeposit)
import Cardano.Ledger.Rules.ValidationMode (Test)
import Data.List.NonEmpty (NonEmpty)
import Lens.Micro ((&), (.~))
import Test.Cardano.Ledger.Binary.RoundTrip (cborTrip, embedTripExpectation)
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Cardano.Ledger.Dijkstra.Arbitrary ()
import qualified Validation

coinsPerUTxOByte :: CoinPerByte
coinsPerUTxOByte = CoinPerByte (CompactCoin 4_310)

pparams :: PParams DijkstraEra
pparams = emptyPParams & ppCoinsPerUTxOByteL .~ coinsPerUTxOByte

scarceAda :: Coin
scarceAda = Coin 1_000

-- | An implicit output: all its ada merged in the assets, deposit 0 — the
-- legacy marker.
implicitTxOut :: Coin -> DijkstraTxOut DijkstraEra -> DijkstraTxOut DijkstraEra
implicitTxOut mergedAda txOut =
  txOut
    & assetsAdaTxOutL .~ mergedAda
    & capacityDepositTxOutL .~ mempty

-- | Run the two-lane rule on one created output, keeping the lanes apart:
-- 'Left' collects explicit-lane mismatches, 'Right' implicit-lane ones.
validateCreatedOutput ::
  DijkstraTxOut DijkstraEra ->
  Test
    ( Either
        (NonEmpty (TxOut DijkstraEra, Mismatch RelEQ CapacityDeposit))
        (NonEmpty (TxOut DijkstraEra, Mismatch RelGTEQ Coin))
    )
validateCreatedOutput txOut =
  validateOutputCapacityDeposit
    Left
    Right
    pparams
    [mkSized (eraProtVerLow @DijkstraEra) txOut]

spec :: Spec
spec = describe "TxOut translation" $ do
  prop "a Conway-encoded output decodes as its upgraded, implicit form" $
    forAll (arbitrary @(TxOut ConwayEra)) $ \conwayTxOut ->
      embedTripExpectation
        (eraProtVerHigh @ConwayEra)
        (eraProtVerLow @DijkstraEra)
        (cborTrip @(TxOut ConwayEra) @(DijkstraTxOut DijkstraEra))
        (\decodedTxOut _ -> decodedTxOut `shouldBe` upgradeTxOut @DijkstraEra conwayTxOut)
        conwayTxOut

  prop "entry-time restructuring passes an explicit exact output through unchanged" $
    forAll (arbitrary @(DijkstraTxOut DijkstraEra)) $ \txOut ->
      fundCapacityDeposit coinsPerUTxOByte (setCapacityDepositTxOut pparams txOut)
        === setCapacityDepositTxOut pparams txOut

  prop "the implicit lane accepts a merged output covering the tariff" $
    forAll (arbitrary @(DijkstraTxOut DijkstraEra)) $ \txOut ->
      validateCreatedOutput (implicitTxOut (Coin 10_000_000) txOut) === Validation.Success ()

  prop "the implicit lane rejects a merged output below the tariff" $
    forAll (arbitrary @(DijkstraTxOut DijkstraEra)) $ \txOut ->
      validateCreatedOutput (implicitTxOut scarceAda txOut)
        === Validation.Failure
          ( pure . Right . pure $
              ( implicitTxOut scarceAda txOut
              , Mismatch
                  { mismatchSupplied = scarceAda
                  , mismatchExpected =
                      getMinCoinTxOut pparams (implicitTxOut scarceAda txOut)
                        <> depositFieldGrowthAllowance coinsPerUTxOByte
                  }
              )
          )

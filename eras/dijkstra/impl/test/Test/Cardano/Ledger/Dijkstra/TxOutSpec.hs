{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Dijkstra.TxOutSpec (spec) where

import Cardano.Ledger.Binary (decNoShareCBOR, decodeFull', encodeMemPack, serialize', sizedSize)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Compactible (fromCompact, toCompact)
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Core
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Dijkstra.TxOut
import Cardano.Ledger.Dijkstra.TxOut.ApplicationAssets (ApplicationAssets (..))
import Cardano.Ledger.Dijkstra.TxOut.CapacityDeposit (CapacityDeposit (..))
import Cardano.Ledger.Dijkstra.TxOut.Value (OutputValue)
import Cardano.Ledger.Tools (ensureMinCoinTxOut, setMinCoinTxOut)
import Control.Exception (evaluate)
import Lens.Micro
import Test.Cardano.Ledger.Binary.RoundTrip (mkTrip, roundTrip)
import Test.Cardano.Ledger.Common hiding (output)
import Test.Cardano.Ledger.Dijkstra.Arbitrary ()
import qualified Test.Cardano.Ledger.Dijkstra.TxOut.Fixture as Fixture

spec :: Spec
spec = describe "Dijkstra output representation" $ do
  prop "round-trips both forms and their allocations through CBOR" $
    \(output :: TxOut DijkstraEra) -> do
      -- Setup
      let version = eraProtVerLow @DijkstraEra
      -- Exercise
      let decoded = decodeFull' version (serialize' version output)
      -- Verify
      decoded `shouldBe` Right output
  prop "round-trips form and allocation through MemPack state CBOR" $
    \(output :: TxOut DijkstraEra) -> do
      -- Setup
      let trip = mkTrip encodeMemPack decNoShareCBOR
      -- Exercise
      let decoded = roundTrip (eraProtVerLow @DijkstraEra) trip output
      -- Verify
      either (expectationFailure . show) (`shouldBe` output) decoded
  prop "decodes Conway bytes as the structural implicit upgrade" $
    \(output :: TxOut ConwayEra) -> do
      -- Setup
      let historicalBytes = serialize' (eraProtVerHigh @ConwayEra) output
      -- Exercise
      let decoded = decodeFull' @(TxOut DijkstraEra) (eraProtVerLow @DijkstraEra) historicalBytes
      -- Verify
      decoded `shouldBe` Right (upgradeTxOut @DijkstraEra output)
  prop "the implicit upgrade preserves Conway's canonical output bytes" $
    \(output :: TxOut ConwayEra) -> do
      -- Setup
      let historicalBytes = serialize' (eraProtVerHigh @ConwayEra) output
      -- Exercise
      let upgradedBytes = serialize' (eraProtVerLow @DijkstraEra) (upgradeTxOut @DijkstraEra output)
      -- Verify
      upgradedBytes `shouldBe` historicalBytes
  prop "compact output allocation preserves the typed allocation" $
    \(allocation :: OutputValue) -> do
      -- Setup
      let original = allocation
      -- Exercise
      let restored = fmap fromCompact (toCompact original)
      -- Verify
      restored `shouldBe` Just original
  prop "reading and setting the typed allocation is identity" $
    \(output :: TxOut DijkstraEra) -> do
      -- Setup
      let original = output
      -- Exercise
      let updated = original & outputValueTxOutL .~ original ^. outputValueTxOutL
      -- Verify
      updated `shouldBe` original
  prop "typed allocation replacement reads back" $
    \(output :: TxOut DijkstraEra) (allocation :: OutputValue) -> do
      -- Setup
      let original = output
      -- Exercise
      let updated = original & outputValueTxOutL .~ allocation
      -- Verify
      updated ^. outputValueTxOutL `shouldBe` allocation
  prop "typed allocation replacement preserves the form" $
    \(output :: TxOut DijkstraEra) (allocation :: OutputValue) -> do
      -- Setup
      let original = output
      -- Exercise
      let updated = original & outputValueTxOutL .~ allocation
      -- Verify
      updated ^. capacityDepositFormTxOutL `shouldBe` original ^. capacityDepositFormTxOutL
  prop "the last allocation replacement wins" $
    \(output :: TxOut DijkstraEra) (first :: OutputValue) (second :: OutputValue) -> do
      -- Setup
      let original = output
      -- Exercise
      let updated = original & outputValueTxOutL .~ first & outputValueTxOutL .~ second
      -- Verify
      updated `shouldBe` (original & outputValueTxOutL .~ second)
  it "counts capacity and application ADA once" $ do
    -- Setup
    let output = Fixture.splitOutput
    -- Exercise
    let actual = output ^. potCoinsTxOutF
    -- Verify
    actual `shouldBe` Coin 20
  it "equal-total allocations have equal full accounting projections" $ do
    -- Setup
    let first = Fixture.splitOutput
        second = Fixture.otherAllocation
    -- Exercise
    let firstPot = first ^. potValueTxOutF
    -- Verify
    firstPot `shouldBe` second ^. potValueTxOutF
  it "equal-total allocations remain distinct outputs" $ do
    -- Setup
    let first = Fixture.splitOutput
        second = Fixture.otherAllocation
    -- Exercise
    let equal = first == second
    -- Verify
    equal `shouldBe` False
  it "equal-total allocations have distinct CBOR" $ do
    -- Setup
    let first = Fixture.splitOutput
        second = Fixture.otherAllocation
    -- Exercise
    let encoded = serialize' (eraProtVerLow @DijkstraEra) first
    -- Verify
    encoded `shouldNotBe` serialize' (eraProtVerLow @DijkstraEra) second
  it "explicit zero is classified as explicit" $ do
    -- Setup
    let output = Fixture.explicitZeroOutput
    -- Exercise
    let form = output ^. capacityDepositFormTxOutL
    -- Verify
    form `shouldBe` ExplicitCapacityDeposit
  it "the merged output is classified as implicit" $ do
    -- Setup
    let output = Fixture.implicitOutput
    -- Exercise
    let form = output ^. capacityDepositFormTxOutL
    -- Verify
    form `shouldBe` ImplicitCapacityDeposit
  it "explicit zero and implicit allocation are structurally distinct" $ do
    -- Setup
    let explicit = Fixture.explicitZeroOutput
        implicit = Fixture.implicitOutput
    -- Exercise
    let equal = explicit == implicit
    -- Verify
    equal `shouldBe` False
  it "round-trips explicit zero without converting it to implicit" $ do
    -- Setup
    let output = Fixture.explicitZeroOutput
        version = eraProtVerLow @DijkstraEra
    -- Exercise
    let decoded = decodeFull' version (serialize' version output)
    -- Verify
    decoded `shouldBe` Right output
  it "round-trips implicit form without adding a stated deposit" $ do
    -- Setup
    let output = Fixture.implicitOutput
        version = eraProtVerLow @DijkstraEra
    -- Exercise
    let decoded = decodeFull' version (serialize' version output)
    -- Verify
    decoded `shouldBe` Right output
  it "explicit zero and implicit form have different encodings" $ do
    -- Setup
    let explicit = Fixture.explicitZeroOutput
        implicit = Fixture.implicitOutput
    -- Exercise
    let encoded = serialize' (eraProtVerLow @DijkstraEra) explicit
    -- Verify
    encoded `shouldNotBe` serialize' (eraProtVerLow @DijkstraEra) implicit
  it "prices implicit outputs on the canonical explicit shape" $ do
    -- Setup
    let implicit = Fixture.implicitOutput
        explicit = Fixture.explicitZeroOutput
    -- Exercise
    let measured = sizedSize (measureTxOut implicit)
    -- Verify
    measured `shouldBe` sizedSize (measureTxOut explicit)
  it "the application coin setter reads back the new amount" $ do
    -- Setup
    let output = Fixture.splitOutput
    -- Exercise
    let updated = output & coinTxOutL .~ Coin 3
    -- Verify
    updated ^. coinTxOutL `shouldBe` Coin 3
  it "changing application ADA preserves capacity" $ do
    -- Setup
    let output = Fixture.splitOutput
    -- Exercise
    let updated = output & coinTxOutL .~ Coin 3
    -- Verify
    updated ^. capacityDepositTxOutL `shouldBe` CapacityDeposit (Coin 7)
  it "changing application ADA preserves native assets" $ do
    -- Setup
    let output = Fixture.splitOutput
    -- Exercise
    let updated = output & coinTxOutL .~ Coin 3
    -- Verify
    nativeAssets (updated ^. applicationAssetsTxOutL)
      `shouldBe` nativeAssets (output ^. applicationAssetsTxOutL)
  it "changing application ADA updates the output total" $ do
    -- Setup
    let output = Fixture.splitOutput
    -- Exercise
    let updated = output & coinTxOutL .~ Coin 3
    -- Verify
    updated ^. potCoinsTxOutF `shouldBe` Coin 10
  it "changing capacity preserves application assets" $ do
    -- Setup
    let output = Fixture.splitOutput
    -- Exercise
    let updated = output & capacityDepositTxOutL .~ CapacityDeposit (Coin 2)
    -- Verify
    updated ^. applicationAssetsTxOutL `shouldBe` output ^. applicationAssetsTxOutL
  it "changing capacity preserves explicit form" $ do
    -- Setup
    let output = Fixture.splitOutput
    -- Exercise
    let updated = output & capacityDepositTxOutL .~ CapacityDeposit (Coin 2)
    -- Verify
    updated ^. capacityDepositFormTxOutL `shouldBe` ExplicitCapacityDeposit
  it "changing capacity updates the output total" $ do
    -- Setup
    let output = Fixture.splitOutput
    -- Exercise
    let updated = output & capacityDepositTxOutL .~ CapacityDeposit (Coin 2)
    -- Verify
    updated ^. potCoinsTxOutF `shouldBe` Coin 15
  it "does not silently serialize away an implicit output's nonzero deposit" $ do
    -- Setup
    let malformed = Fixture.implicitOutput & capacityDepositTxOutL .~ CapacityDeposit (Coin 1)
    -- Exercise
    let encoded = serialize' (eraProtVerLow @DijkstraEra) malformed
    -- Verify
    evaluate encoded
      `shouldThrow` errorCall "DijkstraTxOut: implicit output carries an allocated capacity deposit"
  it "the zero-application fixture carries the exact capacity tariff" $ do
    -- Setup
    let output = Fixture.zeroApplicationOutput
    -- Exercise
    let required = getCapacityDepositRequirement Fixture.pricedPParams output
    -- Verify
    output ^. capacityDepositTxOutL `shouldBe` required
  it "explicit outputs require no minimum application ADA" $ do
    -- Setup
    let output = Fixture.zeroApplicationOutput
    -- Exercise
    let minimumApplicationCoins = getMinCoinTxOut Fixture.pricedPParams output
    -- Verify
    minimumApplicationCoins `shouldBe` Coin 0
  it "sized explicit outputs require no minimum application ADA" $ do
    -- Setup
    let output = measureTxOut Fixture.zeroApplicationOutput
    -- Exercise
    let minimumApplicationCoins = getMinCoinSizedTxOut Fixture.pricedPParams output
    -- Verify
    minimumApplicationCoins `shouldBe` Coin 0
  it "implicit outputs retain the capacity tariff and growth allowance" $ do
    -- Setup
    let output = Fixture.implicitOutput
        required = unCapacityDeposit (getCapacityDepositRequirement Fixture.pricedPParams output)
    -- Exercise
    let minimumCoins = getMinCoinTxOut Fixture.pricedPParams output
    -- Verify
    minimumCoins `shouldBe` required <> Coin 34480
  it "ensuring the minimum preserves an exact zero-application output" $ do
    -- Setup
    let output = Fixture.zeroApplicationOutput
    -- Exercise
    let updated = ensureMinCoinTxOut Fixture.pricedPParams output
    -- Verify
    updated `shouldBe` output
  it "setting the explicit minimum changes only application ADA" $ do
    -- Setup
    let output = Fixture.splitOutput
    -- Exercise
    let updated = setMinCoinTxOut Fixture.pricedPParams output
    -- Verify
    updated `shouldBe` (output & coinTxOutL .~ Coin 0)
  it "ensuring application ADA does not fund an invalid explicit zero deposit" $ do
    -- Setup
    let output = Fixture.explicitZeroOutput
    -- Exercise
    let updated = ensureMinCoinTxOut Fixture.pricedPParams output
    -- Verify
    updated ^. capacityDepositTxOutL `shouldBe` CapacityDeposit (Coin 0)

{-# LANGUAGE PatternSynonyms #-}

module Test.Cardano.Ledger.Dijkstra.TxOut.AllocationSpec (spec) where

import Cardano.Ledger.Babbage.TxOut (BabbageEraTxOut (..), valueEitherBabbageTxOutL)
import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Core (EraTxOut (..), coinTxOutL)
import Cardano.Ledger.Dijkstra.TxOut (
  DijkstraTxOut (DijkstraTxOut),
  capacityDepositTxOutF,
  fromBabbageTxOut,
  toBabbageTxOut,
 )
import Cardano.Ledger.Dijkstra.TxOut.LedgerInstances ()
import Cardano.Ledger.Dijkstra.TxOut.Value (OutputValue (..))
import Cardano.Ledger.Dijkstra.TxOut.Value.Translation (AllocationError (..))
import Cardano.Ledger.Val (coin)
import Control.Exception (evaluate)
import Lens.Micro ((&), (.~), (^.))
import Test.Cardano.Ledger.Common
import qualified Test.Cardano.Ledger.Dijkstra.TxOut.Allocation.Fixture as Fixture

spec :: Spec
spec = describe "DijkstraTxOut allocation" $ do
  forM_ Fixture.noncanonicalProjections $ \projection ->
    it "preserves the quantities and fields of a noncanonical compact ADA output" $
      ( Fixture.projectedOutputFields . toBabbageTxOut
          <$> fromBabbageTxOut Fixture.suppliedDeposit projection
      )
        `shouldBe` Right (Fixture.projectedOutputFields projection)

  forM_ Fixture.allocationCases $ \(name, fixture) -> describe name $ do
    let totalProjection = Fixture.totalProjection fixture
        txOut = Fixture.allocatedOutput fixture

    it "stores the supplied capacity deposit" $
      txOut ^. capacityDepositTxOutF `shouldBe` Fixture.suppliedDeposit

    it "reads the total value including the deposit" $
      txOut ^. valueTxOutL `shouldBe` Fixture.expectedTotalValue fixture

    it "preserves the deposit when replacing the total value" $
      (txOut & valueTxOutL .~ Fixture.replacementValue)
        ^. capacityDepositTxOutF
          `shouldBe` Fixture.suppliedDeposit

    it "preserves the deposit when replacing total ADA" $
      (txOut & coinTxOutL .~ Fixture.replacementCoins)
        ^. capacityDepositTxOutF
          `shouldBe` Fixture.suppliedDeposit

    it "assigns the remainder of a replacement total to application assets" $ do
      let DijkstraTxOut _ allocation _ _ = txOut & valueTxOutL .~ Fixture.replacementValue
      applicationAssets allocation `shouldBe` Fixture.replacementAssets

    it "fails explicitly when a replacement total cannot fund the stored deposit" $
      evaluate ((txOut & coinTxOutL .~ Fixture.insufficientTotalCoins) ^. capacityDepositTxOutF)
        `shouldThrow` anyErrorCall

    it "preserves the deposit when replacing the address" $
      (txOut & addrTxOutL .~ Fixture.replacementAddress)
        ^. capacityDepositTxOutF
          `shouldBe` Fixture.suppliedDeposit

    it "preserves the deposit when replacing the datum" $
      (txOut & datumTxOutL .~ Fixture.replacementDatum)
        ^. capacityDepositTxOutF
          `shouldBe` Fixture.suppliedDeposit

    it "preserves the deposit when replacing the reference script" $
      (txOut & referenceScriptTxOutL .~ SJust Fixture.replacementScript)
        ^. capacityDepositTxOutF
          `shouldBe` Fixture.suppliedDeposit

    it "exposes both allocations through the public pattern" $ do
      let DijkstraTxOut _ allocation _ _ = txOut
      allocation
        `shouldBe` OutputValue Fixture.suppliedDeposit (Fixture.expectedApplicationAssets fixture)

    it "reconstructs the output through the public pattern" $ do
      let DijkstraTxOut address allocation datum script = txOut
      DijkstraTxOut address allocation datum script `shouldBe` txOut

    it "merges the deposit and application assets into the total projection" $
      toBabbageTxOut txOut `shouldBe` totalProjection

    it "includes the deposit exactly once in projected ADA" $
      coin (either id fromCompact (toBabbageTxOut txOut ^. valueEitherBabbageTxOutL))
        `shouldBe` coin (Fixture.expectedTotalValue fixture)

    it "recovers the expected allocation from the total projection" $
      fromBabbageTxOut Fixture.suppliedDeposit totalProjection `shouldBe` Right txOut

    it "reconstructs the same storage from the projection and retained deposit" $
      fromBabbageTxOut (txOut ^. capacityDepositTxOutF) (toBabbageTxOut txOut) `shouldBe` Right txOut

    it "rejects a negative requested deposit" $
      fromBabbageTxOut Fixture.negativeDeposit totalProjection
        `shouldBe` Left (NegativeCapacityDeposit Fixture.negativeDeposit)

    it "rejects a requested deposit exceeding total ADA" $
      fromBabbageTxOut Fixture.excessiveDeposit totalProjection
        `shouldBe` Left
          ( CapacityDepositExceedsOutputCoins
              (coin (Fixture.expectedTotalValue fixture))
              Fixture.excessiveDeposit
          )

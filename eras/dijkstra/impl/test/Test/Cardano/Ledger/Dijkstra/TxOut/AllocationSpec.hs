{-# LANGUAGE PatternSynonyms #-}

module Test.Cardano.Ledger.Dijkstra.TxOut.AllocationSpec (spec) where

import Cardano.Ledger.Babbage.TxOut (BabbageEraTxOut (..))
import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import Cardano.Ledger.Core (EraTxOut (..), coinTxOutL)
import Cardano.Ledger.Dijkstra.TxOut (
  DijkstraTxOut (DijkstraTxOut),
  capacityDepositTxOutF,
  fromBabbageTxOut,
  toBabbageTxOut,
 )
import Cardano.Ledger.Dijkstra.TxOut.Value (OutputValue (..))
import Lens.Micro ((&), (.~), (^.))
import Test.Cardano.Ledger.Common
import qualified Test.Cardano.Ledger.Dijkstra.TxOut.Allocation.Fixture as Fixture

spec :: Spec
spec = describe "DijkstraTxOut allocation" $
  forM_ Fixture.allocationCases $ \(name, fixture) -> describe name $ do
    let applicationProjection = Fixture.applicationProjection fixture
        txOut = fromBabbageTxOut Fixture.suppliedDeposit applicationProjection

    it "stores the supplied capacity deposit" $
      txOut ^. capacityDepositTxOutF `shouldBe` Fixture.suppliedDeposit

    it "reads application assets independently of the deposit" $
      txOut ^. valueTxOutL `shouldBe` Fixture.expectedApplicationAssets fixture

    it "preserves the deposit when replacing application assets" $
      (txOut & valueTxOutL .~ Fixture.replacementAssets)
        ^. capacityDepositTxOutF
          `shouldBe` Fixture.suppliedDeposit

    it "preserves the deposit when replacing application ADA" $
      (txOut & coinTxOutL .~ Fixture.replacementCoins)
        ^. capacityDepositTxOutF
          `shouldBe` Fixture.suppliedDeposit

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

    it "retains the application's compact storage in the projection" $
      toBabbageTxOut txOut `shouldBe` applicationProjection

    it "reconstructs the same storage from the projection and retained deposit" $
      fromBabbageTxOut (txOut ^. capacityDepositTxOutF) (toBabbageTxOut txOut) `shouldBe` txOut

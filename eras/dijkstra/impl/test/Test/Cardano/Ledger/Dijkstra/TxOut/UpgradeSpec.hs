{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Dijkstra.TxOut.UpgradeSpec (spec) where

import Cardano.Ledger.Core (EraTxOut (..))
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.TxOut (
  DijkstraTxOut (DijkstraTxOut),
  capacityDepositTxOutF,
 )
import Cardano.Ledger.Dijkstra.TxOut.ApplicationAssets (nativeAssets)
import Cardano.Ledger.Dijkstra.TxOut.Translation (fromConway, fromConway')
import Cardano.Ledger.Dijkstra.TxOut.Value (outputCoins)
import Cardano.Ledger.Mary.Value (MaryValue (..))
import Control.Exception (evaluate)
import Lens.Micro ((^.))
import Test.Cardano.Ledger.Common
import qualified Test.Cardano.Ledger.Dijkstra.TxOut.Upgrade.Fixture as Fixture

spec :: Spec
spec = describe "DijkstraTxOut upgrade" $ do
  forM_ Fixture.policyPrices $ \price -> describe ("source price " <> show price) $
    forM_ Fixture.outputSizeScenarios $ \(name, source) -> describe name $ do
      let txOut = upgradeTxOut @DijkstraEra (Fixture.pricedPParams price) source
          DijkstraTxOut _ allocation _ _ = txOut
          MaryValue sourceCoins sourceNativeAssets = source ^. valueTxOutL

      it "recovers the capacity deposit using the standard Conway policy" $
        (txOut ^. capacityDepositTxOutF) `shouldBe` Fixture.capacityDepositAtPrice price source

      it "preserves total ADA across both allocations" $
        outputCoins allocation `shouldBe` sourceCoins

      it "preserves native assets in the application allocation" $
        nativeAssets (txOut ^. valueTxOutL) `shouldBe` sourceNativeAssets

  it "returns the allocation error when the source cannot fund its deposit" $
    fromConway (Fixture.pricedPParams 4310) Fixture.underfundedOutput
      `shouldBe` Left Fixture.underfundedAllocationError

  it "propagates a negative-deposit error from allocation recovery" $
    fromConway' Fixture.recoverNegativeCapacityDeposit Fixture.fundedOutput
      `shouldBe` Left Fixture.negativeAllocationError

  it "fails the upgrade explicitly when the source cannot fund its deposit" $
    evaluate
      (upgradeTxOut @DijkstraEra (Fixture.pricedPParams 4310) Fixture.underfundedOutput)
      `shouldThrow` errorCall (Fixture.allocationInvariantViolation Fixture.underfundedAllocationError)

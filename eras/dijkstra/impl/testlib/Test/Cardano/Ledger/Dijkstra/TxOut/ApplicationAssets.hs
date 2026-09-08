{-# OPTIONS_GHC -Wno-orphans #-}

-- | Test instances live beside their concept. Coin and MultiAsset provide their
-- own test instances in testlib, so these cannot live in the production module.
module Test.Cardano.Ledger.Dijkstra.TxOut.ApplicationAssets () where

import Cardano.Ledger.Dijkstra.TxOut.ApplicationAssets (ApplicationAssets (..))
import Cardano.Ledger.Mary.Value (MaryValue (..))
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Mary.Arbitrary ()
import Test.Cardano.Ledger.Mary.TreeDiff ()

instance Arbitrary ApplicationAssets where
  arbitrary = do
    MaryValue coins native <- arbitrary
    pure $ ApplicationAssets coins native

instance ToExpr ApplicationAssets

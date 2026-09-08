{-# OPTIONS_GHC -Wno-orphans #-}

-- | Test instances require Coin's testlib instances and stay out of the library.
module Test.Cardano.Ledger.Dijkstra.TxOut.CapacityDeposit () where

import Cardano.Ledger.Compactible (Compactible (fromCompact))
import Cardano.Ledger.Dijkstra.TxOut.CapacityDeposit (CapacityDeposit (..))
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Cardano.Ledger.TreeDiff ()

instance Arbitrary CapacityDeposit where
  arbitrary = CapacityDeposit . fromCompact <$> arbitrary

instance ToExpr CapacityDeposit

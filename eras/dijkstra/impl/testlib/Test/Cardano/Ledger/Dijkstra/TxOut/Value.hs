{-# OPTIONS_GHC -Wno-orphans #-}

-- | Test instances follow the production concept's module ownership.
module Test.Cardano.Ledger.Dijkstra.TxOut.Value () where

import Cardano.Ledger.Dijkstra.TxOut.Value (OutputValue (..))
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Dijkstra.TxOut.ApplicationAssets ()
import Test.Cardano.Ledger.Dijkstra.TxOut.CapacityDeposit ()

instance Arbitrary OutputValue where
  arbitrary = OutputValue <$> arbitrary <*> arbitrary

instance ToExpr OutputValue

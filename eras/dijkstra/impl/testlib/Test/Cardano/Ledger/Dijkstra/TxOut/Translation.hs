{-# OPTIONS_GHC -Wno-orphans #-}

-- | Translation-failure test instances depend on the existing Coin testlib.
module Test.Cardano.Ledger.Dijkstra.TxOut.Translation () where

import Cardano.Ledger.Dijkstra.TxOut.Translation (CapacityDepositAllocationError (..))
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Cardano.Ledger.Dijkstra.TxOut.CapacityDeposit ()
import Test.Cardano.Ledger.TreeDiff ()

instance Arbitrary CapacityDepositAllocationError where
  arbitrary =
    oneof
      [ CapacityDepositInsufficient <$> arbitrary <*> arbitrary
      , CapacityDepositNoExactAllocation <$> arbitrary
      , CapacityDepositInvalidTotal <$> arbitrary
      ]

instance ToExpr CapacityDepositAllocationError

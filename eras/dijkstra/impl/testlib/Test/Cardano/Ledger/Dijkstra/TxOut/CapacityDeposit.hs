{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Test instances of 'CapacityDeposit', kept next to the concept. They are
-- orphans by necessity, not by choice: 'Arbitrary' and 'ToExpr' of the
-- underlying 'Cardano.Ledger.Coin.Coin' are themselves testlib orphans
-- upstream, so these cannot live in
-- "Cardano.Ledger.Dijkstra.TxOut.CapacityDeposit" without the library
-- depending on testlibs.
module Test.Cardano.Ledger.Dijkstra.TxOut.CapacityDeposit () where

import Cardano.Ledger.Dijkstra.TxOut.CapacityDeposit (
  CapacityDeposit (..),
  CompactForm (..),
 )
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Cardano.Ledger.TreeDiff ()

instance Arbitrary CapacityDeposit where
  arbitrary = CapacityDeposit <$> arbitrary

instance Arbitrary (CompactForm CapacityDeposit) where
  arbitrary = CompactCapacityDeposit <$> arbitrary

deriving newtype instance ToExpr CapacityDeposit

deriving newtype instance ToExpr (CompactForm CapacityDeposit)

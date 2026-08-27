{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Test instances of 'Assets', kept next to the concept. They are orphans
-- by necessity, not by choice: 'Arbitrary' and 'ToExpr' of the underlying
-- 'MaryValue' are themselves testlib orphans upstream, so these cannot live
-- in "Cardano.Ledger.Dijkstra.Assets" without the library depending on
-- testlibs.
module Test.Cardano.Ledger.Dijkstra.Assets () where

import Cardano.Ledger.Dijkstra.Assets (Assets (..), CompactForm (..))
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Mary.Arbitrary ()
import Test.Cardano.Ledger.Mary.TreeDiff ()

instance Arbitrary Assets where
  arbitrary = Assets <$> arbitrary

instance Arbitrary (CompactForm Assets) where
  arbitrary = CompactAssets <$> arbitrary

deriving newtype instance ToExpr Assets

deriving newtype instance ToExpr (CompactForm Assets)

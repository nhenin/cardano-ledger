{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Dijkstra.TxOut.Historical.Fixture (
  boundedOutputs,
) where

import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import Lens.Micro
import Test.Cardano.Ledger.Common hiding (output)

-- | Bound the collection and each coin amount so the trusted compact summation
-- has no Word64 overflow, independently of the era's native-asset generator.
boundedOutputs :: (EraTxOut era, Arbitrary (TxOut era)) => Gen [TxOut era]
boundedOutputs = do
  count <- choose (0, 12)
  vectorOf count $ do
    output <- arbitrary
    coins <- Coin <$> choose (0, 1000000)
    pure $ output & coinTxOutL .~ coins

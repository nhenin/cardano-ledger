{-# LANGUAGE OverloadedStrings #-}

module Test.Cardano.Ledger.Dijkstra.TxOut.ValueSpec (spec) where

import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Dijkstra.TxOut.ApplicationAssets (ApplicationAssets (..))
import Cardano.Ledger.Dijkstra.TxOut.CapacityDeposit (CapacityDeposit (..))
import Cardano.Ledger.Dijkstra.TxOut.Value (OutputValue (..), outputCoins)
import Cardano.Ledger.Hashes (ScriptHash (..))
import Cardano.Ledger.Mary.AssetName (AssetName (..))
import Cardano.Ledger.Mary.MultiAsset (MultiAsset (..))
import Cardano.Ledger.Mary.PolicyID (PolicyID (..))
import qualified Data.Map.Strict as Map
import Test.Cardano.Ledger.Common

spec :: Spec
spec = describe "OutputValue allocation" $
  forM_ nativeFixtures $ \(scenario, native) -> describe scenario $ do
    let assets coins = ApplicationAssets (Coin coins) (MultiAsset native)
        allocation deposit coins = OutputValue (CapacityDeposit (Coin deposit)) (assets coins)

    it "counts both ADA allocations once, including zero allocations" $
      forM_ [(0, 0, 0), (0, 5, 5), (2, 0, 2), (2, 5, 7)] $ \(deposit, coins, expectedTotal) ->
        outputCoins (allocation deposit coins) `shouldBe` Coin expectedTotal

    it "distinguishes allocations even when their total ADA and native assets agree" $ do
      let first = allocation 2 5
          second = allocation 3 4
      outputCoins first `shouldBe` outputCoins second
      first `shouldNotBe` second

-- Raw zeros and empty policies intentionally exercise representation retention,
-- not output validity. Native quantities never contribute to the ADA total.
nativeFixtures :: [(String, Map.Map PolicyID (Map.Map AssetName Integer))]
nativeFixtures =
  [ ("ADA only", Map.empty)
  ,
    ( "native assets with raw zero and empty entries"
    , Map.fromList
        [
          ( PolicyID (ScriptHash "00000000000000000000000000000000000000000000000000000000")
          , Map.fromList [(AssetName "token", 100), (AssetName "zero", 0)]
          )
        , (PolicyID (ScriptHash "01010101010101010101010101010101010101010101010101010101"), Map.empty)
        ]
    )
  ]

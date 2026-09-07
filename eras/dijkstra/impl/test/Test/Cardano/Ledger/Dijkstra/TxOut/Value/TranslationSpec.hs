{-# LANGUAGE OverloadedStrings #-}

module Test.Cardano.Ledger.Dijkstra.TxOut.Value.TranslationSpec (spec) where

import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Dijkstra.TxOut.ApplicationAssets (ApplicationAssets (..))
import Cardano.Ledger.Dijkstra.TxOut.CapacityDeposit (CapacityDeposit (..))
import Cardano.Ledger.Dijkstra.TxOut.Value (OutputValue (..), outputCoins)
import Cardano.Ledger.Dijkstra.TxOut.Value.Translation (
  AllocationError (..),
  fromMaryValue,
  toMaryValue,
 )
import Cardano.Ledger.Hashes (ScriptHash (..))
import Cardano.Ledger.Mary.AssetName (AssetName (..))
import Cardano.Ledger.Mary.MultiAsset (MultiAsset (..))
import Cardano.Ledger.Mary.PolicyID (PolicyID (..))
import Cardano.Ledger.Mary.Value (MaryValue (..))
import qualified Data.Map.Strict as Map
import Test.Cardano.Ledger.Common

spec :: Spec
spec = describe "OutputValue translation" $ do
  forM_ nativeFixtures $ \(scenario, native) -> describe scenario $ do
    it "allocates zero, part or all of total ADA while retaining exact native entries" $
      forM_ [(0, 0, 0), (13, 0, 13), (13, 4, 9), (13, 13, 0)] $ \(sourceCoins, requested, remaining) -> do
        let source = MaryValue (Coin sourceCoins) (MultiAsset native)
            deposit = CapacityDeposit (Coin requested)
        case fromMaryValue deposit source of
          Left err -> expectationFailure $ "Unexpected allocation failure: " <> show err
          Right value -> do
            capacityDeposit value `shouldBe` deposit
            applicationCoins (applicationAssets value) `shouldBe` Coin remaining
            outputCoins value `shouldBe` Coin sourceCoins
            let MultiAsset mappedNative = nativeAssets (applicationAssets value)
                MaryValue mappedTotal (MultiAsset flattenedNative) = toMaryValue value
            mappedNative `shouldBe` native
            mappedTotal `shouldBe` Coin sourceCoins
            flattenedNative `shouldBe` native

    it "round-trips an allocation when its deposit is retained separately" $ do
      let first = OutputValue (CapacityDeposit (Coin 4)) (ApplicationAssets (Coin 9) (MultiAsset native))
          second = OutputValue (CapacityDeposit (Coin 9)) (ApplicationAssets (Coin 4) (MultiAsset native))
      -- Flattening alone cannot distinguish these allocations.
      first `shouldNotBe` second
      toMaryValue first `shouldBe` toMaryValue second
      forM_ [first, second] $ \value ->
        fromMaryValue (capacityDeposit value) (toMaryValue value) `shouldBe` Right value

  it "rejects a negative requested deposit" $ do
    let deposit = CapacityDeposit (Coin (-1))
    fromMaryValue deposit (MaryValue (Coin 10) mempty)
      `shouldBe` Left (NegativeCapacityDeposit deposit)

  it "reports available and requested ADA when the allocation cannot be funded" $
    forM_ [(10, 11), (-1, 0)] $ \(available, requested) -> do
      let availableCoins = Coin available
          deposit = CapacityDeposit (Coin requested)
      fromMaryValue deposit (MaryValue availableCoins mempty)
        `shouldBe` Left (CapacityDepositExceedsOutputCoins availableCoins deposit)

-- The mapper deliberately accepts unchecked native maps. Direct Map equality
-- observes raw entries that pointwise MultiAsset/MaryValue equality can ignore.
nativeFixtures :: [(String, Map.Map PolicyID (Map.Map AssetName Integer))]
nativeFixtures =
  [ ("ADA only", Map.empty)
  ,
    ( "unchecked native quantities and empty policies"
    , Map.fromList
        [
          ( PolicyID (ScriptHash "00000000000000000000000000000000000000000000000000000000")
          , Map.fromList [(AssetName "token", 100), (AssetName "negative", -3), (AssetName "zero", 0)]
          )
        , (PolicyID (ScriptHash "01010101010101010101010101010101010101010101010101010101"), Map.empty)
        ]
    )
  ]

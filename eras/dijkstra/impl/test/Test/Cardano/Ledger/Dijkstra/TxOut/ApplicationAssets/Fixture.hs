{-# LANGUAGE OverloadedStrings #-}

module Test.Cardano.Ledger.Dijkstra.TxOut.ApplicationAssets.Fixture (
  genApplicationBalance,
  genBalancePair,
  genBalanceTriple,
  genScaledBalances,
  genOutputAssets,
  genAdaOnlyAssets,
  genApplicationCoins,
  assetsWithCoinsAndToken,
  nonCompactableAssets,
  maximumOutputAssets,
  rawZeroAssets,
) where

import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Dijkstra.TxOut.ApplicationAssets (ApplicationAssets (..))
import Cardano.Ledger.Hashes (ScriptHash (..))
import Cardano.Ledger.Mary.AssetName (AssetName (..))
import Cardano.Ledger.Mary.MultiAsset (MultiAsset (..), multiAssetFromList)
import Cardano.Ledger.Mary.PolicyID (PolicyID (..))
import Cardano.Ledger.Mary.Value (MaryValue (..))
import qualified Data.Map.Strict as Map
import Data.Word (Word64)
import Test.Cardano.Ledger.Common

-- Balances may be signed during accounting, independently of output validity.
genApplicationBalance :: Gen ApplicationAssets
genApplicationBalance =
  ApplicationAssets
    <$> (MaryValue <$> genApplicationCoins <*> genNativeAssets (chooseInteger (-10000, 10000)))

genBalancePair :: Gen (ApplicationAssets, ApplicationAssets)
genBalancePair = (,) <$> genApplicationBalance <*> genApplicationBalance

genBalanceTriple :: Gen (ApplicationAssets, ApplicationAssets, ApplicationAssets)
genBalanceTriple = (,,) <$> genApplicationBalance <*> genApplicationBalance <*> genApplicationBalance

genScaledBalances :: Gen (Integer, ApplicationAssets, ApplicationAssets)
genScaledBalances =
  (,,) <$> chooseInteger (-100, 100) <*> genApplicationBalance <*> genApplicationBalance

genOutputAssets :: Gen ApplicationAssets
genOutputAssets =
  ApplicationAssets
    <$> ( MaryValue
            <$> (Coin <$> oneof [elements [0, maximumQuantity], chooseInteger (0, 1000000)])
            <*> genNativeAssets (chooseInteger (0, 10000))
        )

genAdaOnlyAssets :: Gen ApplicationAssets
genAdaOnlyAssets = (\coins -> ApplicationAssets (MaryValue coins mempty)) <$> genApplicationCoins

genApplicationCoins :: Gen Coin
genApplicationCoins = Coin <$> chooseInteger (-1000000, 1000000)

genNativeAssets :: Gen Integer -> Gen MultiAsset
genNativeAssets genQuantity = do
  selectedAssets <-
    sublistOf
      [ (firstPolicy, AssetName "token")
      , (firstPolicy, AssetName "second")
      , (secondPolicy, AssetName "token")
      ]
  quantities <- vectorOf (length selectedAssets) genQuantity
  pure $
    multiAssetFromList
      [(policy, assetName, quantity) | ((policy, assetName), quantity) <- zip selectedAssets quantities]

assetsWithCoinsAndToken :: Integer -> Integer -> ApplicationAssets
assetsWithCoinsAndToken coins quantity =
  ApplicationAssets
    (MaryValue (Coin coins) (multiAssetFromList [(firstPolicy, AssetName "token", quantity)]))

nonCompactableAssets :: [(String, ApplicationAssets)]
nonCompactableAssets =
  [ ("negative application ADA", assetsWithCoinsAndToken (-1) 1)
  , ("application ADA beyond Word64", assetsWithCoinsAndToken (maximumQuantity + 1) 1)
  , ("a negative native quantity", assetsWithCoinsAndToken 1 (-1))
  , ("a native quantity beyond Word64", assetsWithCoinsAndToken 1 (maximumQuantity + 1))
  ]

maximumOutputAssets :: ApplicationAssets
maximumOutputAssets = assetsWithCoinsAndToken maximumQuantity maximumQuantity

rawZeroAssets :: ApplicationAssets
rawZeroAssets =
  ApplicationAssets $
    MaryValue (Coin 0) $
      MultiAsset $
        Map.fromList
          [ (firstPolicy, Map.singleton (AssetName "token") 0)
          , (secondPolicy, Map.empty)
          ]

maximumQuantity :: Integer
maximumQuantity = toInteger (maxBound :: Word64)

firstPolicy :: PolicyID
firstPolicy = PolicyID (ScriptHash "00000000000000000000000000000000000000000000000000000000")

secondPolicy :: PolicyID
secondPolicy = PolicyID (ScriptHash "01010101010101010101010101010101010101010101010101010101")

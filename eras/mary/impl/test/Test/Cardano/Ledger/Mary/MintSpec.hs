{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Mary.MintSpec (spec) where

import Cardano.Ledger.Binary (serialize')
import Cardano.Ledger.Core (TxLevel (TopTx), eraProtVerLow, mkBasicTxBody)
import Cardano.Ledger.Hashes (ScriptHash (..))
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Mary.Mint
import Cardano.Ledger.Mary.TxBody
import Cardano.Ledger.Mary.Value (
  AssetName (..),
  MultiAsset (..),
  PolicyID (..),
  flattenMultiAsset,
  policies,
 )
import Data.Group (Group (invert))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Lens.Micro ((&), (.~), (^.))
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Mary.Arbitrary ()

spec :: Spec
spec = describe "Forging" $ do
  prop "separates minted and burned quantities" $ \(multiAsset :: MultiAsset) ->
    let forging = Forging multiAsset
        minted = unMintedAssets (mintedAssets forging)
        burned = unBurnedAssets (burnedAssets forging)
     in do
          all ((> 0) . third) (flattenMultiAsset minted) `shouldBe` True
          all ((> 0) . third) (flattenMultiAsset burned) `shouldBe` True
          minted <> invert burned `shouldBe` multiAsset

  prop "drops zero quantities from both projections" $ \(policy :: PolicyID) (assetName :: AssetName) ->
    let forging = Forging (MultiAsset (Map.singleton policy (Map.singleton assetName 0)))
     in do
          flattenMultiAsset (unMintedAssets (mintedAssets forging)) `shouldBe` []
          flattenMultiAsset (unBurnedAssets (burnedAssets forging)) `shouldBe` []

  prop "transaction-body views preserve the existing mint lens" $ \(multiAsset :: MultiAsset) ->
    let forging = Forging multiAsset
        txBody = mkBasicTxBody @MaryEra @TopTx & forgingTxBodyL .~ forging
     in do
          txBody ^. mintTxBodyL `shouldBe` multiAsset
          unForging (txBody ^. forgingTxBodyL) `shouldBe` multiAsset
          txBody ^. mintedAssetsTxBodyF `shouldBe` mintedAssets forging
          txBody ^. burnedAssetsTxBodyF `shouldBe` burnedAssets forging
          txBody ^. mintPoliciesTxBodyF `shouldBe` policies multiAsset
          txBody ^. mintedTxBodyF `shouldBe` policies multiAsset

  describe "Raw mint representation" $
    forM_ rawMintFixtures $ \(scenarioName, expectedMap, expectedPolicies) -> describe scenarioName $ do
      let
        rawMint = MultiAsset expectedMap
        legacyBody = mkBasicTxBody @MaryEra @TopTx & mintTxBodyL .~ rawMint
        forgingBody = mkBasicTxBody @MaryEra @TopTx & forgingTxBodyL .~ Forging rawMint

      it "preserves raw zero quantities and empty policy maps" $ do
        -- Compare the nested maps directly: MultiAsset equality disregards
        -- zero/empty distinctions that the representation must retain.
        rawMap (forgingBody ^. mintTxBodyL) `shouldBe` expectedMap
        rawMap (unForging (forgingBody ^. forgingTxBodyL)) `shouldBe` expectedMap
        rawMap (unForging (legacyBody ^. forgingTxBodyL)) `shouldBe` expectedMap

      it "matches the legacy mint lens's transaction-body bytes" $
        serialize' (eraProtVerLow @MaryEra) forgingBody
          `shouldBe` serialize' (eraProtVerLow @MaryEra) legacyBody

      it "retains policies with only zero quantities or no assets" $ do
        forgingBody ^. mintPoliciesTxBodyF `shouldBe` expectedPolicies
        legacyBody ^. mintedTxBodyF `shouldBe` expectedPolicies

rawMintFixtures :: [(String, Map.Map PolicyID (Map.Map AssetName Integer), Set.Set PolicyID)]
rawMintFixtures =
  [ ("zero quantity", zeroEntry, Set.singleton zeroPolicy)
  , ("empty policy map", emptyEntry, Set.singleton emptyPolicy)
  ,
    ( "zero quantity and empty policy map"
    , zeroEntry <> emptyEntry
    , Set.fromList [zeroPolicy, emptyPolicy]
    )
  ]
  where
    zeroPolicy = PolicyID (ScriptHash "00000000000000000000000000000000000000000000000000000000")
    emptyPolicy = PolicyID (ScriptHash "01010101010101010101010101010101010101010101010101010101")
    zeroEntry = Map.singleton zeroPolicy (Map.singleton (AssetName "zero") 0)
    emptyEntry = Map.singleton emptyPolicy Map.empty

rawMap :: MultiAsset -> Map.Map PolicyID (Map.Map AssetName Integer)
rawMap (MultiAsset entries) = entries

third :: (a, b, c) -> c
third (_, _, c) = c

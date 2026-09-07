{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Api.Tx.Out.TotalsSpec (
  currentOutputTotalsSpec,
  mixedAssetTotalsSpec,
  collateralReturnTotalsSpec,
) where

import Cardano.Ledger.Api.Tx.Out (totalCoinTxOutF, totalValueTxOutF)
import Cardano.Ledger.Babbage.Collateral (collAdaBalance)
import Cardano.Ledger.Babbage.Core hiding (totalCoinTxOutF, totalValueTxOutF)
import Cardano.Ledger.Coin
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Mary.AssetName (AssetName (..))
import Cardano.Ledger.Mary.MultiAsset (MultiAsset (..))
import Cardano.Ledger.Mary.PolicyID (PolicyID (..))
import Cardano.Ledger.Mary.Value (MaryValue (..))
import Cardano.Ledger.State (UTxO (..), sumAllCoin, sumAllValue, sumCoinUTxO, sumUTxO)
import Cardano.Ledger.TxIn (TxId (..), mkTxInPartial)
import qualified Cardano.Ledger.Val as Val
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Lens.Micro
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Utils (mkDummySafeHash)
import Test.Cardano.Ledger.Shelley.Examples (exampleByronAddress)

-- These compatibility checks describe the seven current output representations.
-- A future split representation must test its override separately, rather than
-- retaining the assumption that mutable holdings and accounting totals agree.
currentOutputTotalsSpec ::
  forall era.
  (EraTxOut era, Arbitrary (TxOut era)) =>
  Spec
currentOutputTotalsSpec = describe "Current output accounting totals" $ do
  prop "retain the existing holdings and compact ADA projections" $ \(txOut :: TxOut era) -> do
    txOut ^. totalValueEitherTxOutF `shouldBe` txOut ^. valueEitherTxOutL
    txOut ^. totalValueTxOutF `shouldBe` txOut ^. valueTxOutL
    txOut ^. totalCoinTxOutF `shouldBe` txOut ^. coinTxOutL
    txOut ^. totalCompactCoinTxOutF `shouldBe` txOut ^. compactCoinTxOutL
    txOut ^. totalCoinTxOutF `shouldBe` Val.coin (txOut ^. totalValueTxOutF)
    txOut ^. isAdaOnlyTxOutF `shouldBe` Val.isAdaOnly (txOut ^. totalValueTxOutF)

  prop "follow both plain and compact holdings updates" $
    \(txOut :: TxOut era) (replacement :: TxOut era) -> do
      let value = replacement ^. valueTxOutL
          compactValue = replacement ^. compactValueTxOutL
      forM_ [Left value, Right compactValue] $ \representation -> do
        let updated = txOut & valueEitherTxOutL .~ representation
        updated ^. totalValueTxOutF `shouldBe` value
        updated ^. totalCoinTxOutF `shouldBe` Val.coin value
        fromCompact (updated ^. totalCompactCoinTxOutF) `shouldBe` Val.coin value

  prop "preserve list and UTxO sums with bounded total ADA" $
    forAll (boundedOutputs @era) $ \outputs -> do
      -- Independent oracle: the pre-refactor holdings projections. At most
      -- twelve outputs with one million lovelace each cannot overflow Word64.
      let expectedValue = foldMap (^. valueTxOutL) outputs
          expectedCoin = foldMap (^. coinTxOutL) outputs
          utxo = indexedUTxO outputs
      sumAllValue outputs `shouldBe` expectedValue
      sumAllCoin outputs `shouldBe` expectedCoin
      sumUTxO utxo `shouldBe` expectedValue
      sumCoinUTxO utxo `shouldBe` expectedCoin

  it "sums empty output collections to zero" $ do
    let outputs = [] :: [TxOut era]
    sumAllValue outputs `shouldBe` mempty
    sumAllCoin outputs `shouldBe` Coin 0
    sumUTxO (indexedUTxO outputs) `shouldBe` mempty
    sumCoinUTxO (indexedUTxO outputs) `shouldBe` Coin 0

indexedUTxO :: [TxOut era] -> UTxO era
indexedUTxO outputs =
  UTxO $ Map.fromList $ zip (mkTxInPartial (TxId (mkDummySafeHash 0)) <$> [0 ..]) outputs

mixedAssetTotalsSpec ::
  forall era.
  (EraTxOut era, Value era ~ MaryValue) =>
  Spec
mixedAssetTotalsSpec =
  it "counts ADA and native quantities independently across mixed outputs" $ do
    let outputs :: [TxOut era]
        outputs =
          mkBasicTxOut exampleByronAddress
            <$> [ MaryValue (Coin 2) mempty
                , MaryValue (Coin 5) (nativeFixture 3)
                , MaryValue (Coin 7) (nativeFixture 4)
                ]
        expected = MaryValue (Coin 14) (nativeFixture 7)
    sumAllValue outputs `shouldBe` expected
    sumAllCoin outputs `shouldBe` Coin 14
    sumUTxO (indexedUTxO outputs) `shouldBe` expected
    sumCoinUTxO (indexedUTxO outputs) `shouldBe` Coin 14

collateralReturnTotalsSpec :: forall era. BabbageEraTxBody era => Spec
collateralReturnTotalsSpec =
  it "subtracts a nonzero collateral return once from all collateral inputs" $ do
    let outputs :: [TxOut era]
        outputs = mkCoinTxOut exampleByronAddress <$> [Coin 11, Coin 7]
        collateral = unUTxO (indexedUTxO outputs)
        txBody :: TxBody TopTx era
        txBody = mkBasicTxBody
        withReturn =
          txBody
            & collateralReturnTxBodyL .~ SJust (mkCoinTxOut exampleByronAddress (Coin 5))
    collAdaBalance txBody collateral `shouldBe` DeltaCoin 18
    collAdaBalance withReturn collateral `shouldBe` DeltaCoin 13

boundedOutputs :: (EraTxOut era, Arbitrary (TxOut era)) => Gen [TxOut era]
boundedOutputs = do
  count <- choose (0, 12)
  vectorOf count $ do
    txOut <- arbitrary
    coins <- Coin <$> choose (0, 1000000)
    pure $ txOut & coinTxOutL .~ coins

nativeFixture :: Integer -> MultiAsset
nativeFixture quantity =
  MultiAsset $
    Map.singleton
      (PolicyID (ScriptHash "00000000000000000000000000000000000000000000000000000000"))
      (Map.singleton (AssetName "token") quantity)

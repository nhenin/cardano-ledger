{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Api.Tx.Out.OutputPot.Fixture (
  currentOutput,
  HoldingsReplacement (..),
  outputWithPlainHoldingsReplacement,
  outputWithCompactHoldingsReplacement,
  OutputCollection (..),
  boundedOutputCollection,
  emptyOutputCollection,
  outputsWithSharedNativeAssets,
  CollateralBalanceCase (..),
  collateralWithoutReturn,
  collateralWithNonzeroReturn,
) where

import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.Coin
import Cardano.Ledger.Mary.AssetName (AssetName (..))
import Cardano.Ledger.Mary.MultiAsset (MultiAsset (..))
import Cardano.Ledger.Mary.PolicyID (PolicyID (..))
import Cardano.Ledger.Mary.Value (MaryValue (..))
import Cardano.Ledger.State (UTxO (..))
import Cardano.Ledger.TxIn (TxId (..), TxIn, mkTxInPartial)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Lens.Micro
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Utils (mkDummySafeHash)
import Test.Cardano.Ledger.Shelley.Examples (exampleByronAddress)

currentOutput :: Arbitrary (TxOut era) => Gen (TxOut era)
currentOutput = arbitrary

data HoldingsReplacement era = HoldingsReplacement
  { replacedOutput :: TxOut era
  , replacementValue :: Value era
  }

deriving instance EraTxOut era => Show (HoldingsReplacement era)

outputWithPlainHoldingsReplacement ::
  forall era. (EraTxOut era, Arbitrary (TxOut era)) => Gen (HoldingsReplacement era)
outputWithPlainHoldingsReplacement = do
  original <- currentOutput @era
  replacement <- currentOutput @era
  let value = replacement ^. valueTxOutL
  pure $ HoldingsReplacement (original & valueEitherTxOutL .~ Left value) value

outputWithCompactHoldingsReplacement ::
  forall era. (EraTxOut era, Arbitrary (TxOut era)) => Gen (HoldingsReplacement era)
outputWithCompactHoldingsReplacement = do
  original <- currentOutput @era
  replacement <- currentOutput @era
  let value = replacement ^. valueTxOutL
      compactValue = replacement ^. compactValueTxOutL
  pure $ HoldingsReplacement (original & valueEitherTxOutL .~ Right compactValue) value

data OutputCollection era = OutputCollection
  { collectionOutputs :: [TxOut era]
  , collectionUTxO :: UTxO era
  , combinedValue :: Value era
  , combinedCoins :: Coin
  }

deriving instance EraTxOut era => Show (OutputCollection era)

boundedOutputCollection ::
  (EraTxOut era, Arbitrary (TxOut era)) => Gen (OutputCollection era)
boundedOutputCollection = do
  outputs <- outputsWithBoundedCoins
  -- Independent oracle: the existing editable holdings projections.
  pure $
    OutputCollection
      outputs
      (utxoFromDistinctOutputReferences outputs)
      (foldMap (^. valueTxOutL) outputs)
      (foldMap (^. coinTxOutL) outputs)

outputsWithBoundedCoins :: (EraTxOut era, Arbitrary (TxOut era)) => Gen [TxOut era]
outputsWithBoundedCoins = do
  -- At most twelve outputs with one million lovelace each cannot overflow Word64.
  count <- choose (0, 12)
  vectorOf count outputWithBoundedCoins

outputWithBoundedCoins :: (EraTxOut era, Arbitrary (TxOut era)) => Gen (TxOut era)
outputWithBoundedCoins = do
  txOut <- currentOutput
  coins <- Coin <$> choose (0, 1000000)
  pure $ txOut & coinTxOutL .~ coins

emptyOutputCollection :: EraTxOut era => OutputCollection era
emptyOutputCollection = OutputCollection [] (UTxO Map.empty) mempty (Coin 0)

outputsWithSharedNativeAssets ::
  forall era. (EraTxOut era, Value era ~ MaryValue) => OutputCollection era
outputsWithSharedNativeAssets =
  let outputs :: [TxOut era]
      outputs =
        mkBasicTxOut exampleByronAddress
          <$> [ MaryValue (Coin 2) mempty
              , MaryValue (Coin 5) (sharedNativeAssets 3)
              , MaryValue (Coin 7) (sharedNativeAssets 4)
              ]
   in OutputCollection
        outputs
        (utxoFromDistinctOutputReferences outputs)
        (MaryValue (Coin 14) (sharedNativeAssets 7))
        (Coin 14)

utxoFromDistinctOutputReferences :: [TxOut era] -> UTxO era
utxoFromDistinctOutputReferences outputs =
  UTxO $ Map.fromList $ zip (mkTxInPartial (TxId (mkDummySafeHash 0)) <$> [0 ..]) outputs

sharedNativeAssets :: Integer -> MultiAsset
sharedNativeAssets quantity =
  MultiAsset $
    Map.singleton
      (PolicyID (ScriptHash "00000000000000000000000000000000000000000000000000000000"))
      (Map.singleton (AssetName "token") quantity)

data CollateralBalanceCase era = CollateralBalanceCase
  { collateralBody :: TxBody TopTx era
  , collateralInputs :: Map.Map TxIn (TxOut era)
  , remainingCollateralCoins :: DeltaCoin
  }

collateralWithoutReturn :: BabbageEraTxBody era => CollateralBalanceCase era
collateralWithoutReturn =
  CollateralBalanceCase mkBasicTxBody collateralInputsWithEighteenCoins (DeltaCoin 18)

collateralWithNonzeroReturn :: BabbageEraTxBody era => CollateralBalanceCase era
collateralWithNonzeroReturn =
  CollateralBalanceCase
    (mkBasicTxBody & collateralReturnTxBodyL .~ SJust (mkCoinTxOut exampleByronAddress (Coin 5)))
    collateralInputsWithEighteenCoins
    (DeltaCoin 13)

collateralInputsWithEighteenCoins :: EraTxOut era => Map.Map TxIn (TxOut era)
collateralInputsWithEighteenCoins =
  unUTxO $
    utxoFromDistinctOutputReferences $
      mkCoinTxOut exampleByronAddress <$> [Coin 11, Coin 7]

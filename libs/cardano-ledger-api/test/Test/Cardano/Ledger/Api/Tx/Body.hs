{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Api.Tx.Body (spec) where

import qualified Cardano.Ledger.Allegra.TxBody as Allegra (AllegraTxBodyRaw (atbrMint))
import qualified Cardano.Ledger.Alonzo.TxBody as Alonzo (AlonzoTxBodyRaw (atbrMint))
import Cardano.Ledger.Api.Era
import Cardano.Ledger.Api.Tx.Body
import qualified Cardano.Ledger.Babbage.TxBody as Babbage (BabbageTxBodyRaw (btbrMint))
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary (serialize')
import Cardano.Ledger.Coin
import Cardano.Ledger.Compactible
import qualified Cardano.Ledger.Conway.TxBody as Conway (ConwayTxBodyRaw (ctbrMint))
import qualified Cardano.Ledger.Dijkstra.TxBody as Dijkstra (
  DijkstraTxBodyRaw (dstbrMint, dtbrMint),
 )
import Cardano.Ledger.Mary.AssetName (AssetName (..))
import Cardano.Ledger.Mary.MultiAsset (MultiAsset (..))
import Cardano.Ledger.Mary.PolicyID (PolicyID (..))
import qualified Cardano.Ledger.Mary.Value as Mary (MaryValue (..))
import Cardano.Ledger.MemoBytes (getMemoRawType, mkMemoizedEra)
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.State
import Cardano.Ledger.Val
import Data.Foldable
import qualified Data.Map.Strict as Map
import Data.MapExtras as Map (extract)
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import Lens.Micro
import Test.Cardano.Ledger.Api.Arbitrary ()
import Test.Cardano.Ledger.Common

totalTxDeposits ::
  (EraTxBody era, EraCertState era) =>
  PParams era ->
  CertState era ->
  TxBody l era ->
  Coin
totalTxDeposits pp dpstate txb =
  numKeys <×> pp ^. ppKeyDepositL <+> snd (foldl' accum (regpools, Coin 0) certs)
  where
    certs = toList (txb ^. certsTxBodyL)
    numKeys = length $ filter isRegStakeTxCert certs
    regpools = Map.keysSet $ psStakePools (dpstate ^. certPStateL)
    accum (!pools, !ans) (RegPoolTxCert stakePoolParams) =
      -- We don't pay a deposit on a pool that is already registered
      if Set.member (sppId stakePoolParams) pools
        then (pools, ans)
        else (Set.insert (sppId stakePoolParams) pools, ans <+> pp ^. ppPoolDepositL)
    accum ans _ = ans

keyTxRefunds ::
  (EraTxBody era, ShelleyEraTxCert era, EraCertState era) =>
  PParams era ->
  CertState era ->
  TxBody l era ->
  Coin
keyTxRefunds pp dpstate tx =
  case foldl' accum (initAccountsMap, Set.empty, mempty) certs of
    (_, _, res) -> res
  where
    certs = tx ^. certsTxBodyL
    initAccountsMap = dpstate ^. certDStateL . accountsL . accountsMapL
    keyDeposit = pp ^. ppKeyDepositL
    accum acc@(!accountsMap, !newlyRegistered, !ans) = \case
      RegTxCert cred
        | Map.member cred accountsMap || Set.member cred newlyRegistered -> acc
        | otherwise -> (accountsMap, Set.insert cred newlyRegistered, ans)
      UnRegTxCert cred ->
        case Map.extract cred accountsMap of
          (Just accountState, newAccountsMap) ->
            (newAccountsMap, newlyRegistered, ans <> fromCompact (accountState ^. depositAccountStateL))
          (Nothing, newAccountsMap)
            | Set.member cred newlyRegistered ->
                (newAccountsMap, Set.delete cred newlyRegistered, ans <> keyDeposit)
            | otherwise -> acc
      _ -> acc

-- | This is the old implementation of `evalBalanceTxBody`. We keep it around to ensure that
-- the produced result hasn't changed
evaluateTransactionBalance ::
  ( MaryEraTxBody era
  , ShelleyEraTxCert era
  , EraCertState era
  , Value era ~ Mary.MaryValue
  ) =>
  PParams era ->
  CertState era ->
  UTxO era ->
  TxBody TopTx era ->
  Value era
evaluateTransactionBalance pp dpstate utxo txBody =
  -- Keep this signed oracle independent of the minted/burned projections used
  -- by the implementation under test.
  evaluateTransactionBalanceShelley pp dpstate utxo txBody
    <> Mary.MaryValue mempty (unForging (txBody ^. forgingTxBodyL))

evaluateTransactionBalanceShelley ::
  (EraTxBody era, ShelleyEraTxCert era, EraCertState era) =>
  PParams era ->
  CertState era ->
  UTxO era ->
  TxBody TopTx era ->
  Value era
evaluateTransactionBalanceShelley pp dpstate utxo txBody = consumed <-> produced
  where
    produced =
      sumUTxO (txouts txBody)
        <+> inject (txBody ^. feeTxBodyL <+> totalTxDeposits pp dpstate txBody)
    consumed =
      sumUTxO (txInsFilter utxo (txBody ^. inputsTxBodyL))
        <> inject (refunds <> withdrawals)
    refunds = keyTxRefunds pp dpstate txBody
    withdrawals = fold . unWithdrawals $ txBody ^. withdrawalsTxBodyL

-- | Randomly lookup pool params and staking credentials to add them as unregistration and
-- undelegation certificates respectively.
genTxBodyFrom ::
  (EraTxBody era, ShelleyEraTxCert era, Arbitrary (TxBody l era), EraCertState era) =>
  CertState era ->
  UTxO era ->
  Gen (TxBody l era)
genTxBodyFrom certState (UTxO u) = do
  txBody <- arbitrary
  inputs <- sublistOf (Map.keys u)
  unDelegCreds <- sublistOf (Map.keys (certState ^. certDStateL . accountsL . accountsMapL))
  deRegKeys <- sublistOf (Map.keys (certState ^. certPStateL . psStakePoolsL))
  network <- arbitrary
  let deReg =
        Map.elems $
          Map.mapWithKey (stakePoolStateToStakePoolParams network) $
            Map.restrictKeys (certState ^. certPStateL . psStakePoolsL) (Set.fromList deRegKeys)
  certs <-
    shuffle $
      toList (txBody ^. certsTxBodyL)
        <> (UnRegTxCert <$> unDelegCreds)
        <> (RegPoolTxCert <$> deReg)
  pure
    ( txBody
        & inputsTxBodyL .~ Set.fromList inputs
        & certsTxBodyL .~ SSeq.fromList certs
    )

propEvalBalanceTxBody ::
  ( EraUTxO era
  , MaryEraTxBody era
  , ShelleyEraTxCert era
  , Arbitrary (TxBody TopTx era)
  , EraCertState era
  , Value era ~ Mary.MaryValue
  ) =>
  PParams era ->
  CertState era ->
  UTxO era ->
  Property
propEvalBalanceTxBody pp certState utxo = do
  property $
    forAll (genTxBodyFrom @_ @TopTx certState utxo) $ \txBody ->
      evalBalanceTxBody pp lookupKeyDeposit isRegPoolId utxo txBody
        `shouldBe` evaluateTransactionBalance pp certState utxo txBody
  where
    lookupKeyDeposit = lookupDepositDState (certState ^. certDStateL)
    isRegPoolId = (`Map.member` psStakePools (certState ^. certPStateL))

propEvalBalanceShelleyTxBody ::
  (EraUTxO era, ShelleyEraTxCert era, Arbitrary (TxBody TopTx era), EraCertState era) =>
  PParams era ->
  CertState era ->
  UTxO era ->
  Property
propEvalBalanceShelleyTxBody pp certState utxo =
  property $
    forAll (genTxBodyFrom @_ @TopTx certState utxo) $ \txBody ->
      evalBalanceTxBody pp lookupKeyDeposit isRegPoolId utxo txBody
        `shouldBe` evaluateTransactionBalanceShelley pp certState utxo txBody
  where
    lookupKeyDeposit = lookupDepositDState (certState ^. certDStateL)
    isRegPoolId = (`Map.member` psStakePools (certState ^. certPStateL))

-- | The balance oracle applies only through Babbage: Conway changes this
-- calculation. Forging access is tested separately for every supported era.
spec :: Spec
spec =
  describe "TxBody" $ do
    describe "ShelleyEra" $ do
      prop "evalBalanceTxBody" $ propEvalBalanceShelleyTxBody @ShelleyEra
      it "forgingTxBodyG reports an unsupported field" $
        mkBasicTxBody @ShelleyEra @TopTx ^. forgingTxBodyG `shouldBe` Nothing
    describe "AllegraEra" $ do
      prop "evalBalanceTxBody" $ propEvalBalanceShelleyTxBody @AllegraEra
      it "forgingTxBodyG reports an unsupported field" $
        mkBasicTxBody @AllegraEra @TopTx ^. forgingTxBodyG `shouldBe` Nothing
    describe "MaryEra" $ do
      prop "evalBalanceTxBody" $ propEvalBalanceTxBody @MaryEra
      forgingApiSpec @MaryEra @TopTx
        (Allegra.atbrMint . getMemoRawType)
        (\mint txBody -> mkMemoizedEra @MaryEra $ (getMemoRawType txBody) {Allegra.atbrMint = mint})
    describe "AlonzoEra" $ do
      prop "evalBalanceTxBody" $ propEvalBalanceTxBody @AlonzoEra
      forgingApiSpec @AlonzoEra @TopTx
        (Alonzo.atbrMint . getMemoRawType)
        (\mint txBody -> mkMemoizedEra @AlonzoEra $ (getMemoRawType txBody) {Alonzo.atbrMint = mint})
    describe "BabbageEra" $ do
      prop "evalBalanceTxBody" $ propEvalBalanceTxBody @BabbageEra
      forgingApiSpec @BabbageEra @TopTx
        (Babbage.btbrMint . getMemoRawType)
        (\mint txBody -> mkMemoizedEra @BabbageEra $ (getMemoRawType txBody) {Babbage.btbrMint = mint})
    describe "ConwayEra" $
      forgingApiSpec @ConwayEra @TopTx
        (Conway.ctbrMint . getMemoRawType)
        (\mint txBody -> mkMemoizedEra @ConwayEra $ (getMemoRawType txBody) {Conway.ctbrMint = mint})
    describe "DijkstraEra" $ do
      forgingApiSpec @DijkstraEra @TopTx
        (Dijkstra.dtbrMint . getMemoRawType)
        (\mint txBody -> mkMemoizedEra @DijkstraEra $ (getMemoRawType txBody) {Dijkstra.dtbrMint = mint})
      describe "SubTx" $
        forgingApiSpec @DijkstraEra @SubTx
          (Dijkstra.dstbrMint . getMemoRawType)
          (\mint txBody -> mkMemoizedEra @DijkstraEra $ (getMemoRawType txBody) {Dijkstra.dstbrMint = mint})

forgingApiSpec ::
  forall era l.
  (MaryEraTxBody era, AnyEraTxBody era, Typeable l) =>
  (TxBody l era -> MultiAsset) ->
  (MultiAsset -> TxBody l era -> TxBody l era) ->
  Spec
forgingApiSpec getRawMint setRawMint = do
  it "forgingTxBodyG distinguishes an empty declaration from an unsupported field" $
    mkBasicTxBody @era @l ^. forgingTxBodyG `shouldBe` Just mempty
  it "forging getters preserve the raw mint map" $
    forM_ rawForgingMaps $ \expectedMap -> do
      let rawBody = setRawMint (MultiAsset expectedMap) basicBody
      -- MultiAsset equality disregards explicit zero quantities and empty
      -- policies, so compare the actual nested maps instead.
      rawMap (unForging (rawBody ^. forgingTxBodyL)) `shouldBe` expectedMap
      fmap (rawMap . unForging) (rawBody ^. forgingTxBodyG) `shouldBe` Just expectedMap
  it "the forging setter preserves the raw map and the rest of the body's bytes" $
    forM_ rawForgingMaps $ \expectedMap -> do
      let rawMint = MultiAsset expectedMap
          rawBody = setRawMint rawMint basicBody
          forgingBody = initialBody & forgingTxBodyL .~ Forging rawMint
      rawMap (getRawMint forgingBody) `shouldBe` expectedMap
      serialize' version forgingBody `shouldBe` serialize' version rawBody
      -- Remove only the raw mint field independently of the API lens. This
      -- checks every other encoded field, including the non-default interval.
      serialize' version (setRawMint mempty forgingBody)
        `shouldBe` serialize' version basicBody
  where
    basicBody =
      mkBasicTxBody @era @l
        & vldtTxBodyL .~ ValidityInterval (SJust 3) (SJust 42)
    -- Begin with a non-empty declaration, so the setter must replace it and
    -- must also be able to clear it when the requested map is empty.
    initialBody = setRawMint (MultiAsset $ Map.unionsWith Map.union rawForgingMaps) basicBody
    version = eraProtVerLow @era

-- These are representation tests, not valid-transaction examples. Direct raw
-- construction deliberately retains zero quantities and empty policy maps.
rawForgingMaps :: [Map.Map PolicyID (Map.Map AssetName Integer)]
rawForgingMaps =
  [ Map.empty
  , minted
  , burned
  , zeroEntry
  , emptyPolicyMap
  , Map.unionsWith Map.union [minted, burned, zeroEntry, emptyPolicyMap]
  ]
  where
    firstPolicy = PolicyID (ScriptHash "00000000000000000000000000000000000000000000000000000000")
    secondPolicy = PolicyID (ScriptHash "01010101010101010101010101010101010101010101010101010101")
    minted = Map.singleton firstPolicy (Map.singleton (AssetName "minted") 7)
    burned = Map.singleton firstPolicy (Map.singleton (AssetName "burned") (-3))
    zeroEntry = Map.singleton firstPolicy (Map.singleton (AssetName "zero") 0)
    emptyPolicyMap = Map.singleton secondPolicy Map.empty

rawMap :: MultiAsset -> Map.Map PolicyID (Map.Map AssetName Integer)
rawMap (MultiAsset entries) = entries

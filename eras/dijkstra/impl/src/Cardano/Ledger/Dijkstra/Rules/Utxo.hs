{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
#if __GLASGOW_HASKELL__ >= 910
-- See https://gitlab.haskell.org/ghc/ghc/-/issues/27342
{-# OPTIONS_GHC -fno-spec-eval #-}
#endif

module Cardano.Ledger.Dijkstra.Rules.Utxo (
  UTXO,
  DijkstraUtxoEnv (..),
  DijkstraUtxoPredFailure (..),
  conwayToDijkstraUtxoPredFailure,
  updateDijkstraUTxOAndInstantStake,
) where

import qualified Cardano.Ledger.Allegra.Rules as Allegra
import qualified Cardano.Ledger.Alonzo.Rules as Alonzo
import Cardano.Ledger.Alonzo.TxWits (unRedeemersL)
import Cardano.Ledger.Babbage.Collateral (collOuts)
import qualified Cardano.Ledger.Babbage.Rules as Babbage
import Cardano.Ledger.BaseTypes (
  Mismatch (..),
  Network,
  Relation (..),
  ShelleyBase,
  SlotNo,
  StrictMaybe (..),
  epochInfo,
  networkId,
  strictMaybe,
  systemStart,
 )
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
  sizedValue,
 )
import Cardano.Ledger.Binary.Coders (
  Decode (..),
  Encode (..),
  decode,
  encode,
  (!>),
  (<!),
 )
import Cardano.Ledger.Coin (Coin (..), DeltaCoin (..), toDeltaCoin)
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Conway.Core
import qualified Cardano.Ledger.Conway.Rules as Conway
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Credential (StakeReference (..))
import Cardano.Ledger.Dijkstra.Era (DijkstraEra, UTXO)
import Cardano.Ledger.Dijkstra.Rules.CapacityDeposit (validateOutputCapacityDeposit)
import Cardano.Ledger.Dijkstra.Rules.Utxos ()
import Cardano.Ledger.Dijkstra.TxBody (DijkstraEraTxBody (..))
import Cardano.Ledger.Dijkstra.TxOut (DijkstraEraTxOut (..), DijkstraTxOut, totalAdaTxOut)
import Cardano.Ledger.Dijkstra.TxOut.CapacityDeposit (CapacityDeposit (..))
import Cardano.Ledger.Dijkstra.UTxO (dijkstraConsumed)
import Cardano.Ledger.Dijkstra.UTxO.Translation (fundCapacityDeposits)
import Cardano.Ledger.Plutus (OrdExUnits)
import Cardano.Ledger.Rules.ValidationMode (Test, failOnJustStatic, runTest, runTestOnSignal)
import Cardano.Ledger.Shelley.LedgerState (UTxOState (..), utxosFeesL)
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Cardano.Ledger.Shelley.UTxO (produced)
import Cardano.Ledger.TxIn (TxIn)
import Cardano.Ledger.Val (Val, (<->))
import Control.DeepSeq (NFData)
import Control.Monad (when)
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition.Extended (
  Embed (..),
  Rule,
  RuleType (Transition),
  STS (..),
  TRC (..),
  TransitionRule,
  failureOnNonEmptyMap,
  judgmentContext,
  liftSTS,
  tellEvent,
  trans,
  validate,
 )
import Data.Bifunctor (first)
import Data.Foldable (foldMap', sequenceA_)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.NonEmpty (NonEmptyMap)
import qualified Data.Map.Strict as Map
import Data.MapExtras (extractKeys)
import qualified Data.OMap.Strict as OMap
import Data.Set.NonEmpty (NonEmptySet)
import Data.Word (Word16, Word32)
import GHC.Generics (Generic)
import Lens.Micro ((&), (<>~), (^.))
import Validation (failureIf, failureUnless)

data DijkstraUtxoEnv era = DijkstraUtxoEnv
  { dueSlot :: SlotNo
  , duePParams :: PParams era
  , dueCertState :: CertState era
  , dueOriginalUtxo :: UTxO era
  }

-- | Predicate failure for the Dijkstra Era
data DijkstraUtxoPredFailure era
  = -- | Subtransition Failures
    UtxosFailure (PredicateFailure (EraRule "UTXOS" era))
  | -- | The bad transaction inputs
    BadInputsUTxO (NonEmptySet TxIn)
  | OutsideValidityIntervalUTxO
      -- | transaction's validity interval
      ValidityInterval
      -- | current slot
      SlotNo
  | MaxTxSizeUTxO (Mismatch RelLTEQ Word32)
  | InputSetEmptyUTxO
  | FeeTooSmallUTxO
      (Mismatch RelGTEQ Coin)
  | ValueNotConservedUTxO
      (Mismatch RelEQ (Value era)) -- Serialise consumed first, then produced
  | -- | the set of addresses with incorrect network IDs
    WrongNetwork
      -- | the expected network id
      Network
      -- | the set of addresses with incorrect network IDs
      (NonEmptySet Addr)
  | -- | list of supplied bad transaction outputs
    OutputBootAddrAttrsTooBig (NonEmpty (TxOut era))
  | -- | list of supplied bad transaction output triples (actualSize,PParameterMaxValue,TxOut)
    OutputTooBigUTxO (NonEmpty (Int, Int, TxOut era))
  | InsufficientCollateral
      -- | balance computed
      DeltaCoin
      -- | the required collateral for the given fee
      Coin
  | -- | The UTxO entries which have the wrong kind of script
    ScriptsNotPaidUTxO (NonEmptyMap TxIn (TxOut era))
  | ExUnitsTooBigUTxO
      (Mismatch RelLTEQ OrdExUnits)
  | -- | The inputs marked for use as fees contain non-ADA tokens
    CollateralContainsNonADA (Value era)
  | -- | Wrong Network ID in body
    WrongNetworkInTxBody
      (Mismatch RelEQ Network)
  | -- | slot number outside consensus forecast range
    OutsideForecast SlotNo
  | -- | There are too many collateral inputs
    TooManyCollateralInputs
      (Mismatch RelLTEQ Word16)
  | NoCollateralInputs
  | -- | The collateral is not equivalent to the total collateral asserted by the transaction
    IncorrectTotalCollateralField
      -- | collateral provided
      DeltaCoin
      -- | collateral amount declared in transaction body
      Coin
  | -- | outputs whose capacity deposit is not exactly the required
    -- @M(o)@, together with the supplied\/expected mismatch. The deposit is
    -- a tariff, not a floor: over-funding is as invalid as under-funding.
    IncorrectCapacityDepositUTxO (NonEmpty (TxOut era, Mismatch RelEQ CapacityDeposit))
  | -- | Implicit (merged-form) outputs whose total ada cannot cover the
    -- tariff the entry-time restructuring will charge
    ImplicitOutputTooSmallUTxO (NonEmpty (TxOut era, Mismatch RelGTEQ Coin))
  | -- | TxIns that appear in both inputs and reference inputs
    BabbageNonDisjointRefInputs (NonEmpty TxIn)
  | PtrPresentInCollateralReturn (TxOut era)
  | -- | Total withdrawals per account that exceed the original account balance
    WithdrawalsExceedAccountBalance (NonEmptyMap AccountAddress (Mismatch RelLTEQ Coin))
  deriving (Generic)

type instance EraRuleFailure "UTXO" DijkstraEra = DijkstraUtxoPredFailure DijkstraEra

type instance EraRuleEvent "UTXO" DijkstraEra = Alonzo.AlonzoUtxoEvent DijkstraEra

instance InjectRuleFailure "UTXO" DijkstraUtxoPredFailure DijkstraEra

instance InjectRuleFailure "UTXO" Conway.ConwayUtxoPredFailure DijkstraEra where
  injectFailure = conwayToDijkstraUtxoPredFailure

instance InjectRuleFailure "UTXO" Babbage.BabbageUtxoPredFailure DijkstraEra where
  injectFailure = conwayToDijkstraUtxoPredFailure . Conway.babbageToConwayUtxoPredFailure

instance InjectRuleFailure "UTXO" Alonzo.AlonzoUtxoPredFailure DijkstraEra where
  injectFailure = conwayToDijkstraUtxoPredFailure . Conway.alonzoToConwayUtxoPredFailure

instance InjectRuleFailure "UTXO" Shelley.ShelleyUtxoPredFailure DijkstraEra where
  injectFailure =
    conwayToDijkstraUtxoPredFailure
      . Conway.allegraToConwayUtxoPredFailure
      . Allegra.shelleyToAllegraUtxoPredFailure

instance InjectRuleFailure "UTXO" Allegra.AllegraUtxoPredFailure DijkstraEra where
  injectFailure = conwayToDijkstraUtxoPredFailure . Conway.allegraToConwayUtxoPredFailure

instance InjectRuleFailure "UTXO" Conway.ConwayUtxosPredFailure DijkstraEra where
  injectFailure = UtxosFailure

instance InjectRuleFailure "UTXO" Alonzo.AlonzoUtxosPredFailure DijkstraEra where
  injectFailure =
    conwayToDijkstraUtxoPredFailure
      . Conway.alonzoToConwayUtxoPredFailure
      . Alonzo.UtxosFailure
      . injectFailure

deriving instance
  ( Era era
  , Show (Value era)
  , Show (PredicateFailure (EraRule "UTXOS" era))
  , Show (TxOut era)
  , Show (Script era)
  , Show TxIn
  ) =>
  Show (DijkstraUtxoPredFailure era)

deriving instance
  ( Era era
  , Eq (Value era)
  , Eq (PredicateFailure (EraRule "UTXOS" era))
  , Eq (TxOut era)
  , Eq (Script era)
  , Eq TxIn
  ) =>
  Eq (DijkstraUtxoPredFailure era)

deriving instance
  ( Era era
  , Ord (Value era)
  , Ord (PredicateFailure (EraRule "UTXOS" era))
  , Ord (TxOut era)
  , Ord (Script era)
  , Ord TxIn
  ) =>
  Ord (DijkstraUtxoPredFailure era)

instance
  ( Era era
  , NFData (Value era)
  , NFData (TxOut era)
  , NFData (PredicateFailure (EraRule "UTXOS" era))
  ) =>
  NFData (DijkstraUtxoPredFailure era)

validateNoPtrInCollateralReturn ::
  ( BabbageEraTxBody era
  , InjectRuleFailure rule DijkstraUtxoPredFailure era
  ) =>
  TxBody TopTx era ->
  Rule (EraRule rule era) ctx ()
validateNoPtrInCollateralReturn txBody = do
  let hasCollateralTxOut = do
        SJust collateralReturn <- pure $ txBody ^. collateralReturnTxBodyL
        Addr _ _ (StakeRefPtr {}) <- pure $ collateralReturn ^. addrTxOutL
        Just collateralReturn
  failOnJustStatic hasCollateralTxOut (injectFailure . PtrPresentInCollateralReturn)

-- | For each account, the total withdrawals across the entire batch should not exceed the original account balance.
-- Unregistered accounts are treated as having 0 balance.
validateBatchWithdrawals ::
  ( EraTx era
  , EraAccounts era
  , DijkstraEraTxBody era
  ) =>
  Accounts era ->
  Tx TopTx era ->
  Test (DijkstraUtxoPredFailure era)
validateBatchWithdrawals accounts tx =
  let allWithdrawals =
        Map.unionsWith (<>) $
          unWithdrawals (tx ^. bodyTxL . withdrawalsTxBodyL)
            : [ unWithdrawals $ subTx ^. bodyTxL . withdrawalsTxBodyL
              | subTx <- OMap.elems $ tx ^. bodyTxL . subTransactionsTxBodyL
              ]
      badWithdrawals =
        Map.mapMaybeWithKey
          ( \acctAddr withdrawn ->
              let balance = getAccountBalance acctAddr
               in if withdrawn > balance
                    then Just Mismatch {mismatchSupplied = withdrawn, mismatchExpected = balance}
                    else Nothing
          )
          allWithdrawals
   in failureOnNonEmptyMap badWithdrawals WithdrawalsExceedAccountBalance
  where
    getAccountBalance (AccountAddress _ (AccountId cred)) =
      case lookupAccountState cred accounts of
        Nothing -> mempty -- unregistered account, 0 balance
        Just accountState -> fromCompact $ accountState ^. balanceAccountStateL

-- | Validate collateral if any transaction in the batch has redeemers.
validateBatchCollateral ::
  forall era rule.
  ( AlonzoEraTx era
  , DijkstraEraTxBody era
  , TxOut era ~ DijkstraTxOut era
  , InjectRuleFailure rule Alonzo.AlonzoUtxoPredFailure era
  , InjectRuleFailure rule Babbage.BabbageUtxoPredFailure era
  ) =>
  PParams era ->
  Tx TopTx era ->
  UTxO era ->
  Test (EraRuleFailure rule era)
validateBatchCollateral pp tx (UTxO utxo) =
  -- TODO OPTIMIZATION: Rewrite in a way that doesn't require this check when rules are executed without validation
  when (hasAnyRedeemers tx) $
    validateTotalCollateralTotalAda pp (tx ^. bodyTxL) utxoCollateral
  where
    utxoCollateral = Map.restrictKeys utxo (tx ^. bodyTxL . collateralInputsTxBodyL)
    hasAnyRedeemers t =
      hasRedeemers t || any hasRedeemers (t ^. bodyTxL . subTransactionsTxBodyL)
    hasRedeemers = not . null . (^. witsTxL . rdmrsTxWitsL . unRedeemersL)

-- | 'Babbage.validateTotalCollateral' with the balance computed on TOTAL
-- ada. The split moved the deposits out of the value, but consuming a
-- collateral input still releases its deposit, and the total collateral a
-- pre-boundary transaction signed was computed on merged values: reading
-- application ada only would both under-count the available collateral and
-- reject era-boundary transactions on 'IncorrectTotalCollateralField'.
validateTotalCollateralTotalAda ::
  forall era rule.
  ( BabbageEraTxBody era
  , TxOut era ~ DijkstraTxOut era
  , InjectRuleFailure rule Alonzo.AlonzoUtxoPredFailure era
  , InjectRuleFailure rule Babbage.BabbageUtxoPredFailure era
  ) =>
  PParams era ->
  TxBody TopTx era ->
  Map.Map TxIn (TxOut era) ->
  Test (EraRuleFailure rule era)
validateTotalCollateralTotalAda pp txBody utxoCollateral =
  sequenceA_
    [ first (fmap injectFailure) $ Alonzo.validateScriptsNotPaidUTxO utxoCollateral
    , first (fmap injectFailure) $ Babbage.validateCollateralContainsNonADA txBody utxoCollateral
    , first (fmap injectFailure) $
        Alonzo.validateInsufficientCollateral pp txBody (totalAdaCollateralBalance txBody utxoCollateral)
    , first (fmap injectFailure) $
        Babbage.validateCollateralEqBalance
          (totalAdaCollateralBalance txBody utxoCollateral)
          (txBody ^. totalCollateralTxBodyL)
    , first (fmap injectFailure) $ failureIf (null utxoCollateral) (Alonzo.NoCollateralInputs @era)
    ]

-- | The collateral balance in total ada: everything the collateral inputs
-- release minus everything the signed return keeps — each side counting
-- application ada plus capacity deposit.
totalAdaCollateralBalance ::
  (BabbageEraTxBody era, TxOut era ~ DijkstraTxOut era) =>
  TxBody TopTx era ->
  Map.Map TxIn (TxOut era) ->
  DeltaCoin
totalAdaCollateralBalance txBody utxoCollateral =
  toDeltaCoin (foldMap' totalAdaTxOut utxoCollateral)
    <-> toDeltaCoin (strictMaybe mempty totalAdaTxOut (txBody ^. collateralReturnTxBodyL))

-- | Ensure that value consumed and produced matches up exactly,  aggregated across the entire batch
-- (top-level transaction and all its sub-transactions).
--
-- > consumed pp utxo txb = produced pp poolParams txb
validateValueNotConservedUTxO ::
  EraUTxO era =>
  PParams era ->
  UTxO era ->
  PState era ->
  TxBody TopTx era ->
  Test (Shelley.ShelleyUtxoPredFailure era)
validateValueNotConservedUTxO pp utxo pState txBody =
  failureUnless (consumedValue == producedValue) $
    Shelley.ValueNotConservedUTxO
      Mismatch
        { mismatchSupplied = consumedValue
        , mismatchExpected = producedValue
        }
  where
    consumedValue = dijkstraConsumed pp utxo txBody
    producedValue = produced pp pState txBody

dijkstraUtxoTransition ::
  forall era.
  ( EraUTxO era
  , EraCertState era
  , DijkstraEraTxBody era
  , DijkstraEraTxOut era
  , TxOut era ~ DijkstraTxOut era
  , AlonzoEraTx era
  , EraStake era
  , InjectRuleFailure "UTXO" Shelley.ShelleyUtxoPredFailure era
  , InjectRuleFailure "UTXO" Allegra.AllegraUtxoPredFailure era
  , InjectRuleFailure "UTXO" Alonzo.AlonzoUtxoPredFailure era
  , InjectRuleFailure "UTXO" Babbage.BabbageUtxoPredFailure era
  , InjectRuleFailure "UTXO" DijkstraUtxoPredFailure era
  , Environment (EraRule "UTXO" era) ~ DijkstraUtxoEnv era
  , State (EraRule "UTXO" era) ~ UTxOState era
  , Signal (EraRule "UTXO" era) ~ StAnnTx TopTx era
  , BaseM (EraRule "UTXO" era) ~ ShelleyBase
  , STS (EraRule "UTXO" era)
  , Event (EraRule "UTXO" era) ~ Alonzo.AlonzoUtxoEvent era
  , -- In this function we call the UTXOS rule, so we need some assumptions
    Environment (EraRule "UTXOS" era) ~ ()
  , State (EraRule "UTXOS" era) ~ ()
  , Signal (EraRule "UTXOS" era) ~ StAnnTx TopTx era
  , Embed (EraRule "UTXOS" era) (EraRule "UTXO" era)
  ) =>
  TransitionRule (EraRule "UTXO" era)
dijkstraUtxoTransition = do
  TRC (DijkstraUtxoEnv slot pp certState originalUtxo, utxos, stAnnTx) <-
    judgmentContext
  let tx = stAnnTx ^. txStAnnTxG
  -- this is the original Accounts, before any transactions were applied
  let accounts = certState ^. certDStateL . accountsL
  let originalPState = certState ^. certPStateL

  let txBody = tx ^. bodyTxL

  {- inInterval (SlotOf Γ) (ValidIntervalOf txTop) -}
  runTest $ Allegra.validateOutsideValidityIntervalUTxO slot txBody

  sysSt <- liftSTS $ asks systemStart
  ei <- liftSTS $ asks epochInfo

  runTest $ Alonzo.validateOutsideForecast ei slot sysSt tx

  {- SpendInputs ≠ ∅ -}
  runTestOnSignal $ Shelley.validateInputSetEmptyUTxO txBody

  let allInputs = txBody ^. allInputsTxBodyF
      inputs = txBody ^. inputsTxBodyL

  {- SpendInputsOf txTop ∪ RefInputsOf txTop ∪ CollInputsOf txTop ⊆ dom(utxo₀) -}
  runTest $ Shelley.validateBadInputsUTxO originalUtxo allInputs

  {- SpendInputsOf txTop ⊆ dom(utxo_s) — prevents double-spend with subtxs -}
  runTest $ Shelley.validateBadInputsUTxO (utxosUtxo utxos) inputs

  {- minfee pp txTop utxo₀ ≤ txfee txb -}
  runTest $ Shelley.validateFeeTooSmallUTxO pp tx originalUtxo

  {- (RedeemersOf txTop ≠ ∅ ⊎ Any (λ txSub → RedeemersOf txSub ≠ ∅) subtxs) → collateralCheck -}
  validate $ validateBatchCollateral pp tx originalUtxo

  runTest $ validateBatchWithdrawals accounts tx

  {- consumed pp utxo₀ txb = produced pp certState txb -}
  runTest $ validateValueNotConservedUTxO pp originalUtxo originalPState txBody

  {- ∀ txout ∈ allOuts txb, capacityDeposit txout = (160 + serSize txout) * coinsPerUTxOByte pp -}
  let allSizedOutputs = txBody ^. allSizedOutputsTxBodyF
  runTest $
    validateOutputCapacityDeposit
      IncorrectCapacityDepositUTxO
      ImplicitOutputTooSmallUTxO
      pp
      allSizedOutputs

  let allOutputs = fmap sizedValue allSizedOutputs
  {- ∀ txout ∈ allOuts txb, serSize (getValue txout) ≤ maxValSize pp -}
  runTest $ Alonzo.validateOutputTooBigUTxO pp allOutputs

  {- ∀ ( _ ↦ (a,_)) ∈ allOuts txb, a ∈ Addrbootstrap → bootstrapAttrsSize a ≤ 64 -}
  runTestOnSignal $ Shelley.validateOutputBootAddrAttrsTooBig allOutputs

  netId <- liftSTS $ asks networkId

  {- ∀(_ → (a, _)) ∈ allOuts txb, netId a = NetworkId -}
  runTestOnSignal $ Shelley.validateWrongNetwork netId allOutputs

  {- (txnetworkid txb = NetworkId) ∨ (txnetworkid txb = ◇) -}
  runTestOnSignal $ Alonzo.validateWrongNetworkInTxBody netId txBody

  {- no Ptr in collateral return -}
  validateNoPtrInCollateralReturn txBody

  {- txsize tx ≤ maxTxSize pp -}
  runTestOnSignal $ Shelley.validateMaxTxSizeUTxO pp tx

  {- totExunits tx ≤ maxTxExUnits pp -}
  runTest $ Alonzo.validateExUnitsTooBigUTxO pp tx

  {- ‖collateral tx‖ ≤ maxCollInputs pp -}
  runTest $ Alonzo.validateTooManyCollateralInputs pp txBody

  () <- trans @(EraRule "UTXOS" era) $ TRC ((), (), stAnnTx)
  updateDijkstraUTxOState
    pp
    certState
    tx
    (Conway.updateTreasuryDonation tx utxos)

-- | The final stage of the UTXO rule, like 'Babbage.updateUTxOState', plus
-- the second application point of the translation: every output entering
-- the UTxO map is restructured by 'fundCapacityDeposits' — an implicit
-- (merged-form) output gets its deposit derived, an explicit exact output
-- passes through unchanged — so the stored state is always split.
updateDijkstraUTxOState ::
  forall era.
  ( AlonzoEraTx era
  , BabbageEraTxBody era
  , EraStake era
  , EraCertState era
  , BabbageEraPParams era
  , TxOut era ~ DijkstraTxOut era
  , Event (EraRule "UTXO" era) ~ Alonzo.AlonzoUtxoEvent era
  ) =>
  PParams era ->
  CertState era ->
  Tx TopTx era ->
  UTxOState era ->
  Rule (EraRule "UTXO" era) 'Transition (UTxOState era)
updateDijkstraUTxOState pp certState tx utxoState =
  case tx ^. isPhase2ValidTxL of
    Phase2Valid -> updateDijkstraUTxOStatePhase2Valid pp certState (tx ^. bodyTxL) utxoState
    Phase2Invalid -> updateDijkstraUTxOStatePhase2Invalid pp (tx ^. bodyTxL) utxoState

updateDijkstraUTxOStatePhase2Valid ::
  forall era.
  ( EraTxBody era
  , EraStake era
  , EraCertState era
  , EraScript era
  , BabbageEraPParams era
  , TxOut era ~ DijkstraTxOut era
  , Event (EraRule "UTXO" era) ~ Alonzo.AlonzoUtxoEvent era
  ) =>
  PParams era ->
  CertState era ->
  TxBody TopTx era ->
  UTxOState era ->
  Rule (EraRule "UTXO" era) 'Transition (UTxOState era)
updateDijkstraUTxOStatePhase2Valid pp certState txBody utxoState = do
  utxoStateWithDeposits <-
    Shelley.updateUTxOStateDeposits
      pp
      certState
      txBody
      (tellEvent . Alonzo.TotalDeposits (hashAnnotated txBody))
      utxoState
  utxoStateWithOutputs <-
    updateDijkstraUTxOAndInstantStake
      pp
      txBody
      (\utxoDeleted utxoAdded -> tellEvent $ Alonzo.TxUTxODiff utxoDeleted utxoAdded)
      utxoStateWithDeposits
  pure $! utxoStateWithOutputs & utxosFeesL <>~ (txBody ^. feeTxBodyL)

-- | 'Shelley.updateUTxOAndInstantStake', with the entering outputs
-- restructured before they join the map. The instant stake is computed from
-- the restructured, stored form.
updateDijkstraUTxOAndInstantStake ::
  ( EraTxBody era
  , EraStake era
  , EraScript era
  , BabbageEraPParams era
  , TxOut era ~ DijkstraTxOut era
  , Monad m
  ) =>
  PParams era ->
  TxBody l era ->
  (UTxO era -> UTxO era -> m ()) ->
  UTxOState era ->
  m (UTxOState era)
updateDijkstraUTxOAndInstantStake pp txBody utxoDiffEvent utxoState =
  applyEnteringOutputs
    utxoDiffEvent
    (fundCapacityDeposits (pp ^. ppCoinsPerUTxOByteL) (txouts txBody))
    (extractKeys (unUTxO (utxosUtxo utxoState)) (txBody ^. inputsTxBodyL))
    utxoState

applyEnteringOutputs ::
  (EraStake era, Monad m) =>
  (UTxO era -> UTxO era -> m ()) ->
  UTxO era ->
  (Map.Map TxIn (TxOut era), Map.Map TxIn (TxOut era)) ->
  UTxOState era ->
  m (UTxOState era)
applyEnteringOutputs utxoDiffEvent enteringOutputs (utxoWithoutInputs, utxoDeleted) utxoState = do
  utxoDiffEvent (UTxO utxoDeleted) enteringOutputs
  pure $!
    utxoState
      { utxosUtxo = UTxO (Map.union utxoWithoutInputs (unUTxO enteringOutputs))
      , utxosInstantStake =
          deleteInstantStake
            (UTxO utxoDeleted)
            (addInstantStake enteringOutputs (utxosInstantStake utxoState))
      }

-- | The collateral path of 'Babbage.updateUTxOState', with the entering
-- collateral-return output restructured like any other entering output.
updateDijkstraUTxOStatePhase2Invalid ::
  forall era.
  ( BabbageEraTxBody era
  , EraStake era
  , BabbageEraPParams era
  , TxOut era ~ DijkstraTxOut era
  ) =>
  PParams era ->
  TxBody TopTx era ->
  UTxOState era ->
  Rule (EraRule "UTXO" era) 'Transition (UTxOState era)
updateDijkstraUTxOStatePhase2Invalid pp txBody utxoState =
  applyCollateralOutputs
    (fundCapacityDeposits (pp ^. ppCoinsPerUTxOByteL) (collOuts txBody))
    (extractKeys (unUTxO (utxosUtxo utxoState)) (txBody ^. collateralInputsTxBodyL))
    utxoState

applyCollateralOutputs ::
  (EraStake era, Val (Value era), TxOut era ~ DijkstraTxOut era, Monad m) =>
  UTxO era ->
  (Map.Map TxIn (TxOut era), Map.Map TxIn (TxOut era)) ->
  UTxOState era ->
  m (UTxOState era)
applyCollateralOutputs collateralOutputs (utxoWithoutCollateral, utxoDeleted) utxoState =
  pure $!
    utxoState
      { utxosUtxo = UTxO (Map.union utxoWithoutCollateral (unUTxO collateralOutputs))
      , utxosFees = utxosFees utxoState <> releasedCollateralAda utxoDeleted collateralOutputs
      , utxosInstantStake =
          deleteInstantStake
            (UTxO utxoDeleted)
            (addInstantStake collateralOutputs (utxosInstantStake utxoState))
      }

-- | The total ada a phase-2-invalid transaction surrenders to the fee pot:
-- everything its collateral inputs held — application ada AND the deposits
-- they release — minus everything the restructured return output keeps,
-- its own deposit included (it stays locked in the UTxO). Balancing totals
-- on both sides is what conserves ada: the merged-world 'collAdaBalance'
-- reads application ada only and would destroy the consumed inputs'
-- deposits.
releasedCollateralAda ::
  (Val (Value era), TxOut era ~ DijkstraTxOut era) =>
  Map.Map TxIn (TxOut era) ->
  UTxO era ->
  Coin
releasedCollateralAda utxoDeleted collateralOutputs =
  Coin $
    unCoin (foldMap' totalAdaTxOut utxoDeleted)
      - unCoin (foldMap' totalAdaTxOut (unUTxO collateralOutputs))

--------------------------------------------------------------------------------
-- UTXO STS
--------------------------------------------------------------------------------

instance
  forall era.
  ( EraTx era
  , EraUTxO era
  , EraStake era
  , DijkstraEraTxBody era
  , AlonzoEraTx era
  , EraRule "UTXO" era ~ UTXO era
  , InjectRuleFailure "UTXO" Shelley.ShelleyUtxoPredFailure era
  , InjectRuleFailure "UTXO" Allegra.AllegraUtxoPredFailure era
  , InjectRuleFailure "UTXO" Alonzo.AlonzoUtxoPredFailure era
  , InjectRuleFailure "UTXO" Babbage.BabbageUtxoPredFailure era
  , InjectRuleFailure "UTXO" Conway.ConwayUtxoPredFailure era
  , InjectRuleFailure "UTXO" DijkstraUtxoPredFailure era
  , Environment (EraRule "UTXO" era) ~ DijkstraUtxoEnv era
  , State (EraRule "UTXO" era) ~ UTxOState era
  , Signal (EraRule "UTXO" era) ~ StAnnTx TopTx era
  , BaseM (EraRule "UTXO" era) ~ ShelleyBase
  , STS (EraRule "UTXO" era)
  , -- In this function we we call the UTXOS rule, so we need some assumptions
    Embed (EraRule "UTXOS" era) (UTXO era)
  , Environment (EraRule "UTXOS" era) ~ ()
  , State (EraRule "UTXOS" era) ~ ()
  , Signal (EraRule "UTXOS" era) ~ StAnnTx TopTx era
  , EraCertState era
  , DijkstraEraTxOut era
  , TxOut era ~ DijkstraTxOut era
  , EraRule "UTXO" era ~ UTXO era
  , SafeToHash (TxWits era)
  ) =>
  STS (UTXO era)
  where
  type State (UTXO era) = UTxOState era
  type Signal (UTXO era) = StAnnTx TopTx era
  type Environment (UTXO era) = DijkstraUtxoEnv era
  type BaseM (UTXO era) = ShelleyBase
  type PredicateFailure (UTXO era) = DijkstraUtxoPredFailure era
  type Event (UTXO era) = Alonzo.AlonzoUtxoEvent era

  initialRules = []

  transitionRules = [dijkstraUtxoTransition @era]

  assertions = [Shelley.validSizeComputationCheck]

instance
  ( STS (Conway.UTXOS era)
  , PredicateFailure (EraRule "UTXOS" era) ~ Conway.ConwayUtxosPredFailure era
  , Event (EraRule "UTXOS" era) ~ Event (Conway.UTXOS era)
  ) =>
  Embed (Conway.UTXOS era) (UTXO era)
  where
  wrapFailed = UtxosFailure
  wrapEvent = Alonzo.UtxosEvent

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

instance
  ( Era era
  , EncCBOR (TxOut era)
  , EncCBOR (Value era)
  , EncCBOR (PredicateFailure (EraRule "UTXOS" era))
  ) =>
  EncCBOR (DijkstraUtxoPredFailure era)
  where
  encCBOR =
    encode . \case
      UtxosFailure a -> Sum (UtxosFailure @era) 0 !> To a
      BadInputsUTxO ins -> Sum (BadInputsUTxO @era) 1 !> To ins
      OutsideValidityIntervalUTxO a b -> Sum OutsideValidityIntervalUTxO 2 !> To a !> To b
      MaxTxSizeUTxO mm -> Sum MaxTxSizeUTxO 3 !> To mm
      InputSetEmptyUTxO -> Sum InputSetEmptyUTxO 4
      FeeTooSmallUTxO mm -> Sum FeeTooSmallUTxO 5 !> To mm
      ValueNotConservedUTxO mm -> Sum (ValueNotConservedUTxO @era) 6 !> To mm
      WrongNetwork right wrongs -> Sum (WrongNetwork @era) 7 !> To right !> To wrongs
      OutputBootAddrAttrsTooBig outs -> Sum (OutputBootAddrAttrsTooBig @era) 9 !> To outs
      OutputTooBigUTxO outs -> Sum (OutputTooBigUTxO @era) 10 !> To outs
      InsufficientCollateral a b -> Sum InsufficientCollateral 11 !> To a !> To b
      ScriptsNotPaidUTxO a -> Sum ScriptsNotPaidUTxO 12 !> To a
      ExUnitsTooBigUTxO mm -> Sum ExUnitsTooBigUTxO 13 !> To mm
      CollateralContainsNonADA a -> Sum CollateralContainsNonADA 14 !> To a
      WrongNetworkInTxBody mm -> Sum WrongNetworkInTxBody 15 !> To mm
      OutsideForecast a -> Sum OutsideForecast 16 !> To a
      TooManyCollateralInputs mm -> Sum TooManyCollateralInputs 17 !> To mm
      NoCollateralInputs -> Sum NoCollateralInputs 18
      IncorrectTotalCollateralField c1 c2 -> Sum IncorrectTotalCollateralField 19 !> To c1 !> To c2
      BabbageNonDisjointRefInputs x -> Sum BabbageNonDisjointRefInputs 21 !> To x
      PtrPresentInCollateralReturn x -> Sum PtrPresentInCollateralReturn 22 !> To x
      WithdrawalsExceedAccountBalance mm -> Sum WithdrawalsExceedAccountBalance 24 !> To mm
      IncorrectCapacityDepositUTxO x -> Sum IncorrectCapacityDepositUTxO 25 !> To x
      ImplicitOutputTooSmallUTxO x -> Sum ImplicitOutputTooSmallUTxO 26 !> To x

instance
  ( Era era
  , DecCBOR (TxOut era)
  , EncCBOR (Value era)
  , DecCBOR (Value era)
  , DecCBOR (PredicateFailure (EraRule "UTXOS" era))
  ) =>
  DecCBOR (DijkstraUtxoPredFailure era)
  where
  decCBOR = decode . Summands "DijkstraUtxoPredFailure" $ \case
    0 -> SumD UtxosFailure <! From
    1 -> SumD BadInputsUTxO <! From
    2 -> SumD OutsideValidityIntervalUTxO <! From <! From
    3 -> SumD MaxTxSizeUTxO <! From
    4 -> SumD InputSetEmptyUTxO
    5 -> SumD FeeTooSmallUTxO <! From
    6 -> SumD ValueNotConservedUTxO <! From
    7 -> SumD WrongNetwork <! From <! From
    9 -> SumD OutputBootAddrAttrsTooBig <! From
    10 -> SumD OutputTooBigUTxO <! From
    11 -> SumD InsufficientCollateral <! From <! From
    12 -> SumD ScriptsNotPaidUTxO <! From
    13 -> SumD ExUnitsTooBigUTxO <! From
    14 -> SumD CollateralContainsNonADA <! From
    15 -> SumD WrongNetworkInTxBody <! From
    16 -> SumD OutsideForecast <! From
    17 -> SumD TooManyCollateralInputs <! From
    18 -> SumD NoCollateralInputs
    19 -> SumD IncorrectTotalCollateralField <! From <! From
    21 -> SumD BabbageNonDisjointRefInputs <! From
    22 -> SumD PtrPresentInCollateralReturn <! From
    24 -> SumD WithdrawalsExceedAccountBalance <! From
    25 -> SumD IncorrectCapacityDepositUTxO <! From
    26 -> SumD ImplicitOutputTooSmallUTxO <! From
    n -> Invalid n

-- =====================================================
-- Injecting from one PredicateFailure to another

conwayToDijkstraUtxoPredFailure ::
  forall era.
  Conway.ConwayUtxoPredFailure era ->
  DijkstraUtxoPredFailure era
conwayToDijkstraUtxoPredFailure = \case
  Conway.BadInputsUTxO x -> BadInputsUTxO x
  Conway.OutsideValidityIntervalUTxO vi slotNo -> OutsideValidityIntervalUTxO vi slotNo
  Conway.MaxTxSizeUTxO m -> MaxTxSizeUTxO m
  Conway.InputSetEmptyUTxO -> InputSetEmptyUTxO
  Conway.FeeTooSmallUTxO m -> FeeTooSmallUTxO m
  Conway.ValueNotConservedUTxO m -> ValueNotConservedUTxO m
  Conway.WrongNetwork x y -> WrongNetwork x y
  Conway.WrongNetworkWithdrawal _ _ -> error "Impossible: `WrongNetworkWithdrawal` for UTXO"
  Conway.OutputTooSmallUTxO _ -> error "Impossible: `OutputTooSmallUTxO` for UTXO"
  Conway.UtxosFailure x -> UtxosFailure x
  Conway.OutputBootAddrAttrsTooBig xs -> OutputBootAddrAttrsTooBig xs
  Conway.OutputTooBigUTxO xs -> OutputTooBigUTxO xs
  Conway.InsufficientCollateral c1 c2 -> InsufficientCollateral c1 c2
  Conway.ScriptsNotPaidUTxO u -> ScriptsNotPaidUTxO u
  Conway.ExUnitsTooBigUTxO m -> ExUnitsTooBigUTxO m
  Conway.CollateralContainsNonADA v -> CollateralContainsNonADA v
  Conway.WrongNetworkInTxBody m -> WrongNetworkInTxBody m
  Conway.OutsideForecast sno -> OutsideForecast sno
  Conway.TooManyCollateralInputs m -> TooManyCollateralInputs m
  Conway.NoCollateralInputs -> NoCollateralInputs
  Conway.IncorrectTotalCollateralField dc c -> IncorrectTotalCollateralField dc c
  Conway.BabbageOutputTooSmallUTxO _ ->
    error
      "Impossible: `BabbageOutputTooSmallUTxO` for UTXO (Dijkstra does not run the merged-value min-ada validator)"
  Conway.BabbageNonDisjointRefInputs txin -> BabbageNonDisjointRefInputs txin

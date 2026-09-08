{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Dijkstra.TxOut.Translation.Fixture (
  ValidationFailure (..),
  pricedPParams,
  unitPricePParams,
  zeroPricePParams,
  conwayOutput,
  decodedConwayOutput,
  implicitOutput,
  explicitOutput,
  underfundedOutput,
  zeroCoinOutput,
  noExactImplicitOutput,
  validateOutput,
  decodedConwayBody,
  conwayBody,
  conwayBodyBytes,
) where

import Cardano.Ledger.BaseTypes (Network (Testnet))
import Cardano.Ledger.Binary (
  DecCBOR (decCBOR),
  DecoderError,
  decodeFull,
  decodeFullAnnotator,
  mkSized,
  serialize,
 )
import Cardano.Ledger.Coin (Coin (..), CompactForm (CompactCoin))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Credential (Credential (ScriptHashObj), StakeReference (StakeRefNull))
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Rules.CapacityDeposit (validateOutputCapacityDeposit)
import Cardano.Ledger.Dijkstra.TxOut (CapacityDepositForm (..))
import Cardano.Ledger.Dijkstra.TxOut.ApplicationAssets (ApplicationAssets (..))
import Cardano.Ledger.Dijkstra.TxOut.CapacityDeposit (CapacityDeposit (..))
import Cardano.Ledger.Dijkstra.TxOut.Value (OutputValue (..))
import Cardano.Ledger.Mary.AssetName (AssetName (..))
import Cardano.Ledger.Mary.MultiAsset (MultiAsset (..))
import Cardano.Ledger.Mary.PolicyID (PolicyID (..))
import Cardano.Ledger.Mary.Value (MaryValue (..))
import qualified Data.ByteString.Lazy as LBS
import Data.List (find)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map.Strict as Map
import GHC.Exts (fromList)
import Lens.Micro ((&), (.~), (^.))
import Validation (validationToEither)

pricedPParams :: PParams DijkstraEra
pricedPParams = emptyPParams & ppCoinsPerUTxOByteL .~ CoinPerByte (CompactCoin 4310)

unitPricePParams :: PParams DijkstraEra
unitPricePParams = emptyPParams & ppCoinsPerUTxOByteL .~ CoinPerByte (CompactCoin 1)

zeroPricePParams :: PParams DijkstraEra
zeroPricePParams = emptyPParams & ppCoinsPerUTxOByteL .~ CoinPerByte (CompactCoin 0)

outputAddress :: Addr
outputAddress =
  Addr
    Testnet
    (ScriptHashObj (ScriptHash "00000000000000000000000000000000000000000000000000000000"))
    StakeRefNull

outputNativeAssets :: MultiAsset
outputNativeAssets =
  MultiAsset $
    Map.singleton
      (PolicyID (ScriptHash "01010101010101010101010101010101010101010101010101010101"))
      (Map.fromList [(AssetName "first", 7), (AssetName "second", 11)])

conwayOutput :: TxOut ConwayEra
conwayOutput = mkBasicTxOut outputAddress (MaryValue (Coin 5000000) outputNativeAssets)

decodedConwayOutput :: Either DecoderError (TxOut DijkstraEra)
decodedConwayOutput =
  decodeFull (eraProtVerLow @DijkstraEra) (serialize (eraProtVerHigh @ConwayEra) conwayOutput)

implicitOutput :: TxOut DijkstraEra
implicitOutput = upgradeTxOut conwayOutput

explicitOutput :: TxOut DijkstraEra
explicitOutput =
  implicitOutput
    & outputValueTxOutL
      .~ OutputValue (CapacityDeposit (Coin 1000000)) (ApplicationAssets (Coin 4000000) outputNativeAssets)
    & capacityDepositFormTxOutL .~ ExplicitCapacityDeposit

underfundedOutput :: TxOut DijkstraEra
underfundedOutput = mkBasicTxOut outputAddress (MaryValue (Coin 1) outputNativeAssets)

zeroCoinOutput :: TxOut DijkstraEra
zeroCoinOutput = mkBasicTxOut outputAddress (MaryValue (Coin 0) mempty)

-- | Select a real encoding-width cycle with an independent exhaustive oracle.
-- Each possible deposit is checked, rather than consulting the production
-- interval solver. A small neighbourhood of the first ADA width boundary is
-- enough for this fixed output shape.
noExactImplicitOutput :: Maybe (TxOut DijkstraEra)
noExactImplicitOutput = find hasNoExactAllocation candidates
  where
    CapacityDeposit (Coin base) = getCapacityDepositRequirement unitPricePParams zeroCoinOutput
    candidates = [mkBasicTxOut outputAddress (MaryValue (Coin total) mempty) | total <- [base + 20 .. base + 40]]
    hasNoExactAllocation output =
      let Coin total = output ^. coinTxOutL
       in all
            ( \deposit ->
                let allocated =
                      output
                        & outputValueTxOutL
                          .~ OutputValue (CapacityDeposit (Coin deposit)) (ApplicationAssets (Coin (total - deposit)) mempty)
                        & capacityDepositFormTxOutL .~ ExplicitCapacityDeposit
                 in CapacityDeposit (Coin deposit) /= getCapacityDepositRequirement unitPricePParams allocated
            )
            [0 .. total]

data ValidationFailure = ExplicitMismatch | ImplicitFloorMismatch | AllocationFailure
  deriving (Eq, Show)

validateOutput :: PParams DijkstraEra -> TxOut DijkstraEra -> Either (NonEmpty ValidationFailure) ()
validateOutput pp output =
  validationToEither $
    validateOutputCapacityDeposit
      (const ExplicitMismatch)
      (const ImplicitFloorMismatch)
      (const AllocationFailure)
      pp
      -- Validation receives the signed output's actual form. Canonical tariff
      -- measurement happens inside the rule and must not change this lane.
      [mkSized (eraProtVerLow @DijkstraEra) output]

conwayBody :: TxBody TopTx ConwayEra
conwayBody = mkBasicTxBody & outputsTxBodyL .~ fromList [conwayOutput]

conwayBodyBytes :: LBS.ByteString
conwayBodyBytes = serialize (eraProtVerHigh @ConwayEra) conwayBody

decodedConwayBody :: Either DecoderError (TxBody TopTx DijkstraEra)
decodedConwayBody = decodeFullAnnotator (eraProtVerLow @DijkstraEra) "ConwayTxBody" decCBOR conwayBodyBytes

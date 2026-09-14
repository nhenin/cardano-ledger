{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Dijkstra.TxOut.Value.Translation.Fixture (
  NativeEntries,
  AllocationCase (..),
  RejectionCase (..),
  nativeScenarios,
  successfulAllocations,
  allocationsWithSameMaryValue,
  allocationsWithRetainedDeposit,
  negativeRequestedDeposit,
  unfundedAllocations,
  pricedPParams,
  capacityDepositAtPrice,
  fundedOutput,
  fundedMaryValue,
  fundedApplicationCoins,
  outputSizeScenarios,
  exactlyFundedOutput,
  underfundedOutput,
  conwayOutput,
) where

import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Babbage.PParams (BabbageEraPParams, ppCoinsPerUTxOByteL)
import Cardano.Ledger.Babbage.TxOut (BabbageTxOut (BabbageTxOut))
import Cardano.Ledger.BaseTypes (Network (..), StrictMaybe (..))
import Cardano.Ledger.Binary (serialize)
import Cardano.Ledger.Coin (Coin (..), CoinPerByte (..), CompactForm (..))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Core (EraTxOut (..), PParams, coinTxOutL, emptyPParams, eraProtVerLow)
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import Cardano.Ledger.Dijkstra.TxOut.ApplicationAssets (ApplicationAssets (..))
import Cardano.Ledger.Dijkstra.TxOut.CapacityDeposit (CapacityDeposit (..))
import Cardano.Ledger.Dijkstra.TxOut.Value (OutputValue (..))
import Cardano.Ledger.Dijkstra.TxOut.Value.Translation (AllocationError (..))
import Cardano.Ledger.Hashes (ScriptHash (..))
import Cardano.Ledger.Mary.AssetName (AssetName (..))
import Cardano.Ledger.Mary.MultiAsset (MultiAsset (..))
import Cardano.Ledger.Mary.PolicyID (PolicyID (..))
import Cardano.Ledger.Mary.Value (MaryValue (..))
import Cardano.Ledger.Plutus (Language (..))
import Cardano.Ledger.Plutus.Data (Datum (..), dataToBinaryData)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import Data.Word (Word64)
import Lens.Micro ((&), (.~))
import Test.Cardano.Ledger.Alonzo.Arbitrary (alwaysSucceeds)
import Test.Cardano.Ledger.Alonzo.Examples (exampleDatum)
import Test.Cardano.Ledger.Shelley.Examples (mkKeyHash, mkScriptHash)

type NativeEntries = Map.Map PolicyID (Map.Map AssetName Integer)

data AllocationCase = AllocationCase
  { allocationSource :: MaryValue
  , allocationDeposit :: CapacityDeposit
  , remainingApplicationCoins :: Coin
  }

data RejectionCase = RejectionCase
  { rejectedSource :: MaryValue
  , rejectedDeposit :: CapacityDeposit
  , expectedRejection :: AllocationError
  }

successfulAllocations :: NativeEntries -> [(String, AllocationCase)]
successfulAllocations native =
  [ ("zero total ADA", allocationCase native 0 0 0)
  , ("zero capacity deposit", allocationCase native 13 0 13)
  , ("partial capacity deposit", allocationCase native 13 4 9)
  , ("all ADA allocated to the capacity deposit", allocationCase native 13 13 0)
  ]

allocationCase :: NativeEntries -> Integer -> Integer -> Integer -> AllocationCase
allocationCase native total requested remaining =
  AllocationCase
    { allocationSource = MaryValue (Coin total) (MultiAsset native)
    , allocationDeposit = CapacityDeposit (Coin requested)
    , remainingApplicationCoins = Coin remaining
    }

allocationsWithSameMaryValue :: NativeEntries -> (OutputValue, OutputValue)
allocationsWithSameMaryValue native =
  ( OutputValue (CapacityDeposit (Coin 4)) (ApplicationAssets (MaryValue (Coin 9) (MultiAsset native)))
  , OutputValue (CapacityDeposit (Coin 9)) (ApplicationAssets (MaryValue (Coin 4) (MultiAsset native)))
  )

allocationsWithRetainedDeposit :: NativeEntries -> [(String, OutputValue)]
allocationsWithRetainedDeposit native =
  let (first, second) = allocationsWithSameMaryValue native
   in [("the allocation with deposit 4", first), ("the allocation with deposit 9", second)]

negativeRequestedDeposit :: RejectionCase
negativeRequestedDeposit =
  let deposit = CapacityDeposit (Coin (-1))
   in RejectionCase
        { rejectedSource = MaryValue (Coin 10) mempty
        , rejectedDeposit = deposit
        , expectedRejection = NegativeCapacityDeposit deposit
        }

unfundedAllocations :: [(String, RejectionCase)]
unfundedAllocations =
  [ ("a deposit above the available ADA", unfundedAllocation 10 11)
  , ("a negative available ADA balance", unfundedAllocation (-1) 0)
  ]

unfundedAllocation :: Integer -> Integer -> RejectionCase
unfundedAllocation available requested =
  let availableCoins = Coin available
      deposit = CapacityDeposit (Coin requested)
   in RejectionCase
        { rejectedSource = MaryValue availableCoins mempty
        , rejectedDeposit = deposit
        , expectedRejection = CapacityDepositExceedsOutputCoins availableCoins deposit
        }

-- Translation accepts unchecked native maps, including negative and zero
-- quantities and empty policies. These fixtures preserve those exact entries.
nativeScenarios :: [(String, NativeEntries)]
nativeScenarios =
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

pricedPParams :: BabbageEraPParams era => Word64 -> PParams era
pricedPParams price = emptyPParams & ppCoinsPerUTxOByteL .~ CoinPerByte (CompactCoin price)

-- Independent expectation for the Conway source output's pricing rule.
capacityDepositAtPrice :: forall era. EraTxOut era => Word64 -> TxOut era -> CapacityDeposit
capacityDepositAtPrice price txOut =
  CapacityDeposit $ Coin $ toInteger price * (160 + toInteger (LBS.length encodedOutput))
  where
    encodedOutput = serialize (eraProtVerLow @era) txOut

fundedMaryValue :: MaryValue
fundedMaryValue =
  MaryValue (Coin 10000000) $
    MultiAsset $
      Map.singleton (PolicyID (mkScriptHash 3)) (Map.singleton (AssetName "token") 100)

baseAddress :: Addr
baseAddress = Addr Testnet (KeyHashObj (mkKeyHash 1)) (StakeRefBase (KeyHashObj (mkKeyHash 2)))

fundedOutput :: TxOut ConwayEra
fundedOutput = mkBasicTxOut baseAddress fundedMaryValue

fundedApplicationCoins :: Coin
fundedApplicationCoins =
  let MaryValue (Coin total) _ = fundedMaryValue
      CapacityDeposit (Coin deposit) = capacityDepositAtPrice 4310 fundedOutput
   in Coin (total - deposit)

outputSizeScenarios :: [(String, TxOut ConwayEra)]
outputSizeScenarios =
  [ ("a base address", fundedOutput)
  ,
    ( "an enterprise address"
    , mkBasicTxOut (Addr Testnet (KeyHashObj (mkKeyHash 1)) StakeRefNull) fundedMaryValue
    )
  ,
    ( "an inline datum"
    , BabbageTxOut baseAddress fundedMaryValue (Datum (dataToBinaryData exampleDatum)) SNothing
    )
  ,
    ( "a reference script"
    , BabbageTxOut baseAddress fundedMaryValue NoDatum (SJust (alwaysSucceeds @'PlutusV1 0))
    )
  ]

-- Both the funded total and its deposit use five-byte CBOR coin encodings,
-- so these boundary amounts leave the output size unchanged.
exactlyFundedOutput :: TxOut ConwayEra
exactlyFundedOutput =
  fundedOutput & coinTxOutL .~ unCapacityDeposit (capacityDepositAtPrice 4310 fundedOutput)

underfundedOutput :: TxOut ConwayEra
underfundedOutput =
  let CapacityDeposit (Coin deposit) = capacityDepositAtPrice 4310 fundedOutput
   in fundedOutput & coinTxOutL .~ Coin (deposit - 1)

conwayOutput :: TxOut ConwayEra
conwayOutput = mkBasicTxOut baseAddress fundedMaryValue

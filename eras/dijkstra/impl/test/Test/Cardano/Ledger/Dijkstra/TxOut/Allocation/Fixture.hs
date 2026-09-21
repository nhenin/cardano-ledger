{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Dijkstra.TxOut.Allocation.Fixture (
  AllocationCase (..),
  allocationCases,
  allocatedOutput,
  expectedTotalValue,
  noncanonicalProjections,
  projectedOutputFields,
  suppliedDeposit,
  negativeDeposit,
  excessiveDeposit,
  insufficientTotalCoins,
  replacementAssets,
  replacementValue,
  replacementCoins,
  replacementAddress,
  replacementDatum,
  replacementScript,
) where

import Cardano.Ledger.Address (Addr (..), compactAddr)
import Cardano.Ledger.Alonzo.Scripts (AlonzoEraScript (fromPlutusScript))
import Cardano.Ledger.Alonzo.TxBody (encodeAddress28, encodeDataHash32)
import Cardano.Ledger.Babbage.TxOut (BabbageTxOut (..))
import Cardano.Ledger.BaseTypes (Network (..), StrictMaybe)
import Cardano.Ledger.Coin (Coin (..), CompactForm (..))
import Cardano.Ledger.Compactible (toCompactPartial)
import Cardano.Ledger.Core (Script)
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Dijkstra.Scripts (PlutusScript (DijkstraPlutusV1, DijkstraPlutusV2))
import Cardano.Ledger.Dijkstra.TxOut (DijkstraTxOut (DijkstraTxOut))
import Cardano.Ledger.Dijkstra.TxOut.ApplicationAssets (ApplicationAssets (..), nativeAssets)
import Cardano.Ledger.Dijkstra.TxOut.CapacityDeposit (CapacityDeposit (..))
import Cardano.Ledger.Dijkstra.TxOut.Value (OutputValue (..))
import Cardano.Ledger.Mary.AssetName (AssetName (..))
import Cardano.Ledger.Mary.MultiAsset (MultiAsset (..))
import Cardano.Ledger.Mary.PolicyID (PolicyID (..))
import Cardano.Ledger.Mary.Value (MaryValue (..))
import Cardano.Ledger.Plutus (Language (..))
import Cardano.Ledger.Plutus.Data (Datum (..), dataToBinaryData, hashData)
import qualified Data.Map.Strict as Map
import Test.Cardano.Ledger.Alonzo.Examples (exampleDatum)
import Test.Cardano.Ledger.Core.Utils (mkDummySafeHash)
import Test.Cardano.Ledger.Plutus (alwaysSucceedsPlutus)
import Test.Cardano.Ledger.Shelley.Examples (mkKeyHash, mkScriptHash)

data AllocationCase = AllocationCase
  { totalProjection :: BabbageTxOut DijkstraEra
  , expectedApplicationAssets :: ApplicationAssets
  }

allocationCases :: [(String, AllocationCase)]
allocationCases =
  [ ("compact application assets", AllocationCase (TxOutCompact' address compactAssets) assets)
  ,
    ( "compact application assets with datum hash"
    , AllocationCase (TxOutCompactDH' address compactAssets datumHash) assets
    )
  ,
    ( "compact inline datum"
    , AllocationCase (TxOutCompactDatum address compactAssets binaryDatum) assets
    )
  ,
    ( "compact reference script"
    , AllocationCase (TxOutCompactRefScript address compactAssets NoDatum script) assets
    )
  ,
    ( "optimized application ADA"
    , AllocationCase (TxOut_AddrHash28_AdaOnly staking address28 coins) adaAssets
    )
  ,
    ( "optimized application ADA with datum hash"
    , AllocationCase (TxOut_AddrHash28_AdaOnly_DataHash32 staking address28 coins hash32) adaAssets
    )
  ]
  where
    address = compactAddr (Addr Testnet (KeyHashObj (mkKeyHash 1)) (StakeRefBase staking))
    staking = KeyHashObj (mkKeyHash 2)
    address28 = encodeAddress28 Testnet (KeyHashObj (mkKeyHash 1))
    coins = CompactCoin 61
    adaAssets = ApplicationAssets (MaryValue (Coin 42) mempty)
    assets = applicationAssetsWithToken 42 7
    compactAssets = toCompactPartial (MaryValue (Coin 61) (nativeAssets assets))
    datumHash = hashData (exampleDatum @DijkstraEra)
    hash32 = encodeDataHash32 datumHash
    binaryDatum = dataToBinaryData (exampleDatum @DijkstraEra)
    script = fromPlutusScript (DijkstraPlutusV1 (alwaysSucceedsPlutus @'PlutusV1 0))

allocatedOutput :: AllocationCase -> DijkstraTxOut
allocatedOutput fixture =
  case totalProjection fixture of
    BabbageTxOut address _ datum script ->
      DijkstraTxOut address (OutputValue suppliedDeposit (expectedApplicationAssets fixture)) datum script

expectedTotalValue :: AllocationCase -> MaryValue
expectedTotalValue fixture = MaryValue (Coin 61) (nativeAssets (expectedApplicationAssets fixture))

noncanonicalProjections :: [BabbageTxOut DijkstraEra]
noncanonicalProjections =
  [ TxOutCompact' (compactAddr address) (toCompactPartial (MaryValue (Coin 61) mempty))
  , TxOutCompactDH' (compactAddr address) (toCompactPartial (MaryValue (Coin 61) mempty)) datumHash
  ]
  where
    address = Addr Testnet (KeyHashObj (mkKeyHash 1)) (StakeRefBase (KeyHashObj (mkKeyHash 2)))
    datumHash = hashData (exampleDatum @DijkstraEra)

projectedOutputFields ::
  BabbageTxOut DijkstraEra -> (Addr, MaryValue, Datum DijkstraEra, StrictMaybe (Script DijkstraEra))
projectedOutputFields (BabbageTxOut address value datum script) = (address, value, datum, script)

suppliedDeposit :: CapacityDeposit
suppliedDeposit = CapacityDeposit (Coin 19)

negativeDeposit :: CapacityDeposit
negativeDeposit = CapacityDeposit (Coin (-1))

excessiveDeposit :: CapacityDeposit
excessiveDeposit = CapacityDeposit (Coin 62)

insufficientTotalCoins :: Coin
insufficientTotalCoins = Coin 18

replacementAssets :: ApplicationAssets
replacementAssets = applicationAssetsWithToken 81 11

replacementValue :: MaryValue
replacementValue = MaryValue (Coin 100) (nativeAssets replacementAssets)

replacementCoins :: Coin
replacementCoins = Coin 91

replacementAddress :: Addr
replacementAddress = Addr Mainnet (KeyHashObj (mkKeyHash 4)) StakeRefNull

replacementDatum :: Datum DijkstraEra
replacementDatum = DatumHash (mkDummySafeHash 9)

replacementScript :: Script DijkstraEra
replacementScript = fromPlutusScript (DijkstraPlutusV2 (alwaysSucceedsPlutus @'PlutusV2 1))

-- Private helpers

applicationAssetsWithToken :: Integer -> Integer -> ApplicationAssets
applicationAssetsWithToken coins quantity =
  ApplicationAssets $
    MaryValue (Coin coins) $
      MultiAsset $
        Map.singleton (PolicyID (mkScriptHash 3)) (Map.singleton (AssetName "token") quantity)

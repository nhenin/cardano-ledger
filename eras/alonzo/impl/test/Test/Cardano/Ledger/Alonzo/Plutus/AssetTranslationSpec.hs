{-# LANGUAGE OverloadedStrings #-}
-- This module deliberately checks the deprecated compatibility entry point.
{-# OPTIONS_GHC -Wno-deprecations #-}

module Test.Cardano.Ledger.Alonzo.Plutus.AssetTranslationSpec (spec) where

import qualified Cardano.Ledger.Alonzo.Plutus.TxInfo as Alonzo
import Cardano.Ledger.Hashes (ScriptHash (..))
import Cardano.Ledger.Mary.Mint (Forging (..))
import Cardano.Ledger.Mary.Value (AssetName (..), MultiAsset (..), PolicyID (..))
import qualified Cardano.Ledger.Plutus.AssetName.Translation as PlutusAssetName
import qualified Cardano.Ledger.Plutus.PolicyID.Translation as PlutusPolicyID
import qualified Cardano.Ledger.Plutus.Value.Translation as PlutusValue
import qualified Cardano.Ledger.Plutus.Value.Translation.V1V2 as V1V2
import qualified Cardano.Ledger.Plutus.Value.Translation.V3V4 as V3V4
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified PlutusLedgerApi.Common as P
import qualified PlutusLedgerApi.V3 as PV3
import Test.Cardano.Ledger.Common

spec :: Spec
spec = describe "Plutus asset translation" $ do
  forM_ fixtures $ \(scenarioName, forging, nativeEntries) -> describe scenarioName $ do
    -- Plutus Data equality observes map entries and their order, unlike
    -- algebraic Value equality, which is insufficient for compatibility.
    it "preserves exact native-only Value data" $
      PV3.toData (PlutusValue.fromLedgerMultiAsset (unForging forging))
        `shouldBe` P.Map (nativeData nativeEntries)

    it "preserves exact V1/V2 data with a leading zero-ada entry" $
      PV3.toData (V1V2.fromLedgerForging forging)
        `shouldBe` P.Map (adaEntry : nativeData nativeEntries)

    it "preserves exact V3/V4 data without an ada entry" $
      PV3.toData (V3V4.fromLedgerForging forging)
        `shouldBe` P.Map (nativeData nativeEntries)

    it "preserves the legacy Alonzo entry point's data" $
      PV3.toData (Alonzo.transMintValue (unForging forging))
        `shouldBe` PV3.toData (V1V2.fromLedgerForging forging)

    it "preserves the legacy native-map entry point's data" $
      PV3.toData (Alonzo.transMultiAsset (unForging forging))
        `shouldBe` PV3.toData (PlutusValue.fromLedgerMultiAsset (unForging forging))

  describe "PolicyID translation" $
    forM_ [("policy A", policyA, policyABytes), ("policy B", policyB, policyBBytes)] $
      \(scenarioName, policy, expectedBytes) -> describe scenarioName $ do
        it "preserves policy-hash bytes as a CurrencySymbol" $
          PV3.toData (PlutusPolicyID.fromLedgerPolicyID policy) `shouldBe` P.B expectedBytes
        it "preserves the legacy policy entry point's data" $
          PV3.toData (Alonzo.transPolicyID policy)
            `shouldBe` PV3.toData (PlutusPolicyID.fromLedgerPolicyID policy)

  describe "AssetName translation"
    $ forM_
      [ ("empty", AssetName "", BS.empty)
      , ("text", AssetName "a", "a")
      , ("binary", AssetName "\NUL\255", BS.pack [0, 255])
      ]
    $ \(scenarioName, assetName, expectedBytes) -> describe scenarioName $ do
      it "preserves asset-name bytes as a TokenName" $
        PV3.toData (PlutusAssetName.fromLedgerAssetName assetName) `shouldBe` P.B expectedBytes
      it "preserves the legacy asset-name entry point's data" $
        PV3.toData (Alonzo.transAssetName assetName)
          `shouldBe` PV3.toData (PlutusAssetName.fromLedgerAssetName assetName)

type NativeEntries = [(BS.ByteString, [(BS.ByteString, Integer)])]

fixtures :: [(String, Forging, NativeEntries)]
fixtures =
  [ ("empty mint", mint [], [])
  ,
    ( "positive mint"
    , mint [(policyA, [(AssetName "a", 10)])]
    , [(policyABytes, [("a", 10)])]
    )
  ,
    ( "negative burn"
    , mint [(policyA, [(AssetName "a", -3)])]
    , [(policyABytes, [("a", -3)])]
    )
  ,
    ( "mixed mint and burn in policy and asset-name order"
    , mint
        [ (policyB, [(AssetName "z", -3), (AssetName "a", 7)])
        , (policyA, [(AssetName "z", 2), (AssetName "a", -1)])
        ]
    ,
      [ (policyABytes, [("a", -1), ("z", 2)])
      , (policyBBytes, [("a", 7), ("z", -3)])
      ]
    )
  ,
    ( "zero native quantity"
    , mint [(policyA, [(AssetName "a", 0)])]
    , [(policyABytes, [("a", 0)])]
    )
  ,
    ( "empty native policy map"
    , mint [(policyA, [])]
    , [(policyABytes, [])]
    )
  ,
    ( "binary and empty native asset names"
    , mint [(policyA, [(AssetName "\NUL\255", -2), (AssetName "", 1)])]
    , [(policyABytes, [("", 1), (BS.pack [0, 255], -2)])]
    )
  ]

-- Construct raw maps intentionally: zero quantities and empty policy maps must
-- not be pruned by fixture construction before their translation is checked.
mint :: [(PolicyID, [(AssetName, Integer)])] -> Forging
mint entries =
  Forging $ MultiAsset $ Map.fromList [(policy, Map.fromList assets) | (policy, assets) <- entries]

policyA, policyB :: PolicyID
policyA = PolicyID (ScriptHash "00000000000000000000000000000000000000000000000000000000")
policyB = PolicyID (ScriptHash "01010101010101010101010101010101010101010101010101010101")

policyABytes, policyBBytes :: BS.ByteString
policyABytes = BS.replicate 28 0
policyBBytes = BS.replicate 28 1

adaEntry :: (P.Data, P.Data)
adaEntry = (P.B BS.empty, P.Map [(P.B BS.empty, P.I 0)])

nativeData :: NativeEntries -> [(P.Data, P.Data)]
nativeData entries =
  [ (P.B policy, P.Map [(P.B assetName, P.I quantity) | (assetName, quantity) <- assets])
  | (policy, assets) <- entries
  ]

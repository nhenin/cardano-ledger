{-# LANGUAGE OverloadedStrings #-}

-- | Raw Ledger fixtures and independent expected Plutus Data representations.
-- Data equality observes entries and ordering that algebraic Value equality
-- does not distinguish, so all translation specs compare Data directly.
module Test.Cardano.Ledger.Plutus.Value.Translation.Fixtures (
  assetNameFixtures,
  policyFixtures,
  nativeAssetFixtures,
  adaEntry,
  nativeData,
) where

import Cardano.Ledger.Hashes (ScriptHash (..))
import Cardano.Ledger.Mary.AssetName (AssetName (..))
import Cardano.Ledger.Mary.MultiAsset (MultiAsset (..))
import Cardano.Ledger.Mary.PolicyID (PolicyID (..))
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified PlutusLedgerApi.Common as P

type NativeEntries = [(BS.ByteString, [(BS.ByteString, Integer)])]

assetNameFixtures :: [(String, AssetName, BS.ByteString)]
assetNameFixtures =
  [ ("empty", AssetName "", BS.empty)
  , ("text", AssetName "a", "a")
  , ("binary", AssetName "\NUL\255", BS.pack [0, 255])
  ]

policyFixtures :: [(String, PolicyID, BS.ByteString)]
policyFixtures =
  [ ("policy A", policyA, policyABytes)
  , ("policy B", policyB, policyBBytes)
  ]

nativeAssetFixtures :: [(String, MultiAsset, NativeEntries)]
nativeAssetFixtures =
  [ ("empty native map", multiAsset [], [])
  ,
    ( "positive native quantity"
    , multiAsset [(policyA, [(AssetName "a", 10)])]
    , [(policyABytes, [("a", 10)])]
    )
  ,
    ( "negative native quantity"
    , multiAsset [(policyA, [(AssetName "a", -3)])]
    , [(policyABytes, [("a", -3)])]
    )
  ,
    ( "mixed signed quantities in policy and asset-name order"
    , multiAsset
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
    , multiAsset [(policyA, [(AssetName "a", 0)])]
    , [(policyABytes, [("a", 0)])]
    )
  ,
    ( "empty native policy map"
    , multiAsset [(policyA, [])]
    , [(policyABytes, [])]
    )
  ,
    ( "binary and empty native asset names"
    , multiAsset [(policyA, [(AssetName "\NUL\255", -2), (AssetName "", 1)])]
    , [(policyABytes, [("", 1), (BS.pack [0, 255], -2)])]
    )
  ]

-- Construct raw maps intentionally: zero quantities and empty policy maps must
-- not be pruned by fixture construction before their translation is checked.
multiAsset :: [(PolicyID, [(AssetName, Integer)])] -> MultiAsset
multiAsset entries =
  MultiAsset $ Map.fromList [(policy, Map.fromList assets) | (policy, assets) <- entries]

policyA, policyB :: PolicyID
policyA = PolicyID (ScriptHash "00000000000000000000000000000000000000000000000000000000")
policyB = PolicyID (ScriptHash "01010101010101010101010101010101010101010101010101010101")

policyABytes, policyBBytes :: BS.ByteString
policyABytes = BS.replicate 28 0
policyBBytes = BS.replicate 28 1

adaEntry :: Integer -> (P.Data, P.Data)
adaEntry amount = (P.B BS.empty, P.Map [(P.B BS.empty, P.I amount)])

nativeData :: NativeEntries -> [(P.Data, P.Data)]
nativeData entries =
  [ (P.B policy, P.Map [(P.B assetName, P.I quantity) | (assetName, quantity) <- assets])
  | (policy, assets) <- entries
  ]

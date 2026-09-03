{-# LANGUAGE BangPatterns #-}

-- | Native-asset adapter in the anti-corruption layer between the Ledger and
-- Plutus bounded contexts. 'PV1.Value' belongs to the Plutus script contract;
-- Ledger terminology and types may evolve independently.
--
-- Identifier and native-map representations are shared by V1-V4; @txInfoMint@
-- conventions belong to the versioned adapters. These modules live in the
-- Alonzo package, where Ledger asset types and Plutus dependencies already meet.
module Cardano.Ledger.Plutus.Value.Translation (
  fromLedgerMultiAsset,
) where

import Cardano.Ledger.Mary.Value (MultiAsset (..))
import qualified Cardano.Ledger.Plutus.AssetName.Translation as PlutusAssetName
import qualified Cardano.Ledger.Plutus.PolicyID.Translation as PlutusPolicyID
import qualified Data.Map.Strict as Map
import qualified PlutusLedgerApi.V1 as PV1
import qualified PlutusLedgerApi.V2 as PV2

-- | Preserve signed quantities, raw zero/empty entries and policy/name ordering.
-- No Ada entry is added by this conversion.
-- Scripts can observe these map details, even when algebraic 'PV1.Value'
-- equality would consider a normalized map equivalent.
fromLedgerMultiAsset :: MultiAsset -> PV1.Value
fromLedgerMultiAsset (MultiAsset m) =
  PV1.Value
    ( toAssocMap
        PlutusPolicyID.fromLedgerPolicyID
        (toAssocMap PlutusAssetName.fromLedgerAssetName id)
        m
    )
  where
    toAssocMap :: (k -> pk) -> (v -> pv) -> Map.Map k v -> PV2.Map pk pv
    toAssocMap transKey transVal =
      PV2.unsafeFromList . Map.foldrWithKey' accWithKey []
      where
        accWithKey key value !acc = (transKey key, transVal value) : acc

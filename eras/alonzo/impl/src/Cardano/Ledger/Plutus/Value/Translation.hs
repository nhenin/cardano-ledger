{-# LANGUAGE BangPatterns #-}

-- | Asset adapter in the anti-corruption layer between the Ledger and
-- Plutus bounded contexts. 'PV1.Value' belongs to the Plutus script contract;
-- Ledger terminology and types may evolve independently.
--
-- Value and identifier representations are shared by V1-V4; @txInfoMint@
-- conventions belong to the versioned adapters. These modules live in the
-- Alonzo package, where Ledger asset types and Plutus dependencies already meet.
module Cardano.Ledger.Plutus.Value.Translation (
  fromLedgerMaryValue,
  fromLedgerMultiAsset,
) where

import qualified Cardano.Ledger.Mary.MultiAsset as Mary (MultiAsset (..))
import qualified Cardano.Ledger.Mary.Value as Mary (MaryValue (..))
import qualified Cardano.Ledger.Plutus.AssetName.Translation as PlutusAssetName
import qualified Cardano.Ledger.Plutus.PolicyID.Translation as PlutusPolicyID
import Cardano.Ledger.Plutus.TxInfo (transCoinToValue)
import qualified Data.Map.Strict as Map
import qualified PlutusLedgerApi.V1 as PV1
import qualified PlutusLedgerApi.V2 as PV2

-- | Translate Ada and native assets, preserving the leading Ada entry even
-- when its quantity is zero, followed by the native map as-is. This conversion
-- does not validate output quantities. 'Mary.MaryValue' names the source
-- representation, not the era of the transaction.
-- Unlike V3/V4 @txInfoMint@, output values retain Ada in every version.
fromLedgerMaryValue :: Mary.MaryValue -> PV1.Value
fromLedgerMaryValue (Mary.MaryValue coin assets) = transCoinToValue coin <> fromLedgerMultiAsset assets

-- | Preserve signed quantities, raw zero/empty entries and policy/name ordering.
-- No Ada entry is added by this conversion.
-- Scripts can observe these map details, even when algebraic 'PV1.Value'
-- equality would consider a normalized map equivalent.
fromLedgerMultiAsset :: Mary.MultiAsset -> PV1.Value
fromLedgerMultiAsset (Mary.MultiAsset m) =
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

-- | Asset-name adapter in the anti-corruption layer between the Ledger and
-- Plutus bounded contexts. Ledger 'AssetName' and Plutus 'PV1.TokenName' have
-- independent vocabulary; their identifier bytes must remain unchanged for
-- scripts. V1-V4 share this representation.
module Cardano.Ledger.Plutus.AssetName.Translation (
  fromLedgerAssetName,
) where

import Cardano.Ledger.Mary.Value (AssetName (..))
import qualified Data.ByteString.Short as SBS
import qualified PlutusLedgerApi.V1 as PV1

-- | Preserve the Ledger asset-name bytes in a Plutus token name.
fromLedgerAssetName :: AssetName -> PV1.TokenName
fromLedgerAssetName (AssetName bs) = PV1.TokenName (PV1.toBuiltin (SBS.fromShort bs))

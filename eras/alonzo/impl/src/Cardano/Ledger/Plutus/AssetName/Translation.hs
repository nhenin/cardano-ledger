-- | Asset-name adapter in the anti-corruption layer between the Ledger and
-- Plutus bounded contexts. Ledger 'Mary.AssetName' and Plutus 'PV1.TokenName' have
-- independent vocabulary; their identifier bytes must remain unchanged for
-- scripts. V1-V4 share this representation.
module Cardano.Ledger.Plutus.AssetName.Translation (
  fromLedgerAssetName,
) where

import qualified Cardano.Ledger.Mary.AssetName as Mary (AssetName (..))
import qualified Data.ByteString.Short as SBS
import qualified PlutusLedgerApi.V1 as PV1

-- | Preserve the Ledger asset-name bytes in a Plutus token name.
fromLedgerAssetName :: Mary.AssetName -> PV1.TokenName
fromLedgerAssetName (Mary.AssetName bs) = PV1.TokenName (PV1.toBuiltin (SBS.fromShort bs))

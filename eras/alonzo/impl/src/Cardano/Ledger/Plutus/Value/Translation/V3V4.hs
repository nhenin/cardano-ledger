-- | Forging adapter in the anti-corruption layer between the Ledger and Plutus
-- bounded contexts. Ledger 'Mary.Forging' names both minting and burning; the Plutus
-- contract still calls this @txInfoMint@ and represents it with 'PV3.MintValue'.
-- V3/V4 share this representation regardless of the transaction's Ledger era.
module Cardano.Ledger.Plutus.Value.Translation.V3V4 (
  fromLedgerForging,
) where

import qualified Cardano.Ledger.Mary.Forging as Mary (Forging, unForging)
import qualified Cardano.Ledger.Plutus.Value.Translation as PlutusValue
import qualified PlutusLedgerApi.V1 as PV1
import qualified PlutusLedgerApi.V3.MintValue as PV3

-- | Translate signed native-asset changes, including burns, into @txInfoMint@.
-- Unlike the V1/V2 contract, this representation has no Ada entry.
fromLedgerForging :: Mary.Forging -> PV3.MintValue
fromLedgerForging = PV3.UnsafeMintValue . PV1.getValue . PlutusValue.fromLedgerMultiAsset . Mary.unForging

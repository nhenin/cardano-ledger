-- | Forging adapter in the anti-corruption layer between the Ledger and Plutus
-- bounded contexts. Ledger 'Forging' names both minting and burning; the Plutus
-- contract still calls this @txInfoMint@ and represents it with 'PV1.Value'.
-- V1/V2 share this representation regardless of the transaction's Ledger era.
module Cardano.Ledger.Plutus.Value.Translation.V1V2 (
  fromLedgerForging,
) where

import Cardano.Ledger.Mary.Mint (Forging (..))
import Cardano.Ledger.Plutus.TxInfo (transCoinToValue)
import qualified Cardano.Ledger.Plutus.Value.Translation as PlutusValue
import Cardano.Ledger.Val (zero)
import qualified PlutusLedgerApi.V1 as PV1

-- | Prepare @txInfoMint@ with its historical leading zero-Ada entry.
-- The entry is script-observable even though Ada cannot be forged; removing
-- it would change script data, not merely simplify an algebraic value.
-- Signed native quantities and their raw map entries are otherwise preserved.
fromLedgerForging :: Forging -> PV1.Value
fromLedgerForging (Forging m) = transCoinToValue zero <> PlutusValue.fromLedgerMultiAsset m

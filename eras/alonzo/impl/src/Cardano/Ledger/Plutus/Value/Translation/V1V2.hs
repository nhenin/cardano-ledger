-- | Forging adapter in the anti-corruption layer between the Ledger and Plutus
-- bounded contexts. Ledger 'Mary.Forging' names both minting and burning; the Plutus
-- contract still calls this @txInfoMint@ and represents it with 'PV1.Value'.
-- V1/V2 share this representation regardless of the transaction's Ledger era.
module Cardano.Ledger.Plutus.Value.Translation.V1V2 (
  fromLedgerForging,
) where

import qualified Cardano.Ledger.Mary.Forging as Mary (Forging (..))
import Cardano.Ledger.Plutus.TxInfo (transCoinToValue)
import qualified Cardano.Ledger.Plutus.Value.Translation as PlutusValue
import Cardano.Ledger.Val (zero)
import qualified PlutusLedgerApi.V1 as PV1

-- | Prepare @txInfoMint@ with its historical leading zero-Ada entry.
-- The Ledger mint field previously used @MaryValue@, whose translation included
-- Ada. It now stores native-only @MultiAsset@ because Ada cannot be forged,
-- but V1/V2 scripts still observe the old entry. Removing it could make
-- previously successful scripts fail, despite algebraic value equivalence.
-- Signed native quantities and their raw map entries are otherwise preserved.
fromLedgerForging :: Mary.Forging -> PV1.Value
fromLedgerForging (Mary.Forging m) = transCoinToValue zero <> PlutusValue.fromLedgerMultiAsset m

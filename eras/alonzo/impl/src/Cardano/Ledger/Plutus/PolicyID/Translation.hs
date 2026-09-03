-- | Policy adapter in the anti-corruption layer between the Ledger and Plutus
-- bounded contexts. Ledger 'Mary.PolicyID' and Plutus 'PV1.CurrencySymbol' have
-- independent vocabulary; their identifier bytes must remain unchanged for
-- scripts. V1-V4 share this representation.
module Cardano.Ledger.Plutus.PolicyID.Translation (
  fromLedgerPolicyID,
) where

import Cardano.Crypto.Hash.Class (hashToBytes)
import Cardano.Ledger.Hashes (ScriptHash (..))
import qualified Cardano.Ledger.Mary.PolicyID as Mary (PolicyID (..))
import qualified PlutusLedgerApi.V1 as PV1

-- | Preserve the Ledger policy's script-hash bytes in a Plutus currency symbol.
fromLedgerPolicyID :: Mary.PolicyID -> PV1.CurrencySymbol
fromLedgerPolicyID (Mary.PolicyID (ScriptHash x)) = PV1.CurrencySymbol (PV1.toBuiltin (hashToBytes x))

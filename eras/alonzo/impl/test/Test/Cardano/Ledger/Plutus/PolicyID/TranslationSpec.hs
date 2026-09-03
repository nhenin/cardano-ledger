module Test.Cardano.Ledger.Plutus.PolicyID.TranslationSpec (spec) where

import qualified Cardano.Ledger.Plutus.PolicyID.Translation as PlutusPolicyID
import qualified PlutusLedgerApi.Common as P
import qualified PlutusLedgerApi.V3 as PV3
import Test.Cardano.Ledger.Common
import qualified Test.Cardano.Ledger.Plutus.Value.Translation.Fixtures as Fixture

spec :: Spec
spec = describe "PolicyID translation" $
  forM_ Fixture.policy $ \(scenarioName, policy, expectedBytes) ->
    describe scenarioName $
      it "preserves policy-hash bytes as a CurrencySymbol" $
        PV3.toData (PlutusPolicyID.fromLedgerPolicyID policy) `shouldBe` P.B expectedBytes

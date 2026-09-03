module Test.Cardano.Ledger.Plutus.Value.Translation.V3V4Spec (spec) where

import Cardano.Ledger.Mary.Forging (Forging (..))
import qualified Cardano.Ledger.Plutus.Value.Translation.V3V4 as PlutusV3V4
import qualified PlutusLedgerApi.Common as P
import qualified PlutusLedgerApi.V3 as PV3
import Test.Cardano.Ledger.Common
import qualified Test.Cardano.Ledger.Plutus.Value.Translation.Fixtures as Fixture

spec :: Spec
spec = describe "Value translation V3/V4" $
  forM_ Fixture.nativeAsset $ \(scenarioName, nativeAssets, nativeEntries) ->
    describe scenarioName $
      it "preserves exact forging data without an ada entry" $
        PV3.toData (PlutusV3V4.fromLedgerForging (Forging nativeAssets))
          `shouldBe` P.Map (Fixture.nativeData nativeEntries)

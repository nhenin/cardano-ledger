module Test.Cardano.Ledger.Plutus.Value.Translation.V3V4Spec (spec) where

import Cardano.Ledger.Mary.Mint (Forging (..))
import qualified Cardano.Ledger.Plutus.Value.Translation.V3V4 as PlutusV3V4
import qualified PlutusLedgerApi.Common as P
import qualified PlutusLedgerApi.V3 as PV3
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Plutus.Value.Translation.Fixtures (
  nativeAssetFixtures,
  nativeData,
 )

spec :: Spec
spec = describe "Value translation V3/V4" $
  forM_ nativeAssetFixtures $ \(scenarioName, nativeAssets, nativeEntries) ->
    describe scenarioName $
      it "preserves exact forging data without an ada entry" $
        PV3.toData (PlutusV3V4.fromLedgerForging (Forging nativeAssets))
          `shouldBe` P.Map (nativeData nativeEntries)

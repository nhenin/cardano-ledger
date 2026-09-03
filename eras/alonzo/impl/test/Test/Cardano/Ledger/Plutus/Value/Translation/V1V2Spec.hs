module Test.Cardano.Ledger.Plutus.Value.Translation.V1V2Spec (spec) where

import Cardano.Ledger.Mary.Mint (Forging (..))
import qualified Cardano.Ledger.Plutus.Value.Translation.V1V2 as PlutusV1V2
import qualified PlutusLedgerApi.Common as P
import qualified PlutusLedgerApi.V3 as PV3
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Plutus.Value.Translation.Fixtures (
  adaEntry,
  nativeAssetFixtures,
  nativeData,
 )

spec :: Spec
spec = describe "Value translation V1/V2" $
  forM_ nativeAssetFixtures $ \(scenarioName, nativeAssets, nativeEntries) ->
    describe scenarioName $
      it "preserves exact forging data with a leading zero-ada entry" $
        PV3.toData (PlutusV1V2.fromLedgerForging (Forging nativeAssets))
          `shouldBe` P.Map (adaEntry 0 : nativeData nativeEntries)

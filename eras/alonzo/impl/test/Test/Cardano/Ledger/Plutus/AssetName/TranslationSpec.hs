module Test.Cardano.Ledger.Plutus.AssetName.TranslationSpec (spec) where

import qualified Cardano.Ledger.Plutus.AssetName.Translation as PlutusAssetName
import qualified PlutusLedgerApi.Common as P
import qualified PlutusLedgerApi.V3 as PV3
import Test.Cardano.Ledger.Common
import qualified Test.Cardano.Ledger.Plutus.Value.Translation.Fixtures as Fixture

spec :: Spec
spec = describe "AssetName translation" $
  forM_ Fixture.assetName $ \(scenarioName, assetName, expectedBytes) ->
    describe scenarioName $
      it "preserves asset-name bytes as a TokenName" $
        PV3.toData (PlutusAssetName.fromLedgerAssetName assetName) `shouldBe` P.B expectedBytes

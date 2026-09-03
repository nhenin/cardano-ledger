module Test.Cardano.Ledger.Plutus.Value.TranslationSpec (spec) where

import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Mary.Value (MaryValue (..))
import qualified Cardano.Ledger.Plutus.Value.Translation as PlutusValue
import qualified PlutusLedgerApi.Common as P
import qualified PlutusLedgerApi.V3 as PV3
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Plutus.Value.Translation.Fixtures (
  adaEntry,
  nativeAssetFixtures,
  nativeData,
 )

spec :: Spec
spec = describe "Value translation" $
  forM_ nativeAssetFixtures $ \(scenarioName, nativeAssets, nativeEntries) -> describe scenarioName $ do
    it "preserves exact native-only Value data" $
      PV3.toData (PlutusValue.fromLedgerMultiAsset nativeAssets)
        `shouldBe` P.Map (nativeData nativeEntries)

    -- These are representation tests, not assertions that every fixture is a
    -- valid output. Signed quantities and raw zero/empty entries deliberately
    -- exercise lossless conversion without introducing output validation.
    forM_ [0, 42] $ \adaAmount ->
      it ("preserves exact MaryValue data with leading Ada " <> show adaAmount) $
        PV3.toData (PlutusValue.fromLedgerMaryValue (MaryValue (Coin adaAmount) nativeAssets))
          `shouldBe` P.Map (adaEntry adaAmount : nativeData nativeEntries)

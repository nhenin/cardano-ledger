{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Dijkstra.UTxO.TranslationSpec (spec) where

import Cardano.Ledger.Binary (serialize')
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Compactible (toCompactPartial)
import Cardano.Ledger.Core (coinTxOutL, eraProtVerLow, translateEra')
import Cardano.Ledger.Dijkstra ()
import Cardano.Ledger.Dijkstra.Core (ppCoinsPerUTxOByteL)
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Dijkstra.TxOut (CapacityDepositForm (..), DijkstraEraTxOut (..))
import Cardano.Ledger.Dijkstra.TxOut.CapacityDeposit (CapacityDeposit (..))
import Cardano.Ledger.Dijkstra.UTxO.Translation (fundCapacityDeposits)
import Cardano.Ledger.Shelley.LedgerState (UTxOState (..))
import Cardano.Ledger.State (EraGov (..), EraStake (..), UTxO (..), sumCoinUTxO, sumUTxO)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Lens.Micro ((^.))
import Test.Cardano.Ledger.Common hiding (output)
import qualified Test.Cardano.Ledger.Dijkstra.UTxO.Translation.Fixture as Fixture

spec :: Spec
spec = describe "Dijkstra UTxO state migration" $ do
  it "retains the translated governance state's current capacity price" migrationRetainsCurrentPrice
  it "allocates capacity at the current price instead of the previous price" migrationUsesCurrentPrice
  it "preserves total UTxO ADA" migrationPreservesTotalCoins
  it "preserves the full UTxO asset accounting projection" migrationPreservesAssets
  it "preserves the historical output's key" migrationPreservesOutputKey
  it "stores the migrated output with an explicit allocation" migrationStoresExplicitAllocation
  it "rebuilds instant stake from application ADA" migrationRebuildsApplicationStake
  it "does not retain the historical merged-ADA stake cache" migrationReplacesHistoricalStake
  it "removes all migrated stake when the migrated UTxO is deleted" migrationStakeCanBeDeleted
  it "preserves the certificate deposit pot" migrationPreservesCertificateDeposits
  it "preserves the fee pot" migrationPreservesFees
  it "preserves the donation pot" migrationPreservesDonations
  it "does not reprice an already explicit migrated allocation" migratedAllocationIsNotRepriced

migrationRetainsCurrentPrice :: Expectation
migrationRetainsCurrentPrice = do
  -- Setup
  let original = Fixture.historicalState
  -- Exercise
  let migrated = translateEra' @DijkstraEra Fixture.migrationContext original
  -- Verify
  utxosGovState migrated
    ^. curPParamsGovStateL
      . ppCoinsPerUTxOByteL
      `shouldBe` Fixture.currentPrice

migrationUsesCurrentPrice :: Expectation
migrationUsesCurrentPrice = do
  -- Setup
  let original = Fixture.historicalState
  -- Exercise
  let migrated = translateEra' @DijkstraEra Fixture.migrationContext original
  output <- expectJust (Map.lookup Fixture.outputKey (unUTxO (utxosUtxo migrated)))
  -- Verify: price the actual explicit bytes independently of the production
  -- requirement helper. The fixture's previous price is 1, its current is 4310.
  output
    ^. capacityDepositTxOutL
      `shouldBe` CapacityDeposit
        (Coin ((160 + fromIntegral (BS.length (serialize' (eraProtVerLow @DijkstraEra) output))) * 4310))

migrationPreservesTotalCoins :: Expectation
migrationPreservesTotalCoins = do
  -- Setup
  let original = Fixture.historicalState
  -- Exercise
  let migrated = translateEra' @DijkstraEra Fixture.migrationContext original
  -- Verify
  sumCoinUTxO (utxosUtxo migrated) `shouldBe` Fixture.originalCoins

migrationPreservesAssets :: Expectation
migrationPreservesAssets = do
  -- Setup
  let original = Fixture.historicalState
  -- Exercise
  let migrated = translateEra' @DijkstraEra Fixture.migrationContext original
  -- Verify
  sumUTxO (utxosUtxo migrated) `shouldBe` sumUTxO (utxosUtxo original)

migrationPreservesOutputKey :: Expectation
migrationPreservesOutputKey = do
  -- Setup
  let original = Fixture.historicalState
  -- Exercise
  let migrated = translateEra' @DijkstraEra Fixture.migrationContext original
  -- Verify
  Map.keysSet (unUTxO (utxosUtxo migrated)) `shouldBe` Map.keysSet (unUTxO (utxosUtxo original))

migrationStoresExplicitAllocation :: Expectation
migrationStoresExplicitAllocation = do
  -- Setup
  let original = Fixture.historicalState
  -- Exercise
  let migrated = translateEra' @DijkstraEra Fixture.migrationContext original
  output <- expectJust (Map.lookup Fixture.outputKey (unUTxO (utxosUtxo migrated)))
  -- Verify
  output ^. capacityDepositFormTxOutL `shouldBe` ExplicitCapacityDeposit

migrationRebuildsApplicationStake :: Expectation
migrationRebuildsApplicationStake = do
  -- Setup
  let original = Fixture.historicalState
  -- Exercise
  let migrated = translateEra' @DijkstraEra Fixture.migrationContext original
  output <- expectJust (Map.lookup Fixture.outputKey (unUTxO (utxosUtxo migrated)))
  -- Verify
  utxosInstantStake migrated
    ^. instantStakeCredentialsL
      `shouldBe` Map.singleton Fixture.stakingCredential (toCompactPartial (output ^. coinTxOutL))

migrationReplacesHistoricalStake :: Expectation
migrationReplacesHistoricalStake = do
  -- Setup
  let original = Fixture.historicalState
  -- Exercise
  let migrated = translateEra' @DijkstraEra Fixture.migrationContext original
  -- Verify
  utxosInstantStake migrated
    ^. instantStakeCredentialsL
      `shouldNotBe` (utxosInstantStake original ^. instantStakeCredentialsL)

migrationStakeCanBeDeleted :: Expectation
migrationStakeCanBeDeleted = do
  -- Setup
  let original = Fixture.historicalState
  -- Exercise
  let migrated = translateEra' @DijkstraEra Fixture.migrationContext original
      remainingStake = deleteInstantStake (utxosUtxo migrated) (utxosInstantStake migrated)
  -- Verify
  remainingStake ^. instantStakeCredentialsL `shouldBe` Map.empty

migrationPreservesCertificateDeposits :: Expectation
migrationPreservesCertificateDeposits = do
  -- Setup
  let original = Fixture.historicalState
  -- Exercise
  let migrated = translateEra' @DijkstraEra Fixture.migrationContext original
  -- Verify
  utxosDeposited migrated `shouldBe` Coin 3

migrationPreservesFees :: Expectation
migrationPreservesFees = do
  -- Setup
  let original = Fixture.historicalState
  -- Exercise
  let migrated = translateEra' @DijkstraEra Fixture.migrationContext original
  -- Verify
  utxosFees migrated `shouldBe` Coin 5

migrationPreservesDonations :: Expectation
migrationPreservesDonations = do
  -- Setup
  let original = Fixture.historicalState
  -- Exercise
  let migrated = translateEra' @DijkstraEra Fixture.migrationContext original
  -- Verify
  utxosDonation migrated `shouldBe` Coin 7

migratedAllocationIsNotRepriced :: Expectation
migratedAllocationIsNotRepriced = do
  -- Setup
  let migrated = translateEra' @DijkstraEra Fixture.migrationContext Fixture.historicalState
      allocated = utxosUtxo migrated
  -- Exercise
  let reinserted = fundCapacityDeposits Fixture.repricedPParams allocated
  -- Verify
  reinserted `shouldBe` allocated

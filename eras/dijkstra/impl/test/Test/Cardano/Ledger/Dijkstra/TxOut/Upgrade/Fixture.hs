module Test.Cardano.Ledger.Dijkstra.TxOut.Upgrade.Fixture (
  policyPrices,
  pricedPParams,
  outputSizeScenarios,
  recoverCapacityDepositAtPrice,
  capacityDepositAtPrice,
  fundedOutput,
  underfundedOutput,
  recoverNegativeCapacityDeposit,
  underfundedAllocationError,
  negativeAllocationError,
  allocationInvariantViolation,
) where

import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Core (RecoverCapacityDeposit, coinTxOutL)
import Cardano.Ledger.Dijkstra.TxOut.CapacityDeposit (CapacityDeposit (..))
import Cardano.Ledger.Dijkstra.TxOut.Value.Translation (
  AllocationError (..),
  requiredCapacityDeposit,
 )
import Data.Word (Word64)
import Lens.Micro ((^.))
import Test.Cardano.Ledger.Dijkstra.TxOut.Value.Translation.Fixture (
  capacityDepositAtPrice,
  fundedOutput,
  outputSizeScenarios,
  pricedPParams,
  underfundedOutput,
 )

policyPrices :: [Word64]
policyPrices = [4310, 8620]

recoverCapacityDepositAtPrice :: Word64 -> RecoverCapacityDeposit ConwayEra
recoverCapacityDepositAtPrice = requiredCapacityDeposit . pricedPParams

recoverNegativeCapacityDeposit :: RecoverCapacityDeposit ConwayEra
recoverNegativeCapacityDeposit =
  CapacityDeposit . Coin . negate . unCoin . unCapacityDeposit . recoverCapacityDepositAtPrice 4310

underfundedAllocationError :: AllocationError
underfundedAllocationError =
  CapacityDepositExceedsOutputCoins
    (underfundedOutput ^. coinTxOutL)
    (capacityDepositAtPrice 4310 underfundedOutput)

negativeAllocationError :: AllocationError
negativeAllocationError =
  NegativeCapacityDeposit (recoverNegativeCapacityDeposit fundedOutput)

allocationInvariantViolation :: AllocationError -> String
allocationInvariantViolation allocationError =
  "Dijkstra.upgradeTxOut: capacity allocation invariant violated: " <> show allocationError

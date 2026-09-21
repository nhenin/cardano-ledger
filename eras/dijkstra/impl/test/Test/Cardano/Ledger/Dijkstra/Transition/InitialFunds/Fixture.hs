{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Dijkstra.Transition.InitialFunds.Fixture (
  FundingCase (..),
  oneLovelaceSurplus,
  applicationEncodingBoundary,
  applicationCoinsAt32BitBoundary,
  fundingBelowEndpointEstimates,
  fundedCases,
  smallFundingCases,
  zeroPriceFunds,
  underfundedCases,
  outsideCoinRange,
  minimumDeposit,
  firstSufficientDeposit,
) where

import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Babbage.PParams (ppCoinsPerUTxOByteL)
import Cardano.Ledger.BaseTypes (Network (..), ProtVer (..), inject)
import Cardano.Ledger.Coin (Coin (..), CoinPerByte (..), CompactForm (..))
import Cardano.Ledger.Core (EraTxOut (..), PParams, emptyPParams, eraProtVerLow, ppProtocolVersionL)
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Dijkstra.TxOut.CapacityDeposit (CapacityDeposit (..))
import Cardano.Ledger.Dijkstra.TxOut.LedgerInstances ()
import Cardano.Ledger.Dijkstra.TxOut.Value (OutputValue (..))
import Data.List (find)
import Data.Word (Word64)
import Lens.Micro ((&), (.~))
import Test.Cardano.Ledger.Shelley.Examples (mkKeyHash)

data FundingCase = FundingCase
  { protocolParameters :: PParams DijkstraEra
  , address :: Addr
  , amount :: Coin
  }

oneLovelaceSurplus :: FundingCase
oneLovelaceSurplus = baseAddressFunding 1 483

applicationEncodingBoundary :: FundingCase
applicationEncodingBoundary = baseAddressFunding 4310 1061146

applicationCoinsAt32BitBoundary :: FundingCase
applicationCoinsAt32BitBoundary = baseAddressFunding 4310 4295971526

fundingBelowEndpointEstimates :: FundingCase
fundingBelowEndpointEstimates = baseAddressFunding 287 65536

fundedCases :: [(String, FundingCase)]
fundedCases =
  [ ("one lovelace above the required deposit", oneLovelaceSurplus)
  , ("application coins crossing the 65536 encoding boundary", applicationEncodingBoundary)
  , ("funding below both endpoint estimates", fundingBelowEndpointEstimates)
  , ("application coins crossing the 32-bit encoding boundary", applicationCoinsAt32BitBoundary)
  , ("the maximum representable coin amount", baseAddressFunding 4310 maximumCoins)
  ]

smallFundingCases :: [(String, FundingCase)]
smallFundingCases =
  [ (addressName <> ", price " <> show price <> ", amount " <> show coins, fundingCase)
  | (addressName, fundingAddress) <-
      [("base address", baseAddress), ("enterprise address", enterpriseAddress)]
  , price <- [0, 1, 2]
  , coins <- [0, 23, 24, 199, 200, 226, 227, 228, 255, 256, 483, 484, 511, 512]
  , let fundingCase = FundingCase (pricedPParams price) fundingAddress (Coin coins)
  ]

zeroPriceFunds :: [(String, FundingCase)]
zeroPriceFunds =
  [ ("zero coins", baseAddressFunding 0 0)
  , ("ordinary funding", baseAddressFunding 0 483)
  , ("the maximum representable coin amount", baseAddressFunding 0 maximumCoins)
  ]

underfundedCases :: [(String, FundingCase)]
underfundedCases =
  [ ("ordinary funding below the minimum", baseAddressFunding 1 100)
  , ("a required deposit above Word64", baseAddressFunding maxBound maximumCoins)
  ]

outsideCoinRange :: [(String, FundingCase)]
outsideCoinRange =
  [ ("negative coins", baseAddressFunding 0 (-1))
  , ("coins beyond Word64", baseAddressFunding 0 (maximumCoins + 1))
  ]

minimumDeposit :: FundingCase -> OutputValue -> CapacityDeposit
minimumDeposit funding =
  CapacityDeposit
    . getMinCoinTxOut (protocolParameters funding)
    . mkBasicTxOut (address funding)

-- Enumerate actual outputs rather than duplicating the allocator's search.
-- This oracle is used only for the small funding amounts above.
firstSufficientDeposit :: FundingCase -> Maybe CapacityDeposit
firstSufficientDeposit funding =
  find coversMinimum [CapacityDeposit (Coin deposit) | deposit <- [0 .. coins]]
  where
    Coin coins = amount funding
    coversMinimum deposit@(CapacityDeposit (Coin deposited)) =
      deposit >= minimumDeposit funding (OutputValue deposit (inject (Coin (coins - deposited))))

-- Private helpers

baseAddressFunding :: Word64 -> Integer -> FundingCase
baseAddressFunding price = FundingCase (pricedPParams price) baseAddress . Coin

pricedPParams :: Word64 -> PParams DijkstraEra
pricedPParams price =
  emptyPParams
    & ppCoinsPerUTxOByteL .~ CoinPerByte (CompactCoin price)
    & ppProtocolVersionL .~ ProtVer (eraProtVerLow @DijkstraEra) 0

baseAddress :: Addr
baseAddress = Addr Testnet (KeyHashObj (mkKeyHash 1)) (StakeRefBase (KeyHashObj (mkKeyHash 2)))

enterpriseAddress :: Addr
enterpriseAddress = Addr Testnet (KeyHashObj (mkKeyHash 1)) StakeRefNull

maximumCoins :: Integer
maximumCoins = toInteger (maxBound :: Word64)

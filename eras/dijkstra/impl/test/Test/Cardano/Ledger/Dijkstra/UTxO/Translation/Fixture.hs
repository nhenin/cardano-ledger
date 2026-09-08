{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Dijkstra.UTxO.Translation.Fixture (
  historicalState,
  migrationContext,
  outputKey,
  stakingCredential,
  currentPrice,
  originalCoins,
  repricedPParams,
) where

import Cardano.Ledger.BaseTypes (Network (Testnet))
import Cardano.Ledger.Coin (Coin (..), CompactForm (CompactCoin))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Credential (Credential (KeyHashObj), StakeReference (StakeRefBase))
import Cardano.Ledger.Dijkstra ()
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Dijkstra.Genesis (DijkstraGenesis)
import Cardano.Ledger.Mary.AssetName (AssetName (..))
import Cardano.Ledger.Mary.MultiAsset (MultiAsset (..))
import Cardano.Ledger.Mary.PolicyID (PolicyID (..))
import Cardano.Ledger.Mary.Value (MaryValue (..))
import Cardano.Ledger.Shelley.LedgerState (UTxOState, smartUTxOState)
import Cardano.Ledger.State (txouts)
import Cardano.Ledger.TxIn (TxIn (..))
import qualified Data.Map.Strict as Map
import GHC.Exts (fromList)
import Lens.Micro ((&), (.~))
import Test.Cardano.Ledger.Dijkstra.Examples (exampleDijkstraGenesis)

migrationContext :: DijkstraGenesis
migrationContext = exampleDijkstraGenesis

currentPrice :: CoinPerByte
currentPrice = CoinPerByte (CompactCoin 4310)

originalCoins :: Coin
originalCoins = Coin 5000000

stakingCredential :: Credential Staking
stakingCredential =
  KeyHashObj (KeyHash "02020202020202020202020202020202020202020202020202020202")

outputAddress :: Addr
outputAddress =
  Addr
    Testnet
    (KeyHashObj (KeyHash "00000000000000000000000000000000000000000000000000000000"))
    (StakeRefBase stakingCredential)

historicalOutput :: TxOut ConwayEra
historicalOutput =
  mkBasicTxOut outputAddress $
    MaryValue
      originalCoins
      ( MultiAsset $
          Map.singleton
            (PolicyID (ScriptHash "01010101010101010101010101010101010101010101010101010101"))
            (Map.singleton (AssetName "token") 100)
      )

historicalBody :: TxBody TopTx ConwayEra
historicalBody = mkBasicTxBody & outputsTxBodyL .~ fromList [historicalOutput]

outputKey :: TxIn
outputKey = TxIn (txIdTxBody historicalBody) minBound

historicalPParams :: PParams ConwayEra
historicalPParams = emptyPParams & ppCoinsPerUTxOByteL .~ currentPrice

historicalGovernance :: GovState ConwayEra
historicalGovernance =
  emptyGovState
    & curPParamsGovStateL
      .~ historicalPParams
    & prevPParamsGovStateL
      .~ (emptyPParams & ppCoinsPerUTxOByteL .~ CoinPerByte (CompactCoin 1))

-- The base staking reference contributes to the instant-stake cache; account
-- registration belongs to certificate state and is not part of UTxOState.
historicalState :: UTxOState ConwayEra
historicalState =
  smartUTxOState
    historicalPParams
    (txouts historicalBody)
    (Coin 3)
    (Coin 5)
    historicalGovernance
    (Coin 7)

repricedPParams :: PParams DijkstraEra
repricedPParams = emptyPParams & ppCoinsPerUTxOByteL .~ CoinPerByte (CompactCoin 9000)

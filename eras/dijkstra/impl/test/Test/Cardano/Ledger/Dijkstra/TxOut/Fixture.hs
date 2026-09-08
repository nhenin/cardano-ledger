{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Dijkstra.TxOut.Fixture (
  splitOutput,
  otherAllocation,
  explicitZeroOutput,
  implicitOutput,
  pricedPParams,
  zeroApplicationOutput,
) where

import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Babbage.Core (CoinPerByte (..), ppCoinsPerUTxOByteL)
import Cardano.Ledger.BaseTypes (Network (Testnet), StrictMaybe (SNothing))
import Cardano.Ledger.Binary (serialize')
import Cardano.Ledger.Coin (Coin (..), CompactForm (CompactCoin))
import Cardano.Ledger.Core (
  EraTxOut (mkBasicTxOut),
  PParams,
  emptyPParams,
  eraProtVerLow,
 )
import Cardano.Ledger.Credential (Credential (KeyHashObj), StakeReference (StakeRefNull))
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Dijkstra.TxOut (
  DijkstraEraTxOut (capacityDepositTxOutL),
  DijkstraTxOut,
  mkDijkstraTxOut,
 )
import Cardano.Ledger.Dijkstra.TxOut.ApplicationAssets (ApplicationAssets (..))
import Cardano.Ledger.Dijkstra.TxOut.CapacityDeposit (CapacityDeposit (..))
import Cardano.Ledger.Dijkstra.TxOut.Value (OutputValue (..))
import Cardano.Ledger.Hashes (ScriptHash (..))
import Cardano.Ledger.Keys (KeyHash (..))
import Cardano.Ledger.Mary.AssetName (AssetName (..))
import Cardano.Ledger.Mary.MultiAsset (MultiAsset (..))
import Cardano.Ledger.Mary.PolicyID (PolicyID (..))
import Cardano.Ledger.Mary.Value (MaryValue (..))
import Cardano.Ledger.Plutus.Data (Datum (NoDatum))
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Lens.Micro ((&), (.~))

address :: Addr
address =
  Addr
    Testnet
    (KeyHashObj (KeyHash "00000000000000000000000000000000000000000000000000000000"))
    StakeRefNull

assets :: Coin -> ApplicationAssets
assets coins =
  ApplicationAssets
    coins
    ( MultiAsset $
        Map.singleton
          (PolicyID (ScriptHash "01010101010101010101010101010101010101010101010101010101"))
          (Map.singleton (AssetName "token") 100)
    )

splitOutput :: DijkstraTxOut DijkstraEra
splitOutput =
  mkDijkstraTxOut address (OutputValue (CapacityDeposit (Coin 7)) (assets (Coin 13))) NoDatum SNothing

otherAllocation :: DijkstraTxOut DijkstraEra
otherAllocation =
  mkDijkstraTxOut address (OutputValue (CapacityDeposit (Coin 9)) (assets (Coin 11))) NoDatum SNothing

explicitZeroOutput :: DijkstraTxOut DijkstraEra
explicitZeroOutput =
  mkDijkstraTxOut address (OutputValue (CapacityDeposit (Coin 0)) (assets (Coin 20))) NoDatum SNothing

implicitOutput :: DijkstraTxOut DijkstraEra
implicitOutput = mkBasicTxOut address (MaryValue (Coin 20) (nativeAssets (assets (Coin 20))))

pricedPParams :: PParams DijkstraEra
pricedPParams = emptyPParams & ppCoinsPerUTxOByteL .~ CoinPerByte (CompactCoin 4310)

-- Price independently of the minimum-coin API under test. Both the provisional
-- and resulting deposits use five-byte CBOR integers, so this output is exact
-- after one assignment. The spec separately verifies that premise.
zeroApplicationOutput :: DijkstraTxOut DijkstraEra
zeroApplicationOutput =
  provisional
    & capacityDepositTxOutL
      .~ CapacityDeposit
        (Coin ((160 + fromIntegral (BS.length (serialize' (eraProtVerLow @DijkstraEra) provisional))) * 4310))
  where
    provisional :: DijkstraTxOut DijkstraEra
    provisional =
      mkDijkstraTxOut
        address
        (OutputValue (CapacityDeposit (Coin 1000000)) (assets (Coin 0)))
        NoDatum
        SNothing

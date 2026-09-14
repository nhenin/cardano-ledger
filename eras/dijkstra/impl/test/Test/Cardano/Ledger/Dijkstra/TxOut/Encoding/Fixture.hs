{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Dijkstra.TxOut.Encoding.Fixture (
  outputCases,
  protocolVersion,
  sharedCredentials,
  withoutCapacityDeposit,
  legacyMemPackBytes,
) where

import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.BaseTypes (Version)
import Cardano.Ledger.Binary (
  EncCBOR (encCBOR),
  Interns,
  encodeMapLen,
  encodeMemPack,
  internsFromSet,
  serialize,
 )
import Cardano.Ledger.Core (eraProtVerLow)
import Cardano.Ledger.Credential (Credential, StakeReference (..))
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.TxOut (
  DijkstraTxOut (DijkstraTxOut),
  fromBabbageTxOut,
  toBabbageTxOut,
 )
import Cardano.Ledger.Dijkstra.TxOut.Value (OutputValue (..))
import Cardano.Ledger.Keys (KeyRole (Staking))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Set as Set
import qualified Test.Cardano.Ledger.Dijkstra.TxOut.Allocation.Fixture as Allocation

outputCases :: [(String, DijkstraTxOut)]
outputCases =
  [ (name, fromBabbageTxOut Allocation.suppliedDeposit (Allocation.applicationProjection fixture))
  | (name, fixture) <- Allocation.allocationCases
  ]

protocolVersion :: Version
protocolVersion = eraProtVerLow @DijkstraEra

sharedCredentials :: DijkstraTxOut -> Interns (Credential Staking)
sharedCredentials (DijkstraTxOut (Addr _ _ (StakeRefBase credential)) _ _ _) =
  internsFromSet (Set.singleton credential)
sharedCredentials _ = mempty

withoutCapacityDeposit :: DijkstraTxOut -> LBS.ByteString
withoutCapacityDeposit (DijkstraTxOut address (OutputValue _ assets) _ _) =
  serialize protocolVersion $
    encodeMapLen 2
      <> encCBOR (0 :: Word)
      <> encCBOR address
      <> encCBOR (1 :: Word)
      <> encCBOR assets

legacyMemPackBytes :: DijkstraTxOut -> LBS.ByteString
legacyMemPackBytes = serialize protocolVersion . encodeMemPack . toBabbageTxOut

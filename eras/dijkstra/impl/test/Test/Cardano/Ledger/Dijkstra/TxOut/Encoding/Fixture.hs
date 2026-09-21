{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Dijkstra.TxOut.Encoding.Fixture (
  outputCases,
  protocolVersion,
  sharedCredentials,
  withoutCapacityDeposit,
  legacyMemPackBytes,
  allocatedApplicationMemPackBytes,
) where

import Cardano.Ledger.Address (Addr (..))
import qualified Cardano.Ledger.Babbage.TxOut as Babbage
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
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Dijkstra.TxOut (
  DijkstraTxOut (DijkstraTxOut),
  toBabbageTxOut,
 )
import Cardano.Ledger.Dijkstra.TxOut.ApplicationAssets (ApplicationAssets (..))
import Cardano.Ledger.Dijkstra.TxOut.CapacityDeposit (CapacityDeposit)
import Cardano.Ledger.Dijkstra.TxOut.Value (OutputValue (..))
import Cardano.Ledger.Keys (KeyRole (Staking))
import qualified Data.ByteString.Lazy as LBS
import Data.MemPack (MemPack (..), packTagM, packedTagByteCount, unknownTagM, unpackTagM)
import qualified Data.Set as Set
import qualified Test.Cardano.Ledger.Dijkstra.TxOut.Allocation.Fixture as Allocation

outputCases :: [(String, DijkstraTxOut)]
outputCases =
  [ (name, Allocation.allocatedOutput fixture)
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

-- | The existing layout stores the deposit followed by the application's
-- compact payload. It must not change when the total-value adapters change.
allocatedApplicationMemPackBytes :: DijkstraTxOut -> LBS.ByteString
allocatedApplicationMemPackBytes (DijkstraTxOut address (OutputValue deposit (ApplicationAssets assets)) datum script) =
  serialize protocolVersion $
    encodeMemPack $
      StoredAllocation deposit (Babbage.BabbageTxOut address assets datum script)

-- Private helpers

data StoredAllocation = StoredAllocation CapacityDeposit (Babbage.BabbageTxOut DijkstraEra)

instance MemPack StoredAllocation where
  packedByteCount (StoredAllocation deposit applicationOutput) =
    packedTagByteCount + packedByteCount deposit + packedByteCount applicationOutput
  packM (StoredAllocation deposit applicationOutput) =
    packTagM 6 >> packM deposit >> packM applicationOutput
  unpackM = do
    tag <- unpackTagM
    case tag of
      6 -> StoredAllocation <$> unpackM <*> unpackM
      _ -> unknownTagM @StoredAllocation tag

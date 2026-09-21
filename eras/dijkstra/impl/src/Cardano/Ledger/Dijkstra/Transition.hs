{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Transition (
  TransitionConfig (..),
) where

import Cardano.Ledger.Conway
import Cardano.Ledger.Conway.Transition (
  ConwayEraTransition,
  conwayRegisterInitialFundsThenStaking,
 )
import Cardano.Ledger.Dijkstra.Era
import Cardano.Ledger.Dijkstra.Genesis
import qualified Cardano.Ledger.Dijkstra.Transition.InitialFunds as InitialFunds
import Cardano.Ledger.Dijkstra.Translation ()
import Cardano.Ledger.Shelley.Transition
import GHC.Generics
import Lens.Micro
import NoThunks.Class (NoThunks (..))

instance EraTransition DijkstraEra where
  data TransitionConfig DijkstraEra = DijkstraTransitionConfig
    { dtcDijkstraGenesis :: !DijkstraGenesis
    , dtcConwayTransitionConfig :: !(TransitionConfig ConwayEra)
    }
    deriving (Show, Eq, Generic)

  mkTransitionConfig = DijkstraTransitionConfig

  injectIntoTestState = conwayRegisterInitialFundsThenStaking

  allocateInitialFunds protocolParameters address =
    either failInitialFundsAllocation id . InitialFunds.allocateInitialFunds protocolParameters address

  tcPreviousEraConfigL =
    lens dtcConwayTransitionConfig (\dtc pc -> dtc {dtcConwayTransitionConfig = pc})

  tcTranslationContextL =
    lens dtcDijkstraGenesis (\dtc ag -> dtc {dtcDijkstraGenesis = ag})

instance ConwayEraTransition DijkstraEra

instance NoThunks (TransitionConfig DijkstraEra)

-- Private helpers

failInitialFundsAllocation :: InitialFunds.InitialFundsAllocationError -> a
failInitialFundsAllocation =
  error . ("Dijkstra.allocateInitialFunds: " <>) . show

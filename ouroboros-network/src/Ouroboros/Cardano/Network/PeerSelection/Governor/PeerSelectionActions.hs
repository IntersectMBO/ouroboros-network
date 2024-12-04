{-# LANGUAGE NamedFieldPuns #-}
module Ouroboros.Cardano.Network.PeerSelection.Governor.PeerSelectionActions where

import Cardano.Network.PeerSelection.Bootstrap (UseBootstrapPeers)
import Control.Concurrent.Class.MonadSTM
import Ouroboros.Cardano.Network.ArgumentsExtra (CardanoArgumentsExtra (..))
import Ouroboros.Network.PeerSelection.Governor.Types (PeerSelectionTargets)

-- | Cardano Node PeerSelection Actions extension data type.
--
-- It contain specific PeerSelection actions parameters to guide the Outbound
-- Governor.
--
data CardanoPeerSelectionActions m =
  CardanoPeerSelectionActions {
    -- | Retrieve peer targets for Genesis & non-Genesis modes
    -- from node's configuration for the current state
    --
    cpsaGenesisPeerTargets    :: PeerSelectionTargets

    -- | Read the current bootstrap peers flag
  , cpsaReadUseBootstrapPeers :: STM m UseBootstrapPeers
  }

cardanoExtraArgsToPeerSelectionActions :: CardanoArgumentsExtra m
                                       -> CardanoPeerSelectionActions m
cardanoExtraArgsToPeerSelectionActions CardanoArgumentsExtra {
                                         caeGenesisPeerTargets
                                       , caeReadUseBootstrapPeers
                                       } =
  CardanoPeerSelectionActions {
    cpsaGenesisPeerTargets       = caeGenesisPeerTargets
  , cpsaReadUseBootstrapPeers = caeReadUseBootstrapPeers
  }


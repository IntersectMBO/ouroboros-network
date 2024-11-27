module Cardano.Network.PeerSelection.Governor.PeerSelectionActions where

import Cardano.Network.ArgumentsExtra (ConsensusModePeerTargets)
import Cardano.Network.PeerSelection.Bootstrap (UseBootstrapPeers)
import Control.Concurrent.Class.MonadSTM

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
    cpsaPeerTargets           :: ConsensusModePeerTargets

    -- | Read the current bootstrap peers flag
  , cpsaReadUseBootstrapPeers :: STM m UseBootstrapPeers
  }

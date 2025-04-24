{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Network.PeerSelection.Governor.PeerSelectionActions where

import Cardano.Network.PeerSelection.Bootstrap (UseBootstrapPeers)
import Control.Concurrent.Class.MonadSTM
import Ouroboros.Network.PeerSelection.Governor.Types (PeerSelectionTargets)

-- | Cardano Node PeerSelection Actions extension data type.
--
-- It contain specific PeerSelection actions parameters to guide the Outbound
-- Governor.
--
data ExtraPeerSelectionActions m =
  ExtraPeerSelectionActions {
    -- | Retrieve peer targets for Genesis & non-Genesis modes
    -- from node's configuration for the current state
    --
    genesisPeerTargets    :: PeerSelectionTargets

    -- | Read the current bootstrap peers flag
  , readUseBootstrapPeers :: STM m UseBootstrapPeers
  }


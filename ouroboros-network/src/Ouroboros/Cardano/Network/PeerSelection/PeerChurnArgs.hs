module Ouroboros.Cardano.Network.PeerSelection.PeerChurnArgs where

import Cardano.Network.ConsensusMode (ConsensusMode)
import Cardano.Network.PeerSelection.Bootstrap (UseBootstrapPeers)
import Control.Concurrent.Class.MonadSTM.Strict
import Ouroboros.Cardano.Network.PeerSelection.Types (ChurnMode)
import Ouroboros.Network.BlockFetch.ConsensusInterface (FetchMode)
import Ouroboros.Network.PeerSelection.Governor.Types (PeerSelectionTargets)

data CardanoPeerChurnArgs m =
  CardanoPeerChurnArgs {
    cpcaModeVar            :: StrictTVar m ChurnMode
  , cpcaReadFetchMode      :: STM m FetchMode
  , cpcaGenesisPeerTargets :: PeerSelectionTargets
  , cpcaReadUseBootstrap   :: STM m UseBootstrapPeers
  , cpcaConsensusMode      :: ConsensusMode
  }


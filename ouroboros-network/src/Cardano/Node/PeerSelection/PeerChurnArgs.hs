module Cardano.Node.PeerSelection.PeerChurnArgs where

import Cardano.Node.ArgumentsExtra (ConsensusModePeerTargets)
import Cardano.Node.ConsensusMode (ConsensusMode)
import Cardano.Node.PeerSelection.Bootstrap (UseBootstrapPeers)
import Cardano.Node.PeerSelection.Types (ChurnMode)
import Control.Concurrent.Class.MonadSTM.Strict
import Ouroboros.Network.BlockFetch.ConsensusInterface (FetchMode)

data CardanoPeerChurnArgs m =
  CardanoPeerChurnArgs {
    cpcaModeVar          :: StrictTVar m ChurnMode
  , cpcaReadFetchMode    :: STM m FetchMode
  , cpcaPeerTargets      :: ConsensusModePeerTargets
  , cpcaReadUseBootstrap :: STM m UseBootstrapPeers
  , cpcaConsensusMode    :: ConsensusMode
  }


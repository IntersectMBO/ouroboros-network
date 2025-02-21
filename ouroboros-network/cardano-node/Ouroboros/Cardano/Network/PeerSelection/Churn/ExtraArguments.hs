module Ouroboros.Cardano.Network.PeerSelection.Churn.ExtraArguments where

import Cardano.Network.ConsensusMode (ConsensusMode)
import Cardano.Network.PeerSelection.Bootstrap (UseBootstrapPeers)
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Tracer (Tracer)
import Ouroboros.Cardano.Network.Types (ChurnMode)
import Ouroboros.Network.BlockFetch.ConsensusInterface (FetchMode)
import Ouroboros.Network.PeerSelection.Governor.Types (PeerSelectionTargets)

data ExtraArguments m =
  ExtraArguments {
    modeVar            :: StrictTVar m ChurnMode
  , readFetchMode      :: STM m FetchMode
  , genesisPeerTargets :: PeerSelectionTargets
  , readUseBootstrap   :: STM m UseBootstrapPeers
  , consensusMode      :: ConsensusMode
  , tracerChurnMode    :: Tracer m TracerChurnMode
  }

newtype TracerChurnMode = TraceChurnMode ChurnMode
  deriving Show

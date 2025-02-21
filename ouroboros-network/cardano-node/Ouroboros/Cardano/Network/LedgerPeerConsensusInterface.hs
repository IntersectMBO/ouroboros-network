module Ouroboros.Cardano.Network.LedgerPeerConsensusInterface where

import Cardano.Network.PeerSelection.LocalRootPeers (OutboundConnectionsState)
import Cardano.Network.Types (LedgerStateJudgement)
import Control.Concurrent.Class.MonadSTM (MonadSTM (..))

-- | Cardano Node specific consensus interface actions.
--
data LedgerPeersConsensusInterface m =
  LedgerPeersConsensusInterface {
    getLedgerStateJudgement        :: STM m LedgerStateJudgement

    -- | Callback provided by consensus to inform it if the node is
    -- connected to only local roots or also some external peers.
    --
    -- This is useful in order for the Bootstrap State Machine to
    -- simply refuse to transition from TooOld to YoungEnough while
    -- it only has local peers.
    --
  , updateOutboundConnectionsState :: OutboundConnectionsState -> STM m ()
  }

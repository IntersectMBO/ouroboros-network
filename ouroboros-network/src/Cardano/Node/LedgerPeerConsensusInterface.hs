module Cardano.Node.LedgerPeerConsensusInterface where

import Cardano.Node.PeerSelection.LocalRootPeers (OutboundConnectionsState)
import Cardano.Node.Types (LedgerStateJudgement)
import Control.Concurrent.Class.MonadSTM (MonadSTM (..))

-- | Cardano Node specific consensus interface actions.
--
data CardanoLedgerPeersConsensusInterface m =
  CardanoLedgerPeersConsensusInterface {
    clpciGetLedgerStateJudgement        :: STM m LedgerStateJudgement

    -- | Callback provided by consensus to inform it if the node is
    -- connected to only local roots or also some external peers.
    --
    -- This is useful in order for the Bootstrap State Machine to
    -- simply refuse to transition from TooOld to YoungEnough while
    -- it only has local peers.
    --
  , clpciUpdateOutboundConnectionsState :: OutboundConnectionsState -> STM m ()
  }

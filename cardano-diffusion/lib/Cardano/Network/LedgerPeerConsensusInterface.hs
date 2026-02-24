{-# LANGUAGE RankNTypes #-}

module Cardano.Network.LedgerPeerConsensusInterface
  ( LedgerPeersConsensusInterface (..)
    -- * Re-exports
  , FetchMode (..)
  , LedgerStateJudgement (..)
  , OutboundConnectionsState (..)
  ) where

import Control.Concurrent.Class.MonadSTM (MonadSTM (..))

import Cardano.Network.LedgerStateJudgement
import Cardano.Network.PeerSelection.LocalRootPeers
           (OutboundConnectionsState (..))
import Ouroboros.Network.Block (Point)
import Ouroboros.Network.BlockFetch.ConsensusInterface (FetchMode (..))
import Ouroboros.Network.PeerSelection.LedgerPeers.Type (RawBlockHash)


-- | Cardano Node specific consensus interface actions.
--
data LedgerPeersConsensusInterface m =
  LedgerPeersConsensusInterface {
    -- | Required for BlockFetch protocol
    readFetchMode                  :: STM m FetchMode

  , getLedgerStateJudgement        :: STM m LedgerStateJudgement

    -- | Callback provided by consensus to inform it if the node is
    -- connected to only local roots or also some external peers.
    --
    -- This is useful in order for the Bootstrap State Machine to
    -- simply refuse to transition from TooOld to YoungEnough while
    -- it only has local peers.
    --
  , updateOutboundConnectionsState :: OutboundConnectionsState -> STM m ()

  -- | Ask the Consensus layer's immutable DB for a point. The callback will yield either:
  --   - the block at the target slot if there is a block in the immutable DB at that slot;
  --   - or the block from the next occupied slot.
  , getBlockHash
      :: forall r. Point RawBlockHash -> (m (Maybe (Point RawBlockHash)) -> m r) -> m r
  }

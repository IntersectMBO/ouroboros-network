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
import Ouroboros.Network.Block (Point, SlotNo, StandardHash)
import Ouroboros.Network.BlockFetch.ConsensusInterface (FetchMode (..))


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

  , getBlockHash
      :: forall r. SlotNo -> (forall blk. StandardHash blk => STM m (Point blk) -> r) -> r
  }

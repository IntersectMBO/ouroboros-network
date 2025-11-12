{-# LANGUAGE RankNTypes #-}

module Cardano.Network.LedgerPeerConsensusInterface
  ( LedgerPeersConsensusInterface (..)
    -- * Re-exports
  , FetchMode (..)
  , LedgerStateJudgement (..)
  , OutboundConnectionsState (..)
  ) where

import Control.Concurrent.Class.MonadSTM (MonadSTM (..))

import Cardano.Crypto.Hash (Blake2b_256, Hash)
import Cardano.Network.LedgerStateJudgement
import Cardano.Network.PeerSelection.LocalRootPeers
           (OutboundConnectionsState (..))
import Ouroboros.Network.Block (SlotNo)
import Ouroboros.Network.BlockFetch.ConsensusInterface (FetchMode (..))
import Ouroboros.Network.Point (Block)


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
      :: forall r. SlotNo -> (forall a. STM m (Block SlotNo (Hash Blake2b_256 a)) -> r) -> r
  }

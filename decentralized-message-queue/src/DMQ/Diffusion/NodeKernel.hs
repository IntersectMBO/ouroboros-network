{-# LANGUAGE NamedFieldPuns #-}

module DMQ.Diffusion.NodeKernel where

import Control.Concurrent.Class.MonadMVar
import Control.Concurrent.Class.MonadSTM.Strict

import System.Random (StdGen)
import System.Random qualified as Random

import Ouroboros.Network.BlockFetch (FetchClientRegistry,
           newFetchClientRegistry)
import Ouroboros.Network.ConnectionId (ConnectionId (..))
import Ouroboros.Network.PeerSelection.Governor.Types
           (makePublicPeerSelectionStateVar)
import Ouroboros.Network.PeerSharing (PeerSharingAPI, PeerSharingRegistry,
           newPeerSharingAPI, newPeerSharingRegistry,
           ps_POLICY_PEER_SHARE_MAX_PEERS, ps_POLICY_PEER_SHARE_STICKY_TIME)
import Ouroboros.Network.TxSubmission.Inbound.V2.Registry
import Ouroboros.Network.TxSubmission.Mempool.Simple (Mempool)
import Ouroboros.Network.TxSubmission.Mempool.Simple qualified as Mempool

import DMQ.Protocol.SigSubmission.Type (Sig, SigId)

data NodeKernel ntnAddr m =
  NodeKernel {
    -- | The fetch client registry, used for the keep alive clients.
    fetchClientRegistry :: FetchClientRegistry (ConnectionId ntnAddr) () () m

    -- | Read the current peer sharing registry, used for interacting with
    -- the PeerSharing protocol
  , peerSharingRegistry :: PeerSharingRegistry ntnAddr m
  , peerSharingAPI      :: PeerSharingAPI ntnAddr StdGen m
  , mempool             :: Mempool m Sig
  , sigChannelVar       :: TxChannelsVar m ntnAddr SigId Sig
  , sigMempoolSem       :: TxMempoolSem m
  , sigSharedTxStateVar :: SharedTxStateVar m ntnAddr SigId Sig
  }

newNodeKernel :: ( MonadLabelledSTM m
                 , MonadMVar m
                 , Ord ntnAddr
                 )
              => StdGen
              -> m (NodeKernel ntnAddr m)
newNodeKernel rng = do
  publicPeerSelectionStateVar <- makePublicPeerSelectionStateVar

  fetchClientRegistry <- newFetchClientRegistry
  peerSharingRegistry <- newPeerSharingRegistry

  mempool <- Mempool.empty
  sigChannelVar <- newTxChannelsVar
  sigMempoolSem <- newTxMempoolSem
  let (rng', rng'') = Random.split rng
  sigSharedTxStateVar <- newSharedTxStateVar rng'

  peerSharingAPI <-
    newPeerSharingAPI
      publicPeerSelectionStateVar
      rng''
      ps_POLICY_PEER_SHARE_STICKY_TIME
      ps_POLICY_PEER_SHARE_MAX_PEERS

  pure NodeKernel { fetchClientRegistry
                  , peerSharingRegistry
                  , peerSharingAPI
                  , mempool
                  , sigChannelVar
                  , sigMempoolSem
                  , sigSharedTxStateVar
                  }


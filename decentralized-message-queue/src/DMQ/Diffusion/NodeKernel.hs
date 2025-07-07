{-# LANGUAGE NamedFieldPuns #-}

module DMQ.Diffusion.NodeKernel where

import System.Random (StdGen)

import Control.Concurrent.Class.MonadSTM (MonadLabelledSTM)
import Control.DeepSeq (NFData)
import NoThunks.Class (NoThunks)
import Ouroboros.Network.BlockFetch (FetchClientRegistry,
           newFetchClientRegistry)
import Ouroboros.Network.ConnectionId (ConnectionId (..))
import Ouroboros.Network.PeerSelection.Governor.Types
           (makePublicPeerSelectionStateVar)
import Ouroboros.Network.PeerSharing (PeerSharingAPI, PeerSharingRegistry,
           newPeerSharingAPI, newPeerSharingRegistry,
           ps_POLICY_PEER_SHARE_MAX_PEERS, ps_POLICY_PEER_SHARE_STICKY_TIME)

data NodeKernel ntnAddr m =
  NodeKernel {
    -- | The fetch client registry, used for the keep alive clients.
    fetchClientRegistry :: FetchClientRegistry (ConnectionId ntnAddr) () () m

    -- | Read the current peer sharing registry, used for interacting with
    -- the PeerSharing protocol
  , peerSharingRegistry :: PeerSharingRegistry ntnAddr m
  , peerSharingAPI      :: PeerSharingAPI ntnAddr StdGen m
  }

newNodeKernel :: ( MonadLabelledSTM m
                 , Ord ntnAddr
                 , NFData ntnAddr
                 , NoThunks ntnAddr
                 )
              => StdGen
              -> m (NodeKernel ntnAddr m)
newNodeKernel peerSharingRng = do
  publicPeerSelectionStateVar <- makePublicPeerSelectionStateVar

  fetchClientRegistry <- newFetchClientRegistry
  peerSharingRegistry <- newPeerSharingRegistry

  peerSharingAPI <- newPeerSharingAPI publicPeerSelectionStateVar
                                      peerSharingRng
                                      ps_POLICY_PEER_SHARE_STICKY_TIME
                                      ps_POLICY_PEER_SHARE_MAX_PEERS

  pure NodeKernel { fetchClientRegistry
                  , peerSharingRegistry
                  , peerSharingAPI
                  }


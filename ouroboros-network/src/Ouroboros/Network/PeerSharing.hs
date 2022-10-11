{-# LANGUAGE TupleSections #-}
module Ouroboros.Network.PeerSharing where

import           Ouroboros.Network.Protocol.PeerSharing.Client
                     (PeerSharingClient (..))
import           Ouroboros.Network.Protocol.PeerSharing.Server
                     (PeerSharingServer (..))
import           Ouroboros.Network.Protocol.PeerSharing.Type (PeerSharingAmount)

peerSharingClient :: Monad m
                  => PeerSharingAmount
                  -> PeerSharingClient peer m [peer]
peerSharingClient amount =
  SendMsgShareRequest amount (return . SendMsgDone . return)


peerSharingServer :: Monad m
                  => (PeerSharingAmount -> m [peer])
                  -> PeerSharingServer peer m
peerSharingServer computePeersToShare =
  PeerSharingServer
    { recvMsgShareRequest = \amount -> (,) <$> computePeersToShare amount
                                          <*> return (peerSharingServer computePeersToShare)
    }

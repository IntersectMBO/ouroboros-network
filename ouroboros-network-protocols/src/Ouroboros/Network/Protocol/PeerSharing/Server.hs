{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RecordWildCards #-}

module Ouroboros.Network.Protocol.PeerSharing.Server where

import Network.TypedProtocol.Peer.Server
import Ouroboros.Network.Protocol.PeerSharing.Type

data PeerSharingServer peerAddress m = PeerSharingServer {
  -- | The client sent us a 'MsgShareRequest'. We have need to compute the
  -- response.
  --
  recvMsgShareRequest :: PeerSharingAmount
                      -> m ( [peerAddress]
                           , PeerSharingServer peerAddress m
                           )
  }

peerSharingServerPeer :: Monad m
                      => PeerSharingServer peerAddress m
                      -> Server (PeerSharing peerAddress) NonPipelined StIdle m ()
peerSharingServerPeer PeerSharingServer{..} =
  -- Await to receive a message
  Await $ \msg ->
    -- Can be either 'MsgShareRequest' or 'MsgDone'
    case msg of
      -- Compute the response and send 'MsgSharePeers' message
      MsgShareRequest amount -> Effect $ do
        (resp, server) <- recvMsgShareRequest amount
        return $
          Yield (MsgSharePeers resp)
                (peerSharingServerPeer server)
      -- Nothing to do.
      MsgDone -> Done ()

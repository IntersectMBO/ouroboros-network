{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Ouroboros.Network.Protocol.PeerSharing.Server where

import Network.TypedProtocol.Peer.Server
import Ouroboros.Network.Protocol.PeerSharing.Type

newtype PeerSharingServer peerAddress m = PeerSharingServer {
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
  Await \case
    -- Can be either 'MsgShareRequest' or 'MsgDone'
    MsgShareRequest amount -> Effect do
      -- Compute the response and send 'MsgSharePeers' message
      (resp, server) <- recvMsgShareRequest amount
      return $
        Yield (MsgSharePeers resp)
              (peerSharingServerPeer server)
    -- Nothing to do.
    MsgDone -> Done ()

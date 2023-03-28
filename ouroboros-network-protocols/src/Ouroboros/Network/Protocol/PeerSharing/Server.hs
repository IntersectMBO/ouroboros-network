{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RecordWildCards #-}

module Ouroboros.Network.Protocol.PeerSharing.Server where

import           Network.TypedProtocol.Core (Peer (..), PeerHasAgency (..),
                     PeerRole (..))
import           Ouroboros.Network.Protocol.PeerSharing.Type
                     (ClientHasAgency (..), Message (..), NobodyHasAgency (..),
                     PeerSharing (..), PeerSharingAmount, ServerHasAgency (..))

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
                      -> Peer (PeerSharing peerAddress) AsServer StIdle m ()
peerSharingServerPeer PeerSharingServer{..} =
  -- Await receival of a message from the client
  Await (ClientAgency TokIdle) $ \msg ->
    -- Can be either 'MsgShareRequest' or 'MsgDone'
    case msg of
      -- Compute the response and send 'MsgSharePeers' message
      MsgShareRequest amount -> Effect $ do
        (resp, server) <- recvMsgShareRequest amount
        return $
          Yield (ServerAgency TokBusy)
                (MsgSharePeers resp)
                (peerSharingServerPeer server)
      -- Nothing to do.
      MsgDone -> Done TokDone ()

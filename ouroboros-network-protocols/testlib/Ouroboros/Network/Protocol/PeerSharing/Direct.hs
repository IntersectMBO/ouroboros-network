{-# LANGUAGE NamedFieldPuns #-}

module Ouroboros.Network.Protocol.PeerSharing.Direct where

import           Ouroboros.Network.Protocol.PeerSharing.Client
import           Ouroboros.Network.Protocol.PeerSharing.Server

direct :: Monad m
       => PeerSharingServer peer m
       -> PeerSharingClient peer m b
       -> m b
direct PeerSharingServer {}
       (SendMsgDone mdone) = mdone
direct PeerSharingServer { recvMsgShareRequest }
       (SendMsgShareRequest amount mclient) = do
    (peers, server) <- recvMsgShareRequest amount
    client <- mclient peers
    direct server client

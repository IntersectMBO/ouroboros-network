{-# LANGUAGE NamedFieldPuns #-}

module Ouroboros.Network.Protocol.ChainSync.Direct where

import Ouroboros.Network.Protocol.ChainSync.Client as Client
import Ouroboros.Network.Protocol.ChainSync.Server as Server

-- | The 'ClientStream m' and 'ServerStream m' types are complementary. The
-- former can be used to feed the latter directly, in the same thread.
-- That's demonstrated here by constructing 'direct'.
--
direct :: Monad m
       => ChainSyncServer header point m a
       -> ChainSyncClient header point m b
       -> m (a, b)
direct (ChainSyncServer mserver) (ChainSyncClient mclient) = do
  server <- mserver
  client <- mclient
  direct_ server client

direct_ :: Monad m
        => ServerStIdle header point m a
        -> ClientStIdle header point m b
        -> m (a, b)
direct_  ServerStIdle{recvMsgRequestNext}
        (Client.SendMsgRequestNext stNext stAwait) = do
    mresp <- recvMsgRequestNext
    case mresp of
      Left  resp    -> directStNext resp stNext
      Right waiting -> do resp <- waiting
                          stNext' <- stAwait
                          directStNext resp stNext'
  where
    directStNext (SendMsgRollForward header pHead server')
                  ClientStNext{recvMsgRollForward} = do
      direct server' (recvMsgRollForward header pHead)

    directStNext (SendMsgRollBackward pIntersect pHead server')
                  ClientStNext{recvMsgRollBackward} = do
      direct server' (recvMsgRollBackward pIntersect pHead)

direct_  ServerStIdle{recvMsgFindIntersect}
        (Client.SendMsgFindIntersect points
          ClientStIntersect{recvMsgIntersectImproved,
                            recvMsgIntersectUnchanged}) = do
    sIntersect <- recvMsgFindIntersect points
    case sIntersect of
      SendMsgIntersectImproved  pIntersect pHead server' ->
        direct server' (recvMsgIntersectImproved pIntersect pHead)

      SendMsgIntersectUnchanged            pHead server' ->
        direct server' (recvMsgIntersectUnchanged pHead)

direct_ ServerStIdle{recvMsgDoneClient}
       (Client.SendMsgDone clientDone) = do
    msgDoneClient <- recvMsgDoneClient
    return (msgDoneClient, clientDone)

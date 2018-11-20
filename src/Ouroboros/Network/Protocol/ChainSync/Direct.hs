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

direct  ServerStIdle{recvMsgRequestNext}
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
      client' <- recvMsgRollForward header pHead
      direct server' client'

    directStNext (SendMsgRollBackward pIntersect pHead server')
                  ClientStNext{recvMsgRollBackward} = do
      client' <- recvMsgRollBackward pIntersect pHead
      direct server' client'

direct  ServerStIdle{recvMsgFindIntersect}
       (Client.SendMsgFindIntersect points
          ClientStIntersect{recvMsgIntersectImproved,
                            recvMsgIntersectUnchanged}) = do
    sIntersect <- recvMsgFindIntersect points
    case sIntersect of
      SendMsgIntersectImproved  pIntersect pHead server' -> do
        client' <- recvMsgIntersectImproved pIntersect pHead
        direct server' client'

      SendMsgIntersectUnchanged            pHead server' -> do
        client' <- recvMsgIntersectUnchanged pHead
        direct server' client'

direct ServerStIdle{recvMsgDoneClient}
       (Client.SendMsgDone clientDone) =
    return (recvMsgDoneClient, clientDone)

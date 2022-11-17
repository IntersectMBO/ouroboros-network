{-# LANGUAGE NamedFieldPuns #-}

module Ouroboros.Network.Protocol.ChainSync.Direct where

import           Ouroboros.Network.Protocol.ChainSync.Client as Client
import           Ouroboros.Network.Protocol.ChainSync.Server as Server

-- | The 'ClientStream m' and 'ServerStream m' types are complementary. The
-- former can be used to feed the latter directly, in the same thread.
-- That's demonstrated here by constructing 'direct'.
--
direct :: Monad m
       => ChainSyncServer header point tip m a
       -> ChainSyncClient header point tip m b
       -> m (a, b)
direct (ChainSyncServer mserver) (ChainSyncClient mclient) = do
  server <- mserver
  client <- mclient
  direct_ server client

direct_ :: Monad m
        => ServerStIdle header point tip m a
        -> ClientStIdle header point tip m b
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
    directStNext (SendMsgRollForward header tip server')
                  ClientStNext{recvMsgRollForward} = do
      direct server' (recvMsgRollForward header tip)

    directStNext (SendMsgRollBackward pIntersect tip server')
                  ClientStNext{recvMsgRollBackward} = do
      direct server' (recvMsgRollBackward pIntersect tip)

direct_  ServerStIdle{recvMsgFindIntersect}
        (Client.SendMsgFindIntersect points
          ClientStIntersect{recvMsgIntersectFound,
                            recvMsgIntersectNotFound}) = do
    sIntersect <- recvMsgFindIntersect points
    case sIntersect of
      SendMsgIntersectFound  pIntersect tip server' ->
        direct server' (recvMsgIntersectFound pIntersect tip)

      SendMsgIntersectNotFound          tip server' ->
        direct server' (recvMsgIntersectNotFound         tip)

direct_ ServerStIdle{recvMsgDoneClient}
       (Client.SendMsgDone clientDone) = do
    msgDoneClient <- recvMsgDoneClient
    return (msgDoneClient, clientDone)

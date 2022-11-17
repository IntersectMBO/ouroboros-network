{-# LANGUAGE GADTs          #-}
{-# LANGUAGE NamedFieldPuns #-}

module Ouroboros.Network.Protocol.ChainSync.DirectPipelined (directPipelined) where

import           Network.TypedProtocol.Proofs

import           Ouroboros.Network.Protocol.ChainSync.ClientPipelined as ClientPipelined
import           Ouroboros.Network.Protocol.ChainSync.Server as Server


directPipelined :: Monad m
                => ChainSyncServer header point tip m a
                -> ChainSyncClientPipelined header point tip m b
                -> m (a, b)
directPipelined (ChainSyncServer mserver) (ChainSyncClientPipelined mclient) =
    directStIdleM EmptyQ mserver mclient


directStIdleM :: Monad m
                 => Queue n (ChainSyncInstruction header point tip)
                 -> m (ServerStIdle            header point tip m a)
                 -> m (ClientPipelinedStIdle n header point tip m b)
                 -> m (a, b)

directStIdleM queue mServerStIdle mClientStIdle = do
    serverStIdle <- mServerStIdle
    clientStIdle <- mClientStIdle
    directStIdle queue serverStIdle clientStIdle


directStIdle :: Monad m
             => Queue n (ChainSyncInstruction header point tip)
             -> ServerStIdle            header point tip m a
             -> ClientPipelinedStIdle n header point tip m b
             -> m (a, b)
directStIdle queue@EmptyQ ServerStIdle {recvMsgRequestNext} (SendMsgRequestNext stClientNext stClientAwait) = do
    mStServerNext <- recvMsgRequestNext
    case mStServerNext of
      Left stServerNext    -> directStNext stServerNext stClientNext
      Right mStServerAwait -> do
        stServerNext' <- mStServerAwait
        stClientNext' <- stClientAwait
        directStNext stServerNext' stClientNext'

  where
    directStNext (SendMsgRollForward header tip (ChainSyncServer mStServerIdle)) ClientStNext {recvMsgRollForward} =
      directStIdleM queue mStServerIdle (recvMsgRollForward header tip)

    directStNext (SendMsgRollBackward pIntersect tip (ChainSyncServer mStServerIdle)) ClientStNext {recvMsgRollBackward} =
      directStIdleM queue mStServerIdle (recvMsgRollBackward pIntersect tip)

directStIdle queue (ServerStIdle {recvMsgRequestNext}) (SendMsgRequestNextPipelined stClientIdle) = do
    mStServerNext <- recvMsgRequestNext
    case mStServerNext of
      Left stServerNext    -> directStIdlePipelined stServerNext
      Right mStServerAwait -> do
        stServerNext' <- mStServerAwait
        directStIdlePipelined stServerNext'
  where
    directStIdlePipelined (SendMsgRollForward header tip (ChainSyncServer mStServerIdle)) = do
      stServerIdle <- mStServerIdle
      directStIdle (enqueue (RollForward header tip) queue) stServerIdle stClientIdle

    directStIdlePipelined (SendMsgRollBackward pIntersect tip (ChainSyncServer mStServerIdle)) = do
      stServerIdle <- mStServerIdle
      directStIdle (enqueue (RollBackward pIntersect tip) queue) stServerIdle stClientIdle

directStIdle (ConsQ (RollForward header tip) queue) stServerIdle (CollectResponse _ ClientStNext {recvMsgRollForward}) = do
    stClientIdle <- recvMsgRollForward header tip
    directStIdle queue stServerIdle stClientIdle

directStIdle (ConsQ (RollBackward pIntersect tip) queue) stServerIdle (CollectResponse _ ClientStNext {recvMsgRollBackward}) = do
    stClientIdle <- recvMsgRollBackward pIntersect tip
    directStIdle queue stServerIdle stClientIdle

directStIdle queue@EmptyQ ServerStIdle {recvMsgFindIntersect}
             (SendMsgFindIntersect points
                ClientPipelinedStIntersect { recvMsgIntersectFound
                                           , recvMsgIntersectNotFound}) = do
    sIntersect <- recvMsgFindIntersect points
    case sIntersect of
      SendMsgIntersectFound pIntersect tip (ChainSyncServer mStServerIdle) ->
        directStIdleM queue mStServerIdle (recvMsgIntersectFound pIntersect tip)

      SendMsgIntersectNotFound          tip (ChainSyncServer mStServerIdle) ->
        directStIdleM queue mStServerIdle (recvMsgIntersectNotFound         tip)

directStIdle EmptyQ ServerStIdle {recvMsgDoneClient} (SendMsgDone clientDone) = do
    msgDoneClient <- recvMsgDoneClient
    return (msgDoneClient, clientDone)


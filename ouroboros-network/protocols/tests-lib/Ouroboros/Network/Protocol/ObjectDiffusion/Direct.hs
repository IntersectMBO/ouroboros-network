{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Ouroboros.Network.Protocol.ObjectDiffusion.Direct (directPipelined) where

import Data.Map.Strict qualified as Map

import Network.TypedProtocol.Core
import Network.TypedProtocol.Proofs (Queue (..), enqueue)

import Ouroboros.Network.Protocol.ObjectDiffusion.inbound
import Ouroboros.Network.Protocol.ObjectDiffusion.outbound


directPipelined
  :: forall objectId object m a b.
     Monad m
  => ObjectDiffusionOutboundPipelined objectId object m a
  -> ObjectDiffusionInbound          objectId object m b
  -> m (a, b)
directPipelined (ObjectDiffusionOutboundPipelined mOutbound)
                (ObjectDiffusionInbound mInbound) = do
    outbound <- mOutbound
    inbound <- mInbound
    directSender EmptyQ outbound inbound
  where
    directSender :: forall (n :: N).
                    Queue          n (Collect objectId object)
                 -> OutboundStIdle n objectId object m a
                 -> InboundStIdle    objectId object m b
                 -> m (a, b)
    directSender q (SendMsgRequestobjectIdsBlocking ackNo reqNo a outboundNext)
                   InboundStIdle{recvMsgRequestobjectIds} = do
      reply <- recvMsgRequestobjectIds SingBlocking ackNo reqNo
      case reply of
        SendMsgReplyobjectIds (BlockingReply objectIds) inbound' -> do
          outbound' <- outboundNext objectIds
          directSender q outbound' inbound'
        SendMsgDone b -> (,b) <$> a

    directSender q (SendMsgRequestobjectIdsPipelined ackNo reqNo outboundNext)
                   InboundStIdle{recvMsgRequestobjectIds} = do
      reply <- recvMsgRequestobjectIds SingNonBlocking ackNo reqNo
      case reply of
        SendMsgReplyobjectIds (NonBlockingReply objectIds) inbound' -> do
          outbound' <- outboundNext
          directSender (enqueue (CollectobjectIds reqNo objectIds) q) outbound' inbound'

    directSender q (SendMsgRequestObjectsPipelined objectIds outboundNext)
                   InboundStIdle{recvMsgRequestObjects} = do
      outbound' <- outboundNext
      SendMsgReplyObjects objects inbound' <- recvMsgRequestObjects (Map.keys objectIds)
      directSender (enqueue (CollectObjects objectIds objects) q) outbound' inbound'

    directSender q (CollectPipelined (Just outbound) _) inbound = do
      outbound' <- outbound
      directSender q outbound' inbound

    directSender (ConsQ c q) (CollectPipelined _ collect) inbound = do
      outbound' <- collect c
      directSender q outbound' inbound

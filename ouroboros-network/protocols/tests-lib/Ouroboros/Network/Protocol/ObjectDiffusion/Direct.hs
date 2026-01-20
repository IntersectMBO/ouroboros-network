{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.Protocol.ObjectDiffusion.Direct (directPipelined) where


import Network.TypedProtocol.Core
import Network.TypedProtocol.Proofs (Queue (..), enqueue)

import Ouroboros.Network.Protocol.ObjectDiffusion.Inbound
import Ouroboros.Network.Protocol.ObjectDiffusion.Outbound
import Ouroboros.Network.Protocol.ObjectDiffusion.Type (BlockingReplyList (..),
           SingBlockingStyle (..))


directPipelined
  :: forall objectId object m a.
     Monad m
  => ObjectDiffusionOutbound         objectId object m a
  -> ObjectDiffusionInboundPipelined objectId object m a
  -> m a
directPipelined (ObjectDiffusionOutbound mOutbound)
                (ObjectDiffusionInboundPipelined inbound) = do
    outbound <- mOutbound
    directSender EmptyQ inbound outbound
  where
    directSender :: forall (n :: N).
                    Queue          n (Collect objectId object)
                 -> InboundStIdle n objectId object m a
                 -> OutboundStIdle    objectId object m a
                 -> m a
    directSender q (SendMsgRequestObjectIdsBlocking ackNo reqNo inboundNext)
                   OutboundStIdle{recvMsgRequestObjectIds} = do
      reply <- recvMsgRequestObjectIds SingBlocking ackNo reqNo
      case reply of
        SendMsgReplyObjectIds (BlockingReply objectIds) outbound' -> do
          let inbound' = inboundNext objectIds
          directSender q inbound' outbound'

    directSender q (SendMsgRequestObjectIdsPipelined ackNo reqNo inbound')
                   OutboundStIdle{recvMsgRequestObjectIds} = do
      reply <- recvMsgRequestObjectIds SingNonBlocking ackNo reqNo
      case reply of
        SendMsgReplyObjectIds (NonBlockingReply objectIds) outbound' -> do
          directSender (enqueue (CollectObjectIds reqNo objectIds) q) inbound' outbound'

    directSender q (SendMsgRequestObjectsPipelined objectIds inbound')
                   OutboundStIdle{recvMsgRequestObjects} = do
      SendMsgReplyObjects objects outbound' <- recvMsgRequestObjects objectIds
      directSender (enqueue (CollectObjects objectIds objects) q) inbound' outbound'

    directSender q (CollectPipelined (Just noWaitInbound') _inboundNext) outbound = do
      directSender q noWaitInbound' outbound

    directSender (ConsQ c q) (CollectPipelined _maybeNoWaitInbound' inboundNext) outbound = do
      let inbound' = inboundNext c
      directSender q inbound' outbound

    directSender q (WithEffect mInbound) outbound = do
      inbound' <- mInbound
      directSender q inbound' outbound

    directSender EmptyQ (SendMsgDone v) _outbound = pure v

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Ouroboros.Network.Protocol.TxSubmission.Direct (directPipelined) where

import           Network.TypedProtocol.Pipelined
import           Network.TypedProtocol.Proofs (Queue (..), enqueue)

import           Ouroboros.Network.Protocol.TxSubmission.Client
import           Ouroboros.Network.Protocol.TxSubmission.Server


directPipelined
  :: forall txid tx m a b.
     Monad m
  => TxSubmissionServerPipelined txid tx m a
  -> TxSubmissionClient          txid tx m b
  -> m (a, b)
directPipelined (TxSubmissionServerPipelined mserver)
                (TxSubmissionClient mclient) = do
    server <- mserver
    client <- mclient
    directSender EmptyQ server client
  where
    directSender :: forall (n :: N).
                    Queue        n (Collect txid tx)
                 -> ServerStIdle n txid tx m a
                 -> ClientStIdle   txid tx m b
                 -> m (a, b)
    directSender q (SendMsgRequestTxIdsBlocking ackNo reqNo a serverNext)
                   ClientStIdle{recvMsgRequestTxIds} = do
      reply <- recvMsgRequestTxIds TokBlocking ackNo reqNo
      case reply of
        SendMsgReplyTxIds (BlockingReply txids) client' -> do
          server' <- serverNext txids
          directSender q server' client'
        SendMsgDone b -> (,b) <$> a

    directSender q (SendMsgRequestTxIdsPipelined ackNo reqNo serverNext)
                   ClientStIdle{recvMsgRequestTxIds} = do
      reply <- recvMsgRequestTxIds TokNonBlocking ackNo reqNo
      case reply of
        SendMsgReplyTxIds (NonBlockingReply txids) client' -> do
          server' <- serverNext
          directSender (enqueue (CollectTxIds reqNo txids) q) server' client'

    directSender q (SendMsgRequestTxsPipelined txids serverNext)
                   ClientStIdle{recvMsgRequestTxs} = do
      server' <- serverNext
      SendMsgReplyTxs txs client' <- recvMsgRequestTxs txids
      directSender (enqueue (CollectTxs txids txs) q) server' client'

    directSender q (CollectPipelined (Just server') _) client =
      directSender q server' client

    directSender (ConsQ c q) (CollectPipelined _ collect) client = do
      server' <- collect c
      directSender q server' client

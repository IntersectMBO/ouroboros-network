{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Ouroboros.Network.Protocol.TxSubmission2.Direct (directPipelined) where

import           Data.Type.Queue (Queue, SingQueueF (..), (|>))
import           Data.Word (Word16)

import           Ouroboros.Network.Protocol.TxSubmission2.Client
import           Ouroboros.Network.Protocol.TxSubmission2.Server
import           Ouroboros.Network.Protocol.TxSubmission2.Type

data F txid tx st st' where
    FTxIds :: Word16
           -> [(txid, TxSizeInBytes)]
           -> F txid tx (StTxIds NonBlocking) StIdle

    FTx    :: [txid]
           -> [tx]
           -> F txid tx StTxs StIdle

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
    directSender SingEmptyF server client
  where
    directSender :: forall (q :: Queue (TxSubmission2 txid tx)).
                    SingQueueF (F txid tx) q
                 -> ServerStIdle txid tx q m a
                 -> ClientStIdle txid tx   m b
                 -> m (a, b)
    directSender q (SendMsgRequestTxIdsBlocking ackNo reqNo a serverNext)
                   ClientStIdle{recvMsgRequestTxIds} = do
      reply <- recvMsgRequestTxIds SingBlocking ackNo reqNo
      case reply of
        SendMsgReplyTxIds (BlockingReply txids) client' -> do
          server' <- serverNext txids
          directSender q server' client'
        SendMsgDone b -> (,b) <$> a

    directSender q (SendMsgRequestTxIdsPipelined ackNo reqNo serverNext)
                   ClientStIdle{recvMsgRequestTxIds} = do
      reply <- recvMsgRequestTxIds SingNonBlocking ackNo reqNo
      case reply of
        SendMsgReplyTxIds (NonBlockingReply txids) client' -> do
          server' <- serverNext
          directSender (q |> FTxIds reqNo txids) server' client'

    directSender q (SendMsgRequestTxsPipelined txids serverNext)
                   ClientStIdle{recvMsgRequestTxs} = do
      server' <- serverNext
      SendMsgReplyTxs txs client' <- recvMsgRequestTxs txids
      directSender (q |> FTx txids txs) server' client'

    directSender q (CollectPipelined (Just server') _ _) client =
      directSender q server' client

    directSender (SingConsF (FTxIds reqNo txids) q)
                 (CollectPipelined _ collectTxIds _)
                 client = do
      server' <- collectTxIds reqNo txids
      directSender q server' client

    directSender (SingConsF (FTx txids txs) q)
                 (CollectPipelined _ _ collectTxs)
                 client = do
      server' <- collectTxs txids txs
      directSender q server' client

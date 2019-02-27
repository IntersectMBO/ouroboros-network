{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.Protocol.TxSubmission.Direct where

import           Network.TypedProtocol.Proofs

import           Ouroboros.Network.Protocol.TxSubmission.Client
import           Ouroboros.Network.Protocol.TxSubmission.Server


direct
  :: forall hash tx m a b.
     Monad m
  => TxSubmissionClientPipelined hash tx m a
  -> TxSubmissionServer hash tx m b
  -> m (a, b)
direct (TxSubmissionClientPipelined client) (TxSubmissionServer mserver) =
    mserver >>= directSender EmptyQ client
  where
    directSender :: Queue n (Either [hash] tx)
                 -> TxSubmissionSender   hash tx n (Collection hash tx) m a
                 -> TxSubmissionHandlers hash tx                        m b
                 -> m (a, b)
    directSender q (SendMsgGetHashes n next) TxSubmissionHandlers {getHashes} = do
      (hs, handlers) <- getHashes n
      sender <- next hs
      directSender q sender handlers

    directSender q (SendMsgGetHashesPipelined n next) TxSubmissionHandlers {getHashes} = do
      (hs, handlers) <- getHashes n
      sender <- next
      directSender (enqueue (Left hs) q) sender handlers

    directSender q (SendMsgGetTx hash next) TxSubmissionHandlers {getTx} = do
      (tx, handlers) <- getTx hash
      sender <- next
      directSender (enqueue (Right tx) q) sender handlers

    directSender q (CollectPipelined (Just next) _) handlers =
      directSender q next handlers

    directSender (ConsQ c q) (CollectPipelined _ collect) handlers = do
      sender <- collect c
      directSender q sender handlers

    directSender _ (SendMsgDone a) TxSubmissionHandlers {done = b} =
      return (a, b)

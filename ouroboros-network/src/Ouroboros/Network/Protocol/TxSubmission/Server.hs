{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.Protocol.TxSubmission.Server
  ( TxSubmissionServerPipelined (..)
  , TxSubmissionSender (..)
  , Collection
  , txSubmissionServerPeerPipelined
  , txSubmissionSender
  )
  where

import           Data.Word (Word16)

import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Pipelined

import           Ouroboros.Network.Protocol.TxSubmission.Type

--
-- Pipelined server
--

-- | This type represents what @'PeerReceiver'@ will collect, since
-- tx-submission server can pipeline both @'SendMsgGetHash'@ and @'MsgGetTx'@
-- we need a sum of both @[hash]@ and @tx@ types.
--
type Collection hash tx = Either [hash] tx

data TxSubmissionServerPipelined hash tx m a where
  TxSubmissionServerPipelined
    :: TxSubmissionSender hash tx Z (Collection hash tx) m a
    -> TxSubmissionServerPipelined hash tx m a

-- |
-- @'TxSubmissionSender'@ can be transformed into a pipelined @'PeerSender'@.
-- We are only pipelining trasactions requests, e.g. @'MsgTx'@.
-- 
-- @c@ variable will be instantiated with @'Collect' hash tx@.
data TxSubmissionSender hash tx (n :: N) c m a where

  -- |
  -- Ask the server for a list of transaction hashes by sending @'MsgGetHashes'@.
  --
  SendMsgGetHashes
    :: Word16
    -> ([hash] -> m (TxSubmissionSender hash tx n c m a))
    -> TxSubmissionSender hash tx n c m a

  -- |
  -- Piplined version of @'SendMsgGetHashes'@.
  --
  SendMsgGetHashesPipelined
    :: Word16
    -> m (TxSubmissionSender hash tx (S n) c m a)
    -> TxSubmissionSender hash tx    n c m a

  -- |
  -- Possibly pipelined @'MsgGetTx'@..
  --
  SendMsgGetTx
    :: hash
    -> m (TxSubmissionSender hash tx (S n) c m a)
    -> TxSubmissionSender hash tx    n  c m a

  -- |
  -- Collect pipelined responses, either @'MsgSendHashes'@ or @'MsgTx'@.
  --
  CollectPipelined
    ::    Maybe (TxSubmissionSender hash tx (S n) c m a)
    -> (c ->  m (TxSubmissionSender hash tx    n  c m a))
    ->           TxSubmissionSender hash tx (S n) c m a

  -- |
  -- Terminate the tx-submission protocol.
  --
  SendMsgDone
    :: a
    -> TxSubmissionSender hash tx Z c m a

-- |
-- Transform a @'TxSubmissionServerPipelined'@ into a @'PeerPipelined'@ which
-- pipelines @'MsgGetTx'@ messages.
--
txSubmissionServerPeerPipelined
    :: forall hash tx m a.
       Functor m
    => TxSubmissionServerPipelined hash tx m a
    -> PeerPipelined (TxSubmission hash tx) AsServer StIdle m a
txSubmissionServerPeerPipelined (TxSubmissionServerPipelined sender) =
  PeerPipelined $ txSubmissionSender sender

-- |
-- The @'PeerSender'@ which asks for available transaction hashes and pipelines
-- @'MsgGetTx'@s.
--
txSubmissionSender
    :: forall hash tx (n :: N) m a.
       Functor m
    => TxSubmissionSender hash tx n (Collection hash tx) m a
    -> PeerSender (TxSubmission hash tx) AsServer StIdle n (Collection hash tx) m a

txSubmissionSender (SendMsgDone a) =
    SenderYield (ServerAgency TokIdle) MsgDone (SenderDone TokDone a)

txSubmissionSender (SendMsgGetHashes n next) =
    SenderPipeline
      (ServerAgency TokIdle)
      (MsgGetHashes n)
      (ReceiverAwait (ClientAgency TokSendHashes) $ \msg -> case msg of
         MsgSendHashes hs -> ReceiverDone (Left hs))
      (SenderCollect Nothing
        $ \c -> case c of
            Left hs -> SenderEffect (txSubmissionSender <$> (next hs))
            _       -> error "txSubmissionSender: impossible happend")

txSubmissionSender (SendMsgGetHashesPipelined n next) =
    SenderPipeline
      (ServerAgency TokIdle)
      (MsgGetHashes n)
      (ReceiverAwait (ClientAgency TokSendHashes) $ \msg -> case msg of
        MsgSendHashes hs -> ReceiverDone (Left hs))
      (SenderEffect $ txSubmissionSender <$> next)

txSubmissionSender (SendMsgGetTx hash next) =
    SenderPipeline
      (ServerAgency TokIdle)
      (MsgGetTx hash)
      (ReceiverAwait (ClientAgency TokSendTx) $ \msg -> case msg of
        MsgTx tx -> ReceiverDone (Right tx))
      (SenderEffect $ txSubmissionSender <$> next)

txSubmissionSender (CollectPipelined next collect) =
    SenderCollect
      (txSubmissionSender <$> next)
      (SenderEffect . fmap txSubmissionSender . collect)

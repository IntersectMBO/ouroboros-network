{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ouroboros.Network.Protocol.BlockFetch.Client where

import Protocol.Core
import Pipes (Producer)
import qualified Pipes

import Ouroboros.Network.Protocol.BlockFetch.Type

{-------------------------------------------------------------------------------
  Client stream of @'BlockRequestProtocol'@ protocol
-------------------------------------------------------------------------------}

newtype BlockRequestSender range m a = BlockRequestSender {
    runBlockRequestSender :: m (BlockFetchClientRequest range m a)
  }

data BlockFetchClientRequest range m a where

  -- | Send a request for a range of blocks.
  --
  BlockFetchClientRange
    :: range
    -> BlockRequestSender range m a
    -> BlockFetchClientRequest range m a

  -- | The client decided to end the protocol.
  --
  BlockFetchClientDone
    :: a
    -> BlockFetchClientRequest range m a

blockRequestSenderFromProducer
  :: Monad m
  => Producer range m a
  -> BlockRequestSender range m a
blockRequestSenderFromProducer producer = BlockRequestSender $
  Pipes.next producer >>= \nxt -> case nxt of
    Left a                   -> return $ BlockFetchClientDone a
    Right (range, producer') -> return $ BlockFetchClientRange range (blockRequestSenderFromProducer producer')

blockRequestSenderStream
  :: ( Monad m )
  => BlockRequestSender range m a
  -> Peer BlockRequestProtocol
      (BlockRequestMessage range)
      (Yielding StClientIdle)
      (Finished StClientDone)
      m a
blockRequestSenderStream (BlockRequestSender sender) = lift $ sender >>= return . request
 where
  request (BlockFetchClientRange range next) =
    part (MessageRequestRange range) $
      (blockRequestSenderStream next)
  request (BlockFetchClientDone a) = out MessageRequestDone (done a)

{-------------------------------------------------------------------------------
  Client stream of @'BlockFetchProtocol'@ protocol
-------------------------------------------------------------------------------}

-- | Handlers for server responses>
--
data BlockFetchReceiver block m a =
     BlockFetchReceiver {

       -- | The server has started streaming blocks.
       --
       recvMsgStartBatch :: m (BlockFetchReceiveBlocks block m a),

       -- | The block fetch request failed because a requested range was not on
       -- a producer's chain.
       --
       recvMsgNoBlocks   :: m (BlockFetchReceiver block m a),

       -- | The server terminated the protocol.
       --
       recvMsgDoneClient :: a
     }

-- | Block download handlers.
--
data BlockFetchReceiveBlocks block m a =
     BlockFetchReceiveBlocks {
       recvMsgBlock       :: block -> m (BlockFetchReceiveBlocks block m a),
       recvMsgBatchDone   :: m (BlockFetchReceiver block m a),
       recvMsgServerError :: m (BlockFetchReceiver block m a)
     }

-- | Construct @'Peer' 'BlockFetchProtocol'@ from
-- @'BlockFetchReceiver'@.
--
blockFetchReceiverStream
  :: forall m block a.
     ( Functor m )
  => BlockFetchReceiver block m a
  -> Peer BlockFetchProtocol
      (BlockFetchMessage block)
      (Awaiting StServerAwaiting)
      (Finished StServerDone)
      m a
blockFetchReceiverStream BlockFetchReceiver{recvMsgStartBatch,recvMsgNoBlocks,recvMsgDoneClient} =
  await $ \msg -> case msg of
    MessageStartBatch -> lift (fetchBlocks <$> recvMsgStartBatch)
    MessageNoBlocks   -> lift (blockFetchReceiverStream <$> recvMsgNoBlocks)
    MessageServerDone -> done recvMsgDoneClient
 where
  fetchBlocks
    :: BlockFetchReceiveBlocks block m a
    -> Peer BlockFetchProtocol
        (BlockFetchMessage block)
        (Awaiting StServerSending)
        (Finished StServerDone)
        m a
  fetchBlocks BlockFetchReceiveBlocks{recvMsgBlock,recvMsgBatchDone,recvMsgServerError} =
    await $ \msg -> case msg of
      MessageBlock block -> lift (fetchBlocks <$> recvMsgBlock block)
      MessageBatchDone   -> lift (blockFetchReceiverStream <$> recvMsgBatchDone)
      MessageServerError -> lift (blockFetchReceiverStream <$> recvMsgServerError)

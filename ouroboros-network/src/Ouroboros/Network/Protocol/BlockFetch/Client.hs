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
  Client stream of @'BlockFetchClientProtocol'@ protocol
-------------------------------------------------------------------------------}

newtype BlockFetchClientSender range m a = BlockFetchClientSender {
    runBlockFetchClientSender :: m (BlockFetchClientRequest range m a)
  }

data BlockFetchClientRequest range m a where

  -- | Send a request for a range of blocks.
  --
  BlockFetchClientRange
    :: range
    -> BlockFetchClientSender range m a
    -> BlockFetchClientRequest range m a

  -- | The client decided to end the protocol.
  --
  BlockFetchClientDone
    :: a
    -> BlockFetchClientRequest range m a

blockFetchClientSenderFromProducer
  :: Monad m
  => Producer range m a
  -> BlockFetchClientSender range m a
blockFetchClientSenderFromProducer producer = BlockFetchClientSender $
  Pipes.next producer >>= \nxt -> case nxt of
    Left a                   -> return $ BlockFetchClientDone a
    Right (range, producer') -> return $ BlockFetchClientRange range (blockFetchClientSenderFromProducer producer')

blockFetchClientSenderStream
  :: ( Monad m )
  => BlockFetchClientSender range m a
  -> Peer BlockFetchClientProtocol
      (BlockRequestClientMessage range)
      (Yielding StClientIdle)
      (Finished StClientDone)
      m a
blockFetchClientSenderStream (BlockFetchClientSender sender) = lift $ sender >>= return . request
 where
  request (BlockFetchClientRange range next) =
    part (MessageRequestRange range) $
      (blockFetchClientSenderStream next)
  request (BlockFetchClientDone a) = out MessageDone (done a)

{-------------------------------------------------------------------------------
  Client stream of @'BlockFetchServerProtocol'@ protocol
-------------------------------------------------------------------------------}

-- | Handlers for server responses>
--
data BlockFetchClientReceiver block m a =
     BlockFetchClientReceiver {

       -- | The server has started streaming blocks.
       --
       recvMsgStartBatch :: m (BlockFetchClientReceiveBlocks block m a),

       -- | The block fetch request failed because a requested range was not on
       -- a producer's chain.
       --
       recvMsgNoBlocks   :: m (BlockFetchClientReceiver block m a),

       -- | The server terminated the protocol.
       --
       recvMsgDoneClient :: a
     }

-- | Block download handlers.
--
data BlockFetchClientReceiveBlocks block m a =
     BlockFetchClientReceiveBlocks {
       recvMsgBlock       :: block -> m (BlockFetchClientReceiveBlocks block m a),
       recvMsgBatchDone   :: m (BlockFetchClientReceiver block m a),
       recvMsgServerError :: m (BlockFetchClientReceiver block m a)
     }

-- | Construct @'Peer' 'BlockFetchServerProtocol'@ from
-- @'BlockFetchClientReceiver'@.
--
blockFetchClientReceiverStream
  :: forall m block a.
     ( Functor m )
  => BlockFetchClientReceiver block m a
  -> Peer BlockFetchServerProtocol
      (BlockRequestServerMessage block)
      (Awaiting StServerAwaiting)
      (Finished StServerDone)
      m a
blockFetchClientReceiverStream BlockFetchClientReceiver{recvMsgStartBatch,recvMsgNoBlocks,recvMsgDoneClient} =
  await $ \msg -> case msg of
    MessageStartBatch -> lift (fetchBlocks <$> recvMsgStartBatch)
    MessageNoBlocks   -> lift (blockFetchClientReceiverStream <$> recvMsgNoBlocks)
    MessageServerDone -> done recvMsgDoneClient
 where
  fetchBlocks
    :: BlockFetchClientReceiveBlocks block m a
    -> Peer BlockFetchServerProtocol
        (BlockRequestServerMessage block)
        (Awaiting StServerSending)
        (Finished StServerDone)
        m a
  fetchBlocks BlockFetchClientReceiveBlocks{recvMsgBlock,recvMsgBatchDone,recvMsgServerError} =
    await $ \msg -> case msg of
      MessageBlock block -> lift (fetchBlocks <$> recvMsgBlock block)
      MessageBatchDone   -> lift (blockFetchClientReceiverStream <$> recvMsgBatchDone)
      MessageServerError -> lift (blockFetchClientReceiverStream <$> recvMsgServerError)

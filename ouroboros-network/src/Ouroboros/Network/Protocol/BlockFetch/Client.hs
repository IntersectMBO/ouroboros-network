{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ouroboros.Network.Protocol.BlockFetch.Client where

import Protocol.Core

import Ouroboros.Network.Protocol.BlockFetch.Type

{-------------------------------------------------------------------------------
  Client stream of @'BlockFetchClientProtocol'@ protocol
-------------------------------------------------------------------------------}

data BlockFetchClientSender header m a where

  -- | Send a request for a range of blocks.
  --
  SendBlockRequestMsg
    :: ChainRange header
    -> m (BlockFetchClientSender header m a)
    -> BlockFetchClientSender header m a

  -- | The client decided to end the protocol.
  --
  SendMsgDone
    :: a
    -> BlockFetchClientSender header m a

blockFetchClientSenderStream
  :: ( Functor m )
  => BlockFetchClientSender header m a
  -> Peer BlockFetchClientProtocol
      (BlockRequestClientMessage header)
      (Yielding StClientIdle)
      (Finished StClientDone)
      m a
blockFetchClientSenderStream (SendBlockRequestMsg range next) =
  part (MessageRequestRange range) $
    lift (blockFetchClientSenderStream <$> next)
blockFetchClientSenderStream (SendMsgDone a) = out MessageDone (done a)

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
  :: forall m block header a.
     ( Functor m )
  => BlockFetchClientReceiver block m a
  -> Peer BlockFetchServerProtocol
      (BlockRequestServerMessage header block)
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
        (BlockRequestServerMessage header block)
        (Awaiting StServerSending)
        (Finished StServerDone)
        m a
  fetchBlocks BlockFetchClientReceiveBlocks{recvMsgBlock,recvMsgBatchDone,recvMsgServerError} =
    await $ \msg -> case msg of
      MessageBlock block -> lift (fetchBlocks <$> recvMsgBlock block)
      MessageBatchDone   -> lift (blockFetchClientReceiverStream <$> recvMsgBatchDone)
      MessageServerError -> lift (blockFetchClientReceiverStream <$> recvMsgServerError)

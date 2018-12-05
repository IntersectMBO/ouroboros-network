{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.Protocol.BlockFetch.Server
  ( BlockFetchServerReceiver (..)
  , constantReceiver
  , blockFetchServerReceiverStream
  , BlockFetchServer (..)
  , BlockFetchSender (..)
  , BlockFetchSendBlocks (..)
  , blockFetchServerStream
  , connectThroughQueue
  )
  where

import Data.Functor (($>))
import Numeric.Natural (Natural)
import Pipes (Producer)
import qualified Pipes

import Protocol.Core

import Ouroboros.Network.MonadClass.MonadSTM (MonadSTM (..))

import Ouroboros.Network.Protocol.BlockFetch.Type

{-------------------------------------------------------------------------------
  Server stream of @'BlockFetchClientProtocol'@ protocol
-------------------------------------------------------------------------------}

data BlockFetchServerReceiver header m a =
  BlockFetchServerReceiver {
    -- | handler for @'MessageRequestRange'@ of the @'BlockFetchClientProtocol'@
    --
    recvMessageRequestRange :: ChainRange header -> m (BlockFetchServerReceiver header m a),
    -- | handler for @'MessageDone'@ of the @'BlockFetchClientProtocol'@
    --
    recvMessageDone :: a
  }

-- | A receiver which applies an action to the received input, e.g. writting the
-- received @'ChainRange' header@ to an internal queue.
--
constantReceiver
  :: Functor m
  => (ChainRange header -> m ())
  -> a
  -> BlockFetchServerReceiver header m a
constantReceiver handleRequest recvMessageDone = BlockFetchServerReceiver {
    recvMessageRequestRange = \range -> handleRequest range $> constantReceiver handleRequest recvMessageDone,
    recvMessageDone
  }

blockFetchServerReceiverStream
  :: forall header m a.
     Functor m
  => BlockFetchServerReceiver header m a
  -> Peer BlockFetchClientProtocol
      (BlockRequestClientMessage header)
        ('Awaiting 'StClientIdle)
        ('Finished 'StClientDone)
        m a
blockFetchServerReceiverStream BlockFetchServerReceiver{recvMessageRequestRange,recvMessageDone} =
  await $ \msg -> case msg of
    MessageRequestRange range -> lift (blockFetchServerReceiverStream <$> recvMessageRequestRange range)
    MessageDone -> done recvMessageDone

{-------------------------------------------------------------------------------
  Server stream of @'BlockFetchServerProtocol'@ protocol
-------------------------------------------------------------------------------}

-- | @'BlockFetchServer'@ serves blocks to the corresponding client.
--
newtype BlockFetchServer header block m a = BlockFetchServer {
    runBlockFetchServer :: m (BlockFetchSender header block m a)
  }

-- | Send batches of blocks, when a batch is sent loop using
-- @'BlockFetchServer'@.
--
data BlockFetchSender header block m a where

  -- | Initiate a batch of blocks.
  SendMessageStartBatch
    :: block
    -> m (BlockFetchSendBlocks header block m a)
    -> BlockFetchSender header block m a

  -- | We served a batch, now loop using @'BlockFetchServer'@
  SendMessageNoBlocks
    :: BlockFetchServer header block m a
    -> BlockFetchSender header block m a

  -- | Server decided to end the protocol.
  SendMessageDone
    :: a
    -> BlockFetchSender header block m a

-- | Stream batch of blocks until 
--
data BlockFetchSendBlocks header block m a where

  -- | Send a single block and recurse.
  --
  SendMessageBlock
    :: block
    -> m (BlockFetchSendBlocks header block m a)
    -> BlockFetchSendBlocks header block m a

  -- | End of the stream of blocks.
  --
  SendMessageBatchDone
    :: BlockFetchServer header block m a
    -> BlockFetchSendBlocks header block m a

-- | Interpratation of @'BlockFetchServer'@ as a @'Peer'@ of the
-- @'BlockFetchServerProtocol'@ protocol.
--
blockFetchServerStream
  :: forall header block m a.
     Functor m
  => BlockFetchServer header block m a
  -> Peer BlockFetchServerProtocol
      (BlockRequestServerMessage header block)
      ('Yielding 'StServerAwaiting)
      ('Finished 'StServerDone)
      m a
blockFetchServerStream server = lift $ handleStAwait <$> runBlockFetchServer server
 where
  handleStAwait
    :: BlockFetchSender header block m a
    -> Peer BlockFetchServerProtocol
        (BlockRequestServerMessage header block)
        ('Yielding 'StServerAwaiting)
        ('Finished 'StServerDone)
        m a
  handleStAwait (SendMessageStartBatch block msender)
    = part (MessageStartBatch block) (lift $ sendBlocks <$> msender)
  handleStAwait (SendMessageNoBlocks server') = blockFetchServerStream server'
  handleStAwait (SendMessageDone a)           = out MessageServerDone (done a)

  sendBlocks
    :: BlockFetchSendBlocks header block m a
    -> Peer BlockFetchServerProtocol
        (BlockRequestServerMessage header block)
        ('Yielding 'StServerSending)
        ('Finished 'StServerDone)
        m a
  sendBlocks (SendMessageBlock block msender) =
    part (MessageBlock block) (lift $ sendBlocks <$> msender)
  sendBlocks (SendMessageBatchDone server') =
    part MessageBatchDone (blockFetchServerStream server')

-- | Connection between the server side of @'BlockFetchClientProtocol'@ and the
-- server side of @'BlockFetchServerProtocol'@>
--
connectThroughQueue'
  :: forall header block m.
     MonadSTM m
  => TBQueue m (ChainRange header)
  -> (ChainRange header -> m (Maybe (Producer block m ())))
  -> ( BlockFetchServerReceiver header m ()
     , BlockFetchServer header block m ()
     )
connectThroughQueue' queue blockStream = (receiver, server)
 where
  receiver :: BlockFetchServerReceiver header m ()
  receiver = constantReceiver (atomically . writeTBQueue queue) ()

  server :: BlockFetchServer header block m ()
  server = BlockFetchServer $ do
    mstream <- atomically (readTBQueue queue) >>= blockStream
    case mstream :: Maybe (Producer block m ()) of
      Nothing     -> return $ SendMessageNoBlocks server
      Just stream -> do
        nxt <- Pipes.next stream
        case nxt of
          Left _                 -> return $ SendMessageNoBlocks server
          Right (block, stream') -> return $ SendMessageStartBatch block (sendStream stream')

  sendStream
    :: Producer block m ()
    -> m (BlockFetchSendBlocks header block m ())
  sendStream stream = do
    nxt <- Pipes.next stream
    case nxt of
      Left _             -> return $ SendMessageBatchDone server
      Right (b, stream') -> return $ SendMessageBlock b (sendStream stream')

-- | Connect server side of @'BlockFetchClientProtocol'@ and
-- @'BlockFetchSErverProtocol'@ thought a freshly constructed @'TBQueue'@.
--
connectThroughQueue
  :: forall header block m.
     MonadSTM m
  => Natural
  -- ^ queue size
  -> (ChainRange header -> m (Maybe (Producer block m ())))
  -> m ( BlockFetchServerReceiver header m ()
       , BlockFetchServer header block m ()
       )
connectThroughQueue queueSize blockStream = do
  queue <- atomically $ newTBQueue queueSize
  return $ connectThroughQueue' queue blockStream

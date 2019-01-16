{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Ouroboros.Network.Protocol.BlockFetch.Server
  ( BlockFetchServerReceiver (..)
  , constantReceiver
  , blockFetchServerReceiverStream
  , blockFetchServerReceiverPipe
  , BlockFetchServerSender (..)
  , BlockFetchSender (..)
  , BlockFetchSendBlocks (..)
  , blockFetchServerStream
  , blockFetchServerProducer
  , connectThroughQueue
  )
  where

import Control.Monad (join)
import Data.Functor (($>))
import Data.Void (Void)
import Numeric.Natural (Natural)
import Pipes (Producer', Pipe)
import qualified Pipes

import Control.Monad.Class.MonadSTM (MonadSTM (..))

import Protocol.Core

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
        (Awaiting StClientIdle)
        (Finished StClientDone)
        m a
blockFetchServerReceiverStream BlockFetchServerReceiver{recvMessageRequestRange,recvMessageDone} =
  await $ \msg -> case msg of
    MessageRequestRange range -> lift (blockFetchServerReceiverStream <$> recvMessageRequestRange range)
    MessageDone -> done recvMessageDone

blockFetchServerReceiverPipe
  :: forall header block m a.
     Monad m
  => (ChainRange header -> Pipes.Producer' block m a)
  -> BlockFetchServerReceiver header m Void
  -> Pipe (ChainRange header) block m a
blockFetchServerReceiverPipe blockStream BlockFetchServerReceiver {recvMessageRequestRange} = do
  range <- Pipes.await
  _ <- blockStream range
  join $ Pipes.lift $ blockFetchServerReceiverPipe blockStream <$> recvMessageRequestRange range

{-------------------------------------------------------------------------------
  Server stream of @'BlockFetchServerProtocol'@ protocol
-------------------------------------------------------------------------------}

-- | @'BlockFetchServer'@ serves blocks to the corresponding client.
--
newtype BlockFetchServerSender header block m a = BlockFetchServerSender {
    runBlockFetchServerSender :: m (BlockFetchSender header block m a)
  }

-- | Send batches of blocks, when a batch is sent loop using
-- @'BlockFetchServer'@.
--
data BlockFetchSender header block m a where

  -- | Initiate a batch of blocks.
  SendMessageStartBatch
    :: m (BlockFetchSendBlocks header block m a)
    -> BlockFetchSender header block m a

  -- | We served a batch, now loop using @'BlockFetchServerSender'@
  SendMessageNoBlocks
    :: BlockFetchServerSender header block m a
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
    :: BlockFetchServerSender header block m a
    -> BlockFetchSendBlocks header block m a

-- | Interpratation of @'BlockFetchServerSender'@ as a @'Peer'@ of the
-- @'BlockFetchServerProtocol'@ protocol.
--
blockFetchServerStream
  :: forall header block m a.
     Functor m
  => BlockFetchServerSender header block m a
  -> Peer BlockFetchServerProtocol
      (BlockRequestServerMessage header block)
      (Yielding StServerAwaiting)
      (Finished StServerDone)
      m a
blockFetchServerStream server = lift $ handleStAwait <$> runBlockFetchServerSender server
 where
  handleStAwait
    :: BlockFetchSender header block m a
    -> Peer BlockFetchServerProtocol
        (BlockRequestServerMessage header block)
        (Yielding StServerAwaiting)
        (Finished StServerDone)
        m a
  handleStAwait (SendMessageStartBatch msender)
    = part MessageStartBatch (lift $ sendBlocks <$> msender)
  handleStAwait (SendMessageNoBlocks server') = blockFetchServerStream server'
  handleStAwait (SendMessageDone a)           = out MessageServerDone (done a)

  sendBlocks
    :: BlockFetchSendBlocks header block m a
    -> Peer BlockFetchServerProtocol
        (BlockRequestServerMessage header block)
        (Yielding StServerSending)
        (Finished StServerDone)
        m a
  sendBlocks (SendMessageBlock block msender) =
    part (MessageBlock block) (lift $ sendBlocks <$> msender)
  sendBlocks (SendMessageBatchDone server') =
    part MessageBatchDone (blockFetchServerStream server')

blockFetchServerProducer
  :: forall header block m a. Monad m
  => BlockFetchServerSender header block m a
  -> Producer' block m a
blockFetchServerProducer (BlockFetchServerSender mserver) =
  join $ Pipes.lift (sendBlocks <$> mserver)
 where
  sendBlocks :: BlockFetchSender header block m a
             -> Producer' block m a
  sendBlocks (SendMessageStartBatch msender) =
    join $ Pipes.lift $ streamBlocks <$> msender
  sendBlocks (SendMessageNoBlocks server) = blockFetchServerProducer server
  sendBlocks (SendMessageDone a) = return a

  streamBlocks :: BlockFetchSendBlocks header block m a
               -> Producer' block m a
  streamBlocks (SendMessageBlock block mstreamer) = do
    Pipes.yield block
    join $ Pipes.lift (streamBlocks <$> mstreamer)
  streamBlocks (SendMessageBatchDone sender) = blockFetchServerProducer sender

-- | Connection between the server side of @'BlockFetchClientProtocol'@ and the
-- server side of @'BlockFetchServerProtocol'@>
--
connectThroughQueue'
  :: forall header block m.
     MonadSTM m
  => TBQueue m (ChainRange header)
  -> (ChainRange header -> Pipes.Producer' block m ())
  -> ( BlockFetchServerReceiver header m ()
     , BlockFetchServerSender header block m ()
     )
connectThroughQueue' queue blockStream = (receiver, server)
 where
  receiver :: BlockFetchServerReceiver header m ()
  receiver = constantReceiver (atomically . writeTBQueue queue) ()

  server :: BlockFetchServerSender header block m ()
  server = BlockFetchServerSender $ do
    stream <- atomically (readTBQueue queue) >>= Pipes.next . blockStream
    case stream of
      Left _             -> return $ SendMessageNoBlocks server
      Right (b, stream') -> return $ SendMessageStartBatch (sendStream b stream')

  sendStream
    :: block
    -> Pipes.Producer block m ()
    -> m (BlockFetchSendBlocks header block m ())
  sendStream b stream =
    return $ SendMessageBlock b $ do
    nxt <- Pipes.next stream
    case nxt of
      Left _              -> return $ SendMessageBatchDone server
      Right (b', stream') -> sendStream b' stream'

-- | Connect server side of @'BlockFetchClientProtocol'@ and
-- @'BlockFetchSErverProtocol'@ thought a freshly constructed @'TBQueue'@.
--
connectThroughQueue
  :: forall header block m.
     MonadSTM m
  => Natural
  -- ^ queue size
  -> (ChainRange header -> Producer' block m ())
  -> m ( BlockFetchServerReceiver header m ()
       , BlockFetchServerSender header block m ()
       )
connectThroughQueue queueSize blockStream = do
  queue <- atomically $ newTBQueue queueSize
  return $ connectThroughQueue' queue blockStream

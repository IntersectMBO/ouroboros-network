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
  , blockFetchServerSender
  , blockFetchServerSenderToProducer
  , connectThroughQueue
  )
  where

import Control.Monad (join)
import Data.Functor (($>))
import Data.Void (Void)
import Numeric.Natural (Natural)
import Pipes (Producer', Pipe)
import qualified Pipes

import Protocol.Core

import Ouroboros.Network.MonadClass.MonadSTM (MonadSTM (..))

import Ouroboros.Network.Protocol.BlockFetch.Type

{-------------------------------------------------------------------------------
  Server stream of @'BlockFetchClientProtocol'@ protocol
-------------------------------------------------------------------------------}

data BlockFetchServerReceiver range m a =
  BlockFetchServerReceiver {
    -- | handler for @'MessageRequestRange'@ of the @'BlockFetchClientProtocol'@
    --
    recvMessageRequestRange :: range -> m (BlockFetchServerReceiver range m a),
    -- | handler for @'MessageDone'@ of the @'BlockFetchClientProtocol'@
    --
    recvMessageDone :: a
  }

-- | A receiver which applies an action to the received input, e.g. writting the
-- received @range@ to an internal queue.
--
constantReceiver
  :: Functor m
  => (range -> m ())
  -> a
  -> BlockFetchServerReceiver range m a
constantReceiver handleRequest recvMessageDone = BlockFetchServerReceiver {
    recvMessageRequestRange = \range -> handleRequest range $> constantReceiver handleRequest recvMessageDone,
    recvMessageDone
  }

blockFetchServerReceiverStream
  :: forall range m a.
     Functor m
  => BlockFetchServerReceiver range m a
  -> Peer BlockFetchClientProtocol
      (BlockRequestClientMessage range)
        (Awaiting StClientIdle)
        (Finished StClientDone)
        m a
blockFetchServerReceiverStream BlockFetchServerReceiver{recvMessageRequestRange,recvMessageDone} =
  await $ \msg -> case msg of
    MessageRequestRange range -> lift (blockFetchServerReceiverStream <$> recvMessageRequestRange range)
    MessageDone -> done recvMessageDone

blockFetchServerReceiverPipe
  :: forall range block m a.
     Monad m
  => (range -> Pipes.Producer' block m a)
  -> BlockFetchServerReceiver range m Void
  -> Pipe range block m a
blockFetchServerReceiverPipe blockStream BlockFetchServerReceiver {recvMessageRequestRange} = do
  range <- Pipes.await
  _ <- blockStream range
  join $ Pipes.lift $ blockFetchServerReceiverPipe blockStream <$> recvMessageRequestRange range

{-------------------------------------------------------------------------------
  Server stream of @'BlockFetchServerProtocol'@ protocol
-------------------------------------------------------------------------------}

-- | @'BlockFetchServer'@ serves blocks to the corresponding client.
newtype BlockFetchServerSender block m a = BlockFetchServerSender {
    runBlockFetchServerSender :: m (BlockFetchSender block m a)
  }

-- | Send batches of blocks, when a batch is sent loop using
-- @'BlockFetchServer'@.
--
data BlockFetchSender block m a where

  -- | Initiate a batch of blocks.
  SendMessageStartBatch
    :: m (BlockFetchSendBlocks block m a)
    -> BlockFetchSender block m a

  -- | We served a batch, now loop using @'BlockFetchServerSender'@
  SendMessageNoBlocks
    :: BlockFetchServerSender block m a
    -> BlockFetchSender block m a

  -- | Server decided to end the protocol.
  SendMessageServerDone
    :: a
    -> BlockFetchSender block m a

-- | Stream batch of blocks until
--
data BlockFetchSendBlocks block m a where

  -- | Send a single block and recurse.
  --
  SendMessageBlock
    :: block
    -> m (BlockFetchSendBlocks block m a)
    -> BlockFetchSendBlocks block m a

  -- | End of the stream of blocks.
  --
  SendMessageBatchDone
    :: BlockFetchServerSender block m a
    -> BlockFetchSendBlocks block m a

-- | Interpratation of @'BlockFetchServerSender'@ as a @'Peer'@ of the
-- @'BlockFetchServerProtocol'@ protocol.
--
blockFetchServerStream
  :: forall block m a.
     Functor m
  => BlockFetchServerSender block m a
  -> Peer BlockFetchServerProtocol
      (BlockRequestServerMessage block)
      (Yielding StServerAwaiting)
      (Finished StServerDone)
      m a
blockFetchServerStream server = lift $ handleStAwait <$> runBlockFetchServerSender server
 where
  handleStAwait
    :: BlockFetchSender block m a
    -> Peer BlockFetchServerProtocol
        (BlockRequestServerMessage block)
        (Yielding StServerAwaiting)
        (Finished StServerDone)
        m a
  handleStAwait (SendMessageStartBatch msender)
    = part MessageStartBatch (lift $ sendBlocks <$> msender)
  handleStAwait (SendMessageNoBlocks server') = blockFetchServerStream server'
  handleStAwait (SendMessageServerDone a)     = out MessageServerDone (done a)

  sendBlocks
    :: BlockFetchSendBlocks block m a
    -> Peer BlockFetchServerProtocol
        (BlockRequestServerMessage block)
        (Yielding StServerSending)
        (Finished StServerDone)
        m a
  sendBlocks (SendMessageBlock block msender) =
    part (MessageBlock block) (lift $ sendBlocks <$> msender)
  sendBlocks (SendMessageBatchDone server') =
    part MessageBatchDone (blockFetchServerStream server')

blockFetchServerSenderToProducer
  :: forall block m a. Monad m
  => BlockFetchServerSender block m a
  -> Producer' block m a
blockFetchServerSenderToProducer (BlockFetchServerSender mserver) =
  join $ Pipes.lift (sendBlocks <$> mserver)
 where
  sendBlocks :: BlockFetchSender block m a
             -> Producer' block m a
  sendBlocks (SendMessageStartBatch msender) =
    join $ Pipes.lift $ streamBlocks <$> msender
  sendBlocks (SendMessageNoBlocks sender) = blockFetchServerSenderToProducer sender
  sendBlocks (SendMessageServerDone a) = return a

  streamBlocks :: BlockFetchSendBlocks block m a
               -> Producer' block m a
  streamBlocks (SendMessageBlock block mstreamer) = do
    Pipes.yield block
    join $ Pipes.lift (streamBlocks <$> mstreamer)
  streamBlocks (SendMessageBatchDone sender) = blockFetchServerSenderToProducer sender

blockFetchServerSender
  :: forall m range block a.
     Monad m
  => a
  -> m range
  -> (range -> m (Maybe (Pipes.Producer block m a)))
  -> BlockFetchServerSender block m a
blockFetchServerSender serverDone mrange blockStream = BlockFetchServerSender $ do
  range <- mrange
  mstream <- blockStream range
  case mstream of
    Nothing     -> return $ SendMessageServerDone serverDone
    Just stream -> do
      stream' <- Pipes.next stream
      case stream' of
        Left _             -> return $ SendMessageNoBlocks (blockFetchServerSender serverDone mrange blockStream)
        Right (b, next) -> return $ SendMessageStartBatch (sendStream b next)
 where
  sendStream
    :: block
    -> Pipes.Producer block m a
    -> m (BlockFetchSendBlocks block m a)
  sendStream b stream =
    return $ SendMessageBlock b $ do
    nxt <- Pipes.next stream
    case nxt of
      Left _              -> return $ SendMessageBatchDone (blockFetchServerSender serverDone mrange blockStream)
      Right (b', stream') -> sendStream b' stream'

-- | Connection between the server side of @'BlockFetchClientProtocol'@ and the
-- server side of @'BlockFetchServerProtocol'@>
--
connectThroughQueue'
  :: forall range block m.
     MonadSTM m
  => TBQueue m range
  -> (range -> m (Maybe (Pipes.Producer block m ())))
  -> ( BlockFetchServerReceiver range m ()
     , BlockFetchServerSender block m ()
     )
connectThroughQueue' queue blockStream = (receiver, server)
 where
  receiver :: BlockFetchServerReceiver range m ()
  receiver = constantReceiver (atomically . writeTBQueue queue) ()

  server :: BlockFetchServerSender block m ()
  server = blockFetchServerSender () (atomically $ readTBQueue queue) blockStream

-- | Connect server side of @'BlockFetchClientProtocol'@ and
-- @'BlockFetchSErverProtocol'@ thought a freshly constructed @'TBQueue'@.
--
connectThroughQueue
  :: forall range block m.
     MonadSTM m
  => Natural
  -- ^ queue size
  -> (range -> m (Maybe (Pipes.Producer block m ())))
  -> m ( BlockFetchServerReceiver range m ()
       , BlockFetchServerSender block m ()
       )
connectThroughQueue queueSize blockStream = do
  queue <- atomically $ newTBQueue queueSize
  return $ connectThroughQueue' queue blockStream

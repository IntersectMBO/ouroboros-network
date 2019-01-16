{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Ouroboros.Network.Protocol.BlockFetch.Server
  ( BlockRequestReceiver (..)
  , constantReceiver
  , blockRequestReceiverStream
  , BlockFetchServerSender (..)
  , BlockFetchSender (..)
  , BlockFetchSendBlocks (..)
  , StreamElement (..)
  , blockFetchServerStream
  , blockFetchServerSender
  , connectThroughQueue
  )
  where

import Data.Functor (($>))
import Numeric.Natural (Natural)
import qualified Pipes

import Control.Monad.Class.MonadSTM (MonadSTM (..))

import Protocol.Core

import Ouroboros.Network.Protocol.BlockFetch.Type

{-------------------------------------------------------------------------------
  Server stream of @'BlockRequestProtocol'@ protocol
-------------------------------------------------------------------------------}

data BlockRequestReceiver range m a =
  BlockRequestReceiver {
    -- | handler for @'MessageRequestRange'@ of the @'BlockRequestProtocol'@
    --
    recvMessageRequestRange :: range -> m (BlockRequestReceiver range m a),
    -- | handler for @'MessageRequestDone'@ of the @'BlockRequestProtocol'@
    --
    recvMessageDone :: m a
  }

-- | A receiver which applies an action to the received input, e.g. writting the
-- received @range@ to an internal queue.
--
constantReceiver
  :: Functor m
  => (range -> m ())
  -> m a
  -> BlockRequestReceiver range m a
constantReceiver handleRequest recvMessageDone = BlockRequestReceiver {
    recvMessageRequestRange = \range -> handleRequest range $> constantReceiver handleRequest recvMessageDone,
    recvMessageDone
  }

blockRequestReceiverStream
  :: forall range m a.
     Functor m
  => BlockRequestReceiver range m a
  -> Peer BlockRequestProtocol
        (BlockRequestMessage range)
        (Awaiting StClientIdle)
        (Finished StClientDone)
        m a
blockRequestReceiverStream BlockRequestReceiver{recvMessageRequestRange,recvMessageDone} =
  await $ \msg -> case msg of
    MessageRequestRange range -> lift (blockRequestReceiverStream <$> recvMessageRequestRange range)
    MessageRequestDone -> lift (done <$> recvMessageDone)

{-------------------------------------------------------------------------------
  Server stream of @'BlockFetchProtocol'@ protocol
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
-- @'BlockFetchProtocol'@ protocol.
--
blockFetchServerStream
  :: forall block m a.
     Functor m
  => BlockFetchServerSender block m a
  -> Peer BlockFetchProtocol
      (BlockFetchMessage block)
      (Yielding StServerAwaiting)
      (Finished StServerDone)
      m a
blockFetchServerStream server = lift $ handleStAwait <$> runBlockFetchServerSender server
 where
  handleStAwait
    :: BlockFetchSender block m a
    -> Peer BlockFetchProtocol
        (BlockFetchMessage block)
        (Yielding StServerAwaiting)
        (Finished StServerDone)
        m a
  handleStAwait (SendMessageStartBatch msender)
    = part MessageStartBatch (lift $ sendBlocks <$> msender)
  handleStAwait (SendMessageNoBlocks server') = blockFetchServerStream server'
  handleStAwait (SendMessageServerDone a)     = out MessageServerDone (done a)

  sendBlocks
    :: BlockFetchSendBlocks block m a
    -> Peer BlockFetchProtocol
        (BlockFetchMessage block)
        (Yielding StServerSending)
        (Finished StServerDone)
        m a
  sendBlocks (SendMessageBlock block msender) =
    part (MessageBlock block) (lift $ sendBlocks <$> msender)
  sendBlocks (SendMessageBatchDone server') =
    part MessageBatchDone (blockFetchServerStream server')

-- | Sender of the @'BlockFetchProtocol'@.  It may send
-- @'MessageServerDone'@ under two conditions:
--
--  * @'StreamElement' range@ returned @'End'@, which means that the
--    corresponding @'BlockRequestProtocol'@ has terminated
--  * the @'blockStream'@ handler returned @'Nothing'@
--
blockFetchServerSender
  :: forall m range block a.
     Monad m
  => a
  -- ^ return value
  -> m (StreamElement range)
  -- ^ stream of ranges; when @'End'@ is read the protocol will terminate.
  -> (range -> m (Maybe (Pipes.Producer block m a)))
  -- ^ range handler returning a stream of blocks, when it return @'Nothing'@
  -- the @'MessageServerDone'@ will terminate the protocol.
  -> BlockFetchServerSender block m a
blockFetchServerSender serverDone mrange blockStream = BlockFetchServerSender $ do
  element <- mrange
  case element of
    End           -> return $ SendMessageServerDone serverDone
    Element range -> do
      mstream <- blockStream range
      case mstream of
        Nothing     -> return $ SendMessageServerDone serverDone
        Just stream -> do
          stream' <- Pipes.next stream
          case stream' of
            Left _          -> return $ SendMessageNoBlocks (blockFetchServerSender serverDone mrange blockStream)
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

data StreamElement a
    = Element a
    | End
  deriving (Show, Eq)

-- | Connection between the server side of @'BlockRequestProtocol'@ and the
-- server side of @'BlockFetchProtocol'@>
--
connectThroughQueue'
  :: forall range block m.
     MonadSTM m
  => TBQueue m (StreamElement range)
  -> (range -> m (Maybe (Pipes.Producer block m ())))
  -> ( BlockRequestReceiver range m ()
     , BlockFetchServerSender block m ()
     )
connectThroughQueue' queue blockStream = (receiver, server)
 where
  receiver :: BlockRequestReceiver range m ()
  receiver = constantReceiver (atomically . writeTBQueue queue . Element) (atomically (writeTBQueue queue End))

  server :: BlockFetchServerSender block m ()
  server = blockFetchServerSender () (atomically $ readTBQueue queue) blockStream

-- | Connect server side of @'BlockRequestProtocol'@ and
-- @'BlockFetchSErverProtocol'@ thought a freshly constructed @'TBQueue'@.
--
connectThroughQueue
  :: forall range block m.
     MonadSTM m
  => Natural
  -- ^ queue size
  -> (range -> m (Maybe (Pipes.Producer block m ())))
  -> m ( BlockRequestReceiver range m ()
       , BlockFetchServerSender block m ()
       )
connectThroughQueue queueSize blockStream = do
  queue <- atomically $ newTBQueue queueSize
  return $ connectThroughQueue' queue blockStream

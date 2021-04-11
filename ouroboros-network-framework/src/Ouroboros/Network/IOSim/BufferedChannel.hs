{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.IOSim.BufferedChannel
  ( BufferedChannel (..)
  , Size
  , SuccessOrFailure (..)
  , Attenuation (..)
  , newConnectedBufferedChannelPair
  , CloseError (..)
  -- * Trace
  , BufferedChannelTrace (..)
  -- * Utils
  , resourceVanishedIOError
  ) where

import           Prelude hiding (read)

import           Control.Monad (when, unless)
import qualified Control.Monad.Class.MonadSTM as LazySTM
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import           Control.Monad.Class.MonadThrow
import           Control.Tracer (Tracer, traceWith)

import           GHC.IO.Exception

import qualified Data.ByteString.Lazy as BL
import           Data.Functor (($>))
import           Data.Int (Int64)
import           Data.Maybe (isJust)
import           Data.Sequence (Seq ((:<|), Empty))
import qualified Data.Sequence as Seq
import           Data.Typeable (Typeable)

import           Network.Mux.Trace (MuxError (..), MuxErrorType (..))


-- | Message frames passed through the network.
--
data Message =
      MsgClose
    | MsgBytes BL.ByteString
  deriving Eq


--
-- QueueChannel
--

-- | 'QueueChannel' is the low level bearer used by the simulated snocket.
--
-- Each read / write queues can be closed independently.  Read queue is closed
-- once 'MsgClose' is read from the queue; dually, write queue is closed once
-- 'MsgClose' is written.
--
data QueueChannel m = QueueChannel {
    qcRead        :: StrictTVar m (Maybe (TQueue m Message)),
    qcWrite       :: StrictTVar m (Maybe (TQueue m Message))
  }

--
-- QueueChannel API
--

readQueueChannelSTM :: ( MonadSTM        m
                       , MonadThrow (STM m)
                       )
                    => QueueChannel m -> STM m Message
readQueueChannelSTM QueueChannel { qcRead } = do
    a <- readTVar qcRead >>= traverse readTQueue
    case a of
      Nothing           -> throwSTM (resourceVanishedIOError "sim.readQueueChannel")
      Just msg@MsgClose -> writeTVar qcRead Nothing
                        >> return msg
      Just msg          -> return msg


writeQueueChannel :: MonadSTM m
                  => QueueChannel m -> Message -> m Bool
writeQueueChannel QueueChannel { qcWrite } msg =
    atomically $ do
      mq <- readTVar qcWrite
      case mq of
        Nothing -> return False
        Just q  -> writeTQueue q msg
                >> case msg of
                      MsgClose -> writeTVar qcWrite Nothing
                      _        -> return ()
                >> return True


newConnectedQueueChannelPair :: MonadSTM m
                             => STM m ( QueueChannel m
                                      , QueueChannel m )
newConnectedQueueChannelPair = do
    read  <- newTQueue
    write <- newTQueue
    q  <- QueueChannel <$> newTVar (Just read)
                       <*> newTVar (Just write)
    q' <- QueueChannel <$> newTVar (Just write)
                       <*> newTVar (Just read)
    return (q, q')


--
-- BufferedChannel
--


-- | A BufferedChannel supports:
--
-- - unbuffered writes
-- - buffered reads
-- - two-way close handshake with 120s timeout
--
-- Read side is closed as soon as 'MsgClose' is received.  Buffering allows to
-- hold the messages that were read from the network but not passed to an
-- application layer while the read side of 'QueueChannel' was closed.
--
data BufferedChannel m = BufferedChannel {
    bcRead  :: m BL.ByteString,
    bcWrite :: BL.ByteString -> m (),
    bcClose :: m ()
  }


-- | Error thrown when expected 'MsgClose' does not arrive within `120s`.
--
-- This excpetion should not be handled by a simulation, it is a proof of a half
-- closed connection, which should never happen when using connection manager.
--
data CloseError = CloseTimeoutError
  deriving (Show, Typeable)

instance Exception CloseError


data SuccessOrFailure = Success | Failure

type Size = Int64

-- | Attenuation of a channel.
--
data Attenuation = Attenuation {
    aReadAttenuation  :: Time -> Size -> ( DiffTime,
                                           SuccessOrFailure ),
    aWriteAttenuation :: Maybe Int
  }


-- | Make a 'BufferChannel' from a 'QueueChannel'.
--
newBufferedChannel :: forall m.
                      ( MonadSTM        m
                      , MonadTime       m
                      , MonadTimer      m
                      , MonadThrow      m
                      , MonadThrow (STM m)
                      )
                   => Tracer m BufferedChannelTrace
                   -> Attenuation
                   -> QueueChannel m
                   -> STM m (BufferedChannel m)
newBufferedChannel tr Attenuation { aReadAttenuation,
                                    aWriteAttenuation } qc = do
    bufferVar       <- newTVar Empty
    writeCounterVar <- newTVar 0
    return BufferedChannel { bcRead  = bcRead bufferVar
                           , bcWrite = bcWrite writeCounterVar
                           , bcClose = bcClose bufferVar
                           }
  where
    bcRead :: StrictTVar m (Seq Message) -> m BL.ByteString
    bcRead bufferVar = do
      msg <- atomically $ do
        as <- readTVar bufferVar
        case as of
          Empty                   -> readQueueChannelSTM qc
          msg@MsgClose    :<| _   -> return msg
          msg@MsgBytes {} :<| as' -> writeTVar bufferVar as'
                                  $> msg
      t <- getMonotonicTime
      case msg of
        -- match the 'Bearer.Snocket' behavour and throw 'MuxError'
        -- when null byte is received from the network.
        MsgClose -> do
          case aReadAttenuation t 1 of
            ( d, _       ) -> threadDelay d
                           >> throwIO (MuxError MuxBearerClosed
                                                "closed when reading data")
        MsgBytes bs -> 
          case aReadAttenuation t (BL.length bs) of
            ( d, Success ) -> threadDelay d
                           >> return bs

            ( d, Failure ) -> threadDelay d
                           >> throwIO (resourceVanishedIOError
                                        "BufferedChannel.read")

    bcWrite :: StrictTVar m Int
            -> BL.ByteString
            -> m ()
    bcWrite writeCounterVar bs = do
      wCount <- atomically $ do
        modifyTVar writeCounterVar succ
        readTVar writeCounterVar
      case aWriteAttenuation of
        Just limit  | wCount >= limit
                   -> throwIO $
                        resourceVanishedIOError
                          "BufferedChannel.write: write limit reached"
        _          -> return ()

      sent <- writeQueueChannel qc (MsgBytes bs)
      when (not sent) $
        throwIO (resourceVanishedIOError "BufferedChannel.write")

    bcClose :: StrictTVar m (Seq Message)
            -> m ()
    bcClose bufferVar = do
      sent <- writeQueueChannel qc MsgClose
      traceWith tr (BufferedChannelClosing sent)
      
      -- TODO: switch to timeout once its fixed.
      d <- registerDelay 120
      res <-
        atomically $
          (LazySTM.readTVar d >>= \b -> check b $> Nothing)
          `orElse`
          (fmap Just $ do
            buffered <- readTVar bufferVar
            unless (MsgClose `elem` buffered) $ do
              msgs <- readTVar (qcRead qc)
                  >>= traverse tryReadAllTQueue
              case msgs of
                Nothing -> return ()
                Just as -> if MsgClose `elem` as
                             then writeTVar bufferVar (buffered Seq.>< as)
                             else retry)

      traceWith tr (BufferedChannelClosed (isJust res))
      case res of
        Just _  -> return ()
        Nothing -> throwIO CloseTimeoutError

    tryReadAllTQueue :: forall a. TQueue m a -> STM m (Seq a)
    tryReadAllTQueue q = go Empty
      where
        go :: Seq a -> STM m (Seq a)
        go !as = do
          ma <- tryReadTQueue q
          case ma of
            Nothing -> return as
            Just a  -> go (as Seq.|> a)


-- | Create a pair of connected 'BufferedChannel's.
--
newConnectedBufferedChannelPair :: forall m.
                                   ( MonadSTM        m
                                   , MonadTime       m
                                   , MonadTimer      m
                                   , MonadThrow      m
                                   , MonadThrow (STM m)
                                   )
                                => Tracer m BufferedChannelTrace
                                -> Tracer m BufferedChannelTrace
                                -> Attenuation
                                -> Attenuation
                                -> STM m (BufferedChannel m, BufferedChannel m)
newConnectedBufferedChannelPair tr tr' attenuation attenuation' = do
    (c, c') <- newConnectedQueueChannelPair
    b  <- newBufferedChannel tr  attenuation  c
    b' <- newBufferedChannel tr' attenuation' c'
    return (b, b')


--
-- Trace
--

data BufferedChannelTrace =
    BufferedChannelClosing Bool
  | BufferedChannelClosed  Bool
  deriving Show

--
-- Utils
--

resourceVanishedIOError :: String -> IOError
resourceVanishedIOError ioe_location = IOError
  { ioe_handle      = Nothing
  , ioe_type        = ResourceVanished
  , ioe_location
  , ioe_description = ""
  , ioe_errno       = Nothing
  , ioe_filename    = Nothing
  }

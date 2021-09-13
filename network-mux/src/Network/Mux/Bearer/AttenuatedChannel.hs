{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Mux.Bearer.AttenuatedChannel
  ( AttenuatedChannel (..)
  , Size
  , SuccessOrFailure (..)
  , Attenuation (..)
  , newConnectedAttenuatedChannelPair
  , attenuationChannelAsMuxBearer
  , CloseError (..)
  -- * Trace
  , AttenuatedChannelTrace (..)
  -- * Utils
  , resourceVanishedIOError
  ) where

import           Prelude hiding (read)

import           Control.Monad (when)
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
import           Data.Typeable (Typeable)

import           Network.Mux.Codec
import           Network.Mux.Time
import           Network.Mux.Timeout
import           Network.Mux.Trace
import           Network.Mux.Types


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

readQueueChannel :: ( MonadSTM        m
                    , MonadThrow (STM m)
                    )
                 => QueueChannel m -> m Message
readQueueChannel QueueChannel { qcRead } =
    atomically $ do
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
-- AttenuatedChannel
--


-- | An AttenuatedChannel supports:
--
-- - attenuation applied after reading a message from 'QueueChannel';
-- - two-way close handshake with 120s timeout.  Read side is closed as soon as
--   an internal 'MsgClose' is received, write side has to be closed explicietly.
--
data AttenuatedChannel m = AttenuatedChannel {
    acRead  :: m BL.ByteString,
    acWrite :: BL.ByteString -> m (),
    acClose :: m ()
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


-- | Make a 'AttenuatedChannel' from a 'QueueChannel'.
--
newAttenuatedChannel :: forall m.
                        ( MonadSTM        m
                        , MonadTime       m
                        , MonadTimer      m
                        , MonadThrow      m
                        , MonadThrow (STM m)
                        )
                     => Tracer m AttenuatedChannelTrace
                     -> Attenuation
                     -> QueueChannel m
                     -> STM m (AttenuatedChannel m)
newAttenuatedChannel tr Attenuation { aReadAttenuation,
                                      aWriteAttenuation } qc = do
    writeCounterVar <- newTVar 0
    return AttenuatedChannel { acRead
                             , acWrite = acWrite writeCounterVar
                             , acClose
                             }
  where
    acRead :: m BL.ByteString
    acRead = do
      msg <- readQueueChannel qc
      t <- getMonotonicTime
      case msg of
        -- match the 'Bearer.Snocket' behavour and throw 'MuxError'
        -- when null byte is received from the network.
        MsgClose -> do
          case aReadAttenuation t 1 of
            ( d, _       ) -> traceWith tr AttChannRemoteClose
                           >> threadDelay d
                           >> throwIO (MuxError MuxBearerClosed
                                                "closed when reading data")
        MsgBytes bs ->
          case aReadAttenuation t (BL.length bs) of
            ( d, Success ) -> threadDelay d
                           >> return bs

            ( d, Failure ) -> threadDelay d
                           >> throwIO (resourceVanishedIOError
                                        "AttenuatedChannel.read")

    acWrite :: StrictTVar m Int
            -> BL.ByteString
            -> m ()
    acWrite writeCounterVar bs = do
      wCount <- atomically $ do
        modifyTVar writeCounterVar succ
        readTVar writeCounterVar
      case aWriteAttenuation of
        Just limit  | wCount >= limit
                   -> throwIO $
                        resourceVanishedIOError
                          "AttenuatedChannel.write: write limit reached"
        _          -> return ()

      sent <- writeQueueChannel qc (MsgBytes bs)
      when (not sent) $
        throwIO (resourceVanishedIOError "AttenuatedChannel.write")

    acClose :: m ()
    acClose = do
      sent <- writeQueueChannel qc MsgClose
      traceWith tr (AttChannClosing sent)

      -- TODO: switch to timeout once it's fixed.
      d <- registerDelay 120
      res <-
        atomically $
          (LazySTM.readTVar d >>= \b -> check b $> Nothing)
          `orElse`
          (fmap Just $ do
              msg <- readTVar (qcRead qc)
                  >>= traverse readTQueue
              case msg of
                Nothing       -> return ()
                Just MsgClose -> return ()
                Just _        -> retry)

      traceWith tr (AttChannClosed (isJust res))
      case res of
        Just _  -> return ()
        Nothing -> throwIO CloseTimeoutError


-- | Create a pair of connected 'AttenuatedChannel's.
--
newConnectedAttenuatedChannelPair
    :: forall m.
       ( MonadSTM        m
       , MonadTime       m
       , MonadTimer      m
       , MonadThrow      m
       , MonadThrow (STM m)
       )
    => Tracer m AttenuatedChannelTrace
    -> Tracer m AttenuatedChannelTrace
    -> Attenuation
    -> Attenuation
    -> STM m (AttenuatedChannel m, AttenuatedChannel m)
newConnectedAttenuatedChannelPair tr tr' attenuation attenuation' = do
    (c, c') <- newConnectedQueueChannelPair
    b  <- newAttenuatedChannel tr  attenuation  c
    b' <- newAttenuatedChannel tr' attenuation' c'
    return (b, b')

attenuationChannelAsMuxBearer :: forall m.
                                 ( MonadThrow         m
                                 , MonadMonotonicTime m
                                 )
                              => SDUSize
                              -> DiffTime
                              -> Tracer m MuxTrace
                              -> AttenuatedChannel m
                              -> MuxBearer m
attenuationChannelAsMuxBearer sduSize sduTimeout muxTracer chan =
    MuxBearer {
      read    = readMux,
      write   = writeMux,
      sduSize
    }
  where
    readMux :: TimeoutFn m -> m (MuxSDU, Time)
    readMux timeoutFn = do
      traceWith muxTracer $ MuxTraceRecvHeaderStart
      mbuf <- timeoutFn sduTimeout $ acRead chan
      case mbuf of
        Nothing -> do
          traceWith muxTracer MuxTraceSDUReadTimeoutException
          throwIO (MuxError MuxSDUReadTimeout "Mux SDU Timeout")

        Just buf -> do
          let (hbuf, payload) = BL.splitAt 8 buf
          case decodeMuxSDU hbuf of
            Left  e      -> throwIO e
            Right muxsdu -> do
              let header = msHeader muxsdu
              traceWith muxTracer $ MuxTraceRecvHeaderEnd header
              ts <- getMonotonicTime
              traceWith muxTracer $ MuxTraceRecvDeltaQObservation header ts
              return (muxsdu {msBlob = payload}, ts)

    writeMux :: TimeoutFn m -> MuxSDU -> m Time
    writeMux _ sdu = do
      ts <- getMonotonicTime
      let ts32 = timestampMicrosecondsLow32Bits ts
          sdu' = setTimestamp sdu (RemoteClockModel ts32)
          buf  = encodeMuxSDU sdu'
      traceWith muxTracer $ MuxTraceSendStart (msHeader sdu')
      acWrite chan buf

      traceWith muxTracer $ MuxTraceSendEnd
      return ts

--
-- Trace
--

data AttenuatedChannelTrace =
    AttChannClosing Bool
  | AttChannClosed  Bool
  | AttChannRemoteClose
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

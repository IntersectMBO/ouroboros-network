{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Mux.Bearer.Queues
  ( QueueChannel (..)
  , queueChannelAsBearer
  ) where

import Data.ByteString.Lazy qualified as BL

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Tracer

import Network.Mux.Codec qualified as Mx
import Network.Mux.Time as Mx
import Network.Mux.Timeout qualified as Mx
import Network.Mux.Trace qualified as Mx
import Network.Mux.Types (Bearer)
import Network.Mux.Types qualified as Mx

data QueueChannel m = QueueChannel {
    readQueue  :: StrictTBQueue m BL.ByteString,
    writeQueue :: StrictTBQueue m BL.ByteString
  }


queueChannelAsBearer
  :: forall m.
     ( MonadSTM   m
     , MonadMonotonicTime m
     , MonadThrow m
     )
  => Mx.SDUSize
  -> Tracer m Mx.Trace
  -> QueueChannel m
  -> Bearer m Mx.Unbuffered
queueChannelAsBearer sduSize tracer QueueChannel { writeQueue, readQueue } = do
      Mx.Bearer {
        Mx.read      = readMux,
        Mx.write     = writeMux,
        Mx.writeMany = writeMuxMany,
        Mx.sduSize   = sduSize,
        Mx.batchSize = 2 * (fromIntegral $ Mx.getSDUSize sduSize),
        Mx.name      = "queue-channel"
      }
    where
      readMux :: Mx.TimeoutFn m -> m (Mx.SDU, Time)
      readMux _ = do
          traceWith tracer Mx.TraceRecvHeaderStart
          buf <- atomically $ readTBQueue readQueue
          let (hbuf, payload) = BL.splitAt 8 buf
          case Mx.decodeSDU hbuf of
              Left  e      -> throwIO e
              Right header -> do
                  traceWith tracer $ Mx.TraceRecvHeaderEnd (Mx.msHeader header)
                  ts <- getMonotonicTime
                  traceWith tracer $ Mx.TraceRecvDeltaQObservation (Mx.msHeader header) ts
                  return (header {Mx.msBlob = payload}, ts)

      writeMux :: Mx.TimeoutFn m -> Mx.SDU -> m Time
      writeMux _ sdu = do
          ts <- getMonotonicTime
          let ts32 = Mx.timestampMicrosecondsLow32Bits ts
              sdu' = Mx.setTimestamp sdu (Mx.RemoteClockModel ts32)
              buf  = Mx.encodeSDU sdu'
          traceWith tracer $ Mx.TraceSendStart (Mx.msHeader sdu')
          atomically $ writeTBQueue writeQueue buf
          traceWith tracer Mx.TraceSendEnd
          return ts

      writeMuxMany :: Mx.TimeoutFn m -> [Mx.SDU] -> m Time
      writeMuxMany timeoutFn sdus = do
        ts <- getMonotonicTime
        mapM_ (writeMux timeoutFn) sdus
        return ts

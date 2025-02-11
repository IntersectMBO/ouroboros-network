{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Mux.Bearer.Queues
  ( QueueChannel (..)
  , queueChannelAsMuxBearer
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
import Network.Mux.Types (MuxBearer)
import Network.Mux.Types qualified as Mx

data QueueChannel m = QueueChannel {
    readQueue  :: StrictTBQueue m BL.ByteString,
    writeQueue :: StrictTBQueue m BL.ByteString
  }


queueChannelAsMuxBearer
  :: forall m.
     ( MonadSTM   m
     , MonadMonotonicTime m
     , MonadThrow m
     )
  => Mx.SDUSize
  -> Tracer m Mx.MuxTrace
  -> QueueChannel m
  -> MuxBearer m
queueChannelAsMuxBearer sduSize tracer QueueChannel { writeQueue, readQueue } = do
      Mx.MuxBearer {
        Mx.read    = readMux,
        Mx.write   = writeMux,
        Mx.writeMany   = writeMuxMany,
        Mx.sduSize = sduSize,
        Mx.batchSize = 2 * (fromIntegral $ Mx.getSDUSize sduSize)
      }
    where
      readMux :: Mx.TimeoutFn m -> m (Mx.MuxSDU, Time)
      readMux _ = do
          traceWith tracer Mx.MuxTraceRecvHeaderStart
          buf <- atomically $ readTBQueue readQueue
          let (hbuf, payload) = BL.splitAt 8 buf
          case Mx.decodeMuxSDU hbuf of
              Left  e      -> throwIO e
              Right header -> do
                  traceWith tracer $ Mx.MuxTraceRecvHeaderEnd (Mx.msHeader header)
                  ts <- getMonotonicTime
                  traceWith tracer $ Mx.MuxTraceRecvDeltaQObservation (Mx.msHeader header) ts
                  return (header {Mx.msBlob = payload}, ts)

      writeMux :: Mx.TimeoutFn m -> Mx.MuxSDU -> m Time
      writeMux _ sdu = do
          ts <- getMonotonicTime
          let ts32 = Mx.timestampMicrosecondsLow32Bits ts
              sdu' = Mx.setTimestamp sdu (Mx.RemoteClockModel ts32)
              buf  = Mx.encodeMuxSDU sdu'
          traceWith tracer $ Mx.MuxTraceSendStart (Mx.msHeader sdu')
          atomically $ writeTBQueue writeQueue buf
          traceWith tracer Mx.MuxTraceSendEnd
          return ts

      writeMuxMany :: Mx.TimeoutFn m -> [Mx.MuxSDU] -> m Time
      writeMuxMany timeoutFn sdus = do
        ts <- getMonotonicTime
        mapM_ (writeMux timeoutFn) sdus
        return ts


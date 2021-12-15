{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Mux.Bearer.Queues (queuesAsMuxBearer) where

import qualified Data.ByteString.Lazy as BL

import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Tracer

import qualified Network.Mux as Mx
import qualified Network.Mux.Codec as Mx
import           Network.Mux.Time as Mx
import qualified Network.Mux.Timeout as Mx
import           Network.Mux.Types (MuxBearer, SDUSize)
import qualified Network.Mux.Types as Mx


queuesAsMuxBearer
  :: forall m.
     ( MonadSTM   m
     , MonadTime  m
     , MonadThrow m
     )
  => Tracer m Mx.MuxTrace
  -> TBQueue m BL.ByteString
  -> TBQueue m BL.ByteString
  -> SDUSize
  -> MuxBearer m
queuesAsMuxBearer tracer writeQueue readQueue sduSize = do
      Mx.MuxBearer {
        Mx.read    = readMux,
        Mx.write   = writeMux,
        Mx.sduSize = sduSize
      }
    where
      readMux :: Mx.TimeoutFn m -> m (Mx.MuxSDU, Time)
      readMux _ = do
          traceWith tracer $ Mx.MuxTraceRecvHeaderStart
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
          traceWith tracer $ Mx.MuxTraceSendEnd
          return ts


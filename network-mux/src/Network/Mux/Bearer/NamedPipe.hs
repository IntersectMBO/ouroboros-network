{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Mux.Bearer.NamedPipe (namedPipeAsBearer) where

import Control.Monad (when)
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (traverse_)
import Data.Int (Int64)

import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import Control.Tracer

import Network.Mux.Codec qualified as Mx
import Network.Mux.Time qualified as Mx
import Network.Mux.Timeout qualified as Mx
import Network.Mux.Trace qualified as Mx
import Network.Mux.Types qualified as Mx

import System.Win32 (HANDLE)
import System.Win32.Async qualified as Win32.Async


-- | Named pipe bearer.  The 'HANDLE' must be associated with IO completion port
-- using 'System.Win32.Async.associateWithIOCompletionPort'.
--
namedPipeAsBearer :: Mx.SDUSize
                  -> HANDLE
                  -> Mx.Bearer IO
namedPipeAsBearer sduSize h =
    Mx.Bearer {
        Mx.read           = readNamedPipe,
        Mx.write          = writeNamedPipe,
        Mx.writeMany      = writeNamedPipeMany,
        Mx.sduSize        = sduSize,
        Mx.batchSize      = fromIntegral $ Mx.getSDUSize sduSize,
        Mx.name           = "named-pipe",
        Mx.egressInterval = 0
      }
  where
    readNamedPipe :: Tracer IO Mx.BearerTrace -> Mx.TimeoutFn IO -> IO (Mx.SDU, Time)
    readNamedPipe tracer _ = do
      traceWith tracer Mx.TraceRecvHeaderStart
      hbuf <- recvLen' tracer True Mx.msHeaderLength []
      case Mx.decodeSDU hbuf of
        Left e -> throwIO e
        Right header@Mx.SDU { Mx.msHeader } -> do
          traceWith tracer $ Mx.TraceRecvHeaderEnd msHeader
          blob <- recvLen' tracer False (fromIntegral $ Mx.mhLength msHeader) []
          ts <- getMonotonicTime
          traceWith tracer (Mx.TraceRecvDeltaQObservation msHeader ts)
          return (header {Mx.msBlob = blob}, ts)

    recvLen' :: Tracer IO Mx.BearerTrace -> Bool -> Int64 -> [BL.ByteString] -> IO BL.ByteString
    recvLen' _tracer _ 0 bufs = return (BL.concat $ reverse bufs)
    recvLen' tracer waitingOnNextHeader l bufs = do
      traceWith tracer $ Mx.TraceRecvStart $ fromIntegral l
      buf <- BL.fromStrict <$> Win32.Async.readHandle h (fromIntegral l)
                `catch` Mx.handleIOException "readHandle errored"
      if BL.null buf
        then do
          when waitingOnNextHeader
            $ threadDelay 1
          throwIO $ Mx.BearerClosed (show h ++
              " closed when reading data, waiting on next header " ++
              show waitingOnNextHeader)
        else do
          traceWith tracer (Mx.TraceRecvEnd (fromIntegral $ BL.length buf))
          recvLen' tracer False (l - BL.length buf) (buf : bufs)

    writeNamedPipe :: Tracer IO Mx.BearerTrace -> Mx.TimeoutFn IO -> Mx.SDU -> IO Time
    writeNamedPipe tracer _ sdu = do
      ts <- getMonotonicTime
      let ts32 = Mx.timestampMicrosecondsLow32Bits ts
          sdu' = Mx.setTimestamp sdu (Mx.RemoteClockModel ts32)
          buf  = Mx.encodeSDU sdu'
      traceWith tracer $ Mx.TraceSendStart (Mx.msHeader sdu')
      traverse_ (Win32.Async.writeHandle h) (BL.toChunks buf)
        `catch` Mx.handleIOException "writeHandle errored"
      traceWith tracer Mx.TraceSendEnd
      return ts

    writeNamedPipeMany :: Tracer IO Mx.BearerTrace -> Mx.TimeoutFn IO -> [Mx.SDU] -> IO Time
    writeNamedPipeMany tracer timeoutFn sdus = do
      ts <- getMonotonicTime
      mapM_ (writeNamedPipe tracer timeoutFn) sdus
      return ts

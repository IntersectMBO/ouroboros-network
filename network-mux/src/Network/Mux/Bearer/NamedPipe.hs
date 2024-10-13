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
import Network.Mux.Trace (MuxTrace)
import Network.Mux.Trace qualified as Mx
import Network.Mux.Types (MuxBearer)
import Network.Mux.Types qualified as Mx

import System.Win32 (HANDLE)
import System.Win32.Async qualified as Win32.Async


-- | Named pipe bearer.  The 'HANDLE' must be associated with IO completion port
-- using 'System.Win32.Async.associateWithIOCompletionPort'.
--
namedPipeAsBearer :: Mx.SDUSize
                  -> Tracer IO MuxTrace
                  -> HANDLE
                  -> MuxBearer IO
namedPipeAsBearer sduSize tracer h =
    Mx.MuxBearer {
        Mx.read    = readNamedPipe,
        Mx.write   = writeNamedPipe,
        Mx.sduSize = sduSize,
        Mx.name    = "named-pipe"
      }
  where
    readNamedPipe :: Mx.TimeoutFn IO -> IO (Mx.MuxSDU, Time)
    readNamedPipe _ = do
      traceWith tracer Mx.MuxTraceRecvHeaderStart
      hbuf <- recvLen' True 8 []
      case Mx.decodeMuxSDU hbuf of
        Left e -> throwIO e
        Right header@Mx.MuxSDU { Mx.msHeader } -> do
          traceWith tracer $ Mx.MuxTraceRecvHeaderEnd msHeader
          blob <- recvLen' False (fromIntegral $ Mx.mhLength msHeader) []
          ts <- getMonotonicTime
          traceWith tracer (Mx.MuxTraceRecvDeltaQObservation msHeader ts)
          return (header {Mx.msBlob = blob}, ts)

    recvLen' :: Bool -> Int64 -> [BL.ByteString] -> IO BL.ByteString
    recvLen' _ 0 bufs = return (BL.concat $ reverse bufs)
    recvLen' waitingOnNextHeader l bufs = do
      traceWith tracer $ Mx.MuxTraceRecvStart $ fromIntegral l
      buf <- BL.fromStrict <$> Win32.Async.readHandle h (fromIntegral l)
                `catch` Mx.handleIOException "readHandle errored"
      if BL.null buf
        then do
          when waitingOnNextHeader
            $ threadDelay 1
          throwIO $ Mx.MuxError Mx.MuxBearerClosed (show h ++
              " closed when reading data, waiting on next header " ++
              show waitingOnNextHeader)
        else do
          traceWith tracer (Mx.MuxTraceRecvEnd (fromIntegral $ BL.length buf))
          recvLen' False (l - BL.length buf) (buf : bufs)

    writeNamedPipe :: Mx.TimeoutFn IO -> Mx.MuxSDU -> IO Time
    writeNamedPipe _ sdu = do
      ts <- getMonotonicTime
      let ts32 = Mx.timestampMicrosecondsLow32Bits ts
          sdu' = Mx.setTimestamp sdu (Mx.RemoteClockModel ts32)
          buf  = Mx.encodeMuxSDU sdu'
      traceWith tracer $ Mx.MuxTraceSendStart (Mx.msHeader sdu')
      traverse_ (Win32.Async.writeHandle h) (BL.toChunks buf)
        `catch` Mx.handleIOException "writeHandle errored"
      traceWith tracer Mx.MuxTraceSendEnd
      return ts

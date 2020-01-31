{-# LANGUAGE ScopedTypeVariables #-}

module Network.Mux.Bearer.NamedPipe
  ( namedPipeAsBearer ) where

import           Control.Monad (when)
import qualified Data.ByteString.Lazy as BL
import           Data.Int (Int64)
import           Data.Foldable (traverse_)

import           GHC.Stack

import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import           Control.Tracer

import qualified Network.Mux as Mx
import           Network.Mux.Types (MuxBearer)
import qualified Network.Mux.Types as Mx
import           Network.Mux.Trace (MuxTrace)
import qualified Network.Mux.Trace as Mx
import qualified Network.Mux.Time as Mx
import qualified Network.Mux.Codec as Mx

import           System.Win32               (HANDLE)
import qualified System.Win32.Async      as Win32.Async


-- | Named pipe bearer.  The 'HANDLE' must be associated with IO completion port
-- using 'System.Win32.Async.associateWithIOCompletionPort'.
--
namedPipeAsBearer :: Tracer IO MuxTrace
                  -> HANDLE
                  -> MuxBearer IO
namedPipeAsBearer tracer h =
    Mx.MuxBearer {
        Mx.read    = readNamedPipe,
        Mx.write   = writeNamedPipe,
        Mx.sduSize = 24576
      }
  where
    readNamedPipe :: HasCallStack => IO (Mx.MuxSDU, Time)
    readNamedPipe = do
      traceWith tracer Mx.MuxTraceRecvHeaderStart
      hbuf <- recvLen' True 8 []
      case Mx.decodeMuxSDU hbuf of
        Left e -> throwM e
        Right header -> do
          traceWith tracer $ Mx.MuxTraceRecvHeaderEnd header
          traceWith tracer $ Mx.MuxTraceRecvPayloadStart (fromIntegral $ Mx.msLength header)
          blob <- recvLen' False (fromIntegral $ Mx.msLength header) []
          ts <- getMonotonicTime
          traceWith tracer (Mx.MuxTraceRecvDeltaQObservation header ts)
          traceWith tracer $ Mx.MuxTraceRecvPayloadEnd blob
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
          throwM $ Mx.MuxError Mx.MuxBearerClosed (show h ++
              " closed when reading data, waiting on next header " ++
              show waitingOnNextHeader) callStack
        else do
          traceWith tracer (Mx.MuxTraceRecvEnd buf)
          recvLen' False (l - fromIntegral (BL.length buf)) (buf : bufs)

    writeNamedPipe :: Mx.MuxSDU -> IO Time
    writeNamedPipe sdu = do
      ts <- getMonotonicTime
      let ts32 = Mx.timestampMicrosecondsLow32Bits ts
          sdu' = sdu { Mx.msTimestamp = Mx.RemoteClockModel ts32 }
          buf  = Mx.encodeMuxSDU sdu'
      traceWith tracer $ Mx.MuxTraceSendStart sdu'
      traverse_ (Win32.Async.writeHandle h) (BL.toChunks buf)
        `catch` Mx.handleIOException "writeHandle errored"
      traceWith tracer Mx.MuxTraceSendEnd
      return ts

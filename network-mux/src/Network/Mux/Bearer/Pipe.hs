{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Mux.Bearer.Pipe (
    PipeChannel (..)
  , pipeChannelFromHandles
#if defined(mingw32_HOST_OS)
  , pipeChannelFromNamedPipe
#endif
  , pipeAsBearer
  ) where

import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime.SI
import           Control.Tracer
import qualified Data.ByteString.Lazy as BL
import           System.IO (Handle, hFlush)

#if defined(mingw32_HOST_OS)
import           Data.Foldable (traverse_)

import qualified System.Win32.Types as Win32 (HANDLE)
import qualified System.Win32.Async as Win32.Async
#endif

import           Network.Mux.Types (Bearer)
import qualified Network.Mux.Types as Mx
import qualified Network.Mux.Trace as Mx
import qualified Network.Mux.Codec as Mx
import qualified Network.Mux.Time as Mx
import qualified Network.Mux.Timeout as Mx


-- | Abstraction over various types of handles.  We provide two instances:
--
--  * based on 'Handle': os independent, but will not work well on Windows,
--  * based on 'Win32.HANDLE': Windows specific.
--
data PipeChannel = PipeChannel {
    readHandle  :: Int -> IO BL.ByteString,
    writeHandle :: BL.ByteString -> IO ()
  }

pipeChannelFromHandles :: Handle
                       -- ^ read handle
                       -> Handle
                       -- ^ write handle
                       -> PipeChannel
pipeChannelFromHandles r w = PipeChannel {
    readHandle  = BL.hGet r,
    writeHandle = \a -> BL.hPut w a >> hFlush w
  }

#if defined(mingw32_HOST_OS)
-- | Create a 'PipeChannel' from a named pipe.  This allows to emulate
-- anonymous pipes using named pipes on Windows.
--
pipeChannelFromNamedPipe :: Win32.HANDLE
                         -> PipeChannel
pipeChannelFromNamedPipe h = PipeChannel {
      readHandle  = fmap BL.fromStrict . Win32.Async.readHandle h,
      writeHandle = traverse_ (Win32.Async.writeHandle h) . BL.toChunks
    }
#endif

pipeAsBearer
  :: Mx.SDUSize
  -> PipeChannel
  -> Bearer IO
pipeAsBearer sduSize channel =
      Mx.Bearer {
          Mx.read           = readPipe,
          Mx.write          = writePipe,
          Mx.writeMany      = writePipeMany,
          Mx.sduSize        = sduSize,
          Mx.name           = "pipe",
          Mx.batchSize      = fromIntegral $ Mx.getSDUSize sduSize,
          Mx.egressInterval = 0
        }
    where
      readPipe :: Tracer IO Mx.BearerTrace -> Mx.TimeoutFn IO -> IO (Mx.SDU, Time)
      readPipe tracer _ = do
          traceWith tracer Mx.TraceRecvHeaderStart
          hbuf <- recvLen' (fromIntegral Mx.msHeaderLength) []
          case Mx.decodeSDU hbuf of
              Left e -> throwIO e
              Right header@Mx.SDU { Mx.msHeader } -> do
                  traceWith tracer $ Mx.TraceRecvHeaderEnd msHeader
                  blob <- recvLen' (fromIntegral $ Mx.mhLength msHeader) []
                  ts <- getMonotonicTime
                  traceWith tracer (Mx.TraceRecvDeltaQObservation msHeader ts)
                  return (header {Mx.msBlob = blob}, ts)
        where
          recvLen' :: Int -> [BL.ByteString] -> IO BL.ByteString
          recvLen' 0 bufs = return $ BL.concat $ reverse bufs
          recvLen' l bufs = do
              traceWith tracer $ Mx.TraceRecvStart l
              buf <- readHandle channel l
                        `catch` Mx.handleIOException "readHandle errored"
              if BL.null buf
                  then throwIO $ Mx.BearerClosed "Pipe closed when reading data"
                  else do
                      traceWith tracer $ Mx.TraceRecvEnd (fromIntegral $ BL.length buf)
                      recvLen' (l - fromIntegral (BL.length buf)) (buf : bufs)

      writePipe :: Tracer IO Mx.BearerTrace -> Mx.TimeoutFn IO -> Mx.SDU -> IO Time
      writePipe tracer _ sdu = do
          ts <- getMonotonicTime
          let ts32 = Mx.timestampMicrosecondsLow32Bits ts
              sdu' = Mx.setTimestamp sdu (Mx.RemoteClockModel ts32)
              buf  = Mx.encodeSDU sdu'
          traceWith tracer $ Mx.TraceSendStart (Mx.msHeader sdu')
          writeHandle channel buf
            `catch` Mx.handleIOException "writeHandle errored"
          traceWith tracer Mx.TraceSendEnd
          return ts

      writePipeMany :: Tracer IO Mx.BearerTrace -> Mx.TimeoutFn IO -> [Mx.SDU] -> IO Time
      writePipeMany tracer timeoutFn sdus = do
        ts <- getMonotonicTime
        mapM_ (writePipe tracer timeoutFn) sdus
        return ts


{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Mux.Bearer.Pipe (
    PipeChannel (..)
  , pipeChannelFromHandles
#if defined(mingw32_HOST_OS)
  , pipeChannelFromNamedPipe
#endif
  , pipeAsMuxBearer
  , runMuxWithPipes
  ) where

import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Tracer
import qualified Data.ByteString.Lazy as BL
import           GHC.Stack
import           System.IO (Handle, hFlush)

#if defined(mingw32_HOST_OS)
import           Data.Foldable (traverse_)

import qualified System.Win32.Types as Win32 (HANDLE)
import qualified System.Win32.Async as Win32.Async
#endif

import qualified Network.Mux as Mx
import           Network.Mux.Types (MuxBearer)
import qualified Network.Mux.Types as Mx
import qualified Network.Mux.Trace as Mx
import qualified Network.Mux.Codec as Mx
import qualified Network.Mux.Time as Mx


-- | Abstraction over various types of handles.  We provide two instances:
--
--  * based on 'Handle': os independepnt, but will not work well on Windows,
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

pipeAsMuxBearer
  :: Tracer IO Mx.MuxTrace
  -> PipeChannel
  -> MuxBearer IO
pipeAsMuxBearer tracer channel =
      Mx.MuxBearer {
          Mx.read    = readPipe,
          Mx.write   = writePipe,
          Mx.sduSize = 32768
        }
    where
      readPipe :: HasCallStack => IO (Mx.MuxSDU, Time)
      readPipe = do
          traceWith tracer $ Mx.MuxTraceRecvHeaderStart
          hbuf <- recvLen' 8 []
          case Mx.decodeMuxSDU hbuf of
              Left e     -> throwM e
              Right header -> do
                  traceWith tracer $ Mx.MuxTraceRecvHeaderEnd header
                  traceWith tracer $ Mx.MuxTraceRecvPayloadStart (fromIntegral $ Mx.msLength header)
                  blob <- recvLen' (fromIntegral $ Mx.msLength header) []
                  ts <- getMonotonicTime
                  traceWith tracer (Mx.MuxTraceRecvDeltaQObservation header ts)
                  traceWith tracer $ Mx.MuxTraceRecvPayloadEnd blob
                  return (header {Mx.msBlob = blob}, ts)

      recvLen' :: Int -> [BL.ByteString] -> IO BL.ByteString
      recvLen' 0 bufs = return $ BL.concat $ reverse bufs
      recvLen' l bufs = do
          traceWith tracer $ Mx.MuxTraceRecvStart l
          buf <- readHandle channel l
                    `catch` Mx.handleIOException "readHandle errored"
          if BL.null buf
              then throwM $ Mx.MuxError Mx.MuxBearerClosed "Pipe closed when reading data" callStack
              else do
                  traceWith tracer $ Mx.MuxTraceRecvEnd buf
                  recvLen' (l - fromIntegral (BL.length buf)) (buf : bufs)

      writePipe :: Mx.MuxSDU -> IO Time
      writePipe sdu = do
          ts <- getMonotonicTime
          let ts32 = Mx.timestampMicrosecondsLow32Bits ts
              sdu' = sdu { Mx.msTimestamp = Mx.RemoteClockModel ts32 }
              buf  = Mx.encodeMuxSDU sdu'
          traceWith tracer $ Mx.MuxTraceSendStart sdu'
          writeHandle channel buf
            `catch` Mx.handleIOException "writeHandle errored"
          traceWith tracer $ Mx.MuxTraceSendEnd
          return ts

runMuxWithPipes
    :: Tracer IO Mx.MuxTrace
    -> Mx.MuxApplication appType IO a b
    -> PipeChannel
    -> IO ()
runMuxWithPipes muxTracer app channel = do
    let bearer = pipeAsMuxBearer muxTracer channel
    Mx.muxStart muxTracer app bearer

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Mux.Bearer.Pipe (
    pipeAsMuxBearer
  , runMuxWithPipes
  ) where

import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Tracer
import qualified Data.ByteString.Lazy as BL
import           GHC.Stack
import           System.IO (Handle, hFlush)

import qualified Network.Mux as Mx
import           Network.Mux.Types (MuxBearer)
import qualified Network.Mux.Types as Mx
import qualified Network.Mux.Codec as Mx
import qualified Network.Mux.Interface as Mx
import qualified Network.Mux.Time as Mx


pipeAsMuxBearer
  :: Tracer IO (Mx.MuxTrace ptcl)
  -> Handle -- ^ read handle
  -> Handle -- ^ write handle
  -> IO (MuxBearer IO)
pipeAsMuxBearer tracer pcRead pcWrite = do
      mxState <- atomically $ newTVar Mx.Larval
      return $ Mx.MuxBearer {
          Mx.read = readPipe,
          Mx.write = writePipe,
          Mx.sduSize = 32768,
          Mx.state = mxState
        }
    where
      readPipe :: HasCallStack => IO (Mx.MuxSDU, Time)
      readPipe = do
          traceWith tracer $ Mx.MuxTraceRecvHeaderStart
          hbuf <- recvLen' pcRead 8 []
          case Mx.decodeMuxSDU hbuf of
              Left e     -> throwM e
              Right header -> do
                  --say $ printf "decoded mux header, goint to read %d bytes" (Mx.msLength header)
                  traceWith tracer $ Mx.MuxTraceRecvHeaderEnd header
                  traceWith tracer $ Mx.MuxTraceRecvPayloadStart (fromIntegral $ Mx.msLength header)
                  blob <- recvLen' pcRead (fromIntegral $ Mx.msLength header) []
                  ts <- getMonotonicTime
                  traceWith tracer (Mx.MuxTraceRecvDeltaQObservation header ts)
                  traceWith tracer $ Mx.MuxTraceRecvPayloadEnd blob
                  --hexDump blob ""
                  return (header {Mx.msBlob = blob}, ts)

      recvLen' :: Handle -> Int -> [BL.ByteString] -> IO BL.ByteString
      recvLen' _ 0 bufs = return $ BL.concat $ reverse bufs
      recvLen' pd l bufs = do
          traceWith tracer $ Mx.MuxTraceRecvStart l
          buf <- BL.hGet pd l
                    `catch` Mx.handleIOException "hGet errored"
          if BL.null buf
              then throwM $ Mx.MuxError Mx.MuxBearerClosed "Pipe closed when reading data" callStack
              else do
                  traceWith tracer $ Mx.MuxTraceRecvEnd buf
                  recvLen' pd (l - fromIntegral (BL.length buf)) (buf : bufs)

      writePipe :: Mx.MuxSDU -> IO Time
      writePipe sdu = do
          ts <- getMonotonicTime
          let ts32 = Mx.timestampMicrosecondsLow32Bits ts
              sdu' = sdu { Mx.msTimestamp = Mx.RemoteClockModel ts32 }
              buf  = Mx.encodeMuxSDU sdu'
          traceWith tracer $ Mx.MuxTraceSendStart sdu'
          BL.hPut pcWrite buf
              `catch` Mx.handleIOException "hPut errored"
          hFlush pcWrite
          traceWith tracer $ Mx.MuxTraceSendEnd
          return ts

runMuxWithPipes
    :: ( Mx.ProtocolEnum ptcl, Ord ptcl, Enum ptcl, Bounded ptcl, Show ptcl
       , Mx.MiniProtocolLimits ptcl)
    => Tracer IO (Mx.WithMuxBearer String (Mx.MuxTrace ptcl))
    -> peerid
    -> Mx.MuxApplication appType peerid ptcl IO a b
    -> Handle -- ^ read handle
    -> Handle -- ^ write handle
    -> IO ()
runMuxWithPipes tracer peerid app pcRead pcWrite = do
    let muxTracer = Mx.WithMuxBearer "Pipe" `contramap` tracer
    bearer <- pipeAsMuxBearer muxTracer pcRead pcWrite
    Mx.muxStart muxTracer peerid app bearer


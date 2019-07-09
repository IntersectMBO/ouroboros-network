{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Mux.Bearer.Pipe (
    pipeAsMuxBearer
  , runMuxWithPipes
  ) where

import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Data.Word
import qualified Data.ByteString.Lazy as BL
import           GHC.Stack
import           System.IO (Handle, hClose, hFlush)

import qualified Network.Mux as Mx
import           Network.Mux.Types (MuxBearer)
import qualified Network.Mux.Types as Mx
import qualified Network.Mux.Codec as Mx
import qualified Network.Mux.Interface as Mx
import qualified Network.Mux.Time as Mx


pipeAsMuxBearer
  :: forall ptcl.
     ( Mx.ProtocolEnum ptcl
     , Ord ptcl
     , Enum ptcl
     , Bounded ptcl
     )
  => Handle -- ^ read handle
  -> Handle -- ^ write handle
  -> IO (MuxBearer ptcl IO)
pipeAsMuxBearer pcRead pcWrite = do
      mxState <- atomically $ newTVar Mx.Larval
      return $ Mx.MuxBearer {
          Mx.read = readPipe,
          Mx.write = writePipe,
          Mx.close = closePipe,
          Mx.sduSize = sduSize,
          Mx.state = mxState
        }
    where
      readPipe :: (HasCallStack)
               => IO (Mx.MuxSDU ptcl, Time IO)
      readPipe = do
          hbuf <- recvLen' pcRead 8 []
          case Mx.decodeMuxSDU hbuf of
              Left e     -> throwM e
              Right header -> do
                  --say $ printf "decoded mux header, goint to read %d bytes" (Mx.msLength header)
                  blob <- recvLen' pcRead (fromIntegral $ Mx.msLength header) []
                  ts <- getMonotonicTime
                  --hexDump blob ""
                  return (header {Mx.msBlob = blob}, ts)

      recvLen' :: Handle -> Int -> [BL.ByteString] -> IO BL.ByteString
      recvLen' _ 0 bufs = return $ BL.concat $ reverse bufs
      recvLen' pd l bufs = do
          buf <- BL.hGet pd l
          if BL.null buf
              then throwM $ Mx.MuxError Mx.MuxBearerClosed "Pipe closed when reading data" callStack
              else recvLen' pd (l - fromIntegral (BL.length buf)) (buf : bufs)

      writePipe :: Mx.MuxSDU ptcl
                -> IO (Time IO)
      writePipe sdu = do
          ts <- getMonotonicTime
          let ts32 = Mx.timestampMicrosecondsLow32Bits ts
              sdu' = sdu { Mx.msTimestamp = Mx.RemoteClockModel ts32 }
              buf  = Mx.encodeMuxSDU sdu'
          BL.hPut pcWrite buf
          hFlush pcWrite
          return ts

      closePipe :: IO ()
      closePipe = do
          hClose pcRead
          hClose pcWrite

      sduSize :: IO Word16
      sduSize = return 32768

runMuxWithPipes
    :: ( Mx.ProtocolEnum ptcl, Ord ptcl, Enum ptcl, Bounded ptcl, Show ptcl
       , Mx.MiniProtocolLimits ptcl)
    => Mx.MuxApplication appType ptcl IO BL.ByteString a b
    -> Handle -- ^ read handle
    -> Handle -- ^ write handle
    -> IO ()
runMuxWithPipes app pcRead pcWrite = do
    bearer <- pipeAsMuxBearer pcRead pcWrite
    Mx.muxStart app bearer


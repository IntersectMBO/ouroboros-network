{-# LANGUAGE CPP                 #-}
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
import           Network.Mux.Win32.InterruptibleRead

import           Network.Mux.Bearer.Socket (hexDump) -- XXX

import Text.Printf

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
          printf "read pipe!\n"
          hbuf <- recvLen' pcRead 8 []
          hexDump hbuf "read header: "
          case Mx.decodeMuxSDU hbuf of
              Left e     -> do
                  printf "failed to decode mux header %s" (show e)
                  throwM e
              Right header -> do
                  printf "decoded mux header, goint to read %d bytes\n" (Mx.msLength header)
                  blob <- recvLen' pcRead (fromIntegral $ Mx.msLength header) []
                  ts <- getMonotonicTime
                  hexDump blob ""
                  return (header {Mx.msBlob = blob}, ts)

      recvLen' :: Handle -> Int -> [BL.ByteString] -> IO BL.ByteString
      recvLen' _ 0 bufs = return $ BL.concat $ reverse bufs
      recvLen' pd l bufs = do
#if defined(mingw32_HOST_OS)
          bufStrict <- iread pd l
          let buf = BL.fromStrict bufStrict
#else
          buf <- BL.hGet pd l
#endif
          if BL.null buf
              then do 
                printf "Pipe closed"
                return BL.empty
                --throwM $ Mx.MuxError Mx.MuxBearerClosed "Pipe closed when reading data" callStack
              else recvLen' pd (l - fromIntegral (BL.length buf)) (buf : bufs)

      writePipe :: Mx.MuxSDU ptcl
                -> IO (Time IO)
      writePipe sdu = do
          ts <- getMonotonicTime
          let ts32 = Mx.timestampMicrosecondsLow32Bits ts
              sdu' = sdu { Mx.msTimestamp = Mx.RemoteClockModel ts32 }
              buf  = Mx.encodeMuxSDU sdu'
          hexDump buf "Pipe writing "
          we <- try $ BL.hPut pcWrite buf
          case we of
               Left (e::SomeException) -> do
                   printf "hPut failed with %s\n" (show e)
                   throwM e
               Right _ -> printf "Wrote done\n"
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
    => peerid
    -> Mx.MuxApplication appType peerid ptcl IO a b
    -> Handle -- ^ read handle
    -> Handle -- ^ write handle
    -> IO ()
runMuxWithPipes peerid app pcRead pcWrite = do
    bearer <- pipeAsMuxBearer pcRead pcWrite
    Mx.muxStart peerid app bearer


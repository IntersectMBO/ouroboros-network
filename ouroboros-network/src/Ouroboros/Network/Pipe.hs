{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.Pipe (
    startPipe
  ) where

import           Control.Monad
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Data.Word
import           GHC.Stack
import           System.IO (Handle, hClose, hFlush)

import qualified Ouroboros.Network.Mux as Mx
import           Ouroboros.Network.Time
import           Ouroboros.Network.Mux.Types (MuxBearer)
import qualified Ouroboros.Network.Mux.Types as Mx

import qualified Data.ByteString.Lazy as BL

data PipeCtx = PipeCtx {
      pcRead  :: Handle
    , pcWrite :: Handle
    }

pipeAsMuxBearer
  :: forall ptcl.
     ( Mx.ProtocolEnum ptcl
     , Ord ptcl
     , Enum ptcl
     , Bounded ptcl
     )
  => PipeCtx
  -> IO (MuxBearer ptcl IO)
pipeAsMuxBearer ctx = do
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
          hbuf <- recvLen' (pcRead ctx) 8 []
          case Mx.decodeMuxSDUHeader hbuf of
              Left e     -> throwM e
              Right header -> do
                  --say $ printf "decoded mux header, goint to read %d bytes" (Mx.msLength header)
                  blob <- recvLen' (pcRead ctx)
                                    (fromIntegral $ Mx.msLength header) []
                  ts <- getMonotonicTime
                  --say $ (scName ctx) ++ " read blob"
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
          let ts32 = timestampMicrosecondsLow32Bits ts
              sdu' = sdu { Mx.msTimestamp = Mx.RemoteClockModel ts32 }
              buf  = Mx.encodeMuxSDU sdu'
          BL.hPut (pcWrite ctx) buf
          hFlush (pcWrite ctx)
          return ts

      closePipe :: IO ()
      closePipe = do
          hClose (pcRead ctx)
          hClose (pcWrite ctx)

      sduSize :: IO Word16
      sduSize = return 32768


startPipe :: (Mx.ProtocolEnum ptcl, Ord ptcl, Enum ptcl, Bounded ptcl)
          => Mx.MiniProtocolDescriptions ptcl IO
          -> (Handle, Handle) -> IO ()
startPipe mpds (r, w) = do
    let ctx = PipeCtx r w
    bearer <- pipeAsMuxBearer ctx
    void $ async $ Mx.muxStart mpds bearer Nothing


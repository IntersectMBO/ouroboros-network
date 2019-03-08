{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.Pipe (
    startPipe
  ) where

import           Control.Monad
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTimer
import           Data.Bits
import           Data.Word
import           GHC.Stack
import           System.IO (Handle, hClose, hFlush)

import qualified Ouroboros.Network.Mux as Mx


import qualified Data.ByteString.Lazy as BL

data PipeCtx = PipeCtx {
      pcRead  :: Handle
    , pcWrite :: Handle
    }

closePipe :: PipeCtx -> IO ()
closePipe ctx = do
    hClose (pcRead ctx)
    hClose (pcWrite ctx)

sduSize :: PipeCtx -> IO Word16
sduSize _ = return 32768

writePipe :: (Mx.ProtocolEnum ptcl, Ord ptcl, Enum ptcl, Bounded ptcl)
          => PipeCtx
          -> Mx.MuxSDU ptcl
          -> IO (Time IO)
writePipe ctx sdu = do
    ts <- getMonotonicTime
    let sdu' = sdu { Mx.msTimestamp = Mx.RemoteClockModel $ fromIntegral $ ts .&. 0xffffffff }
        buf = Mx.encodeMuxSDU sdu'
    BL.hPut (pcWrite ctx) buf
    hFlush (pcWrite ctx)
    return ts

readPipe :: (HasCallStack, Mx.ProtocolEnum ptcl, Ord ptcl, Enum ptcl, Bounded ptcl)
         => PipeCtx
         -> IO (Mx.MuxSDU ptcl, Time IO)
readPipe ctx = do
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
  where
    recvLen' :: Handle -> Int -> [BL.ByteString] -> IO BL.ByteString
    recvLen' _ 0 bufs = return $ BL.concat $ reverse bufs
    recvLen' pd l bufs = do
        buf <- BL.hGet pd l
        if BL.null buf
            then throwM $ Mx.MuxError Mx.MuxBearerClosed "Pipe closed when reading data" callStack
            else recvLen' pd (l - fromIntegral (BL.length buf)) (buf : bufs)

startPipe :: (Mx.ProtocolEnum ptcl, Ord ptcl, Enum ptcl, Bounded ptcl)
          => [(Mx.Version, Mx.MiniProtocolDescriptions ptcl IO)]
          -> Mx.MuxStyle
          -> (Handle, Handle) -> IO ()
startPipe verMpds style (r, w) = do
    let ctx = PipeCtx r w
    bearer <- Mx.muxBearerNew (writePipe ctx) (readPipe ctx) (sduSize ctx) (closePipe ctx)
    void $ async $ Mx.muxStart verMpds bearer style Nothing


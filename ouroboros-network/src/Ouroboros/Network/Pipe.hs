{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.Pipe (
    PipeCtx
  , createConnectedPipeCtx
  , runNetworkNodeWithPipe
  ) where

import           Control.Monad
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTimer
import           Data.Bits
import           Data.Word
import qualified Data.ByteString.Lazy as BL
import           GHC.Stack
import           System.Process (createPipe)
import           System.IO (Handle, hClose, hFlush)

import qualified Ouroboros.Network.Mux as Mx
import           Ouroboros.Network.Mux.Types (MuxBearer)
import qualified Ouroboros.Network.Mux.Types as Mx
import qualified Ouroboros.Network.Mux.Control as Mx
import qualified Ouroboros.Network.Mux.Interface as Mx


-- |
-- Pair of @'Handle'@ necessary to run a netowrk node.  You can create a pair
-- of connected @'PipeCtx'@ with @'createConnectedPipeCtx'@.
--
data PipeCtx = PipeCtx {
      pcRead  :: Handle
    , pcWrite :: Handle
    }

-- |
-- Create a pair of connected @'PipeCtx'@s.
-- 
createConnectedPipeCtx
  :: IO (PipeCtx, PipeCtx)
createConnectedPipeCtx = do
  (r,  w)  <- createPipe
  (r', w') <- createPipe
  return (PipeCtx r w', PipeCtx r' w)

pipeAsMuxBearer
  :: forall ptcl.
     ( Mx.ProtocolEnum ptcl
     , Ord ptcl
     , Enum ptcl
     , Bounded ptcl
     , Mx.MiniProtocolLimits ptcl
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
          let sdu' = sdu { Mx.msTimestamp = Mx.RemoteClockModel $ fromIntegral $ ts .&. 0xffffffff }
              buf = Mx.encodeMuxSDU sdu'
          BL.hPut (pcWrite ctx) buf
          hFlush (pcWrite ctx)
          return ts

      closePipe :: IO ()
      closePipe = do
          hClose (pcRead ctx)
          hClose (pcWrite ctx)

      sduSize :: IO Word16
      sduSize = return 32768

runNetworkNodeWithPipe
    :: (Mx.ProtocolEnum ptcl, Ord ptcl, Enum ptcl, Bounded ptcl, Show ptcl, Mx.MiniProtocolLimits ptcl)
    => [Mx.SomeVersion]
    -> (Mx.SomeVersion -> Maybe (ptcl -> Mx.MuxPeer IO))
    -> Mx.MuxStyle
    -> PipeCtx
    -> IO ()
runNetworkNodeWithPipe knownMuxVersions protocols style ctx = do
    let  mpds sv = (Mx.miniProtocolDescription . ) <$> protocols sv
    bearer <- pipeAsMuxBearer ctx
    void $ async $ Mx.muxStart knownMuxVersions mpds bearer style Nothing

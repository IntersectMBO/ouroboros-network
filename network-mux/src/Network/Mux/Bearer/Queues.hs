{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Mux.Bearer.Queues
  ( queuesAsMuxBearer
  , startMuxSTM
  ) where

import qualified Data.ByteString.Lazy as BL
import           Data.Word (Word16)

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import           Control.Monad.Class.MonadThrow

import qualified Network.Mux as Mx
import qualified Network.Mux.Interface as Mx
import           Network.Mux.Types (MuxBearer)
import qualified Network.Mux.Types as Mx
import           Network.Mux.Time as Mx


queuesAsMuxBearer
  :: forall ptcl m.
     ( MonadSTM   m
     , MonadTime  m
     , MonadThrow m
     , Mx.ProtocolEnum ptcl
     )
  => TBQueue m BL.ByteString
  -> TBQueue m BL.ByteString
  -> Word16
  -> Maybe (TBQueue m (Mx.MiniProtocolId ptcl, Mx.MiniProtocolMode, Time m))
  -> m (MuxBearer ptcl m)
queuesAsMuxBearer writeQueue readQueue sduSize traceQueue = do
      mxState <- atomically $ newTVar Mx.Larval
      return $ Mx.MuxBearer {
          Mx.read    = readMux,
          Mx.write   = writeMux,
          Mx.close   = return (),
          Mx.sduSize = sduSizeMux,
          Mx.state   = mxState
        }
    where
      readMux :: m (Mx.MuxSDU ptcl, Time m)
      readMux = do
          buf <- atomically $ readTBQueue readQueue
          let (hbuf, payload) = BL.splitAt 8 buf
          case Mx.decodeMuxSDUHeader hbuf of
              Left  e      -> throwM e
              Right header -> do
                  ts <- getMonotonicTime
                  case traceQueue of
                        Just q  -> atomically $ do
                            full <- isFullTBQueue q
                            if full then return ()
                                    else writeTBQueue q (Mx.msId header, Mx.msMode header, ts)
                        Nothing -> return ()
                  return (header {Mx.msBlob = payload}, ts)

      writeMux :: Mx.MuxSDU ptcl
               -> m (Time m)
      writeMux sdu = do
          ts <- getMonotonicTime
          let ts32 = Mx.timestampMicrosecondsLow32Bits ts
              sdu' = sdu { Mx.msTimestamp = Mx.RemoteClockModel ts32 }
              buf  = Mx.encodeMuxSDU sdu'
          atomically $ writeTBQueue writeQueue buf
          return ts

      sduSizeMux :: m Word16
      sduSizeMux = return $ sduSize

startMuxSTM
  :: ( MonadAsync m
     , MonadCatch m
     , MonadMask m
     , MonadSay m
     , MonadSTM m
     , MonadThrow m 
     , MonadThrow (STM m)
     , MonadTime m
     , MonadTimer m
     , Ord ptcl
     , Enum ptcl
     , Bounded ptcl
     , Mx.ProtocolEnum ptcl
     , Show ptcl
     , Mx.MiniProtocolLimits ptcl
     )
  => Mx.MuxApplication appType ptcl m BL.ByteString a b
  -> TBQueue m BL.ByteString
  -> TBQueue m BL.ByteString
  -> Word16
  -> Maybe (TBQueue m (Mx.MiniProtocolId ptcl, Mx.MiniProtocolMode, Time m))
  -> m (Async m (Maybe SomeException))
startMuxSTM app wq rq mtu trace = async spawn
  where
    spawn = bracket (queuesAsMuxBearer wq rq mtu trace) Mx.close $ \bearer -> do
        res_e <- try $ Mx.muxStart app bearer
        case res_e of
             Left  e -> return (Just e)
             Right _ -> return Nothing

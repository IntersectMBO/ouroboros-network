{-# LANGUAGE GADTs               #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.KeepAlive
  ( KeepAliveInterval (..)
  , keepAliveClient
  , keepAliveServer

  , TraceKeepAliveClient
  ) where

import           Control.Exception (assert)
import qualified Control.Monad.Class.MonadSTM as Lazy
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import           Control.Tracer (Tracer, traceWith)
import qualified Data.Map.Strict as M

import           Ouroboros.Network.Mux (RunOrStop (..))
import           Ouroboros.Network.DeltaQ
import           Ouroboros.Network.Protocol.KeepAlive.Client
import           Ouroboros.Network.Protocol.KeepAlive.Server


newtype KeepAliveInterval = KeepAliveInterval { keepAliveInterval :: DiffTime }

data TraceKeepAliveClient peer =
    AddSample peer DiffTime PeerGSV

instance Show peer => Show (TraceKeepAliveClient peer) where
    show (AddSample peer rtt gsv) = "AddSample " ++ show peer ++ " sample: " ++ show rtt
        ++ " gsv: " ++ show gsv

keepAliveClient
    :: forall m peer.
       ( MonadSTM   m
       , MonadMonotonicTime m
       , MonadTimer m
       , Ord peer
       )
    => Tracer m (TraceKeepAliveClient peer)
    -> peer
    -> (StrictTVar m (M.Map peer PeerGSV))
    -> KeepAliveInterval
    -> StrictTVar m (Maybe Time)
    -> KeepAliveClient m ()
keepAliveClient tracer peer dqCtx KeepAliveInterval { keepAliveInterval } startTimeV =
    SendMsgKeepAlive go
  where
    payloadSize = 2

    decisionSTM :: Lazy.TVar m Bool
                -> STM  m RunOrStop
    decisionSTM delayVar = Lazy.readTVar delayVar >>= fmap (const Run) . check

    go :: m (KeepAliveClient m ())
    go = do
      endTime <- getMonotonicTime
      startTime_m <- atomically $ readTVar startTimeV
      case startTime_m of
           Just startTime -> do
               let rtt = diffTime endTime startTime
                   sample = fromSample startTime endTime payloadSize
               gsv' <- atomically $ do
                   m <- readTVar dqCtx
                   assert (peer `M.member` m) $ do
                     let m' = M.adjust (\a -> a <> sample) peer m
                     writeTVar dqCtx m'
                     return $ (M.!) m' peer
               traceWith tracer $ AddSample peer rtt gsv'

           Nothing        -> return ()

      let keepAliveInterval' = case startTime_m of
                                    Just _  -> keepAliveInterval
                                    Nothing -> 0 -- The first time we send a packet directly.

      delayVar <- registerDelay keepAliveInterval'
      decision <- atomically (decisionSTM delayVar)
      now <- getMonotonicTime
      atomically $ writeTVar startTimeV $ Just now
      case decision of
        Run  -> pure (SendMsgKeepAlive go)
        Stop -> pure (SendMsgDone (pure ()))


keepAliveServer
  :: forall m.  Applicative m
  => KeepAliveServer m ()
keepAliveServer = KeepAliveServer {
    recvMsgKeepAlive = pure keepAliveServer,
    recvMsgDone      = pure ()
  }

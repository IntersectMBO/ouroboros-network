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
import           Data.Maybe (fromJust)
import qualified Data.Map.Strict as M
import           System.Random (StdGen, random)

import           Ouroboros.Network.Mux (RunOrStop (..), ScheduledStop)
import           Ouroboros.Network.DeltaQ
import           Ouroboros.Network.Protocol.KeepAlive.Client
import           Ouroboros.Network.Protocol.KeepAlive.Server
import           Ouroboros.Network.Protocol.KeepAlive.Type


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
    -> StdGen
    -> ScheduledStop m
    -> peer
    -> (StrictTVar m (M.Map peer PeerGSV))
    -> KeepAliveInterval
    -> KeepAliveClient m ()
keepAliveClient tracer inRng shouldStopSTM peer dqCtx KeepAliveInterval { keepAliveInterval } =
    let (cookie, rng) = random inRng in
    SendMsgKeepAlive (Cookie cookie) (go rng Nothing)
  where
    payloadSize = 2

    decisionSTM :: Lazy.TVar m Bool
                -> STM  m RunOrStop
    decisionSTM delayVar = do
       shouldStop <- shouldStopSTM
       case shouldStop of
            Stop -> return Stop
            Run  -> do
              done <- Lazy.readTVar delayVar
              if done
                 then return Run
                 else retry

    go :: StdGen -> Maybe Time -> m (KeepAliveClient m ())
    go rng startTime_m = do
      endTime <- getMonotonicTime
      case startTime_m of
           Just startTime -> do
               let rtt = diffTime endTime startTime
                   sample = fromSample startTime endTime payloadSize
               gsv' <- atomically $ do
                   m <- readTVar dqCtx
                   assert (peer `M.member` m) $ do
                     let (gsv', m') = M.updateLookupWithKey (\_ a -> Just $ a <> sample) peer m
                     writeTVar dqCtx m'
                     return $ fromJust gsv'
               traceWith tracer $ AddSample peer rtt gsv'

           Nothing        -> return ()

      let keepAliveInterval' = case startTime_m of
                                    Just _  -> keepAliveInterval
                                    Nothing -> 0 -- The first time we send a packet directly.

      delayVar <- registerDelay keepAliveInterval'
      decision <- atomically (decisionSTM delayVar)
      now <- getMonotonicTime
      case decision of
        Run  ->
            let (cookie, rng') = random rng in
            pure (SendMsgKeepAlive (Cookie cookie) $ go rng' $ Just now)
        Stop -> pure (SendMsgDone (pure ()))


keepAliveServer
  :: forall m.  Applicative m
  => KeepAliveServer m ()
keepAliveServer = KeepAliveServer {
    recvMsgKeepAlive = pure keepAliveServer,
    recvMsgDone      = pure ()
  }

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.KeepAlive
  ( KeepAliveInterval (..)
  , keepAliveClient
  , keepAliveServer
  , TraceKeepAliveClient (..)
  ) where

import Control.Concurrent.Class.MonadSTM qualified as Lazy
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Exception (assert)
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import Control.Tracer (Tracer, traceWith)
import Data.Map.Strict qualified as M
import Data.Maybe (fromJust)
import System.Random (StdGen, random)

import Ouroboros.Network.ControlMessage (ControlMessage (..), ControlMessageSTM)
import Ouroboros.Network.DeltaQ
import Ouroboros.Network.Protocol.KeepAlive.Client
import Ouroboros.Network.Protocol.KeepAlive.Server
import Ouroboros.Network.Protocol.KeepAlive.Type


newtype KeepAliveInterval = KeepAliveInterval { keepAliveInterval :: DiffTime }

data TraceKeepAliveClient peer =
    AddSample peer DiffTime PeerGSV

instance Show peer => Show (TraceKeepAliveClient peer) where
    show (AddSample peer rtt gsv) = "AddSample " ++ show peer ++ " sample: " ++ show rtt
        ++ " gsv: " ++ show gsv

keepAliveClient
    :: forall m peer.
       ( MonadTimer m
       , Ord peer
       )
    => Tracer m (TraceKeepAliveClient peer)
    -> StdGen
    -> ControlMessageSTM m
    -> peer
    -> StrictTVar m (M.Map peer PeerGSV)
    -> KeepAliveInterval
    -> KeepAliveClient m ()
keepAliveClient tracer inRng controlMessageSTM peer dqCtx KeepAliveInterval { keepAliveInterval } =
    let (cookie, rng) = random inRng in
    KeepAliveClient $ do
      startTime <- getMonotonicTime
      return $ SendMsgKeepAlive (Cookie cookie) (go rng startTime)
  where
    payloadSize = 2

    decisionSTM :: Lazy.TVar m Bool
                -> STM  m ControlMessage
    decisionSTM delayVar = do
       controlMessage <- controlMessageSTM
       case controlMessage of
            Terminate -> return Terminate

            -- Continue
            _  -> do
              done <- Lazy.readTVar delayVar
              if done
                 then return Continue
                 else retry

    go :: StdGen -> Time -> m (KeepAliveClientSt m ())
    go rng startTime = do
      endTime <- getMonotonicTime
      let rtt = diffTime endTime startTime
          sample = fromSample startTime endTime payloadSize
      gsv' <- atomically $ do
          m <- readTVar dqCtx
          assert (peer `M.member` m) $ do
            let (gsv', m') = M.updateLookupWithKey
                    (\_ a -> if sampleTime a == Time 0 -- Ignore the initial dummy value
                                then Just sample
                                else Just $ sample <> a
                    ) peer m
            writeTVar dqCtx m'
            return $ fromJust gsv'
      traceWith tracer $ AddSample peer rtt gsv'

      delayVar <- registerDelay keepAliveInterval
      decision <- atomically (decisionSTM delayVar)
      now <- getMonotonicTime
      case decision of
        -- 'decisionSTM' above cannot return 'Quiesce'
        Quiesce   -> error "keepAliveClient: impossible happened"
        Continue  ->
            let (cookie, rng') = random rng in
            pure (SendMsgKeepAlive (Cookie cookie) $ go rng' now)
        Terminate -> pure (SendMsgDone (pure ()))


keepAliveServer
  :: forall m.  Applicative m
  => KeepAliveServer m ()
keepAliveServer = KeepAliveServer {
    recvMsgKeepAlive = pure keepAliveServer,
    recvMsgDone      = pure ()
  }

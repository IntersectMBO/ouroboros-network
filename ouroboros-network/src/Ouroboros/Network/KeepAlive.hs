{-# LANGUAGE GADTs               #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.KeepAlive
  ( KeepAliveInterval (..)
  , keepAliveClient
  , keepAliveServer
  ) where

import qualified Control.Monad.Class.MonadSTM as Lazy
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import qualified Data.Map.Strict as M

import           Ouroboros.Network.Mux (RunOrStop (..))
import           Ouroboros.Network.DeltaQ
import           Ouroboros.Network.Protocol.KeepAlive.Client
import           Ouroboros.Network.Protocol.KeepAlive.Server


newtype KeepAliveInterval = KeepAliveInterval { keepAliveInterval :: DiffTime }

keepAliveClient
    :: forall m peer.
       ( MonadSTM   m
       , MonadMonotonicTime m
       , MonadTimer m
       , Ord peer
       )
    => peer
    -> (StrictTVar m (M.Map peer PeerGSV))
    -> KeepAliveInterval
    -> StrictTVar m (Maybe Time)
    -> KeepAliveClient m ()
keepAliveClient _peer _dqCtx KeepAliveInterval { keepAliveInterval } startTimeV =
    SendMsgKeepAlive go
  where
    decisionSTM :: Lazy.TVar m Bool
                -> STM  m RunOrStop
    decisionSTM delayVar = Lazy.readTVar delayVar >>= fmap (const Run) . check

    go :: m (KeepAliveClient m ())
    go = do
      _endTime <- getMonotonicTime
      startTime_m <- atomically $ readTVar startTimeV
      case startTime_m of
           Just _startTime -> return () -- TODO add sample
           Nothing        -> return ()

      delayVar <- registerDelay keepAliveInterval
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

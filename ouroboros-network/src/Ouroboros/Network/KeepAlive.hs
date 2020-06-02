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

import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadTime (DiffTime)
import           Control.Monad.Class.MonadTimer

import           Ouroboros.Network.Mux (RunOrStop (..), ScheduledStop)
import           Ouroboros.Network.Protocol.KeepAlive.Client
import           Ouroboros.Network.Protocol.KeepAlive.Server


newtype KeepAliveInterval = KeepAliveInterval { keepAliveInterval :: DiffTime }


keepAliveClient
    :: forall m.
       ( MonadSTM   m
       , MonadTimer m
       )
    => ScheduledStop m
    -> KeepAliveInterval
    -> KeepAliveClient m ()
keepAliveClient shouldStopSTM KeepAliveInterval { keepAliveInterval } =
    SendMsgKeepAlive go
  where
    decisionSTM :: TVar m Bool
                -> STM  m RunOrStop
    decisionSTM delayVar =
      do
       readTVar delayVar >>= fmap (const Run) . check
      `orElse`
      shouldStopSTM

    go :: m (KeepAliveClient m ())
    go = do
      delayVar <- registerDelay keepAliveInterval
      decision <- atomically (decisionSTM delayVar)
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

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module MonadClass.MonadSTMTimer
  ( MonadSTMTimer (..)
  ) where

import qualified Control.Concurrent.STM.TVar as STM
import qualified Control.Monad.STM as STM

import qualified GHC.Event as GHC (TimeoutKey, getSystemTimerManager,
                     registerTimeout, unregisterTimeout, updateTimeout)

import           MonadClass.MonadSTM
import           MonadClass.MonadTimer

data TimeoutState = TimeoutPending | TimeoutFired | TimeoutCancelled

class MonadSTM m stm => MonadSTMTimer m stm where
  data Timeout m :: *

  timeoutState   :: Timeout m -> stm TimeoutState

  newTimeout     :: Duration (Time m) -> m (Timeout m)
  updateTimeout  :: Timeout m -> Duration (Time m) -> m ()
  cancelTimeout  :: Timeout m -> m ()

instance MonadSTMTimer IO STM.STM where
  data Timeout IO = TimeoutIO !(STM.TVar TimeoutState) !GHC.TimeoutKey

  timeoutState (TimeoutIO var _key) = STM.readTVar var

  newTimeout = \usec -> do
      var <- STM.newTVarIO TimeoutPending
      mgr <- GHC.getSystemTimerManager
      key <- GHC.registerTimeout mgr usec (STM.atomically (timeoutAction var))
      return (TimeoutIO var key)
    where
      timeoutAction var = do
        x <- STM.readTVar var
        case x of
          TimeoutPending   -> STM.writeTVar var TimeoutFired
          TimeoutFired     -> error "MonadSTMTimer(IO): invariant violation"
          TimeoutCancelled -> return ()

  updateTimeout (TimeoutIO _var key) usec = do
      mgr <- GHC.getSystemTimerManager
      GHC.updateTimeout mgr key usec

  cancelTimeout (TimeoutIO var key) = do
      STM.atomically $ do
        x <- STM.readTVar var
        case x of
          TimeoutPending   -> STM.writeTVar var TimeoutCancelled
          TimeoutFired     -> return ()
          TimeoutCancelled -> return ()
      mgr <- GHC.getSystemTimerManager
      GHC.unregisterTimeout mgr key

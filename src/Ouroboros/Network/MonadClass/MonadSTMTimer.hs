{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Ouroboros.Network.MonadClass.MonadSTMTimer (
    MonadSTMTimer(..)
  , TimeoutState(..)
  ) where

import           Data.Functor (void)
import qualified Control.Concurrent          as IO
import qualified Control.Concurrent.STM.TVar as STM
import qualified Control.Monad.STM           as STM

import qualified GHC.Event as GHC (TimeoutKey, getSystemTimerManager,
                     registerTimeout, unregisterTimeout, updateTimeout)

import           Ouroboros.Network.MonadClass.MonadSTM
import           Ouroboros.Network.MonadClass.MonadTimer

data TimeoutState = TimeoutPending | TimeoutFired | TimeoutCancelled

class MonadSTM m => MonadSTMTimer m where
  data Timeout m :: *

  readTimeout    :: Timeout m -> Tr m TimeoutState

  newTimeout     :: Duration (Time m) -> m (Timeout m)
  updateTimeout  :: Timeout m -> Duration (Time m) -> m ()
  cancelTimeout  :: Timeout m -> m ()

  -- | Returns @True@ when the timeout is fired, or @False@ if it is cancelled.
  awaitTimeout   :: Timeout m -> Tr m Bool
  awaitTimeout t  = do s <- readTimeout t
                       case s of
                         TimeoutPending   -> retry
                         TimeoutFired     -> return True
                         TimeoutCancelled -> return False

  threadDelay    :: Duration (Time m) -> m ()
  threadDelay d   = void . atomically . awaitTimeout =<< newTimeout d

instance MonadSTMTimer IO where
  data Timeout IO = TimeoutIO !(STM.TVar TimeoutState) !GHC.TimeoutKey

  readTimeout (TimeoutIO var _key) = STM.readTVar var

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

  threadDelay d = IO.threadDelay d

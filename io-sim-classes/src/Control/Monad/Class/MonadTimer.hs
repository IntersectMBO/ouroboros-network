{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DefaultSignatures          #-}

module Control.Monad.Class.MonadTimer (
    MonadTimer(..)
  , TimeoutState(..)
  ) where

import qualified Control.Concurrent as IO
import qualified Control.Concurrent.STM.TVar as STM
import qualified Control.Monad.STM as STM
import           Control.Exception (assert)
import           Data.Functor (void)
import           Data.Time.Clock (DiffTime, diffTimeToPicoseconds)

import qualified GHC.Event as GHC (TimeoutKey, getSystemTimerManager,
                     registerTimeout, unregisterTimeout, updateTimeout)

import           Control.Monad.Class.MonadFork (MonadFork(..))
import           Control.Monad.Class.MonadSTM


data TimeoutState = TimeoutPending | TimeoutFired | TimeoutCancelled

class MonadSTM m => MonadTimer m where
  data Timeout m :: *

  -- | Create a new timeout which will fire at the given time duration in
  -- the future.
  --
  -- The timeout will start in the 'TimeoutPending' state and either
  -- fire at or after the given time leaving it in the 'TimeoutFired' state,
  -- or it may be cancelled with 'cancelTimeout', leaving it in the
  -- 'TimeoutCancelled' state.
  --
  -- Timeouts /cannot/ be reset to the pending state once fired or cancelled
  -- (as this would be very racy). You should create a new timeout if you need
  -- this functionality.
  --
  newTimeout     :: DiffTime -> m (Timeout m)

  -- | Read the current state of a timeout. This does not block, but returns
  -- the current state. It is your responsibility to use 'retry' to wait.
  --
  -- Alternatively you may wish to use the convenience utility 'awaitTimeout'
  -- to wait for just the fired or cancelled outcomes.
  --
  -- You should consider the cancelled state if you plan to use 'cancelTimeout'.
  --
  readTimeout    :: Timeout m -> STM m TimeoutState

  -- Adjust when this timer will fire, to the given duration into the future.
  --
  -- It is safe to race this concurrently against the timer firing. It will
  -- have no effect if the timer fires first.
  --
  -- The new time can be before or after the original expiry time, though
  -- arguably it is an application design flaw to move timeouts sooner.
  --
  updateTimeout  :: Timeout m -> DiffTime -> m ()

  -- | Cancel a timeout (unless it has already fired), putting it into the
  -- 'TimeoutCancelled' state. Code reading and acting on the timeout state
  -- need to handle such cancellation appropriately.
  --
  -- It is safe to race this concurrently against the timer firing. It will
  -- have no effect if the timer fires first.
  --
  cancelTimeout  :: Timeout m -> m ()

  -- | Returns @True@ when the timeout is fired, or @False@ if it is cancelled.
  awaitTimeout   :: Timeout m -> STM m Bool
  awaitTimeout t  = do s <- readTimeout t
                       case s of
                         TimeoutPending   -> retry
                         TimeoutFired     -> return True
                         TimeoutCancelled -> return False

  threadDelay    :: DiffTime -> m ()
  threadDelay d   = void . atomically . awaitTimeout =<< newTimeout d

  registerDelay :: DiffTime -> m (TVar m Bool)

  default registerDelay :: MonadFork m => DiffTime -> m (TVar m Bool)
  registerDelay d = do
    v <- atomically $ newTVar False
    t <- newTimeout d
    _ <- fork $ atomically (awaitTimeout t >>= writeTVar v)
    return v


--
-- Instances for IO
--

instance MonadTimer IO where
  data Timeout IO = TimeoutIO !(STM.TVar TimeoutState) !GHC.TimeoutKey

  readTimeout (TimeoutIO var _key) = STM.readTVar var

  newTimeout = \d -> do
      var <- STM.newTVarIO TimeoutPending
      mgr <- GHC.getSystemTimerManager
      key <- GHC.registerTimeout mgr (diffTimeToMicrosecondsAsInt d)
                                     (STM.atomically (timeoutAction var))
      return (TimeoutIO var key)
    where
      timeoutAction var = do
        x <- STM.readTVar var
        case x of
          TimeoutPending   -> STM.writeTVar var TimeoutFired
          TimeoutFired     -> error "MonadTimer(IO): invariant violation"
          TimeoutCancelled -> return ()

  -- In GHC's TimerManager this has no effect if the timer already fired.
  -- It is safe to race against the timer firing.
  updateTimeout (TimeoutIO _var key) d = do
      mgr <- GHC.getSystemTimerManager
      GHC.updateTimeout mgr key (diffTimeToMicrosecondsAsInt d)

  cancelTimeout (TimeoutIO var key) = do
      STM.atomically $ do
        x <- STM.readTVar var
        case x of
          TimeoutPending   -> STM.writeTVar var TimeoutCancelled
          TimeoutFired     -> return ()
          TimeoutCancelled -> return ()
      mgr <- GHC.getSystemTimerManager
      GHC.unregisterTimeout mgr key

  threadDelay d = IO.threadDelay (diffTimeToMicrosecondsAsInt d)

  registerDelay = STM.registerDelay . diffTimeToMicrosecondsAsInt

diffTimeToMicrosecondsAsInt :: DiffTime -> Int
diffTimeToMicrosecondsAsInt d =
    let usec :: Integer
        usec = diffTimeToPicoseconds d `div` 1000000 in
    -- Can only represent usec times that fit within an Int, which on 32bit
    -- systems means 2^31 usec, which is only ~35 minutes.
    assert (usec <= fromIntegral (maxBound :: Int)) $
    fromIntegral usec


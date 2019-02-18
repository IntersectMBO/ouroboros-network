{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Control.Monad.Class.MonadTimer (
    MonadTime(..)
  , MonadTimer(..)
  , TimeoutState(..)
  , TimeMeasure(..)
  , mult
  , fromStart
  ) where

import qualified Control.Concurrent as IO
import qualified Control.Concurrent.STM.TVar as STM
import qualified Control.Monad.STM as STM
import           Data.Functor (void)
import           Data.Word (Word64)

import qualified GHC.Event as GHC (TimeoutKey, getSystemTimerManager,
                     registerTimeout, unregisterTimeout, updateTimeout)

import           Control.Monad.Class.MonadFork (fork)
import           Control.Monad.Class.MonadSTM


class (Ord t, Ord (Duration t), Num (Duration t)) => TimeMeasure t where
  type Duration t :: *

  diffTime :: t -> t -> Duration t
  addTime  :: Duration t -> t -> t
  zero :: t

-- | Helper function to multiply `Duration t` by an `Int`.
--
mult :: Num d => Int -> d -> d
mult n = sum . replicate n

-- | Count time since @'zero'@.
--
fromStart :: TimeMeasure t => Duration t -> t
fromStart = flip addTime zero

class (Monad m, TimeMeasure (Time m), Show (Time m)) => MonadTime m where
  type Time m :: *

  getMonotonicTime :: m (Time m)

data TimeoutState = TimeoutPending | TimeoutFired | TimeoutCancelled

class (MonadSTM m, MonadTime m) => MonadTimer m where
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
  newTimeout     :: Duration (Time m) -> m (Timeout m)

  -- | Read the current state of a timeout. This does not block, but returns
  -- the current state. It is your responsibility to use 'retry' to wait.
  --
  -- Alternatively you may wish to use the convenience utility 'awaitTimeout'
  -- to wait for just the fired or cancelled outcomes.
  --
  -- You should consider the cancelled state if you plan to use 'cancelTimeout'.
  --
  readTimeout    :: Timeout m -> Tr m TimeoutState

  -- Adjust when this timer will fire, to the given duration into the future.
  --
  -- It is safe to race this concurrently against the timer firing. It will
  -- have no effect if the timer fires first.
  --
  -- The new time can be before or after the original expiry time, though
  -- arguably it is an application design flaw to move timeouts sooner.
  --
  updateTimeout  :: Timeout m -> Duration (Time m) -> m ()

  -- | Cancel a timeout (unless it has already fired), putting it into the
  -- 'TimeoutCancelled' state. Code reading and acting on the timeout state
  -- need to handle such cancellation appropriately.
  --
  -- It is safe to race this concurrently against the timer firing. It will
  -- have no effect if the timer fires first.
  --
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

  registerDelay :: Duration (Time m) -> m (TVar m Bool)
  registerDelay d = do
    v <- atomically $ newTVar False
    t <- newTimeout d
    _ <- fork $ atomically (awaitTimeout t >>= writeTVar v)
    return v

--
-- Instances for IO
--

instance TimeMeasure Int where
  type Duration Int = Int -- microseconds

  diffTime t t' = t-t'
  addTime  d t  = t+d
  zero = 0

instance MonadTime IO where
  type Time IO = Int -- microseconds
  getMonotonicTime = fmap (fromIntegral . (`div` 1000)) getMonotonicNSec

foreign import ccall unsafe "getMonotonicNSec"
    getMonotonicNSec :: IO Word64

instance MonadTimer IO where
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
          TimeoutFired     -> error "MonadTimer(IO): invariant violation"
          TimeoutCancelled -> return ()

  -- In GHC's TimerManager this has no effect if the timer already fired.
  -- It is safe to race against the timer firing.
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

  registerDelay = STM.registerDelay

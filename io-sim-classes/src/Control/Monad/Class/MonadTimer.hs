{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Monad.Class.MonadTimer (
    MonadTime(..)
  , MonadTimer(..)
  , TimeoutState(..)
  , TimeMeasure(..)

    -- * Duration type
  , Duration
  , secondsDuration
  , microsecondsDuration
  , durationSeconds
  , durationMicroseconds
  , multiplyDuration
  ) where

import qualified Control.Concurrent as IO
import qualified Control.Concurrent.STM.TVar as STM
import qualified Control.Monad.STM as STM
import           Control.Exception (assert)
import           Data.Functor (void)
import           Data.Int (Int64)
import           Data.Word (Word64)
import           Data.Fixed as Fixed (Fixed(MkFixed), Micro)

import qualified GHC.Event as GHC (TimeoutKey, getSystemTimerManager,
                     registerTimeout, unregisterTimeout, updateTimeout)

import           Control.Monad.Class.MonadFork (MonadFork(..))
import           Control.Monad.Class.MonadSTM

-- | Time duration, with microsecond precision.
--
-- This is a vector in time so it can represent negative as well as positive
-- durations. This is useful for measuring the time between two events.
--
-- Construct using 'fromIntegral' and 'fromRational' in units of seconds. Use
-- 'microsecondsDuration' to create from microseconds.
--
-- Use 'Num' and 'Fractional' operations, and 'toRational', 'durationSeconds'
-- and 'durationMicroseconds' to convert.
--
newtype Duration = Duration Fixed.Micro
  deriving (Eq, Ord, Show, Num, Fractional)

-- | A duration in seconds, with microsecond precision.
durationSeconds :: Duration -> Fixed.Micro
durationSeconds (Duration micro) = micro

-- | A duration in microseconds.
durationMicroseconds :: Duration -> Int64
durationMicroseconds (Duration (MkFixed micro)) = fromIntegral micro

-- | Make a duration given a value in seconds, with microsecond precision.
secondsDuration :: Fixed.Micro -> Duration
secondsDuration = Duration

-- | Make a duration given a value in microseconds.
microsecondsDuration :: Int64 -> Duration
microsecondsDuration = Duration . MkFixed . fromIntegral

multiplyDuration :: Real a => a -> Duration -> Duration
multiplyDuration a (Duration d) = Duration (realToFrac a * d)


-- | A type that represents points in time. The operations
--
class Ord t => TimeMeasure t where

  -- | The time duration between two points in time (positive or negative).
  diffTime  :: t -> t -> Duration

  -- | Add a duration to a point in time, giving another time.
  addTime   :: Duration -> t -> t

  -- | The time epoch where points in time are measured relative to.
  --
  -- For example POSIX time is relative to 1970-01-01 00:00 UTC. For a
  -- monotonic clock this would be an arbitrary point, not related to any
  -- wall clock time.
  --
  zeroTime  :: t

class (Monad m, TimeMeasure (Time m)) => MonadTime m where
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
  newTimeout     :: Duration -> m (Timeout m)

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
  updateTimeout  :: Timeout m -> Duration -> m ()

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

  threadDelay    :: Duration -> m ()
  threadDelay d   = void . atomically . awaitTimeout =<< newTimeout d

  registerDelay :: Duration -> m (TVar m Bool)

  default registerDelay :: MonadFork m => Duration -> m (TVar m Bool)
  registerDelay d = do
    v <- atomically $ newTVar False
    t <- newTimeout d
    _ <- fork $ atomically (awaitTimeout t >>= writeTVar v)
    return v

--
-- Instances for IO
--

-- | Time in a monotonic clock, with microsecond precision. The epoch for this
-- clock is arbitrary and does not correspond to any wall clock or calendar.
--
newtype MonotonicTimeIO = MonotonicTimeIO Int64
  deriving (Eq, Ord, Show)

instance TimeMeasure MonotonicTimeIO where
  diffTime (MonotonicTimeIO t) (MonotonicTimeIO t') =
    microsecondsDuration (t - t')

  addTime d (MonotonicTimeIO t) =
    MonotonicTimeIO (t + durationMicroseconds d)

  zeroTime = MonotonicTimeIO 0

instance MonadTime IO where
  type Time IO = MonotonicTimeIO
  getMonotonicTime = fmap (MonotonicTimeIO . fromIntegral . (`div` 1000))
                          getMonotonicNSec

foreign import ccall unsafe "getMonotonicNSec"
    getMonotonicNSec :: IO Word64

instance MonadTimer IO where
  data Timeout IO = TimeoutIO !(STM.TVar TimeoutState) !GHC.TimeoutKey

  readTimeout (TimeoutIO var _key) = STM.readTVar var

  newTimeout = \d -> do
      var <- STM.newTVarIO TimeoutPending
      mgr <- GHC.getSystemTimerManager
      key <- GHC.registerTimeout mgr (durationMicrosecondsAsInt d)
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
      GHC.updateTimeout mgr key (durationMicrosecondsAsInt d)

  cancelTimeout (TimeoutIO var key) = do
      STM.atomically $ do
        x <- STM.readTVar var
        case x of
          TimeoutPending   -> STM.writeTVar var TimeoutCancelled
          TimeoutFired     -> return ()
          TimeoutCancelled -> return ()
      mgr <- GHC.getSystemTimerManager
      GHC.unregisterTimeout mgr key

  threadDelay d = IO.threadDelay (durationMicrosecondsAsInt d)

  registerDelay = STM.registerDelay . durationMicrosecondsAsInt

durationMicrosecondsAsInt :: Duration -> Int
durationMicrosecondsAsInt d =
    let usec :: Int64
        usec = durationMicroseconds d in
    -- Can only represent usec times that fit within an Int, which on 32bit
    -- systems means 2^31 usec, which is only ~35 minutes.
    assert (usec <= fromIntegral (maxBound :: Int)) $
    fromIntegral usec


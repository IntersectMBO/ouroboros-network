{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Monad.Class.MonadTime (
    MonadTime(..)
  , TimeMeasure(..)

    -- * Duration type
  , Duration
  , secondsDuration
  , microsecondsDuration
  , durationSeconds
  , durationMicroseconds
  , multiplyDuration
  ) where

import           Data.Int (Int64)
import           Data.Word (Word64)
import           Data.Fixed as Fixed (Fixed(MkFixed), Micro)


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


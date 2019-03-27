{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Monad.Class.MonadTime (
    MonadTime(..)
  , TimeMeasure(..)
  , DiffTime
  ) where

import           Data.Word (Word64)
import           Data.Time.Clock (DiffTime)
import qualified Data.Time.Clock as Time


-- | A type that represents points in time.
--
class Ord t => TimeMeasure t where

  -- | The time duration between two points in time (positive or negative).
  diffTime  :: t -> t -> DiffTime

  -- | Add a duration to a point in time, giving another time.
  addTime   :: DiffTime -> t -> t

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

-- | Time in a monotonic clock, with high precision. The epoch for this
-- clock is arbitrary and does not correspond to any wall clock or calendar.
--
instance TimeMeasure DiffTime where
  diffTime t t' = t - t'
  addTime  d t  = d + t
  zeroTime      = 0

instance MonadTime IO where
  type Time IO = DiffTime
  getMonotonicTime = fmap (Time.picosecondsToDiffTime . (* 1000) . toInteger)
                          getMonotonicNSec

foreign import ccall unsafe "getMonotonicNSec"
    getMonotonicNSec :: IO Word64


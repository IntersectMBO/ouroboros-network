
module Control.Monad.Class.MonadTime (
    MonadTime(..)
  , Time(..)
  , diffTime
  , addTime
  , DiffTime
  , UTCTime
  ) where

import           Data.Word (Word64)
import           Data.Time.Clock (DiffTime, UTCTime)
import qualified Data.Time.Clock as Time


-- | A point in time in a monotonic clock.
--
-- The epoch for this clock is arbitrary and does not correspond to any wall
-- clock or calendar, and is /not guaranteed/ to be the same epoch across
-- program runs. It is represented as the 'DiffTime' from this arbitrary epoch.
--
newtype Time = Time DiffTime
  deriving (Eq, Ord, Show)

-- | The time duration between two points in time (positive or negative).
diffTime :: Time -> Time -> DiffTime
diffTime (Time t) (Time t') = t - t'

-- | Add a duration to a point in time, giving another time.
addTime :: DiffTime -> Time -> Time
addTime d (Time t) = Time (d + t)


class Monad m => MonadTime m where

  -- | Time in a monotonic clock, with high precision. The epoch for this
  -- clock is arbitrary and does not correspond to any wall clock or calendar.
  --
  getMonotonicTime :: m Time

  -- | Wall clock time.
  --
  getCurrentTime   :: m UTCTime


--
-- Instances for IO
--

instance MonadTime IO where
  getMonotonicTime =
      fmap conv getMonotonicNSec
    where
      conv :: Word64 -> Time
      conv = Time . Time.picosecondsToDiffTime . (* 1000) . toInteger

  getCurrentTime = Time.getCurrentTime

foreign import ccall unsafe "getMonotonicNSec"
    getMonotonicNSec :: IO Word64


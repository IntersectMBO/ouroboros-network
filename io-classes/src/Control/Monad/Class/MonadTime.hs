{-# LANGUAGE DeriveGeneric #-}

module Control.Monad.Class.MonadTime
  ( MonadTime (..)
  , MonadMonotonicTime (..)
    -- * 'DiffTime' and its action on 'Time'
  , Time (..)
  , diffTime
  , addTime
  , DiffTime
    -- * 'NominalTime' and its action on 'UTCTime'
  , UTCTime
  , diffUTCTime
  , addUTCTime
  , NominalDiffTime
  ) where

import           Control.Monad.Reader
import           Data.Time.Clock (DiffTime, NominalDiffTime, UTCTime,
                     addUTCTime, diffUTCTime)
import qualified Data.Time.Clock as Time
import           Data.Word (Word64)
import           GHC.Generics (Generic (..))

-- | A point in time in a monotonic clock.
--
-- The epoch for this clock is arbitrary and does not correspond to any wall
-- clock or calendar, and is /not guaranteed/ to be the same epoch across
-- program runs. It is represented as the 'DiffTime' from this arbitrary epoch.
--
newtype Time = Time DiffTime
  deriving (Eq, Ord, Show, Generic)

-- | The time duration between two points in time (positive or negative).
diffTime :: Time -> Time -> DiffTime
diffTime (Time t) (Time t') = t - t'

-- | Add a duration to a point in time, giving another time.
addTime :: DiffTime -> Time -> Time
addTime d (Time t) = Time (d + t)

infixr 9 `addTime`


class Monad m => MonadMonotonicTime m where
  -- | Time in a monotonic clock, with high precision. The epoch for this
  -- clock is arbitrary and does not correspond to any wall clock or calendar.
  --
  getMonotonicTime :: m Time

class MonadMonotonicTime m => MonadTime m where
  -- | Wall clock time.
  --
  getCurrentTime   :: m UTCTime

--
-- Instances for IO
--

instance MonadMonotonicTime IO where
  getMonotonicTime =
      fmap conv getMonotonicNSec
    where
      conv :: Word64 -> Time
      conv = Time . Time.picosecondsToDiffTime . (* 1000) . toInteger

instance MonadTime IO where
  getCurrentTime = Time.getCurrentTime

foreign import ccall unsafe "getMonotonicNSec"
    getMonotonicNSec :: IO Word64

--
-- Instance for ReaderT
--

instance MonadMonotonicTime m => MonadMonotonicTime (ReaderT r m) where
  getMonotonicTime = lift getMonotonicTime

instance MonadTime m => MonadTime (ReaderT r m) where
  getCurrentTime   = lift getCurrentTime

module Ouroboros.Consensus.BlockchainTime.WallClock.Default (defaultSystemTime) where

import           Control.Monad
import           Control.Tracer
import           Data.Time (UTCTime, diffUTCTime)

import           Control.Monad.Class.MonadTime (MonadTime (..))

import           Ouroboros.Consensus.BlockchainTime.WallClock.Types
import           Ouroboros.Consensus.BlockchainTime.WallClock.Util
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.Time

defaultSystemTime :: (MonadTime m, MonadDelay m)
                  => SystemStart
                  -> Tracer m (TraceBlockchainTimeEvent UTCTime)
                  -> SystemTime m
defaultSystemTime start tracer = SystemTime {
      systemTimeCurrent = toRelativeTime start <$> getCurrentTime
    , systemTimeWait    = waitForSystemStart start tracer
    }

-- | Wait until system start if necessary
waitForSystemStart :: (MonadTime m, MonadDelay m)
                   => SystemStart
                   -> Tracer m (TraceBlockchainTimeEvent UTCTime)
                   -> m ()
waitForSystemStart start tracer = do
    now <- getCurrentTime
    when (getSystemStart start > now) $ do
      let delay = getSystemStart start `diffUTCTime` now
      traceWith tracer $ TraceStartTimeInTheFuture start delay
      threadDelay (nominalDelay delay)

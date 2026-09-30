{-# LANGUAGE NamedFieldPuns #-}

-- | Counters of scheduled egress: a snapshot of the node's buckets, taken by
-- the counters loop beside the mux counters.
module Network.Mux.Egress.Counters
  ( SchedulingCounts (..)
  , schedulingCounts
  , egressWaitBounds
  , egressBurstBounds
  , egressBurstWidths
  ) where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Class.MonadTime.SI

import Data.Word (Word64)

import Network.Mux.Egress.Bucket (Bucket, BucketStats (..), bucketSnapshot,
           burstBounds, burstWidths, emptyBucketStats, waitBounds)

-- | Scheduled egress. Counters are cumulative; the level and the queue
-- lengths are values at the snapshot.
data SchedulingCounts = SchedulingCounts {
  scDirectBytes        :: !Word64,     -- ^ the node's own requests, on credit
  scSliceBytes         :: !Word64,     -- ^ the slice, from its own share
  scSliceBorrowedBytes :: !Word64,     -- ^ the slice, on the budget's idle capacity
  scSliceBatches       :: !Word64,
  scSliceWaitTokens    :: !DiffTime,   -- ^ summed over the slice's grants
  scSliceWaitWritable  :: !DiffTime,
  scScheduledBytes     :: !Word64,
  scScheduledBatches   :: !Word64,
  scWaitTokens         :: !DiffTime,   -- ^ summed over the ranked queue's grants
  scWaitWritable       :: !DiffTime,
  scWaitsOver          :: ![(DiffTime, Word64)],
    -- ^ the ranked queue's grants whose token wait exceeded each bound
  scBudgetLevel        :: !Int,        -- ^ bytes; negative while repaying credit
  scBudgetQueued       :: !Int,
  scSliceQueued        :: !Int,
  scBursts             :: !Word64,     -- ^ busy periods of the budget's queue
  scBusyTime           :: !DiffTime,   -- ^ time with a bearer queued on the budget
  scContendedTime      :: !DiffTime,   -- ^ time with two or more, when the order decides
  scBurstsOver         :: ![(DiffTime, Word64)],
    -- ^ busy periods longer than each bound
  scBurstsWider        :: ![(Int, Word64)]
    -- ^ busy periods with at least this many bearers queued at once
  }
  deriving (Eq, Show)

-- | The token-wait bounds 'scWaitsOver' counts against.
egressWaitBounds :: [DiffTime]
egressWaitBounds = waitBounds

-- | The busy-period lengths 'scBurstsOver' counts against.
egressBurstBounds :: [DiffTime]
egressBurstBounds = burstBounds

-- | The queue widths 'scBurstsWider' counts against.
egressBurstWidths :: [Int]
egressBurstWidths = burstWidths

-- | The budget's bucket and the slice's, if any, at an instant.
schedulingCounts :: MonadSTM m => Time -> (Bucket m, Maybe (Bucket m)) -> STM m SchedulingCounts
schedulingCounts now (budget, slice_m) = do
  (b, level, queued) <- bucketSnapshot budget now
  s_m <- traverse (`bucketSnapshot` now) slice_m
  let slice       = maybe emptyStats (\(st, _, _) -> st) s_m
      sliceQueued = maybe 0 (\(_, _, q) -> q) s_m
  return SchedulingCounts {
    scDirectBytes        = bsCredited b,
    scSliceBytes         = bsBytes slice - bsBorrowed slice,
    scSliceBorrowedBytes = bsBorrowed slice,
    scSliceBatches       = bsBatches slice,
    scSliceWaitTokens    = bsWaitTokens slice,
    scSliceWaitWritable  = bsWaitWritable slice,
    scScheduledBytes     = bsBytes b,
    scScheduledBatches   = bsBatches b,
    scWaitTokens         = bsWaitTokens b,
    scWaitWritable       = bsWaitWritable b,
    scWaitsOver          = zip waitBounds (bsWaitsOver b),
    scBudgetLevel        = floor level,
    scBudgetQueued       = queued,
    scSliceQueued        = sliceQueued,
    scBursts             = bsBursts b,
    scBusyTime           = bsBusyTime b,
    scContendedTime      = bsContendedTime b,
    scBurstsOver         = zip burstBounds (bsBurstsOver b),
    scBurstsWider        = zip burstWidths (bsBurstsWider b)
  }
  where
    emptyStats = emptyBucketStats

{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Counters of scheduled egress: a snapshot of the node's buckets, taken by
-- the counters loop beside the mux counters.
module Network.Mux.Egress.Counters
  ( SchedulingCounts (..)
  , TierCounts (..)
  , schedulingCounts
  , egressWaitBounds
  , egressBurstBounds
  , egressBurstWidths
  , egressBurstGap
  , egressBurstCap
  , egressBurstGrantFloor
  , egressServedWindow
  ) where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Class.MonadTime.SI

import Data.Map.Strict qualified as Map
import Data.Word (Word64, Word8)
import GHC.Generics (Generic)

import Network.Mux.Egress.Bucket (Bucket, BucketStats (..), TierGrants (..),
           bucketSnapshot, burstBounds, burstWidths, emptyBucketStats,
           servedByTier, servedWindow, servedWithin, waitBounds)
import Network.Mux.Egress.Burst (burstCap, burstGap, burstGrantFloor)

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
  scBursts             :: !Word64,     -- ^ busy periods of the budget's queue:
                                       --   from a first credited bearer queued to none
  scBusyTime           :: !DiffTime,   -- ^ time with a credited bearer queued on the budget
  scContendedTime      :: !DiffTime,   -- ^ time with two or more, when the order decides
  scBurstsOver         :: ![(DiffTime, Word64)],
    -- ^ busy periods longer than each bound
  scBurstsWider        :: ![(Int, Word64)],
    -- ^ busy periods with at least this many credited bearers queued at once
  scBurstsDropped      :: !Word64,
    -- ^ burst records closed with no room to keep them until the next trace
  scTiers              :: ![(Word8, TierCounts)]
    -- ^ by tier: what the queue granted its bearers, and how many wait now
  }
  deriving (Eq, Show, Generic)

-- | The budget's queue seen from one tier: 0 local roots, 1 partners, 2 pool
-- relays, 3 strangers, 4 the rest, 255 unranked bearers. The fast path is not
-- ranked for the bytes and batches, but its bearers count as served.
data TierCounts = TierCounts {
  tcBytes   :: !Word64,
  tcBatches :: !Word64,
  tcQueued  :: !Int,
  tcServed  :: !Int     -- ^ bearers granted within 'egressServedWindow'
  }
  deriving (Eq, Show, Generic)

-- | The token-wait bounds 'scWaitsOver' counts against.
egressWaitBounds :: [DiffTime]
egressWaitBounds = waitBounds

-- | The busy-period lengths 'scBurstsOver' counts against.
egressBurstBounds :: [DiffTime]
egressBurstBounds = burstBounds

-- | The queue widths 'scBurstsWider' counts against.
egressBurstWidths :: [Int]
egressBurstWidths = burstWidths

-- | The least idle time that ends a burst of credited demand.
egressBurstGap :: DiffTime
egressBurstGap = burstGap

-- | The longest burst record; a longer burst is closed and the next opened.
egressBurstCap :: DiffTime
egressBurstCap = burstCap

-- | The least grant that opens or extends a burst; smaller ones are filler.
egressBurstGrantFloor :: Int
egressBurstGrantFloor = burstGrantFloor

-- | How long a grant keeps its bearer in 'tcServed'.
egressServedWindow :: DiffTime
egressServedWindow = servedWindow

-- | The budget's bucket and the slice's, if any, at an instant.
schedulingCounts :: MonadSTM m => Time -> (Bucket m, Maybe (Bucket m)) -> STM m SchedulingCounts
schedulingCounts now (budget, slice_m) = do
  (b, level, queued, queuedByTier) <- bucketSnapshot budget now
  s_m <- traverse (`bucketSnapshot` now) slice_m
  let slice       = maybe emptyStats (\(st, _, _, _) -> st) s_m
      sliceQueued = maybe 0 (\(_, _, q, _) -> q) s_m
      tiers       = Map.toList $ Map.unionsWith plus
                      [ (\(TierGrants n k) -> TierCounts n k 0 0) <$> bsTiers b
                      , (\q -> TierCounts 0 0 q 0) <$> queuedByTier
                      , TierCounts 0 0 0 <$> servedByTier (servedWithin servedWindow now (bsServed b)) ]
      plus (TierCounts n k q s) (TierCounts n' k' q' s') =
        TierCounts (n + n') (k + k') (q + q') (s + s')
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
    scBurstsWider        = zip burstWidths (bsBurstsWider b),
    scBurstsDropped      = bsBurstsDropped b,
    scTiers              = tiers
  }
  where
    emptyStats = emptyBucketStats

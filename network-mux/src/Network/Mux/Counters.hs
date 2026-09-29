{-# LANGUAGE NamedFieldPuns #-}

-- | Node-wide mux counters: how muxes on one side, node-to-node or
-- node-to-client, have failed, counted once per mux as it dies, and traced
-- periodically.
--
module Network.Mux.Counters
  ( MuxCounters
  , newMuxCounters
  , EgressCounts (..)
  , SchedulingCounts (..)
  , IngressCounts (..)
  , CountersTrace (..)
  , countMuxerFailure
  , countDemuxerFailure
  , countGateTimeout
  , readMuxCounters
  , countersLoop
  , countersInterval
  , egressWaitBounds
  , egressBurstBounds
  , egressBurstWidths
  ) where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Exception (SomeException, fromException)
import Control.Monad (forever)
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import Control.Tracer (Tracer, traceWith)

import Data.Word (Word64)

import Network.Mux.Egress.Bucket (BucketStats (..), Bucket, bucketSnapshot, burstBounds,
           burstWidths, emptyBucketStats, waitBounds)
import Network.Mux.Trace (Error (..))

-- | The write side.
data EgressCounts = EgressCounts {
  ecWriteTimeouts     :: !Word64,  -- ^ peers that took nothing within the SDU timeout
  ecWriteTimeoutsGate :: !Word64,  -- ^ of those, at the writability gate of
                                   --   scheduled egress, before the write
  ecScheduling        :: !(Maybe SchedulingCounts)
    -- ^ scheduled egress, in a snapshot of node-to-node counters when egress
    -- is scheduled
  }
  deriving (Eq, Show)

-- | Scheduled egress. Counters are cumulative; the level and the queue
-- lengths are values at the snapshot.
data SchedulingCounts = SchedulingCounts {
  scDirectBytes        :: !Word64,     -- ^ the node's own requests, on credit
  scSliceBytes         :: !Word64,     -- ^ the slice, from its own share
  scSliceBorrowedBytes :: !Word64,     -- ^ the slice, on the budget's idle capacity
  scScheduledBytes     :: !Word64,
  scScheduledBatches   :: !Word64,
  scWaitTokens         :: !DiffTime,   -- ^ scheduled and slice grants together
  scWaitWritable       :: !DiffTime,
  scWaitsOver          :: ![(DiffTime, Word64)],
    -- ^ grants whose token wait exceeded each bound
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

-- | The read side.
data IngressCounts = IngressCounts {
  icReadTimeouts   :: !Word64,    -- ^ an SDU header arrived, the rest did not in time
  icOverruns       :: !Word64,    -- ^ a peer exceeded a mini-protocol's ingress limit
  icProtocolErrors :: !Word64,    -- ^ undecodable SDUs, unknown mini-protocols,
                                  --   data an initiator-only mux cannot take
  icBearerClosed   :: !Word64     -- ^ the peer closed the connection
  }
  deriving (Eq, Show)

-- | Counters shared by every mux on one side.
newtype MuxCounters m = MuxCounters (StrictTVar m (Sides EgressCounts IngressCounts))

-- | The write side's count and the read side's, each evaluated as it is
-- stored, so an update never leaves the previous count behind a thunk.
data Sides a b = Sides !a !b
  deriving (Eq, Show)

newMuxCounters :: MonadSTM m => m (MuxCounters m)
newMuxCounters = MuxCounters <$> newTVarIO (Sides (EgressCounts 0 0 Nothing) (IngressCounts 0 0 0 0))

readMuxCounters :: MonadSTM m => MuxCounters m -> STM m (EgressCounts, IngressCounts)
readMuxCounters (MuxCounters v) = (\(Sides eg ing) -> (eg, ing)) <$> readTVar v

-- | Count the exception a mux's muxer died with.
countMuxerFailure :: MonadSTM m => MuxCounters m -> SomeException -> STM m ()
countMuxerFailure (MuxCounters v) e =
  case fromException e of
       Just SDUWriteTimeout ->
         modifyTVar v (\(Sides eg ing) -> Sides eg { ecWriteTimeouts = ecWriteTimeouts eg + 1 } ing)
       _ -> return ()

-- | Count a writability gate that timed out; the write timeout itself is
-- counted by 'countMuxerFailure' when the mux dies of it.
countGateTimeout :: MonadSTM m => MuxCounters m -> STM m ()
countGateTimeout (MuxCounters v) =
  modifyTVar v (\(Sides eg ing) -> Sides eg { ecWriteTimeoutsGate = ecWriteTimeoutsGate eg + 1 } ing)

-- | Count the exception a mux's demuxer died with.
countDemuxerFailure :: MonadSTM m => MuxCounters m -> SomeException -> STM m ()
countDemuxerFailure (MuxCounters v) e =
  case fromException e of
       Just SDUReadTimeout          -> bump (\c -> c { icReadTimeouts   = icReadTimeouts c + 1 })
       Just IngressQueueOverRun {}  -> bump (\c -> c { icOverruns       = icOverruns c + 1 })
       Just UnknownMiniProtocol {}  -> bump (\c -> c { icProtocolErrors = icProtocolErrors c + 1 })
       Just InitiatorOnly {}        -> bump (\c -> c { icProtocolErrors = icProtocolErrors c + 1 })
       Just SDUDecodeError {}       -> bump (\c -> c { icProtocolErrors = icProtocolErrors c + 1 })
       Just BearerClosed {}         -> bump (\c -> c { icBearerClosed   = icBearerClosed c + 1 })
       _                            -> return ()
  where
    bump f = modifyTVar v (\(Sides eg ing) -> Sides eg (f ing))

-- | A snapshot of one side's counters.
data CountersTrace =
    TraceRemoteEgress  EgressCounts
  | TraceRemoteIngress IngressCounts
  | TraceLocalEgress   EgressCounts
  | TraceLocalIngress  IngressCounts
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

-- | A prime, so that snapshots do not keep step with other periodic work such
-- as keep-alive, and below a 10 s scrape.
countersInterval :: DiffTime
countersInterval = 7

-- | Every @interval@, snapshot the node-to-node and the node-to-client
-- counters and trace both sides of each; with scheduled egress, the budget's
-- bucket and the slice's, if any, go into the node-to-node egress snapshot.
countersLoop :: (MonadDelay m, MonadSTM m)
             => MuxCounters m    -- ^ node-to-node
             -> MuxCounters m    -- ^ node-to-client
             -> Maybe (Bucket m, Maybe (Bucket m))
             -> DiffTime
             -> Tracer m CountersTrace
             -> m void
countersLoop remote local buckets interval tracer = forever $ do
  threadDelay interval
  now <- getMonotonicTime
  ((re, ri), (le, li), sc) <- atomically $
    (,,) <$> readMuxCounters remote
         <*> readMuxCounters local
         <*> traverse (schedulingCounts now) buckets
  traceWith tracer (TraceRemoteEgress re { ecScheduling = sc })
  traceWith tracer (TraceRemoteIngress ri)
  traceWith tracer (TraceLocalEgress le)
  traceWith tracer (TraceLocalIngress li)

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
    scScheduledBytes     = bsBytes b,
    scScheduledBatches   = bsBatches b,
    scWaitTokens         = bsWaitTokens b + bsWaitTokens slice,
    scWaitWritable       = bsWaitWritable b + bsWaitWritable slice,
    scWaitsOver          = zip waitBounds (zipWith (+) (bsWaitsOver b) (bsWaitsOver slice)),
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

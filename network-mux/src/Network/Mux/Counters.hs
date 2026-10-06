{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes     #-}

-- | Node-wide mux counters: how muxes on one side, node-to-node or
-- node-to-client, have failed, counted once per mux as it dies, and traced
-- periodically.
--
module Network.Mux.Counters
  ( MuxCounters
  , newMuxCounters
  , EgressCounts (..)
  , SchedulingCounts (..)
  , TierCounts (..)
  , IngressCounts (..)
  , ByteCounts
  , ByteSlots
  , MuxByteCells (..)
  , withByteCells
  , CountersTrace (..)
  , countMuxerFailure
  , countDemuxerFailure
  , countGateTimeout
  , readMuxCounters
  , snapshotMuxCountersWith
  , countersLoop
  , schedulingCounts
  , schedulingCountsWith
  , countersInterval
  , egressWaitBounds
  , egressBurstBounds
  , egressBurstWidths
  ) where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Exception (SomeException, fromException)
import Control.Monad (forever)
import Control.Monad.Class.MonadThrow (MonadMask, bracket)
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import Control.Tracer (Tracer, traceWith)

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Merge.Strict qualified as Map
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Word (Word64, Word8)

import Network.Mux.Egress.Bucket (Bucket, BucketStats (..), TierGrants (..),
           bucketSnapshot, burstBounds, burstWidths, emptyBucketStats,
           waitBounds)
import Network.Mux.Trace (Error (..))
import Network.Mux.Types (MiniProtocolDir, MiniProtocolNum)

-- | The write side.
data EgressCounts = EgressCounts {
  ecWriteTimeouts     :: !Word64,  -- ^ peers that took nothing within the SDU timeout
  ecWriteTimeoutsGate :: !Word64,  -- ^ of those, at the writability gate of
                                   --   scheduled egress, before the write
  ecScheduling        :: !(Maybe SchedulingCounts),
    -- ^ scheduled egress, in a snapshot of node-to-node counters when egress
    -- is scheduled
  ecBytes             :: !ByteCounts
    -- ^ bytes written, by mini-protocol and direction
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
  scBurstsWider        :: ![(Int, Word64)],
    -- ^ busy periods with at least this many bearers queued at once
  scTiers              :: ![(Word8, TierCounts)]
    -- ^ by tier: what the queue granted its bearers, and how many wait now
  }
  deriving (Eq, Show)

-- | The budget's queue seen from one tier: 0 local roots, 1 partners, 2 the
-- rest, 255 unranked bearers. The fast path is not ranked and counts nowhere.
data TierCounts = TierCounts {
  tcBytes   :: !Word64,
  tcBatches :: !Word64,
  tcQueued  :: !Int
  }
  deriving (Eq, Show)

-- | The read side.
data IngressCounts = IngressCounts {
  icReadTimeouts   :: !Word64,    -- ^ an SDU header arrived, the rest did not in time
  icOverruns       :: !Word64,    -- ^ a peer exceeded a mini-protocol's ingress limit
  icProtocolErrors :: !Word64,    -- ^ undecodable SDUs, unknown mini-protocols,
                                  --   data an initiator-only mux cannot take
  icBearerClosed   :: !Word64,    -- ^ the peer closed the connection
  icBytes          :: !ByteCounts -- ^ bytes read, by mini-protocol and direction
  }
  deriving (Eq, Show)

-- | Bytes on the wire, SDU headers included, by mini-protocol number and the
-- direction of our side of it: what our initiators and our responders sent,
-- or what was read for them.
type ByteCounts = Map (MiniProtocolNum, MiniProtocolDir) Word64

-- | Read-byte counters, one per mini-protocol and direction a mux runs; the
-- demuxer makes them and keeps each in its dispatch table entry.
type ByteSlots m = Map (MiniProtocolNum, MiniProtocolDir) (StrictTVar m Word64)

-- | One mux's byte counts, written only by its own muxers and demuxer, so no
-- two connections share a variable on the data path: what was sent, once per
-- batch, and the read counters the demuxer publishes as it starts.
data MuxByteCells m = MuxByteCells {
  mbcSent :: !(StrictTVar m ByteCounts),
  mbcRecv :: !(StrictTVar m (ByteSlots m))
  }

-- | The counters as counts, leaving out the ones still at zero, as the sent
-- side does: a protocol appears once it has carried something.
readSlots :: MonadSTM m => StrictTVar m (ByteSlots m) -> STM m ByteCounts
readSlots cell = Map.filter (/= 0) <$> (readTVar cell >>= traverse readTVar)

-- | Counters shared by every mux on one side: failures, counted as a mux
-- dies, and the byte cells of the live muxes beside the totals of the ones
-- that have ended, so a total never goes down when a connection closes.
data MuxCounters m = MuxCounters {
  mcFailures :: !(StrictTVar m (Sides EgressCounts IngressCounts)),
  mcLive     :: !(StrictTVar m (IntMap (MuxByteCells m))),
  mcNext     :: !(StrictTVar m Int),
  mcRetired  :: !(StrictTVar m (Sides ByteCounts ByteCounts))
  }

-- | The write side's count and the read side's, each evaluated as it is
-- stored, so an update never leaves the previous count behind a thunk.
data Sides a b = Sides !a !b
  deriving (Eq, Show)

newMuxCounters :: MonadSTM m => m (MuxCounters m)
newMuxCounters =
  MuxCounters <$> newTVarIO (Sides (EgressCounts 0 0 Nothing Map.empty) (IngressCounts 0 0 0 0 Map.empty))
              <*> newTVarIO IntMap.empty
              <*> newTVarIO 0
              <*> newTVarIO (Sides Map.empty Map.empty)

-- | The failure counts with the bytes of every mux, ended or live, summed in,
-- in one transaction.
readMuxCounters :: MonadSTM m => MuxCounters m -> STM m (EgressCounts, IngressCounts)
readMuxCounters = snapshotMuxCountersWith id

-- | The same, with each read run by @runTx@: in a transaction for the totals
-- and one per live mux, so no transaction spans what every muxer and demuxer
-- writes. The totals and the live set go together, before the cells: a mux
-- that ends in between is in the live set and not yet in the totals, and its
-- cell, which ending leaves as it was, counts it once; a sum never goes down
-- across snapshots.
snapshotMuxCountersWith :: (MonadSTM m, Monad n)
                        => (forall a. STM m a -> n a)
                        -> MuxCounters m -> n (EgressCounts, IngressCounts)
snapshotMuxCountersWith runTx counters = do
  (totals, live) <- runTx (readMuxTotals counters)
  sumMuxCounters totals <$> mapM (runTx . readMuxCells) live

-- | The failure counts, the totals of the muxes that have ended, and the
-- cells of the ones alive.
readMuxTotals :: MonadSTM m => MuxCounters m
              -> STM m ((Sides EgressCounts IngressCounts, Sides ByteCounts ByteCounts), [MuxByteCells m])
readMuxTotals MuxCounters { mcFailures, mcLive, mcRetired } =
  (,) <$> ((,) <$> readTVar mcFailures <*> readTVar mcRetired)
      <*> (IntMap.elems <$> readTVar mcLive)

-- | One mux's bytes sent and read.
readMuxCells :: MonadSTM m => MuxByteCells m -> STM m (ByteCounts, ByteCounts)
readMuxCells MuxByteCells { mbcSent, mbcRecv } = (,) <$> readTVar mbcSent <*> readSlots mbcRecv

sumMuxCounters :: (Sides EgressCounts IngressCounts, Sides ByteCounts ByteCounts)
               -> [(ByteCounts, ByteCounts)] -> (EgressCounts, IngressCounts)
sumMuxCounters (Sides eg ing, Sides sentDone recvDone) cells =
  ( eg  { ecBytes = Map.unionsWith (+) (sentDone : map fst cells) }
  , ing { icBytes = Map.unionsWith (+) (recvDone : map snd cells) } )

-- | Byte cells for one mux for the duration of @k@: registered as it starts,
-- and on the way out their counts move into the retired totals in the same
-- transaction that unregisters them. Without counters there are no cells.
withByteCells :: (MonadSTM m, MonadMask m)
              => Maybe (MuxCounters m) -> (Maybe (MuxByteCells m) -> m a) -> m a
withByteCells Nothing k = k Nothing
withByteCells (Just MuxCounters { mcLive, mcNext, mcRetired }) k =
    bracket register retire (k . Just . snd)
  where
    register = atomically $ do
      i     <- stateTVar mcNext (\n -> (n, n + 1))
      cells <- MuxByteCells <$> newTVar Map.empty <*> newTVar Map.empty
      modifyTVar mcLive (IntMap.insert i cells)
      return (i, cells)
    retire (i, MuxByteCells { mbcSent, mbcRecv }) = atomically $ do
      sent <- readTVar mbcSent
      recv <- readSlots mbcRecv
      modifyTVar mcRetired (\(Sides s r) -> Sides (Map.unionWith (+) s sent) (Map.unionWith (+) r recv))
      modifyTVar mcLive (IntMap.delete i)

-- | Count the exception a mux's muxer died with.
countMuxerFailure :: MonadSTM m => MuxCounters m -> SomeException -> STM m ()
countMuxerFailure MuxCounters { mcFailures = v } e =
  case fromException e of
       Just SDUWriteTimeout ->
         modifyTVar v (\(Sides eg ing) -> Sides eg { ecWriteTimeouts = ecWriteTimeouts eg + 1 } ing)
       _ -> return ()

-- | Count a writability gate that timed out; the write timeout itself is
-- counted by 'countMuxerFailure' when the mux dies of it.
countGateTimeout :: MonadSTM m => MuxCounters m -> STM m ()
countGateTimeout MuxCounters { mcFailures = v } =
  modifyTVar v (\(Sides eg ing) -> Sides eg { ecWriteTimeoutsGate = ecWriteTimeoutsGate eg + 1 } ing)

-- | Count the exception a mux's demuxer died with.
countDemuxerFailure :: MonadSTM m => MuxCounters m -> SomeException -> STM m ()
countDemuxerFailure MuxCounters { mcFailures = v } e =
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
  -- small transactions, one per mux and per bucket: the counters need no
  -- consistency across them, and one over everything the muxes write would
  -- be restarted by every batch
  (re, ri) <- snapshotMuxCountersWith atomically remote
  (le, li) <- snapshotMuxCountersWith atomically local
  sc       <- traverse (schedulingCountsWith atomically now) buckets
  traceWith tracer (TraceRemoteEgress re { ecScheduling = sc })
  traceWith tracer (TraceRemoteIngress ri)
  traceWith tracer (TraceLocalEgress le)
  traceWith tracer (TraceLocalIngress li)

-- | The scheduling counters, in one transaction.
schedulingCounts :: MonadSTM m => Time -> (Bucket m, Maybe (Bucket m)) -> STM m SchedulingCounts
schedulingCounts = schedulingCountsWith id

-- | The same, the budget's snapshot and the slice's in a transaction each.
schedulingCountsWith :: (MonadSTM m, Monad n)
                     => (forall a. STM m a -> n a)
                     -> Time -> (Bucket m, Maybe (Bucket m)) -> n SchedulingCounts
schedulingCountsWith runTx now (budget, slice_m) = do
  (b, level, queued, queuedByTier) <- runTx (bucketSnapshot budget now)
  s_m <- traverse (runTx . (`bucketSnapshot` now)) slice_m
  let slice       = maybe emptyStats (\(st, _, _, _) -> st) s_m
      sliceQueued = maybe 0 (\(_, _, q, _) -> q) s_m
      tiers       = Map.toList $ Map.merge
                      (Map.mapMissing (\_ (TierGrants n k) -> TierCounts n k 0))
                      (Map.mapMissing (\_ q -> TierCounts 0 0 q))
                      (Map.zipWithMatched (\_ (TierGrants n k) q -> TierCounts n k q))
                      (bsTiers b) queuedByTier
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
    scBurstsWider        = zip burstWidths (bsBurstsWider b),
    scTiers              = tiers
  }
  where
    emptyStats = emptyBucketStats

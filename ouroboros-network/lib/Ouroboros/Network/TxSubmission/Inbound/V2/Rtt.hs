{-# LANGUAGE DataKinds #-}

-- |
-- Per-peer and global round-trip-time (RTT) sliding windows for the V2
-- tx-submission inbound side.
--
-- Each peer keeps two time-bucketed, t-digest-backed windows: one for txid
-- request/reply round trips and one for tx-body request/reply round trips.
-- The windows are fed by the peer worker thread as replies are collected
-- (see 'recordRttSample'); the central bookkeeping thread merges them across
-- all peers into a global summary (see 'globalRttStats'), and the per-peer
-- windows are available for tuning a peer's request spacing (see
-- 'peerRttStats').
--
-- Window sizing: a 180s window of 5s buckets, t-digest compression 20.
module Ouroboros.Network.TxSubmission.Inbound.V2.Rtt
  ( RttKind (..)
  , PeerRtt
  , emptyPeerRtt
  , recordRttSample
  , peerRttStats
  , globalRttStats
  , txidRttP95
  , interTxSpaceFor
  , expectedRtts
  ) where

import Control.Monad.Class.MonadTime.SI (DiffTime, Time)
import Data.Maybe (fromMaybe)
import NoThunks.Class (NoThunks (..))

import Data.TDigest (TDigest)
import Data.TDigest qualified as TD
import Data.Window.DigestBucketed (BucketedDigestWindow)
import Data.Window.DigestBucketed qualified as DB

import Ouroboros.Network.SizeInBytes (SizeInBytes, getSizeInBytes)
import Ouroboros.Network.TxSubmission.Inbound.V2.Types (RttStats (..))

-- | Which round trip a sample measures.
data RttKind = RttTxId | RttTxBody
  deriving (Eq, Show)

-- | t-digest compression for the RTT windows.  20 is ample for an RTT
-- telemetry/tuning signal: t-digest keeps its finest resolution at the tails
-- (so p95/p99 stay accurate), and per-bucket digests hold far fewer samples
-- than this anyway.
type RttComp = 20

-- | An RTT window: a bucketed time window of millisecond samples backed by
-- per-bucket t-digests.
type RttWindow = BucketedDigestWindow Time RttComp

-- | Width of a single RTT bucket.
rttBucketDuration :: DiffTime
rttBucketDuration = 5

-- | Duration of the RTT sliding window.
rttWindowDuration :: DiffTime
rttWindowDuration = 180

-- | The two RTT windows a peer maintains.
data PeerRtt = PeerRtt {
    rttTxIdWin   :: !RttWindow,
    rttTxBodyWin :: !RttWindow
  }

-- window-stats provides 'NFData' but not 'NoThunks'.  The windows are written
-- strictly (constructor bang fields plus strict-TVar writes), so we treat
-- 'PeerRtt' as opaque to the no-thunks check rather than walking the
-- finger-tree / t-digest internals on every write.
instance NoThunks PeerRtt where
  showTypeOf _  = "PeerRtt"
  wNoThunks _ _ = return Nothing

emptyPeerRtt :: PeerRtt
emptyPeerRtt = PeerRtt emptyWin emptyWin
  where
    emptyWin = DB.empty rttBucketDuration rttWindowDuration

-- | Record one round-trip-time sample for the given kind: the elapsed time
-- between sending the request and collecting its reply, stamped at @now@.
recordRttSample :: RttKind -> Time -> DiffTime -> PeerRtt -> PeerRtt
recordRttSample kind now rtt prtt =
  case kind of
    RttTxId   -> prtt { rttTxIdWin   = DB.insert sample (rttTxIdWin prtt) }
    RttTxBody -> prtt { rttTxBodyWin = DB.insert sample (rttTxBodyWin prtt) }
  where
    sample = (now, diffTimeToMillis rtt)

-- | Per-peer RTT summary (txid, tx-body) over the window ending at @now@.
-- Stale samples (older than the window duration relative to @now@) are
-- evicted first, so an idle peer reports an empty summary.
peerRttStats :: Time -> PeerRtt -> (RttStats, RttStats)
peerRttStats now prtt =
  ( windowStats now (rttTxIdWin prtt)
  , windowStats now (rttTxBodyWin prtt)
  )

-- | Global RTT summary (txid, tx-body) across all peers, over the window
-- ending at @now@.  Each peer's window is trimmed to @now@ before its
-- t-digest is merged, so peers that have gone idle drop out of the merge.
globalRttStats :: Time -> [PeerRtt] -> (RttStats, RttStats)
globalRttStats now prtts =
  ( mergedStats now [ rttTxIdWin p   | p <- prtts ]
  , mergedStats now [ rttTxBodyWin p | p <- prtts ]
  )

-- | The peer's base round-trip-time estimate: the 95th-percentile txid
-- request/reply RTT over the window, or 'Nothing' when the txid window holds
-- fewer than 'rttMinSamples' samples (cold start).
--
-- The txid round-trip (request txids -> reply) is the base because we request
-- txids from every peer continuously, so a network-latency estimate is
-- available even for peers we rarely fetch bodies from, and it is a clean
-- network measure free of body-transfer size.  The transfer cost of the
-- bodies is added on top by 'interTxSpaceFor'.
txidRttP95 :: Time -> PeerRtt -> Maybe DiffTime
txidRttP95 now prtt
  | DB.windowSize now win < rttMinSamples = Nothing
  | otherwise = millisToDiffTime <$> DB.windowQuantile 0.95 now win
  where
    win = rttTxIdWin prtt

-- | RTT-derived exclusive-fetch lease ('interTxSpace') for a body request of
-- the given size: the base RTT scaled by the number of TCP round trips the
-- transfer is expected to need ('expectedRtts'), clamped to @[30ms, cap]@.
-- Falls back to the policy default when no base RTT estimate is available yet.
interTxSpaceFor :: DiffTime        -- ^ fallback (policy 'interTxSpace')
                -> DiffTime        -- ^ cap (policy 'inflightTimeout')
                -> Maybe DiffTime  -- ^ base RTT estimate ('txidRttP95')
                -> SizeInBytes     -- ^ total bytes requested
                -> DiffTime
interTxSpaceFor fallback _   Nothing        _       = fallback
interTxSpaceFor _        cap (Just baseRtt) reqSize =
  max rttLeaseFloor (min cap (fromIntegral (expectedRtts reqSize) * baseRtt))

-- | Number of TCP round trips a transfer of the given size is expected to
-- need, modelling slow-start from an initial congestion window of 'initCwnd'
-- segments: cumulative deliverable bytes after @k@ rounds is
-- @initCwnd * mss * (2^k - 1)@, so @rounds = ceil (log2 (bytes/initWin + 1))@.
expectedRtts :: SizeInBytes -> Int
expectedRtts reqSize =
  max 1 (ceiling (logBase 2 (bytes / initWindowBytes + 1)))
  where
    bytes          = fromIntegral (getSizeInBytes reqSize) :: Double
    initWindowBytes = fromIntegral (initCwnd * mss)        :: Double

-- | Minimum txid-RTT samples before 'txidRttP95' yields an estimate.
rttMinSamples :: Int
rttMinSamples = 10

-- | TCP maximum segment size and initial congestion window assumed by
-- 'expectedRtts'.
mss, initCwnd :: Int
mss      = 1460
initCwnd = 10

-- | Lower bound on the exclusive-fetch lease.
rttLeaseFloor :: DiffTime
rttLeaseFloor = 0.030

--
-- internal
--

millisToDiffTime :: Double -> DiffTime
millisToDiffTime ms = realToFrac (ms / 1000)

diffTimeToMillis :: DiffTime -> Double
diffTimeToMillis d = realToFrac d * 1000

windowStats :: Time -> RttWindow -> RttStats
windowStats now win = digestStats (DB.windowSize now win) (DB.windowMeasure now win)

mergedStats :: Time -> [RttWindow] -> RttStats
mergedStats now ws =
  digestStats (sum (map (DB.windowSize now) ws))
              (mconcat (map (DB.windowMeasure now) ws))

digestStats :: Int -> TDigest RttComp -> RttStats
digestStats n td = RttStats {
    rttCount  = n,
    rttP50Ms  = q 0.5,
    rttP90Ms  = q 0.9,
    rttP95Ms  = q 0.95,
    rttP99Ms  = q 0.99,
    rttMeanMs = fromMaybe 0 (TD.mean td)
  }
  where
    q p = fromMaybe 0 (TD.quantile p td)

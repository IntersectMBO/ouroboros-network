{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE CPP              #-}
{-# LANGUAGE DataKinds        #-}

-- | Cascade-eviction benchmark for "Data.Window.DigestTimeBatched",
-- used to compare lazy vs. strict 'bmDigest' in the finger-tree
-- measure.
--
-- Each case streams a 'cascadeSamples' input into a window with 1-second
-- buckets and 60-bucket retention, then queries the median (forcing the
-- top-level measure through 'windowDigest'). Timings are monotonic from
-- 'GHC.Clock.getMonotonicTime'; allocation, GC count and residency
-- come from 'GHC.Stats' when the RTS is built with @-T@.
--
-- The cases alternate dense stretches with wide idle gaps. The gap
-- puts each next sample's eviction cutoff past many sealed buckets in
-- a single 'FT.dropUntil'; a lazy 'bmDigest' can discard the cached
-- subtree measures for those buckets without forcing their digest
-- combines, while a strict variant has already paid for them at
-- insertion time.
--
-- Measured lazy vs. strict (bmDigest strict + no manual force in
-- evictBefore), @-O2 +RTS -T@, wall / alloc:
--
--   *  10x (6000 @ 0.01, gap 60s) —  60k samples — 1.36x / 1.30x
--   *  20x (6000 @ 0.01, gap 60s) — 120k samples — 1.25x / 1.30x
--   *  50x (3000 @ 0.01, gap 60s) — 150k samples — 1.23x / 1.22x
--   * 100x (1000 @ 0.01, gap 60s) — 100k samples — 1.21x / 1.31x
--
module Main (main) where

import Control.Exception (evaluate)
#if !MIN_VERSION_base(4,20,0)
import Data.Foldable
#endif
import Data.Time (UTCTime (..), addUTCTime, fromGregorian)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTime)
import GHC.Stats (RTSStats (..), getRTSStats, getRTSStatsEnabled)
import System.Mem (performMajorGC)
import Text.Printf (printf)

import Data.Window.DigestTimeBatched qualified as DTB


type Comp = 100
type DigestW = DTB.TimedDigestWindow UTCTime Comp

base :: UTCTime
base = UTCTime (fromGregorian 2024 1 1) 0

mkT :: Double -> UTCTime
mkT s = realToFrac s `addUTCTime` base

-- | Alternating dense/gap stream.
--
-- Each cycle emits @nDense@ samples spaced by @denseStep@ seconds
-- (duration @(nDense - 1) * denseStep@), then a silent @gapDur@ before
-- the next cycle. The first sample of the next cycle triggers a seal
-- AND — if @gapDur@ is a significant fraction of the retention window —
-- moves the eviction cutoff past many buckets at once, causing a
-- cascade drop that lazy 'bmDigest' can avoid combining.
cascadeSamples
    :: Int              -- ^ number of cycles
    -> Int              -- ^ samples per dense stretch
    -> Double           -- ^ time step within a dense stretch (s)
    -> Double           -- ^ gap between stretches (s)
    -> [(UTCTime, Double)]
cascadeSamples cycles nDense denseStep gapDur =
    [ (mkT (fromIntegral k * cycleDur + fromIntegral i * denseStep)
      , fromIntegral (k * nDense + i)
      )
    | k <- [0 .. cycles - 1]
    , i <- [0 .. nDense - 1]
    ]
  where
    stretchDur = fromIntegral (nDense - 1) * denseStep
    cycleDur   = stretchDur + gapDur

-- Statistics captured around a measured action. All GHC.Stats counters
-- are Word64; keep them so on all architectures.
data Snapshot = Snapshot
    { snapAllocated :: !Word64
    , snapMaxLive   :: !Word64
    , snapGC        :: !Word64
    }

zeroSnap :: Snapshot
zeroSnap = Snapshot 0 0 0

takeSnap :: IO Snapshot
takeSnap = do
    ok <- getRTSStatsEnabled
    if ok
      then do
        s <- getRTSStats
        pure Snapshot
          { snapAllocated = allocated_bytes s
          , snapMaxLive   = max_live_bytes s
          , snapGC        = fromIntegral (gcs s)  -- gcs :: Word32
          }
      else pure zeroSnap

benchCase :: String -> IO () -> IO ()
benchCase label action = do
    performMajorGC
    s0 <- takeSnap
    t0 <- getMonotonicTime
    action
    t1 <- getMonotonicTime
    s1 <- takeSnap
    let dt    = t1 - t0
        alloc = fromIntegral (snapAllocated s1 - snapAllocated s0) / (1024 * 1024 :: Double)
        gcs'  = snapGC s1 - snapGC s0
        maxL  = fromIntegral (snapMaxLive s1) / (1024 * 1024 :: Double)
    printf "%-46s  %8.4f s   %8.2f MB alloc   %6d GCs   %6.2f MB max_live\n"
      label dt alloc gcs' maxL

main :: IO ()
main = do
    putStrLn "window-stats: DigestTimeBatched cascade-eviction benchmark"
    putStrLn "(compile with -O2 and pass +RTS -T for allocation/GC stats)"
    putStrLn ""
    printf "%-46s  %10s   %19s   %9s   %14s\n"
      "case" "wall" "alloc" "gcs" "max_live"

    -- Cases use bucketDur = 1.0 s, retention = 60 buckets.
    let cascade cycles nDense denseStep gapDur =
          benchCase
            (printf "cascade: %d× (%d @ %.3fs dense, then %.1fs gap)"
              cycles nDense denseStep gapDur)
            (do
              let !samples = cascadeSamples cycles nDense denseStep gapDur
                  !w0     = DTB.empty 1.0 60 :: DigestW
                  !w      = foldl' (flip DTB.insert) w0 samples
              _ <- evaluate (DTB.windowMedian w)
              pure ())

    -- Full-window cascade: each cycle fills the window then drops it.
    cascade 10 6000 0.01 60.0    -- 60k samples, 10 cascades of 60 buckets
    cascade 20 6000 0.01 60.0    -- 120k samples, 20 cascades of 60 buckets

    -- Half-window cascade: less accumulation, more cascade events.
    cascade 50 3000 0.01 60.0    -- 150k samples, 50 cascades of ~30 buckets

    -- Shallow cascade: small drops per cascade, many events.
    cascade 100 1000 0.01 60.0   -- 100k samples, 100 cascades of ~10 buckets

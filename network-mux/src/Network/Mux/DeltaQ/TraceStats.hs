{-# LANGUAGE BangPatterns #-}

module Network.Mux.DeltaQ.TraceStats
  ( step
  , stepBurst
  , OneWayDeltaQSample (..)
  , constructSample
  , StatsA
  , initialStatsA
  ) where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM

import Control.Monad.Class.MonadTime.SI
import Network.Mux.DeltaQ.TraceStatsSupport
import Network.Mux.DeltaQ.TraceTypes


-- | Per-observation processing step for round-trip observations.
--
-- Historically (pre-cookie-scheme) the first argument was the peer's
-- send timestamp; this module then translated it into the local
-- clock frame via a reference point to compute a one-way transit
-- time. That path was closed off when 'Network.Mux.RTT' replaced the
-- clock-domain timestamp with an opaque cookie.
--
-- The step now accepts the *round-trip* delay directly — computed
-- from the local send/receive times of a cookie/echo pair — and
-- feeds the same 'estimateGS'-style accumulator. The resulting
-- 'OneWayDeltaQSample' remains meaningful, but its fields now
-- characterise round-trip behaviour rather than one-way transit;
-- consumers should re-interpret 'estDeltaQS' etc. as G/S estimates
-- of the round-trip signal.
step :: DiffTime               -- ^ round-trip delay for this observation
     -> Time                   -- ^ local clock time
     -> Int                    -- ^ observed SDU size in octets
     -> StatsA                 -- ^ accumulation state
     -> (Maybe OneWayDeltaQSample, StatsA)
step delay = withSamplePeriod update
  where
    !rtt = S $ realToFrac delay
    update _localTS obsSize s = recordObservation s obsSize rtt


-- | Per-observation processing step for burst-continuation SDUs
-- (see 'IngressEcho'/'EchoBurstSDU' in "Network.Mux.RTT"). The
-- 'DiffTime' argument is the inter-SDU gap — the interval between
-- this SDU and the previous echo of the same cookie — which
-- approximates @S · size@ for the peer's outbound serialisation
-- with essentially no intercept G.
--
-- Feeds a separate through-origin estimator ('estBurstS' on the
-- emitted sample) that shares the sample-period cadence with the
-- RTT-driven G/S regression but doesn't touch its accumulator.
stepBurst
    :: DiffTime                -- ^ inter-SDU gap
    -> Time                    -- ^ local clock time
    -> Int                     -- ^ observed SDU size in octets
    -> StatsA
    -> (Maybe OneWayDeltaQSample, StatsA)
stepBurst gap = withSamplePeriod update
  where
    !gapS = S $ realToFrac gap
    update _localTS obsSize s =
      s { burstSumSize = burstSumSize s + obsSize
        , burstSumGap  = burstSumGap  s + gapS
        , burstCount   = succ (burstCount s)
        }


-- | Common sample-period plumbing shared by 'step' and 'stepBurst'.
--
-- Handles the "first observation of a period", "within period" and
-- "just past the period boundary" cases; delegates the
-- observation-kind-specific bookkeeping to the caller-supplied
-- 'update' function.
withSamplePeriod
    :: (Time -> Int -> StatsA -> StatsA)   -- ^ observation-specific update
    -> Time                                -- ^ observation time
    -> Int                                 -- ^ observation size
    -> StatsA
    -> (Maybe OneWayDeltaQSample, StatsA)
withSamplePeriod update localTS obsSize s =
  case referenceTimePoint s of
    Nothing ->
      -- first observation of a sample period
      ( Nothing
      , update localTS obsSize $
          s { referenceTimePoint = Just $! localTS
            , nextSampleAt       = sampleInterval `addTime` localTS
            , timeLastObs        = localTS
            }
      )
    Just _ | localTS <= nextSampleAt s ->
      -- within the current period
      (Nothing, update localTS obsSize s { timeLastObs = localTS })
    _ ->
      -- past the current period: emit the built-up sample, restart.
      let !sample = constructSample s
          !fresh  = initialStatsA { referenceTimePoint = Just $! localTS
                                  , nextSampleAt       = sampleInterval `addTime` localTS
                                  , timeLastObs        = localTS
                                  }
      in  (Just sample, update localTS obsSize fresh)


recordObservation :: StatsA -> Int -> SISec -> StatsA
recordObservation s obsSize rtt
  = let f Nothing  = Just $! makePerSizeRecord rtt
        f (Just a) = Just $! makePerSizeRecord rtt <> a
    in s { numObservations = succ (numObservations s)
         , observables     = IM.alter f obsSize (observables s)
         }


-- | Consume the accumulator and produce a 'OneWayDeltaQSample'.
constructSample :: StatsA -> OneWayDeltaQSample
constructSample sa = OneWaySample
  { duration       = realToFrac $
                     maybe 0 (timeLastObs sa `diffTime`) (referenceTimePoint sa)
  , sumPackets     = population
  , sumTotalSDU    = totalSDUOctets
  , estDeltaQS     = normCheck dQSEst
  , estBurstS      = burstS
  , estDeltaQVMean = normCheck $ vSum / pop
  , estDeltaQVVar  = normCheck $ (vSum2 - vSum * vSum / pop) / pop
  , estR           = normCheck rEst
  , sizeDist       = show [ (a,count b, let S mtt = minTransitTime b in mtt)
                          | (a, b) <- IM.toAscList (observables sa)
                          , count b > 0]
  }
  where
    -- the sample population size
    population = numObservations sa
    pop        = fromIntegral population
    -- Handle the empty population condition
    normCheck x
      | IM.size (observables sa) > 1 = x
      | otherwise                    = nan
    -- gather the data for the G,S estimations
    (totalSDUOctets, minSRev)
      = IM.foldrWithKey accum (0, []) $ observables sa

    accum :: Int -> PerSizeRecord
          -> (Int, [(Int, SISec)])
          -> (Int, [(Int, SISec)])
    accum nOctets psr (sumSize, minS)
      = ( sumSize + count psr * nOctets
        , (nOctets, minTransitTime psr) : minS)

    -- fit a line to get the G,S estimation
    (dQGEst, dQSEst, rEst) = estimateGS minSRev

    -- normalise all the observations
    normalisedObservations :: IntMap PerSizeRecord
    normalisedObservations
      = let norm n = S . fromRational . toRational
                     $ dQGEst + fromIntegral n * dQSEst
        in IM.mapWithKey (\k -> normalisePSR (norm k)) (observables sa)

    -- calculate the total population V stats
    vSum   :: Double
    vSum2  :: Double
    vSum'  :: SISec
    vSum2' :: SISec2

    (vSum, vSum2)
      = let v, v2 :: Float
            S  v  = vSum'
            S2 v2 = vSum2'
        in (fromRational . toRational $ v, fromRational . toRational $ v2)
    (vSum', vSum2')
      = IM.foldr vCalc (0,0) normalisedObservations

    vCalc :: PerSizeRecord
          -> (SISec, SISec2)
          -> (SISec, SISec2)
    vCalc psr (x, x2)
      = (x + sumTransitTime psr, x2 + sumTransitTimeSq psr)

    -- Burst-derived per-byte serialisation slope: Σ gap / Σ size.
    -- Through-origin (no G intercept): burst-SDU gaps model the
    -- peer's inter-SDU serialisation cost, not RTT. NaN when no
    -- burst observations landed in this period.
    burstS
      | burstCount sa == 0 || burstSumSize sa == 0 = nan
      | otherwise =
          let S g = burstSumGap sa
          in  fromRational (toRational g)
              / fromIntegral (burstSumSize sa)


-- | Round-trip characterisation over a sample interval. Field names
-- other than 'estBurstS' are preserved for downstream compatibility;
-- their semantics shifted from "one-way transit" to "round-trip"
-- with the cookie migration (see track.md).
data OneWayDeltaQSample = OneWaySample
  { duration       :: Double -- SI Seconds of activity captured
  , sumPackets     :: Int
  , sumTotalSDU    :: Int
  , estDeltaQS     :: Double -- SI Seconds per byte of the peer's response,
                             -- derived from the (roundTrip ~ size) regression.
  , estBurstS      :: Double -- SI Seconds per byte, derived from burst-SDU
                             -- inter-SDU gaps only — a through-origin
                             -- estimator of peer-side serialisation cost
                             -- with no G intercept. NaN when the period saw
                             -- no burst continuations.
  , estDeltaQVMean :: Double -- SI Seconds
  , estDeltaQVVar  :: Double
  , estR           :: Double -- R estimate
  , sizeDist       :: String -- temporary to show size distribution
  }


data StatsA = StatsA
  { -- per sample
    referenceTimePoint :: !(Maybe Time)
  , nextSampleAt       :: !Time
    -- per observation
  , numObservations    :: !Int
  , timeLastObs        :: !Time
  , observables        :: !(IntMap PerSizeRecord)
    -- burst-SDU accumulators (see 'stepBurst')
  , burstSumSize       :: !Int
  , burstSumGap        :: !SISec
  , burstCount         :: !Int
  }


data PerSizeRecord = PSR
  { minTransitTime   :: !SISec
  , count            :: !Int
  , sumTransitTime   :: !SISec
  , sumTransitTimeSq :: !SISec2
  }

instance Semigroup PerSizeRecord where
  a <> b = PSR { minTransitTime   = minTransitTime a `min` minTransitTime b
               , count            = count a + count b
               , sumTransitTime   = sumTransitTime a + sumTransitTime b
               , sumTransitTimeSq = sumTransitTimeSq a + sumTransitTimeSq b
               }

-- | Normalise given the calculated G,S for the size
normalisePSR :: SISec -> PerSizeRecord -> PerSizeRecord
normalisePSR norm psr
  = let adj  = fromIntegral (count psr) * norm
        stt' = sumTransitTime psr - adj
        ttt (S a) (S b)
             = S2 $ a * b
    in psr { minTransitTime   = minTransitTime   psr - norm
           , sumTransitTime   = stt'
           , sumTransitTimeSq = sumTransitTimeSq psr
                                - norm `ttt` (2 * stt' + norm)
           }


initialStatsA :: StatsA
initialStatsA = StatsA
  { referenceTimePoint = Nothing
  , nextSampleAt       = noTime
  , numObservations    = 0
  , timeLastObs        = noTime
  , observables        = IM.empty
  , burstSumSize       = 0
  , burstSumGap        = 0
  , burstCount         = 0
  }
  where
    noTime = Time 0

makePerSizeRecord :: SISec -> PerSizeRecord
makePerSizeRecord tt = PSR
  { minTransitTime   = tt
  , count            = 1
  , sumTransitTime   = tt
  , sumTransitTimeSq = squareSISec tt
  }


-- | Sample interval; a 'OneWayDeltaQSample' fires no more often than
-- this. Matches the pre-migration cadence.
sampleInterval :: DiffTime
sampleInterval = 10

nan :: Double
nan = 0/0

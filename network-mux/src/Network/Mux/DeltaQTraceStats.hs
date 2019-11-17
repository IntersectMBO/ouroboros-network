module Network.Mux.DeltaQTraceStats
 ( step
 , OneWayDeltaQSample(..)
 , constructSample
 , StatsA
 , initialStatsA
 )
where

import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import           Data.Maybe
import           Data.Word (Word32)
import           Numeric.IEEE (nan)


import           Control.Monad.Class.MonadTime
import           Network.Mux.DeltaQTraceStatsSupport
import           Network.Mux.DeltaQTraceTypes
import           Network.Mux.Types

-- the per observation procesing step
step :: StatsA                  -- ^ accumulation state
     -> RemoteClockModel        -- ^ Remote clock timestamp
     -> Time                    -- ^ Local clock timestamp
     -> Int                     -- ^ the number of octets in the
                                --   observed outcome
     -> (Maybe OneWayDeltaQSample, StatsA)
step s remoteTS localTS obsSize
 | isNothing (referenceTimePoint s) -- first observation this sample period
   = step (s { referenceTimePoint = Just $! (unRemoteClockModel remoteTS, localTS)
             , nextSampleAt       = sampleInterval `addTime` localTS
             , timeLastObs        = localTS -- for single observation in sample case
             })
         remoteTS localTS obsSize
 | localTS <= nextSampleAt s    -- still in a sample period
  = let Just refTimePoint = referenceTimePoint s
        transitTime       = calcTransitTime refTimePoint remoteTS localTS
    in (Nothing, recordObservation s localTS obsSize transitTime)
 | otherwise                    -- need to start next sample period
  = let sample  = constructSample s
        (_, s') = step initialStatsA remoteTS localTS obsSize
    in (Just sample, s')

-- Calculate the transit time by transforming the remotely reported
-- emit time into local clock domain then calculating differences.
calcTransitTime :: (Word32, Time)
                -> RemoteClockModel
                -> Time
                -> SISec
calcTransitTime (remoteRefTS, localRefTS) remoteTS' localTS
  = let remoteTS
          = unRemoteClockModel remoteTS'
        remoteClockDiffAsTimeDiff
          = (remoteClockPrecision *) . fromRational . fromIntegral
        correctedEmitTime
          | remoteTS >= remoteRefTS
          = (remoteClockDiffAsTimeDiff $ remoteTS - remoteRefTS)
            `addTime` localRefTS
          | otherwise -- wrap has occurred
          = (remoteClockDiffAsTimeDiff $ maxBound - (remoteRefTS - remoteTS))
            `addTime` localRefTS
    in S $! fromRational (toRational (localTS `diffTime` correctedEmitTime))

recordObservation :: StatsA -> Time -> Int -> SISec -> StatsA
recordObservation s obsTime obsSize transitTime
  = let f Nothing  = Just $! makePerSizeRecord transitTime
        f (Just a) = Just $! makePerSizeRecord transitTime <> a
    in s { timeLastObs     = obsTime
         , numObservations = succ (numObservations s)
         , observables     = IM.alter f obsSize (observables s)
         }

-- This might benefit from some strictness analysis, what are the
-- likely usage patterns?, do we want a single use collapse the
-- collective set of thunks or not?
--
-- There is the issue of "bias" (in its statistical meaning) here. The
-- approach here is pragmatic, we are going to use the uncorrected
-- sample standard deviation here as it has a defined solution for a
-- single sample.
--
-- Given that the consumer of this statistic also has access to the
-- population size, they could reconstruct the underlying measures and
-- take it from there.
--
-- We return `NaN` for the appropraiate satistics when the population
-- is empty.
constructSample :: StatsA -> OneWayDeltaQSample
constructSample sa = OneWaySample
  { sumPackets     = population
  , sumTotalSDU    = totalSDUOctets
  , estDeltaQS     = popCheck dQSEst
  , estDeltaQVMean = popCheck
                     $ vSum / (fromIntegral population)
  , estDeltaQVStd  = popCheck . sqrt
                     $ (vSum2 - (vSum * vSum)) / (fromIntegral population)
  }
  where
    -- the sample population size
    population = numObservations sa
    -- Handle the empty population condition
    popCheck x
      | population > 0 = x
      | otherwise      = nan
    -- gather the data for the G,S estimations
    (totalSDUOctets, minSRev)
      = IM.foldrWithKey accum (0, []) $ observables sa
    accum nOctets psr (sumSize, minS)
      = ( sumSize + (count psr) * nOctets
        , (nOctets, minTransitTime psr) : minS)
    -- fit a line to get the G,S estimation
    (dQGEst, dQSEst, _REst) = estimateGS minSRev
    -- calculate the total population V stats
    (vSum, vSum2)
      = let S  v  = vSum'
            S2 v2 = vSum2'
        in (fromRational . toRational $ v, fromRational . toRational $ v2)
    (vSum', vSum2')
      = IM.foldrWithKey vCalc (0,0) $ observables sa
    vCalc nOctets psr' (x, x2)
      = let norm = S . fromRational . toRational
                   $ dQGEst + (fromIntegral nOctets) * dQSEst
            psr  = normalisePSR norm psr'
        in (x + sumTransitTime psr, x2 + sumTransitTimeSq psr)

-- | One way measurement for interval. Note that the fields are lazy
--   here so that only calcuation necessary to satisfy strictness of
--   use occurs.
data OneWayDeltaQSample = OneWaySample
  { sumPackets     :: Int
  , sumTotalSDU    :: Int
  , estDeltaQS     :: Double -- octets per second
  , estDeltaQVMean :: Double -- SI Seconds
  , estDeltaQVStd  :: Double
  }

-- | Statistics accumulator. Strict evaluation used to keep the memory
--   footprint strictly bounded.
data StatsA = StatsA
  { -- per sample
    referenceTimePoint :: !(Maybe (Word32, Time))
  , nextSampleAt       :: !Time
    -- per observation
  , numObservations    :: !Int
  , timeLastObs        :: !Time
  , observables        :: !(IntMap PerSizeRecord)
  }

-- This _may_ not be the best representation, but it does appear to be
-- an adequate one. There are known issues with numerical stabilty for
-- this representation approach in floating point arithmetic where the
-- values being measured are "large" and the variablity in the sampled
-- population is "small" (i.e the inherent rounding effect of floating
-- point arithmetic has an effect).
--
-- This is very unlikely to cause an issue here as:
--
--   a) the modulo model of the RemoteClockModel (and hence the
--   establishment of a clock reference offset for a given sample)
--   means that we are only ever recording differences - thus any
--   absolute clock differences get factored out.
--
--   b) the transit delay for a measurement will be small, (probablly
--   not even credible) worst case ~10^3 / 10^4 seconds, the finite
--   mantissa of IEEE754
--   (https://en.wikipedia.org/wiki/IEEE_754#Representation_and_encoding_in_memory)
--   even for 32 bit representation (24bits / 7.22 decimal digits)
--   represents an bound on the inherent measured population
--   variability.
--
--   c) longer term clock drift is covered here by the restablishing
--   of the clock reference offsets every sampling period. Given a
--   reasonable sampling period (say 10 seconds) clock drift ( <
--   100ppm) can't amount to a significant error over such a period.
--
-- To conclude, reasonable model of delay is < 1second, the precision
-- of delay measurement is 10^-6 - this all fits nicely withing a
-- IEEE754 32bit representation with its 7.22 decimal digit
-- mantissa. Haskell `Float`s are adequate for this purpose.

data PerSizeRecord = PSR
  { minTransitTime   :: !SISec
  , count            :: !Int
  , sumTransitTime   :: !SISec
  , sumTransitTimeSq :: !SISec2
  }

instance Semigroup PerSizeRecord where
  a <> b = PSR { minTransitTime   = (minTransitTime a) `min` (minTransitTime b)
               , count            = count a + count b
               , sumTransitTime   = sumTransitTime a + sumTransitTime b
               , sumTransitTimeSq = sumTransitTimeSq a + sumTransitTimeSq b
               }

-- | Normalise given the calculated G,S for the size
normalisePSR :: SISec -> PerSizeRecord -> PerSizeRecord
normalisePSR norm psr
  = psr { minTransitTime   = minTransitTime   psr + norm
        , sumTransitTime   = sumTransitTime   psr + adj
        , sumTransitTimeSq = sumTransitTimeSq psr + squareSISec adj
        }
    where
      adj = (fromIntegral (count psr) * norm)

-- | Initial StatsA
initialStatsA :: StatsA
initialStatsA = StatsA
  { referenceTimePoint = Nothing
  , nextSampleAt       = noTime
  , numObservations    = 0
  , timeLastObs        = noTime
  , observables        = IM.empty
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

-- May want to make this a configuration variable

-- NOTE this interval must be less than the wrap around time of the
-- `RemoteClockModel`. The remote clock model has a precision of
-- `remoteClockPrecision`.
sampleInterval :: DiffTime
sampleInterval = check 10
  where
    check n
     | n > 0 && n < wrapInterval
       = n
     | otherwise
       = error "Infeasible sampleInterval"
    ticksPerRemoteClockWrap -- how many micro secs?
      = fromIntegral $ (maxBound `asTypeOf` (unRemoteClockModel undefined))
    wrapInterval
      = remoteClockPrecision * (fromRational ticksPerRemoteClockWrap)

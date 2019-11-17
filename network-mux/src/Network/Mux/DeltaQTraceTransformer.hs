{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.Mux.DeltaQTraceTransformer
where

import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import           Data.Maybe
import           Data.Word (Word32)


import           Control.Monad.Class.MonadTime
import           Network.Mux.Types

-- Map time intervals to real numbers, for the arithmetic.
newtype SISec  = S  Float -- this is all the precision we need,
  deriving (Eq, Ord, Num)
newtype SISec2 = S2 Float -- are there performance reasons to use Double?
  deriving (Eq, Ord, Num)

squareSISec :: SISec -> SISec2
squareSISec (S x) = S2 $ x * x

-- the per observation procesing step
step :: StatsA -> RemoteClockModel -> Time -> Int -> (Maybe Sample, StatsA)
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

constructSample :: StatsA -> Sample
constructSample _ = undefined

-- temp
data Sample

-- | Statistics accumulator
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

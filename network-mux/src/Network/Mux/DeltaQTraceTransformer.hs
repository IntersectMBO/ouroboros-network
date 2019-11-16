{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.Mux.DeltaQTraceTransformer
where

import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import           Data.Maybe

import           Control.Monad.Class.MonadTime
import           Network.Mux.Types

-- Map time intervals to real numbers, for the arithmetic.
newtype SISec  = S  Float -- this is all the precision we need,
  deriving (Eq, Ord, Num)
newtype SISec2 = S2 Float -- are there performance reasons to use Double?
  deriving (Eq, Ord, Num)

squareSISec :: SISec -> SISec2
squareSISec (S x) = S2 $ x * x

-- the observation step
step :: StatsA -> RemoteClockModel -> Time -> Int -> (Maybe Sample, StatsA)
step s remoteTS localTS obsSize
 | isNothing (referenceTimePoint s)
   = step (s { referenceTimePoint = Just $! (remoteTS, localTS)
             , nextSampleAt       = sampleInterval `addTime` localTS
             , timeLastObs        = localTS -- for single observation in sample case
             })
         remoteTS localTS obsSize
 | localTS <= nextSampleAt s
  = let Just refTimePoint = referenceTimePoint s
        transitTime       = calcTransitTime refTimePoint remoteTS localTS
    in (Nothing, recordObservation s localTS obsSize transitTime)
 | otherwise
  = let sample  = constructSample s
        (_, s') = step initialStatsA remoteTS localTS obsSize
    in (Just sample, s')

calcTransitTime :: (RemoteClockModel, Time)
                -> RemoteClockModel
                -> Time
                -> SISec
calcTransitTime _rt _rts _lts
  = undefined

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
    referenceTimePoint :: !(Maybe (RemoteClockModel, Time))
  , nextSampleAt       :: !Time
    -- per observation
  , numObservations    :: !Int
  , timeLastObs        :: !Time
  , observables        :: !(IntMap PerSizeRecord)
  }

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

sampleInterval :: DiffTime
sampleInterval = 10

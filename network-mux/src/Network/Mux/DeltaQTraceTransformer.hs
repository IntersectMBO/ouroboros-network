module Network.Mux.DeltaQTraceTransformer
where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Maybe

import Control.Monad.Class.MonadTime
import Network.Mux.Types

-- Map time intervals to real numbers, for the arithmetic.
newtype SISec  = S  Float -- this is all the precision we need,
newtype SISec2 = S2 Float -- are there performance reasons to use Double?

step :: StatsA -> RemoteClockModel -> Time -> Int -> (Maybe Sample, StatsA)
step s remoteTS localTS obsSize
 | isNothing (referenceTimePoint s)
   = step (s { referenceTimePoint = Just (remoteTS, localTS)
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
recordObservation _s _obsTime _obsSize _transitTime
  = undefined


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
  , observables        :: !(IntMap (SISec, Int, SISec, SISec2))
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

-- May want to make some of these configuration variables

sampleInterval :: DiffTime
sampleInterval = 10

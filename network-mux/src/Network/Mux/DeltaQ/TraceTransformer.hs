{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module Network.Mux.DeltaQ.TraceTransformer
  ( initDeltaQTracer
  , initDeltaQTracer'
  , initDeltaQTracers
  ) where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Class.MonadTime.SI
import Control.Tracer
import Data.Functor.Identity

import Network.Mux.DeltaQ.TraceStats
import Network.Mux.Trace
import Network.Mux.Types


-- | Create a trace transformer that will emit
--   `MuxTraceRecvDeltaQSample` no more frequently than every 10
--   seconds (when in use).
initDeltaQTracer :: (MonadSTM m, MonadMonotonicTime m)
                 => m (Tracer m BearerTrace -> Tracer m BearerTrace)
initDeltaQTracer = dqTracer <$> newTVarIO initialStatsA

initDeltaQTracer' :: (MonadSTM m, MonadMonotonicTime m)
                  => Tracer m BearerTrace
                  -> m (Tracer m BearerTrace)
initDeltaQTracer' tr = do
    v <- newTVarIO initialStatsA
    return $ dqTracer v tr

-- The transformer now consumes round-trip observations produced by
-- 'Network.Mux.RTT.processIngress' — carried in
-- 'TraceRecvDeltaQObservation' as (SDU length, round-trip delay).
-- The one-way transit variant it used to derive from clock-domain
-- timestamps is gone (cookies carry no timing info); the G/S
-- estimates the accumulator produces now characterise round-trip
-- behaviour rather than one-way transit.
dqTracer :: (MonadSTM m, MonadMonotonicTime m)
         => StrictTVar m StatsA
         -> Tracer m BearerTrace
         -> Tracer m BearerTrace
dqTracer sTvar tr = mkTracer go
  where
    -- The 'MiniProtocolNum' is available on the observation but
    -- currently ignored — bucketing / filtering per protocol is a
    -- downstream concern (see the tx-submission-cleanup appendix in
    -- track.md). All observations feed a single 'StatsA'.
    -- The 'MiniProtocolNum' is available on both observation types
    -- but currently ignored — bucketing / filtering per protocol is
    -- a downstream concern (see the tx-submission-cleanup appendix
    -- in track.md). All observations feed a single 'StatsA'.
    go (TraceRecvDeltaQObservation _mpNum obsSize delay) = do
      now <- getMonotonicTime
      updateRTT delay now (fromIntegral obsSize)
        >>= maybe (return ()) (traceWith tr . formatSample)
    -- Burst-SDU gaps feed 'stepBurst', which populates a separate
    -- through-origin estimator ('estBurstS' on the emitted sample).
    -- Shares the sample-period cadence with the RTT stream via a
    -- common 'withSamplePeriod' helper in 'TraceStats'.
    go (TraceRecvBurstSDU _mpNum obsSize gap) = do
      now <- getMonotonicTime
      updateBurst gap now (fromIntegral obsSize)
        >>= maybe (return ()) (traceWith tr . formatSample)
    go te@TraceEmitDeltaQ
      = emitSample >> traceWith tr te
    go x
      = pure ()

    updateRTT delay now n
      = atomically (stateTVar sTvar (step delay now n))

    updateBurst gap now n
      = atomically (stateTVar sTvar (stepBurst gap now n))

    emitSample
      =  atomically (stateTVar sTvar processSample)
         >>= traceWith tr . formatSample

    processSample s
      = (constructSample s, initialStatsA)

    formatSample (OneWaySample {..})
      = TraceRecvDeltaQSample duration sumPackets sumTotalSDU
                              estDeltaQS estBurstS
                              estDeltaQVMean estDeltaQVVar
                              estR sizeDist


initDeltaQTracers :: (MonadSTM m, MonadMonotonicTime m)
                  => Tracers m
                  -> m (Tracers m)
initDeltaQTracers tracers = do
    bearerTracer' <- initDeltaQTracer' (Identity >$< bearerTracer tracers)
    return $ tracers { bearerTracer = runIdentity >$< bearerTracer' }

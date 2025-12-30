{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module Network.Mux.DeltaQ.TraceTransformer
  ( initDeltaQTracer
  , initDeltaQTracer'
  , initDeltaQTracers
  ) where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Tracer
import Data.Functor.Contravariant ((>$<))
import Data.Functor.Identity

import Network.Mux.DeltaQ.TraceStats
import Network.Mux.Trace
import Network.Mux.Types


-- | Create a trace transformer that will emit
--   `MuxTraceRecvDeltaQSample` no more frequently than every 10
--   seconds (when in use).
initDeltaQTracer :: MonadSTM m
                 => m (Tracer m BearerTrace -> Tracer m BearerTrace)
initDeltaQTracer = newTVarIO initialStatsA >>= pure . dqTracer

initDeltaQTracer' :: MonadSTM m
                  => Tracer m BearerTrace
                  -> m (Tracer m BearerTrace)
initDeltaQTracer' tr = do
    v <- newTVarIO initialStatsA
    return $ dqTracer v tr

dqTracer :: MonadSTM m
         => StrictTVar m StatsA
         -> Tracer m BearerTrace
         -> Tracer m BearerTrace
dqTracer sTvar tr = Tracer go
  where
    go (TraceRecvDeltaQObservation SDUHeader { mhTimestamp, mhLength } t)
      = update mhTimestamp t (fromIntegral mhLength)
        >>= maybe (return ()) (traceWith tr . formatSample)
    go te@TraceEmitDeltaQ
      = emitSample >> traceWith tr te
    go x
      = traceWith tr x

    update rClock lClock n
      = atomically (stateTVar sTvar (step rClock lClock n))

    emitSample
      =  atomically (stateTVar sTvar processSample)
         >>= traceWith tr . formatSample

    processSample s
      = (constructSample s, initialStatsA)

    formatSample (OneWaySample {..})
      = TraceRecvDeltaQSample duration sumPackets sumTotalSDU
                              estDeltaQS estDeltaQVMean estDeltaQVVar
                              estR sizeDist


initDeltaQTracers :: MonadSTM m
                  => Tracers m
                  -> m (Tracers m)
initDeltaQTracers tracers = do
    bearerTracer' <- initDeltaQTracer' (Identity >$< bearerTracer tracers)
    return $ tracers { bearerTracer = runIdentity >$< bearerTracer' }

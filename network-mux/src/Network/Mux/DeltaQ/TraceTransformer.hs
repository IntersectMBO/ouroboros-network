{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module Network.Mux.DeltaQ.TraceTransformer
  ( initDeltaQTracer
  , initDeltaQTracer'
  ) where

import           Control.Monad.Class.MonadSTM.Strict
import           Control.Tracer

import           Network.Mux.DeltaQ.TraceStats
import           Network.Mux.Trace
import           Network.Mux.Types


-- | Create a trace transformer that will emit
--   `MuxTraceRecvDeltaQSample` no more frequently than every 10
--   seconds (when in use).
initDeltaQTracer :: MonadSTM m
                 => m (Tracer m MuxTrace -> Tracer m MuxTrace)
initDeltaQTracer = newTVarIO initialStatsA >>= pure . dqTracer

initDeltaQTracer' :: MonadSTM m
                  => Tracer m MuxTrace
                  -> m (Tracer m MuxTrace)
initDeltaQTracer' tr = do
    v <- newTVarIO initialStatsA
    return $ dqTracer v tr

dqTracer :: MonadSTM m
         => StrictTVar m StatsA
         -> Tracer m MuxTrace
         -> Tracer m MuxTrace
dqTracer sTvar tr = Tracer go
  where
    go (MuxTraceRecvDeltaQObservation MuxSDUHeader { mhTimestamp, mhLength } t)
      = update mhTimestamp t (fromIntegral mhLength)
        >>= maybe (return ()) (traceWith tr . formatSample)
    go te@(MuxTraceCleanExit {})
       = emitSample >> traceWith tr te
    go te@(MuxTraceExceptionExit {})
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
      = MuxTraceRecvDeltaQSample duration sumPackets sumTotalSDU
                                 estDeltaQS estDeltaQVMean estDeltaQVVar
                                 estR sizeDist

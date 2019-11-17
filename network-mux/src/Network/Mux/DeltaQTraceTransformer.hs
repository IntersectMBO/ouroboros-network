{-# LANGUAGE RecordWildCards #-}
module Network.Mux.DeltaQTraceTransformer
  (initDeltaQTracer)
where

import Control.Tracer
import Control.Monad.Class.MonadSTM.Strict

import Network.Mux.Types
import Network.Mux.DeltaQTraceStats


-- | Create a trace transformer that will emit
--   `MuxTraceRecvDeltaQSample` no more frequently than every 10
--   seconds (when in use).
initDeltaQTracer :: MonadSTM m
                 => m (Tracer m (MuxTrace pctl) -> Tracer m (MuxTrace pctl))
initDeltaQTracer = newTVarM initialStatsA >>= pure . dqTracer


dqTracer :: MonadSTM m
         => StrictTVar m StatsA
         -> Tracer m (MuxTrace pctl)
         -> Tracer m (MuxTrace pctl)
dqTracer sTvar tr = Tracer go
  where
    go (MuxTraceRecvDeltaQObservation pdu t)
      = update (msTimestamp pdu) t (fromIntegral $ msLength pdu)
        >>= maybe (return ()) (traceWith tr . formatSample)
    go te@(MuxTraceCleanExit {})
       = emitSample >> traceWith tr te
    go te@(MuxTraceExceptionExit {})
       = emitSample >> traceWith tr te
    go x
      = traceWith tr x

    update rClock lClock n
      = atomically (updateTVar sTvar (step rClock lClock n))

    emitSample
      =  atomically (updateTVar sTvar processSample)
         >>= traceWith tr . formatSample

    processSample s
      = (initialStatsA, constructSample s)

    formatSample (OneWaySample {..})
      = MuxTraceRecvDeltaQSample duration sumPackets sumTotalSDU
                                 estDeltaQS estDeltaQVMean estDeltaQVStd

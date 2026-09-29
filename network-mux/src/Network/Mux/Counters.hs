{-# LANGUAGE NamedFieldPuns #-}

-- | Node-wide mux counters: how muxes on one side, node-to-node or
-- node-to-client, have failed, counted once per mux as it dies, and traced
-- periodically.
--
module Network.Mux.Counters
  ( MuxCounters
  , newMuxCounters
  , EgressCounts (..)
  , IngressCounts (..)
  , CountersTrace (..)
  , countMuxerFailure
  , countDemuxerFailure
  , readMuxCounters
  , countersLoop
  , countersInterval
  ) where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Exception (SomeException, fromException)
import Control.Monad (forever)
import Control.Monad.Class.MonadTimer.SI
import Control.Tracer (Tracer, traceWith)

import Data.Word (Word64)

import Network.Mux.Trace (Error (..))

-- | The write side.
data EgressCounts = EgressCounts {
  ecWriteTimeouts :: !Word64      -- ^ peers that took nothing within the SDU timeout
  }
  deriving (Eq, Show)

-- | The read side.
data IngressCounts = IngressCounts {
  icReadTimeouts   :: !Word64,    -- ^ an SDU header arrived, the rest did not in time
  icOverruns       :: !Word64,    -- ^ a peer exceeded a mini-protocol's ingress limit
  icProtocolErrors :: !Word64,    -- ^ undecodable SDUs, unknown mini-protocols,
                                  --   data an initiator-only mux cannot take
  icBearerClosed   :: !Word64     -- ^ the peer closed the connection
  }
  deriving (Eq, Show)

-- | Counters shared by every mux on one side.
newtype MuxCounters m = MuxCounters (StrictTVar m (Sides EgressCounts IngressCounts))

-- | The write side's count and the read side's, each evaluated as it is
-- stored, so an update never leaves the previous count behind a thunk.
data Sides a b = Sides !a !b
  deriving (Eq, Show)

newMuxCounters :: MonadSTM m => m (MuxCounters m)
newMuxCounters = MuxCounters <$> newTVarIO (Sides (EgressCounts 0) (IngressCounts 0 0 0 0))

readMuxCounters :: MonadSTM m => MuxCounters m -> STM m (EgressCounts, IngressCounts)
readMuxCounters (MuxCounters v) = (\(Sides eg ing) -> (eg, ing)) <$> readTVar v

-- | Count the exception a mux's muxer died with.
countMuxerFailure :: MonadSTM m => MuxCounters m -> SomeException -> STM m ()
countMuxerFailure (MuxCounters v) e =
  case fromException e of
       Just SDUWriteTimeout ->
         modifyTVar v (\(Sides eg ing) -> Sides eg { ecWriteTimeouts = ecWriteTimeouts eg + 1 } ing)
       _ -> return ()

-- | Count the exception a mux's demuxer died with.
countDemuxerFailure :: MonadSTM m => MuxCounters m -> SomeException -> STM m ()
countDemuxerFailure (MuxCounters v) e =
  case fromException e of
       Just SDUReadTimeout          -> bump (\c -> c { icReadTimeouts   = icReadTimeouts c + 1 })
       Just IngressQueueOverRun {}  -> bump (\c -> c { icOverruns       = icOverruns c + 1 })
       Just UnknownMiniProtocol {}  -> bump (\c -> c { icProtocolErrors = icProtocolErrors c + 1 })
       Just InitiatorOnly {}        -> bump (\c -> c { icProtocolErrors = icProtocolErrors c + 1 })
       Just SDUDecodeError {}       -> bump (\c -> c { icProtocolErrors = icProtocolErrors c + 1 })
       Just BearerClosed {}         -> bump (\c -> c { icBearerClosed   = icBearerClosed c + 1 })
       _                            -> return ()
  where
    bump f = modifyTVar v (\(Sides eg ing) -> Sides eg (f ing))

-- | A snapshot of one side's counters.
data CountersTrace =
    TraceRemoteEgress  EgressCounts
  | TraceRemoteIngress IngressCounts
  | TraceLocalEgress   EgressCounts
  | TraceLocalIngress  IngressCounts
  deriving (Eq, Show)

-- | A prime, so that snapshots do not keep step with other periodic work such
-- as keep-alive, and below a 10 s scrape.
countersInterval :: DiffTime
countersInterval = 7

-- | Every @interval@, snapshot the node-to-node and the node-to-client
-- counters and trace both sides of each.
countersLoop :: (MonadDelay m, MonadSTM m)
             => MuxCounters m    -- ^ node-to-node
             -> MuxCounters m    -- ^ node-to-client
             -> DiffTime
             -> Tracer m CountersTrace
             -> m void
countersLoop remote local interval tracer = forever $ do
  threadDelay interval
  ((re, ri), (le, li)) <- atomically $ (,) <$> readMuxCounters remote <*> readMuxCounters local
  traceWith tracer (TraceRemoteEgress re)
  traceWith tracer (TraceRemoteIngress ri)
  traceWith tracer (TraceLocalEgress le)
  traceWith tracer (TraceLocalIngress li)

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Ouroboros.Network.PeerSelection.PeerMetric
  ( -- * Peer metrics
    PeerMetrics
  , PeerMetricsConfiguration (..)
  , newPeerMetric
    -- * Metric calculations
  , upstreamyness
  , fetchynessBytes
  , fetchynessBlocks
    -- * Tracers
  , headerMetricTracer
  , fetchedMetricTracer
    -- * Metrics reporters
  , ReportPeerMetrics (..)
  , nullMetric
  , reportMetric
    -- * Internals
    -- only exported for testing purposes
  , SlotMetric
  , newPeerMetric'
  ) where

import           Control.Concurrent.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadTime
import           Control.Tracer (Tracer (..), contramap, nullTracer)
import           Data.IntPSQ (IntPSQ)
import qualified Data.IntPSQ as IntPSQ
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           Cardano.Slotting.Slot (SlotNo (..))
import           Ouroboros.Network.DeltaQ (SizeInBytes)
import           Ouroboros.Network.NodeToNode (ConnectionId (..))
import           Ouroboros.Network.PeerSelection.PeerMetric.Type


newtype PeerMetricsConfiguration = PeerMetricsConfiguration {
      -- | The maximum numbers of slots we will store data for.  On some chains
      -- sometimes this corresponds to 1h worth of metrics *sighs*.
      --
      -- this number MUST correspond to number of headers / blocks which are
      -- produced in one hour.
      maxEntriesToTrack :: Int
    }


type SlotMetric p = IntPSQ SlotNo (p, Time)

-- | Mutable peer metrics state accessible via 'STM'.
--
newtype PeerMetrics m p = PeerMetrics {
    peerMetricsVar :: StrictTVar m (PeerMetricsState p)
  }

-- | Internal state
--
data PeerMetricsState p = PeerMetricsState {
    headerMetrics  :: SlotMetric p
  , fetchedMetrics :: SlotMetric (p, SizeInBytes)
  }


newPeerMetric
    :: MonadSTM m
    => m (PeerMetrics m p)
newPeerMetric = newPeerMetric' IntPSQ.empty IntPSQ.empty


newPeerMetric'
    :: MonadSTM m
    => SlotMetric p
    -> SlotMetric (p, SizeInBytes)
    -> m (PeerMetrics m p)
newPeerMetric' headerMetrics fetchedMetrics =
  PeerMetrics <$> newTVarIO PeerMetricsState {
                                headerMetrics,
                                fetchedMetrics
                              }

reportMetric
    :: forall m p.
       ( MonadSTM m )
     => PeerMetricsConfiguration
     -> PeerMetrics m p
     -> ReportPeerMetrics m (ConnectionId p)
reportMetric config peerMetrics =
  ReportPeerMetrics (headerMetricTracer  config peerMetrics)
                    (fetchedMetricTracer config peerMetrics)

nullMetric
    :: MonadSTM m
    => ReportPeerMetrics m p
nullMetric =
  ReportPeerMetrics nullTracer nullTracer


slotMetricKey :: SlotNo -> Int
slotMetricKey (SlotNo s) = fromIntegral s


-- | Tracer which updates header metrics (upstreameness).
--
headerMetricTracer
    :: forall m p.
       ( MonadSTM m )
    => PeerMetricsConfiguration
    -> PeerMetrics m p
    -> Tracer (STM m) (TraceLabelPeer (ConnectionId p) (SlotNo, Time))
headerMetricTracer config PeerMetrics{peerMetricsVar} =
    (\(TraceLabelPeer con d) -> TraceLabelPeer (remoteAddress con) d)
    `contramap`
    metricsTracer
      (headerMetrics <$> readTVar peerMetricsVar)
      (\headerMetrics -> modifyTVar peerMetricsVar
                                    (\metrics -> metrics { headerMetrics }))
      config


-- | Tracer which updates fetched metrics (fetchyness).
--
fetchedMetricTracer
    :: forall m p.
       ( MonadSTM m )
    => PeerMetricsConfiguration
    -> PeerMetrics m p
    -> Tracer (STM m) (TraceLabelPeer (ConnectionId p)
                                      ( SizeInBytes
                                      , SlotNo
                                      , Time
                                      ))
fetchedMetricTracer config PeerMetrics{peerMetricsVar} =
    (\(TraceLabelPeer con (bytes, slot, time)) ->
       TraceLabelPeer (remoteAddress con, bytes) (slot, time))
    `contramap`
     metricsTracer
       (fetchedMetrics <$> readTVar peerMetricsVar)
       (\fetchedMetrics -> modifyTVar peerMetricsVar
                                      (\metrics -> metrics { fetchedMetrics }))
       config

metricsTracer
    :: forall m p.  ( MonadSTM m )
    => STM m (SlotMetric p)       -- ^ read metrics
    -> (SlotMetric p -> STM m ()) -- ^ update metrics
    -> PeerMetricsConfiguration
    -> Tracer (STM m) (TraceLabelPeer p (SlotNo, Time))
metricsTracer getMetrics writeMetrics PeerMetricsConfiguration { maxEntriesToTrack } = Tracer $ \(TraceLabelPeer !peer (!slot, !time)) -> do
    metrics <- getMetrics
    case IntPSQ.lookup (slotMetricKey slot) metrics of
         Nothing -> do
             let metrics' = IntPSQ.insert (slotMetricKey slot) slot (peer, time) metrics
             if IntPSQ.size metrics' > maxEntriesToTrack
                then
                  case IntPSQ.minView metrics' of
                       Nothing -> error "impossible empty pq" -- We just inserted an element!
                       Just (_, minSlotNo, _, metrics'') ->
                            if minSlotNo == slot
                               then return ()
                               else writeMetrics metrics''
             else writeMetrics metrics'
         Just (_, (_, oldTime)) ->
             if oldTime <= time
                then return ()
                else writeMetrics (IntPSQ.insert (slotMetricKey slot) slot (peer, time) metrics)


--
-- Metrics
--
-- * upstreameness
-- * fetchyness by blocks
-- * fetchyness by bytes
--

-- | Returns a Map which counts the number of times a given peer was the first
-- to present us with a block/header.
--
upstreamyness
    :: forall p m.
       MonadSTM m
    => Ord p
    => PeerMetrics m p
    -> STM m (Map p Int)
upstreamyness PeerMetrics {peerMetricsVar} =
    upstreamynessImpl <$> readTVar peerMetricsVar


upstreamynessImpl
    :: forall p.
       Ord p
    => PeerMetricsState p
    -> Map p Int
upstreamynessImpl PeerMetricsState { headerMetrics } =
    IntPSQ.fold' count Map.empty headerMetrics
  where
    count :: Int
          -> SlotNo
          -> (p,Time)
          -> Map p Int
          -> Map p Int
    count _ _ (peer,_) m =
        Map.alter fn peer m
      where
        fn :: Maybe Int -> Maybe Int
        fn Nothing  = Just 1
        fn (Just c) = Just $! c + 1


-- | Returns a Map which counts the number of bytes downloaded for a given
-- peer.
--
fetchynessBytes
    :: forall p m.
       MonadSTM m
    => Ord p
    => PeerMetrics m p
    -> STM m (Map p Int)
fetchynessBytes PeerMetrics {peerMetricsVar} =
    fetchynessBytesImpl <$> readTVar peerMetricsVar

fetchynessBytesImpl
    :: forall p.
       Ord p
    => PeerMetricsState p
    -> Map p Int
fetchynessBytesImpl PeerMetricsState { fetchedMetrics } =
    IntPSQ.fold' count Map.empty fetchedMetrics
  where
    count :: Int
          -> SlotNo
          -> ((p, SizeInBytes), Time)
          -> Map p Int
          -> Map p Int
    count _ _ ((peer, bytes),_) m =
        Map.alter fn peer m
      where
        fn :: Maybe Int -> Maybe Int
        fn Nothing         = Just $ fromIntegral bytes
        fn (Just oldBytes) = Just $! oldBytes + fromIntegral bytes


-- | Returns a Map which counts the number of times a given peer was the first
-- we downloaded a block from.
--
fetchynessBlocks
    :: forall p m.
       MonadSTM m
    => Ord p
    => PeerMetrics m p
    -> STM m (Map p Int)
fetchynessBlocks PeerMetrics {peerMetricsVar} =
    fetchynessBlocksImpl <$> readTVar peerMetricsVar

fetchynessBlocksImpl
    :: forall p.
       Ord p
    => PeerMetricsState p
    -> Map p Int
fetchynessBlocksImpl PeerMetricsState { fetchedMetrics } =
    IntPSQ.fold' count Map.empty fetchedMetrics
  where
    count :: Int
          -> SlotNo
          -> ((p, SizeInBytes), Time)
          -> Map p Int
          -> Map p Int
    count _ _ ((peer, _),_) m =
        Map.alter fn peer m
      where
        fn :: Maybe Int -> Maybe Int
        fn Nothing  = Just 1
        fn (Just c) = Just $! c + 1



{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Ouroboros.Network.PeerSelection.PeerMetric
  ( -- * Peer metrics
    PeerMetrics
  , newPeerMetric
  , getHeaderMetrics
  , getFetchedMetrics
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

import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadTime
import           Control.Tracer (Tracer (..), contramap, nullTracer)
import           Data.IntPSQ (IntPSQ)
import qualified Data.IntPSQ as Pq
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           Cardano.Slotting.Slot (SlotNo (..))
import           Ouroboros.Network.DeltaQ (SizeInBytes)
import           Ouroboros.Network.NodeToNode (ConnectionId (..))
import           Ouroboros.Network.PeerSelection.PeerMetric.Type


-- The maximum numbers of slots we will store data for.
-- On some chains sometimes this corresponds to 1h
-- worth of metrics *sighs*.
maxEntriesToTrack :: Int
maxEntriesToTrack = 180


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
newPeerMetric = newPeerMetric' Pq.empty Pq.empty


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
     => PeerMetrics m p
     -> ReportPeerMetrics m (ConnectionId p)
reportMetric peerMetrics =
  ReportPeerMetrics (headerMetricTracer peerMetrics)
                    (fetchedMetricTracer peerMetrics)

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
    => PeerMetrics m p
    -> Tracer (STM m) (TraceLabelPeer (ConnectionId p) (SlotNo, Time))
headerMetricTracer PeerMetrics{peerMetricsVar} =
    (\(TraceLabelPeer con d) -> TraceLabelPeer (remoteAddress con) d)
    `contramap`
    metricsTracer
      (headerMetrics <$> readTVar peerMetricsVar)
      (\headerMetrics -> modifyTVar peerMetricsVar
                                    (\metrics -> metrics { headerMetrics }))


-- | Tracer which updates fetched metrics (fetchyness).
--
fetchedMetricTracer
    :: forall m p.
       ( MonadSTM m )
    => PeerMetrics m p
    -> Tracer (STM m) (TraceLabelPeer (ConnectionId p)
                                      ( SizeInBytes
                                      , SlotNo
                                      , Time
                                      ))
fetchedMetricTracer PeerMetrics{peerMetricsVar} =
    (\(TraceLabelPeer con (bytes, slot, time)) ->
       TraceLabelPeer (remoteAddress con, bytes) (slot, time))
    `contramap`
     metricsTracer
       (fetchedMetrics <$> readTVar peerMetricsVar)
       (\fetchedMetrics -> modifyTVar peerMetricsVar
                                      (\metrics -> metrics { fetchedMetrics }))

metricsTracer
    :: forall m p.  ( MonadSTM m )
    => STM m (SlotMetric p)       -- ^ read metrics
    -> (SlotMetric p -> STM m ()) -- ^ update metrics
    -> Tracer (STM m) (TraceLabelPeer p (SlotNo, Time))
metricsTracer getMetrics writeMetrics = Tracer $ \(TraceLabelPeer !peer (!slot, !time)) -> do
    metrics <- getMetrics
    case Pq.lookup (slotMetricKey slot) metrics of
         Nothing -> do
             let metrics' = Pq.insert (slotMetricKey slot) slot (peer, time) metrics
             if Pq.size metrics' > maxEntriesToTrack
                then
                  case Pq.minView metrics' of
                       Nothing -> error "impossible empty pq" -- We just inserted an element!
                       Just (_, minSlotNo, _, metrics'') ->
                            if minSlotNo == slot
                               then return ()
                               else writeMetrics metrics''
             else writeMetrics metrics'
         Just (_, (_, oldTime)) ->
             if oldTime <= time
                then return ()
                else writeMetrics (Pq.insert (slotMetricKey slot) slot (peer, time) metrics)


getHeaderMetrics
    :: MonadSTM m
    => PeerMetrics m p
    -> STM m (SlotMetric p)
getHeaderMetrics PeerMetrics{peerMetricsVar} =
    headerMetrics <$> readTVar peerMetricsVar

getFetchedMetrics
    :: MonadSTM m
    => PeerMetrics m p
    -> STM m (SlotMetric (p, SizeInBytes))
getFetchedMetrics PeerMetrics{peerMetricsVar} =
    fetchedMetrics <$> readTVar peerMetricsVar


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
    :: forall p.  ( Ord p )
    => SlotMetric p
    -> Map p Int
upstreamyness = Pq.fold' count Map.empty
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
    :: forall p.  ( Ord p )
    => SlotMetric (p, SizeInBytes)
    -> Map p Int
fetchynessBytes = Pq.fold' count Map.empty
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
    :: forall p.  ( Ord p )
    => SlotMetric (p, SizeInBytes)
    -> Map p Int
fetchynessBlocks = Pq.fold' count Map.empty
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



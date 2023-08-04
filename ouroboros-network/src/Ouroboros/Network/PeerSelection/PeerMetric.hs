{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}


module Ouroboros.Network.PeerSelection.PeerMetric
  ( -- * Peer metrics
    PeerMetrics
  , PeerMetricsConfiguration (..)
  , newPeerMetric
    -- * Metric calculations
  , joinedPeerMetricAt
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

import           Control.Concurrent.Class.MonadSTM.Strict.TVar.Checked.Switch
import           Control.DeepSeq (NFData (..))
import           Control.Monad (when)
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadTime.SI
import           Control.Tracer (Tracer (..), contramap, nullTracer)
import           Data.Bifunctor (Bifunctor (..))
import           Data.IntPSQ (IntPSQ)
import qualified Data.IntPSQ as IntPSQ
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Monoid (Sum (..))
import           Data.OrdPSQ (OrdPSQ)
import qualified Data.OrdPSQ as OrdPSQ
import           GHC.Generics

import           NoThunks.Class
import           NoThunks.Class.Orphans ()

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
  deriving (Show, Generic, NoThunks, NFData)


-- | Integer based metric ordered by 'SlotNo' which holds the peer and time.
--
-- The `p` parameter is truly polymorphic.  For `upstreamyness` and we use peer
-- address, and for `fetchyness` it is a pair of peer id and bytes downloaded.
--
type SlotMetric p = IntPSQ SlotNo (p, Time)

-- | Peer registry ordered by slot when a peer joined the peer metric.
--
type PeerRegistry p = OrdPSQ p SlotNo AverageMetrics

-- | Peer registry ordered by slot when a peer was last seen.
--
type LastSeenRegistry p = OrdPSQ p SlotNo ()

-- | Mutable peer metrics state accessible via 'STM'.
--
newtype PeerMetrics m p = PeerMetrics {
    peerMetricsVar :: StrictTVar m (PeerMetricsState p)
  }

-- | Internal state
--
data PeerMetricsState p = PeerMetricsState {

    -- | Header metrics.
    --
    headerMetrics    :: !(SlotMetric p),

    -- | Fetch metrics.
    --
    fetchedMetrics   :: !(SlotMetric (p, SizeInBytes)),

    -- | Registry recording when a peer joined the board of 'PeerMetrics'.  The
    -- values are average header and fetched metrics.
    --
    peerRegistry     :: !(PeerRegistry p),

    -- | A registry which indicates when the last time a peer was seen.
    --
    -- If a peer hasn't been seen since the oldest recorded slot number, it will
    -- be removed.
    --
    lastSeenRegistry :: !(LastSeenRegistry p),

    -- | Latest slot registered in the leader board.
    --
    lastSlotNo       :: !SlotNo,

    -- | Metrics configuration.  Its kept here just for convenience.
    --
    metricsConfig    :: !PeerMetricsConfiguration
  }
  deriving (Show, Generic, NoThunks, NFData)


-- | Average results at a given slot.
--
data AverageMetrics = AverageMetrics {
    averageUpstreamyness    :: !Int,
    averageFetchynessBlocks :: !Int,
    averageFetchynessBytes  :: !Int
  }
  deriving (Show, Generic, NoThunks, NFData)


newPeerMetric
    :: (MonadLabelledSTM m, NoThunks p, NFData p)
    => PeerMetricsConfiguration
    -> m (PeerMetrics m p)
newPeerMetric = newPeerMetric' IntPSQ.empty IntPSQ.empty


newPeerMetric'
    :: (MonadLabelledSTM m, NoThunks p, NFData p)
    => SlotMetric p
    -> SlotMetric (p, SizeInBytes)
    -> PeerMetricsConfiguration
    -> m (PeerMetrics m p)
newPeerMetric' headerMetrics fetchedMetrics metricsConfig =
    case rnf state of
      () -> atomically $ do
        a <- newTVarWithInvariant (\a -> show <$> unsafeNoThunks a)
                                  state
        labelTVar a "peermetrics"
        return (PeerMetrics a)
  where
    state = PeerMetricsState {
                headerMetrics,
                fetchedMetrics,
                peerRegistry     = OrdPSQ.empty,
                lastSeenRegistry = OrdPSQ.empty,
                lastSlotNo       = SlotNo 0,
                metricsConfig
              }

updateLastSlot :: SlotNo -> PeerMetricsState p -> PeerMetricsState p
updateLastSlot slotNo state@PeerMetricsState { lastSlotNo }
  | slotNo > lastSlotNo = state { lastSlotNo = slotNo }
  | otherwise           = state


firstSlotNo :: PeerMetricsState p -> Maybe SlotNo
firstSlotNo PeerMetricsState {headerMetrics, fetchedMetrics} =
    (\a b -> min (f a) (f b))
      <$> IntPSQ.minView headerMetrics
      <*> IntPSQ.minView fetchedMetrics
  where
    f :: (a, SlotNo, b, c) -> SlotNo
    f (_, slotNo, _, _) = slotNo


reportMetric
    :: forall m p.
       ( MonadSTM m
       , Ord p
       )
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


slotToInt :: SlotNo -> Int
slotToInt = fromIntegral . unSlotNo


-- | Tracer which updates header metrics (upstreameness) and inserts new peers
-- into 'peerRegistry'.
--
headerMetricTracer
    :: forall m p.
       ( MonadSTM m
       , Ord p
       )
    => PeerMetricsConfiguration
    -> PeerMetrics m p
    -> Tracer (STM m) (TraceLabelPeer (ConnectionId p) (SlotNo, Time))
headerMetricTracer config peerMetrics@PeerMetrics{peerMetricsVar} =
    bimap remoteAddress fst
    `contramap`
    peerRegistryTracer peerMetrics
 <> first remoteAddress
    `contramap`
    metricsTracer
      (headerMetrics <$> readTVar peerMetricsVar)
      (\headerMetrics -> modifyTVar peerMetricsVar
                                    (\metrics -> metrics { headerMetrics }))
      config


-- | Tracer which updates fetched metrics (fetchyness) and inserts new peers
-- into 'peerRegistry'.
--
fetchedMetricTracer
    :: forall m p.
       ( MonadSTM m
       , Ord p
       )
    => PeerMetricsConfiguration
    -> PeerMetrics m p
    -> Tracer (STM m) (TraceLabelPeer (ConnectionId p)
                                      ( SizeInBytes
                                      , SlotNo
                                      , Time
                                      ))
fetchedMetricTracer config peerMetrics@PeerMetrics{peerMetricsVar} =
    bimap remoteAddress (\(_, slotNo, _) -> slotNo)
    `contramap`
    peerRegistryTracer peerMetrics
 <> (\(TraceLabelPeer ConnectionId { remoteAddress } (!bytes, slot, time)) ->
       TraceLabelPeer (remoteAddress, bytes) (slot, time))
    `contramap`
     metricsTracer
       (fetchedMetrics <$> readTVar peerMetricsVar)
       (\fetchedMetrics -> modifyTVar peerMetricsVar
                                      (\metrics -> metrics { fetchedMetrics }))
       config


--
--  peer registry tracer which maintains 'peerRegistry' and 'lastSeenRegistry'
--

-- | Insert new peer into 'PeerMetricsState'.  If this peer hasn't been
-- recorded before, we compute the current average score and record it in
-- 'peerRegistry'.  Entries in `peerRegistry' are only kept if they are newer
-- than the oldest slot in the 'headerMetrics' and 'fetchedMetrics'.
--
-- Implementation detail:
-- We need first check 'lastSeenRegistry' which checks if a peer is part of the
-- leader board.  If a peer has not contributed to 'PeerMetrics' in
-- `maxEntriesToTrack` slots, we will consider it as a new peer.  Without using
-- `lastSeenRegistry` we could consider a peer new while it exists in peer
-- metrics for a very long time.  Just using `peerRegistry` does not guarantee
-- that.
--
insertPeer :: forall p. Ord p
           => p
           -> SlotNo -- ^ current slot
           -> PeerMetricsState p
           -> PeerMetricsState p
insertPeer p !slotNo
           peerMetricsState@PeerMetricsState { lastSeenRegistry, peerRegistry } =
    if p `OrdPSQ.member` lastSeenRegistry
    then peerMetricsState
    else case OrdPSQ.alter f p peerRegistry of
           (False, !peerRegistry') -> peerMetricsState { peerRegistry = peerRegistry' }
           (True,  _peerRegistry') -> peerMetricsState
  where
    f :: Maybe (SlotNo, AverageMetrics) -> (Bool, Maybe (SlotNo, AverageMetrics))
    f a@Just {} = (True,  a)
    f Nothing   = (False, Just $! (slotNo, metrics))
      where
        !metrics = AverageMetrics {
                     averageUpstreamyness    = avg (upstreamynessImpl    peerMetricsState),
                     averageFetchynessBytes  = avg (fetchynessBytesImpl  peerMetricsState),
                     averageFetchynessBlocks = avg (fetchynessBlocksImpl peerMetricsState)
                   }

    avg :: Map p Int -> Int
    avg m | Map.null m = 0
    avg m =
      -- division truncated towards the plus infinity, rather then the minus
      -- infinity
      case getSum (foldMap Sum m) `divMod` Map.size m of
        (x, 0) -> x
        (x, _) -> x + 1


-- | A tracer which takes care about:
--
-- * inserting new peers to 'peerRegistry'
-- * removing old entries of 'peerRegistry'
--
-- * inserting new peers to 'lastSeenRegistry'
-- * removing old entries of 'lastSeenRegistry'
--
peerRegistryTracer :: forall p m.
                      ( MonadSTM m
                      , Ord p
                      )
                   => PeerMetrics m p
                   -> Tracer (STM m) (TraceLabelPeer p SlotNo)
peerRegistryTracer PeerMetrics { peerMetricsVar } =
    Tracer $ \(TraceLabelPeer peer slotNo) -> do
      -- order matters: 'insertPeer' must access the previous value of
      -- lastSeenRegistry
      modifyTVar peerMetricsVar $ updateLastSlot slotNo
                                . witnessedPeer peer slotNo
                                . insertPeer peer slotNo
                                . afterSlot
  where
    snd_ (_, slotNo, _, _) = slotNo

    -- remove all entries which are older than the oldest slot in the
    -- 'PeerMetrics'
    afterSlot :: PeerMetricsState p -> PeerMetricsState p
    afterSlot peerMetrics@PeerMetricsState { headerMetrics,
                                             fetchedMetrics,
                                             peerRegistry,
                                             lastSeenRegistry } =
      let -- the oldest slot in the metrics leader board
          slotNo :: SlotNo
          slotNo = fromMaybe 0 $
            (snd_ <$> IntPSQ.minView headerMetrics)
            `min`
            (snd_ <$> IntPSQ.minView fetchedMetrics)

      in peerMetrics
           { peerRegistry     = snd (OrdPSQ.atMostView slotNo peerRegistry),
             lastSeenRegistry = snd (OrdPSQ.atMostView slotNo lastSeenRegistry)
           }

    witnessedPeer :: p -> SlotNo
                  -> PeerMetricsState p -> PeerMetricsState p
    witnessedPeer peer slotNo
                  peerMetrics@PeerMetricsState { lastSeenRegistry } =
                  peerMetrics { lastSeenRegistry =
                                  OrdPSQ.insert peer slotNo () lastSeenRegistry
                              }


--
-- Metrics tracer
--

-- | A metrics tracer which updates the metric.
--
metricsTracer
    :: forall m p.
       MonadSTM m
    => STM m (SlotMetric p)       -- ^ read metrics
    -> (SlotMetric p -> STM m ()) -- ^ update metrics
    -> PeerMetricsConfiguration
    -> Tracer (STM m) (TraceLabelPeer p (SlotNo, Time))
metricsTracer getMetrics writeMetrics PeerMetricsConfiguration { maxEntriesToTrack } =
    Tracer $ \(TraceLabelPeer !peer (!slot, !time)) -> do
      metrics <- getMetrics
      let !k = slotToInt slot
          !v = (peer, time)
      case IntPSQ.lookup k metrics of
           Nothing -> do
             let metrics' = IntPSQ.insert k slot v metrics
             if IntPSQ.size metrics' > maxEntriesToTrack
             -- drop last element if the metric board is too large
             then case IntPSQ.minView metrics' of
                    Nothing -> error "impossible empty pq"
                            -- We just inserted an element!
                    Just (_, minSlotNo, _, metrics'') ->
                         when (minSlotNo /= slot) $
                              writeMetrics metrics''
             else writeMetrics metrics'
           Just (_, (_, oldTime)) ->
               when (oldTime > time) $
                    writeMetrics (IntPSQ.insert k slot v metrics)


joinedPeerMetricAt
    :: forall p m.
       MonadSTM m
    => Ord p
    => PeerMetrics m p
    -> STM m (Map p SlotNo)
joinedPeerMetricAt PeerMetrics {peerMetricsVar} =
    joinedPeerMetricAtImpl <$> readTVar peerMetricsVar


joinedPeerMetricAtImpl
    :: forall p.
       Ord p
    => PeerMetricsState p
    -> Map p SlotNo
joinedPeerMetricAtImpl PeerMetricsState { peerRegistry } =
    OrdPSQ.fold' (\p slotNo _ m -> Map.insert p slotNo m) Map.empty peerRegistry

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
upstreamynessImpl state@PeerMetricsState { headerMetrics,
                                           peerRegistry,
                                           lastSlotNo,
                                           metricsConfig
                                         } =
    Map.unionWith (+) (IntPSQ.fold' count Map.empty headerMetrics)
                      (OrdPSQ.fold' (countCorrection (firstSlotNo state))
                                    Map.empty peerRegistry)
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

    countCorrection :: Maybe SlotNo
                    -> p
                    -> SlotNo
                    -> AverageMetrics
                    -> Map p Int
                    -> Map p Int
    countCorrection minSlotNo peer joinedAt AverageMetrics { averageUpstreamyness } m =
        Map.insert peer
                   (adjustAvg metricsConfig
                              minSlotNo
                              joinedAt
                              lastSlotNo
                              averageUpstreamyness)
                   m


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
fetchynessBytesImpl state@PeerMetricsState { fetchedMetrics,
                                             peerRegistry,
                                             lastSlotNo,
                                             metricsConfig
                                           } =
    Map.unionWith (+) (IntPSQ.fold' count Map.empty fetchedMetrics)
                      (OrdPSQ.fold' (countCorrection (firstSlotNo state))
                                     Map.empty peerRegistry)
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

    countCorrection :: Maybe SlotNo
                    -> p
                    -> SlotNo
                    -> AverageMetrics
                    -> Map p Int
                    -> Map p Int
    countCorrection minSlotNo peer joinedAt AverageMetrics { averageFetchynessBytes } m =
        Map.insert peer
                   (adjustAvg metricsConfig
                              minSlotNo
                              joinedAt
                              lastSlotNo
                              averageFetchynessBytes)
                   m


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
fetchynessBlocksImpl state@PeerMetricsState { fetchedMetrics,
                                              peerRegistry,
                                              lastSlotNo,
                                              metricsConfig
                                            } =
    Map.unionWith (+) (IntPSQ.fold' count Map.empty fetchedMetrics)
                      (OrdPSQ.fold' (countCorrection (firstSlotNo state))
                                    Map.empty peerRegistry)
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

    countCorrection :: Maybe SlotNo
                    -> p
                    -> SlotNo
                    -> AverageMetrics
                    -> Map p Int
                    -> Map p Int
    countCorrection minSlotNo peer joinedAt AverageMetrics { averageFetchynessBlocks } m =
        Map.insert peer
                   (adjustAvg metricsConfig
                              minSlotNo
                              joinedAt
                              lastSlotNo
                              averageFetchynessBlocks)
                   m


--
-- Utils
--


adjustAvg :: PeerMetricsConfiguration
          -> Maybe SlotNo -- ^ smallest slot number
          -> SlotNo       -- ^ slot when joined the leader board
          -> SlotNo       -- ^ current slot
          -> Int
          -> Int
adjustAvg PeerMetricsConfiguration { maxEntriesToTrack } minSlotNo joinedSlotNo lastSlotNo avg
    -- when there are only a few results in the 'PeerMetricsState' we don't
    -- take into account the average.  This allows the system to start, without
    -- penalising the peers which we connected to early.
    | lastSlot - minSlot + 1 < maxEntriesToTrack `div` 2
    = 0

    -- the peer is too old to take the correction into account.
    | lastSlot - joinedSlot + 1 >= maxEntriesToTrack
    = 0

    | otherwise
    = (maxEntriesToTrack + joinedSlot - lastSlot) * avg
      `div` maxEntriesToTrack
  where
    minSlot, lastSlot, joinedSlot :: Int
    minSlot    = maybe 1 slotToInt minSlotNo
    lastSlot   = slotToInt lastSlotNo
    joinedSlot = slotToInt joinedSlotNo

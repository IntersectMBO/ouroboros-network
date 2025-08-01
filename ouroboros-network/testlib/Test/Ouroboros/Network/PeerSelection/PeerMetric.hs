{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE ScopedTypeVariables        #-}

#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-x-partial #-}
#endif
module Test.Ouroboros.Network.PeerSelection.PeerMetric where


import Control.Concurrent.Class.MonadSTM qualified as LazySTM
import Control.Concurrent.Class.MonadSTM.Strict
import Control.DeepSeq (NFData (..))
import Control.Monad (when)
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import Control.Tracer (Tracer (..), traceWith)

import Data.Foldable as Foldable (foldl', foldr')
import Data.List (sortOn)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Merge.Strict qualified as Map
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import Network.Mux.Trace (TraceLabelPeer (..))

import Ouroboros.Network.ConnectionId
import Ouroboros.Network.PeerSelection.PeerMetric (PeerMetrics,
           PeerMetricsConfiguration (..), ReportPeerMetrics (..),
           fetchynessBlocks, fetchynessBytes, joinedPeerMetricAt, newPeerMetric,
           reportMetric, upstreamyness)
import Ouroboros.Network.SizeInBytes

import Cardano.Slotting.Slot (SlotNo (..))

import Control.Monad.IOSim

import NoThunks.Class

import Test.Ouroboros.Network.Data.Script

import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)


tests :: TestTree
tests = testGroup "Ouroboros.Network.PeerSelection.PeerMetric"
  [ testProperty "insert peer invariant"       prop_insert_peer
  , testProperty "metrics results are bounded" prop_metrics_are_bounded
  , testProperty "size property"               prop_bounded_size
  ]



newtype TestAddress = TestAddress Int
  deriving stock   (Show, Eq, Ord)
  deriving newtype (NoThunks, NFData)

instance Arbitrary TestAddress where
    arbitrary = do
      size <- choose (0, 20)
      TestAddress . getPositive <$> resize size arbitrary
    shrink (TestAddress addr) =
      TestAddress . getPositive <$> shrink (Positive addr)

data Event =
    FetchedHeader TestAddress SlotNo
  | FetchedBlock  TestAddress SlotNo SizeInBytes
  deriving Show

eventPeer :: Event -> TestAddress
eventPeer (FetchedHeader peer _)   = peer
eventPeer (FetchedBlock  peer _ _) = peer

eventSlot :: Event -> SlotNo
eventSlot (FetchedHeader _ slotNo)   = slotNo
eventSlot (FetchedBlock  _ slotNo _) = slotNo

instance Arbitrary Event where
    arbitrary = oneof [ FetchedHeader <$> arbitrary
                                      <*> (SlotNo . getSmall . getPositive <$> arbitrary)
                      , FetchedBlock  <$> arbitrary
                                      <*> (SlotNo . getSmall . getPositive <$> arbitrary)
                                      <*> (SizeInBytes <$> (arbitrary `suchThat` \sizeInBytes -> 0 < sizeInBytes && sizeInBytes <= 2_000_000))
                      ]
    shrink  FetchedHeader {} = []
    shrink (FetchedBlock peer slotNo size) =
      [ FetchedBlock peer slotNo (SizeInBytes size')
      | size' <- shrink (getSizeInBytes size)
      , size' > 0
      ]


newtype FixedScript = FixedScript { getFixedScript :: Script Event }
  deriving Show

-- | Order events by 'SlotNo'
--
-- TODO: 'SizeInBytes' should be a function of 'SlotNo'
--
mkFixedScript :: Script Event -> FixedScript
mkFixedScript (Script events) = FixedScript
                              . Script
                              $ NonEmpty.sortWith
                                  eventSlot
                                  events

instance Arbitrary FixedScript where
    -- Generated scripts must be long enough. We ignore first 100 results, to
    -- avoid effects when the peer metrics has not enough data  and thus it is
    -- ignoring averages: 'Ouroboros.Network.PeerSelection.PeerMetric.adjustAvg'.
    arbitrary = mkFixedScript
            <$> resize 360 arbitrary
                `suchThat` \(Script as) -> NonEmpty.length as > 100
    shrink (FixedScript script) = mkFixedScript `map` shrink script


mkTimedScript :: FixedScript -> TimedScript Event
mkTimedScript = go . fmap (\a -> (a, eventSlot a)) . getFixedScript
  where
    go :: Script (Event, SlotNo) -> TimedScript Event
    go (Script script) = Script
                       . NonEmpty.fromList
                       . foldr' f []
                       $ zip events ((Just . snd) `map` tail events ++ [Nothing])
      where
        events = NonEmpty.toList script

    f :: ((Event, SlotNo), Maybe SlotNo)
      -> [(Event, ScriptDelay)]
      -> [(Event, ScriptDelay)]
    f ((event, slotNo), nextSlotNo) as =
      (event, Delay $ slotDiffTime slotNo nextSlotNo) : as

    slotToTime :: SlotNo -> Time
    slotToTime (SlotNo slotNo) = Time $ realToFrac slotNo -- each slot takes 1s

    slotDiffTime :: SlotNo -> Maybe SlotNo -> DiffTime
    slotDiffTime _slotNo Nothing           = 0
    slotDiffTime  slotNo (Just nextSlotNo) = slotToTime nextSlotNo
                                  `diffTime` slotToTime slotNo


data PeerMetricsTrace = PeerMetricsTrace {
      pmtPeer             :: TestAddress,
      pmtSlot             :: SlotNo,
      pmtUpstreamyness    :: Map TestAddress Int,
      pmtFetchynessBytes  :: Map TestAddress Int,
      pmtFetchynessBlocks :: Map TestAddress Int,
      pmtJoinedAt         :: Map TestAddress SlotNo
    }
  deriving Show

simulatePeerMetricScript
  :: forall m.
     ( MonadDelay m
     , MonadLabelledSTM m
     )
  => Tracer m PeerMetricsTrace
  -> PeerMetricsConfiguration
  -> FixedScript
  -> m ()
simulatePeerMetricScript tracer config script = do
      peerMetrics <- newPeerMetric config
      let reporter :: ReportPeerMetrics m (ConnectionId TestAddress)
          reporter = reportMetric config peerMetrics
      v <- initScript timedScript
      go v peerMetrics reporter
    where
      timedScript ::  TimedScript Event
      timedScript = mkTimedScript script

      go :: LazySTM.TVar m (TimedScript Event)
         -> PeerMetrics m TestAddress
         -> ReportPeerMetrics m (ConnectionId TestAddress)
         -> m ()
      go v peerMetrics reporter@ReportPeerMetrics { reportHeader, reportFetch } = do
        (continue, (ev, delay)) <- (\case Left  a -> (False, a)
                                          Right a -> (True,  a))
                               <$> stepScriptOrFinish v
        time <- getMonotonicTime
        peer <- case ev of
          FetchedHeader peer slotNo -> do
            atomically $ traceWith reportHeader
                       $ TraceLabelPeer ConnectionId {
                                            localAddress  = TestAddress 0,
                                            remoteAddress = peer
                                          }
                                        (slotNo, time)
            return peer

          FetchedBlock peer slotNo size -> do
            atomically $ traceWith reportFetch
                       $ TraceLabelPeer ConnectionId {
                                            localAddress  = TestAddress 0,
                                            remoteAddress = peer
                                          }
                                        (size, slotNo, time)
            return peer

        trace <- atomically $
           PeerMetricsTrace peer (eventSlot ev)
                 <$> upstreamyness      peerMetrics
                 <*> fetchynessBytes    peerMetrics
                 <*> fetchynessBlocks   peerMetrics
                 <*> joinedPeerMetricAt peerMetrics
        traceWith tracer trace

        when continue $ do
          threadDelay (interpretScriptDelay delay)
          go v peerMetrics reporter

interpretScriptDelay :: ScriptDelay -> DiffTime
interpretScriptDelay NoDelay       = 0
interpretScriptDelay ShortDelay    = 1
interpretScriptDelay LongDelay     = 3600
interpretScriptDelay (Delay delay) = delay


-- | Check that newly added peer is never in the 20% worst performing peers (if
-- there are at least 5 results).
--
prop_insert_peer :: FixedScript -> Property
prop_insert_peer script =
    label ("length: "
           ++ show (  len `div` band      * band
                   , (len `div` band + 1) * band  - 1
                   )) $
    label (case trace of
            [] -> "empty"
            _  -> "non-empty") $
               foldMap go
             $ zip (Nothing : Just `map` trace) trace
  where
    band = 50
    len = case getFixedScript script of Script as -> NonEmpty.length as

    config :: PeerMetricsConfiguration
    config = PeerMetricsConfiguration { maxEntriesToTrack = 180 }

    sim :: IOSim s ()
    sim = simulatePeerMetricScript (Tracer traceM) config script

    -- drop first 90 slots
    trace :: [PeerMetricsTrace]
    trace = dropWhile (\a -> pmtSlot a <= firstSlot + 90)
          $ selectTraceEventsDynamic (runSimTrace sim)
      where
        firstSlot = case script of
            FixedScript (Script (a :| _)) -> eventSlot a

    go :: (Maybe PeerMetricsTrace, PeerMetricsTrace)
       -> Every
    go (Nothing, _) = Every True
    go (Just prev, res@PeerMetricsTrace { pmtPeer             = peer,
                                          pmtUpstreamyness    = upstreamynessResults,
                                          pmtFetchynessBytes  = fetchynessBytesResults,
                                          pmtFetchynessBlocks = fetchynessBlocksResults,
                                          pmtJoinedAt         = joinedAtResults
                                        }) =
      if peer `Map.member` pmtUpstreamyness prev
      || peer `Map.member` pmtFetchynessBytes prev
      || peer `Map.member` pmtFetchynessBlocks prev
      then Every True
      else Every ( counterexample (show (res, prev))
                 $ checkResult "upstreamyness"    peer joinedAtResults upstreamynessResults)
        <> Every ( counterexample (show (res ,prev))
                 $ checkResult "fetchynessBytes"  peer joinedAtResults fetchynessBytesResults)
        <> Every ( counterexample (show (res, prev))
                 $ checkResult "fetchynessBlocks" peer joinedAtResults fetchynessBlocksResults)

    -- check that the peer is not in 20% worst peers, but only if there are more
    -- than 5 results.
    checkResult :: String
                -> TestAddress
                -> Map TestAddress SlotNo
                -> Map TestAddress Int
                -> Property
    checkResult name peer joinedAt m =
          (\peers -> counterexample (name ++ ": peer (" ++ show peer ++ ") member of "
                                          ++ show (peers, m'))
                                    (Set.size peers < 5 || Set.notMember peer peers))
        . Set.fromList
        . map fst
        . take (size `div` 5)
        . sortOn (snd :: (a, (Int, Maybe SlotNo)) -> (Int, Maybe SlotNo))
        . Map.toList
        $ m'
      where
        m' = Map.merge (Map.mapMissing (\_ a -> (a, Nothing)))
                        Map.dropMissing
                       (Map.zipWithMatched (\_ a b -> (a, Just b)))
                       m
                       joinedAt
        size = Map.size m

-- | Check that the results are always positive.
--
prop_metrics_are_bounded :: FixedScript -> Property
prop_metrics_are_bounded script =
    property $ foldMap go trace
  where
    config :: PeerMetricsConfiguration
    config = PeerMetricsConfiguration { maxEntriesToTrack = 180 }

    sim :: IOSim s ()
    sim = simulatePeerMetricScript (Tracer traceM) config script

    trace :: [PeerMetricsTrace]
    trace = selectTraceEventsDynamic (runSimTrace sim)

    safeMaximum :: Map a Int -> Int
    safeMaximum m | Map.null m = 0
    safeMaximum m = maximum m

    -- We bound each result by twice the maximum value, that's very
    -- conservative. Less conservative would be maximal value plus average of
    -- last `maxEntriesToTrack` results or so.
    bound :: Int
    bound =
        (2 *)
      . safeMaximum
      . Map.fromListWith (+)
      . foldr (\a as -> case a of
                  FetchedHeader peer _   -> (peer, 1) : as
                  FetchedBlock  peer _ _ -> (peer, 1) : as)
              []
      $ case getFixedScript script of
          Script as -> as

    fetchyness_bytes_bound :: Int
    fetchyness_bytes_bound =
        (2 *)
      . safeMaximum
      . fmap fromIntegral
      . Map.fromListWith (+)
      . foldr (\a as -> case a of
                  FetchedHeader {}          -> as
                  FetchedBlock peer _ bytes -> (peer, bytes) : as)
              []
      $ case getFixedScript script of
          Script as -> as


    go :: PeerMetricsTrace -> Every
    go PeerMetricsTrace { pmtUpstreamyness,
                          pmtFetchynessBytes,
                          pmtFetchynessBlocks
                        } =
         foldMap (\a -> Every
                      $ counterexample
                          (show ("upstreameness", a, bound, pmtUpstreamyness))
                          (a >= 0))
                 pmtUpstreamyness
      <> foldMap (\a -> Every
                      $ counterexample
                          (show ("fetchynessBytes", a, fetchyness_bytes_bound, pmtFetchynessBytes))
                          (a >= 0 && a <= fetchyness_bytes_bound))
                 pmtFetchynessBytes
      <> foldMap (\a -> Every
                      $ counterexample
                          (show ("fetchynessBlocks", a, bound))
                          (a >= 0))
                 pmtFetchynessBlocks


-- | Check that the result are bounded.
--
-- The bound is 'maxEntriesToTrack' times number of peers in the simulation.
-- This could be lowered by computing number of peers in each
-- 'maxEntriesToTrack' slots window.
--
prop_bounded_size :: Positive Int -> FixedScript -> Property
prop_bounded_size (Positive maxEntriesToTrack) script =
    property $ foldMap go trace
  where
    config :: PeerMetricsConfiguration
    config = PeerMetricsConfiguration { maxEntriesToTrack }

    sim :: IOSim s ()
    sim = simulatePeerMetricScript (Tracer traceM) config script

    trace :: [PeerMetricsTrace]
    trace = selectTraceEventsDynamic (runSimTrace sim)

    number_of_peers :: Int
    number_of_peers = Set.size
                    . Set.fromList
                    . Foldable.foldl' (\as a -> eventPeer a : as) []
                    $ case getFixedScript script of
                        Script as -> as

    bound :: Int
    bound = maxEntriesToTrack * number_of_peers

    go :: PeerMetricsTrace -> Every
    go PeerMetricsTrace {
           pmtUpstreamyness,
           pmtFetchynessBytes,
           pmtFetchynessBlocks
         } = Every ( counterexample
                     (    "upstreamyness: "
                       ++ show (Map.size pmtUpstreamyness)
                       ++ " ≰ "
                       ++ show maxEntriesToTrack )
                     ( Map.size pmtUpstreamyness <= bound )
                   )
          <> Every ( counterexample
                     (    "fetchynessBytes: "
                       ++ show (Map.size pmtFetchynessBytes)
                       ++ " ≰ "
                       ++ show maxEntriesToTrack)
                     ( Map.size pmtFetchynessBytes <= bound )
                   )
          <> Every ( counterexample
                     (    "fetchynessBlocks: "
                       ++ show (Map.size pmtFetchynessBlocks)
                       ++ " ≰ "
                       ++ show maxEntriesToTrack)
                     ( Map.size pmtFetchynessBlocks <= bound )
                   )

--
-- The following are focused on creating micro-benchmarks
-- (rather than property testing):
--

-- | microbenchmark1 n - one test of simple property on a FixedScript of length n:
--   one input, one property, one test.
--
-- We split into generating input and running/checking it so we can
-- more accurately measure the latter.

microbenchmark1GenerateInput :: Bool -> Int -> IO FixedScript
microbenchmark1GenerateInput verbose' n = do
  es <- generate (vector n)
  let fixedScript = mkFixedScript (Script (NonEmpty.fromList es))
  when verbose' $
    mapM_ print (let FixedScript s = fixedScript in s)
  return fixedScript

microbenchmark1ProcessInput :: FixedScript -> IO ()
microbenchmark1ProcessInput =
  quickCheckWith (stdArgs{maxSuccess=1}) . prop_simScript

microbenchmark1 :: Bool -> Int -> IO ()
microbenchmark1 verbose' n =
  microbenchmark1GenerateInput verbose' n >>= microbenchmark1ProcessInput


-- | one simple property (pmtUpstreamyness >= 0) checked on the trace of a script:
prop_simScript :: FixedScript -> Property
prop_simScript script =
    property $ go $ last trace
  where
    config :: PeerMetricsConfiguration
    config = PeerMetricsConfiguration { maxEntriesToTrack = 500 }

    sim :: IOSim s ()
    sim = simulatePeerMetricScriptWithoutDelays (Tracer traceM) config script

    trace :: [PeerMetricsTrace]
    trace = selectTraceEventsDynamic (runSimTrace sim)

    go :: PeerMetricsTrace -> Every
    go PeerMetricsTrace { pmtUpstreamyness,
                          pmtFetchynessBytes=_,
                          pmtFetchynessBlocks=_
                        } =
         foldMap (\a -> Every
                      $ counterexample
                          (show ("upstreamyness", a, pmtUpstreamyness))
                          (a >= 0))
                 pmtUpstreamyness

-- | similar to 'simulatePeerMetricScript': but we don't do in
-- "real/simulated" time: not calling getMonotonicTime and
-- threadDelay.
simulatePeerMetricScriptWithoutDelays
  :: forall m.
     ( MonadLabelledSTM m )
  => Tracer m PeerMetricsTrace
  -> PeerMetricsConfiguration
  -> FixedScript
  -> m ()
simulatePeerMetricScriptWithoutDelays tracer config script = do
      peerMetrics <- newPeerMetric config
      let reporter :: ReportPeerMetrics m (ConnectionId TestAddress)
          reporter = reportMetric config peerMetrics
      v <- initScript timedScript
      go v peerMetrics reporter (Time 0)
    where
      timedScript ::  TimedScript Event
      timedScript = mkTimedScript script

      go :: LazySTM.TVar m (TimedScript Event)
         -> PeerMetrics m TestAddress
         -> ReportPeerMetrics m (ConnectionId TestAddress)
         -> Time
         -> m ()
      go v peerMetrics reporter@ReportPeerMetrics { reportHeader, reportFetch } time = do
        (continue, (ev, delay)) <- (\case Left  a -> (False, a)
                                          Right a -> (True,  a))
                               <$> stepScriptOrFinish v
        peer <- case ev of
          FetchedHeader peer slotNo -> do
            atomically $ traceWith reportHeader
                       $ TraceLabelPeer ConnectionId {
                                            localAddress  = TestAddress 0,
                                            remoteAddress = peer
                                          }
                                        (slotNo, time)
            return peer

          FetchedBlock peer slotNo size -> do
            atomically $ traceWith reportFetch
                       $ TraceLabelPeer ConnectionId {
                                            localAddress  = TestAddress 0,
                                            remoteAddress = peer
                                          }
                                        (size, slotNo, time)
            return peer

        trace <- atomically $
           PeerMetricsTrace peer (eventSlot ev)
                 <$> upstreamyness      peerMetrics
                 <*> fetchynessBytes    peerMetrics
                 <*> fetchynessBlocks   peerMetrics
                 <*> joinedPeerMetricAt peerMetrics
        traceWith tracer trace

        when continue $
          go v peerMetrics reporter (interpretScriptDelay delay `addTime` time)

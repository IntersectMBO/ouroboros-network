{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}


{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Ouroboros.Network.PeerSelection (tests) where

import qualified Data.ByteString.Char8 as BS
import           Data.Function (on)
import           Data.List (groupBy, foldl')
import           Data.Maybe (listToMaybe, isNothing, fromMaybe)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Void (Void)

import qualified Data.Signal as Signal
import           Data.Signal (Signal, Events, E(E), TS(TS))
import qualified Data.OrdPSQ as PSQ

import           Control.Monad.Class.MonadTime
import           Control.Tracer (Tracer (..))

import qualified Network.DNS as DNS (defaultResolvConf)
import           Network.Socket (SockAddr)
import           Network.Mux.Timeout

import           Ouroboros.Network.PeerSelection.Governor hiding
                     (PeerSelectionState (..))
import qualified Ouroboros.Network.PeerSelection.Governor as Governor
import qualified Ouroboros.Network.PeerSelection.KnownPeers as KnownPeers
import qualified Ouroboros.Network.PeerSelection.LocalRootPeers as LocalRootPeers
import qualified Ouroboros.Network.PeerSelection.EstablishedPeers as EstablishedPeers
import           Ouroboros.Network.PeerSelection.RootPeersDNS

import           Test.Ouroboros.Network.PeerSelection.Instances
import qualified Test.Ouroboros.Network.PeerSelection.LocalRootPeers
import           Test.Ouroboros.Network.PeerSelection.MockEnvironment hiding (tests)
import qualified Test.Ouroboros.Network.PeerSelection.MockEnvironment
import           Test.Ouroboros.Network.PeerSelection.PeerGraph

import           Test.QuickCheck
import           Test.QuickCheck.Signal
import           Test.Tasty (TestTree, testGroup, after, DependencyType(..))
import           Test.Tasty.QuickCheck (testProperty)


tests :: TestTree
tests =
  testGroup "Ouroboros.Network.PeerSelection"
  [ Test.Ouroboros.Network.PeerSelection.LocalRootPeers.tests
  , Test.Ouroboros.Network.PeerSelection.MockEnvironment.tests
  , testGroup "basic"
    [ testProperty "has output"         prop_governor_hasoutput
    , testProperty "no failure"         prop_governor_nofail
    , testProperty "no livelock"        prop_governor_nolivelock
    ]

    -- The no livelock property is needed to ensure other tests terminate
  , after AllSucceed "Ouroboros.Network.PeerSelection.basic" $
    testGroup "safety"
    [ testProperty "no excess busyness" prop_governor_nobusyness
    , testProperty "event coverage"     prop_governor_trace_coverage
    , testProperty "connection status"  prop_governor_connstatus
    ]

    -- The no livelock property is needed to ensure other tests terminate
  , after AllSucceed "Ouroboros.Network.PeerSelection.basic" $
    testGroup "progress"
    [ testProperty "gossip reachable"    prop_governor_gossip_1hr

--  , testProperty "progresses towards public root peers target (from below)"
--                 prop_governor_target_publicroots

    , testProperty "progresses towards known peers target (from below)"
                   prop_governor_target_known_below
    , testProperty "progresses towards known peers target (from above)"
                   prop_governor_target_known_above

    , testProperty "progresses towards established peers target (from below)"
                   prop_governor_target_established_below
    , testProperty "progresses towards established peers target (from above)"
                   prop_governor_target_established_above

    , testProperty "progresses towards active peers target (from below)"
                   prop_governor_target_active_below
    , testProperty "progresses towards active peers target (from above)"
                   prop_governor_target_active_above
    ]
  ]
  --TODO: We should add separate properties to check that we do not overshoot
  -- our targets: known peers from below can overshoot, but all the others
  -- should be precise and not overshoot. The public root target from below
  -- is a one-sided target and we can and will overshoot, but we should not
  -- overshoot by too much.


--
-- QuickCheck properties
--

-- We start with basic properties in the style of "never does bad things"
-- and progress to properties that check that it "eventually does good things".
--
-- In the "never does bad things" category we have:
--
-- * A basic state space exploration property that checks we don't encounter
--   internal errors. This includes some limited checking that we get adequate
--   coverage of the different actions, by looking for coverage of all the
--   trace events. The coverage checks here are useful to give us confidence
--   about coverage for some of the other properties.
--
-- * A no-livelock property. This checks that the governor does not get stuck
--   doing too many steps at a single moment in (virtual) time. It's quite easy
--   to write bugs that don't cause the governor to fail, but cause it to go
--   into a busy cycle. See also the "no excessive busyness" property for a
--   more advanced version.
--
-- * A "no excessive busyness" property. This checks that the governor does not
--   remain too busy for too long. It's quite easy to write bugs that don't
--   cause the governor to fail, but cause it to go into fairly-busy cycles.
--
-- * A state consistency property that the governor's view of part of the state
--   and the "true" state of the mock environment are maintained in an
--   appropriate correspondence.
--
-- In the "eventually does good things" category we have:
--
-- * A basic property to check the governor does produce non-trivial traces.
--
-- * A cold peer gossip "reachable" property: that the governor either hits
--   its target for the number of cold peers, or finds all the reachable peers.
--
-- * A known peer target progress property: that the governor makes progress
--   within a bounded time towards its known peers target, from below and above.
--
-- * An established peer target property: the same as above but for established
--   peers.
--
-- * An active peer target property: the same as above but for active peers.
--
-- Properties that we would like to have:
--
-- * A public root peers target property: that the governor hits its target for
--   for the number of public root peers (or as near as possible), and does
--   not "grossly" overshoot. Since the public roots is a one sided target, but
--   we don't want to overshoot excessively.
--
-- * A local root peers target property: that the governor hits its target for
--   getting all its local root peers into the established state, and a target
--   number of them into the active state (or as near as possible).
--
-- Other properties we might like to think about
--
-- * time to find new nodes after a graph change is ok
-- * targets or root peer set dynamic


-- | As the most basic property we run the governor and check that it produces
-- any trace output at all. It should elicit some activity, unless the test
-- environment is actually empty.
--
prop_governor_hasoutput :: GovernorMockEnvironment -> Bool
prop_governor_hasoutput env =
    let trace = selectPeerSelectionTraceEvents $
                  runGovernorInMockEnvironment env

     in hasOutput env (selectGovernorEvents trace)

hasOutput :: GovernorMockEnvironment
          -> [(Time, TracePeerSelection PeerAddr)]
          -> Bool
hasOutput _   (_:_) = True
hasOutput env []    = isEmptyEnv env

isEmptyEnv :: GovernorMockEnvironment -> Bool
isEmptyEnv GovernorMockEnvironment {
             localRootPeers,
             publicRootPeers,
             targets
           } =
    (LocalRootPeers.null localRootPeers
      || all (\(t,_) -> targetNumberOfKnownPeers t == 0) targets)
 && (Set.null publicRootPeers
      || all (\(t,_) -> targetNumberOfRootPeers  t == 0) targets)


-- | As a basic property we run the governor to explore its state space a bit
-- and check it does not throw any exceptions (assertions such as invariant
-- violations).
--
-- We do /not/ assume freedom from livelock for this property, so we run the
-- governor for a maximum number of trace events rather than for a fixed
-- simulated time.
--
prop_governor_nofail :: GovernorMockEnvironment -> Bool
prop_governor_nofail env =
    let trace = take 5000 .
                selectPeerSelectionTraceEvents $
                  runGovernorInMockEnvironment env

     in foldl' (flip seq) True
          [ assertPeerSelectionState st ()
          | (_, GovernorDebug (TraceGovernorState _ _ st)) <- trace ]


-- | It is relatively easy to write bugs where the governor is stuck in a tight
-- cycle of continuous activity. Due to the way the I\/O simulator manages
-- virtual time, these bugs exhibits themselves by infinite trace activity
-- without time advancing.
--
-- It is important to catch these bugs early in the set of tests, since it is
-- hard to write many of the other more interesting properties if there are
-- these kinds of livelock bugs. Or to put it another way, the other properties
-- can be expressed more simply if they can assume within event traces that the
-- time always advances after some finite number of events.
--
prop_governor_nolivelock :: GovernorMockEnvironment -> Property
prop_governor_nolivelock env =
    let trace = take 5000 .
                selectGovernorEvents .
                selectPeerSelectionTraceEvents $
                  runGovernorInMockEnvironment env
     in case tooManyEventsBeforeTimeAdvances 1000 trace of
          Nothing -> property True
          Just (t, es) ->
            counterexample
              ("over 1000 events at time: " ++ show t ++ "\n" ++
               "first 50 events: " ++ (unlines . map show . take 50 $ es)) $
            property False


-- | Scan the trace and return any occurrence where we have at least threshold
-- events before virtual time moves on. Return the tail of the trace from that
-- point on.
--
tooManyEventsBeforeTimeAdvances :: Int -> [(Time, e)] -> Maybe (Time, [e])
tooManyEventsBeforeTimeAdvances _         []     = Nothing
tooManyEventsBeforeTimeAdvances threshold trace0 =
    go [ (t, diffTime t' t, e)
       | ((t, e), (t', _)) <- zip trace0 (tail trace0) ]
  where
    go []                = Nothing
    go trace@((t,_,_):_) = case countdown threshold trace of
                             Just es' -> go es'
                             Nothing  -> Just (t, trace')
                               where
                                 trace' = take threshold [ e | (_,_,e) <- trace ]

    countdown 0 _  = Nothing
    countdown _ [] = Just []
    countdown n ((_t,dt,_e):es)
      | dt == 0    = countdown (n-1) es
      | otherwise  = Just es


-- | It is easy to get bugs where the governor is stuck in a cycle, working but
-- not making progress. This kind of bug would result in the governor thread
-- being excessively busy, so it might not be easily noticed.
--
-- This is more subtle and general than a simple livelock test that just checks
-- we don't get completely stuck. This property is about the possibility that
-- the governor is excessively busy over some period of time. This includes
-- "slow" livelocks where time advances during some of the steps in the cycle.
-- More interestingly this is also about a failure to converge and return to
-- being idle sufficiently quickly.
--
-- For example the governor could gets stuck in a cycle promoting and demoting
-- a peer once a second. In such a failure mode it will have a continuous level
-- of activity and will not return to being idle (perhaps ever or perhaps for
-- an extended period until some other environment perturbation gets us out of
-- the cycle).
--
-- The approach we take is based on the observation that the governor can
-- (quite reasonably) start with a big burst of activity (e.g. as it gossips
-- to discover a big graph) but that in the long term it settles down and only
-- has small bursts of activity in reaction to perturbations in the environment
-- such as failures or changes in targets.
--
-- The approach we take is to look at spans of busy activity followed by
-- periods of idleness. If the spans of busy activity are too long then we
-- fail. So this counts the time of busyness not the number of events. We
-- account for activity in the environment that the governor needs to respond
-- to by counting \"perturbation credits"\: more credits means we allow longer
-- spans of busyness.
--
-- More specifically: we look at runs of events where the time between events
-- is less than a threshold. This implies there follows a threshold level of
-- idleness. Starting or within that run of events there can be environment
-- events. These are the perturbations from the environment that we expect to
-- trigger a series of responses from the governor. So we expect longer periods
-- of business for bigger perturbations. We sum all the perturbations credits
-- included in a run of events. We use a formula that relates the credits to
-- the permitted time span of the run. If the run is within the permitted time
-- span then it is ok, otherwise it is a failure (and the run is the
-- counterexample).
--
-- TODO: This test uses static root peers, but we should move to dynamic root
-- peers.
--
prop_governor_nobusyness :: GovernorMockEnvironment -> Property
prop_governor_nobusyness env =
    let trace = selectPeerSelectionTraceEvents $
                  runGovernorInMockEnvironment env

     in case tooBusyForTooLong (takeFirstNHours 10 trace) of
          Nothing -> property True
          Just (busyStartTime, busyEndTime, credits, trace') ->
            counterexample
              ("busy span too long\n" ++
               "start time:   "     ++ show busyStartTime ++ "\n" ++
               "end time:     "     ++ show busyEndTime ++ "\n" ++
               "span credits: "     ++ show credits ++ "\n" ++
               "first 50 events:\n" ++
                 (unlines . map show . take 50 $ trace')) $
            property False

--
tooBusyForTooLong :: [(Time, TestTraceEvent)]
                  -> Maybe (Time, Time, DiffTime,
                            [(Time, TestTraceEvent)])
tooBusyForTooLong trace0 =
    -- Pass in each timed event, with the diff-time to the next event
    idle [ (t, diffTime t' t, e)
         | ((t, e), (t', _)) <- zip trace0 (tail trace0) ]
  where
    -- How long between events before we say it's the end of a busy span
    sameSpanThreshold :: DiffTime
    sameSpanThreshold = 45

    -- Starting credits for a busy span, even if there are no triggering
    -- environment events. The value chosen here takes account of the normal
    -- exponential backoff is 2+4+8+16+32 = 62, before a gap of 64 that's
    -- bigger than the sameSpanThreshold of 45.
    initialEventCredits :: DiffTime
    initialEventCredits = 65

    -- We process the event trace linearly, flipping between two states: idle
    -- and busy. In the idle state, the next (non-debug) event flips us into
    -- the busy state, starting with some minimal initial credits.

    idle :: [(Time, DiffTime, TestTraceEvent)]
         -> Maybe (Time, Time, DiffTime, [(Time, TestTraceEvent)])
    idle [] = Nothing
    idle ((_, _, GovernorDebug{}):trace') = idle trace'
    idle trace@((busyStartTime,_,_):_) =
      case busy busyStartTime initialEventCredits trace of
        Right trace' -> idle trace'
        Left (busyEndTime, credits) ->
          Just (busyStartTime, busyEndTime, credits, trace')
            where
              trace' = [ (t, e)
                       | (t,_dt, e) <-
                           takeWhile (\(t,_,_) -> t <= busyEndTime) trace
                       , case e of
                           GovernorDebug{} -> False
                           _               -> True
                       ]

    busy :: Time -> DiffTime -> [(Time, DiffTime, TestTraceEvent)]
         -> Either (Time, DiffTime) [(Time, DiffTime, TestTraceEvent)]

    -- For normal governor events we check if the length of the busy time span
    -- is now too big (adjusted for any perturbation credits). If so we've
    -- found a violation.
    busy !busyStartTime !credits ((busyEndTime, _dt, GovernorEvent{}) : _trace')
      | busySpanLength > credits = Left (busyEndTime, credits)
      where
        busySpanLength = diffTime busyEndTime busyStartTime

    -- We also look at how long it is to the next event to see if this is the
    -- last event in the busy span, and if so we return to idle.
    busy !_busyStartTime !_credits ((_t, dt, _event) : trace')
      | dt > sameSpanThreshold = Right trace'

    -- For environment events we calculate the perturbation credits this
    -- contributes and add it to our running total.
    busy !busyStartTime !credits ((_, _, MockEnvEvent e) : trace') =
      busy busyStartTime (credits + fromIntegral (envEventCredits e)) trace'

    -- Otherwise we move on to the next event, updating the length of this busy
    -- time span.
    busy !busyStartTime !credits (_ : trace') =
      busy busyStartTime credits trace'

    -- running out of events before we find a violation is ok
    busy !_ !_ [] = Right []


envEventCredits :: TraceMockEnv -> Int
envEventCredits (TraceEnvAddPeers peerGraph) = 80 * 5 + length adjacency * 5
                   where
                     PeerGraph adjacency = peerGraph

envEventCredits (TraceEnvSetLocalRoots  peers) = LocalRootPeers.size peers
envEventCredits (TraceEnvSetPublicRoots peers) = Set.size peers
envEventCredits  TraceEnvPublicRootTTL         = 60
envEventCredits (TraceEnvGossipTTL _)          = 30

envEventCredits (TraceEnvSetTargets PeerSelectionTargets {
                   targetNumberOfRootPeers = _,
                   targetNumberOfKnownPeers,
                   targetNumberOfEstablishedPeers,
                   targetNumberOfActivePeers
                 }) = 80
                    + 10 * (targetNumberOfKnownPeers
                          + targetNumberOfEstablishedPeers
                          + targetNumberOfActivePeers)

envEventCredits (TraceEnvPeersDemote Noop   _) = 10
envEventCredits (TraceEnvPeersDemote ToWarm _) = 30
envEventCredits (TraceEnvPeersDemote ToCold _) = 30

envEventCredits  TraceEnvPeersStatus{}         = 0
-- These events are visible in the environment but are the result of actions
-- initiated by the governor, hence the get no credit.
envEventCredits  TraceEnvRootsResult{}         = 0
envEventCredits  TraceEnvGossipRequest{}       = 0
envEventCredits  TraceEnvGossipResult{}        = 0



-- | A coverage property, much like 'prop_governor_nofail' but we look to see
-- that we get adequate coverage of the state space. We look for all the trace
-- events that the governor can produce, and tabules which ones we see.
--
prop_governor_trace_coverage :: GovernorMockEnvironment -> Property
prop_governor_trace_coverage env =
    let trace = take 5000 .
                selectPeerSelectionTraceEvents $
                  runGovernorInMockEnvironment env

        traceNumsSeen  = collectTraces trace
        traceNamesSeen = allTraceNames `Map.restrictKeys` traceNumsSeen

     in coverTable "trace events" [ (n, 1) | n <- Map.elems allTraceNames ] $
        tabulate   "trace events" (Map.elems traceNamesSeen)
        True
        --TODO: use cover to check we do indeed get them all. There are a few
        -- cases we do not cover yet. These should be fixed first.

collectTraces :: [(Time, TestTraceEvent)] -> Set Int
collectTraces trace =
    Set.fromList [ traceNum e | (_, GovernorEvent e) <- trace ]

traceNum :: TracePeerSelection peeraddr -> Int
traceNum TraceLocalRootPeersChanged{} = 00
traceNum TraceTargetsChanged{}        = 01
traceNum TracePublicRootsRequest{}    = 02
traceNum TracePublicRootsResults{}    = 03
traceNum TracePublicRootsFailure{}    = 04
traceNum TraceGossipRequests{}        = 05
traceNum TraceGossipResults{}         = 06
traceNum TraceForgetColdPeers{}       = 07
traceNum TracePromoteColdPeers{}      = 08
--traceNum TracePromoteColdLocalPeers{} = 09
traceNum TracePromoteColdFailed{}     = 10
traceNum TracePromoteColdDone{}       = 11
traceNum TracePromoteWarmPeers{}      = 12
--traceNum TracePromoteWarmLocalPeers{} = 13
traceNum TracePromoteWarmFailed{}     = 14
traceNum TracePromoteWarmDone{}       = 15
traceNum TraceDemoteWarmPeers{}       = 16
traceNum TraceDemoteWarmFailed{}      = 17
traceNum TraceDemoteWarmDone{}        = 18
traceNum TraceDemoteHotPeers{}        = 19
traceNum TraceDemoteHotFailed{}       = 20
traceNum TraceDemoteHotDone{}         = 21
traceNum TraceDemoteAsynchronous{}    = 22
traceNum TraceGovernorWakeup{}        = 23
traceNum TraceChurnWait{}             = 24

allTraceNames :: Map Int String
allTraceNames =
  Map.fromList
   [ (00, "TraceLocalRootPeersChanged")
   , (01, "TraceTargetsChanged")
   , (02, "TracePublicRootsRequest")
   , (03, "TracePublicRootsResults")
   , (04, "TracePublicRootsFailure")
   , (05, "TraceGossipRequests")
   , (06, "TraceGossipResults")
   , (07, "TraceForgetColdPeers")
   , (08, "TracePromoteColdPeers")
-- , (09, "TracePromoteColdLocalPeers")
   , (10, "TracePromoteColdFailed")
   , (11, "TracePromoteColdDone")
   , (12, "TracePromoteWarmPeers")
-- , (13, "TracePromoteWarmLocalPeers")
   , (14, "TracePromoteWarmFailed")
   , (15, "TracePromoteWarmDone")
   , (16, "TraceDemoteWarmPeers")
   , (17, "TraceDemoteWarmFailed")
   , (18, "TraceDemoteWarmDone")
   , (19, "TraceDemoteHotPeers")
   , (20, "TraceDemoteHotFailed")
   , (21, "TraceDemoteHotDone")
   , (22, "TraceDemoteAsynchronous")
   , (23, "TraceGovernorWakeup")
   , (24, "TraceChurnWait")
   ]


-- | Run the governor for up to 1 hour (simulated obviously) and look at the
-- set of known peers it has selected. This uses static targets and root peers.
--
-- As a basic correctness property, the peers the governor selects must be a
-- subset of those that are in principle reachable in the mock network
-- environment.
--
-- More interestingly, we expect the governor to find enough peers. Either it
-- must find all the reachable ones, or if the target for the number of known
-- peers to find is too low then it should at least find the target number.
--
prop_governor_gossip_1hr :: GovernorMockEnvironment -> Property
prop_governor_gossip_1hr env@GovernorMockEnvironment {
                               peerGraph,
                               localRootPeers,
                               publicRootPeers,
                               targets
                             } =
    let trace      = selectPeerSelectionTraceEvents $
                       runGovernorInMockEnvironment env {
                         targets = singletonScript (targets', NoDelay)
                       }
        Just found = knownPeersAfter1Hour trace
        reachable  = firstGossipReachablePeers peerGraph
                       (LocalRootPeers.keysSet localRootPeers <> publicRootPeers)
     in subsetProperty    found reachable
   .&&. bigEnoughProperty found reachable
  where
    -- This test is only about testing gossiping,
    -- so do not try to establish connections:
    targets' :: PeerSelectionTargets
    targets' = (fst (scriptHead targets)) {
                 targetNumberOfEstablishedPeers = 0,
                 targetNumberOfActivePeers      = 0
               }

    knownPeersAfter1Hour :: [(Time, TestTraceEvent)] -> Maybe (Set PeerAddr)
    knownPeersAfter1Hour trace =
      listToMaybe
        [ KnownPeers.toSet (Governor.knownPeers st)
        | (_, GovernorDebug (TraceGovernorState _ _ st))
            <- reverse (takeFirstNHours 1 trace)
        ]

    -- The ones we find should be a subset of the ones possible to find
    subsetProperty found reachable =
      counterexample ("reachable: " ++ show reachable ++ "\n" ++
                      "found:     " ++ show found) $
      property (found `Set.isSubsetOf` reachable)

    -- We expect to find enough of them, either the target number or the
    -- maximum reachable.
    bigEnoughProperty found reachable
        -- But there's an awkward corner case: if the number of public roots
        -- available is bigger than the target then we will likely not get
        -- all the roots (but which subset we get is random), but if we don't
        -- get all the roots then the set of peers actually reachable is
        -- incomplete, so we cannot expect to reach the usual target.
        --
        -- But we can at least expect to hit the target for root peers.
      | Set.size publicRootPeers >  targetNumberOfRootPeers targets'
      = property (Set.size found >= targetNumberOfRootPeers targets')

      | otherwise
      = counterexample ("reachable : " ++ show reachable ++ "\n" ++
                        "found     : " ++ show found ++ "\n" ++
                        "found #   : " ++ show (Set.size found) ++ "\n" ++
                        "expected #: " ++ show expected) $
        property (Set.size found == expected)
      where
        expected = Set.size reachable `min` targetNumberOfKnownPeers targets'


-- | Check the governor's view of connection status does not lag behind reality
-- by too much.
--
prop_governor_connstatus :: GovernorMockEnvironment -> Property
prop_governor_connstatus env =
    let trace = takeFirstNHours 1
              . selectPeerSelectionTraceEvents $
                  runGovernorInMockEnvironment env
        --TODO: check any actually get a true status output and try some deliberate bugs
     in conjoin (map ok (groupBy ((==) `on` fst) trace))
  where
    -- We look at events when the environment's view of the state of all the
    -- peer connections changed, and check that before simulated time advances
    -- the governor's view of the same state was brought in sync.
    --
    -- We do that by finding the env events and then looking for the last
    -- governor state event before time moves on.
    ok :: [(Time, TestTraceEvent)] -> Property
    ok trace =
        counterexample ("last few events:\n" ++ (unlines . map show) trace) $
        case (lastEnvStatus, lastGovStatus) of
          (Nothing, _)                     -> property True
          (Just envStatus, Just govStatus) -> envStatus === govStatus
          (Just envStatus, Nothing)        -> envStatus === Map.empty
      where
        lastEnvStatus =
          listToMaybe
            [ Map.filter (not . isPeerCold) status
            | (_, MockEnvEvent (TraceEnvPeersStatus status)) <- reverse trace ]

        isPeerCold PeerCold = True
        isPeerCold _        = False

        lastGovStatus =
          listToMaybe
            [ Governor.establishedPeersStatus st
            | (_, GovernorDebug (TraceGovernorState _ _ st)) <- reverse trace ]


--
-- Progress properties
--

-- | The main progress property for known peers: that we make progress towards
-- the target for known peers from below. See 'prop_governor_target_known_above'
-- for the (simpler) corresponding property for hitting the target from above.
--
-- Intuitively the property we want is that the governor either hits its target
-- for the number of known peers, or gets as close as reasonably possible. The
-- environment may be such that it prevents the governor from reaching its
-- target, e.g. because the target is too high, or not all peers may be
-- reachable by the gossip graph.
--
-- We approach this property as the conjunction of several simpler properties.
-- We take this approach for three main reasons.
--
-- 1. Firstly modularity help us break down a complex problem into simpler
--    problems. Overall this progress idea turns out to be quite subtle and
--    tricky to express precisely in a way that actually works.
-- 2. Secondly, modularity can give us opportunities to reuse code in other
--    properties and we want to have progress properties for all the governor
--    targets.
-- 3. Thirdly, it turns out to be hard to dictate in a universal way precisely
--    what the governor can be expected to do. It is simpler to specify looser
--    constraints on what it must and must not do. We can then argue informally
--    that the combination of properties must lead to the kinds of outcomes we
--    intend.
--
-- We decompose the progress property into the following (informally stated)
-- properties:
--
-- 1. The set of peers the governor knows about is a subset of the peers the
--    environment has told the governor about.
--
--    This is a weak property since it simply says that the governor does not
--    invent things out of thin air. We might expect that we could strengthen
--    this property to require that the subset be maximal in some sense however
--    such a property is violated by dynamic targets. There are also timing
--    issues which would complicate such a strengthened property: the governor
--    has legitimate reasons to update its internal state some time after the
--    environment informs it about new peers.
--
-- 2. If the governor is below target and has the opportunity to gossip then
--    within a bounded time it should perform a gossip with one of its known
--    peers.
--
--    This is the primary progress property. It is a relatively weak property:
--    we do not require that progress is actually made, just that opportunities
--    for progress are taken when available. We cannot always demand actual
--    progress since there are environments where it is not possible to make
--    progress, even though opportunities for gossip remain available. Examples
--    include environments where the total set of peers in the graph is less
--    than the target for known peers.
--
-- 3. The governor should not gossip too frequently with any individual peer,
--    except when the governor forgets known peers.
--
--    This is both useful in its own right, but it also helps to strengthen the
--    primary property by helping to ensure that the choices of which peers to
--    gossip with are reasonable. In the primary property we do not require that
--    the peer the  the governor chooses to gossip with is one of the
--    opportunities as defined by the property. We do not require this because
--    the set of opportunities is a lower bound not an upper bound, and trying
--    to make it a tight bound becomes complex and over-specifies behaviour.
--    There is the danger however that the governor could appear to try to make
--    progress by gossiping but always picking useless choices that avoid making
--    actual progress. By requiring that the governor not gossip with any
--    individual peer too often we can shrink the set of peers the governor can
--    choose and thus force the governor to eventually pick other peers to
--    gossip with, which should mean the governor eventually picks peers that
--    can enable progress.
--
-- 4. When the governor does perform a gossip, within a bounded time it should
--    include the results into its known peer set, or the known peer set should
--    reach its target size.
--
--    This helps to strengthen the primary progress property by ensuring the
--    results of gossip are used to make progress when that is possible.
--
-- 5. The governor should not shrink its known peer set except when it is above
--    the target size.
--
--    This also helps to strengthen the second property by ensuring monotonic
--    progress, except when we overshoot targets or when targets are reduced.
--
-- The overall progress argument is then an semi-formal argument, structured
-- much like classic proofs about loops. A classic loop proof has two parts: 1.
-- if the loop does terminate it gets the right result, and 2. it must
-- eventually terminate because it makes progress in some measure that is
-- bounded.
--
-- Of course in our setting there is no termination, but we can reach a goal
-- and remain in a steady state until the environment changes. Our argument is
-- that the governor makes progress to increase the size of its set of known
-- peers until either it hits its target number of known peers, or it reaches a
-- maximal possible set. As a corollary if the targets do not change too
-- frequently then it will eventually hit the target or reach a maximal set.
--
-- Property number 1 above tells us that if we do reach our goal condition that
-- we will have a correct result, as property 1 tells us that all the governors'
-- known peers are ones supplied by the environment.
--
-- Progress from below relies on the combination of property 2, 3, 4 and 5.
-- Property 2 tells us that we eventually do some gossip with some peer, but
-- does not by itself establish that we make progress in a bounded measure.
-- Property 3 gives us the bounded measure. Property 3 gives us a set of peers
-- that we have not gossiped with recently. When the governor does gossip with
-- a peer then it is removed from this set (but scheduled to be added back some
-- time later). So the measure is the size of this set of peers. It is clearly
-- bounded below by the empty set. So the combination of 2 and 3 tells us we
-- make progress in this bounded measure, but that does not directly translate
-- into increasing the size of the known peers set. Properties 4 and 5 tell us
-- that progress with gossiping will eventually translate into increasing the
-- size of the known peers set if that is possible.
--
-- There is one known wrinkle to this argument to do with property 3 that when
-- the governor gossips with a peer it is removed from the tracking set however
-- it gets added back some time later. If they get added back too soon then it
-- would undermine the progress argument because it would break the argument
-- about decreasing the bounded measure. This is readily solved however: we
-- simply need to make sure the time scale for gossip frequency is relatively
-- long, and the other progress bounds are relatively short.
--
prop_governor_target_known_below :: GovernorMockEnvironment -> Property
prop_governor_target_known_below env =
      prop_governor_target_known_1_valid_subset      env
 .&&. prop_governor_target_known_2_opportunity_taken env
 .&&. prop_governor_target_known_3_not_too_chatty    env
 .&&. prop_governor_target_known_4_results_used      env
 .&&. prop_governor_target_known_5_no_shrink_below   env


-- | The set of peers the governor knows about is a subset of the peers the
-- environment has told the governor about.
--
-- We derive a number of signals:
--
-- 1. A signal of the accumulation of all the peers the environment has ever
--    told the governor about, based on the environment trace.
--
-- 2. A signal of the set of known peers in the governor state.
--
-- Based on these signals we check:
--
-- * That the governor known peers is a subset of the accumulated environment
--   known peers.
--
prop_governor_target_known_1_valid_subset :: GovernorMockEnvironment
                                          -> Property
prop_governor_target_known_1_valid_subset env =
    let events = Signal.eventsFromListUpToTime (Time (60 * 60))
               . selectPeerSelectionTraceEvents
               . runGovernorInMockEnvironment
               $ env

        envKnownPeersSig :: Signal (Set PeerAddr)
        envKnownPeersSig =
            Signal.nubBy ((==) `on` Set.size)
          . Signal.scanl Set.union Set.empty
          . Signal.fromChangeEvents Set.empty
          . Signal.selectEvents
              (\case
                  TraceEnvSetLocalRoots  x -> Just (LocalRootPeers.keysSet x)
                  TraceEnvRootsResult    x -> Just (Set.fromList x)
                  TraceEnvGossipResult _ x -> Just (Set.fromList x)
                  _                        -> Nothing
              )
          . selectEnvEvents
          $ events

        govKnownPeersSig :: Signal (Set PeerAddr)
        govKnownPeersSig =
          selectGovState (KnownPeers.toSet . Governor.knownPeers) events

        validState :: Set PeerAddr -> Set PeerAddr -> Bool
        validState knownPeersEnv knownPeersGov =
          knownPeersGov `Set.isSubsetOf` knownPeersEnv

     in counterexample
          "Signal key: (environment known peers, governor known peers)" $

        signalProperty 20 show (uncurry validState) $
          (,) <$> envKnownPeersSig
              <*> govKnownPeersSig


-- | If the governor is below target and has the opportunity to gossip then
-- within a bounded time it should perform a gossip with one of its known peers.
--
-- We derive a number of signals:
--
-- 1. A signal of the target for known peers from the environment
--
-- 2. A signal of the set of known peers in the governor state.
--
-- 3. A signal of the set of peers with which the governor has gossiped
--    recently, based on the requests to the environment
--
-- 4. Based on 2 and 3, a signal of the set of gossip opportunities: the
--    current known peers that are not in the recent gossip set.
--
-- 5. A signal of the environment gossip request events.
--
-- 6. Based on 1, 2, 4 and 5, a signal that becomes False if for 30 seconds:
--    the number of known peers is below target; the set of opportunities is
--    non empty; and no gossip request event has occurred.
--
-- Based on these signals we check:
--
-- * That the signal 6 remains True at all times.
--
prop_governor_target_known_2_opportunity_taken :: GovernorMockEnvironment
                                               -> Property
prop_governor_target_known_2_opportunity_taken env =

    let events = Signal.eventsFromListUpToTime (Time (10 * 60 * 60))
               . selectPeerSelectionTraceEvents
               . runGovernorInMockEnvironment
               $ env

        envTargetsSig :: Signal Int
        envTargetsSig =
          selectEnvTargets targetNumberOfKnownPeers events

        govKnownPeersSig :: Signal (Set PeerAddr)
        govKnownPeersSig =
          selectGovState (KnownPeers.toSet . Governor.knownPeers) events

        envGossipUnavailableSig :: Signal (Set PeerAddr)
        envGossipUnavailableSig =
            Signal.keyedLinger
              -- peers are unavailable for gossip for at least an
              -- hour after each gossip interaction
              (60 * 60)
              (maybe Set.empty Set.singleton)
          . Signal.fromEvents
          . Signal.selectEvents
              (\case TraceEnvGossipRequest peer _ -> Just peer
                     _                            -> Nothing)
          . selectEnvEvents
          $ events

        -- We define the governor's gossip opportunities at any point in time
        -- to be the governor's set of known peers, less the ones we can see
        -- that it has gossiped with recently.
        --
        gossipOpportunitiesSig :: Signal (Set PeerAddr)
        gossipOpportunitiesSig =
          (Set.\\) <$> govKnownPeersSig <*> envGossipUnavailableSig

        -- Note that we only require that the governor try to gossip, it does
        -- not have to succeed.
        envGossipsEventsAsSig :: Signal (Maybe PeerAddr)
        envGossipsEventsAsSig =
            Signal.fromEvents
          . Signal.selectEvents
              (\case TraceEnvGossipRequest addr _ -> Just addr
                     _                            -> Nothing)
          . selectEnvEvents
          $ events

        -- The signal of all the things of interest for this property.
        -- This is used to compute the final predicate, and is also what
        -- we want to report if there is a property violation.
        combinedSig :: Signal (Int,
                               Set PeerAddr,
                               Set PeerAddr,
                               Maybe PeerAddr)
        combinedSig =
          (,,,) <$> envTargetsSig
                <*> govKnownPeersSig
                <*> gossipOpportunitiesSig
                <*> envGossipsEventsAsSig

        -- This is the ultimate predicate signal
        gossipOpportunitiesOkSig :: Signal Bool
        gossipOpportunitiesOkSig =
          Signal.truncateAt (Time (60 * 60 * 10)) $
          governorEventuallyTakesGossipOpportunities combinedSig

     in counterexample
          "Signal key: (target, known peers, opportunities, gossip event)" $

        -- Check the predicate signal but for failures report the input signal
        signalProperty 20 (show . snd) fst $
          (,) <$> gossipOpportunitiesOkSig
              <*> combinedSig


governorEventuallyTakesGossipOpportunities
  :: Signal (Int, Set PeerAddr, Set PeerAddr, Maybe PeerAddr)
  -> Signal Bool

governorEventuallyTakesGossipOpportunities =
    -- Time out and fail after 30 seconds if we enter and remain in a bad state
    fmap not
  . Signal.timeout timeLimit badState
  where
    timeLimit :: DiffTime
    timeLimit = 30

    badState (target, govKnownPeers, gossipOpportunities, gossipEvent) =

        -- A bad state is one where we are below target;
        Set.size govKnownPeers < target

        -- where we do have opportunities; and
     && not (Set.null gossipOpportunities)

        -- are not performing an action to take the opportunity.
     && isNothing gossipEvent

        -- Note that if a gossip does take place, we do /not/ require the gossip
        -- target to be a member of the gossipOpportunities. This is because
        -- the gossip opportunities set is a lower bound not an upper bound.
        -- There is a separate property to check that we do not gossip too
        -- frequently with any individual peer.



-- | The governor should not gossip too frequently with any individual peer,
-- except when the governor forgets known peers.
--
-- We derive a number of signals:
--
-- Based on these signals we check:
--
prop_governor_target_known_3_not_too_chatty :: GovernorMockEnvironment
                                            -> Property
prop_governor_target_known_3_not_too_chatty env =
    let events = Signal.eventsFromListUpToTime (Time (10 * 60 * 60))
               . selectPeerSelectionTraceEvents
               . runGovernorInMockEnvironment
               $ env

        gossipOk Nothing      _           = True
        gossipOk (Just peers) unavailable =
          Set.null (peers `Set.intersection` unavailable)

     in signalProperty 20 show (uncurry gossipOk) $
          recentGossipActivity 3600 events


recentGossipActivity :: DiffTime
                     -> Events TestTraceEvent
                     -> Signal (Maybe (Set PeerAddr), Set PeerAddr)
recentGossipActivity d =
    Signal.fromChangeEvents (Nothing, Set.empty)
  . Signal.primitiveTransformEvents (go Set.empty PSQ.empty)
    --TODO: we should be able to avoid primitiveTransformEvents and express
    -- this as some combo of keyed linger and keyed until.
  where
    go :: Set PeerAddr
       -> PSQ.OrdPSQ PeerAddr Time ()
       -> [E TestTraceEvent]
       -> [E (Maybe (Set PeerAddr), Set PeerAddr)]
    go !recentSet !recentPSQ txs@(E (TS t _) _ : _)
      | Just (k, t', _, recentPSQ') <- PSQ.minView recentPSQ
      , t' <= t
      , let recentSet' = Set.delete k recentSet
      = E (TS t' 0) (Nothing, recentSet')
      : go recentSet' recentPSQ' txs

    -- When we see a gossip request we add it to the recent set and schedule
    -- it to be removed again at time d+t. We arrange for the change in the
    -- recent set to happen after the gossip event.
    go !recentSet !recentPSQ
        (E (TS t i) (GovernorEvent (TraceGossipRequests _ _ _ addrs)) : txs) =
      let recentSet' = recentSet <> addrs
          recentPSQ' = foldl' (\q a -> PSQ.insert a t' () q) recentPSQ addrs
          t'         = d `addTime` t
       in E (TS t i)     (Just addrs, recentSet)
        : E (TS t (i+1)) (Nothing, recentSet') -- updated in next change at same time
        : go recentSet' recentPSQ' txs

    -- When the governor is forced to forget known peers, we drop it from
    -- the recent activity tracking, which means if it is added back again
    -- later then we can gossip with it again earlier than the normal limit.
    --
    -- Alternatively we could track this more coarsely by dropping all tracking
    -- when the targets are adjusted downwards, but we use small target
    -- adjustments to perform churn.
    --
    -- There is a separate property to check that the governor does not forget
    -- peers unnecessarily.
    --
    go !recentSet !recentPSQ
        (E t (GovernorEvent (TraceForgetColdPeers _ _ addrs)) : txs) =
      let recentSet' = foldl' (flip Set.delete) recentSet addrs
          recentPSQ' = foldl' (flip PSQ.delete) recentPSQ addrs
       in E t (Nothing, recentSet')
        : go recentSet' recentPSQ' txs

    go !recentSet !recentPSQ (_ : txs) =
      go recentSet recentPSQ txs

    go !_ !_ [] = []


-- | When the governor does perform a gossip, within a bounded time it should
-- include the results into its known peer set, or the known peer set should
-- reach its target size.
--
-- We derive a number of signals:
--
-- 1. A signal of the target for known peers from the environment
--
-- 2. A signal of the set of known peers in the governor state.
--
-- 3. A signal of the environment gossip result events, as the set of results
--    at any point in time.
--
-- 4. Based on 1, 2 and 3, a signal that tracks a set of peers that we have
--    gossiped with, such that the peers remain in the set until either they
--    appear in the governor known peers set or until the known peer set
--    reaches its target size.
--
-- 5. Based on 4, a signal of the subset of elements that have been a member
--    continuously for at least X seconds duration.
--
-- Based on these signals we assert:
--
-- * That the signal 4 above is always empty.
--
prop_governor_target_known_4_results_used :: GovernorMockEnvironment -> Property
prop_governor_target_known_4_results_used env =
    let events = Signal.eventsFromListUpToTime (Time (10 * 60 * 60))
               . selectPeerSelectionTraceEvents
               . runGovernorInMockEnvironment
               $ env

        envTargetsSig :: Signal Int
        envTargetsSig =
          selectEnvTargets targetNumberOfKnownPeers events

        govKnownPeersSig :: Signal (Set PeerAddr)
        govKnownPeersSig =
          selectGovState (KnownPeers.toSet . Governor.knownPeers) events

        envGossipResultsSig :: Signal (Set PeerAddr)
        envGossipResultsSig =
            fmap (maybe Set.empty Set.fromList)
          . Signal.fromEvents
          . Signal.selectEvents
              (\case TraceEnvGossipResult _ addrs -> Just addrs
                     _                            -> Nothing)
          . selectEnvEvents
          $ events

        gossipResultsUntilKnown :: Signal (Set PeerAddr)
        gossipResultsUntilKnown =
          Signal.keyedUntil
            (\(_, _, gossips)    -> gossips) -- start set
            (\(_, known, _)      -> known)   -- stop set
            (\(target, known, _) -> Set.size known <= target) -- reset condition
            ((,,) <$> envTargetsSig
                  <*> govKnownPeersSig
                  <*> envGossipResultsSig)

        gossipResultsUnknownTooLong :: Signal (Set PeerAddr)
        gossipResultsUnknownTooLong =
          Signal.keyedTimeout
            (10 + 1) -- policyGossipOverallTimeout
            id
            gossipResultsUntilKnown

     in counterexample
          ("\nSignal key: (known peers, gossip result, results unknown, " ++
           "results unknown too long)") $

        signalProperty 20 show
          (\(_,_,_,_,x) -> Set.null x) $
          (,,,,) <$> envTargetsSig
                 <*> govKnownPeersSig
                 <*> envGossipResultsSig
                 <*> gossipResultsUntilKnown
                 <*> gossipResultsUnknownTooLong


-- | The governor should not shrink its known peer set except when it is above
-- the target size.
--
-- We derive a number of signals:
--
-- 1. A signal of the target for known peers from the environment
--
-- 2. A signal of the set of known peers in the governor state.
--
-- 3. Based on 2, a signal of change events when the set of known peers shrinks.
--
-- 4. Based on 1, 2 and 3, a signal of unexpected shrink events: a signal that
--    is True when there is a shrink event and the new size of the set of known
--    peers is below the target.
--
-- Based on these signals we assert:
--
-- * That the signal 4 above is always False.
--
prop_governor_target_known_5_no_shrink_below :: GovernorMockEnvironment -> Property
prop_governor_target_known_5_no_shrink_below env =
    let events = Signal.eventsFromListUpToTime (Time (10 * 60 * 60))
               . selectPeerSelectionTraceEvents
               . runGovernorInMockEnvironment
               $ env

        envTargetsSig :: Signal Int
        envTargetsSig =
          selectEnvTargets targetNumberOfKnownPeers events

        govKnownPeersSig :: Signal (Set PeerAddr)
        govKnownPeersSig =
          selectGovState (KnownPeers.toSet . Governor.knownPeers) events

        knownPeersShrinksSig :: Signal (Set PeerAddr)
        knownPeersShrinksSig =
            Signal.nub
          . fmap (fromMaybe Set.empty)
          $ Signal.difference
              (\x x' -> x Set.\\ x')
              govKnownPeersSig

        unexpectedShrink :: Signal Bool
        unexpectedShrink =
          -- Note that when we observe a shrink, the known peers set at the
          -- same time is the new shrunk value. This means our test has to be
          -- Set.size known < target rather than Set.size known <= target
          -- It also has the bonus of checking that we are checking that the
          -- size of the known peer set after the shrink is not strictly
          -- smaller than the target, which means we're checking that we do
          -- not undershoot the target: from above we hit the target exactly.
          (\target known shrinks ->
                not (Set.null shrinks)
             && Set.size known < target
          ) <$> envTargetsSig
            <*> govKnownPeersSig
            <*> knownPeersShrinksSig

     in counterexample
          "\nSignal key: (target, known peers, shrinks, unexpected)" $

        signalProperty 20 show
          (\(_,_,_,unexpected) -> not unexpected)
          ((,,,) <$> envTargetsSig
                 <*> govKnownPeersSig
                 <*> knownPeersShrinksSig
                 <*> unexpectedShrink)



-- | The governor should shrink its known peer set within a bounded time when
-- it is above the target size.
--
-- This deals with hitting the target from above. We have to allow some bounded
-- time rather than demand instant shrinking because in some situation the
-- governor must demote active or established peers before it can forget known
-- peers.
--
-- We derive a number of signals:
--
-- 1. A signal of the effective target for known peers from the environment,
--    based on both the given target and the local root peers.
--
-- 2. A signal of the set of known peers in the governor state.
--
-- 3. Based on 2, a signal of change events when the set of known peers shrinks.
--
-- 5. Based on 1, 2 and 3, a signal that becomes True if for X seconds, the
--    known peers is above target and there is no shrink event.
--
-- Based on these signals we check:
--
-- * That the signal 5 above is always False.
--
prop_governor_target_known_above :: GovernorMockEnvironment -> Property
prop_governor_target_known_above env =
    let events = Signal.eventsFromListUpToTime (Time (10 * 60 * 60))
               . selectPeerSelectionTraceEvents
               . runGovernorInMockEnvironment
               $ env

        envTargetsSig :: Signal PeerSelectionTargets
        envTargetsSig =
          selectEnvTargets id events

        govLocalRootPeersSig :: Signal (Set PeerAddr)
        govLocalRootPeersSig =
          selectGovState (LocalRootPeers.keysSet . Governor.localRootPeers)
                         events

        govPublicRootPeersSig :: Signal (Set PeerAddr)
        govPublicRootPeersSig =
          selectGovState Governor.publicRootPeers events

        govKnownPeersSig :: Signal (Set PeerAddr)
        govKnownPeersSig =
          selectGovState (KnownPeers.toSet . Governor.knownPeers) events

        govEstablishedPeersSig :: Signal (Set PeerAddr)
        govEstablishedPeersSig =
          selectGovState (EstablishedPeers.toSet . Governor.establishedPeers)
                         events

        -- There are no demotion opportunities if we're at or below target.
        -- Otherwise, the opportunities for demotion are known peers that
        -- are not currently established and are not local.
        demotionOpportunity targets local public known established
          | Set.size known <= targetNumberOfKnownPeers targets
          = Set.empty

         | otherwise
         = known Set.\\ established
                 Set.\\ local
                 Set.\\ publicProtected
          where
            -- Furthermore, public roots are protected from demotion if we are
            -- at or below target for roots peers.
            publicProtected
              | Set.size local + Set.size public
                 <= targetNumberOfRootPeers targets
              = public

              | otherwise
              = Set.empty

        deomotionOpportunities :: Signal (Set PeerAddr)
        deomotionOpportunities =
          demotionOpportunity
            <$> envTargetsSig
            <*> govLocalRootPeersSig
            <*> govPublicRootPeersSig
            <*> govKnownPeersSig
            <*> govEstablishedPeersSig

        demotionOpportunitiesIgnoredTooLong :: Signal (Set PeerAddr)
        demotionOpportunitiesIgnoredTooLong =
          Signal.keyedTimeout
            10 -- seconds
            id
            deomotionOpportunities

     in counterexample
          ("\nSignal key: (target (root, known), local peers, public peers, known peers, " ++
           "established peers, demotion opportunities, ignored too long)") $

        signalProperty 20 show
          (\(_,_,_,_,_,_,toolong) -> Set.null toolong)
          ((,,,,,,) <$> ((\t -> (targetNumberOfRootPeers t,
                                 targetNumberOfKnownPeers t)) <$> envTargetsSig)
                    <*> govLocalRootPeersSig
                    <*> govPublicRootPeersSig
                    <*> govKnownPeersSig
                    <*> govEstablishedPeersSig
                    <*> deomotionOpportunities
                    <*> demotionOpportunitiesIgnoredTooLong)


-- | Check that the governor can hit (but not overshoot) its target for the
-- number of warm peers. This has to be bounded by what is possible: we cannot
-- always find enough peers, and when we can, some of them fail.
--
-- This is a somewhat tricky property to express because it is non-trivial to
-- find the maximum number of possible established connections by inspecting
-- the mock environment.
--
-- We approach it in three parts: from above, from below and statistically.
--
-- The simplest is from above: the environment knows how many established
-- connections there are at any point in (virtual) time, and what the targets
-- are. So we can easily compare the two. This can be a tight bound above.
-- When the target is stable, the governor should never overshoot the target.
-- When the target changes to be smaller, the governor should shrink the number
-- of established connections to be within the target within a relatively short
-- period of time.
--
--
--
-- Tracking very precisely the maximum number of peers we could reasonably
-- establish connections to is tricky and hence prone to mistakes in the test
-- definition. So as an extra sanity check we take a simpler but fuzzy approach.
-- In some fraction of test runs, the environment should be such that it is
-- possible to actually hit the target for the number of established peers. So
-- we label the cases where this happens, and then we can use a statistical
-- test to assert that this happens in some fraction of test cases.
--
prop_governor_target_established_below :: GovernorMockEnvironment -> Property
prop_governor_target_established_below env =
    let events = Signal.eventsFromListUpToTime (Time (10 * 60 * 60))
               . selectPeerSelectionTraceEvents
               . runGovernorInMockEnvironment
               $ env

        envTargetsSig :: Signal Int
        envTargetsSig =
          selectEnvTargets targetNumberOfEstablishedPeers events

        govKnownPeersSig :: Signal (Set PeerAddr)
        govKnownPeersSig =
          selectGovState (KnownPeers.toSet . Governor.knownPeers) events

        govEstablishedPeersSig :: Signal (Set PeerAddr)
        govEstablishedPeersSig =
          selectGovState
            (EstablishedPeers.toSet . Governor.establishedPeers)
            events

        govEstablishedFailuresSig :: Signal (Set PeerAddr)
        govEstablishedFailuresSig =
            Signal.keyedLinger
              180 -- 3 minutes  -- TODO: too eager to reconnect?
              (fromMaybe Set.empty)
          . Signal.fromEvents
          . Signal.selectEvents
              (\case TracePromoteColdFailed _ _ peer _ _ ->
                       --TODO: the environment does not yet cause this to happen
                       -- it requires synchronous failure in the establish action
                       Just (Set.singleton peer)
                     --TODO: what about TraceDemoteWarmDone ?
                     -- these are also not immediate candidates
                     -- why does the property not fail for not tracking these?
                     TraceDemoteAsynchronous status
                       | Set.null failures -> Nothing
                       | otherwise         -> Just failures
                       where
                         failures = Map.keysSet (Map.filter (==PeerCold) status)
                     _ -> Nothing
              )
          . selectGovEvents
          $ events

        promotionOpportunities :: Signal (Set PeerAddr)
        promotionOpportunities =
          (\target known established recentFailures ->
              -- There are no opportunities if we're at or above target
              if Set.size established >= target
                then Set.empty
                else known Set.\\ established
                           Set.\\ recentFailures
          ) <$> envTargetsSig
            <*> govKnownPeersSig
            <*> govEstablishedPeersSig
            <*> govEstablishedFailuresSig

        promotionOpportunitiesIgnoredTooLong :: Signal (Set PeerAddr)
        promotionOpportunitiesIgnoredTooLong =
          Signal.keyedTimeout
            10 -- seconds
            id
            promotionOpportunities

     in counterexample
          ("\nSignal key: (target, known peers, established peers, recent failures, " ++
           "opportunities, ignored too long)") $

        signalProperty 20 show
          (\(_,_,_,_,_,toolong) -> Set.null toolong)
          ((,,,,,) <$> envTargetsSig
                   <*> govKnownPeersSig
                   <*> govEstablishedPeersSig
                   <*> govEstablishedFailuresSig
                   <*> promotionOpportunities
                   <*> promotionOpportunitiesIgnoredTooLong)



prop_governor_target_active_below :: GovernorMockEnvironment -> Property
prop_governor_target_active_below env =
    let events = Signal.eventsFromListUpToTime (Time (10 * 60 * 60))
               . selectPeerSelectionTraceEvents
               . runGovernorInMockEnvironment
               $ env

        envTargetsSig :: Signal Int
        envTargetsSig =
          selectEnvTargets targetNumberOfActivePeers events

        govEstablishedPeersSig :: Signal (Set PeerAddr)
        govEstablishedPeersSig =
          selectGovState
            (EstablishedPeers.toSet . Governor.establishedPeers)
            events

        govActivePeersSig :: Signal (Set PeerAddr)
        govActivePeersSig =
          selectGovState Governor.activePeers events

        govActiveFailuresSig :: Signal (Set PeerAddr)
        govActiveFailuresSig =
            Signal.keyedLinger
              180 -- 3 minutes  -- TODO: too eager to reconnect?
              (fromMaybe Set.empty)
          . Signal.fromEvents
          . Signal.selectEvents
              (\case TracePromoteWarmFailed _ _ peer _ ->
                       --TODO: the environment does not yet cause this to happen
                       -- it requires synchronous failure in the establish action
                       Just (Set.singleton peer)
                     --TODO
                     TraceDemoteAsynchronous status
                       | Set.null failures -> Nothing
                       | otherwise         -> Just failures
                       where
                         failures = Map.keysSet (Map.filter (==PeerWarm) status)
                     _ -> Nothing
              )
          . selectGovEvents
          $ events

        promotionOpportunities :: Signal (Set PeerAddr)
        promotionOpportunities =
          (\target established active recentFailures ->
              -- There are no opportunities if we're at or above target
              if Set.size active >= target
                then Set.empty
                else established Set.\\ active
                                 Set.\\ recentFailures
          ) <$> envTargetsSig
            <*> govEstablishedPeersSig
            <*> govActivePeersSig
            <*> govActiveFailuresSig

        promotionOpportunitiesIgnoredTooLong :: Signal (Set PeerAddr)
        promotionOpportunitiesIgnoredTooLong =
          Signal.keyedTimeout
            10 -- seconds
            id
            promotionOpportunities

     in counterexample
          ("\nSignal key: (target, known peers, active peers, recent failures, " ++
           "opportunities, ignored too long)") $

        signalProperty 20 show
          (\(_,_,_,_,_,toolong) -> Set.null toolong)
          ((,,,,,) <$> envTargetsSig
                   <*> govEstablishedPeersSig
                   <*> govActivePeersSig
                   <*> govActiveFailuresSig
                   <*> promotionOpportunities
                   <*> promotionOpportunitiesIgnoredTooLong)


prop_governor_target_established_above :: GovernorMockEnvironment -> Property
prop_governor_target_established_above env =
    let events = Signal.eventsFromListUpToTime (Time (10 * 60 * 60))
               . selectPeerSelectionTraceEvents
               . runGovernorInMockEnvironment
               $ env

        envTargetsSig :: Signal Int
        envTargetsSig =
          selectEnvTargets targetNumberOfEstablishedPeers events

        govEstablishedPeersSig :: Signal (Set PeerAddr)
        govEstablishedPeersSig =
          selectGovState
            (EstablishedPeers.toSet . Governor.establishedPeers)
            events

        establishedPeersShrinksSig :: Signal (Set PeerAddr)
        establishedPeersShrinksSig =
            Signal.nub
          . fmap (fromMaybe Set.empty)
          $ Signal.difference
              (\x x' -> x Set.\\ x')
              govEstablishedPeersSig

        aboveTargetTooLong :: Signal Bool
        aboveTargetTooLong =
          Signal.timeout 15
            (\(target, established, shrinks) ->
                  Set.size established > target
               && Set.null shrinks)
            ((,,) <$> envTargetsSig
                  <*> govEstablishedPeersSig
                  <*> establishedPeersShrinksSig)

     in counterexample
          "\nSignal key: (target, established peers, shrinks, above target too long)" $

        signalProperty 20 show
          (\(_,_,_,toolong) -> not toolong)
          ((,,,) <$> envTargetsSig
                 <*> govEstablishedPeersSig
                 <*> establishedPeersShrinksSig
                 <*> aboveTargetTooLong)


prop_governor_target_active_above :: GovernorMockEnvironment -> Property
prop_governor_target_active_above env =
    let events = Signal.eventsFromListUpToTime (Time (10 * 60 * 60))
               . selectPeerSelectionTraceEvents
               . runGovernorInMockEnvironment
               $ env

        envTargetsSig :: Signal Int
        envTargetsSig =
          selectEnvTargets targetNumberOfActivePeers events

        govActivePeersSig :: Signal (Set PeerAddr)
        govActivePeersSig =
          selectGovState Governor.activePeers events

        activePeersShrinksSig :: Signal (Set PeerAddr)
        activePeersShrinksSig =
            Signal.nub
          . fmap (fromMaybe Set.empty)
          $ Signal.difference
              (\x x' -> x Set.\\ x')
              govActivePeersSig

        aboveTargetTooLong :: Signal Bool
        aboveTargetTooLong =
          Signal.timeout 15
            (\(target, active, shrinks) ->
                  Set.size active > target
               && Set.null shrinks)
            ((,,) <$> envTargetsSig
                  <*> govActivePeersSig
                  <*> activePeersShrinksSig)

     in counterexample
          "\nSignal key: (target, active peers, shrinks, above target too long)" $

        signalProperty 20 show
          (\(_,_,_,toolong) -> not toolong)
          ((,,,) <$> envTargetsSig
                 <*> govActivePeersSig
                 <*> activePeersShrinksSig
                 <*> aboveTargetTooLong)


--
-- Utils for properties
--

takeFirstNHours :: DiffTime -> [(Time, a)] -> [(Time, a)]
takeFirstNHours h = takeWhile (\(t,_) -> t < Time (60*60*h))

selectEnvEvents :: Events TestTraceEvent -> Events TraceMockEnv
selectEnvEvents = Signal.selectEvents
                    (\case MockEnvEvent e -> Just e
                           _              -> Nothing)

selectGovEvents :: Events TestTraceEvent
                -> Events (TracePeerSelection PeerAddr)
selectGovEvents = Signal.selectEvents
                    (\case GovernorEvent e -> Just e
                           _               -> Nothing)

selectGovState :: Eq a
               => (Governor.PeerSelectionState PeerAddr () -> a)
               -> Events TestTraceEvent
               -> Signal a
selectGovState f =
    Signal.nub
  . fmap f
  . Signal.fromChangeEvents Governor.emptyPeerSelectionState
  . Signal.selectEvents
      (\case GovernorDebug (TraceGovernorState _ _ st) -> Just st
             _                                         -> Nothing)

selectEnvTargets :: Eq a
                 => (PeerSelectionTargets -> a)
                 -> Events TestTraceEvent
                 -> Signal a
selectEnvTargets f =
    Signal.nub
  . fmap f
  . Signal.fromChangeEvents nullPeerSelectionTargets
  . Signal.selectEvents
      (\case TraceEnvSetTargets targets -> Just targets
             _                          -> Nothing)
  . selectEnvEvents

--
-- Live examples
--

-- | Run the 'publicRootPeersProvider' in IO with a stdout tracer to observe
-- what it does.
--
-- This is a manual test that runs in IO and has to be observed to see that it
-- is doing something sensible. It is not run automatically.
--
_governorFindingPublicRoots :: Int -> [DomainAddress] -> IO Void
_governorFindingPublicRoots targetNumberOfRootPeers domains =
    withTimeoutSerial $ \timeout ->
    publicRootPeersProvider
      tracer
      timeout
      DNS.defaultResolvConf
      domains $ \requestPublicRootPeers ->

        peerSelectionGovernor
          tracer tracer tracer
          actions { requestPublicRootPeers }
          policy
  where
    tracer :: Show a => Tracer IO a
    tracer  = Tracer (BS.putStrLn . BS.pack . show)

    actions :: PeerSelectionActions SockAddr () IO
    actions = PeerSelectionActions {
                readLocalRootPeers       = return [],
                readPeerSelectionTargets = return targets,
                requestPeerGossip        = \_ -> return [],
                requestPublicRootPeers   = \_ -> return (Set.empty, 0),
                peerStateActions         = PeerStateActions {
                  establishPeerConnection  = error "establishPeerConnection",
                  monitorPeerConnection    = error "monitorPeerConnection",
                  activatePeerConnection   = error "activatePeerConnection",
                  deactivatePeerConnection = error "deactivatePeerConnection",
                  closePeerConnection      = error "closePeerConnection"
                }
              }

    targets :: PeerSelectionTargets
    targets = PeerSelectionTargets {
                targetNumberOfRootPeers        = targetNumberOfRootPeers,
                targetNumberOfKnownPeers       = targetNumberOfRootPeers,
                targetNumberOfEstablishedPeers = 0,
                targetNumberOfActivePeers      = 0
              }

    policy :: PeerSelectionPolicy SockAddr IO
    policy  = PeerSelectionPolicy {
                policyPickKnownPeersForGossip = pickTrivially,
                policyPickColdPeersToForget   = pickTrivially,
                policyPickColdPeersToPromote  = pickTrivially,
                policyPickWarmPeersToPromote  = pickTrivially,
                policyPickHotPeersToDemote    = pickTrivially,
                policyPickWarmPeersToDemote   = pickTrivially,
                policyFindPublicRootTimeout   = 5,
                policyMaxInProgressGossipReqs = 0,
                policyGossipRetryTime         = 0, -- seconds
                policyGossipBatchWaitTime     = 0, -- seconds
                policyGossipOverallTimeout    = 0  -- seconds
              }
    pickTrivially :: Applicative m => Set SockAddr -> Int -> m (Set SockAddr)
    pickTrivially m n = pure . Set.take n $ m

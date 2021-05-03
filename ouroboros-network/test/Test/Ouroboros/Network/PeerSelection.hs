{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}


{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Ouroboros.Network.PeerSelection (tests) where

import qualified Data.ByteString.Char8 as BS
import           Data.Function (on)
import           Data.List (groupBy, foldl')
import           Data.Maybe (listToMaybe)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Void (Void)

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
import           Ouroboros.Network.PeerSelection.RootPeersDNS

import           Test.Ouroboros.Network.PeerSelection.Instances
import qualified Test.Ouroboros.Network.PeerSelection.LocalRootPeers
import           Test.Ouroboros.Network.PeerSelection.MockEnvironment hiding (tests)
import qualified Test.Ouroboros.Network.PeerSelection.MockEnvironment
import           Test.Ouroboros.Network.PeerSelection.PeerGraph

import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)


tests :: TestTree
tests =
  testGroup "Ouroboros.Network.PeerSelection"
  [ Test.Ouroboros.Network.PeerSelection.LocalRootPeers.tests
  , Test.Ouroboros.Network.PeerSelection.MockEnvironment.tests
  , testGroup "governor"
    [ testProperty "has output"         prop_governor_hasoutput
    , testProperty "no failure"         prop_governor_nofail
    , testProperty "no livelock"        prop_governor_nolivelock
    , testProperty "no excess busyness" prop_governor_nobusyness
    , testProperty "event coverage"     prop_governor_trace_coverage
    , testProperty "gossip reachable"   prop_governor_gossip_1hr
    , testProperty "connection status"  prop_governor_connstatus
    ]
  ]


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
-- Properties that we would like to have:
--
-- * A "no excessive busyness" property. This checks that the governor does not
--   remain too busy for too long. It's quite easy to write bugs that don't
--   cause the governor to fail, but cause it to go into fairly-busy cycles.
--
-- * A public root peers target property: that the governor hits its target for
--   for the number of public root peers (or as near as possible), and does
--   note "grossly" overshoot. Since the public roots is a one sided target, but
--   we don't want to overshoot excessively.
--
-- * A warm peer target property: that the governor hits its established peers
--   target (or as near as possible).
--
-- * A hot peer target property: that the governor hits its active peers
--   target (or as near as possible).
--
-- * A local root peers target property: that the governor hits it target for
--   getting all its local root peers into the established state, and a target
--   number of them into the active state (or as near as possible).
--
-- Other properties we might like to think about
--
-- * for vaguely stable envs, we do stablise at our target number of cold peers
-- * time to stabilise after a change is not crazy
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
-- Utils for properties
--

takeFirstNHours :: DiffTime -> [(Time, a)] -> [(Time, a)]
takeFirstNHours h = takeWhile (\(t,_) -> t < Time (60*60*h))


--
-- Live examples
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

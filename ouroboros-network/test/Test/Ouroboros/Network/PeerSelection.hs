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
import           Data.List (groupBy)
import qualified Data.Map.Strict as Map
import           Data.Maybe (listToMaybe)
import           Data.Set (Set)
import qualified Data.Set as Set
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
import           Ouroboros.Network.PeerSelection.RootPeersDNS

import           Test.Ouroboros.Network.PeerSelection.Instances
import           Test.Ouroboros.Network.PeerSelection.MockEnvironment hiding (tests)
import qualified Test.Ouroboros.Network.PeerSelection.MockEnvironment

import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)


tests :: TestTree
tests =
  testGroup "Ouroboros.Network.PeerSelection"
  [ Test.Ouroboros.Network.PeerSelection.MockEnvironment.tests
  , testProperty "governor gossip reachable in 1hr" prop_governor_gossip_1hr
  , testProperty "governor connection status"       prop_governor_connstatus
  , testProperty "governor no livelock"             prop_governor_nolivelock
  ]


--
-- QuickCheck properties
--

-- Things we might like to test...
--
-- * for even insane environments, there is no insane behaviour
--   trace properties:
--   * progress: all actions should make monotonic progress
--   * no busy work: limit on number of governor iterations before time advances
--   * trace sanity: model of state can be reconstructed from trace events
--
-- * for vaguely stable envs, we do stablise at our target number of cold peers
-- * we stabilise without going insane even if the available nodes are fewer than the target
-- * time to stabilise after a change is not crazy
-- * time to find new nodes after a graph change is ok
-- * targets or root peer set dynamic
-- * check local root peers are what we expect
-- * check governor view of connection status does not lag reality too much


-- | Run the governor for up to 24 hours (simulated obviously) and see if it
-- throws any exceptions (assertions such as invariant violations) or if it
-- encounters a livelock situation.
--
-- | It is easy to get bugs where the governor is stuck in a busy loop working
-- but not making progress. This kind of bug would result in the governor
-- thread consuming all the cpu, but wouldn't actually stop the node, so might
-- not be easily noticed.
--
-- We check for this condition by requiring that trace events a certain number
-- of events apart are sufficiently far apart in time too. This will be
-- violated if the governor starts making very slow forward progress.
--
-- This uses static targets and root peers.
--
-- TODO: Reenable this testcase.
prop_governor_nolivelock :: GovernorMockEnvironment -> Property
prop_governor_nolivelock env =
    within 10_000_000 $
    let trace = takeFirstNHours 24 .
                selectGovernorEvents .
                selectPeerSelectionTraceEvents $
                  runGovernorInMockEnvironment env

     in
{-
       -- uncomment to check expected distribution
       tabulate  "env size"   [renderRanges 10 envSize] $
       tabulate  "max events" [renderRanges 10 (maxEvents 5 trace)] $
       tabulate  "events/graph ratio"
                 [show (maxEvents 5 trace `div` envSize)] $
-}
       hasOutput trace

       -- Check we don't get too many events within a given time span.
       -- How many events is too many? It scales with the graph size.
       -- The ratio between them is from experimental evidence.
  .&&. let maxevents = (2+envSize) * 8 -- ratio from experiments
           timespan  = 5               -- seconds
           actual    = maxEvents (floor timespan) trace
        in counterexample ("Too many events in a span of time!\n"
                        ++ "  time span:  " ++ show timespan ++ " seconds\n"
                        ++ "  env size:   " ++ show envSize ++ "\n"
                        ++ "  num events: " ++ show actual) $

           property (makesAdequateProgress maxevents timespan
                                           (map fst trace))
  where
    hasOutput :: [(Time, TracePeerSelection PeerAddr)] -> Property
    hasOutput (_:_) = property True
    hasOutput []    = counterexample "no trace output" $
                      property (isEmptyEnv env)

    envSize         = length g + length (targets env)
                        where PeerGraph g = peerGraph env
    maxEvents n     = maximum
                    . (0:)
                    . map length
                    . timeSpans n

    timeSpans :: Int -> [(Time, a)] -> [[(Time, a)]]
    timeSpans _ []           = []
    timeSpans n (x@(t,_):xs) =
      let (xs', xs'') = span (\(t',_) -> t' <= addTime (fromIntegral n) t) (x:xs)
       in xs' : timeSpans n xs''

isEmptyEnv :: GovernorMockEnvironment -> Bool
isEmptyEnv GovernorMockEnvironment {
             localRootPeers,
             publicRootPeers,
             targets
           } =
    (Map.null localRootPeers
      || all (\(t,_) -> targetNumberOfKnownPeers t == 0) targets)
 && (Set.null publicRootPeers
      || all (\(t,_) -> targetNumberOfRootPeers  t == 0) targets)


-- Check that events that are 100 events apart have an adequate time
-- between them, to indicate we're not in a busy livelock situation.
makesAdequateProgress :: Int -> DiffTime -> [Time] -> Bool
makesAdequateProgress n adequate ts =
    go ts (drop n ts)
  where
    go (a:as) (b:bs)
      | diffTime b a < adequate = False
      | otherwise               = go as bs
    go _ _ = True

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
prop_governor_gossip_1hr :: GovernorMockEnvironmentWithoutAsyncDemotion -> Property
prop_governor_gossip_1hr (GovernorMockEnvironmentWAD env@GovernorMockEnvironment{
                              peerGraph,
                              localRootPeers,
                              publicRootPeers,
                              targets
                            }) =
    let trace      = selectPeerSelectionTraceEvents $
                       runGovernorInMockEnvironment env {
                         targets = singletonScript (targets', NoDelay)
                       }
        Just found = knownPeersAfter1Hour trace
        reachable  = firstGossipReachablePeers peerGraph
                       (Map.keysSet localRootPeers <> publicRootPeers)
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
prop_governor_connstatus :: GovernorMockEnvironmentWithoutAsyncDemotion -> Bool
prop_governor_connstatus (GovernorMockEnvironmentWAD env) =
    let trace = takeFirstNHours 1
              . selectPeerSelectionTraceEvents $
                  runGovernorInMockEnvironment env
        --TODO: check any actually get a true status output and try some deliberate bugs
     in all ok (groupBy ((==) `on` fst) trace)
  where
    -- We look at events when the environment's view of the state of all the
    -- peer connections changed, and check that before simulated time advances
    -- the governor's view of the same state was brought in sync.
    --
    -- We do that by finding the env events and then looking for the last
    -- governor state event before time moves on.
    ok :: [(Time, TestTraceEvent)] -> Bool
    ok trace =
        case (lastTrueStatus, lastTestStatus) of
          (Nothing, _)                       -> True
          (Just trueStatus, Just testStatus) -> trueStatus == testStatus
          (Just _,          Nothing)         -> False
      where
        lastTrueStatus =
          listToMaybe
            [ status
            | (_, MockEnvEvent (TraceEnvPeersStatus status)) <- reverse trace ]

        lastTestStatus =
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
          tracer tracer
          actions { requestPublicRootPeers }
          policy
  where
    tracer :: Show a => Tracer IO a
    tracer  = Tracer (BS.putStrLn . BS.pack . show)

    actions :: PeerSelectionActions SockAddr () IO
    actions = PeerSelectionActions {
                readLocalRootPeers       = return Map.empty,
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

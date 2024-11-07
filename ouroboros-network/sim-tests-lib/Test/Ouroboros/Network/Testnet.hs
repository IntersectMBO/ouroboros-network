{-# LANGUAGE CPP                 #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

#if defined(mingw32_HOST_OS)
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
#endif
#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-x-partial #-}
#endif

module Test.Ouroboros.Network.Testnet (tests) where

import Control.Exception (AssertionFailed (..), catch, evaluate, fromException)
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadTest (exploreRaces)
import Control.Monad.Class.MonadTime.SI (DiffTime, Time (Time), addTime,
           diffTime)
import Control.Monad.IOSim

import Data.Bifoldable (bifoldMap)
import Data.Dynamic (fromDynamic)
import Data.Foldable (fold)
import Data.IP qualified as IP
import Data.List as List (find, foldl', intercalate, tails)
import Data.List.Trace qualified as Trace
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust, mapMaybe)
import Data.Monoid (Sum (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Time (secondsToDiffTime)
import Data.Typeable (Typeable)
import Data.Void (Void)
import Data.Word (Word32)

import GHC.IO.Exception as GHC (IOErrorType (..), IOException (..))
import System.Random (mkStdGen)

import Network.DNS.Types qualified as DNS

import Ouroboros.Network.Block (BlockNo (..))
import Ouroboros.Network.BlockFetch (FetchMode (..), TraceFetchClientState (..))
import Ouroboros.Network.ConnectionHandler (ConnectionHandlerTrace)
import Ouroboros.Network.ConnectionId
import Ouroboros.Network.ConnectionManager.Core qualified as CM
import Ouroboros.Network.ConnectionManager.Test.Timeouts (TestProperty (..),
           classifyActivityType, classifyEffectiveDataFlow,
           classifyNegotiatedDataFlow, classifyPrunings, classifyTermination,
           groupConns, mkProperty, ppTransition, verifyAllTimeouts)
import Ouroboros.Network.ConnectionManager.Test.Utils
           (abstractStateIsFinalTransition,
           abstractStateIsFinalTransitionTVarTracing, connectionManagerTraceMap,
           validTransitionMap, verifyAbstractTransition,
           verifyAbstractTransitionOrder)
import Ouroboros.Network.ConnectionManager.Types
import Ouroboros.Network.ConsensusMode
import Ouroboros.Network.ExitPolicy (RepromoteDelay (..))
import Ouroboros.Network.InboundGovernor qualified as IG
import Ouroboros.Network.InboundGovernor.Test.Utils (inboundGovernorTraceMap,
           remoteStrIsFinalTransition, serverTraceMap, validRemoteTransitionMap,
           verifyRemoteTransition, verifyRemoteTransitionOrder)
import Ouroboros.Network.Mock.ConcreteBlock (BlockHeader)
import Ouroboros.Network.NodeToNode (DiffusionMode (..))
import Ouroboros.Network.PeerSelection.Bootstrap (UseBootstrapPeers (..),
           requiresBootstrapPeers)
import Ouroboros.Network.PeerSelection.Governor hiding (PeerSelectionState (..))
import Ouroboros.Network.PeerSelection.Governor qualified as Governor
import Ouroboros.Network.PeerSelection.LedgerPeers
import Ouroboros.Network.PeerSelection.PeerAdvertise (PeerAdvertise (..))
import Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing (..))
import Ouroboros.Network.PeerSelection.PeerStateActions
import Ouroboros.Network.PeerSelection.PeerTrustable (PeerTrustable (..))
import Ouroboros.Network.PeerSelection.PublicRootPeers qualified as PublicRootPeers
import Ouroboros.Network.PeerSelection.RootPeersDNS.DNSActions hiding
           (DNSorIOError (IOError))
import Ouroboros.Network.PeerSelection.RootPeersDNS.LocalRootPeers
           (TraceLocalRootPeers (..))
import Ouroboros.Network.PeerSelection.State.EstablishedPeers qualified as EstablishedPeers
import Ouroboros.Network.PeerSelection.State.KnownPeers qualified as KnownPeers
import Ouroboros.Network.PeerSelection.State.LocalRootPeers (HotValency (..),
           WarmValency (..))
import Ouroboros.Network.PeerSelection.State.LocalRootPeers qualified as LocalRootPeers
import Ouroboros.Network.PeerSelection.Types
import Ouroboros.Network.PeerSharing (PeerSharingResult (..))
import Ouroboros.Network.Server2 qualified as Server
import Ouroboros.Network.Testing.Data.AbsBearerInfo
import Ouroboros.Network.Testing.Data.Script
import Ouroboros.Network.Testing.Data.Signal
import Ouroboros.Network.Testing.Data.Signal qualified as Signal
import Ouroboros.Network.Testing.Utils hiding (SmallDelay, debugTracer)

import Simulation.Network.Snocket (BearerInfo (..))

import Test.Ouroboros.Network.LedgerPeers (LedgerPools (..))
import Test.Ouroboros.Network.Testnet.Internal
import Test.Ouroboros.Network.Testnet.Node (config_REPROMOTE_DELAY)
import Test.Ouroboros.Network.Testnet.Node.Kernel
import Test.QuickCheck
import Test.QuickCheck.Monoids
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup "Ouroboros.Network.Testnet"
  [ testGroup "generators"
    [ testProperty "diffusionScript fixupCommands idempotent"
                    prop_diffusionScript_fixupCommands
    , testProperty "diffusionScript command script valid"
                   prop_diffusionScript_commandScript_valid
    ]
  , testGroup "IOSimPOR"
    [ nightlyTest $ testProperty "no failure"
                      (testWithIOSimPOR prop_diffusion_nofail 10000)
    , nightlyTest $ testProperty "no livelock"
                      (testWithIOSimPOR prop_diffusion_nolivelock 10000)
    , nightlyTest $ testProperty "dns can recover from fails"
                      (testWithIOSimPOR prop_diffusion_dns_can_recover 10000)
    , nightlyTest $ testProperty "target established public"
                      (testWithIOSimPOR prop_diffusion_target_established_public 10000)
    , nightlyTest $ testProperty "target active public"
                      (testWithIOSimPOR prop_diffusion_target_active_public 10000)
    , nightlyTest $ testProperty "target established local"
                      (testWithIOSimPOR prop_diffusion_target_established_local 10000)
    , nightlyTest $ testProperty "target active local"
                      (testWithIOSimPOR prop_diffusion_target_active_local 10000)
    , nightlyTest $ testProperty "target active root"
                      (testWithIOSimPOR prop_diffusion_target_active_root 10000)
    , nightlyTest $ testProperty "target active below"
                      (testWithIOSimPOR prop_diffusion_target_active_below 10000)
    , nightlyTest $ testProperty "target active local below"
                      (testWithIOSimPOR prop_diffusion_target_active_local_below 10000)
    , nightlyTest $ testProperty "async demotion"
                      (testWithIOSimPOR prop_diffusion_async_demotions 10000)
    , nightlyTest $ testProperty "target active local above"
                      (testWithIOSimPOR prop_diffusion_target_active_local_above 10000)
    , nightlyTest $ testProperty "connection manager valid transitions"
                      (testWithIOSimPOR prop_diffusion_cm_valid_transitions 10000)
    , nightlyTest $ testProperty "connection manager valid transition order"
                      (testWithIOSimPOR prop_diffusion_cm_valid_transition_order_iosim_por 10000)
    , nightlyTest $ testProperty "connection manager no dodgy traces"
                      (testWithIOSimPOR prop_diffusion_cm_no_dodgy_traces 10000)
    , nightlyTest $ testProperty "peer selection actions no dodgy traces"
                      (testWithIOSimPOR prop_diffusion_peer_selection_actions_no_dodgy_traces 10000)
    , nightlyTest $ testProperty "inbound governor valid transitions"
                      (testWithIOSimPOR prop_diffusion_ig_valid_transitions 10000)
    , nightlyTest $ testProperty "inbound governor valid transition order"
                      (testWithIOSimPOR prop_diffusion_ig_valid_transition_order 10000)
    , nightlyTest $ testProperty "cm & ig timeouts enforced"
                      (testWithIOSimPOR prop_diffusion_timeouts_enforced 10000)
    , nightlyTest $ testProperty "any Cold async demotion"
                      (testWithIOSimPOR prop_track_coolingToCold_demotions 10000)
    , nightlyTest $ testProperty "only bootstrap peers in fallback state"
                      (testWithIOSimPOR prop_only_bootstrap_peers_in_fallback_state 10000)
    , nightlyTest $ testProperty "no non trustable peers before caught up state"
                      (testWithIOSimPOR prop_no_non_trustable_peers_before_caught_up_state 10000)
    , testGroup "Churn"
      [ nightlyTest $ testProperty "no timeouts"
                        (testWithIOSimPOR prop_churn_notimeouts 10000)
      , nightlyTest $ testProperty "steps"
                        (testWithIOSimPOR prop_churn_steps 10000)
      ]
    , testGroup "unit"
      [ nightlyTest $ testProperty "unit cm" unit_cm_valid_transitions ]
    ]
  , testGroup "IOSim"
    [ testProperty "no failure"
                   (testWithIOSim prop_diffusion_nofail 125000)
    , testProperty "no livelock"
                   (testWithIOSim prop_diffusion_nolivelock 125000)
    , testProperty "dns can recover from fails"
                   (testWithIOSim prop_diffusion_dns_can_recover 125000)
    , testProperty "unit #4191"
                   unit_4191
    , testProperty "target established public"
                   (testWithIOSim prop_diffusion_target_established_public 125000)
    , testProperty "target active public"
                   (testWithIOSim prop_diffusion_target_active_public 125000)
    , testProperty "target established local"
                   (testWithIOSim prop_diffusion_target_established_local 125000)
    , testProperty "unit reconnect"
                   prop_unit_reconnect
    , testProperty "target active local"
                   (testWithIOSim prop_diffusion_target_active_local 125000)
    , testProperty "target active root"
                   (testWithIOSim prop_diffusion_target_active_root 125000)
    , testProperty "target active below"
                   (testWithIOSim prop_diffusion_target_active_below 125000)
    , testProperty "target active local below"
                   (testWithIOSim prop_diffusion_target_active_local_below 250000)
    , testProperty "async demotion"
                   (testWithIOSim prop_diffusion_async_demotions 125000)
    , testProperty "async demotion (unit)"
                   unit_diffusion_async_demotions
    , testProperty "target active local above"
                   (testWithIOSim prop_diffusion_target_active_local_above 125000)
    , testProperty "connection manager valid transitions"
                   (testWithIOSim prop_diffusion_cm_valid_transitions 125000)
    , testProperty "connection manager valid transition order"
                   (testWithIOSim prop_diffusion_cm_valid_transition_order 125000)
    , testProperty "unit 4258"
                   prop_unit_4258
    , testProperty "connection manager no dodgy traces"
                   (testWithIOSim prop_diffusion_cm_no_dodgy_traces 125000)
    , testProperty "peer selection actions no dodgy traces"
                   (testWithIOSim prop_diffusion_peer_selection_actions_no_dodgy_traces 125000)
    , testProperty "inbound governor valid transitions"
                   (testWithIOSim prop_diffusion_ig_valid_transitions 125000)
    , testProperty "inbound governor valid transition order"
                   (testWithIOSim prop_diffusion_ig_valid_transition_order 125000)
    , testProperty "cm & ig timeouts enforced"
                   (testWithIOSim prop_diffusion_timeouts_enforced 125000)
    , testProperty "any Cold async demotion"
                   (testWithIOSim prop_track_coolingToCold_demotions 125000)
    , testProperty "unit #4177" unit_4177
    , testProperty "connect failure" prop_connect_failure
    , testProperty "accept failure" prop_accept_failure
    , testProperty "only bootstrap peers in fallback state"
                   (testWithIOSim prop_only_bootstrap_peers_in_fallback_state 125000)
    , testProperty "no non trustable peers before caught up state"
                   (testWithIOSim prop_no_non_trustable_peers_before_caught_up_state 125000)
    , testGroup "Peer Sharing"
      [ testProperty "share a peer"
                     unit_peer_sharing
      ]
    , testGroup "Churn"
      [ testProperty "no timeouts"
                     (testWithIOSim prop_churn_notimeouts 125000)
      , testProperty "steps"
                     (testWithIOSim prop_churn_steps 5000)
      ]
    , testGroup "coverage"
      [ testProperty "server trace coverage"
                     prop_server_trace_coverage
      , testProperty "peer selection actions trace coverage"
                     prop_peer_selection_action_trace_coverage
      , testProperty "peer selection trace coverage"
                     prop_peer_selection_trace_coverage
      , testProperty "connection manager trace coverage"
                     prop_connection_manager_trace_coverage
      , testProperty "connection manager transitions coverage"
                     prop_connection_manager_transitions_coverage
      , testProperty "inbound governor trace coverage"
                     prop_inbound_governor_trace_coverage
      , testProperty "inbound governor transitions coverage"
                     prop_inbound_governor_transitions_coverage
      , testProperty "fetch client state trace coverage"
                     prop_fetch_client_state_trace_coverage
      ]
    , testGroup "hot diffusion script"
      [ testProperty "target active public"
                     prop_hot_diffusion_target_active_public
      , testProperty "target active local"
                     prop_hot_diffusion_target_active_local
      , testProperty "target active root"
                     prop_hot_diffusion_target_active_root
      ]
    ]
  ]

traceFromList :: [a] -> Trace (SimResult ()) a
traceFromList = Trace.fromList (MainReturn  (Time 0) (Labelled (ThreadId []) (Just "main")) () [])

testWithIOSim :: (SimTrace Void -> Int -> Property)
              -> Int
              -> AbsBearerInfo
              -> DiffusionScript
              -> Property
testWithIOSim f traceNumber bi ds =
  let sim :: forall s . IOSim s Void
      sim = diffusionSimulation (toBearerInfo bi)
                                ds
                                iosimTracer
   in labelDiffusionScript ds
    $ f (runSimTrace sim) traceNumber

testWithIOSimPOR :: (SimTrace Void -> Int -> Property)
                 -> Int
                 -> AbsBearerInfo
                 -> DiffusionScript
                 -> Property
testWithIOSimPOR f traceNumber bi ds =
  let sim :: forall s . IOSim s Void
      sim = do
        exploreRaces
        diffusionSimulation (toBearerInfo bi)
                            ds
                            iosimTracer
   in labelDiffusionScript ds
    $ exploreSimTrace id sim $ \_ ioSimTrace ->
        f ioSimTrace  traceNumber

-- | This test checks a IOSimPOR false positive bug with the connection
-- manager state transition traces no longer happens.
--
unit_cm_valid_transitions :: Property
unit_cm_valid_transitions =
  let bi = AbsBearerInfo
            { abiConnectionDelay         = SmallDelay
            , abiInboundAttenuation      = NoAttenuation FastSpeed
            , abiOutboundAttenuation     = NoAttenuation FastSpeed
            , abiInboundWriteFailure     = Nothing
            , abiOutboundWriteFailure    = Just 0
            , abiAcceptFailure           = Nothing
            , abiSDUSize                 = LargeSDU
            }
      ds = DiffusionScript
            (SimArgs 1 10)
            (Script ((Map.empty, ShortDelay) :| [(Map.empty, LongDelay)]))
            [ ( NodeArgs
                  (-2)
                  InitiatorAndResponderDiffusionMode
                  (Just 269)
                  (Map.fromList [(RelayAccessAddress "0:71:0:1:0:1:0:1" 65534,
                                  DoAdvertisePeer)])
                  GenesisMode
                  (Script (DontUseBootstrapPeers :| []))
                  (TestAddress (IPAddr (read "0:79::1:0:0") 3))
                  PeerSharingDisabled
                  [ (HotValency {getHotValency = 1},
                     WarmValency {getWarmValency = 1},
                     Map.fromList [(RelayAccessAddress "0:71:0:1:0:1:0:1" 65534,
                                    (DoAdvertisePeer, IsTrustable))])
                   ]
                  (Script (LedgerPools [] :| []))
                  (ConsensusModePeerTargets
                    { deadlineTargets = PeerSelectionTargets
                        { targetNumberOfRootPeers                 = 4
                        , targetNumberOfKnownPeers                = 4
                        , targetNumberOfEstablishedPeers          = 3
                        , targetNumberOfActivePeers               = 2
                        , targetNumberOfKnownBigLedgerPeers       = 4
                        , targetNumberOfEstablishedBigLedgerPeers = 1
                        , targetNumberOfActiveBigLedgerPeers      = 1
                        }
                    , syncTargets = PeerSelectionTargets
                        { targetNumberOfRootPeers                 = 0
                        , targetNumberOfKnownPeers                = 4
                        , targetNumberOfEstablishedPeers          = 0
                        , targetNumberOfActivePeers               = 0
                        , targetNumberOfKnownBigLedgerPeers       = 4
                        , targetNumberOfEstablishedBigLedgerPeers = 4
                        , targetNumberOfActiveBigLedgerPeers      = 3
                        }
                    })
                  (Script (DNSTimeout {getDNSTimeout = 0.325} :| []))
                  (Script (DNSLookupDelay {getDNSLookupDelay = 0.1} :|
                    [DNSLookupDelay {getDNSLookupDelay = 0.072}]))
                  Nothing
                  False
                  (Script (FetchModeBulkSync :| [FetchModeBulkSync]))
                  , [JoinNetwork 0.5]
                )
              , ( NodeArgs
                  0
                  InitiatorAndResponderDiffusionMode
                  (Just 90)
                  Map.empty
                  GenesisMode
                  (Script (DontUseBootstrapPeers :| []))
                  (TestAddress (IPAddr (read "0:71:0:1:0:1:0:1") 65534))
                  PeerSharingEnabled
                  [ (HotValency {getHotValency = 1},
                     WarmValency {getWarmValency = 1},
                     Map.fromList [(RelayAccessAddress "0:79::1:0:0" 3,
                                    (DoNotAdvertisePeer, IsTrustable))])
                   ]
                  (Script (LedgerPools [] :| []))
                  (ConsensusModePeerTargets
                    { deadlineTargets = PeerSelectionTargets
                        { targetNumberOfRootPeers                 = 1
                        , targetNumberOfKnownPeers                = 1
                        , targetNumberOfEstablishedPeers          = 1
                        , targetNumberOfActivePeers               = 1
                        , targetNumberOfKnownBigLedgerPeers       = 4
                        , targetNumberOfEstablishedBigLedgerPeers = 3
                        , targetNumberOfActiveBigLedgerPeers      = 3
                        }
                    , syncTargets = PeerSelectionTargets
                        { targetNumberOfRootPeers                 = 0
                        , targetNumberOfKnownPeers                = 1
                        , targetNumberOfEstablishedPeers          = 1
                        , targetNumberOfActivePeers               = 1
                        , targetNumberOfKnownBigLedgerPeers       = 4
                        , targetNumberOfEstablishedBigLedgerPeers = 2
                        , targetNumberOfActiveBigLedgerPeers      = 2
                        }
                    })
                  (Script (DNSTimeout {getDNSTimeout = 0.18} :| []))
                  (Script (DNSLookupDelay {getDNSLookupDelay = 0.125} :| []))
                  (Just (BlockNo 2))
                  False
                  (Script (FetchModeDeadline :| []))
                  , [JoinNetwork 1.484848484848]
                )
            ]
      s = ControlAwait
            [ ScheduleMod
                (RacyThreadId [3,1,3,1,2,3,2,1], 7)
                ControlDefault
                [ (RacyThreadId [3,1,3,1,2,3,2], 32)
                , (RacyThreadId [3,1,3,1,2,3,2], 33)
                , (RacyThreadId [2,1,3,1,4], 8)
                , (RacyThreadId [2,1,3,1,4], 9)
                , (RacyThreadId [2,1,3,1,4], 10)
                , (RacyThreadId [2,1,3,1,4], 11)
                , (RacyThreadId [2,1,3,1,4], 12)
                , (RacyThreadId [2,1,3,1,4,1], 0)
                , (RacyThreadId [2,1,3,1,4,1], 1)
                , (RacyThreadId [2,1,3,1,4,1], 2)
                , (RacyThreadId [2,1,3,1,4,1], 3)
                , (RacyThreadId [2,1,3,1,4,1], 4)
                , (RacyThreadId [2,1,3,1,4,1], 5)
                , (RacyThreadId [2,1,3,1,4,1], 6)
                , (RacyThreadId [2,1,3,1,4,1], 7)
                , (RacyThreadId [2,1,3,1,4,1], 8)
                , (RacyThreadId [2,1,3,1,4,1,1], 0)
                , (RacyThreadId [2,1,3,1,4,1,1], 1)
                , (RacyThreadId [2,1,3,1,4,1,1], 2)
                , (RacyThreadId [2,1,3,1,4,1,1], 3)
                , (RacyThreadId [2,1,3,1,4,1], 9)
                , (RacyThreadId [2,1,3,1,4,1], 10)
                , (RacyThreadId [2,1,3,1,4,1], 11)
                , (RacyThreadId [2,1,3,1,4,1], 12)
                , (RacyThreadId [2,1,3,1,4,1], 13)
                , (RacyThreadId [2,1,3,1,4,1], 14)
                , (RacyThreadId [2,1,3,1,4,1], 15)
                , (RacyThreadId [2,1,3,1,4], 13)
                , (RacyThreadId [2,1,3,1,4], 14)
                , (RacyThreadId [2,1,3,1,4], 15)
                , (RacyThreadId [2,1,3,1,4], 16)
                , (RacyThreadId [3,1,3,1,2,3,2], 34)
                ]
            ]
      sim :: forall s. IOSim s Void
      sim = do
        exploreRaces
        diffusionSimulation (toBearerInfo bi) ds iosimTracer
  in exploreSimTrace (\a -> a { explorationReplay = Just s }) sim $ \_ ioSimTrace ->
       prop_diffusion_cm_valid_transition_order_iosim_por ioSimTrace 10000

-- | As a basic property we run the governor to explore its state space a bit
-- and check it does not throw any exceptions (assertions such as invariant
-- violations).
--
-- We do /not/ assume freedom from livelock for this property, so we run the
-- governor for a maximum number of trace events rather than for a fixed
-- simulated time.
--
prop_diffusion_nofail :: SimTrace Void
                      -> Int
                      -> Property
prop_diffusion_nofail ioSimTrace traceNumber =
  let trace = Trace.toList
            . fmap (\(WithTime t (WithName _ b)) -> (t, b))
            . withTimeNameTraceEvents
                   @DiffusionTestTrace
                   @NtNAddr
            . Trace.take traceNumber
            $ ioSimTrace

   -- run in `IO` so we can catch the pure 'AssertionFailed' exception
   in ioProperty $ do
     r <-
       evaluate ( List.foldl' (flip seq) True
              $ [ assertPeerSelectionState st ()
                | (_, DiffusionDebugPeerSelectionTrace (TraceGovernorState _ _ st)) <- trace ]
              )
       `catch` \(AssertionFailed _) -> return False
     if r
     then return $ property True
     else do
       putStrLn $ intercalate "\n" $ map show trace
       -- the ioSimTrace is infinite, but it will terminate with `AssertionFailed`
       error "impossible!"

-- | This test coverage of 'CM.Trace' constructors.
--
prop_connection_manager_trace_coverage :: AbsBearerInfo
                                       -> DiffusionScript
                                       -> Property
prop_connection_manager_trace_coverage defaultBearerInfo diffScript =

  let sim :: forall s . IOSim s Void
      sim = diffusionSimulation (toBearerInfo defaultBearerInfo)
                                diffScript
                                iosimTracer

      events :: [CM.Trace
                  NtNAddr
                  (ConnectionHandlerTrace NtNVersion NtNVersionData)]
      events = mapMaybe (\case DiffusionConnectionManagerTrace st -> Just st
                               _                                  -> Nothing
                        )
             . Trace.toList
             . fmap (\(WithTime _ (WithName _ b)) -> b)
             . withTimeNameTraceEvents
                @DiffusionTestTrace
                @NtNAddr
             . Trace.take 125000
             $ runSimTrace sim

      eventsSeenNames = map connectionManagerTraceMap events

   -- TODO: Add checkCoverage here
   in tabulate "connection manager trace" eventsSeenNames
      True

-- | This tests coverage of ConnectionManager transitions.
--
prop_connection_manager_transitions_coverage :: AbsBearerInfo
                                             -> DiffusionScript
                                             -> Property
prop_connection_manager_transitions_coverage defaultBearerInfo diffScript =

  let sim :: forall s . IOSim s Void
      sim = diffusionSimulation (toBearerInfo defaultBearerInfo)
                                diffScript
                                iosimTracer

      events :: [AbstractTransitionTrace NtNAddr]
      events = mapMaybe (\case DiffusionConnectionManagerTransitionTrace st ->
                                   Just st
                               _ -> Nothing
                        )
             . Trace.toList
             . fmap (\(WithTime _ (WithName _ b)) -> b)
             . withTimeNameTraceEvents
                @DiffusionTestTrace
                @NtNAddr
             . Trace.take 125000
             $ runSimTrace sim


      transitionsSeenNames = map (snd . validTransitionMap . ttTransition)
                                 events

   -- TODO: Add checkCoverage here
   in tabulate "connection manager transitions" transitionsSeenNames
      True

-- | This test coverage of InboundGovernorTrace constructors.
--
prop_inbound_governor_trace_coverage :: AbsBearerInfo
                                     -> DiffusionScript
                                     -> Property
prop_inbound_governor_trace_coverage defaultBearerInfo diffScript =

  let sim :: forall s . IOSim s Void
      sim = diffusionSimulation (toBearerInfo defaultBearerInfo)
                                diffScript
                                iosimTracer

      events :: [IG.Trace NtNAddr]
      events = mapMaybe (\case DiffusionInboundGovernorTrace st -> Just st
                               _                                -> Nothing
                        )
             . Trace.toList
             . fmap (\(WithTime _ (WithName _ b)) -> b)
             . withTimeNameTraceEvents
                @DiffusionTestTrace
                @NtNAddr
             . Trace.take 125000
             $ runSimTrace sim

      eventsSeenNames = map inboundGovernorTraceMap events

   -- TODO: Add checkCoverage here
   in tabulate "inbound governor trace" eventsSeenNames
      True

-- | This test coverage of InboundGovernor transitions.
--
prop_inbound_governor_transitions_coverage :: AbsBearerInfo
                                           -> DiffusionScript
                                           -> Property
prop_inbound_governor_transitions_coverage defaultBearerInfo diffScript =
  let sim :: forall s . IOSim s Void
      sim = diffusionSimulation (toBearerInfo defaultBearerInfo)
                                diffScript
                                iosimTracer

      events :: [IG.RemoteTransitionTrace NtNAddr]
      events = mapMaybe (\case DiffusionInboundGovernorTransitionTrace st ->
                                    Just st
                               _ -> Nothing
                        )
             . Trace.toList
             . fmap (\(WithTime _ (WithName _ b)) -> b)
             . withTimeNameTraceEvents
                @DiffusionTestTrace
                @NtNAddr
             . Trace.take 125000
             $ runSimTrace sim

      transitionsSeenNames = map (snd . validRemoteTransitionMap . ttTransition)
                                 events

   -- TODO: Add checkCoverage here
   in tabulate "inbound governor transitions" transitionsSeenNames
      True

-- | This test coverage of TraceFetchClientState BlockHeader constructors,
-- namely accept errors.
--
prop_fetch_client_state_trace_coverage :: AbsBearerInfo
                                       -> DiffusionScript
                                       -> Property
prop_fetch_client_state_trace_coverage defaultBearerInfo diffScript =
  let sim :: forall s . IOSim s Void
      sim = diffusionSimulation (toBearerInfo defaultBearerInfo)
                                diffScript
                                iosimTracer

      events :: [TraceFetchClientState BlockHeader]
      events = mapMaybe (\case DiffusionFetchTrace st ->
                                    Just st
                               _ -> Nothing
                        )
             . Trace.toList
             . fmap (\(WithTime _ (WithName _ b)) -> b)
             . withTimeNameTraceEvents
                @DiffusionTestTrace
                @NtNAddr
             . Trace.take 125000
             $ runSimTrace sim

      transitionsSeenNames = map traceFetchClientStateMap events

   -- TODO: Add checkCoverage here
   in tabulate "fetch client state trace" transitionsSeenNames
      True
  where
    traceFetchClientStateMap :: TraceFetchClientState BlockHeader
                             -> String
    traceFetchClientStateMap AddedFetchRequest{}   = "AddedFetchRequest"
    traceFetchClientStateMap AcknowledgedFetchRequest{} =
      "AcknowledgedFetchRequest"
    traceFetchClientStateMap SendFetchRequest{}    = "SendFetchRequest"
    traceFetchClientStateMap StartedFetchBatch{}   = "StartedFetchBatch"
    traceFetchClientStateMap CompletedBlockFetch{} = "CompletedBlockFetch"
    traceFetchClientStateMap CompletedFetchBatch{} = "CompletedFetchBatch"
    traceFetchClientStateMap RejectedFetchBatch{}  = "RejectedFetchBatch"
    traceFetchClientStateMap (ClientTerminating n) = "ClientTerminating "
                                                   ++ show n

-- | Same as PeerSelection test 'prop_governor_only_bootstrap_peers_in_fallback_state'
--
prop_only_bootstrap_peers_in_fallback_state :: SimTrace Void
                                            -> Int
                                            -> Property
prop_only_bootstrap_peers_in_fallback_state ioSimTrace traceNumber =
  let events :: [Events DiffusionTestTrace]
      events = Trace.toList
             . fmap ( Signal.eventsFromList
                    . fmap (\(WithName _ (WithTime t b)) -> (t, b))
                    )
             . splitWithNameTrace
             . fmap (\(WithTime t (WithName name b)) -> WithName name (WithTime t b))
             . withTimeNameTraceEvents
                @DiffusionTestTrace
                @NtNAddr
             . Trace.take traceNumber
             $ ioSimTrace

     in conjoin
      $ (\ev ->
        let evsList = eventsToList ev
            lastTime = fst
                     . last
                     $ evsList
         in classifySimulatedTime lastTime
          $ classifyNumberOfEvents (length evsList)
          $ verify_only_bootstrap_peers_in_fallback_state ev
        )
      <$> events

  where
    verify_only_bootstrap_peers_in_fallback_state events =
      let govUseBootstrapPeers :: Signal UseBootstrapPeers
          govUseBootstrapPeers =
            selectDiffusionPeerSelectionState Governor.bootstrapPeersFlag events

          govLedgerStateJudgement :: Signal LedgerStateJudgement
          govLedgerStateJudgement =
            selectDiffusionPeerSelectionState Governor.ledgerStateJudgement events

          trJoinKillSig :: Signal JoinedOrKilled
          trJoinKillSig =
              Signal.fromChangeEvents Killed -- Default to TrKillingNode
            . Signal.selectEvents
                (\case TrJoiningNetwork -> Just Joined
                       TrKillingNode    -> Just Killed
                       _                -> Nothing
                )
            . selectDiffusionSimulationTrace
            $ events

          govKnownPeers :: Signal (Set NtNAddr)
          govKnownPeers =
            selectDiffusionPeerSelectionState (KnownPeers.toSet . Governor.knownPeers) events

          govTrustedPeers :: Signal (Set NtNAddr)
          govTrustedPeers =
            selectDiffusionPeerSelectionState
              (\st -> LocalRootPeers.keysSet (LocalRootPeers.clampToTrustable (Governor.localRootPeers st))
                   <> PublicRootPeers.getBootstrapPeers (Governor.publicRootPeers st)
              ) events

          -- Signal.keyedUntil receives 2 functions one that sets start of the
          -- set signal, one that ends it and another that stops all.
          --
          -- In this particular case we want a signal that is keyed beginning
          -- on a TrJoiningNetwork and ends on TrKillingNode, giving us a Signal
          -- with the periods when a node was alive.
          trIsNodeAlive :: Signal Bool
          trIsNodeAlive =
                not . Set.null
            <$> Signal.keyedUntil (fromJoinedOrKilled (Set.singleton ())
                                                      Set.empty)
                                  (fromJoinedOrKilled Set.empty
                                                      (Set.singleton ()))
                                  (const False)
                                  trJoinKillSig

          keepNonTrustablePeersTooLong :: Signal (Set NtNAddr)
          keepNonTrustablePeersTooLong =
            Signal.keyedTimeout
              -- Due to the possibilities of the node being reconfigured
              -- frequently and disconnection timeouts we have to increase
              -- this value
              300 -- seconds
              (\(knownPeers, useBootstrapPeers, trustedPeers, lsj, isAlive) ->
                if isAlive && requiresBootstrapPeers useBootstrapPeers lsj
                   then knownPeers `Set.difference` trustedPeers
                   else Set.empty
              )
              ((,,,,) <$> govKnownPeers
                      <*> govUseBootstrapPeers
                      <*> govTrustedPeers
                      <*> govLedgerStateJudgement
                       <*> trIsNodeAlive
              )
       in counterexample (intercalate "\n" $ map show $ Signal.eventsToList events)
        $ signalProperty 20 show
            Set.null
            keepNonTrustablePeersTooLong

-- | Same as PeerSelection test 'prop_governor_no_non_trustable_peers_before_caught_up_state'
--
prop_no_non_trustable_peers_before_caught_up_state :: SimTrace Void
                                                   -> Int
                                                   -> Property
prop_no_non_trustable_peers_before_caught_up_state ioSimTrace traceNumber =
  let events :: [Events DiffusionTestTrace]
      events = Trace.toList
             . fmap ( Signal.eventsFromList
                    . fmap (\(WithName _ (WithTime t b)) -> (t, b))
                    )
             . splitWithNameTrace
             . fmap (\(WithTime t (WithName name b)) -> WithName name (WithTime t b))
             . withTimeNameTraceEvents
                @DiffusionTestTrace
                @NtNAddr
             . Trace.take traceNumber
             $ ioSimTrace

     in conjoin
      $ (\ev ->
        let evsList = eventsToList ev
            lastTime = fst
                     . last
                     $ evsList
         in classifySimulatedTime lastTime
          $ classifyNumberOfEvents (length evsList)
          $ verify_no_non_trustable_peers_before_caught_up_state ev
        )
      <$> events

  where
    verify_no_non_trustable_peers_before_caught_up_state events =
      let govUseBootstrapPeers :: Signal UseBootstrapPeers
          govUseBootstrapPeers =
            selectDiffusionPeerSelectionState Governor.bootstrapPeersFlag
                                              events

          govLedgerStateJudgement :: Signal LedgerStateJudgement
          govLedgerStateJudgement =
            selectDiffusionPeerSelectionState Governor.ledgerStateJudgement
                                              events

          govKnownPeers :: Signal (Set NtNAddr)
          govKnownPeers =
            selectDiffusionPeerSelectionState (KnownPeers.toSet . Governor.knownPeers)
                                              events

          govTrustedPeers :: Signal (Set NtNAddr)
          govTrustedPeers =
            selectDiffusionPeerSelectionState
              (\st -> LocalRootPeers.keysSet (LocalRootPeers.clampToTrustable (Governor.localRootPeers st))
                   <> PublicRootPeers.getBootstrapPeers (Governor.publicRootPeers st)
              )
              events

          govHasOnlyBootstrapPeers :: Signal Bool
          govHasOnlyBootstrapPeers =
            selectDiffusionPeerSelectionState Governor.hasOnlyBootstrapPeers events

          trJoinKillSig :: Signal JoinedOrKilled
          trJoinKillSig =
              Signal.fromChangeEvents Killed -- Default to TrKillingNode
            . Signal.selectEvents
                (\case TrJoiningNetwork -> Just Joined
                       TrKillingNode    -> Just Killed
                       _                -> Nothing
                )
            . selectDiffusionSimulationTrace
            $ events

          -- Signal.keyedUntil receives 2 functions one that sets start of the
          -- set signal, one that ends it and another that stops all.
          --
          -- In this particular case we want a signal that is keyed beginning
          -- on a TrJoiningNetwork and ends on TrKillingNode, giving us a Signal
          -- with the periods when a node was alive.
          trIsNodeAlive :: Signal Bool
          trIsNodeAlive =
                not . Set.null
            <$> Signal.keyedUntil (fromJoinedOrKilled (Set.singleton ())
                                                      Set.empty)
                                  (fromJoinedOrKilled Set.empty
                                                      (Set.singleton ()))
                                  (const False)
                                  trJoinKillSig

          keepNonTrustablePeersTooLong :: Signal (Set NtNAddr)
          keepNonTrustablePeersTooLong =
            Signal.keyedTimeout
              10 -- seconds
              (\( knownPeers, trustedPeers
                , useBootstrapPeers, lsj, hasOnlyBootstrapPeers, isAlive) ->
                  if isAlive && hasOnlyBootstrapPeers && requiresBootstrapPeers useBootstrapPeers lsj
                     then Set.difference knownPeers trustedPeers
                     else Set.empty
              )
              ((,,,,,) <$> govKnownPeers
                       <*> govTrustedPeers
                       <*> govUseBootstrapPeers
                       <*> govLedgerStateJudgement
                       <*> govHasOnlyBootstrapPeers
                       <*> trIsNodeAlive
              )

       in counterexample (intercalate "\n" $ map show $ Signal.eventsToList events)
        $ signalProperty 20 show
            Set.null
            keepNonTrustablePeersTooLong

-- | Unit test which covers issue #4177
--
-- Reconfiguration of local root peers should not remove peers which are being
-- demoted.
unit_4177 :: Property
unit_4177 = prop_inbound_governor_transitions_coverage absNoAttenuation script
  where
    script :: DiffusionScript
    script =
      DiffusionScript (SimArgs 1 10)
        (singletonTimedScript Map.empty)
        [ ( NodeArgs (-6) InitiatorAndResponderDiffusionMode (Just 180)
              (Map.fromList [(RelayAccessDomain "test2" 65535, DoAdvertisePeer)])
              PraosMode
              (Script (UseBootstrapPeers [RelayAccessDomain "bootstrap" 00000] :| []))
              (TestAddress (IPAddr (read "0:7:0:7::") 65533))
              PeerSharingDisabled
              [ (1,1,Map.fromList [(RelayAccessDomain "test2" 65535,(DoNotAdvertisePeer, IsNotTrustable))
              , (RelayAccessAddress "0:6:0:3:0:6:0:5" 65530,(DoNotAdvertisePeer, IsNotTrustable))])
              ]
              (Script (LedgerPools [] :| []))
              ConsensusModePeerTargets {
                deadlineTargets = nullPeerSelectionTargets {
                    targetNumberOfKnownPeers = 2,
                    targetNumberOfEstablishedPeers = 2,
                    targetNumberOfActivePeers = 1,

                    targetNumberOfKnownBigLedgerPeers = 0,
                    targetNumberOfEstablishedBigLedgerPeers = 0,
                    targetNumberOfActiveBigLedgerPeers = 0 },
                syncTargets = nullPeerSelectionTargets }
              (Script (DNSTimeout {getDNSTimeout = 0.239} :| [DNSTimeout {getDNSTimeout = 0.181},DNSTimeout {getDNSTimeout = 0.185},DNSTimeout {getDNSTimeout = 0.14},DNSTimeout {getDNSTimeout = 0.221}]))
              (Script (DNSLookupDelay {getDNSLookupDelay = 0.067} :| [DNSLookupDelay {getDNSLookupDelay = 0.097},DNSLookupDelay {getDNSLookupDelay = 0.101},DNSLookupDelay {getDNSLookupDelay = 0.096},DNSLookupDelay {getDNSLookupDelay = 0.051}]))
              Nothing
              False
              (Script (FetchModeDeadline :| []))
          , [JoinNetwork 1.742857142857
            ,Reconfigure 6.33333333333 [(1,1,Map.fromList [(RelayAccessDomain "test2" 65535,(DoAdvertisePeer, IsNotTrustable))]),
                                        (1,1,Map.fromList [(RelayAccessAddress "0:6:0:3:0:6:0:5" 65530,(DoAdvertisePeer, IsNotTrustable))
                                       ])]
            ,Reconfigure 23.88888888888 [(1,1,Map.empty),(1,1,Map.fromList [(RelayAccessAddress "0:6:0:3:0:6:0:5" 65530,(DoAdvertisePeer, IsNotTrustable))])]
            ,Reconfigure 4.870967741935 [(1,1,Map.fromList [(RelayAccessDomain "test2" 65535,(DoAdvertisePeer, IsNotTrustable))])]
            ]
          )
        , ( NodeArgs 1 InitiatorAndResponderDiffusionMode (Just 135)
             (Map.fromList [(RelayAccessAddress "0:7:0:7::" 65533, DoAdvertisePeer)])
             PraosMode
              (Script (UseBootstrapPeers [RelayAccessDomain "bootstrap" 00000] :| []))
             (TestAddress (IPAddr (read "0:6:0:3:0:6:0:5") 65530))
             PeerSharingDisabled
             []
             (Script (LedgerPools [] :| []))
             ConsensusModePeerTargets {
               deadlineTargets = nullPeerSelectionTargets {
                   targetNumberOfRootPeers = 2,
                   targetNumberOfKnownPeers = 5,
                   targetNumberOfEstablishedPeers = 1,
                   targetNumberOfActivePeers = 1 },
               syncTargets = nullPeerSelectionTargets }
             (Script (DNSTimeout {getDNSTimeout = 0.28}
                  :| [DNSTimeout {getDNSTimeout = 0.204},
                      DNSTimeout {getDNSTimeout = 0.213}
                     ]))
             (Script (DNSLookupDelay {getDNSLookupDelay = 0.066}
                  :| [DNSLookupDelay {getDNSLookupDelay = 0.102},
                      DNSLookupDelay {getDNSLookupDelay = 0.031}
                     ]))
             Nothing
             False
             (Script (FetchModeDeadline :| []))
          , [JoinNetwork 0.183783783783
            ,Reconfigure 4.533333333333 [(1,1,Map.empty)]
            ]
          )
        ]


-- | Attempt to reproduce local root disconnect bug
--
-- Configure relay A to have relay B as localroot peer.
-- Start relay B then start relay A.
-- Verify that the relay A is connected to relay B.
-- Then just restart relay B.
-- The connection will never be re-established again.
--
prop_track_coolingToCold_demotions :: SimTrace Void
                                   -> Int
                                   -> Property
prop_track_coolingToCold_demotions ioSimTracer traceNumber =
  let events :: [Events DiffusionTestTrace]
      events = Trace.toList
             . fmap ( Signal.eventsFromList
                    . fmap (\(WithName _ (WithTime t b)) -> (t, b))
                    )
             . splitWithNameTrace
             . fmap (\(WithTime t (WithName name b)) -> WithName name (WithTime t b))
             . withTimeNameTraceEvents
                @DiffusionTestTrace
                @NtNAddr
             . Trace.take traceNumber
             $ ioSimTracer

     in conjoin
      $ (\ev ->
        let evsList = eventsToList ev
            lastTime = fst
                     . last
                     $ evsList
         in classifySimulatedTime lastTime
          $ classifyNumberOfEvents (length evsList)
          $ verify_coolingToColdDemotions ev
        )
      <$> events

  where
    verify_coolingToColdDemotions :: Events DiffusionTestTrace
                                  -> Property
    verify_coolingToColdDemotions events =
      let govInProgressDemoteToCold :: Signal (Set NtNAddr)
          govInProgressDemoteToCold =
            selectDiffusionPeerSelectionState
              Governor.inProgressDemoteToCold
              events


          trJoinKillSig :: Signal JoinedOrKilled
          trJoinKillSig =
              Signal.fromChangeEvents Killed -- Default to TrKillingNode
            . Signal.selectEvents
                (\case TrJoiningNetwork -> Just Joined
                       TrKillingNode    -> Just Killed
                       _                -> Nothing
                )
            . selectDiffusionSimulationTrace
            $ events

          -- Signal.keyedUntil receives 2 functions one that sets start of the
          -- set signal, one that ends it and another that stops all.
          --
          -- In this particular case we want a signal that is keyed beginning
          -- on a TrJoiningNetwork and ends on TrKillingNode, giving us a Signal
          -- with the periods when a node was alive.
          trIsNodeAlive :: Signal Bool
          trIsNodeAlive =
                not . Set.null
            <$> Signal.keyedUntil (fromJoinedOrKilled (Set.singleton ())
                                                      Set.empty)
                                  (fromJoinedOrKilled Set.empty
                                                      (Set.singleton ()))
                                  (const False)
                                  trJoinKillSig

          govInProgressDemoteToColdWhileAlive :: Signal (Maybe (Set NtNAddr))
          govInProgressDemoteToColdWhileAlive =
            (\isAlive inProgressDemoteToCold ->
              if isAlive then Just inProgressDemoteToCold
                         else Nothing
            ) <$> trIsNodeAlive
              <*> govInProgressDemoteToCold

          allInProgressDemoteToCold :: [NtNAddr]
          allInProgressDemoteToCold = Set.toList
                                    . Set.unions
                                    . mapMaybe snd
                                    . Signal.eventsToList
                                    . Signal.toChangeEvents
                                    $ govInProgressDemoteToColdWhileAlive

          notInProgressDemoteToColdForTooLong =
            map (\addr ->
                  Signal.keyedTimeout
                    120
                    (\case
                        Just s | Set.member addr s -> Set.singleton addr
                        _                          -> Set.empty
                    )
                    govInProgressDemoteToColdWhileAlive
                )
                allInProgressDemoteToCold

       in conjoin
        $ map (signalProperty 20 show Set.null) notInProgressDemoteToColdForTooLong

-- | This test coverage of ServerTrace constructors, namely accept errors.
--
prop_server_trace_coverage :: AbsBearerInfo
                           -> DiffusionScript
                           -> Property
prop_server_trace_coverage defaultBearerInfo diffScript =

  let sim :: forall s . IOSim s Void
      sim = diffusionSimulation (toBearerInfo defaultBearerInfo)
                                diffScript
                                iosimTracer

      events :: [Server.Trace NtNAddr]
      events = mapMaybe (\case DiffusionServerTrace st -> Just st
                               _                       -> Nothing
                        )
             . Trace.toList
             . fmap (\(WithTime _ (WithName _ b)) -> b)
             . withTimeNameTraceEvents
                @DiffusionTestTrace
                @NtNAddr
             . Trace.take 125000
             $ runSimTrace sim

      eventsSeenNames = map serverTraceMap events

   -- TODO: Add checkCoverage here
   in tabulate "server trace" eventsSeenNames
      True

-- | This test coverage of PeerSelectionActionsTrace constructors.
--
prop_peer_selection_action_trace_coverage :: AbsBearerInfo
                                          -> DiffusionScript
                                          -> Property
prop_peer_selection_action_trace_coverage defaultBearerInfo diffScript =
  let sim :: forall s . IOSim s Void
      sim = diffusionSimulation (toBearerInfo defaultBearerInfo)
                                diffScript
                                iosimTracer

      events :: [PeerSelectionActionsTrace NtNAddr NtNVersion]
      events = mapMaybe (\case DiffusionPeerSelectionActionsTrace st -> Just st
                               _                                     -> Nothing
                        )
             . Trace.toList
             . fmap (\(WithTime _ (WithName _ b)) -> b)
             . withTimeNameTraceEvents
                @DiffusionTestTrace
                @NtNAddr
             . Trace.take 125000
             $ runSimTrace sim

      peerSelectionActionsTraceMap :: PeerSelectionActionsTrace NtNAddr NtNVersion
                                   -> String
      peerSelectionActionsTraceMap (PeerStatusChanged _)          =
        "PeerStatusChanged"
      peerSelectionActionsTraceMap (PeerStatusChangeFailure _ ft) =
        "PeerStatusChangeFailure " ++ show ft
      peerSelectionActionsTraceMap (PeerMonitoringError _ se)     =
        "PeerMonitoringError " ++ show se
      peerSelectionActionsTraceMap (PeerMonitoringResult _ wspt)  =
        "PeerMonitoringResult " ++ show wspt
      peerSelectionActionsTraceMap (AcquireConnectionError e)      =
        "AcquireConnectionError " ++ show e

      eventsSeenNames = map peerSelectionActionsTraceMap events

   -- TODO: Add checkCoverage here
   in tabulate "peer selection actions trace" eventsSeenNames
      True

-- | This test coverage of TracePeerSelection constructors.
--
prop_peer_selection_trace_coverage :: AbsBearerInfo
                                   -> DiffusionScript
                                   -> Property
prop_peer_selection_trace_coverage defaultBearerInfo diffScript =
  let sim :: forall s . IOSim s Void
      sim = diffusionSimulation (toBearerInfo defaultBearerInfo)
                                diffScript
                                iosimTracer

      events :: [TracePeerSelection NtNAddr]
      events = mapMaybe (\case DiffusionPeerSelectionTrace st -> Just st
                               _                              -> Nothing
                        )
             . Trace.toList
             . fmap (\(WithTime _ (WithName _ b)) -> b)
             . withTimeNameTraceEvents
                @DiffusionTestTrace
                @NtNAddr
             . Trace.take 125000
             $ runSimTrace sim

      peerSelectionTraceMap :: TracePeerSelection NtNAddr -> String
      peerSelectionTraceMap TraceLocalRootPeersChanged {}            =
        "TraceLocalRootPeersChanged"
      peerSelectionTraceMap TraceTargetsChanged {}                   =
        "TraceTargetsChanged"
      peerSelectionTraceMap TracePublicRootsRequest {}               =
        "TracePublicRootsRequest"
      peerSelectionTraceMap TracePublicRootsResults {}               =
        "TracePublicRootsResults"
      peerSelectionTraceMap (TracePublicRootsFailure se _ _)         =
        "TracePublicRootsFailure " ++ show se
      peerSelectionTraceMap TracePeerShareRequests {}                =
        "TracePeerShareRequests"
      peerSelectionTraceMap TracePeerShareResults {}                 =
        "TracePeerShareResults"
      peerSelectionTraceMap TracePeerShareResultsFiltered {}         =
        "TracePeerShareResultsFiltered"
      peerSelectionTraceMap TracePickInboundPeers {}                 =
        "TracePickInboundInboundPeers"
      peerSelectionTraceMap TraceForgetColdPeers {}                  =
        "TraceForgetColdPeers"
      peerSelectionTraceMap TracePromoteColdPeers {}                 =
        "TracePromoteColdPeers"
      peerSelectionTraceMap TracePromoteColdLocalPeers {}            =
        "TracePromoteColdLocalPeers"
      peerSelectionTraceMap TracePromoteColdFailed {}                =
        "TracePromoteColdFailed"
      peerSelectionTraceMap TracePromoteColdDone {}                  =
        "TracePromoteColdDone"
      peerSelectionTraceMap TracePromoteWarmPeers {}                 =
        "TracePromoteWarmPeers"
      peerSelectionTraceMap TracePromoteWarmLocalPeers {}            =
        "TracePromoteWarmLocalPeers"
      peerSelectionTraceMap TracePromoteWarmFailed {}                =
        "TracePromoteWarmFailed"
      peerSelectionTraceMap TracePromoteWarmDone {}                  =
        "TracePromoteWarmDone"
      peerSelectionTraceMap TracePromoteWarmAborted {}               =
        "TracePromoteWarmAborted"
      peerSelectionTraceMap TraceDemoteWarmPeers {}                  =
        "TraceDemoteWarmPeers"
      peerSelectionTraceMap TraceDemoteWarmFailed {}                 =
        "TraceDemoteWarmFailed"
      peerSelectionTraceMap TraceDemoteWarmDone {}                   =
        "TraceDemoteWarmDone"
      peerSelectionTraceMap TraceDemoteHotPeers {}                   =
        "TraceDemoteHotPeers"
      peerSelectionTraceMap TraceDemoteLocalHotPeers {}              =
        "TraceDemoteLocalHotPeers"
      peerSelectionTraceMap TraceDemoteHotFailed {}                  =
        "TraceDemoteHotFailed"
      peerSelectionTraceMap TraceDemoteHotDone {}                    =
        "TraceDemoteHotDone"
      peerSelectionTraceMap TraceDemoteAsynchronous {}               =
        "TraceDemoteAsynchronous"
      peerSelectionTraceMap TraceDemoteLocalAsynchronous {}          =
        "TraceDemoteLocalAsynchronous"
      peerSelectionTraceMap TraceGovernorWakeup                      =
        "TraceGovernorWakeup"
      peerSelectionTraceMap TraceChurnWait {}                        =
        "TraceChurnWait"
      peerSelectionTraceMap (TraceChurnMode cm)                      =
        "TraceChurnMode " ++ show cm
      peerSelectionTraceMap TraceForgetBigLedgerPeers {}             =
        "TraceForgetBigLedgerPeers"
      peerSelectionTraceMap TraceBigLedgerPeersRequest {}            =
        "TraceBigLedgerPeersRequest"
      peerSelectionTraceMap TraceBigLedgerPeersResults {}            =
        "TraceBigLedgerPeersResults"
      peerSelectionTraceMap TraceBigLedgerPeersFailure {}            =
        "TraceBigLedgerPeersFailure"
      peerSelectionTraceMap TracePromoteColdBigLedgerPeers {}        =
        "TracePromoteColdBigLedgerPeers"
      peerSelectionTraceMap TracePromoteColdBigLedgerPeerFailed {}   =
        "TracePromoteColdBigLedgerPeerFailed"
      peerSelectionTraceMap TracePromoteColdBigLedgerPeerDone {}     =
        "TracePromoteColdBigLedgerPeerDone"
      peerSelectionTraceMap TracePromoteWarmBigLedgerPeers {}        =
        "TracePromoteWarmBigLedgerPeers"
      peerSelectionTraceMap TracePromoteWarmBigLedgerPeerFailed {}   =
        "TracePromoteWarmBigLedgerPeerFailed"
      peerSelectionTraceMap TracePromoteWarmBigLedgerPeerDone {}     =
        "TracePromoteWarmBigLedgerPeerDone"
      peerSelectionTraceMap TracePromoteWarmBigLedgerPeerAborted {}  =
        "TracePromoteWarmBigLedgerPeerAborted"
      peerSelectionTraceMap TraceDemoteWarmBigLedgerPeers {}         =
        "TraceDemoteWarmBigLedgerPeers"
      peerSelectionTraceMap TraceDemoteWarmBigLedgerPeerFailed {}    =
        "TraceDemoteWarmBigLedgerPeerFailed"
      peerSelectionTraceMap TraceDemoteWarmBigLedgerPeerDone {}      =
        "TraceDemoteWarmBigLedgerPeerDone"
      peerSelectionTraceMap TraceDemoteHotBigLedgerPeers {}          =
        "TraceDemoteHotBigLedgerPeers"
      peerSelectionTraceMap TraceDemoteHotBigLedgerPeerFailed {}     =
        "TraceDemoteHotBigLedgerPeerFailed"
      peerSelectionTraceMap TraceDemoteHotBigLedgerPeerDone {}       =
        "TraceDemoteHotBigLedgerPeerDone"
      peerSelectionTraceMap TraceDemoteBigLedgerPeersAsynchronous {} =
        "TraceDemoteBigLedgerPeersAsynchronous"
      peerSelectionTraceMap (TraceLedgerStateJudgementChanged lsj)   =
        "TraceLedgerStateJudgementChanged " ++ show lsj
      peerSelectionTraceMap TraceOnlyBootstrapPeers                  =
        "TraceOnlyBootstrapPeers"
      peerSelectionTraceMap TraceBootstrapPeersFlagChangedWhilstInSensitiveState =
        "TraceBootstrapPeersFlagChangedWhilstInSensitiveState"
      peerSelectionTraceMap (TraceUseBootstrapPeersChanged {}) =
        "TraceUseBootstrapPeersChanged"
      peerSelectionTraceMap (TraceOutboundGovernorCriticalFailure {}) =
        "TraceOutboundGovernorCriticalFailure"
      peerSelectionTraceMap TraceDebugState {}                       =
        "TraceDebugState"
      peerSelectionTraceMap a@TraceChurnAction {}                    =
        show a
      peerSelectionTraceMap a@TraceChurnTimeout {}                   =
        show a
      peerSelectionTraceMap (TraceVerifyPeerSnapshot result)         =
        "TraceVerifyPeerSnapshot " <> show result
      eventsSeenNames = map peerSelectionTraceMap events

   -- TODO: Add checkCoverage here
   in tabulate "peer selection trace" eventsSeenNames
      True

-- | A variant of
-- 'Test.Ouroboros.Network.ConnectionHandler.Network.PeerSelection.prop_governor_nolivelock'
-- but for running on Diffusion. This test doesn't check for events occuring at the same
-- time but rather for events happening between an interval (usual 1s). This is because,
-- since Diffusion is much more complex and can run more than 1 node in parallel, time
-- might progress but very slowly almost like a livelock. We want to safeguard from such
-- cases.
--
prop_diffusion_nolivelock :: SimTrace Void
                          -> Int
                          -> Property
prop_diffusion_nolivelock ioSimTrace traceNumber =
    let trace :: [(Time, ThreadId (IOSim s), Maybe ThreadLabel, SimEventType)]
        trace = take traceNumber
              . traceEvents
              $ ioSimTrace

        lastTime :: Time
        lastTime = getTime (last trace)

     in classifySimulatedTime lastTime
      $ check_governor_nolivelock (secondsToDiffTime 0)
                                  trace
  where
    check_governor_nolivelock :: DiffTime
                              -> [(Time, ThreadId (IOSim s), Maybe ThreadLabel, SimEventType)]
                              -> Property
    check_governor_nolivelock dt trace =
      let trace' = (\(t, tid, tl, e) -> (t, (tid, tl, e)))
                 <$> trace

          numberOfEvents = 10000 * 5 -- 5 is the maximum number of nodes in a simulation

       in case tooManyEventsBeforeTimeAdvances numberOfEvents dt trace' of
            Nothing -> property True
            Just es ->
              counterexample
                ("over " ++ show numberOfEvents ++ " events in "
                ++ show dt ++ "\n" ++ "first " ++ show numberOfEvents
                ++ " events: " ++ (unlines . map show . take numberOfEvents $ es))
              $ property False

    tooManyEventsBeforeTimeAdvances :: Int
                                    -> DiffTime
                                    -> [(Time, e)]
                                    -> Maybe [(Time, e)]
    tooManyEventsBeforeTimeAdvances _         _  []     = Nothing
    tooManyEventsBeforeTimeAdvances threshold dt trace0 =
        go (groupByTime dt trace0)
      where
        groupByTime :: DiffTime -> [(Time, e)] -> [[(Time, e)]]
        groupByTime _ [] = []
        groupByTime dtime trace@((t, _):_) =
          let (tl, tr) = span (\(t', _) -> diffTime t' t <= dtime) trace
           in tl : groupByTime dtime tr

        go :: [[(Time, e)]] -> Maybe [(Time, e)]
        go []    = Nothing
        go (h:t)
          | countdown threshold h = go t
          | otherwise = Just h

        countdown 0 (_ : _)  = False
        countdown _ []       = True
        countdown n (_ : es) = countdown (n-1) es

-- | Test that verifies that that we can recover from DNS lookup failures.
--
-- This checks that if a node is configured with a local root peer through DNS,
-- and then the peer gets disconnected, the DNS lookup fails (so you can’t
-- reconnect). After a bit DNS lookup succeeds and you manage to connect again.
--
prop_diffusion_dns_can_recover :: SimTrace Void
                               -> Int
                               -> Property
prop_diffusion_dns_can_recover ioSimTrace traceNumber =
    let events :: [Events DiffusionTestTrace]
        events = Trace.toList
               . fmap ( Signal.eventsFromList
                      . fmap (\(WithName _ (WithTime t b)) -> (t, b))
                      )
               . splitWithNameTrace
               . fmap (\(WithTime t (WithName name b)) -> WithName name (WithTime t b))
               . withTimeNameTraceEvents
                  @DiffusionTestTrace
                  @NtNAddr
               . Trace.take traceNumber
               $ ioSimTrace

     in conjoin
      $ (\ev ->
        let evsList = eventsToList ev
            lastTime = fst
                     . last
                     $ evsList
         in classifySimulatedTime lastTime
          $ classifyNumberOfEvents (length evsList)
          $ verify_dns_can_recover ev
        )
      <$> events

  where

    -- | Policy for TTL for positive results
    -- | Policy for TTL for negative results
    -- Cache negative response for 3hrs
    -- Otherwise, use exponential backoff, up to a limit
    ttlForDnsError :: DNS.DNSError -> DiffTime -> DiffTime
    ttlForDnsError DNS.NameError _ = 10800
    ttlForDnsError _           ttl = clipTTLAbove (ttl * 2 + 5)

    ttlForResults :: [DNS.TTL] -> DiffTime
    -- This case says we have a successful reply but there is no answer.
    -- This covers for example non-existent TLDs since there is no authority
    -- to say that they should not exist.
    ttlForResults []   = ttlForDnsError DNS.NameError 0
    ttlForResults ttls = clipTTLBelow
                       . clipTTLAbove
                       . (fromIntegral :: Word32 -> DiffTime)
                       $ maximum ttls

    -- | Limit insane TTL choices.
    clipTTLAbove :: DiffTime -> DiffTime
    clipTTLAbove = min 86400  -- and 24hrs

    clipTTLBelow :: DiffTime -> DiffTime
    clipTTLBelow = max 60  -- and 1 min

    verify_dns_can_recover :: Events DiffusionTestTrace -> Property
    verify_dns_can_recover events =
        verify Map.empty Map.empty 0 (Time 0) (Signal.eventsToList events)

    verify :: Map DNS.Domain Time
           -> Map DNS.Domain DiffTime
           -> Int
           -> Time
           -> [(Time, DiffusionTestTrace)]
           -> Property
    verify toRecover ttlMap recovered time [] =
      counterexample (show toRecover ++ " none of these DNS names recovered\n"
                     ++ "Final time: " ++ show time ++ "\n"
                     ++ "TTL time: " ++ show ttlMap ++ "\n"
                     ++ "Number of recovered: " ++ show recovered )
                     (all (>= time) toRecover || recovered > 0)
    verify toRecover ttlMap recovered time ((t, ev):evs) =
      case ev of
        DiffusionLocalRootPeerTrace
          (TraceLocalRootFailure dap (DNSError err)) ->
            let dns = dapDomain dap
                ttl = fromMaybe 0 $ Map.lookup dns ttlMap
                ttl' = ttlForDnsError err ttl
                ttlMap' = Map.insert dns ttl' ttlMap
             in verify (Map.insert dns (addTime ttl' t) toRecover)
                        ttlMap'
                        recovered t evs
        DiffusionLocalRootPeerTrace
          (TraceLocalRootReconfigured _ _) ->
            verify Map.empty ttlMap recovered t evs
        DiffusionLocalRootPeerTrace (TraceLocalRootResult dap r) ->
          let dns = dapDomain dap
              ttls = map snd r
              ttlMap' = Map.insert dns (ttlForResults ttls) ttlMap
           in case Map.lookup dns toRecover of
                Nothing -> verify toRecover ttlMap' recovered t evs
                Just _  -> verify (Map.delete dns toRecover)
                                  ttlMap'
                                  (recovered + 1)
                                  t
                                  evs
        DiffusionDiffusionSimulationTrace TrReconfiguringNode ->
          verify Map.empty ttlMap recovered t evs
        _ -> verify toRecover ttlMap recovered time evs


-- | Unit test which covers issue #4191
--
unit_4191 :: Property
unit_4191 = testWithIOSim prop_diffusion_dns_can_recover 125000 absInfo script
  where
    ioerr =
      IOError
        { ioe_handle      = Nothing,
          ioe_type        = ResourceVanished,
          ioe_location    = "AttenuationChannel",
          ioe_description = "attenuation",
          ioe_errno       = Nothing,
          ioe_filename    = Nothing
        }
    absInfo =
      AbsBearerInfo
        { abiConnectionDelay = SmallDelay,
          abiInboundAttenuation = NoAttenuation NormalSpeed,
          abiOutboundAttenuation = ErrorInterval NormalSpeed (Time 17.666666666666) 888 ioerr,
          abiInboundWriteFailure = Nothing,
          abiOutboundWriteFailure = Just 2,
          abiAcceptFailure = Nothing, abiSDUSize = LargeSDU
        }
    script =
      DiffusionScript
        (SimArgs 1 20)
        (singletonTimedScript $
           Map.fromList
             [ ("test2", [ (read "810b:4c8a:b3b5:741:8c0c:b437:64cf:1bd9", 300)
                         , (read "254.167.216.215", 300)
                         , (read "27.173.29.254", 300)
                         , (read "61.238.34.238", 300)
                         , (read "acda:b62d:6d7d:50f7:27b6:7e34:2dc6:ee3d", 300)
                         ])
             , ("test3", [ (read "903e:61bc:8b2f:d98f:b16e:5471:c83d:4430", 300)
                         , (read "19.40.90.161", 300)
                         ])
             ])
        [(NodeArgs
            16
            InitiatorAndResponderDiffusionMode
            (Just 224)
            Map.empty
            PraosMode
            (Script (UseBootstrapPeers [RelayAccessDomain "bootstrap" 00000] :| []))
            (TestAddress (IPAddr (read "0.0.1.236") 65527))
            PeerSharingDisabled
            [ (2,2,Map.fromList [ (RelayAccessDomain "test2" 15,(DoNotAdvertisePeer, IsNotTrustable))
                                , (RelayAccessDomain "test3" 4,(DoAdvertisePeer, IsNotTrustable))])
            ]
            (Script (LedgerPools [] :| []))
            ConsensusModePeerTargets {
              deadlineTargets = PeerSelectionTargets
                { targetNumberOfRootPeers = 6,
                  targetNumberOfKnownPeers = 7,
                  targetNumberOfEstablishedPeers = 7,
                  targetNumberOfActivePeers = 6,

                  targetNumberOfKnownBigLedgerPeers = 0,
                  targetNumberOfEstablishedBigLedgerPeers = 0,
                  targetNumberOfActiveBigLedgerPeers = 0
                },
              syncTargets = nullPeerSelectionTargets }
            (Script (DNSTimeout {getDNSTimeout = 0.406} :| [ DNSTimeout {getDNSTimeout = 0.11}
                                                           , DNSTimeout {getDNSTimeout = 0.333}
                                                           , DNSTimeout {getDNSTimeout = 0.352}
                                                           , DNSTimeout {getDNSTimeout = 0.123}
                                                           , DNSTimeout {getDNSTimeout = 0.12}
                                                           , DNSTimeout {getDNSTimeout = 0.23}
                                                           , DNSTimeout {getDNSTimeout = 0.311}
                                                           , DNSTimeout {getDNSTimeout = 0.37}
                                                           , DNSTimeout {getDNSTimeout = 0.153}
                                                           , DNSTimeout {getDNSTimeout = 0.328}
                                                           , DNSTimeout {getDNSTimeout = 0.239}
                                                           , DNSTimeout {getDNSTimeout = 0.261}
                                                           , DNSTimeout {getDNSTimeout = 0.15}
                                                           , DNSTimeout {getDNSTimeout = 0.26}
                                                           , DNSTimeout {getDNSTimeout = 0.37}
                                                           , DNSTimeout {getDNSTimeout = 0.28}
                                                           ]))
            (Script (DNSLookupDelay {getDNSLookupDelay = 0.124} :| [ DNSLookupDelay {getDNSLookupDelay = 0.11}
                                                                   , DNSLookupDelay {getDNSLookupDelay = 0.129}
                                                                   , DNSLookupDelay {getDNSLookupDelay = 0.066}
                                                                   , DNSLookupDelay {getDNSLookupDelay = 0.125}
                                                                   , DNSLookupDelay {getDNSLookupDelay = 0.046}
                                                                   , DNSLookupDelay {getDNSLookupDelay = 0.135}
                                                                   , DNSLookupDelay {getDNSLookupDelay = 0.05}
                                                                   , DNSLookupDelay {getDNSLookupDelay = 0.039}
                                                                   ]))
            Nothing
            False
            (Script (FetchModeDeadline :| []))
            , [ JoinNetwork 6.710144927536
              , Kill 7.454545454545
              , JoinNetwork 10.763157894736
              , Reconfigure 0.415384615384 [(1,1,Map.empty)
              , (1,1,Map.empty)]
              , Reconfigure 15.550561797752 [(1,1,Map.empty)
              , (1,1,Map.fromList [(RelayAccessDomain "test2" 15,(DoAdvertisePeer, IsNotTrustable))])]
              , Reconfigure 82.85714285714 []
              ])
        ]


-- | Verify that some connect failures are fatal.
--
prop_connect_failure :: AbsIOError -> Property
prop_connect_failure (AbsIOError ioerr) =
  label (if isFatal (ioe_type ioerr) then "fatal IOError" else "non-fatal IOError") $
  testWithIOSim
    (\trace _ ->
       let -- events generated by `nodeAddr`
           events :: Events DiffusionTestTrace
           events = Signal.eventsFromList
                  . map (\(WithTime t b) -> (t, b))
                  . Trace.toList
                  . fmap (\(WithTime t (WithName _ b)) -> WithTime t b)
                  . Trace.filter (\(WithTime _ (WithName name _)) -> name == nodeAddr)
                  . withTimeNameTraceEvents
                     @DiffusionTestTrace
                     @NtNAddr
                  . Trace.take noEvents
                  $ trace
           evs = eventsToList (selectDiffusionSimulationTrace events)
       in  counterexample (Trace.ppTrace show (ppSimEvent 0 0 0) . Trace.take noEvents $ trace)
         . counterexample (show evs)
         . (if isFatal (ioe_type ioerr)
           then -- verify that the node was killed by the right exception
                any (\case
                      TrErrored e | Just (ExceptionInHandler _ e') <- fromException e
                                  , Just e'' <- fromException e'
                                  , e'' == ioerr
                                  -> True
                      _           -> False)
           else -- verify that the ndoe was not killed by the `ioerr` exception
                all (\case
                      TrErrored {} -> False
                      _            -> True)
           )
         . map snd
         $ evs
    ) noEvents absInfo script
  where
    -- must be in sync with rethrowPolicy in `Ouroboros.Network.Diffusion.P2P`
    isFatal :: IOErrorType -> Bool
    isFatal ResourceExhausted    = True
    isFatal UnsupportedOperation = True
    isFatal InvalidArgument      = True
    isFatal ProtocolError        = True
    isFatal _                    = False

    noEvents = 5000
    absInfo =
      AbsBearerInfo
        { abiConnectionDelay = SmallDelay,
          abiInboundAttenuation = NoAttenuation NormalSpeed,
          abiOutboundAttenuation = ErrorInterval NormalSpeed (Time 0) 1000 ioerr,
          abiInboundWriteFailure = Nothing,
          abiOutboundWriteFailure = Nothing,
          abiAcceptFailure = Nothing, abiSDUSize = LargeSDU
        }

    nodeIP   = read "10.0.0.0"
    nodePort = 1
    nodeAddr = TestAddress (IPAddr nodeIP nodePort)

    relayIP   = read "10.0.0.1"
    relayPort = 1

    script =
      DiffusionScript
        (SimArgs 1 20)
        (singletonTimedScript Map.empty)
        [ (NodeArgs {
            naSeed = 0,
            naDiffusionMode = InitiatorAndResponderDiffusionMode,
            naMbTime = Just 224,
            naPublicRoots = Map.empty,
            naConsensusMode = PraosMode,
            naBootstrapPeers = Script (DontUseBootstrapPeers :| []),
            naAddr = TestAddress (IPAddr nodeIP nodePort),
            naPeerSharing = PeerSharingDisabled,
            naLocalRootPeers = [(1,1,Map.fromList [(RelayAccessAddress relayIP relayPort,(DoNotAdvertisePeer, IsNotTrustable))])],
            naLedgerPeers = Script (LedgerPools [] :| []),
            naPeerTargets = ConsensusModePeerTargets {
              deadlineTargets = PeerSelectionTargets {
                targetNumberOfRootPeers = 1,
                targetNumberOfKnownPeers = 1,
                targetNumberOfEstablishedPeers = 1,
                targetNumberOfActivePeers = 1,

                targetNumberOfKnownBigLedgerPeers = 0,
                targetNumberOfEstablishedBigLedgerPeers = 0,
                targetNumberOfActiveBigLedgerPeers = 0
              },
              syncTargets = nullPeerSelectionTargets
            },
            naDNSTimeoutScript = Script (DNSTimeout {getDNSTimeout = 0} :| []),
            naDNSLookupDelayScript = Script (DNSLookupDelay {getDNSLookupDelay = 0} :| []),
            naChainSyncExitOnBlockNo = Nothing,
            naChainSyncEarlyExit = False,
            naFetchModeScript = Script (FetchModeDeadline :| [])
          }
          , [JoinNetwork 10]
          ),
          (NodeArgs {
            naSeed = 0,
            naDiffusionMode = InitiatorAndResponderDiffusionMode,
            naMbTime = Just 224,
            naPublicRoots = Map.empty,
            naConsensusMode = PraosMode,
            naBootstrapPeers = Script (DontUseBootstrapPeers :| []),
            naAddr = TestAddress (IPAddr relayIP relayPort),
            naPeerSharing = PeerSharingDisabled,
            naLocalRootPeers = [],
            naLedgerPeers = Script (LedgerPools [] :| []),
            naPeerTargets = ConsensusModePeerTargets {
              deadlineTargets = PeerSelectionTargets {
                targetNumberOfRootPeers = 0,
                targetNumberOfKnownPeers = 0,
                targetNumberOfEstablishedPeers = 0,
                targetNumberOfActivePeers = 0,

                targetNumberOfKnownBigLedgerPeers = 0,
                targetNumberOfEstablishedBigLedgerPeers = 0,
                targetNumberOfActiveBigLedgerPeers = 0
              },
              syncTargets = nullPeerSelectionTargets
            },
            naDNSTimeoutScript = Script (DNSTimeout {getDNSTimeout = 0} :| []),
            naDNSLookupDelayScript = Script (DNSLookupDelay {getDNSLookupDelay = 0} :| []),
            naChainSyncExitOnBlockNo = Nothing,
            naChainSyncEarlyExit = False,
            naFetchModeScript = Script (FetchModeDeadline :| [])
          }
          , [JoinNetwork 0]
          )
        ]


-- | Verify that some connect failures are fatal.
--
prop_accept_failure :: AbsIOError -> Property
prop_accept_failure (AbsIOError ioerr) =
  label (if isFatal ioerr then "fatal IOError" else "non-fatal IOError") $
  testWithIOSim
    (\trace _ ->
       let -- events generated by `nodeAddr`
           events :: Events DiffusionTestTrace
           events = Signal.eventsFromList
                  . map (\(WithTime t b) -> (t, b))
                  . Trace.toList
                  . fmap (\(WithTime t (WithName _ b)) -> WithTime t b)
                  . Trace.filter (\(WithTime _ (WithName name _)) -> name == relayAddr)
                  . withTimeNameTraceEvents
                     @DiffusionTestTrace
                     @NtNAddr
                  . Trace.take noEvents
                  $ trace
           evs = eventsToList (selectDiffusionSimulationTrace events)
       in  -- counterexample (Trace.ppTrace show (ppSimEvent 0 0 0) . Trace.take noEvents $ trace)
           counterexample (show evs)
         . (if isFatal ioerr
           then -- verify that the node was killed by the right exception
                any (\case
                      TrErrored e | Just e' <- fromException e
                                  , e' == ioerr
                                  -> True
                      _           -> False)
           else -- verify that the node was not killed by the `ioerr` exception
                all (\case
                      TrErrored {} -> False
                      _            -> True)
           )
         . map snd
         $ evs
    ) noEvents absInfo script
  where
    -- only ECONNABORTED errors are not fatal
    isFatal :: IOError -> Bool
    isFatal = not . Server.isECONNABORTED

    noEvents = 10000
    absInfo =
      AbsBearerInfo
        { abiConnectionDelay = SmallDelay,
          abiInboundAttenuation = NoAttenuation NormalSpeed,
          abiOutboundAttenuation = NoAttenuation NormalSpeed,
          abiInboundWriteFailure = Nothing,
          abiOutboundWriteFailure = Nothing,
          abiAcceptFailure = Just (SmallDelay, ioerr),
          abiSDUSize = LargeSDU
        }

    nodeIP   = read "10.0.0.0"
    nodePort = 1

    relayIP   = read "10.0.0.1"
    relayPort = 1
    relayAddr = TestAddress (IPAddr relayIP relayPort)

    script =
      DiffusionScript
        (SimArgs 1 20)
        (singletonTimedScript Map.empty)
        [ (NodeArgs {
            naSeed = 0,
            naDiffusionMode = InitiatorAndResponderDiffusionMode,
            naMbTime = Just 224,
            naPublicRoots = Map.empty,
            naConsensusMode = PraosMode,
            naBootstrapPeers = Script (DontUseBootstrapPeers :| []),
            naAddr = TestAddress (IPAddr nodeIP nodePort),
            naPeerSharing = PeerSharingDisabled,
            naLocalRootPeers = [(1,1,Map.fromList [(RelayAccessAddress relayIP relayPort,(DoNotAdvertisePeer, IsNotTrustable))])],
            naLedgerPeers = Script (LedgerPools [] :| []),
            naPeerTargets = ConsensusModePeerTargets {
              deadlineTargets = PeerSelectionTargets {
                targetNumberOfRootPeers = 1,
                targetNumberOfKnownPeers = 1,
                targetNumberOfEstablishedPeers = 1,
                targetNumberOfActivePeers = 1,

                targetNumberOfKnownBigLedgerPeers = 0,
                targetNumberOfEstablishedBigLedgerPeers = 0,
                targetNumberOfActiveBigLedgerPeers = 0
              },
              syncTargets = nullPeerSelectionTargets
            },
            naDNSTimeoutScript = Script (DNSTimeout {getDNSTimeout = 0} :| []),
            naDNSLookupDelayScript = Script (DNSLookupDelay {getDNSLookupDelay = 0} :| []),
            naChainSyncExitOnBlockNo = Nothing,
            naChainSyncEarlyExit = False,
            naFetchModeScript = Script (FetchModeDeadline :| [])
          }
          , [JoinNetwork 10]
          ),
          (NodeArgs {
            naSeed = 0,
            naDiffusionMode = InitiatorAndResponderDiffusionMode,
            naMbTime = Just 224,
            naPublicRoots = Map.empty,
            naConsensusMode = PraosMode,
            naBootstrapPeers = Script (DontUseBootstrapPeers :| []),
            naAddr = TestAddress (IPAddr relayIP relayPort),
            naPeerSharing = PeerSharingDisabled,
            naLocalRootPeers = [],
            naLedgerPeers = Script (LedgerPools [] :| []),
            naPeerTargets = ConsensusModePeerTargets {
              deadlineTargets = PeerSelectionTargets {
                targetNumberOfRootPeers = 0,
                targetNumberOfKnownPeers = 0,
                targetNumberOfEstablishedPeers = 0,
                targetNumberOfActivePeers = 0,

                targetNumberOfKnownBigLedgerPeers = 0,
                targetNumberOfEstablishedBigLedgerPeers = 0,
                targetNumberOfActiveBigLedgerPeers = 0
              },
              syncTargets = nullPeerSelectionTargets
            },
            naDNSTimeoutScript = Script (DNSTimeout {getDNSTimeout = 0} :| []),
            naDNSLookupDelayScript = Script (DNSLookupDelay {getDNSLookupDelay = 0} :| []),
            naChainSyncExitOnBlockNo = Nothing,
            naChainSyncEarlyExit = False,
            naFetchModeScript = Script (FetchModeDeadline :| [])
          }
          , [JoinNetwork 0]
          )
        ]


-- | A variant of
-- 'Test.Ouroboros.Network.PeerSelection.prop_governor_target_established_public'
-- but for running on Diffusion. This means it has to have in consideration the
-- the logs for all nodes running will all appear in the trace and the test
-- property should only be valid while a given node is up and running.
--
-- We do not need separate above and below variants of this property since it
-- is not possible to exceed the target.
--
prop_diffusion_target_established_public :: SimTrace Void
                                         -> Int
                                         -> Property
prop_diffusion_target_established_public ioSimTrace traceNumber =
    let events :: [Events DiffusionTestTrace]
        events = Trace.toList
               . fmap ( Signal.eventsFromList
                      . fmap (\(WithName _ (WithTime t b)) -> (t, b))
                      )
               . splitWithNameTrace
               . fmap (\(WithTime t (WithName name b)) -> WithName name (WithTime t b))
               . withTimeNameTraceEvents
                  @DiffusionTestTrace
                  @NtNAddr
               . Trace.take traceNumber
               $ ioSimTrace

     in conjoin
      $ (\ev ->
        let evsList = eventsToList ev
            lastTime = fst
                     . last
                     $ evsList
         in classifySimulatedTime lastTime
          $ classifyNumberOfEvents (length evsList)
          $ verify_target_established_public ev
        )
      <$> events
  where
    verify_target_established_public :: Events DiffusionTestTrace
                                     -> Property
    verify_target_established_public events =
      let govPublicRootPeersSig :: Signal (Set NtNAddr)
          govPublicRootPeersSig =
            selectDiffusionPeerSelectionState
              (PublicRootPeers.toSet . Governor.publicRootPeers)
              events

          govEstablishedPeersSig :: Signal (Set NtNAddr)
          govEstablishedPeersSig =
            selectDiffusionPeerSelectionState
              (EstablishedPeers.toSet . Governor.establishedPeers)
              events

          govInProgressPromoteColdSig :: Signal (Set NtNAddr)
          govInProgressPromoteColdSig =
            selectDiffusionPeerSelectionState
              Governor.inProgressPromoteCold
              events

          publicInEstablished :: Signal Bool
          publicInEstablished =
            (\publicPeers established inProgressPromoteCold ->
              Set.size
              (publicPeers `Set.intersection`
                (established `Set.union` inProgressPromoteCold))
                > 0
            ) <$> govPublicRootPeersSig
              <*> govEstablishedPeersSig
              <*> govInProgressPromoteColdSig

          meaning :: Bool -> String
          meaning False = "No PublicPeers in Established Set"
          meaning True  = "PublicPeers in Established Set"

          valuesList :: [String]
          valuesList = map (meaning . snd)
                     . Signal.eventsToList
                     . Signal.toChangeEvents
                     $ publicInEstablished

       in checkCoverage
        $ coverTable "established public peers"
                     [("PublicPeers in Established Set", 1)]
        $ tabulate "established public peers" valuesList
          True

-- | A variant of
-- 'Test.Ouroboros.Network.PeerSelection.prop_governor_target_active_public'
-- but for running on Diffusion. This means it has to have in consideration the
-- the logs for all nodes running will all appear in the trace and the test
-- property should only be valid while a given node is up and running.
--
prop_diffusion_target_active_public :: SimTrace Void
                                    -> Int
                                    -> Property
prop_diffusion_target_active_public ioSimTrace traceNumber =
    let events :: [Events DiffusionTestTrace]
        events = Trace.toList
               . fmap ( Signal.eventsFromList
                      . fmap (\(WithName _ (WithTime t b)) -> (t, b))
                      )
               . splitWithNameTrace
               . fmap (\(WithTime t (WithName name b)) -> WithName name (WithTime t b))
               . withTimeNameTraceEvents
                  @DiffusionTestTrace
                  @NtNAddr
               . Trace.take traceNumber
               $ ioSimTrace

     in conjoin
      $ (\ev ->
        let evsList = eventsToList ev
            lastTime = fst
                     . last
                     $ evsList
         in classifySimulatedTime lastTime
          $ classifyNumberOfEvents (length evsList)
          $ verify_target_active_public ev
        )
      <$> events
  where
    verify_target_active_public :: Events DiffusionTestTrace
                                -> Property
    verify_target_active_public events =
        let govPublicRootPeersSig :: Signal (Set NtNAddr)
            govPublicRootPeersSig =
              selectDiffusionPeerSelectionState
                (PublicRootPeers.toSet . Governor.publicRootPeers)
                events

            govActivePeersSig :: Signal (Set NtNAddr)
            govActivePeersSig =
              selectDiffusionPeerSelectionState Governor.activePeers
                                                events

            publicInActive :: Signal Bool
            publicInActive =
              (\publicPeers active ->
                Set.size
                (publicPeers `Set.intersection` active)
                  > 0
              ) <$> govPublicRootPeersSig
                <*> govActivePeersSig

            meaning :: Bool -> String
            meaning False = "No PublicPeers in Active Set"
            meaning True  = "PublicPeers in Active Set"

            valuesList :: [String]
            valuesList = map (meaning . snd)
                       . Signal.eventsToList
                       . Signal.toChangeEvents
                       $ publicInActive

         in checkCoverage
          $ coverTable "active public peers"
                       [("PublicPeers in Active Set", 1)]
          $ tabulate "active public peers" valuesList
            True


-- | This test checks the percentage of local root peers that, at some point,
-- become active.
--
prop_diffusion_target_active_local :: SimTrace Void
                                   -> Int
                                   -> Property
prop_diffusion_target_active_local ioSimTrace traceNumber =
    let events :: [Events DiffusionTestTrace]
        events = Trace.toList
               . fmap ( Signal.eventsFromList
                      . fmap (\(WithName _ (WithTime t b)) -> (t, b))
                      )
               . splitWithNameTrace
               . fmap (\(WithTime t (WithName name b)) -> WithName name (WithTime t b))
               . withTimeNameTraceEvents
                  @DiffusionTestTrace
                  @NtNAddr
               . Trace.take traceNumber
               $ ioSimTrace

     in conjoin
      $ (\ev ->
        let evsList = eventsToList ev
            lastTime = fst
                     . last
                     $ evsList
         in classifySimulatedTime lastTime
          $ classifyNumberOfEvents (length evsList)
          $ verify_target_active_local ev
        )
      <$> events
  where
    verify_target_active_local :: Events DiffusionTestTrace
                               -> Property
    verify_target_active_local events =
        let govLocalRootPeersSig :: Signal (Set NtNAddr)
            govLocalRootPeersSig =
              selectDiffusionPeerSelectionState
                (LocalRootPeers.keysSet . Governor.localRootPeers)
                events

            govActivePeersSig :: Signal (Set NtNAddr)
            govActivePeersSig =
              selectDiffusionPeerSelectionState Governor.activePeers
              events

            localInActive :: Signal Bool
            localInActive =
              (\localPeers active ->
                Set.size
                (localPeers `Set.intersection` active)
                  > 0
              ) <$> govLocalRootPeersSig
                <*> govActivePeersSig

            meaning :: Bool -> String
            meaning False = "No LocalPeers in Active Set"
            meaning True  = "LocalPeers in Active Set"

            valuesList :: [String]
            valuesList = map (meaning . snd)
                       . Signal.eventsToList
                       . Signal.toChangeEvents
                       $ localInActive

         in checkCoverage
          $ coverTable "active local peers"
                       [("LocalPeers in Active Set", 1)]
          $ tabulate "active local peers" valuesList
            True

-- | This test checks that there's at least one root or local root peers in the
-- active set.  This is a statistical tests which is not enforced.
--
-- This test is somewhat similar to `prop_governor_target_active_public`,
-- however that test enforces network level timeouts.
--
prop_diffusion_target_active_root :: SimTrace Void
                                  -> Int
                                  -> Property
prop_diffusion_target_active_root ioSimTrace traceNumber =
    let events :: [Events DiffusionTestTrace]
        events = Trace.toList
               . fmap ( Signal.eventsFromList
                      . fmap (\(WithName _ (WithTime t b)) -> (t, b))
                      )
               . splitWithNameTrace
               . fmap (\(WithTime t (WithName name b)) -> WithName name (WithTime t b))
               . withTimeNameTraceEvents
                  @DiffusionTestTrace
                  @NtNAddr
               . Trace.take traceNumber
               $ ioSimTrace

    in  conjoin
      $ (\ev ->
        let evsList = eventsToList ev
            lastTime = fst
                     . last
                     $ evsList
         in classifySimulatedTime lastTime
          $ classifyNumberOfEvents (length evsList)
          $ verify_target_active_root ev
        )
      <$> events
  where
    verify_target_active_root :: Events DiffusionTestTrace
                              -> Property
    verify_target_active_root events =
        let govLocalRootPeersSig :: Signal (Set NtNAddr)
            govLocalRootPeersSig =
              selectDiffusionPeerSelectionState
                (LocalRootPeers.keysSet . Governor.localRootPeers) events

            govPublicRootPeersSig :: Signal (Set NtNAddr)
            govPublicRootPeersSig =
              selectDiffusionPeerSelectionState
                (PublicRootPeers.toSet . Governor.publicRootPeers) events

            govRootPeersSig :: Signal (Set NtNAddr)
            govRootPeersSig = Set.union <$> govLocalRootPeersSig
                                        <*> govPublicRootPeersSig

            govActivePeersSig :: Signal (Set NtNAddr)
            govActivePeersSig =
              selectDiffusionPeerSelectionState Governor.activePeers events

            rootInActive :: Signal Bool
            rootInActive =
              (\rootPeers active ->
                Set.size
                (rootPeers `Set.intersection` active)
                  > 0
              ) <$> govRootPeersSig
                <*> govActivePeersSig

            meaning :: Bool -> String
            meaning False = "No Root Peers in Active Set"
            meaning True  = "Root Peers in Active Set"

            valuesList :: [String]
            valuesList = map (meaning . snd)
                       . Signal.eventsToList
                       . Signal.toChangeEvents
                       $ rootInActive

         in checkCoverage
          $ coverTable "active root peers"
                       [("Root Peers in Active Set", 1)]
          $ tabulate "active root peers" valuesList
            True


-- | This test checks the percentage of public root peers that, at some point,
-- become active, when using the 'HotDiffusionScript' generator.
--
prop_hot_diffusion_target_active_public :: NonFailingAbsBearerInfo
                                        -> HotDiffusionScript
                                        -> Property
prop_hot_diffusion_target_active_public defaultBearerInfo (HotDiffusionScript sa dns hds) =
  testWithIOSim prop_diffusion_target_active_public 125000 (unNFBI defaultBearerInfo) (DiffusionScript sa dns hds)

-- | This test checks the percentage of local root peers that, at some point,
-- become active, when using the 'HotDiffusionScript' generator.
--
prop_hot_diffusion_target_active_local :: NonFailingAbsBearerInfo
                                       -> HotDiffusionScript
                                       -> Property
prop_hot_diffusion_target_active_local defaultBearerInfo (HotDiffusionScript sa dns hds) =
  testWithIOSim prop_diffusion_target_active_local 125000 (unNFBI defaultBearerInfo) (DiffusionScript sa dns hds)

-- | This test checks the percentage of root peers that, at some point,
-- become active, when using the 'HotDiffusionScript' generator.
--
prop_hot_diffusion_target_active_root :: NonFailingAbsBearerInfo
                                      -> HotDiffusionScript
                                      -> Property
prop_hot_diffusion_target_active_root defaultBearerInfo (HotDiffusionScript sa dns hds) =
  testWithIOSim prop_diffusion_target_active_root 125000 (unNFBI defaultBearerInfo) (DiffusionScript sa dns hds)

-- | A variant of
-- 'Test.Ouroboros.Network.PeerSelection.prop_governor_target_established_local'
-- but for running on Diffusion. This means it has to have in consideration the
-- the logs for all nodes running will all appear in the trace and the test
-- property should only be valid while a given node is up and running.
--
-- We do not need separate above and below variants of this property since it
-- is not possible to exceed the target.
--
prop_diffusion_target_established_local :: SimTrace Void
                                        -> Int
                                        -> Property
prop_diffusion_target_established_local ioSimTrace traceNumber =
    let events :: [Events DiffusionTestTrace]
        events = Trace.toList
               . fmap ( Signal.eventsFromList
                      . fmap (\(WithName _ (WithTime t b)) -> (t, b))
                      )
               . splitWithNameTrace
               . fmap (\(WithTime t (WithName name b)) -> WithName name (WithTime t b))
               . withTimeNameTraceEvents
                  @DiffusionTestTrace
                  @NtNAddr
               . Trace.take traceNumber
               $ ioSimTrace

    in conjoin
      $ (\ev ->
        let evsList = eventsToList ev
            lastTime = fst
                     . last
                     $ evsList
         in classifySimulatedTime lastTime
          $ classifyNumberOfEvents (length evsList)
          $ verify_target_established_local ev
        )
      <$> events

  where
    verify_target_established_local :: Events DiffusionTestTrace
                                    -> Property
    verify_target_established_local events  =
      let govLocalRootPeersSig :: Signal (LocalRootPeers.LocalRootPeers NtNAddr)
          govLocalRootPeersSig =
            selectDiffusionPeerSelectionState Governor.localRootPeers events

          govInProgressPromoteColdSig :: Signal (Set NtNAddr)
          govInProgressPromoteColdSig =
            selectDiffusionPeerSelectionState
              Governor.inProgressPromoteCold
              events

          govInProgressDemoteToColdSig :: Signal (Set NtNAddr)
          govInProgressDemoteToColdSig =
            selectDiffusionPeerSelectionState
              Governor.inProgressDemoteToCold
              events

          govEstablishedPeersSig :: Signal (Set NtNAddr)
          govEstablishedPeersSig =
            selectDiffusionPeerSelectionState
              ( EstablishedPeers.toSet
              . Governor.establishedPeers)
              events

          govEstablishedFailuresSig :: Signal (Set NtNAddr)
          govEstablishedFailuresSig =
              Signal.keyedLinger
                180 -- 3 minutes  -- TODO: too eager to reconnect?
                (fromMaybe Set.empty)
            . Signal.fromEvents
            . Signal.selectEvents
                (\case TracePromoteColdFailed _ _ peer _ _ ->
                         Just (Set.singleton peer)
                       --TODO: what about TraceDemoteWarmDone ?
                       -- these are also not immediate candidates
                       -- why does the property not fail for not tracking these?
                       TraceDemoteAsynchronous status
                         | Set.null failures -> Nothing
                         | otherwise         -> Just failures
                         where
                           failures =
                             Map.keysSet (Map.filter (==PeerCooling) . fmap fst $ status)
                       TraceDemoteLocalAsynchronous status
                         | Set.null failures -> Nothing
                         | otherwise         -> Just failures
                         where
                           failures =
                             Map.keysSet (Map.filter (==PeerCooling) . fmap fst $ status)
                       TracePromoteWarmFailed _ _ peer _ ->
                         Just (Set.singleton peer)
                       _ -> Nothing
                )
            . selectDiffusionPeerSelectionEvents
            $ events

          trJoinKillSig :: Signal JoinedOrKilled
          trJoinKillSig =
              Signal.fromChangeEvents Killed -- Default to TrKillingNode
            . Signal.selectEvents
                (\case TrJoiningNetwork -> Just Joined
                       TrKillingNode    -> Just Killed
                       _                -> Nothing
                )
            . selectDiffusionSimulationTrace
            $ events

          -- Signal.keyedUntil receives 2 functions one that sets start of the
          -- set signal, one that ends it and another that stops all.
          --
          -- In this particular case we want a signal that is keyed beginning
          -- on a TrJoiningNetwork and ends on TrKillingNode, giving us a Signal
          -- with the periods when a node was alive.
          trIsNodeAlive :: Signal Bool
          trIsNodeAlive =
                not . Set.null
            <$> Signal.keyedUntil (fromJoinedOrKilled (Set.singleton ())
                                                      Set.empty)
                                  (fromJoinedOrKilled Set.empty
                                                      (Set.singleton ()))
                                  (const False)
                                  trJoinKillSig

          promotionOpportunities :: Signal (Set NtNAddr)
          promotionOpportunities =
            (\local established recentFailures inProgressPromoteCold isAlive inProgressDemoteToCold ->
              if isAlive then
                Set.unions
                  [ -- There are no opportunities if we're at or above target
                    if Set.size groupEstablished >= warmTarget
                       then Set.empty
                       else groupEstablished Set.\\ established
                                             Set.\\ recentFailures
                                             Set.\\ inProgressPromoteCold
                                             Set.\\ inProgressDemoteToCold
                  | (_, WarmValency warmTarget, group) <- LocalRootPeers.toGroupSets local
                  , let groupEstablished = group `Set.intersection` established
                  ]
              else Set.empty
            ) <$> govLocalRootPeersSig
              <*> govEstablishedPeersSig
              <*> govEstablishedFailuresSig
              <*> govInProgressPromoteColdSig
              <*> trIsNodeAlive
              <*> govInProgressDemoteToColdSig

          promotionOpportunitiesIgnoredTooLong :: Signal (Set NtNAddr)
          promotionOpportunitiesIgnoredTooLong =
            Signal.keyedTimeout
              15 -- seconds
              id
              promotionOpportunities

       in counterexample
            ("\nSignal key: (local root peers, established peers, " ++
             "recent failures, is alive, opportunities, ignored too long)\n" ++
               intercalate "\n" (map show $ eventsToList events)
            )
        $ signalProperty 20 show
              (\(_,_,_,_,_,_, tooLong) -> Set.null tooLong)
              ((,,,,,,) <$> govLocalRootPeersSig
                      <*> govEstablishedPeersSig
                      <*> govEstablishedFailuresSig
                      <*> govInProgressPromoteColdSig
                      <*> trIsNodeAlive
                      <*> promotionOpportunities
                      <*> promotionOpportunitiesIgnoredTooLong
              )

-- | A variant of
-- 'Test.Ouroboros.Network.PeerSelection.prop_governor_target_active_below'
-- but for running on Diffusion. This means it has to have in consideration the
-- the logs for all nodes running will all appear in the trace and the test
-- property should only be valid while a given node is up and running.
--
prop_diffusion_target_active_below :: SimTrace Void
                                   -> Int
                                   -> Property
prop_diffusion_target_active_below ioSimTrace traceNumber =
    let events :: [Events DiffusionTestTrace]
        events = Trace.toList
               . fmap ( Signal.eventsFromList
                      . fmap (\(WithName _ (WithTime t b)) -> (t, b))
                      )
               . splitWithNameTrace
               . fmap (\(WithTime t (WithName name b)) -> WithName name (WithTime t b))
               . withTimeNameTraceEvents
                  @DiffusionTestTrace
                  @NtNAddr
               . Trace.take traceNumber
               $ ioSimTrace

     in conjoin
      $ (\ev ->
        let evsList = eventsToList ev
            lastTime = fst
                     . last
                     $ evsList
         in classifySimulatedTime lastTime
          $ classifyNumberOfEvents (length evsList)
          $ verify_target_active_below ev
        )
      <$> events

  where
    verify_target_active_below :: Events DiffusionTestTrace
                               -> Property
    verify_target_active_below events =
      let govLocalRootPeersSig :: Signal (LocalRootPeers.LocalRootPeers NtNAddr)
          govLocalRootPeersSig =
            selectDiffusionPeerSelectionState Governor.localRootPeers events

          govActiveTargetsSig :: Signal Int
          govActiveTargetsSig =
            selectDiffusionPeerSelectionState
              (targetNumberOfActivePeers . Governor.targets)
              events

          govInProgressDemoteToColdSig :: Signal (Set NtNAddr)
          govInProgressDemoteToColdSig =
            selectDiffusionPeerSelectionState
              Governor.inProgressDemoteToCold
              events

          govEstablishedPeersSig :: Signal (Set NtNAddr)
          govEstablishedPeersSig =
            selectDiffusionPeerSelectionState
              (dropBigLedgerPeers $
                 EstablishedPeers.toSet . Governor.establishedPeers)
              events

          govActivePeersSig :: Signal (Set NtNAddr)
          govActivePeersSig =
            selectDiffusionPeerSelectionState
              (dropBigLedgerPeers Governor.activePeers)
              events

          govActiveFailuresSig :: Signal (Set NtNAddr)
          govActiveFailuresSig =
              Signal.keyedLinger
                180 -- 3 minutes  -- TODO: too eager to reconnect?
                (fromMaybe Set.empty)
            . Signal.fromEvents
            . Signal.selectEvents
                (\case TracePromoteWarmFailed _ _ peer _ ->
                         --TODO: the environment does not yet cause this to happen
                         -- it requires synchronous failure in the establish
                         -- action
                         Just (Set.singleton peer)
                       --TODO
                       TraceDemoteAsynchronous status
                         | Set.null failures -> Nothing
                         | otherwise         -> Just failures
                         where
                           failures = Map.keysSet (Map.filter (==PeerWarm) . fmap fst $ status)
                       TraceDemoteLocalAsynchronous status
                         | Set.null failures -> Nothing
                         | otherwise         -> Just failures
                         where
                           failures = Map.keysSet (Map.filter (==PeerWarm) . fmap fst $ status)
                       TracePromoteWarmBigLedgerPeerFailed _ _ peer _ ->
                         Just (Set.singleton peer)
                       TraceDemoteBigLedgerPeersAsynchronous status
                         | Set.null failures -> Nothing
                         | otherwise -> Just failures
                         where
                           failures = Map.keysSet (Map.filter ((==PeerCooling) . fst) status)
                       _ -> Nothing
                )
            . selectDiffusionPeerSelectionEvents
            $ events

          govInProgressPromoteWarmSig :: Signal (Set NtNAddr)
          govInProgressPromoteWarmSig =
            selectDiffusionPeerSelectionState Governor.inProgressPromoteWarm  events

          trJoinKillSig :: Signal JoinedOrKilled
          trJoinKillSig =
              Signal.fromChangeEvents Killed -- Default to TrKillingNode
            . Signal.selectEvents
                (\case TrJoiningNetwork -> Just Joined
                       TrKillingNode    -> Just Killed
                       _                -> Nothing
                )
            . selectDiffusionSimulationTrace
            $ events

          -- Signal.keyedUntil receives 2 functions one that sets start of the
          -- set signal, one that ends it and another that stops all.
          --
          -- In this particular case we want a signal that is keyed beginning
          -- on a TrJoiningNetwork and ends on TrKillingNode, giving us a Signal
          -- with the periods when a node was alive.
          trIsNodeAlive :: Signal Bool
          trIsNodeAlive =
                not . Set.null
            <$> Signal.keyedUntil (fromJoinedOrKilled (Set.singleton ())
                                                      Set.empty)
                                  (fromJoinedOrKilled Set.empty
                                                      (Set.singleton ()))
                                  (const False)
                                  trJoinKillSig

          -- There are no opportunities if we're at or above target.
          --
          -- We define local root peers not to be promotion opportunities for
          -- the purpose of the general target of active peers.
          -- The local root peers have a separate target with a separate property.
          -- And we cannot count local peers since we can have corner cases where
          -- the only choices are local roots in a group that is already at target
          -- but the general target is still higher. In such situations we do not
          -- want to promote any, since we'd then be above target for the local
          -- root peer group.
          --
          promotionOpportunity target local established active recentFailures isAlive
                               inProgressDemoteToCold inProgressPromoteWarm
            | isAlive && Set.size active < target
            = established Set.\\ active
                          Set.\\ LocalRootPeers.keysSet local
                          Set.\\ recentFailures
                          Set.\\ inProgressDemoteToCold
                          Set.\\ inProgressPromoteWarm

            | otherwise
            = Set.empty

          promotionOpportunities :: Signal (Set NtNAddr)
          promotionOpportunities =
            promotionOpportunity
              <$> govActiveTargetsSig
              <*> govLocalRootPeersSig
              <*> govEstablishedPeersSig
              <*> govActivePeersSig
              <*> govActiveFailuresSig
              <*> trIsNodeAlive
              <*> govInProgressDemoteToColdSig
              <*> govInProgressPromoteWarmSig

          promotionOpportunitiesIgnoredTooLong :: Signal (Set NtNAddr)
          promotionOpportunitiesIgnoredTooLong =
            Signal.keyedTimeout
              10 -- seconds
              id
              promotionOpportunities

       in counterexample
            ("\nSignal key: (local, established peers, active peers, " ++
             "recent failures, opportunities, is node running, ignored too long)") $
          counterexample
            (intercalate "\n" $ map show $ Signal.eventsToList events) $

          signalProperty 20 show
            (\(_, _, _, _, _, _, toolong) -> Set.null toolong)
            ((,,,,,,) <$> govLocalRootPeersSig
                 <*> govEstablishedPeersSig
                 <*> govActivePeersSig
                 <*> govActiveFailuresSig
                 <*> govInProgressPromoteWarmSig
                 <*> trIsNodeAlive
                 <*> promotionOpportunitiesIgnoredTooLong
            )


prop_diffusion_target_active_local_below :: SimTrace Void
                                         -> Int
                                         -> Property
prop_diffusion_target_active_local_below ioSimTrace traceNumber =
    let events :: [Events DiffusionTestTrace]
        events = Trace.toList
               . fmap ( Signal.eventsFromList
                      . fmap (\(WithName _ (WithTime t b)) -> (t, b))
                      )
               . splitWithNameTrace
               . fmap (\(WithTime t (WithName name b)) -> WithName name (WithTime t b))
               . withTimeNameTraceEvents
                  @DiffusionTestTrace
                  @NtNAddr
               . Trace.take traceNumber
               $ ioSimTrace

     in conjoin
      $ (\ev ->
        let evsList = eventsToList ev
            lastTime = fst
                     . last
                     $ evsList
         in classifySimulatedTime lastTime
          $ classifyNumberOfEvents (length evsList)
          $ verify_target_active_below ev
        )
      <$> events

  where
    verify_target_active_below :: Events DiffusionTestTrace
                               -> Property
    verify_target_active_below events =
      let govLocalRootPeersSig :: Signal (LocalRootPeers.LocalRootPeers NtNAddr)
          govLocalRootPeersSig =
            selectDiffusionPeerSelectionState Governor.localRootPeers events

          govEstablishedPeersSig :: Signal (Set NtNAddr)
          govEstablishedPeersSig =
            selectDiffusionPeerSelectionState
              (EstablishedPeers.toSet . Governor.establishedPeers)
              events

          govInProgressDemoteToColdSig :: Signal (Set NtNAddr)
          govInProgressDemoteToColdSig =
            selectDiffusionPeerSelectionState
              Governor.inProgressDemoteToCold
              events

          govInProgressPromoteWarmSig :: Signal (Set NtNAddr)
          govInProgressPromoteWarmSig =
            selectDiffusionPeerSelectionState Governor.inProgressPromoteWarm events

          trJoinKillSig :: Signal JoinedOrKilled
          trJoinKillSig =
              Signal.fromChangeEvents Killed -- Default to TrKillingNode
            . Signal.selectEvents
                (\case TrJoiningNetwork -> Just Joined
                       TrKillingNode    -> Just Killed
                       _                -> Nothing
                )
            . selectDiffusionSimulationTrace
            $ events

          -- Signal.keyedUntil receives 2 functions one that sets start of the
          -- set signal, one that ends it and another that stops all.
          --
          -- In this particular case we want a signal that is keyed beginning
          -- on a TrJoiningNetwork and ends on TrKillingNode, giving us a Signal
          -- with the periods when a node was alive.
          trIsNodeAlive :: Signal Bool
          trIsNodeAlive =
                not . Set.null
            <$> Signal.keyedUntil (fromJoinedOrKilled (Set.singleton ())
                                                      Set.empty)
                                  (fromJoinedOrKilled Set.empty
                                                      (Set.singleton ()))
                                  (const False)
                                  trJoinKillSig

          govActivePeersSig :: Signal (Set NtNAddr)
          govActivePeersSig =
            selectDiffusionPeerSelectionState Governor.activePeers events

          govActiveFailuresSig :: Signal (Set NtNAddr)
          govActiveFailuresSig =
              Signal.keyedLinger
                180 -- 3 minutes  -- TODO: too eager to reconnect?
                (fromMaybe Set.empty)
            . Signal.fromEvents
            . Signal.selectEvents
                (\case TracePromoteWarmFailed _ _ peer _ ->
                         --TODO: the simulation does not yet cause this to happen
                         Just (Set.singleton peer)
                       TraceDemoteAsynchronous status
                         | Set.null failures -> Nothing
                         | otherwise         -> Just failures
                         where
                           -- unlike in the governor case we take into account
                           -- all asynchronous demotions
                           failures = Map.keysSet status
                       TraceDemoteLocalAsynchronous status
                         | Set.null failures -> Nothing
                         | otherwise         -> Just failures
                         where
                           -- unlike in the governor case we take into account
                           -- all asynchronous demotions
                           failures = Map.keysSet status
                       _ -> Nothing
                )
            . selectDiffusionPeerSelectionEvents
            $ events

          promotionOpportunities :: Signal (Set NtNAddr)
          promotionOpportunities =
            (\local established active recentFailures isAlive inProgressDemoteToCold inProgressPromoteWarm ->
              if isAlive then
                Set.unions
                  [ -- There are no opportunities if we're at or above target
                    if Set.size groupActive >= hotTarget
                       then Set.empty
                       else groupEstablished Set.\\ active
                                             Set.\\ recentFailures
                                             Set.\\ inProgressDemoteToCold
                                             Set.\\ inProgressPromoteWarm
                  | (HotValency hotTarget, _, group) <- LocalRootPeers.toGroupSets local
                  , let groupActive      = group `Set.intersection` active
                        groupEstablished = group `Set.intersection` established
                  ]
                        else
              Set.empty
            ) <$> govLocalRootPeersSig
              <*> govEstablishedPeersSig
              <*> govActivePeersSig
              <*> govActiveFailuresSig
              <*> trIsNodeAlive
              <*> govInProgressDemoteToColdSig
              <*> govInProgressPromoteWarmSig

          promotionOpportunitiesIgnoredTooLong :: Signal (Set NtNAddr)
          promotionOpportunitiesIgnoredTooLong =
            Signal.keyedTimeout
              10 -- seconds
              id
              promotionOpportunities

       in counterexample
            ("\nSignal key: (local, established peers, active peers, " ++
             "recent failures, opportunities, ignored too long)") $
          counterexample
            (intercalate "\n" $ map show $ Signal.eventsToList events) $

          signalProperty 20 show
            (\(_,_,_,_,_,toolong) -> Set.null toolong)
            ((,,,,,) <$> (LocalRootPeers.toGroupSets <$> govLocalRootPeersSig)
                     <*> govEstablishedPeersSig
                     <*> govActivePeersSig
                     <*> govActiveFailuresSig
                     <*> promotionOpportunities
                     <*> promotionOpportunitiesIgnoredTooLong)


-- | A testing scenario which reproduces issue #4046
--
async_demotion_network_script :: DiffusionScript
async_demotion_network_script =
    DiffusionScript
      simArgs
      (singletonTimedScript Map.empty)
      [ ( common { naAddr                  = addr1,
                   naLocalRootPeers        = localRoots1,
                   naPeerTargets = ConsensusModePeerTargets {
                     deadlineTargets = Governor.nullPeerSelectionTargets {
                         targetNumberOfKnownPeers = 2,
                           targetNumberOfEstablishedPeers = 2,
                           targetNumberOfActivePeers = 2
                         },
                     syncTargets = peerTargets }
                 }
        , [ JoinNetwork 0
            -- reconfigure the peer to trigger the outbound governor log
          , Reconfigure 240 localRoots1'
          ]
        )
      , ( common { naAddr           = addr2,
                   naLocalRootPeers = [(1,1, Map.fromList [(ra_addr1, (DoNotAdvertisePeer, IsNotTrustable))])] }
        , [JoinNetwork 0, Kill 5, JoinNetwork 20]
        )
      , ( common { naAddr           = addr3,
                   naLocalRootPeers = [(1,1, Map.fromList [(ra_addr1, (DoNotAdvertisePeer, IsNotTrustable))])] }
        , [JoinNetwork 0]
        )
      ]
  where
    addr1    = TestAddress (IPAddr (read "10.0.0.1") 3000)
    ra_addr1 = RelayAccessAddress (read "10.0.0.1") 3000
    localRoots1  = [(2,2, Map.fromList [(ra_addr2, (DoNotAdvertisePeer, IsNotTrustable))
                                       ,(ra_addr3, (DoNotAdvertisePeer, IsNotTrustable))])]
    localRoots1' = [(2,2, Map.fromList [(ra_addr2, (DoAdvertisePeer, IsNotTrustable))
                                       ,(ra_addr3, (DoAdvertisePeer, IsNotTrustable))])]

    addr2    = TestAddress (IPAddr (read "10.0.0.2") 3000)
    ra_addr2 = RelayAccessAddress (read "10.0.0.2") 3000

    addr3    = TestAddress (IPAddr (read "10.0.0.3") 3000)
    ra_addr3 = RelayAccessAddress (read "10.0.0.3") 3000

    simArgs = SimArgs {
        saSlot             = secondsToDiffTime 1,
        saQuota            = 5  -- 5% chance of producing a block
      }
    peerTargets = Governor.nullPeerSelectionTargets {
      targetNumberOfKnownPeers = 1,
      targetNumberOfEstablishedPeers = 1,
      targetNumberOfActivePeers =1 }

    common = NodeArgs {
        naSeed             = 10,
        naDiffusionMode    = InitiatorAndResponderDiffusionMode,
        naMbTime           = Just 1,
        naPublicRoots      = Map.empty,
        naConsensusMode    = PraosMode,
        naBootstrapPeers   = Script (UseBootstrapPeers [RelayAccessDomain "bootstrap" 00000] :| []),
        naAddr             = undefined,
        naLocalRootPeers   = undefined,
        naLedgerPeers      = Script (LedgerPools [] :| []),
        naPeerTargets      = ConsensusModePeerTargets {
            deadlineTargets = peerTargets,
            syncTargets     = peerTargets },
        naDNSTimeoutScript = singletonScript (DNSTimeout 3),
        naDNSLookupDelayScript
                           = singletonScript (DNSLookupDelay 0.2),
        naChainSyncExitOnBlockNo
                           = Nothing,
        naChainSyncEarlyExit
                           = False,
        naPeerSharing      = PeerSharingDisabled,
        naFetchModeScript  = singletonScript FetchModeDeadline
      }


-- | Data type designed for interpretation with `Signal.keyedUntil`.
--
data StartStop a =
    -- | start event
    Start (Set a)
    -- | stop event
  | Stop (Set a)
    -- | stop all
  | StopAll

-- | Show that outbound governor reacts to asynchronous demotions
--
prop_diffusion_async_demotions :: SimTrace Void
                               -> Int
                               -> Property
prop_diffusion_async_demotions ioSimTrace traceNumber =
    let events :: [Events DiffusionTestTrace]
        events = Trace.toList
               . fmap ( Signal.eventsFromList
                      . fmap (\(WithName _ (WithTime t b)) -> (t, b))
                      )
               . splitWithNameTrace
               . fmap (\(WithTime t (WithName name b)) -> WithName name (WithTime t b))
               . withTimeNameTraceEvents
                  @DiffusionTestTrace
                  @NtNAddr
               . traceFromList
               . fmap (\(t, tid, tl, te) -> SimEvent t tid tl te)
               . takeUntilEndofTurn traceNumber
               . traceEvents
               $ ioSimTrace

    in conjoin
      $ (\ev ->
        let evsList = eventsToList ev
            lastTime = fst
                     . last
                     $ evsList
        in counterexample (unlines $ map show evsList)
          $ classifySimulatedTime lastTime
          $ classifyNumberOfEvents (length evsList)
          $ verify_async_demotions ev
        )
      <$> events

  where
    verify_async_demotions :: Events DiffusionTestTrace -> Property
    verify_async_demotions events =

      let demotionOpportunities :: Signal (Set NtNAddr)
          demotionOpportunities =
              Signal.keyedUntil
                (\case Start a -> a
                       _       -> Set.empty)
                (\case Stop a  -> a
                       _       -> Set.empty)
                (\case StopAll -> True
                       _       -> False)
            . Signal.fromEventsWith (Start Set.empty)
            . Signal.selectEvents
                (\case
                  DiffusionPeerSelectionActionsTrace a ->
                    case a of
                      PeerStatusChanged (HotToCooling connId) ->
                          Just $ Start demotions
                        where
                          demotions = Set.singleton (remoteAddress connId)
                      PeerStatusChanged (WarmToCooling connId) ->
                          Just $ Start demotions
                        where
                          demotions = Set.singleton (remoteAddress connId)
                      _ -> Nothing

                  DiffusionPeerSelectionTrace a ->
                    case a of
                      TraceDemoteAsynchronous status ->
                          Just $ Stop failures
                        where
                          failures = Map.keysSet (Map.filter ((==PeerCooling) . fst) status)
                      TraceDemoteBigLedgerPeersAsynchronous status ->
                          Just $ Stop failures
                        where
                          failures = Map.keysSet (Map.filter ((==PeerCooling) . fst) status)
                      TraceDemoteLocalAsynchronous status ->
                          Just $ Stop failures
                        where
                          failures = Map.keysSet (Map.filter ((==PeerCooling) . fst) status)
                      TraceDemoteHotFailed _ _ peeraddr _ ->
                          Just $ Stop failures
                        where
                          failures = Set.singleton peeraddr
                      TraceDemoteWarmFailed _ _ peeraddr _ ->
                          Just $ Stop failures
                        where
                          failures = Set.singleton peeraddr
                      TracePromoteColdFailed _ _ peeraddr _ _ ->
                          Just $ Stop failures
                        where
                          failures = Set.singleton peeraddr
                      TracePromoteWarmFailed _ _ peeraddr _ ->
                          Just $ Stop failures
                        where
                          failures = Set.singleton peeraddr
                      TraceDemoteWarmDone _ _ peeraddr ->
                          Just $ Stop failures
                        where
                          failures = Set.singleton peeraddr
                      TracePromoteColdBigLedgerPeerFailed _ _ peeraddr _ _ ->
                          Just $ Stop failures
                        where
                          failures = Set.singleton peeraddr
                      TracePromoteWarmBigLedgerPeerFailed _ _ peeraddr _ ->
                          Just $ Stop failures
                        where
                          failures = Set.singleton peeraddr
                      TraceDemoteHotBigLedgerPeerFailed _ _ peeraddr _ ->
                          Just $ Stop failures
                        where
                          failures = Set.singleton peeraddr
                      TraceDemoteWarmBigLedgerPeerFailed _ _ peeraddr _ ->
                          Just $ Stop failures
                        where
                          failures = Set.singleton peeraddr
                      TraceDemoteWarmBigLedgerPeerDone _ _ peeraddr ->
                          Just $ Stop failures
                        where
                          failures = Set.singleton peeraddr
                      _ -> Nothing

                  DiffusionConnectionManagerTrace a ->
                    case a of
                      CM.TrConnectionCleanup connId ->
                          Just $ Stop failures
                        where
                          failures = Set.singleton (remoteAddress connId)
                      CM.TrShutdown ->
                          Just StopAll
                      _ -> Nothing

                  _ -> Nothing
                )
            $ events

          demotionOpportunitiesTooLong :: Signal (Set NtNAddr)
          demotionOpportunitiesTooLong =
              Signal.keyedTimeout 1 id demotionOpportunities

      in signalProperty
            20 show Set.null
            demotionOpportunitiesTooLong


unit_diffusion_async_demotions :: Property
unit_diffusion_async_demotions =
    testWithIOSim
      prop_diffusion_async_demotions
      125000
      absNoAttenuation
      async_demotion_network_script



-- | A variant of
-- 'Test.Ouroboros.Network.PeerSelection.prop_governor_target_active_local_above'
-- but for running on Diffusion. This means it has to have in consideration the
-- the logs for all nodes running will all appear in the trace and the test
-- property should only be valid while a given node is up and running.
--
prop_diffusion_target_active_local_above :: SimTrace Void
                                         -> Int
                                         -> Property
prop_diffusion_target_active_local_above ioSimTrace traceNumber =
    let events :: [Events DiffusionTestTrace]
        events = Trace.toList
               .fmap ( Signal.eventsFromList
                      . fmap (\(WithName _ (WithTime t b)) -> (t, b))
                      )
               . splitWithNameTrace
               . fmap (\(WithTime t (WithName name b)) -> WithName name (WithTime t b))
               . withTimeNameTraceEvents
                  @DiffusionTestTrace
                  @NtNAddr
               . Trace.take traceNumber
               $ ioSimTrace

     in conjoin
      $ (\ev ->
        let evsList = eventsToList ev
            lastTime = fst
                     . last
                     $ evsList
         in classifySimulatedTime lastTime
          $ classifyNumberOfEvents (length evsList)
          $ verify_target_active_above ev
        )
      <$> events

  where
    verify_target_active_above :: Events DiffusionTestTrace
                               -> Property
    verify_target_active_above events =
      let govLocalRootPeersSig :: Signal (LocalRootPeers.LocalRootPeers NtNAddr)
          govLocalRootPeersSig =
            selectDiffusionPeerSelectionState Governor.localRootPeers events

          govActivePeersSig :: Signal (Set NtNAddr)
          govActivePeersSig =
            selectDiffusionPeerSelectionState Governor.activePeers events

          govInProgressDemoteToColdSig :: Signal (Set NtNAddr)
          govInProgressDemoteToColdSig =
            selectDiffusionPeerSelectionState Governor.inProgressDemoteToCold events

          trJoinKillSig :: Signal JoinedOrKilled
          trJoinKillSig =
              Signal.fromChangeEvents Killed -- Default to TrKillingNode
            . Signal.selectEvents
                (\case TrJoiningNetwork -> Just Joined
                       TrKillingNode    -> Just Killed
                       _                -> Nothing
                )
            . selectDiffusionSimulationTrace
            $ events

          -- Signal.keyedUntil receives 2 functions one that sets start of the
          -- set signal, one that ends it and another that stops all.
          --
          -- In this particular case we want a signal that is keyed beginning
          -- on a TrJoiningNetwork and ends on TrKillingNode, giving us a Signal
          -- with the periods when a node was alive.
          trIsNodeAlive :: Signal Bool
          trIsNodeAlive =
                not . Set.null
            <$> Signal.keyedUntil (fromJoinedOrKilled (Set.singleton ())
                                                      Set.empty)
                                  (fromJoinedOrKilled Set.empty
                                                      (Set.singleton ()))
                                  (const False)
                                  trJoinKillSig

          demotionOpportunities :: Signal (Set NtNAddr)
          demotionOpportunities =
            (\local active isAlive inProgressDemoteToCold ->
              if isAlive
              then Set.unions
                    [ -- There are no opportunities if we're at or below target
                      if Set.size groupActive <= hotTarget
                         then Set.empty
                         else groupActive
                                Set.\\ inProgressDemoteToCold
                    | (HotValency hotTarget, _, group) <- LocalRootPeers.toGroupSets local
                    , let groupActive = group `Set.intersection` active
                    ]
              else Set.empty
            ) <$> govLocalRootPeersSig
              <*> govActivePeersSig
              <*> trIsNodeAlive
              <*> govInProgressDemoteToColdSig

          demotionOpportunitiesIgnoredTooLong :: Signal (Set NtNAddr)
          demotionOpportunitiesIgnoredTooLong =
            Signal.keyedTimeout
              100 -- seconds
              id
              demotionOpportunities

       in counterexample
            ("\nSignal key: (local peers, active peers, is alive " ++
             "demotion opportunities, ignored too long)") $
          counterexample (intercalate "\n" $ map show $ Signal.eventsToList events) $

          signalProperty 20 show
            (\(_,_,_,_,toolong) -> Set.null toolong)
            ((,,,,) <$> (LocalRootPeers.toGroupSets <$> govLocalRootPeersSig)
                    <*> govActivePeersSig
                    <*> trIsNodeAlive
                    <*> demotionOpportunities
                    <*> demotionOpportunitiesIgnoredTooLong)


-- | A variant of ouroboros-network-framework
-- 'Test.Ouroboros.Network.Server2.prop_connection_manager_valid_transitions'
-- but for running on Diffusion. This means it has to have in consideration
-- that the logs for all nodes running will all appear in the trace and the test
-- property should only be valid while a given node is up and running.
--
prop_diffusion_cm_valid_transitions :: SimTrace Void
                                    -> Int
                                    -> Property
prop_diffusion_cm_valid_transitions ioSimTrace traceNumber =
    let events :: [Trace () (WithName NtNAddr (WithTime DiffusionTestTrace))]
        events = Trace.toList
               . fmap (Trace.fromList ())
               . splitWithNameTrace
               . fmap (\(WithTime t (WithName name b))
                       -> WithName name (WithTime t b))
               . withTimeNameTraceEvents
                  @DiffusionTestTrace
                  @NtNAddr
               . Trace.take traceNumber
               $ ioSimTrace

     in conjoin
      $ (\ev ->
        let evsList = Trace.toList ev
            lastTime = (\(WithName _ (WithTime t _)) -> t)
                     . last
                     $ evsList
         in classifySimulatedTime lastTime
          $ classifyNumberOfEvents (length evsList)
          $ verify_cm_valid_transitions
          $ (\(WithName _ (WithTime _ b)) -> b)
          <$> ev
        )
      <$> events

  where
    verify_cm_valid_transitions :: Trace () DiffusionTestTrace -> Property
    verify_cm_valid_transitions events =
      let abstractTransitionEvents :: Trace () (AbstractTransitionTrace NtNAddr)
          abstractTransitionEvents =
            selectDiffusionConnectionManagerTransitionEvents events

          connectionManagerEvents :: [CM.Trace
                                        NtNAddr
                                        (ConnectionHandlerTrace
                                          NtNVersion
                                          NtNVersionData)]
          connectionManagerEvents =
              Trace.toList
            . selectDiffusionConnectionManagerEvents
            $ events

       in mkProperty
        . bifoldMap
           ( const mempty )
           ( \ trs
            -> TestProperty {
                 tpProperty =
                     (counterexample $!
                       (  "\nconnection:\n"
                       ++ intercalate "\n" (map ppTransition trs))
                       )
                   . foldMap ( \ tr
                              -> All
                               . (counterexample $!
                                   (  "\nUnexpected transition: "
                                   ++ show tr)
                                   )
                               . verifyAbstractTransition
                               $ tr
                             )
                   $ trs,
                 tpNumberOfTransitions = Sum (length trs),
                 tpNumberOfConnections = Sum 1,
                 tpNumberOfPrunings    = classifyPrunings connectionManagerEvents,
                 tpNegotiatedDataFlows = [classifyNegotiatedDataFlow trs],
                 tpEffectiveDataFlows  = [classifyEffectiveDataFlow  trs],
                 tpTerminationTypes    = [classifyTermination        trs],
                 tpActivityTypes       = [classifyActivityType       trs],
                 tpTransitions         = trs
              }
           )
        . fmap (map ttTransition)
        . groupConns id abstractStateIsFinalTransition
        $ abstractTransitionEvents


-- | A variant of ouroboros-network-framework
-- 'Test.Ouroboros.Network.Server2.prop_connection_manager_valid_transition_order'
-- but for running on Diffusion. This means it has to have in consideration the
-- the logs for all nodes running will all appear in the trace and the test
-- property should only be valid while a given node is up and running.
--
-- This test is meant to run with IOSimPOR. It gets the transitions from the
-- traceTVar trace which can't be reordered, hence leading to false positives.
--
-- We can't reliably check for transitions to UnknownState but the IOSim tests
-- already give us quite a lot confidence that there isn't any bugs there.
--
-- Another thing to note is that this trace differs from the IO one in
-- the fact that all connections terminate with a trace to
-- 'UnknownConnectionSt', since we can't do that here we limit ourselves
-- to 'TerminatedSt'.
--
prop_diffusion_cm_valid_transition_order_iosim_por :: SimTrace Void
                                                   -> Int
                                                   -> Property
prop_diffusion_cm_valid_transition_order_iosim_por ioSimTrace traceNumber =
    let events :: [Trace () (WithName NtNAddr (WithTime (AbstractTransitionTrace NtNAddr)))]
        events = Trace.toList
            . fmap (Trace.fromList ())
            . splitWithNameTrace
            . traceSelectTraceEvents
                (\t se ->
                  case se of
                    EventLog dyn
                      -- Traced by traceTVar
                      | Just tr@(TransitionTrace n _)
                        <- fromDynamic dyn
                        -> Just (WithName n (WithTime t tr))
                    _   -> Nothing)
            . Trace.take traceNumber
            $ ioSimTrace

     in conjoin
      $ (\ev ->
        let evsList = Trace.toList ev
            lastTime = (\(WithName _ (WithTime t _)) -> t)
                     . last
                     $ evsList
         in counterexample (intercalate "\n" $ map show $ Trace.toList ev)
          $ classifySimulatedTime lastTime
          $ classifyNumberOfEvents (length evsList)
          $ verify_cm_valid_transition_order
          $ (\(WithName _ (WithTime _ b)) -> b)
          <$> ev
        )
      <$> events
  where
    verify_cm_valid_transition_order :: Trace () (AbstractTransitionTrace NtNAddr) -> Property
    verify_cm_valid_transition_order =
         property
       . bifoldMap
          (const mempty)
          (verifyAbstractTransitionOrder False)
       . fmap (map ttTransition)
       . groupConns id abstractStateIsFinalTransitionTVarTracing

-- | A variant of ouroboros-network-framework
-- 'Test.Ouroboros.Network.Server2.prop_connection_manager_valid_transition_order'
-- but for running on Diffusion. This means it has to have in consideration the
-- the logs for all nodes running will all appear in the trace and the test
-- property should only be valid while a given node is up and running.
--
prop_diffusion_cm_valid_transition_order :: SimTrace Void
                                         -> Int
                                         -> Property
prop_diffusion_cm_valid_transition_order ioSimTrace traceNumber =
    let events :: [Trace () (WithName NtNAddr (WithTime DiffusionTestTrace))]
        events = Trace.toList
               . fmap (Trace.fromList ())
               . splitWithNameTrace
               . fmap (\(WithTime t (WithName name b))
                       -> WithName name (WithTime t b))
               . withTimeNameTraceEvents
                  @DiffusionTestTrace
                  @NtNAddr
               . Trace.take traceNumber
               $ ioSimTrace

     in conjoin
      $ (\ev ->
        let evsList = Trace.toList ev
            lastTime = (\(WithName _ (WithTime t _)) -> t)
                     . last
                     $ evsList
         in classifySimulatedTime lastTime
          $ classifyNumberOfEvents (length evsList)
          $ verify_cm_valid_transition_order
          $ (\(WithName _ (WithTime _ b)) -> b)
          <$> ev
        )
      <$> events
  where
    verify_cm_valid_transition_order :: Trace () DiffusionTestTrace -> Property
    verify_cm_valid_transition_order events =
      let abstractTransitionEvents :: Trace () (AbstractTransitionTrace NtNAddr)
          abstractTransitionEvents =
            selectDiffusionConnectionManagerTransitionEvents events

       in  property
         . bifoldMap
            (const mempty)
            (verifyAbstractTransitionOrder False)
         . fmap (map ttTransition)
         . groupConns id abstractStateIsFinalTransition
         $ abstractTransitionEvents

-- | Unit test that checks issue 4258
-- https://github.com/intersectmbo/ouroboros-network/issues/4258
--
-- TODO: prettify the expression so it's easier to maintain it when things
-- change.
prop_unit_4258 :: Property
prop_unit_4258 =
  let ioerr = IOError
        { ioe_handle      = Nothing
        , ioe_type        = ResourceExhausted
        , ioe_location    = "AttenuationChannel"
        , ioe_description = "attenuation"
        , ioe_errno       = Nothing
        , ioe_filename    = Nothing
        }

      bearerInfo = AbsBearerInfo {
                     abiConnectionDelay = NormalDelay,
                     abiInboundAttenuation = NoAttenuation FastSpeed,
                     abiOutboundAttenuation = NoAttenuation FastSpeed,
                     abiInboundWriteFailure = Nothing,
                     abiOutboundWriteFailure = Nothing,
                     abiAcceptFailure = Just (SmallDelay,ioerr),
                     abiSDUSize = LargeSDU
                   }
      diffScript = DiffusionScript
        (SimArgs 1 10)
        (singletonTimedScript Map.empty)
        [( NodeArgs (-3) InitiatorAndResponderDiffusionMode (Just 224)
             Map.empty
             PraosMode
             (Script (UseBootstrapPeers [RelayAccessDomain "bootstrap" 00000] :| []))
             (TestAddress (IPAddr (read "0.0.0.4") 9))
             PeerSharingDisabled
             [(1,1,Map.fromList [(RelayAccessAddress "0.0.0.8" 65531,(DoNotAdvertisePeer, IsNotTrustable))])]
             (Script (LedgerPools [] :| []))
             ConsensusModePeerTargets {
               deadlineTargets = nullPeerSelectionTargets {
                 targetNumberOfRootPeers = 2,
                 targetNumberOfKnownPeers = 5,
                 targetNumberOfEstablishedPeers = 4,
                 targetNumberOfActivePeers = 1 },
               syncTargets = nullPeerSelectionTargets }
             (Script (DNSTimeout {getDNSTimeout = 0.397}
                 :| [ DNSTimeout {getDNSTimeout = 0.382},
                      DNSTimeout {getDNSTimeout = 0.321},
                      DNSTimeout {getDNSTimeout = 0.143},
                      DNSTimeout {getDNSTimeout = 0.256},
                      DNSTimeout {getDNSTimeout = 0.142},
                      DNSTimeout {getDNSTimeout = 0.341},
                      DNSTimeout {getDNSTimeout = 0.236}
                    ]))
             (Script (DNSLookupDelay {getDNSLookupDelay = 0.065} :| []))
             Nothing
             False
             (Script (FetchModeDeadline :| []))
         , [ JoinNetwork 4.166666666666,
             Kill 0.3,
             JoinNetwork 1.517857142857,
             Reconfigure 0.245238095238 [],
             Reconfigure 4.190476190476 []
           ]
         ),
         ( NodeArgs (-5) InitiatorAndResponderDiffusionMode (Just 269)
             (Map.fromList [(RelayAccessAddress "0.0.0.4" 9, DoAdvertisePeer)])
             PraosMode
             (Script (UseBootstrapPeers [RelayAccessDomain "bootstrap" 00000] :| []))
             (TestAddress (IPAddr (read "0.0.0.8") 65531))
             PeerSharingDisabled
             [(1,1,Map.fromList [(RelayAccessAddress "0.0.0.4" 9,(DoNotAdvertisePeer, IsNotTrustable))])]
             (Script (LedgerPools [] :| []))
             ConsensusModePeerTargets {
               deadlineTargets = nullPeerSelectionTargets {
                 targetNumberOfRootPeers = 4,
                 targetNumberOfKnownPeers = 5,
                 targetNumberOfEstablishedPeers = 3,
                 targetNumberOfActivePeers = 1,

                 targetNumberOfKnownBigLedgerPeers = 0,
                 targetNumberOfEstablishedBigLedgerPeers = 0,
                 targetNumberOfActiveBigLedgerPeers = 0
               },
               syncTargets = nullPeerSelectionTargets }
             (Script (DNSTimeout {getDNSTimeout = 0.281}
                 :| [ DNSTimeout {getDNSTimeout = 0.177},
                      DNSTimeout {getDNSTimeout = 0.164},
                      DNSTimeout {getDNSTimeout = 0.373}
                    ]))
             (Script (DNSLookupDelay {getDNSLookupDelay = 0.133}
                 :| [ DNSLookupDelay {getDNSLookupDelay = 0.128},
                      DNSLookupDelay {getDNSLookupDelay = 0.049},
                      DNSLookupDelay {getDNSLookupDelay = 0.058},
                      DNSLookupDelay {getDNSLookupDelay = 0.042},
                      DNSLookupDelay {getDNSLookupDelay = 0.117},
                      DNSLookupDelay {getDNSLookupDelay = 0.064}
                     ]))
             Nothing
             False
             (Script (FetchModeDeadline :| []))
         , [ JoinNetwork 3.384615384615,
             Reconfigure 3.583333333333 [(1,1,Map.fromList [(RelayAccessAddress "0.0.0.4" 9,(DoNotAdvertisePeer, IsNotTrustable))])],
             Kill 15.55555555555,
             JoinNetwork 30.53333333333,
             Kill 71.11111111111
            ]
         )]
   in testWithIOSim prop_diffusion_cm_valid_transition_order 125000 bearerInfo diffScript

-- | This unit tests checks that for every * -> TerminatedSt Connection
-- Manager transition, there's a corresponding peer selection state update
-- where the peer gets removed from the established set.
--
-- Due to how IOSim currently works, the outbound governor thread is always
-- going to be scheduled first since it is always the first to block (on STM).
-- However this bug is triggered by a race condition between the
-- 'peerMonitoringLoop' and the outbound governor, where the
-- 'peerMonitoringLoop' will update the peer status way too fast and the
-- out-governor won't be able to notice the intermediate state (STM doesn't
-- guarantee all intermediate states are seen). If this happens the
-- out-governor will fail to remove the peer from the established peers set
-- and will think it has a connection to it when it does not.
--
-- If one wishes to check if the bug is present one should (unless IOSim is
-- patched to explore more schedules or IOSimPOR is made more efficient) add a
-- 'threadDelay' to 'evalGuardedDecisions' in the outbound governor code to
-- force it to go to the back of the queue everytime.
--
prop_unit_reconnect :: Property
prop_unit_reconnect =
  let diffScript =
        DiffusionScript
          (SimArgs 1 10)
          (singletonTimedScript Map.empty)
          [(NodeArgs
              (-3)
              InitiatorAndResponderDiffusionMode
              (Just 224)
              Map.empty
              PraosMode
              (Script (DontUseBootstrapPeers :| []))
              (TestAddress (IPAddr (read "0.0.0.0") 0))
              PeerSharingDisabled
              [ (2,2,Map.fromList [ (RelayAccessAddress "0.0.0.1" 0,(DoNotAdvertisePeer, IsNotTrustable))
                                  , (RelayAccessAddress "0.0.0.2" 0,(DoNotAdvertisePeer, IsNotTrustable))
                                  ])
              ]
              (Script (LedgerPools [] :| []))
              ConsensusModePeerTargets {
                deadlineTargets = PeerSelectionTargets {
                    targetNumberOfRootPeers = 1,
                    targetNumberOfKnownPeers = 1,
                    targetNumberOfEstablishedPeers = 1,
                    targetNumberOfActivePeers = 1,

                    targetNumberOfKnownBigLedgerPeers = 0,
                    targetNumberOfEstablishedBigLedgerPeers = 0,
                    targetNumberOfActiveBigLedgerPeers = 0 },
                syncTargets = nullPeerSelectionTargets }
              (Script (DNSTimeout {getDNSTimeout = 10} :| []))
              (Script (DNSLookupDelay {getDNSLookupDelay = 0} :| []))
              Nothing
              False
              (Script (FetchModeDeadline :| []))
          , [ JoinNetwork 0
            ])
          , (NodeArgs
               (-1)
               InitiatorAndResponderDiffusionMode
               (Just 2)
               Map.empty
               PraosMode
               (Script (DontUseBootstrapPeers :| []))
               (TestAddress (IPAddr (read "0.0.0.1") 0))
               PeerSharingDisabled
               [(1,1,Map.fromList [(RelayAccessAddress "0.0.0.0" 0,(DoNotAdvertisePeer, IsNotTrustable))])]
               (Script (LedgerPools [] :| []))
               ConsensusModePeerTargets {
                 deadlineTargets = PeerSelectionTargets {
                     targetNumberOfRootPeers = 1,
                     targetNumberOfKnownPeers = 1,
                     targetNumberOfEstablishedPeers = 1,
                     targetNumberOfActivePeers = 1,

                     targetNumberOfKnownBigLedgerPeers = 0,
                     targetNumberOfEstablishedBigLedgerPeers = 0,
                     targetNumberOfActiveBigLedgerPeers = 0 },
                 syncTargets = nullPeerSelectionTargets }
             (Script (DNSTimeout {getDNSTimeout = 10} :| [ ]))
             (Script (DNSLookupDelay {getDNSLookupDelay = 0} :| []))
             Nothing
             False
             (Script (FetchModeDeadline :| []))
         , [ JoinNetwork 10
           ])
         ]

      sim :: forall s . IOSim s Void
      sim = diffusionSimulation (toBearerInfo (absNoAttenuation { abiInboundAttenuation  = SpeedAttenuation SlowSpeed (Time 20) 1000
                                                                } ))
                                diffScript
                                iosimTracer

      events :: [Events DiffusionTestTrace]
      events = Trace.toList
             . fmap ( Signal.eventsFromList
                    . fmap (\(WithName _ (WithTime t b)) -> (t, b))
                    )
             . splitWithNameTrace
             . fmap (\(WithTime t (WithName name b)) -> WithName name (WithTime t b))
             . withTimeNameTraceEvents
                @DiffusionTestTrace
                @NtNAddr
             . Trace.take 125000
             $ runSimTrace sim

   in conjoin
    $ verify_consistency
   <$> events

  where
    verify_consistency :: Events DiffusionTestTrace -> Property
    verify_consistency events =
      let govEstablishedPeersSig :: Signal (Set NtNAddr)
          govEstablishedPeersSig =
            selectDiffusionPeerSelectionState'
              (EstablishedPeers.toSet . Governor.establishedPeers)
              events

          govConnectionManagerTransitionsSig :: [E (AbstractTransitionTrace NtNAddr)]
          govConnectionManagerTransitionsSig =
            Signal.eventsToListWithId
            $ Signal.selectEvents
                (\case
                   DiffusionConnectionManagerTransitionTrace tr -> Just tr
                   _                                            -> Nothing
                ) events

       in conjoin
        $ map (\(E ts a) -> case a of
                TransitionTrace addr (Transition _ TerminatedSt) ->
                  eventually ts (Set.notMember addr) govEstablishedPeersSig
                _ -> True -- TODO: Do the opposite
              )
              govConnectionManagerTransitionsSig

-- | Verify that certain traces are never emitted by the simulation.
--
prop_diffusion_cm_no_dodgy_traces :: SimTrace Void
                                  -> Int
                                  -> Property
prop_diffusion_cm_no_dodgy_traces ioSimTrace traceNumber =
    let events :: [Trace () (WithName NtNAddr (WithTime DiffusionTestTrace))]
        events = Trace.toList
               . fmap (Trace.fromList ())
               . splitWithNameTrace
               . fmap (\(WithTime t (WithName name b))
                       -> WithName name (WithTime t b))
               . withTimeNameTraceEvents
                  @DiffusionTestTrace
                  @NtNAddr
               . Trace.take traceNumber
               $ ioSimTrace

     in conjoin
      $ (\ev ->
        let evsList = Trace.toList ev
            lastTime = (\(WithName _ (WithTime t _)) -> t)
                     . last
                     $ evsList
         in classifySimulatedTime lastTime
          $ classifyNumberOfEvents (length evsList)
          $ verify_cm_traces
          $ (\(WithName _ (WithTime _ b)) -> b)
          <$> ev
        )
      <$> events

  where
    verify_cm_traces :: Trace () DiffusionTestTrace -> Property
    verify_cm_traces events =
      let connectionManagerEvents :: [CM.Trace
                                        NtNAddr
                                        (ConnectionHandlerTrace
                                          NtNVersion
                                          NtNVersionData)]
          connectionManagerEvents =
              Trace.toList
            . selectDiffusionConnectionManagerEvents
            $ events

        in conjoin $ map
             (\ev -> case ev of
               CM.TrConnectionExists {}    -> counterexample (show ev) False
               CM.TrForbiddenConnection {} -> counterexample (show ev) False
               _                           -> property True
             ) connectionManagerEvents


prop_diffusion_peer_selection_actions_no_dodgy_traces :: SimTrace Void
                                                      -> Int
                                                      -> Property
prop_diffusion_peer_selection_actions_no_dodgy_traces ioSimTrace traceNumber =
    let events :: [Trace () (WithName NtNAddr (WithTime DiffusionTestTrace))]
        events = Trace.toList
               . fmap (Trace.fromList ())
               . splitWithNameTrace
               . fmap (\(WithTime t (WithName name b))
                       -> WithName name (WithTime t b))
               . withTimeNameTraceEvents
                  @DiffusionTestTrace
                  @NtNAddr
               . Trace.take traceNumber
               $ ioSimTrace

     in
        classifyNumberOfPeerStateActionEvents events
      . conjoin
      $ (\ev ->
        let evsList = Trace.toList ev
            lastTime = (\(WithName _ (WithTime t _)) -> t)
                     . last
                     $ evsList
         in classifySimulatedTime lastTime
          $ classifyNumberOfEvents (length evsList)
          $ verify_psa_traces
          $ fmap (\(WithName _ b) -> b)
            ev
        )
      <$> events

  where
    showBucket :: Int -> Int -> String
    showBucket size a | a < size
                      = show a
                      | otherwise
                      = concat [ "["
                               , show (a `div` size * size)
                               , ", "
                               , show (a `div` size * size + size)
                               , ")"
                               ]

    classifyNumberOfPeerStateActionEvents
      :: [Trace () (WithName NtNAddr (WithTime DiffusionTestTrace))]
      -> Property -> Property
    classifyNumberOfPeerStateActionEvents evs =
          label ("Number of Hot -> Warm successful demotions: "
                 ++ showBucket 10 numOfHotToWarmDemotions)
        . label ("Number of Hot -> Warm timeout errors: "
                 ++ showBucket 5 numOfTimeoutErrors)
        . label ("Number of Hot -> Warm ActivecCold errors: "
                 ++ showBucket 5 numOfActiveColdErrors)
        . label ("Number of Warm -> Hot promotions: "
                 ++ showBucket 5 numOfWarmToHotPromotions)
      where
          evs' :: [PeerSelectionActionsTrace NtNAddr NtNVersion]
          evs' = mapMaybe (\case
                              DiffusionPeerSelectionActionsTrace ev
                                -> Just ev
                              _ -> Nothing)
               . fmap (wtEvent . wnEvent)
               . concatMap Trace.toList
               $ evs

          numOfHotToWarmDemotions  = length
                                   . filter (\case
                                                (PeerStatusChanged HotToWarm{})
                                                  -> True
                                                _ -> False)
                                   $ evs'
          numOfTimeoutErrors       = length
                                   . filter (\case
                                                (PeerStatusChangeFailure HotToWarm{} TimeoutError)
                                                  -> True
                                                _ -> False)
                                   $ evs'
          numOfActiveColdErrors    = length
                                   . filter (\case
                                                (PeerStatusChangeFailure HotToWarm{} ActiveCold)
                                                  -> True
                                                _ -> False)
                                   $ evs'

          numOfWarmToHotPromotions = length
                                   . filter (\case
                                                (PeerStatusChanged WarmToHot{})
                                                  -> True
                                                _ -> False)
                                   $ evs'

    verify_psa_traces :: Trace () (WithTime DiffusionTestTrace) -> Property
    verify_psa_traces events =
      let peerSelectionActionsEvents :: [WithTime (PeerSelectionActionsTrace NtNAddr NtNVersion)]
          peerSelectionActionsEvents =
              Trace.toList
            . selectTimedDiffusionPeerSelectionActionsEvents
            $ events

        in
         ( conjoin
         . map
           (\case
             ev@( WithTime _ (PeerStatusChangeFailure (HotToWarm _) TimeoutError)
                , WithTime _ (PeerStatusChangeFailure (HotToWarm _) ActiveCold)
                )
               -> counterexample (show ev)
                $ counterexample (unlines $ map show peerSelectionActionsEvents)
                  False
             _ -> property True
             )
         $ zip       peerSelectionActionsEvents
               (tail peerSelectionActionsEvents)
         )
         .&&.
         ( let f :: [WithTime (PeerSelectionActionsTrace NtNAddr NtNVersion)] -> Property
               f as = conjoin $ g <$> tails as

               g :: [WithTime (PeerSelectionActionsTrace NtNAddr NtNVersion)] -> Property
               g as@(WithTime demotionTime (PeerStatusChanged HotToCooling{}) : as') =
                 case find (\case
                               WithTime _ (PeerStatusChanged ColdToWarm{}) -> True
                               _ -> False)
                           as' of

                   Nothing                         -> property True
                   Just (WithTime promotionTime _) -> counterexample (show as)
                                                      ( promotionTime `diffTime` demotionTime
                                                     >= repromoteDelay config_REPROMOTE_DELAY
                                                      )
               g as@(WithTime demotionTime (PeerStatusChanged WarmToCooling{}) : as') =
                 case find (\case
                               WithTime _ (PeerStatusChanged ColdToWarm{}) -> True
                               _ -> False)
                           as' of

                   Nothing                         -> property True
                   Just (WithTime promotionTime _) -> counterexample (show as)
                                                      ( promotionTime `diffTime` demotionTime
                                                     >= repromoteDelay config_REPROMOTE_DELAY
                                                      )
               g _ = property True
           in
           conjoin
         . fmap ( conjoin
                . fmap f
                . splitWith (\x -> case x of
                                    (_, Just (WithTime _ (PeerStatusChanged ColdToWarm{})))
                                        -> False
                                    (WithTime _ (PeerMonitoringResult{})
                                      , _)
                                        -> False
                                    -- split trace if there are two consecutive `HotToWarm`, this
                                    -- means that the node was restarted.
                                    (WithTime _ (PeerStatusChanged HotToWarm{})
                                      , Just (WithTime _ (PeerStatusChanged HotToWarm{})))
                                        -> False
                                    (WithTime _ (PeerStatusChangeFailure tr _)
                                      , _) -> case tr of
                                                HotToCooling{}  -> False
                                                WarmToCooling{} -> False
                                                _               -> True
                                    _   -> True
                            )
                )
           -- split the trace into different connections
         . splitIntoStreams
             (\case
                WithTime _ (PeerStatusChanged type_)         -> getConnId type_
                WithTime _ (PeerStatusChangeFailure type_ _) -> getConnId type_
                WithTime _ (PeerMonitoringError connId _)    -> Just connId
                WithTime _ (PeerMonitoringResult connId _)   -> Just connId
                WithTime _ (AcquireConnectionError _)        -> Nothing)
         $ peerSelectionActionsEvents
         )

    getConnId :: PeerStatusChangeType addr -> Maybe (ConnectionId addr)
    getConnId (HotToWarm connId) = Just connId
    getConnId (WarmToHot connId) = Just connId
    getConnId (WarmToCooling connId) = Just connId
    getConnId (HotToCooling connId)  = Just connId
    getConnId (ColdToWarm (Just localAddress) remoteAddress) = Just ConnectionId { localAddress, remoteAddress }
    getConnId _ = Nothing


unit_peer_sharing :: Property
unit_peer_sharing =
    let sim :: forall s. IOSim s Void
        sim = diffusionSimulation (toBearerInfo absNoAttenuation)
                                  script
                                  iosimTracer

        events :: Map NtNAddr [TracePeerSelection NtNAddr]
        events = Map.fromList
               . map (\as -> case as of
                        [] -> -- this should be a test failure!
                              error "invariant violation: no traces for one of the nodes"
                        WithName { wnName } : _ -> (wnName, mapMaybe (\a -> case a of
                                                                              DiffusionPeerSelectionTrace b -> Just b
                                                                              _ -> Nothing)
                                                          . map (wtEvent . wnEvent)
                                                          $ as))
               $ events'

        events' = Trace.toList
               . splitWithNameTrace
               . fmap (\(WithTime t (WithName name b))
                       -> WithName name (WithTime t b))
               . withTimeNameTraceEvents
                  @DiffusionTestTrace
                  @NtNAddr
               -- We need roughly 1200 because:
               -- * first peer sharing request will be issued after
               --   `policyPeerSharAcitvationDelay = 300`
               -- * this request will not bring any new peers, because non of the peers
               --    are yet mature
               -- * inbound connections become mature at 900s (15 mins)
               -- * next peer share request happens after 900s, e.g. around 1200s.
               . Trace.takeWhile (\se -> case se of
                                          SimEvent    {seTime} -> seTime < Time 1250
                                          SimPOREvent {seTime} -> seTime < Time 1250
                                          _                    -> False
                                 )
               $ runSimTrace sim

        verify :: NtNAddr
               -> [TracePeerSelection NtNAddr]
               -> All
        verify addr as | addr == ip_2 =
          let receivedPeers :: Set NtNAddr
              receivedPeers =
                  fold
                . mapMaybe (\case
                              TracePeerShareResults as' -> Just $ fold [ Set.fromList addrs
                                                                       | (_, Right (PeerSharingResult addrs)) <- as'
                                                                       ]
                              _ -> Nothing)
                $          as
          in All $
             counterexample (concat [ show ip_0
                                    , " is not a member of received peers "
                                    , show receivedPeers
                                    ]) $
             ip_0 `Set.member` receivedPeers
        verify _ _ = All True

    in
      -- counterexample (ppEvents trace) $
      counterexample (Map.foldrWithKey (\addr evs s -> concat [ "\n\n===== "
                                                              , show addr
                                                              , " =====\n\n"
                                                              ]
                                                          ++ intercalate "\n" (map show evs)
                                                          ++ s) "" events) $
      Map.foldMapWithKey verify events
  where
    -- initial topology
    -- ip_0  -> ip_1 <- ip_2
    -- target topology
    -- ip_0 <-> ip_1 <- ip_2 -> ip_0
    -- e.g.
    -- * ip_1 should learn about ip_0 by noticing an inbound connection (light
    --   peer sharing), and thus it should be marked as `DoAdvertisePeer`
    -- * ip_2 should learn about ip_0 from ip_1 by peer sharing

    ip_0 = TestAddress $ IPAddr (IP.IPv4 (IP.toIPv4 [0,0,0,0])) 3000
    -- ra_0 = RelayAccessAddress (IP.IPv4 (IP.toIPv4 [0,0,0,0])) 3000

    ip_1 = TestAddress $ IPAddr (IP.IPv4 (IP.toIPv4 [0,0,0,0])) 3001
    ra_1 = RelayAccessAddress (IP.IPv4 (IP.toIPv4 [0,0,0,0])) 3001

    ip_2 = TestAddress $ IPAddr (IP.IPv4 (IP.toIPv4 [0,0,0,0])) 3002
    -- ra_2 = RelayAccessAddress (IP.IPv4 (IP.toIPv4 [0,0,0,0])) 3002

    targets x = let t = PeerSelectionTargets {
                          targetNumberOfRootPeers = x,
                          targetNumberOfKnownPeers = x,
                          targetNumberOfEstablishedPeers = x,
                          targetNumberOfActivePeers = x,
                          targetNumberOfKnownBigLedgerPeers = 0,
                          targetNumberOfEstablishedBigLedgerPeers = 0,
                          targetNumberOfActiveBigLedgerPeers = 0 }
                in ConsensusModePeerTargets { deadlineTargets = t, syncTargets = t }


    defaultNodeArgs naConsensusMode = NodeArgs {
        naSeed = 0,
        naDiffusionMode = InitiatorAndResponderDiffusionMode,
        naMbTime = Nothing,
        naPublicRoots = mempty,
        naBootstrapPeers = singletonScript DontUseBootstrapPeers,
        naAddr = undefined,
        naPeerSharing = PeerSharingEnabled,
        naLocalRootPeers = undefined,
        naLedgerPeers = singletonScript (LedgerPools []),
        naPeerTargets = ConsensusModePeerTargets {
            deadlineTargets = nullPeerSelectionTargets,
            syncTargets = nullPeerSelectionTargets },
        naDNSTimeoutScript = singletonScript (DNSTimeout 300),
        naDNSLookupDelayScript = singletonScript (DNSLookupDelay 0.01),
        naChainSyncEarlyExit = False,
        naChainSyncExitOnBlockNo = Nothing,
        naFetchModeScript = singletonScript FetchModeDeadline,
        naConsensusMode
      }

    script = DiffusionScript
               (mainnetSimArgs 3)
               (singletonScript (mempty, ShortDelay))
               [ ( (defaultNodeArgs GenesisMode) { naAddr = ip_0,
                                     naLocalRootPeers = [(1, 1, Map.fromList [(ra_1, (DoNotAdvertisePeer, IsNotTrustable))])],
                                     naPeerTargets = targets 1
                                   }
                 , [JoinNetwork 0]
                 )
               , ( (defaultNodeArgs PraosMode) { naAddr = ip_1,
                                     naLocalRootPeers = [],
                                     naPeerTargets = targets 2
                                   }
                 , [JoinNetwork 0]
                 )
               , ( (defaultNodeArgs GenesisMode) { naAddr = ip_2,
                                     naLocalRootPeers = [(1, 1, Map.fromList [(ra_1, (DoNotAdvertisePeer, IsNotTrustable))])],
                                     naPeerTargets = targets 2
                                   }
                 , [JoinNetwork 0]
                 )
               ]


-- | This property verifies that when nodes are running without network
-- attenuation, decreasing numbers by churn never timeouts.
--
prop_churn_notimeouts :: SimTrace Void
                      -> Int
                      -> Property
prop_churn_notimeouts ioSimTrace traceNumber =
   let events :: [Events DiffusionTestTrace]
       events = Trace.toList
                . fmap ( Signal.eventsFromList
                      . fmap (\(WithName _ (WithTime t b)) -> (t, b))
                      )
               . splitWithNameTrace
               . fmap (\(WithTime t (WithName name b)) -> WithName name (WithTime t b))
               . withTimeNameTraceEvents
                  @DiffusionTestTrace
                  @NtNAddr
               . Trace.take traceNumber
               $ ioSimTrace
    in  conjoin
      $ (\evs ->
            let evsList :: [TracePeerSelection NtNAddr]
                evsList = snd <$> eventsToList (selectDiffusionPeerSelectionEvents evs)
            in property $ counterexample (intercalate "\n" (show <$> eventsToList evs))
                        $ all noChurnTimeout evsList
        )
     <$> events
  where
    noChurnTimeout :: TracePeerSelection NtNAddr -> Bool
    noChurnTimeout (TraceChurnTimeout _ DecreasedActivePeers _)               = False
    noChurnTimeout (TraceChurnTimeout _ DecreasedActiveBigLedgerPeers _)      = False
    noChurnTimeout (TraceChurnTimeout _ DecreasedEstablishedPeers _)          = False
    noChurnTimeout (TraceChurnTimeout _ DecreasedEstablishedBigLedgerPeers _) = False
    noChurnTimeout (TraceChurnTimeout _ DecreasedKnownPeers _)                = False
    noChurnTimeout (TraceChurnTimeout _ DecreasedKnownBigLedgerPeers _)       = False
    noChurnTimeout  TraceChurnTimeout {}                                      = True
    noChurnTimeout  _                                                         = True


-- | Verify that churn trace consists of repeated list of actions:
--
-- * `DecreasedActivePeers`
-- * `IncreasedActivePeers`
-- * `DecreasedActiveBigLedgerPeers`
-- * `IncreasedActiveBigLedgerPeers`
-- * `DecreasedEstablishedPeers`
-- * `DecreasedEstablishedBigLedgerPeers`
-- * `DecreasedKnownPeers`
-- * `IncreasedKnownPeers`
-- * `IncreasedEstablishedPeers`
-- * `IncreasedEstablishedBigLedgerPeers`
--
prop_churn_steps :: SimTrace Void
                 -> Int
                 -> Property
prop_churn_steps ioSimTrace traceNumber =
    let events :: [Events DiffusionTestTrace]
        events =  Trace.toList
               . fmap ( Signal.eventsFromList
                      . fmap (\(WithName _ (WithTime t b)) -> (t, b))
                      )
               . splitWithNameTrace
               . fmap (\(WithTime t (WithName name b)) -> WithName name (WithTime t b))
               . withTimeNameTraceEvents
                  @DiffusionTestTrace
                  @NtNAddr
               . Trace.take traceNumber
               $ ioSimTrace

    in   conjoin
       $ (\evs ->
           let evsList :: [(Time, TracePeerSelection NtNAddr)]
               evsList = eventsToList (selectDiffusionPeerSelectionEvents evs)
           in  counterexample (intercalate "\n" (show <$> evsList))
             . churnTracePredicate
             . mapMaybe (\case
                          (_, TraceChurnAction _ a _)  -> Just a
                          (_, TraceChurnTimeout _ a _) -> Just a
                          _                            -> Nothing)
             $ evsList
         )
      <$> events
  where
    -- check churn trace
    churnTracePredicate :: [ChurnAction] -> Bool
    churnTracePredicate as =
        all (\(a, b) -> a == b)
      . zip as
      . cycle
      $ [ DecreasedActivePeers
        , IncreasedActivePeers
        , DecreasedActiveBigLedgerPeers
        , IncreasedActiveBigLedgerPeers
        , DecreasedEstablishedPeers
        , DecreasedEstablishedBigLedgerPeers
        , DecreasedKnownPeers
        , IncreasedKnownPeers
        , IncreasedEstablishedPeers
        , IncreasedEstablishedBigLedgerPeers
        ]




-- | Like `(takeWhile f as, dropWhile f as)`
--
splitWhile :: (a -> Bool) -> [a] -> ([a], [a])
splitWhile _ [] = ([], [])
splitWhile f as@(a : as') = if f a
                            then case splitWhile f as' of
                                   (hs, ts) -> (a : hs, ts)
                            else ([], as)




splitWith :: forall a.
             ((a, Maybe a) -> Bool)
             -- ^ a predicate on current and next element in the list.
          ->  [a]
          -> [[a]]
splitWith f = map unzip' . go . zip'
  where
    zip' :: [a] -> [(a, Maybe a)]
    zip' xs = xs `zip` (map Just (tail xs) ++ [Nothing])

    -- reverse of `zip'`
    unzip' :: [(a,Maybe a)] -> [a]
    unzip' = (\(xs, ys) -> take 1 xs ++ catMaybes ys) . unzip

    go :: [(a,Maybe a)] -> [[(a,Maybe a)]]
    go as =
        case splitWhile f as of
          ([], [])           -> []
          ([], (a, _) : as') -> [(a,Nothing)] : go as'
          (xs, [])           -> xs : []
          (xs, _      : as') -> xs : go as'



splitIntoStreams :: Ord k
                 => (a -> Maybe k)
                 -- ^ index function, 'Nothing` values are ignored
                 ->  [a]
                 -> [[a]]
splitIntoStreams f = Map.elems
                   . Map.fromListWith (\a b -> b ++ a)
                   . map (\a -> (fromJust (f a), [a]))
                   . filter (isJust . f)


{-
-- | 'splitWith' partitions elements into sub-lists.
--
prop_splitWith :: ( Arbitrary a
                  , Eq a
                  , Show a
                  )
               => ((a, Maybe a) -> Bool)
               -> [a]
               -> Property
prop_splitWith f as = foldr (++) [] (splitWith f as) === as
-}


-- | A variant of ouroboros-network-framework
-- 'Test.Ouroboros.Network.Server2.prop_inbound_governor_valid_transitions'
-- but for running on Diffusion. This means it has to have in consideration the
-- the logs for all nodes running will all appear in the trace and the test
-- property should only be valid while a given node is up and running.
--
prop_diffusion_ig_valid_transitions :: SimTrace Void
                                    -> Int
                                    -> Property
prop_diffusion_ig_valid_transitions ioSimTrace traceNumber =
    let events :: [Trace () (WithName NtNAddr (WithTime DiffusionTestTrace))]
        events = Trace.toList
               . fmap (Trace.fromList ())
               . splitWithNameTrace
               . fmap (\(WithTime t (WithName name b))
                       -> WithName name (WithTime t b))
               . withTimeNameTraceEvents
                  @DiffusionTestTrace
                  @NtNAddr
               . Trace.take traceNumber
               $ ioSimTrace

     in conjoin
      $ (\ev ->
        let evsList = Trace.toList ev
            lastTime = (\(WithName _ (WithTime t _)) -> t)
                     . last
                     $ evsList
         in classifySimulatedTime lastTime
          $ classifyNumberOfEvents (length evsList)
          $ verify_ig_valid_transitions
          $ (\(WithName _ (WithTime _ b)) -> b)
          <$> ev
        )
      <$> events

  where
    verify_ig_valid_transitions :: Trace () DiffusionTestTrace -> Property
    verify_ig_valid_transitions events =
      let remoteTransitionTraceEvents :: Trace () (IG.RemoteTransitionTrace NtNAddr)
          remoteTransitionTraceEvents =
            selectDiffusionInboundGovernorTransitionEvents events

       in  property
         . bifoldMap
            ( \ _ -> All True )
            ( \ TransitionTrace {ttPeerAddr = peerAddr, ttTransition = tr} ->
                  All
                . counterexample (concat [ "Unexpected transition: "
                                         , show peerAddr
                                         , " "
                                         , show tr
                                         ])
                . verifyRemoteTransition
                $ tr
            )
         $ remoteTransitionTraceEvents

-- | A variant of ouroboros-network-framework
-- 'Test.Ouroboros.Network.Server2.prop_inbound_governor_valid_transition_order'
-- but for running on Diffusion. This means it has to have in consideration the
-- the logs for all nodes running will all appear in the trace and the test
-- property should only be valid while a given node is up and running.
--
prop_diffusion_ig_valid_transition_order :: SimTrace Void
                                         -> Int
                                         -> Property
prop_diffusion_ig_valid_transition_order ioSimTrace traceNumber =
    let events :: [Trace () (WithName NtNAddr (WithTime DiffusionTestTrace))]
        events = Trace.toList
               . fmap (Trace.fromList ())
               . splitWithNameTrace
               . fmap (\(WithTime t (WithName name b))
                       -> WithName name (WithTime t b))
               . withTimeNameTraceEvents
                  @DiffusionTestTrace
                  @NtNAddr
               . Trace.take traceNumber
               $ ioSimTrace

     in conjoin
      $ (\ev ->
        let evsList = Trace.toList ev
            lastTime = (\(WithName _ (WithTime t _)) -> t)
                     . last
                     $ evsList
         in counterexample (Trace.ppTrace show show ev)
          $ classifySimulatedTime lastTime
          $ classifyNumberOfEvents (length evsList)
          $ verify_ig_valid_transition_order
          $ (\(WithName _ (WithTime _ b)) -> b)
          <$> ev
        )
      <$> events

  where
    verify_ig_valid_transition_order :: Trace () DiffusionTestTrace -> Property
    verify_ig_valid_transition_order events =

      let remoteTransitionTraceEvents :: Trace () (IG.RemoteTransitionTrace NtNAddr)
          remoteTransitionTraceEvents =
            selectDiffusionInboundGovernorTransitionEvents events

       in property
        . bifoldMap
           (const mempty)
           (verifyRemoteTransitionOrder False)
        . fmap (map ttTransition)
        . groupConns id remoteStrIsFinalTransition
        $ remoteTransitionTraceEvents

-- | A variant of ouroboros-network-framework
-- 'Test.Ouroboros.Network.Server2.prop_timeouts_enforced'
-- but for running on Diffusion. This means it has to have in consideration the
-- the logs for all nodes running will all appear in the trace and the test
-- property should only be valid while a given node is up and running.
--
-- This test tests simultaneously the ConnectionManager and InboundGovernor's
-- timeouts.
--
prop_diffusion_timeouts_enforced :: SimTrace Void
                                 -> Int
                                 -> Property
prop_diffusion_timeouts_enforced ioSimTrace traceNumber =
    let events :: [Trace () (Time, DiffusionTestTrace)]
        events = Trace.toList
               . fmap ( Trace.fromList ()
                      . fmap (\(WithName _ (WithTime t b)) -> (t, b)))
               . splitWithNameTrace
               . fmap (\(WithTime t (WithName name b))
                       -> WithName name (WithTime t b))
               . withTimeNameTraceEvents
                  @DiffusionTestTrace
                  @NtNAddr
               . Trace.take traceNumber
               $ ioSimTrace

     in conjoin
      $ (\ev ->
        let evsList = Trace.toList ev
            lastTime = fst
                     . last
                     $ evsList
         in classifySimulatedTime lastTime
          $ classifyNumberOfEvents (length evsList)
          $ verify_timeouts
            ev
        )
      <$> events

  where
    verify_timeouts :: Trace () (Time, DiffusionTestTrace) -> Property
    verify_timeouts events =
      let transitionSignal :: Trace (SimResult ()) [(Time, AbstractTransitionTrace NtNAddr)]
          transitionSignal = Trace.fromList (MainReturn (Time 0) (Labelled (ThreadId []) (Just "main")) () [])
                           . Trace.toList
                           . groupConns snd abstractStateIsFinalTransition
                           . selectDiffusionConnectionManagerTransitionEventsTime
                           $ events

       in property
        $ verifyAllTimeouts True transitionSignal


-- Utils
--

data JoinedOrKilled = Joined | Killed
  deriving (Eq, Show)

-- Similar to 'either' but for 'JoinedOrKilled'
fromJoinedOrKilled :: c -> c -> JoinedOrKilled -> c
fromJoinedOrKilled j _ Joined = j
fromJoinedOrKilled _ k Killed = k

getTime :: (Time, ThreadId (IOSim s), Maybe ThreadLabel, SimEventType) -> Time
getTime (t, _, _, _) = t

classifySimulatedTime :: Time -> Property -> Property
classifySimulatedTime lastTime =
        classify (lastTime <= Time (10 * 60)) "simulation time <= 10min"
      . classify (lastTime >  Time (10 * 60)      && lastTime <= Time (20 * 60)) "10min < simulation time <= 20min"
      . classify (lastTime >  Time (20 * 60)      && lastTime <= Time (40 * 60)) "20min < simulation time <= 40min"
      . classify (lastTime >  Time (40 * 60)      && lastTime <= Time (60 * 60)) "40min < simulation time <= 1H"
      . classify (lastTime >  Time (60 * 60)      && lastTime <= Time (5 * 60 * 60)) "1H < simulation time <= 5H"
      . classify (lastTime >  Time (5 * 60 * 60)  && lastTime <= Time (10 * 60 * 60)) "5H < simulation time <= 10H"
      . classify (lastTime >  Time (10 * 60 * 60) && lastTime <= Time (24 * 60 * 60)) "10H < simulation time <= 1 Day"
      . classify (lastTime >= Time (24 * 60 * 60)) "simulation time >= 1 Day"

classifyNumberOfEvents :: Int -> Property -> Property
classifyNumberOfEvents nEvents =
        classify (nEvents <= 100) "Nº Events <= 100"
      . classify (nEvents >= 1000) "Nº Events >= 1000"
      . classify (nEvents >= 10000) "Nº Events >= 10000"
      . classify (nEvents >= 50000) "Nº Events >= 50000"

withTimeNameTraceEvents :: forall b name r. (Typeable b, Typeable name)
                        => Trace r SimEvent
                        -> Trace r (WithTime (WithName name b))
withTimeNameTraceEvents = traceSelectTraceEventsDynamic
                            @r
                            @(WithTime (WithName name b))

selectDiffusionPeerSelectionEvents :: Events DiffusionTestTrace
                                   -> Events (TracePeerSelection NtNAddr)
selectDiffusionPeerSelectionEvents = Signal.selectEvents
                    (\case DiffusionPeerSelectionTrace e -> Just e
                           _                             -> Nothing)

selectDiffusionSimulationTrace :: Events DiffusionTestTrace
                               -> Events DiffusionSimulationTrace
selectDiffusionSimulationTrace = Signal.selectEvents
                    (\case DiffusionDiffusionSimulationTrace e -> Just e
                           _                                   -> Nothing)

selectDiffusionPeerSelectionState :: Eq a
                                  => (forall peerconn. Governor.PeerSelectionState NtNAddr peerconn -> a)
                                  -> Events DiffusionTestTrace
                                  -> Signal a
selectDiffusionPeerSelectionState f =
    Signal.nub
  -- TODO: #3182 Rng seed should come from quickcheck.
  . (\evs ->
       let evsList = Signal.eventsToList evs
       in
         case evsList of
           [] -> Signal.fromChangeEvents (initialState PraosMode) (snd <$> evs)
           (_, (consensusMode, _)):_ ->
             Signal.fromChangeEvents (initialState consensusMode) (snd <$> evs)
    )
  . Signal.selectEvents
      (\case
        DiffusionDebugPeerSelectionTrace (TraceGovernorState _ _ st) -> Just (Governor.consensusMode st, f st)
        _                                                            -> Nothing)
  where
    initialState consensusMode =
      f $ Governor.emptyPeerSelectionState
            (mkStdGen 42)
            consensusMode
            (MinBigLedgerPeersForTrustedState 0) -- ^ todo: fix

selectDiffusionPeerSelectionState' :: (forall peerconn. Governor.PeerSelectionState NtNAddr peerconn -> a)
                                  -> Events DiffusionTestTrace
                                  -> Signal a
selectDiffusionPeerSelectionState' f =
  -- TODO: #3182 Rng seed should come from quickcheck.
    Signal.fromChangeEvents (f $ Governor.emptyPeerSelectionState (mkStdGen 42)
                                                                  PraosMode
                                                                  (MinBigLedgerPeersForTrustedState 0))
  . Signal.selectEvents
      (\case
        DiffusionDebugPeerSelectionTrace (TraceGovernorState _ _ st) -> Just (f st)
        _                                                            -> Nothing)

selectDiffusionConnectionManagerEvents
  :: Trace () DiffusionTestTrace
  -> Trace () (CM.Trace NtNAddr
                 (ConnectionHandlerTrace
                    NtNVersion
                    NtNVersionData))
selectDiffusionConnectionManagerEvents =
  Trace.fromList ()
  . mapMaybe
     (\case DiffusionConnectionManagerTrace e -> Just e
            _                                 -> Nothing)
  . Trace.toList

selectTimedDiffusionPeerSelectionActionsEvents
  :: Trace () (WithTime DiffusionTestTrace)
  -> Trace () (WithTime (PeerSelectionActionsTrace NtNAddr NtNVersion))
selectTimedDiffusionPeerSelectionActionsEvents =
    Trace.fromList ()
  . mapMaybe
      (\case WithTime { wtTime
                      , wtEvent = DiffusionPeerSelectionActionsTrace e
                      } -> Just WithTime { wtTime, wtEvent = e }
             _          -> Nothing)
  . Trace.toList

selectDiffusionConnectionManagerTransitionEvents
  :: Trace () DiffusionTestTrace
  -> Trace () (AbstractTransitionTrace NtNAddr)
selectDiffusionConnectionManagerTransitionEvents =
  Trace.fromList ()
  . mapMaybe
     (\case DiffusionConnectionManagerTransitionTrace e -> Just e
            _                                           -> Nothing)
  . Trace.toList

selectDiffusionConnectionManagerTransitionEventsTime
  :: Trace () (Time, DiffusionTestTrace)
  -> Trace () (Time, AbstractTransitionTrace NtNAddr)
selectDiffusionConnectionManagerTransitionEventsTime =
  Trace.fromList ()
  . mapMaybe
     (\case (t, DiffusionConnectionManagerTransitionTrace e) -> Just (t, e)
            _                                                -> Nothing)
  . Trace.toList

selectDiffusionInboundGovernorTransitionEvents
  :: Trace () DiffusionTestTrace
  -> Trace () (IG.RemoteTransitionTrace NtNAddr)
selectDiffusionInboundGovernorTransitionEvents =
  Trace.fromList ()
  . mapMaybe
     (\case DiffusionInboundGovernorTransitionTrace e -> Just e
            _                                         -> Nothing)
  . Trace.toList

toBearerInfo :: AbsBearerInfo -> BearerInfo
toBearerInfo abi =
    BearerInfo {
        biConnectionDelay      = delay (abiConnectionDelay abi),
        biInboundAttenuation   = attenuation (abiInboundAttenuation abi),
        biOutboundAttenuation  = attenuation (abiOutboundAttenuation abi),
        biInboundWriteFailure  = abiInboundWriteFailure abi,
        biOutboundWriteFailure = abiOutboundWriteFailure abi,
        biAcceptFailures       = (\(errDelay, ioErr) -> (delay errDelay, ioErr)) <$> abiAcceptFailure abi,
        biSDUSize              = toSduSize (abiSDUSize abi)
      }


-- | Like 'take' but includes all the traces of the timestamp at the given
-- index.
--
takeUntilEndofTurn :: Int
                   -> [(Time, ThreadId (IOSim s), Maybe ThreadLabel, SimEventType)]
                   -> [(Time, ThreadId (IOSim s), Maybe ThreadLabel, SimEventType)]
takeUntilEndofTurn n as =
    case splitAt n as of
        ([],  _) -> []
        (hs, ts) ->
            hs ++ takeWhile (\(t,_,_,_) -> t <= tmax) ts
          where
            tmax :: Time
            tmax = case last hs of (t,_,_,_) -> t


labelDiffusionScript :: DiffusionScript -> Property -> Property
labelDiffusionScript (DiffusionScript args _ nodes) =
      label ("sim args: "
              ++ show args)
    . label ("Nº nodes: "
              ++ show (length nodes))
    . label ("Nº nodes in InitiatorOnlyDiffusionMode: "
              ++ show (length $ filter ((== InitiatorOnlyDiffusionMode) . naDiffusionMode . fst) nodes))
    -- todo: add label for GenesisMode syncTargets
    . label ("Nº active peers: "
              ++ show (sum . map (targetNumberOfActivePeers . deadlineTargets . naPeerTargets . fst) $ nodes))
    . label ("Nº active big ledger peers: "
              ++ show (sum . map (targetNumberOfActiveBigLedgerPeers . deadlineTargets . naPeerTargets . fst) $ nodes))
    . label ("average number of active local roots: "
              ++ show (average . map (sum . map (\(HotValency v,_,_) -> v) . naLocalRootPeers . fst) $ nodes))
  where
    average :: [Int] -> Float
    average [] = 0
    average as = realToFrac (sum as) / realToFrac (length as)

    -- TODO: it would be nice to check if the graph is connected if all dns
    -- names can be resolved.

-- | filter out big ledger peers
--
dropBigLedgerPeers
    :: (Governor.PeerSelectionState NtNAddr peerconn -> Set NtNAddr)
    ->  Governor.PeerSelectionState NtNAddr peerconn -> Set NtNAddr
dropBigLedgerPeers f =
  \st -> f st Set.\\ PublicRootPeers.getBigLedgerPeers (Governor.publicRootPeers st)

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

import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadTime.SI (DiffTime, Time (Time), addTime,
           diffTime)
import Control.Monad.IOSim
import Data.Bifoldable (bifoldMap)

import Data.Foldable (fold)
import Data.IP qualified as IP
import Data.List (find, foldl', intercalate, tails)
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

import System.Random (mkStdGen)

import Network.DNS.Types qualified as DNS

import Ouroboros.Network.ConnectionHandler (ConnectionHandlerTrace)
import Ouroboros.Network.ConnectionId
import Ouroboros.Network.ConnectionManager.Types
import Ouroboros.Network.ExitPolicy (RepromoteDelay (..))
import Ouroboros.Network.InboundGovernor hiding (TrUnexpectedlyFalseAssertion)
import Ouroboros.Network.PeerSelection.Governor hiding (PeerSelectionState (..))
import Ouroboros.Network.PeerSelection.Governor qualified as Governor
import Ouroboros.Network.PeerSelection.PeerStateActions
import Ouroboros.Network.PeerSelection.RootPeersDNS.DNSActions
import Ouroboros.Network.PeerSelection.State.EstablishedPeers qualified as EstablishedPeers
import Ouroboros.Network.PeerSelection.State.KnownPeers qualified as KnownPeers
import Ouroboros.Network.PeerSelection.State.LocalRootPeers qualified as LocalRootPeers
import Ouroboros.Network.PeerSelection.Types
import Ouroboros.Network.Server2 (ServerTrace (..))
import Ouroboros.Network.Testing.Data.AbsBearerInfo
import Ouroboros.Network.Testing.Data.Script
import Ouroboros.Network.Testing.Data.Signal
import Ouroboros.Network.Testing.Data.Signal qualified as Signal
import Ouroboros.Network.Testing.Utils hiding (SmallDelay, debugTracer)

import Simulation.Network.Snocket (BearerInfo (..))

import Test.Ouroboros.Network.Diffusion.Node (config_REPROMOTE_DELAY)
import Test.Ouroboros.Network.Diffusion.Node.NodeKernel
import Test.Ouroboros.Network.Testnet.Simulation.Node
import Test.QuickCheck
import Test.QuickCheck.Monoids
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

import Control.Exception (AssertionFailed (..), catch, evaluate)
import Ouroboros.Network.BlockFetch (FetchMode (..), TraceFetchClientState (..))
import Ouroboros.Network.ConnectionManager.Test.Timeouts (TestProperty (..),
           classifyActivityType, classifyEffectiveDataFlow,
           classifyNegotiatedDataFlow, classifyPrunings, classifyTermination,
           groupConns, mkProperty, ppTransition, verifyAllTimeouts)
import Ouroboros.Network.ConnectionManager.Test.Utils
           (abstractStateIsFinalTransition, connectionManagerTraceMap,
           validTransitionMap, verifyAbstractTransition,
           verifyAbstractTransitionOrder)
import Ouroboros.Network.InboundGovernor.Test.Utils (inboundGovernorTraceMap,
           remoteStrIsFinalTransition, serverTraceMap, validRemoteTransitionMap,
           verifyRemoteTransition, verifyRemoteTransitionOrder)
import Ouroboros.Network.Mock.ConcreteBlock (BlockHeader)
import Ouroboros.Network.NodeToNode (DiffusionMode (..))
import Ouroboros.Network.PeerSelection.Bootstrap (UseBootstrapPeers (..))
import Ouroboros.Network.PeerSelection.PeerAdvertise (PeerAdvertise (..))
import Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing (..))
import Ouroboros.Network.PeerSelection.PeerTrustable (PeerTrustable (..))
import Ouroboros.Network.PeerSelection.PublicRootPeers qualified as PublicRootPeers
import Ouroboros.Network.PeerSelection.RootPeersDNS.LocalRootPeers
           (TraceLocalRootPeers (..))
import Ouroboros.Network.PeerSelection.State.LocalRootPeers (HotValency (..),
           WarmValency (..))
import Ouroboros.Network.PeerSharing (PeerSharingResult (..))
import Test.Ouroboros.Network.LedgerPeers (LedgerPools (..))

import Control.Monad.Class.MonadTest (exploreRaces)
import Ouroboros.Network.PeerSelection.Bootstrap (requiresBootstrapPeers)
import Ouroboros.Network.PeerSelection.LedgerPeers
import Cardano.Slotting.Block (BlockNo(..))
import Data.Ratio ((%))

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
    [ testProperty "no failure"
                   (testWithIOSimPOR prop_diffusion_nofail 10000)
    , testProperty "unit1"
                   unit_1
    , testProperty "unit2"
                   unit_2
    , testProperty "no livelock"
                   (testWithIOSimPOR prop_diffusion_nolivelock 10000)
    , testProperty "dns can recover from fails"
                   (testWithIOSimPOR prop_diffusion_dns_can_recover 10000)
    , testProperty "target established public"
                   (testWithIOSimPOR prop_diffusion_target_established_public 10000)
    , testProperty "target active public"
                   (testWithIOSimPOR prop_diffusion_target_active_public 10000)
    , testProperty "target established local"
                   (testWithIOSimPOR prop_diffusion_target_established_local 10000)
    , testProperty "target active local"
                   (testWithIOSimPOR prop_diffusion_target_active_local 10000)
    , testProperty "target active root"
                   (testWithIOSimPOR prop_diffusion_target_active_root 10000)
    , testProperty "target active below"
                   (testWithIOSimPOR prop_diffusion_target_active_below 10000)
    , testProperty "target active local below"
                   (testWithIOSimPOR prop_diffusion_target_active_local_below 10000)
    , testProperty "async demotion"
                   (testWithIOSimPOR prop_diffusion_async_demotions 10000)
    , testProperty "target active local above"
                   (testWithIOSimPOR prop_diffusion_target_active_local_above 10000)
    , testProperty "connection manager valid transitions"
                   (testWithIOSimPOR prop_diffusion_cm_valid_transitions 10000)
    , testProperty "connection manager valid transition order"
                   (testWithIOSimPOR prop_diffusion_cm_valid_transition_order 10000)
    , testProperty "connection manager no dodgy traces"
                   (testWithIOSimPOR prop_diffusion_cm_no_dodgy_traces 10000)
    , testProperty "peer selection actions no dodgy traces"
                   (testWithIOSimPOR prop_diffusion_peer_selection_actions_no_dodgy_traces 10000)
    , testProperty "inbound governor valid transitions"
                   (testWithIOSimPOR prop_diffusion_ig_valid_transitions 10000)
    , testProperty "inbound governor valid transition order"
                   (testWithIOSimPOR prop_diffusion_ig_valid_transition_order 10000)
    , testProperty "cm & ig timeouts enforced"
                   (testWithIOSimPOR prop_diffusion_timeouts_enforced 10000)
    , testProperty "any Cold async demotion"
                   (testWithIOSimPOR prop_track_coolingToCold_demotions 10000)
    , testProperty "only bootstrap peers in fallback state"
                   (testWithIOSimPOR prop_only_bootstrap_peers_in_fallback_state 10000)
    , testProperty "no non trustable peers before caught up state"
                   (testWithIOSimPOR prop_no_non_trustable_peers_before_caught_up_state 10000)
    , testGroup "Churn"
      [ testProperty "no timeouts"
                     (testWithIOSimPOR prop_churn_notimeouts 10000)
      , testProperty "steps"
                     (testWithIOSimPOR prop_churn_steps 10000)
      ]
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
    $ exploreSimTrace (\a -> a {explorationReplay = Just ControlDefault })
                      sim $ \_ ioSimTrace ->
        f ioSimTrace  traceNumber

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
            . fmap (\(WithTime t (WithName n b)) -> (t, n, b))
            . withTimeNameTraceEvents
                   @DiffusionTestTrace
                   @NtNAddr
            $ x

      x = Trace.take traceNumber
        $ ioSimTrace

   -- run in `IO` so we can catch the pure 'AssertionFailed' exception
   in ioProperty $ do
     r <-
       evaluate ( foldl' (flip seq) True
              $ [ assertPeerSelectionState st ()
                | (_, _, DiffusionDebugPeerSelectionTrace (TraceGovernorState _ _ st)) <- trace ]
              )
       `catch` \(AssertionFailed _) -> return False
     case r of
       True  -> return $ property True
       False -> do
         putStrLn $ intercalate "\n" $ map show $ Trace.toList x
         -- the ioSimTrace is infinite, but it will terminate with `AssertionFailed`
         -- error "impossible!"
         return $ property False

unit_2 :: Property
unit_2 = testWithIOSimPOR prop_diffusion_nofail 100000 bi ds
  where
    bi = AbsBearerInfo {abiConnectionDelay = NormalDelay, abiInboundAttenuation = SpeedAttenuation NormalSpeed (Time 5) 119.5, abiOutboundAttenuation = NoAttenuation FastSpeed, abiInboundWriteFailure = Nothing, abiOutboundWriteFailure = Just 14, abiAcceptFailure = Just (NormalDelay,AbsIOErrResourceExhausted), abiSDUSize = NormalSDU}
    ds = DiffusionScript
            (SimArgs 1 10)
            (Script ((Map.fromList [("test1",[("0.0.0.107",300)])],LongDelay) :| [(Map.fromList [],LongDelay),(Map.fromList [("test1",[("0.0.2.118",300)])],LongDelay),(Map.fromList [("test1",[("0.0.1.52",300)])],NoDelay),(Map.fromList [("test1",[("0.0.1.225",300)])],NoDelay),(Map.fromList [("test1",[("0.0.1.229",300)])],ShortDelay),(Map.fromList [("test1",[("0.0.1.101",300)])],LongDelay),(Map.fromList [("test1",[("0.0.1.116",300)])],LongDelay)]))
            [(NodeArgs (1) InitiatorAndResponderDiffusionMode (Just 224) (Map.fromList [(RelayAccessDomain "test1" 65532,DoAdvertisePeer)]) (Script (UseBootstrapPeers [RelayAccessAddress "0.0.0.135" 19,RelayAccessDomain "test5" 65531,RelayAccessDomain "test1" 5,RelayAccessDomain "test3" 65533,RelayAccessAddress "0:382:0:1e8:0:18d:0:1b5" 14] :| [UseBootstrapPeers [RelayAccessDomain "test2" 65531,RelayAccessDomain "test1" 65524,RelayAccessDomain "test1" 65524,RelayAccessDomain "test5" 65525,RelayAccessAddress "0:1b2:0:29:0:c4:0:129" 1,RelayAccessDomain "test4" 4,RelayAccessDomain "test4" 65522,RelayAccessAddress "0:65:0:1b1:0:193:0:147" 65519,RelayAccessDomain "test1" 65523,RelayAccessDomain "test3" 65521,RelayAccessAddress "0.0.3.89" 2,RelayAccessAddress "0.0.1.36" 9,RelayAccessDomain "test4" 2,RelayAccessDomain "test2" 4,RelayAccessDomain "test1" 15],UseBootstrapPeers [RelayAccessAddress "0.0.1.26" 65524,RelayAccessDomain "test4" 65527,RelayAccessDomain "test2" 65535,RelayAccessDomain "test5" 65531,RelayAccessDomain "test3" 65517,RelayAccessAddress "0.0.1.165" 65533,RelayAccessDomain "test3" 6,RelayAccessAddress "0:6f:0:51:0:1ea:0:18a" 17,RelayAccessDomain "test2" 65529,RelayAccessDomain "test3" 17,RelayAccessDomain "test3" 65533,RelayAccessAddress "0:bb:0:1c3:0:1c0:0:17" 65523],UseBootstrapPeers [RelayAccessAddress "0.0.0.137" 8,RelayAccessAddress "0:f9:0:1f:0:c4:0:5a" 65519,RelayAccessAddress "0:65:0:1d0:0:a2:0:d0" 65526,RelayAccessDomain "test2" 65527,RelayAccessAddress "0.0.0.244" 65534,RelayAccessAddress "0:195:0:185:0:6d:0:95" 65526,RelayAccessAddress "0:fa:0:f7:0:109:0:12b" 65526,RelayAccessAddress "0:e4:0:1ce:0:62:0:152" 65527],UseBootstrapPeers [RelayAccessAddress "0.0.3.24" 1,RelayAccessAddress "0.0.1.235" 65534,RelayAccessAddress "0.0.1.177" 1,RelayAccessDomain "test4" 6,RelayAccessDomain "test1" 10,RelayAccessDomain "test1" 2,RelayAccessAddress "0.0.0.201" 65535,RelayAccessAddress "0:1a3:0:3b:0:1e2:0:d7" 10],UseBootstrapPeers [RelayAccessDomain "test1" 65521,RelayAccessDomain "test2" 65522,RelayAccessAddress "0.0.0.118" 8,RelayAccessAddress "0.0.1.91" 3,RelayAccessAddress "0.0.1.112" 65526,RelayAccessDomain "test4" 11,RelayAccessDomain "test5" 65524,RelayAccessAddress "0.0.1.144" 7,RelayAccessDomain "test5" 65527,RelayAccessAddress "0.0.1.73" 65532,RelayAccessDomain "test3" 0],DontUseBootstrapPeers,UseBootstrapPeers [RelayAccessDomain "test3" 3,RelayAccessDomain "test3" 7,RelayAccessDomain "test2" 65521,RelayAccessAddress "0:1d0:0:131:0:71:0:7" 6,RelayAccessAddress "0.0.1.34" 8,RelayAccessAddress "0:d5:0:19e:0:1cc:0:17a" 65529,RelayAccessAddress "0:96:0:12:0:b6:0:1a0" 65532,RelayAccessDomain "test4" 8,RelayAccessDomain "test5" 8,RelayAccessDomain "test4" 65522,RelayAccessAddress "0.0.3.171" 14,RelayAccessDomain "test5" 65534],UseBootstrapPeers [RelayAccessDomain "test3" 65529,RelayAccessDomain "test5" 65520,RelayAccessAddress "0.0.1.155" 65525,RelayAccessDomain "test1" 15,RelayAccessAddress "0:1b5:0:175:0:53:0:174" 65522,RelayAccessAddress "0:10f:0:138:0:1a:0:b8" 65529,RelayAccessDomain "test4" 4,RelayAccessAddress "0.0.1.11" 65529,RelayAccessDomain "test1" 65517,RelayAccessDomain "test1" 65518,RelayAccessAddress "0:1d1:0:188:0:146:0:c7" 65519,RelayAccessAddress "0:e6:0:1ee:0:1b8:0:e7" 65527,RelayAccessDomain "test5" 11],UseBootstrapPeers [RelayAccessDomain "test4" 1],UseBootstrapPeers [RelayAccessDomain "test4" 65532,RelayAccessDomain "test5" 4,RelayAccessAddress "0:325:0:13d:0:192:0:1f6" 0,RelayAccessAddress "0.0.1.228" 5,RelayAccessDomain "test2" 65525,RelayAccessAddress "0.0.1.95" 65528,RelayAccessDomain "test3" 65525,RelayAccessAddress "0:271:0:191:0:1fb:0:1c0" 12,RelayAccessAddress "0:15e:0:4:0:124:0:1b3" 17,RelayAccessDomain "test5" 9,RelayAccessAddress "0.0.0.178" 9,RelayAccessDomain "test3" 4],DontUseBootstrapPeers,UseBootstrapPeers [RelayAccessDomain "test2" 11,RelayAccessAddress "0.0.1.159" 65524,RelayAccessAddress "0:1a9:0:2:0:17f:0:177" 1],UseBootstrapPeers [RelayAccessAddress "0.0.2.117" 17,RelayAccessAddress "0:74:0:159:0:e3:0:100" 65518,RelayAccessAddress "0:93:0:1e9:0:4:0:f0" 65526,RelayAccessDomain "test1" 65518,RelayAccessDomain "test2" 19,RelayAccessDomain "test2" 65535,RelayAccessAddress "0.0.0.210" 65524,RelayAccessDomain "test3" 4,RelayAccessAddress "0.0.1.183" 13],UseBootstrapPeers [RelayAccessAddress "0:a7:0:158:0:1bd:0:1e7" 15,RelayAccessAddress "0:f6:0:1a0:0:1eb:0:1b6" 19,RelayAccessDomain "test5" 65522,RelayAccessAddress "0.0.1.149" 65534,RelayAccessAddress "0:2ac:0:9e:0:166:0:ff" 65524,RelayAccessDomain "test1" 7,RelayAccessAddress "0.0.0.104" 2,RelayAccessAddress "0:ce:0:af:0:1a2:0:ee" 65529],UseBootstrapPeers [RelayAccessAddress "0:89:0:c:0:18b:0:86" 4,RelayAccessDomain "test1" 6,RelayAccessDomain "test1" 65531,RelayAccessAddress "0.0.1.242" 65517,RelayAccessAddress "0.0.1.22" 16,RelayAccessAddress "0:15b:0:42:0:81:0:74" 65535,RelayAccessDomain "test3" 65519,RelayAccessAddress "0.0.1.68" 18,RelayAccessDomain "test3" 65519,RelayAccessAddress "0:170:0:1a9:0:fd:0:cc" 19,RelayAccessDomain "test3" 13,RelayAccessAddress "0:2f1:0:19:0:c0:0:162" 65521,RelayAccessAddress "0.0.1.254" 65522,RelayAccessDomain "test5" 10,RelayAccessDomain "test5" 6,RelayAccessAddress "0.0.0.159" 15,RelayAccessAddress "0:15b:0:19c:0:4c:0:d0" 65520,RelayAccessDomain "test5" 65530],DontUseBootstrapPeers,UseBootstrapPeers [RelayAccessAddress "0.0.3.130" 19,RelayAccessAddress "0.0.0.162" 65529,RelayAccessAddress "0.0.1.27" 65521,RelayAccessDomain "test3" 65531,RelayAccessAddress "0.0.1.77" 65519,RelayAccessAddress "0:105:0:f6:0:a5:0:156" 65525,RelayAccessAddress "0:116:0:155:0:1dc:0:1d4" 1]])) (TestAddress (IPAddr (read "0.0.1.5") 65528)) PeerSharingEnabled [(HotValency {getHotValency = 1},WarmValency {getWarmValency = 1},Map.fromList [(RelayAccessDomain "test1" 65532,(DoNotAdvertisePeer,IsNotTrustable))])] (Script (LedgerPools {getLedgerPools = [(PoolStake {unPoolStake = 2866 % 191767},RelayAccessAddress "1.1.1.1" 1003 :| [RelayAccessAddress "1.1.1.1" 1091]),(PoolStake {unPoolStake = 217357 % 7670680},RelayAccessAddress "1.1.1.1" 1013 :| [RelayAccessAddress "1.1.1.1" 1053,RelayAccessDomain "relay.iohk.example" 1076,RelayAccessDomain "relay.iohk.example" 1068,RelayAccessDomain "relay.iohk.example" 1093]),(PoolStake {unPoolStake = 26987 % 767068},RelayAccessAddress "1.1.1.1" 1015 :| [RelayAccessAddress "1.1.1.1" 1071,RelayAccessDomain "relay.iohk.example" 1081,RelayAccessDomain "relay.iohk.example" 1054,RelayAccessDomain "relay.iohk.example" 1057,RelayAccessAddress "1.1.1.1" 1069]),(PoolStake {unPoolStake = 180357 % 1534136},RelayAccessAddress "1.1.1.1" 1053 :| [RelayAccessDomain "relay.iohk.example" 1094,RelayAccessAddress "1.1.1.1" 1027,RelayAccessAddress "1.1.1.1" 1081,RelayAccessDomain "relay.iohk.example" 1036,RelayAccessAddress "1.1.1.1" 1001,RelayAccessAddress "1.1.1.1" 1046,RelayAccessDomain "relay.iohk.example" 1057,RelayAccessAddress "1.1.1.1" 1007,RelayAccessAddress "1.1.1.1" 1020,RelayAccessDomain "relay.iohk.example" 1096,RelayAccessAddress "1.1.1.1" 1054,RelayAccessDomain "relay.iohk.example" 1074,RelayAccessAddress "1.1.1.1" 1063,RelayAccessDomain "relay.iohk.example" 1007,RelayAccessAddress "1.1.1.1" 1011,RelayAccessDomain "relay.iohk.example" 1035]),(PoolStake {unPoolStake = 818887 % 7670680},RelayAccessAddress "1.1.1.1" 1083 :| [RelayAccessAddress "1.1.1.1" 1080,RelayAccessAddress "1.1.1.1" 1054,RelayAccessDomain "relay.iohk.example" 1054,RelayAccessAddress "1.1.1.1" 1012,RelayAccessAddress "1.1.1.1" 1017,RelayAccessDomain "relay.iohk.example" 1042,RelayAccessAddress "1.1.1.1" 1069,RelayAccessAddress "1.1.1.1" 1084,RelayAccessAddress "1.1.1.1" 1062,RelayAccessDomain "relay.iohk.example" 1004,RelayAccessDomain "relay.iohk.example" 1009,RelayAccessDomain "relay.iohk.example" 1005]),(PoolStake {unPoolStake = 409037 % 7670680},RelayAccessDomain "relay.iohk.example" 1089 :| [RelayAccessDomain "relay.iohk.example" 1033,RelayAccessDomain "relay.iohk.example" 1029,RelayAccessDomain "relay.iohk.example" 1074,RelayAccessDomain "relay.iohk.example" 1069,RelayAccessDomain "relay.iohk.example" 1044,RelayAccessAddress "1.1.1.1" 1068,RelayAccessDomain "relay.iohk.example" 1050,RelayAccessDomain "relay.iohk.example" 1045,RelayAccessDomain "relay.iohk.example" 1040,RelayAccessAddress "1.1.1.1" 1040,RelayAccessDomain "relay.iohk.example" 1047,RelayAccessDomain "relay.iohk.example" 1100]),(PoolStake {unPoolStake = 196201 % 1917670},RelayAccessDomain "relay.iohk.example" 1072 :| [RelayAccessDomain "relay.iohk.example" 1028,RelayAccessAddress "1.1.1.1" 1095,RelayAccessDomain "relay.iohk.example" 1016,RelayAccessAddress "1.1.1.1" 1044,RelayAccessAddress "1.1.1.1" 1035,RelayAccessDomain "relay.iohk.example" 1024,RelayAccessAddress "1.1.1.1" 1046]),(PoolStake {unPoolStake = 134117 % 7670680},RelayAccessDomain "relay.iohk.example" 1072 :| [RelayAccessDomain "relay.iohk.example" 1085,RelayAccessDomain "relay.iohk.example" 1074,RelayAccessAddress "1.1.1.1" 1001,RelayAccessAddress "1.1.1.1" 1078,RelayAccessDomain "relay.iohk.example" 1025,RelayAccessAddress "1.1.1.1" 1084,RelayAccessDomain "relay.iohk.example" 1047,RelayAccessDomain "relay.iohk.example" 1100,RelayAccessDomain "relay.iohk.example" 1012,RelayAccessDomain "relay.iohk.example" 1076,RelayAccessAddress "1.1.1.1" 1037,RelayAccessDomain "relay.iohk.example" 1005,RelayAccessDomain "relay.iohk.example" 1054]),(PoolStake {unPoolStake = 919937 % 7670680},RelayAccessDomain "relay.iohk.example" 1003 :| [RelayAccessDomain "relay.iohk.example" 1011,RelayAccessAddress "1.1.1.1" 1075,RelayAccessDomain "relay.iohk.example" 1006,RelayAccessDomain "relay.iohk.example" 1067,RelayAccessDomain "relay.iohk.example" 1040,RelayAccessDomain "relay.iohk.example" 1039,RelayAccessDomain "relay.iohk.example" 1059,RelayAccessDomain "relay.iohk.example" 1054,RelayAccessDomain "relay.iohk.example" 1091,RelayAccessDomain "relay.iohk.example" 1016,RelayAccessDomain "relay.iohk.example" 1005]),(PoolStake {unPoolStake = 424 % 50465},RelayAccessAddress "1.1.1.1" 1058 :| [RelayAccessDomain "relay.iohk.example" 1081,RelayAccessDomain "relay.iohk.example" 1012,RelayAccessDomain "relay.iohk.example" 1031,RelayAccessAddress "1.1.1.1" 1083]),(PoolStake {unPoolStake = 69222 % 958835},RelayAccessAddress "1.1.1.1" 1047 :| [RelayAccessDomain "relay.iohk.example" 1012,RelayAccessDomain "relay.iohk.example" 1097,RelayAccessAddress "1.1.1.1" 1008,RelayAccessAddress "1.1.1.1" 1036,RelayAccessDomain "relay.iohk.example" 1098,RelayAccessAddress "1.1.1.1" 1003,RelayAccessAddress "1.1.1.1" 1046,RelayAccessAddress "1.1.1.1" 1092]),(PoolStake {unPoolStake = 311611 % 3835340},RelayAccessAddress "1.1.1.1" 1084 :| [RelayAccessAddress "1.1.1.1" 1038,RelayAccessAddress "1.1.1.1" 1000,RelayAccessAddress "1.1.1.1" 1053,RelayAccessDomain "relay.iohk.example" 1062,RelayAccessDomain "relay.iohk.example" 1098,RelayAccessAddress "1.1.1.1" 1059,RelayAccessAddress "1.1.1.1" 1090,RelayAccessAddress "1.1.1.1" 1100]),(PoolStake {unPoolStake = 609647 % 7670680},RelayAccessDomain "relay.iohk.example" 1051 :| [RelayAccessDomain "relay.iohk.example" 1056,RelayAccessAddress "1.1.1.1" 1063,RelayAccessDomain "relay.iohk.example" 1026,RelayAccessDomain "relay.iohk.example" 1093,RelayAccessDomain "relay.iohk.example" 1005,RelayAccessDomain "relay.iohk.example" 1015,RelayAccessAddress "1.1.1.1" 1100,RelayAccessDomain "relay.iohk.example" 1023,RelayAccessDomain "relay.iohk.example" 1075,RelayAccessAddress "1.1.1.1" 1059,RelayAccessDomain "relay.iohk.example" 1030,RelayAccessDomain "relay.iohk.example" 1034,RelayAccessDomain "relay.iohk.example" 1083]),(PoolStake {unPoolStake = 14473 % 201860},RelayAccessDomain "relay.iohk.example" 1060 :| [RelayAccessDomain "relay.iohk.example" 1001,RelayAccessDomain "relay.iohk.example" 1008,RelayAccessAddress "1.1.1.1" 1021,RelayAccessAddress "1.1.1.1" 1003,RelayAccessDomain "relay.iohk.example" 1039,RelayAccessDomain "relay.iohk.example" 1048]),(PoolStake {unPoolStake = 568047 % 7670680},RelayAccessDomain "relay.iohk.example" 1061 :| []),(PoolStake {unPoolStake = 32783 % 1917670},RelayAccessDomain "relay.iohk.example" 1013 :| [RelayAccessDomain "relay.iohk.example" 1033,RelayAccessAddress "1.1.1.1" 1041,RelayAccessAddress "1.1.1.1" 1075])]} :| [LedgerPools {getLedgerPools = [(PoolStake {unPoolStake = 1 % 1},RelayAccessAddress "1.1.1.1" 1047 :| [])]},LedgerPools {getLedgerPools = [(PoolStake {unPoolStake = 1 % 1},RelayAccessDomain "relay.iohk.example" 1012 :| [RelayAccessAddress "0.0.1.5" 65528])]},LedgerPools {getLedgerPools = [(PoolStake {unPoolStake = 1 % 1},RelayAccessDomain "relay.iohk.example" 1016 :| [])]},LedgerPools {getLedgerPools = [(PoolStake {unPoolStake = 1 % 1},RelayAccessDomain "relay.iohk.example" 1085 :| [RelayAccessAddress "0.0.1.5" 65528])]},LedgerPools {getLedgerPools = [(PoolStake {unPoolStake = 1 % 1},RelayAccessAddress "1.1.1.1" 1057 :| [RelayAccessAddress "0.0.1.5" 65528])]},LedgerPools {getLedgerPools = [(PoolStake {unPoolStake = 1 % 1},RelayAccessAddress "1.1.1.1" 1006 :| [])]},LedgerPools {getLedgerPools = [(PoolStake {unPoolStake = 1 % 1},RelayAccessAddress "1.1.1.1" 1047 :| [])]},LedgerPools {getLedgerPools = [(PoolStake {unPoolStake = 1 % 1},RelayAccessAddress "1.1.1.1" 1039 :| [RelayAccessAddress "0.0.1.5" 65528])]},LedgerPools {getLedgerPools = [(PoolStake {unPoolStake = 1 % 1},RelayAccessDomain "relay.iohk.example" 1093 :| [RelayAccessAddress "0.0.1.5" 65528])]}])) (PeerSelectionTargets {targetNumberOfRootPeers = 1, targetNumberOfKnownPeers = 2, targetNumberOfEstablishedPeers = 0, targetNumberOfActivePeers = 0, targetNumberOfKnownBigLedgerPeers = 3, targetNumberOfEstablishedBigLedgerPeers = 3, targetNumberOfActiveBigLedgerPeers = 3}) (Script (DNSTimeout {getDNSTimeout = 0.219} :| [DNSTimeout {getDNSTimeout = 0.37},DNSTimeout {getDNSTimeout = 0.125},DNSTimeout {getDNSTimeout = 0.265},DNSTimeout {getDNSTimeout = 0.408},DNSTimeout {getDNSTimeout = 0.164},DNSTimeout {getDNSTimeout = 0.355},DNSTimeout {getDNSTimeout = 0.225},DNSTimeout {getDNSTimeout = 0.237},DNSTimeout {getDNSTimeout = 0.361},DNSTimeout {getDNSTimeout = 0.371}])) (Script (DNSLookupDelay {getDNSLookupDelay = 0.087} :| [DNSLookupDelay {getDNSLookupDelay = 0.047},DNSLookupDelay {getDNSLookupDelay = 0.023}])) (Nothing) False (Script (FetchModeBulkSync :| [FetchModeBulkSync,FetchModeDeadline,FetchModeDeadline,FetchModeBulkSync,FetchModeDeadline,FetchModeDeadline,FetchModeDeadline,FetchModeDeadline,FetchModeBulkSync]))
             ,[JoinNetwork 1.895833333333,Reconfigure 0.885714285714 [(HotValency {getHotValency = 1},WarmValency {getWarmValency = 1},Map.fromList [(RelayAccessDomain "test1" 65532,(DoNotAdvertisePeer,IsNotTrustable))])],Reconfigure 10.75 [(HotValency {getHotValency = 1},WarmValency {getWarmValency = 1},Map.fromList [])],Kill 3.153846153846,JoinNetwork 1.651351351351,Reconfigure 8.894736842105 [(HotValency {getHotValency = 1},WarmValency {getWarmValency = 1},Map.fromList [(RelayAccessDomain "test1" 65532,(DoNotAdvertisePeer,IsNotTrustable))])],Kill 13.754385964912,JoinNetwork 2.064516129032,Kill 4.375,JoinNetwork 3.75])
            ,(NodeArgs (5) InitiatorAndResponderDiffusionMode (Just 269) (Map.fromList []) (Script (UseBootstrapPeers [RelayAccessAddress "0.0.2.96" 65529,RelayAccessDomain "test1" 13,RelayAccessDomain "test1" 5,RelayAccessDomain "test2" 65522,RelayAccessDomain "test5" 65535,RelayAccessAddress "0:f1:0:1b2:0:41:0:f4" 0,RelayAccessAddress "0:19a:0:1c2:0:15c:0:17e" 65524] :| [UseBootstrapPeers [RelayAccessAddress "0.0.1.25" 12,RelayAccessDomain "test4" 17,RelayAccessDomain "test4" 4],DontUseBootstrapPeers,UseBootstrapPeers [],UseBootstrapPeers [RelayAccessDomain "test5" 9,RelayAccessAddress "0.0.1.120" 65519],UseBootstrapPeers [RelayAccessDomain "test5" 0,RelayAccessAddress "0:1e3:0:2c:0:72:0:ef" 19,RelayAccessAddress "0.0.3.234" 65523,RelayAccessDomain "test2" 13,RelayAccessAddress "0:1f3:0:107:0:2f:0:1d8" 65527,RelayAccessAddress "0:ea:0:119:0:11b:0:15d" 18,RelayAccessDomain "test1" 19,RelayAccessDomain "test3" 65529,RelayAccessDomain "test5" 65519,RelayAccessAddress "0.0.0.163" 8],UseBootstrapPeers [RelayAccessDomain "test5" 14,RelayAccessAddress "0.0.0.237" 65530,RelayAccessDomain "test1" 65531,RelayAccessAddress "0:b0:0:89:0:1c6:0:84" 3,RelayAccessDomain "test4" 65535,RelayAccessDomain "test3" 65534,RelayAccessAddress "0.0.3.213" 10,RelayAccessAddress "0:254:0:93:0:1f3:0:5b" 65523,RelayAccessAddress "0.0.1.118" 11,RelayAccessAddress "0.0.3.78" 5],DontUseBootstrapPeers,UseBootstrapPeers [RelayAccessAddress "0.0.0.134" 65531,RelayAccessAddress "0.0.0.202" 19,RelayAccessAddress "0.0.0.148" 65525,RelayAccessAddress "0.0.1.166" 65528,RelayAccessAddress "0:83:0:18b:0:14:0:18b" 65534,RelayAccessDomain "test2" 65526,RelayAccessDomain "test5" 10],DontUseBootstrapPeers,UseBootstrapPeers [RelayAccessAddress "0.0.1.175" 65528,RelayAccessDomain "test3" 2,RelayAccessAddress "0:279:0:4d:0:130:0:8e" 13,RelayAccessAddress "0.0.0.126" 65520,RelayAccessDomain "test3" 2],UseBootstrapPeers [RelayAccessDomain "test5" 65533,RelayAccessDomain "test5" 17,RelayAccessAddress "0.0.2.189" 65530,RelayAccessDomain "test1" 65527,RelayAccessAddress "0:d0:0:af:0:162:0:186" 19,RelayAccessAddress "0.0.1.180" 65533,RelayAccessDomain "test5" 15,RelayAccessAddress "0.0.3.148" 19,RelayAccessAddress "0.0.0.124" 65524,RelayAccessDomain "test5" 11,RelayAccessDomain "test3" 65517],UseBootstrapPeers [RelayAccessAddress "0.0.1.82" 8,RelayAccessDomain "test1" 13,RelayAccessDomain "test4" 65530,RelayAccessDomain "test2" 65529,RelayAccessDomain "test5" 12],UseBootstrapPeers [RelayAccessAddress "0:195:0:14d:0:143:0:144" 5,RelayAccessDomain "test2" 65518,RelayAccessAddress "0:1dd:0:33:0:83:0:199" 65533]])) (TestAddress (IPAddr (read "0.0.1.116") 65532)) PeerSharingEnabled [(HotValency {getHotValency = 1},WarmValency {getWarmValency = 1},Map.fromList [(RelayAccessAddress "0.0.1.5" 65528,(DoNotAdvertisePeer,IsTrustable))])] (Script (LedgerPools {getLedgerPools = [(PoolStake {unPoolStake = 164453 % 1606724},RelayAccessAddress "1.1.1.1" 1083 :| [RelayAccessAddress "1.1.1.1" 1089,RelayAccessDomain "relay.iohk.example" 1091,RelayAccessDomain "relay.iohk.example" 1048,RelayAccessDomain "relay.iohk.example" 1052,RelayAccessAddress "1.1.1.1" 1016,RelayAccessDomain "relay.iohk.example" 1094,RelayAccessDomain "relay.iohk.example" 1088,RelayAccessAddress "1.1.1.1" 1028,RelayAccessAddress "1.1.1.1" 1048,RelayAccessAddress "1.1.1.1" 1005]),(PoolStake {unPoolStake = 223423 % 8033620},RelayAccessDomain "relay.iohk.example" 1052 :| [RelayAccessDomain "relay.iohk.example" 1013,RelayAccessAddress "1.1.1.1" 1014,RelayAccessAddress "1.1.1.1" 1044,RelayAccessDomain "relay.iohk.example" 1042,RelayAccessDomain "relay.iohk.example" 1002,RelayAccessDomain "relay.iohk.example" 1068]),(PoolStake {unPoolStake = 144861 % 2008405},RelayAccessAddress "1.1.1.1" 1014 :| [RelayAccessAddress "1.1.1.1" 1067,RelayAccessDomain "relay.iohk.example" 1090,RelayAccessDomain "relay.iohk.example" 1074,RelayAccessDomain "relay.iohk.example" 1093,RelayAccessAddress "1.1.1.1" 1081,RelayAccessDomain "relay.iohk.example" 1003,RelayAccessAddress "1.1.1.1" 1024,RelayAccessAddress "1.1.1.1" 1003,RelayAccessDomain "relay.iohk.example" 1041,RelayAccessDomain "relay.iohk.example" 1080,RelayAccessDomain "relay.iohk.example" 1033,RelayAccessAddress "1.1.1.1" 1058,RelayAccessAddress "1.1.1.1" 1054,RelayAccessDomain "relay.iohk.example" 1034,RelayAccessAddress "1.1.1.1" 1057,RelayAccessDomain "relay.iohk.example" 1021]),(PoolStake {unPoolStake = 299681 % 8033620},RelayAccessAddress "1.1.1.1" 1065 :| [RelayAccessAddress "1.1.1.1" 1097]),(PoolStake {unPoolStake = 111829 % 1606724},RelayAccessDomain "relay.iohk.example" 1088 :| []),(PoolStake {unPoolStake = 11358 % 2008405},RelayAccessDomain "relay.iohk.example" 1032 :| [RelayAccessDomain "relay.iohk.example" 1075,RelayAccessAddress "1.1.1.1" 1100,RelayAccessAddress "1.1.1.1" 1003,RelayAccessAddress "1.1.1.1" 1055,RelayAccessDomain "relay.iohk.example" 1020,RelayAccessDomain "relay.iohk.example" 1069,RelayAccessAddress "1.1.1.1" 1022,RelayAccessAddress "1.1.1.1" 1045,RelayAccessDomain "relay.iohk.example" 1096,RelayAccessDomain "relay.iohk.example" 1017,RelayAccessAddress "1.1.1.1" 1076,RelayAccessDomain "relay.iohk.example" 1056,RelayAccessDomain "relay.iohk.example" 1062,RelayAccessAddress "1.1.1.1" 1046,RelayAccessAddress "1.1.1.1" 1006,RelayAccessAddress "1.1.1.1" 1001]),(PoolStake {unPoolStake = 100167 % 8033620},RelayAccessDomain "relay.iohk.example" 1014 :| [RelayAccessDomain "relay.iohk.example" 1019]),(PoolStake {unPoolStake = 586939 % 8033620},RelayAccessAddress "1.1.1.1" 1096 :| [RelayAccessAddress "1.1.1.1" 1035,RelayAccessAddress "1.1.1.1" 1073]),(PoolStake {unPoolStake = 2187 % 401681},RelayAccessDomain "relay.iohk.example" 1033 :| []),(PoolStake {unPoolStake = 89513 % 803362},RelayAccessAddress "1.1.1.1" 1078 :| [RelayAccessDomain "relay.iohk.example" 1031,RelayAccessAddress "1.1.1.1" 1045,RelayAccessDomain "relay.iohk.example" 1068,RelayAccessAddress "1.1.1.1" 1090,RelayAccessDomain "relay.iohk.example" 1003,RelayAccessAddress "1.1.1.1" 1051,RelayAccessAddress "1.1.1.1" 1077,RelayAccessDomain "relay.iohk.example" 1094,RelayAccessAddress "1.1.1.1" 1025,RelayAccessAddress "1.1.1.1" 1080,RelayAccessAddress "1.1.1.1" 1050,RelayAccessDomain "relay.iohk.example" 1050]),(PoolStake {unPoolStake = 524773 % 8033620},RelayAccessDomain "relay.iohk.example" 1024 :| [RelayAccessDomain "relay.iohk.example" 1098,RelayAccessDomain "relay.iohk.example" 1016,RelayAccessAddress "1.1.1.1" 1044,RelayAccessDomain "relay.iohk.example" 1017,RelayAccessDomain "relay.iohk.example" 1062,RelayAccessAddress "1.1.1.1" 1094,RelayAccessAddress "1.1.1.1" 1016,RelayAccessAddress "1.1.1.1" 1095,RelayAccessDomain "relay.iohk.example" 1033,RelayAccessAddress "1.1.1.1" 1067,RelayAccessAddress "1.1.1.1" 1052,RelayAccessDomain "relay.iohk.example" 1004,RelayAccessAddress "1.1.1.1" 1065]),(PoolStake {unPoolStake = 982441 % 8033620},RelayAccessAddress "1.1.1.1" 1050 :| [RelayAccessAddress "1.1.1.1" 1017,RelayAccessDomain "relay.iohk.example" 1035,RelayAccessAddress "1.1.1.1" 1077,RelayAccessAddress "1.1.1.1" 1092,RelayAccessAddress "1.1.1.1" 1034,RelayAccessDomain "relay.iohk.example" 1067,RelayAccessAddress "1.1.1.1" 1053,RelayAccessAddress "1.1.1.1" 1029,RelayAccessAddress "1.1.1.1" 1038,RelayAccessAddress "1.1.1.1" 1016,RelayAccessAddress "1.1.1.1" 1042,RelayAccessDomain "relay.iohk.example" 1000,RelayAccessAddress "1.1.1.1" 1075,RelayAccessAddress "1.1.1.1" 1033,RelayAccessDomain "relay.iohk.example" 1062,RelayAccessDomain "relay.iohk.example" 1032,RelayAccessAddress "1.1.1.1" 1030]),(PoolStake {unPoolStake = 147643 % 1606724},RelayAccessAddress "1.1.1.1" 1017 :| [RelayAccessAddress "1.1.1.1" 1084,RelayAccessAddress "1.1.1.1" 1027,RelayAccessAddress "1.1.1.1" 1026,RelayAccessAddress "1.1.1.1" 1045,RelayAccessDomain "relay.iohk.example" 1012,RelayAccessDomain "relay.iohk.example" 1004,RelayAccessAddress "1.1.1.1" 1029,RelayAccessDomain "relay.iohk.example" 1027,RelayAccessDomain "relay.iohk.example" 1050,RelayAccessDomain "relay.iohk.example" 1032,RelayAccessAddress "1.1.1.1" 1057,RelayAccessAddress "1.1.1.1" 1080,RelayAccessAddress "1.1.1.1" 1039,RelayAccessDomain "relay.iohk.example" 1039]),(PoolStake {unPoolStake = 483223 % 4016810},RelayAccessAddress "1.1.1.1" 1085 :| [RelayAccessAddress "1.1.1.1" 1098,RelayAccessAddress "1.1.1.1" 1089,RelayAccessAddress "1.1.1.1" 1079,RelayAccessDomain "relay.iohk.example" 1022,RelayAccessDomain "relay.iohk.example" 1006,RelayAccessDomain "relay.iohk.example" 1029]),(PoolStake {unPoolStake = 95197 % 1147660},RelayAccessAddress "1.1.1.1" 1065 :| [RelayAccessAddress "1.1.1.1" 1083,RelayAccessAddress "1.1.1.1" 1042,RelayAccessAddress "1.1.1.1" 1085,RelayAccessAddress "1.1.1.1" 1078,RelayAccessDomain "relay.iohk.example" 1050,RelayAccessDomain "relay.iohk.example" 1089,RelayAccessDomain "relay.iohk.example" 1098,RelayAccessAddress "1.1.1.1" 1028,RelayAccessAddress "1.1.1.1" 1004,RelayAccessDomain "relay.iohk.example" 1092,RelayAccessAddress "1.1.1.1" 1084,RelayAccessAddress "1.1.1.1" 1053])]} :| [LedgerPools {getLedgerPools = [(PoolStake {unPoolStake = 1 % 1},RelayAccessAddress "1.1.1.1" 1057 :| [RelayAccessAddress "0.0.1.5" 65528])]},LedgerPools {getLedgerPools = [(PoolStake {unPoolStake = 1 % 1},RelayAccessDomain "relay.iohk.example" 1007 :| [])]},LedgerPools {getLedgerPools = [(PoolStake {unPoolStake = 1 % 1},RelayAccessDomain "relay.iohk.example" 1014 :| [RelayAccessAddress "0.0.1.5" 65528])]},LedgerPools {getLedgerPools = [(PoolStake {unPoolStake = 1 % 1},RelayAccessAddress "1.1.1.1" 1094 :| [RelayAccessAddress "0.0.1.5" 65528])]},LedgerPools {getLedgerPools = [(PoolStake {unPoolStake = 1 % 1},RelayAccessAddress "1.1.1.1" 1098 :| [RelayAccessAddress "0.0.1.5" 65528])]},LedgerPools {getLedgerPools = [(PoolStake {unPoolStake = 1 % 1},RelayAccessAddress "1.1.1.1" 1083 :| [RelayAccessAddress "0.0.1.5" 65528])]},LedgerPools {getLedgerPools = [(PoolStake {unPoolStake = 1 % 1},RelayAccessAddress "1.1.1.1" 1030 :| [])]},LedgerPools {getLedgerPools = [(PoolStake {unPoolStake = 1 % 1},RelayAccessAddress "1.1.1.1" 1083 :| [RelayAccessAddress "0.0.1.5" 65528])]}])) (PeerSelectionTargets {targetNumberOfRootPeers = 0, targetNumberOfKnownPeers = 1, targetNumberOfEstablishedPeers = 1, targetNumberOfActivePeers = 1, targetNumberOfKnownBigLedgerPeers = 3, targetNumberOfEstablishedBigLedgerPeers = 2, targetNumberOfActiveBigLedgerPeers = 2}) (Script (DNSTimeout {getDNSTimeout = 0.386} :| [DNSTimeout {getDNSTimeout = 0.167},DNSTimeout {getDNSTimeout = 0.377}])) (Script (DNSLookupDelay {getDNSLookupDelay = 0.131} :| [DNSLookupDelay {getDNSLookupDelay = 0.081},DNSLookupDelay {getDNSLookupDelay = 0.112}])) (Nothing) False (Script (FetchModeBulkSync :| [FetchModeDeadline,FetchModeDeadline,FetchModeBulkSync]))
             ,[JoinNetwork 6.689655172413,Reconfigure 9.90909090909 [],Reconfigure 3 [(HotValency {getHotValency = 1},WarmValency {getWarmValency = 1},Map.fromList [(RelayAccessAddress "0.0.1.5" 65528,(DoNotAdvertisePeer,IsTrustable))])],Reconfigure 10.4 [(HotValency {getHotValency = 1},WarmValency {getWarmValency = 1},Map.fromList [(RelayAccessAddress "0.0.1.5" 65528,(DoNotAdvertisePeer,IsTrustable))])],Kill 0.869565217391,JoinNetwork 16.464285714285,Reconfigure 10.26923076923 [(HotValency {getHotValency = 1},WarmValency {getWarmValency = 1},Map.fromList [(RelayAccessAddress "0.0.1.5" 65528,(DoNotAdvertisePeer,IsTrustable))])]]
             )]

unit_1 :: Property
unit_1 =
  testWithIOSimPOR prop_diffusion_nofail 10000 bi ds
  where
    bi =
      AbsBearerInfo
        { abiConnectionDelay = SmallDelay
        , abiInboundAttenuation = NoAttenuation FastSpeed
        , abiOutboundAttenuation = NoAttenuation FastSpeed
        , abiInboundWriteFailure = Nothing
        , abiOutboundWriteFailure = Nothing
        , abiAcceptFailure = Nothing
        , abiSDUSize = LargeSDU
        }
    ds =
      DiffusionScript
        (SimArgs 1 6)
        (Script ((Map.fromList [ ("test2",[("0.0.118.44",300)])],ShortDelay)
                               :| [ (Map.fromList [("test2",[("0.0.74.147",300)])],LongDelay)
                                  , (Map.fromList [("test2",[("0.0.7.10",300)])],ShortDelay)
                                  , (Map.fromList [("test2",[("0.0.101.37",300)])],LongDelay)
                                  , (Map.fromList [("test2",[("0.0.37.19",300)])],NoDelay)
                                  , (Map.fromList [("test2",[("0.0.27.221",300)])],LongDelay)
                                  , (Map.fromList [("test2",[("0.0.113.89",300)])],ShortDelay)]))
                [ ( NodeArgs (-27) InitiatorOnlyDiffusionMode (Just 135)
                             (Map.fromList [ (RelayAccessAddress "0.0.37.208" 65532,DoAdvertisePeer)
                                           , (RelayAccessAddress "0.0.104.38" 24,DoAdvertisePeer)])
                             (Script (DontUseBootstrapPeers
                                     :| [ UseBootstrapPeers [RelayAccessAddress "0.0.17.124" 65523,RelayAccessAddress "0.0.2.21" 65523,RelayAccessDomain "test3" 12,RelayAccessDomain "test4" 27,RelayAccessAddress "0.0.34.182" 2,RelayAccessAddress "0.0.12.51" 21,RelayAccessDomain "test4" 26,RelayAccessAddress "0.0.63.61" 65508,RelayAccessAddress "0.0.41.181" 2,RelayAccessAddress "0:18f9:0:225a:0:d10:0:1c52" 65508,RelayAccessDomain "test2" 65520,RelayAccessAddress "0.0.61.40" 9,RelayAccessAddress "0.0.8.247" 65533,RelayAccessAddress "0:3ce4:0:2673:0:22aa:0:3fa" 65520,RelayAccessAddress "0.0.19.143" 65532,RelayAccessDomain "test5" 65509,RelayAccessDomain "test2" 65514,RelayAccessAddress "0:8b4:0:1b70:0:b4c:0:2e7e" 65512,RelayAccessDomain "test4" 13,RelayAccessDomain "test5" 1,RelayAccessDomain "test5" 19,RelayAccessAddress "0.0.57.109" 65524,RelayAccessAddress "0.0.58.226" 22,RelayAccessAddress "0.0.42.189" 26]
                                        , UseBootstrapPeers [RelayAccessAddress "0.0.35.159" 24,RelayAccessAddress "0:2ece:0:b37:0:33ce:0:66d" 11,RelayAccessAddress "0.0.5.120" 65509,RelayAccessDomain "test4" 65516,RelayAccessDomain "test2" 8,RelayAccessAddress "0.0.23.97" 65522]
                                        , DontUseBootstrapPeers
                                        , UseBootstrapPeers [RelayAccessDomain "test5" 65523,RelayAccessDomain "test3" 65510,RelayAccessAddress "0.0.48.191" 65508,RelayAccessAddress "0.0.19.191" 20,RelayAccessAddress "0:25e3:0:3c06:0:3b2a:0:d69" 28,RelayAccessAddress "0.0.32.228" 22,RelayAccessDomain "test3" 13]
                                        , UseBootstrapPeers [RelayAccessDomain "test2" 65523,RelayAccessDomain "test4" 65528,RelayAccessDomain "test4" 23,RelayAccessAddress "0:e96:0:14:0:1d1e:0:a45" 65526,RelayAccessDomain "test1" 65518,RelayAccessAddress "0:359e:0:38fa:0:2bee:0:278d" 65528,RelayAccessDomain "test4" 65516,RelayAccessAddress "0:d4e:0:169:0:3da4:0:3f1b" 12,RelayAccessDomain "test4" 65519,RelayAccessAddress "0:9be:0:18fc:0:260b:0:30af" 8,RelayAccessAddress "0.0.20.174" 25,RelayAccessAddress "0:2c49:0:16ea:0:1c91:0:e4f" 6,RelayAccessAddress "0:2f05:0:2419:0:dc:0:31a9" 65531,RelayAccessAddress "0:707:0:295c:0:3f7e:0:35f" 4,RelayAccessDomain "test2" 20,RelayAccessDomain "test4" 10,RelayAccessAddress "0.0.42.19" 65518,RelayAccessAddress "0:38f4:0:349:0:25d1:0:14fd" 65514,RelayAccessDomain "test2" 65531,RelayAccessAddress "0:2752:0:271:0:e5f:0:3b73" 10,RelayAccessDomain "test4" 65530,RelayAccessDomain "test1" 65535,RelayAccessAddress "0:1dd6:0:181a:0:5eb:0:2dc9" 65529,RelayAccessDomain "test2" 65526,RelayAccessAddress "0.0.40.48" 22,RelayAccessAddress "0.0.6.38" 26]
                                        , UseBootstrapPeers [RelayAccessAddress "0.0.61.60" 65528,RelayAccessDomain "test2" 65513]]))
                             (TestAddress (IPAddr (read "0.0.113.89") 65529))
                             PeerSharingDisabled
                             [(HotValency {getHotValency = 1},WarmValency {getWarmValency = 1},Map.fromList [(RelayAccessAddress "0.0.37.208" 65532,(DoAdvertisePeer,IsTrustable)),(RelayAccessAddress "0.0.104.38" 24,(DoNotAdvertisePeer,IsNotTrustable))])]
                             (Script (LedgerPools {getLedgerPools = [(PoolStake {unPoolStake = 314651 % 3463848},RelayAccessDomain "relay.iohk.example" 1095 :| [RelayAccessAddress "1.1.1.1" 1001,RelayAccessAddress "1.1.1.1" 1062,RelayAccessAddress "1.1.1.1" 1007,RelayAccessAddress "1.1.1.1" 1072,RelayAccessAddress "1.1.1.1" 1057,RelayAccessAddress "1.1.1.1" 1026,RelayAccessAddress "1.1.1.1" 1091,RelayAccessAddress "1.1.1.1" 1065,RelayAccessDomain "relay.iohk.example" 1074,RelayAccessDomain "relay.iohk.example" 1046,RelayAccessAddress "1.1.1.1" 1050,RelayAccessAddress "1.1.1.1" 1066,RelayAccessAddress "1.1.1.1" 1019,RelayAccessDomain "relay.iohk.example" 1031,RelayAccessAddress "1.1.1.1" 1013,RelayAccessDomain "relay.iohk.example" 1035,RelayAccessAddress "1.1.1.1" 1041,RelayAccessAddress "1.1.1.1" 1081,RelayAccessDomain "relay.iohk.example" 1077]),(PoolStake {unPoolStake = 819113 % 10391544},RelayAccessDomain "relay.iohk.example" 1064 :| [RelayAccessDomain "relay.iohk.example" 1062,RelayAccessAddress "1.1.1.1" 1020,RelayAccessAddress "1.1.1.1" 1047,RelayAccessDomain "relay.iohk.example" 1084,RelayAccessAddress "1.1.1.1" 1030,RelayAccessAddress "1.1.1.1" 1082,RelayAccessDomain "relay.iohk.example" 1038,RelayAccessDomain "relay.iohk.example" 1021]),(PoolStake {unPoolStake = 100195 % 1154616},RelayAccessDomain "relay.iohk.example" 1083 :| [RelayAccessDomain "relay.iohk.example" 1067,RelayAccessAddress "1.1.1.1" 1090]),(PoolStake {unPoolStake = 262763 % 3463848},RelayAccessAddress "1.1.1.1" 1041 :| [RelayAccessDomain "relay.iohk.example" 1048,RelayAccessDomain "relay.iohk.example" 1022,RelayAccessAddress "1.1.1.1" 1007,RelayAccessDomain "relay.iohk.example" 1038,RelayAccessAddress "1.1.1.1" 1024,RelayAccessAddress "1.1.1.1" 1053,RelayAccessAddress "1.1.1.1" 1058,RelayAccessAddress "1.1.1.1" 1043,RelayAccessAddress "1.1.1.1" 1076,RelayAccessAddress "1.1.1.1" 1018,RelayAccessAddress "1.1.1.1" 1061,RelayAccessDomain "relay.iohk.example" 1031,RelayAccessAddress "1.1.1.1" 1015,RelayAccessAddress "1.1.1.1" 1063,RelayAccessAddress "1.1.1.1" 1028]),(PoolStake {unPoolStake = 123163 % 1731924},RelayAccessAddress "1.1.1.1" 1002 :| [RelayAccessAddress "1.1.1.1" 1051,RelayAccessDomain "relay.iohk.example" 1087,RelayAccessAddress "1.1.1.1" 1004,RelayAccessAddress "1.1.1.1" 1014,RelayAccessDomain "relay.iohk.example" 1006,RelayAccessDomain "relay.iohk.example" 1058,RelayAccessAddress "1.1.1.1" 1077,RelayAccessDomain "relay.iohk.example" 1095,RelayAccessDomain "relay.iohk.example" 1063,RelayAccessAddress "1.1.1.1" 1040,RelayAccessAddress "1.1.1.1" 1025,RelayAccessAddress "1.1.1.1" 1085,RelayAccessAddress "1.1.1.1" 1098,RelayAccessAddress "1.1.1.1" 1063,RelayAccessAddress "1.1.1.1" 1001]),(PoolStake {unPoolStake = 218951 % 10391544},RelayAccessAddress "1.1.1.1" 1035 :| [RelayAccessAddress "1.1.1.1" 1088,RelayAccessAddress "1.1.1.1" 1060,RelayAccessAddress "1.1.1.1" 1065,RelayAccessAddress "1.1.1.1" 1055,RelayAccessAddress "1.1.1.1" 1097,RelayAccessDomain "relay.iohk.example" 1044,RelayAccessDomain "relay.iohk.example" 1068,RelayAccessDomain "relay.iohk.example" 1046,RelayAccessAddress "1.1.1.1" 1098,RelayAccessAddress "1.1.1.1" 1066,RelayAccessAddress "1.1.1.1" 1014,RelayAccessAddress "1.1.1.1" 1033,RelayAccessAddress "1.1.1.1" 1069,RelayAccessAddress "1.1.1.1" 1041,RelayAccessAddress "1.1.1.1" 1019,RelayAccessDomain "relay.iohk.example" 1064,RelayAccessDomain "relay.iohk.example" 1060,RelayAccessAddress "1.1.1.1" 1087,RelayAccessAddress "1.1.1.1" 1043,RelayAccessAddress "1.1.1.1" 1095,RelayAccessDomain "relay.iohk.example" 1062,RelayAccessAddress "1.1.1.1" 1007,RelayAccessAddress "1.1.1.1" 1052,RelayAccessAddress "1.1.1.1" 1049]),(PoolStake {unPoolStake = 144757 % 1731924},RelayAccessAddress "1.1.1.1" 1046 :| [RelayAccessAddress "1.1.1.1" 1067,RelayAccessAddress "1.1.1.1" 1023,RelayAccessDomain "relay.iohk.example" 1092,RelayAccessAddress "1.1.1.1" 1058,RelayAccessAddress "1.1.1.1" 1064,RelayAccessDomain "relay.iohk.example" 1063,RelayAccessDomain "relay.iohk.example" 1042,RelayAccessDomain "relay.iohk.example" 1054,RelayAccessDomain "relay.iohk.example" 1012,RelayAccessAddress "1.1.1.1" 1091,RelayAccessDomain "relay.iohk.example" 1027,RelayAccessDomain "relay.iohk.example" 1053,RelayAccessAddress "1.1.1.1" 1049,RelayAccessAddress "1.1.1.1" 1012,RelayAccessAddress "1.1.1.1" 1042,RelayAccessDomain "relay.iohk.example" 1082,RelayAccessAddress "1.1.1.1" 1086]),(PoolStake {unPoolStake = 601247 % 10391544},RelayAccessAddress "1.1.1.1" 1057 :| [RelayAccessAddress "1.1.1.1" 1091,RelayAccessAddress "1.1.1.1" 1059,RelayAccessDomain "relay.iohk.example" 1069,RelayAccessAddress "1.1.1.1" 1063,RelayAccessAddress "1.1.1.1" 1095,RelayAccessDomain "relay.iohk.example" 1096,RelayAccessDomain "relay.iohk.example" 1073,RelayAccessAddress "1.1.1.1" 1015,RelayAccessAddress "1.1.1.1" 1042,RelayAccessAddress "1.1.1.1" 1052,RelayAccessAddress "1.1.1.1" 1017,RelayAccessAddress "1.1.1.1" 1075,RelayAccessDomain "relay.iohk.example" 1000,RelayAccessDomain "relay.iohk.example" 1071,RelayAccessDomain "relay.iohk.example" 1049,RelayAccessDomain "relay.iohk.example" 1095,RelayAccessAddress "1.1.1.1" 1033,RelayAccessDomain "relay.iohk.example" 1098,RelayAccessAddress "1.1.1.1" 1049,RelayAccessAddress "1.1.1.1" 1025,RelayAccessDomain "relay.iohk.example" 1055,RelayAccessAddress "1.1.1.1" 1069,RelayAccessAddress "1.1.1.1" 1062,RelayAccessAddress "1.1.1.1" 1051]),(PoolStake {unPoolStake = 437731 % 10391544},RelayAccessAddress "1.1.1.1" 1055 :| [RelayAccessDomain "relay.iohk.example" 1037,RelayAccessAddress "1.1.1.1" 1028]),(PoolStake {unPoolStake = 28996 % 432981},RelayAccessAddress "1.1.1.1" 1011 :| [RelayAccessDomain "relay.iohk.example" 1062,RelayAccessAddress "1.1.1.1" 1031,RelayAccessAddress "1.1.1.1" 1052,RelayAccessAddress "1.1.1.1" 1060,RelayAccessDomain "relay.iohk.example" 1082,RelayAccessAddress "1.1.1.1" 1100,RelayAccessAddress "1.1.1.1" 1009,RelayAccessAddress "1.1.1.1" 1042,RelayAccessAddress "1.1.1.1" 1056,RelayAccessDomain "relay.iohk.example" 1060,RelayAccessDomain "relay.iohk.example" 1042,RelayAccessDomain "relay.iohk.example" 1009,RelayAccessDomain "relay.iohk.example" 1034,RelayAccessAddress "1.1.1.1" 1080,RelayAccessAddress "1.1.1.1" 1064,RelayAccessDomain "relay.iohk.example" 1030,RelayAccessAddress "1.1.1.1" 1097,RelayAccessAddress "1.1.1.1" 1023,RelayAccessAddress "1.1.1.1" 1086,RelayAccessDomain "relay.iohk.example" 1013,RelayAccessDomain "relay.iohk.example" 1031,RelayAccessDomain "relay.iohk.example" 1090]),(PoolStake {unPoolStake = 169705 % 5195772},RelayAccessAddress "1.1.1.1" 1006 :| [RelayAccessAddress "1.1.1.1" 1074,RelayAccessAddress "1.1.1.1" 1066,RelayAccessDomain "relay.iohk.example" 1062,RelayAccessDomain "relay.iohk.example" 1029,RelayAccessDomain "relay.iohk.example" 1070,RelayAccessAddress "1.1.1.1" 1017,RelayAccessAddress "1.1.1.1" 1018,RelayAccessDomain "relay.iohk.example" 1017,RelayAccessAddress "1.1.1.1" 1065,RelayAccessAddress "1.1.1.1" 1032,RelayAccessDomain "relay.iohk.example" 1076,RelayAccessAddress "1.1.1.1" 1099,RelayAccessAddress "1.1.1.1" 1054,RelayAccessAddress "1.1.1.1" 1097,RelayAccessAddress "1.1.1.1" 1085,RelayAccessDomain "relay.iohk.example" 1031,RelayAccessAddress "1.1.1.1" 1005,RelayAccessAddress "1.1.1.1" 1078,RelayAccessAddress "1.1.1.1" 1081,RelayAccessDomain "relay.iohk.example" 1079,RelayAccessDomain "relay.iohk.example" 1053,RelayAccessDomain "relay.iohk.example" 1034]),(PoolStake {unPoolStake = 52760 % 1298943},RelayAccessDomain "relay.iohk.example" 1020 :| [RelayAccessDomain "relay.iohk.example" 1010,RelayAccessDomain "relay.iohk.example" 1022,RelayAccessDomain "relay.iohk.example" 1017,RelayAccessAddress "1.1.1.1" 1006,RelayAccessDomain "relay.iohk.example" 1082,RelayAccessAddress "1.1.1.1" 1048,RelayAccessDomain "relay.iohk.example" 1016,RelayAccessDomain "relay.iohk.example" 1053,RelayAccessDomain "relay.iohk.example" 1097,RelayAccessDomain "relay.iohk.example" 1038,RelayAccessAddress "1.1.1.1" 1074,RelayAccessAddress "1.1.1.1" 1096,RelayAccessAddress "1.1.1.1" 1089,RelayAccessDomain "relay.iohk.example" 1001,RelayAccessAddress "1.1.1.1" 1083,RelayAccessAddress "1.1.1.1" 1020,RelayAccessDomain "relay.iohk.example" 1054,RelayAccessAddress "1.1.1.1" 1054,RelayAccessAddress "1.1.1.1" 1036]),(PoolStake {unPoolStake = 180439 % 2597886},RelayAccessDomain "relay.iohk.example" 1033 :| [RelayAccessDomain "relay.iohk.example" 1059,RelayAccessDomain "relay.iohk.example" 1080,RelayAccessDomain "relay.iohk.example" 1006,RelayAccessAddress "1.1.1.1" 1003,RelayAccessAddress "1.1.1.1" 1023]),(PoolStake {unPoolStake = 616115 % 10391544},RelayAccessAddress "1.1.1.1" 1078 :| [RelayAccessDomain "relay.iohk.example" 1080,RelayAccessAddress "1.1.1.1" 1062,RelayAccessAddress "1.1.1.1" 1013,RelayAccessDomain "relay.iohk.example" 1015,RelayAccessAddress "1.1.1.1" 1011]),(PoolStake {unPoolStake = 6023 % 577308},RelayAccessDomain "relay.iohk.example" 1043 :| [RelayAccessAddress "1.1.1.1" 1077,RelayAccessDomain "relay.iohk.example" 1033,RelayAccessDomain "relay.iohk.example" 1027,RelayAccessAddress "1.1.1.1" 1056,RelayAccessAddress "1.1.1.1" 1049,RelayAccessAddress "1.1.1.1" 1044,RelayAccessAddress "1.1.1.1" 1063,RelayAccessDomain "relay.iohk.example" 1025,RelayAccessDomain "relay.iohk.example" 1051,RelayAccessAddress "1.1.1.1" 1039,RelayAccessDomain "relay.iohk.example" 1041,RelayAccessAddress "1.1.1.1" 1052,RelayAccessDomain "relay.iohk.example" 1007]),(PoolStake {unPoolStake = 991163 % 10391544},RelayAccessDomain "relay.iohk.example" 1087 :| [RelayAccessAddress "1.1.1.1" 1073,RelayAccessAddress "1.1.1.1" 1085,RelayAccessAddress "1.1.1.1" 1065,RelayAccessDomain "relay.iohk.example" 1026,RelayAccessDomain "relay.iohk.example" 1050,RelayAccessDomain "relay.iohk.example" 1099,RelayAccessDomain "relay.iohk.example" 1032,RelayAccessAddress "1.1.1.1" 1024,RelayAccessDomain "relay.iohk.example" 1052,RelayAccessAddress "1.1.1.1" 1056,RelayAccessDomain "relay.iohk.example" 1025,RelayAccessAddress "1.1.1.1" 1087,RelayAccessDomain "relay.iohk.example" 1065,RelayAccessDomain "relay.iohk.example" 1035,RelayAccessDomain "relay.iohk.example" 1027,RelayAccessAddress "1.1.1.1" 1041,RelayAccessDomain "relay.iohk.example" 1061,RelayAccessAddress "1.1.1.1" 1039,RelayAccessAddress "1.1.1.1" 1016,RelayAccessAddress "1.1.1.1" 1058,RelayAccessAddress "1.1.1.1" 1011,RelayAccessAddress "1.1.1.1" 1018,RelayAccessDomain "relay.iohk.example" 1094,RelayAccessDomain "relay.iohk.example" 1075]),(PoolStake {unPoolStake = 59381 % 3463848},RelayAccessDomain "relay.iohk.example" 1025 :| [RelayAccessAddress "1.1.1.1" 1014,RelayAccessAddress "1.1.1.1" 1032,RelayAccessAddress "1.1.1.1" 1016,RelayAccessDomain "relay.iohk.example" 1097,RelayAccessAddress "1.1.1.1" 1053,RelayAccessDomain "relay.iohk.example" 1099,RelayAccessDomain "relay.iohk.example" 1096,RelayAccessAddress "1.1.1.1" 1087,RelayAccessAddress "1.1.1.1" 1037,RelayAccessAddress "1.1.1.1" 1068,RelayAccessDomain "relay.iohk.example" 1030,RelayAccessDomain "relay.iohk.example" 1005,RelayAccessAddress "1.1.1.1" 1019,RelayAccessDomain "relay.iohk.example" 1075,RelayAccessAddress "1.1.1.1" 1035,RelayAccessAddress "1.1.1.1" 1009,RelayAccessDomain "relay.iohk.example" 1036,RelayAccessAddress "1.1.1.1" 1072,RelayAccessAddress "1.1.1.1" 1091,RelayAccessDomain "relay.iohk.example" 1068,RelayAccessAddress "1.1.1.1" 1062,RelayAccessAddress "1.1.1.1" 1022])]} :| [LedgerPools {getLedgerPools = [(PoolStake {unPoolStake = 1 % 1},RelayAccessDomain "relay.iohk.example" 1071 :| [RelayAccessDomain "test2" 65529])]},LedgerPools {getLedgerPools = [(PoolStake {unPoolStake = 1 % 1},RelayAccessAddress "1.1.1.1" 1076 :| [RelayAccessDomain "test2" 65529])]},LedgerPools {getLedgerPools = [(PoolStake {unPoolStake = 1 % 1},RelayAccessDomain "relay.iohk.example" 1057 :| [RelayAccessDomain "test2" 65529])]},LedgerPools {getLedgerPools = [(PoolStake {unPoolStake = 1 % 1},RelayAccessDomain "relay.iohk.example" 1027 :| [RelayAccessDomain "test2" 65529])]},LedgerPools {getLedgerPools = [(PoolStake {unPoolStake = 1 % 1},RelayAccessAddress "1.1.1.1" 1020 :| [])]},LedgerPools {getLedgerPools = [(PoolStake {unPoolStake = 1 % 1},RelayAccessDomain "relay.iohk.example" 1039 :| [])]},LedgerPools {getLedgerPools = [(PoolStake {unPoolStake = 1 % 1},RelayAccessDomain "relay.iohk.example" 1055 :| [])]},LedgerPools {getLedgerPools = [(PoolStake {unPoolStake = 1 % 1},RelayAccessDomain "relay.iohk.example" 1016 :| [])]},LedgerPools {getLedgerPools = [(PoolStake {unPoolStake = 1 % 1},RelayAccessAddress "1.1.1.1" 1076 :| [RelayAccessDomain "test2" 65529])]},LedgerPools {getLedgerPools = [(PoolStake {unPoolStake = 1 % 1},RelayAccessDomain "relay.iohk.example" 1047 :| [])]},LedgerPools {getLedgerPools = [(PoolStake {unPoolStake = 1 % 1},RelayAccessDomain "relay.iohk.example" 1015 :| [RelayAccessDomain "test2" 65529])]},LedgerPools {getLedgerPools = [(PoolStake {unPoolStake = 1 % 1},RelayAccessDomain "relay.iohk.example" 1002 :| [])]},LedgerPools {getLedgerPools = [(PoolStake {unPoolStake = 1 % 1},RelayAccessDomain "relay.iohk.example" 1082 :| [RelayAccessDomain "test2" 65529])]},LedgerPools {getLedgerPools = [(PoolStake {unPoolStake = 1 % 1},RelayAccessAddress "1.1.1.1" 1085 :| [])]},LedgerPools {getLedgerPools = [(PoolStake {unPoolStake = 1 % 1},RelayAccessDomain "relay.iohk.example" 1074 :| [])]},LedgerPools {getLedgerPools = [(PoolStake {unPoolStake = 1 % 1},RelayAccessAddress "1.1.1.1" 1025 :| [RelayAccessDomain "test2" 65529])]},LedgerPools {getLedgerPools = [(PoolStake {unPoolStake = 1 % 1},RelayAccessAddress "1.1.1.1" 1065 :| [RelayAccessDomain "test2" 65529])]},LedgerPools {getLedgerPools = [(PoolStake {unPoolStake = 1 % 1},RelayAccessDomain "relay.iohk.example" 1025 :| [RelayAccessDomain "test2" 65529])]},LedgerPools {getLedgerPools = [(PoolStake {unPoolStake = 1 % 1},RelayAccessAddress "1.1.1.1" 1038 :| [])]},LedgerPools {getLedgerPools = [(PoolStake {unPoolStake = 1 % 1},RelayAccessDomain "relay.iohk.example" 1040 :| [RelayAccessDomain "test2" 65529])]},LedgerPools {getLedgerPools = [(PoolStake {unPoolStake = 1 % 1},RelayAccessDomain "relay.iohk.example" 1034 :| [])]},LedgerPools {getLedgerPools = [(PoolStake {unPoolStake = 1 % 1},RelayAccessAddress "1.1.1.1" 1036 :| [RelayAccessDomain "test2" 65529])]},LedgerPools {getLedgerPools = [(PoolStake {unPoolStake = 1 % 1},RelayAccessDomain "relay.iohk.example" 1054 :| [RelayAccessDomain "test2" 65529])]},LedgerPools {getLedgerPools = [(PoolStake {unPoolStake = 1 % 1},RelayAccessDomain "relay.iohk.example" 1085 :| [RelayAccessDomain "test2" 65529])]},LedgerPools {getLedgerPools = [(PoolStake {unPoolStake = 1 % 1},RelayAccessAddress "1.1.1.1" 1095 :| [RelayAccessDomain "test2" 65529])]}]))
                             (PeerSelectionTargets {targetNumberOfRootPeers = 4, targetNumberOfKnownPeers = 5, targetNumberOfEstablishedPeers = 2, targetNumberOfActivePeers = 1, targetNumberOfKnownBigLedgerPeers = 5, targetNumberOfEstablishedBigLedgerPeers = 4, targetNumberOfActiveBigLedgerPeers = 3})
                             (Script (DNSTimeout {getDNSTimeout = 0.341} :| [DNSTimeout {getDNSTimeout = 0.275},DNSTimeout {getDNSTimeout = 0.288},DNSTimeout {getDNSTimeout = 0.294},DNSTimeout {getDNSTimeout = 0.249},DNSTimeout {getDNSTimeout = 0.308},DNSTimeout {getDNSTimeout = 0.169},DNSTimeout {getDNSTimeout = 0.203},DNSTimeout {getDNSTimeout = 0.146},DNSTimeout {getDNSTimeout = 0.163},DNSTimeout {getDNSTimeout = 0.167},DNSTimeout {getDNSTimeout = 0.275},DNSTimeout {getDNSTimeout = 0.124},DNSTimeout {getDNSTimeout = 0.275},DNSTimeout {getDNSTimeout = 0.18},DNSTimeout {getDNSTimeout = 0.313}]))
                             (Script (DNSLookupDelay {getDNSLookupDelay = 0.047} :| [DNSLookupDelay {getDNSLookupDelay = 0.026},DNSLookupDelay {getDNSLookupDelay = 0.026},DNSLookupDelay {getDNSLookupDelay = 0.073},DNSLookupDelay {getDNSLookupDelay = 0.091},DNSLookupDelay {getDNSLookupDelay = 0.125},DNSLookupDelay {getDNSLookupDelay = 0.075},DNSLookupDelay {getDNSLookupDelay = 0.05},DNSLookupDelay {getDNSLookupDelay = 0.12},DNSLookupDelay {getDNSLookupDelay = 0.051},DNSLookupDelay {getDNSLookupDelay = 0.034},DNSLookupDelay {getDNSLookupDelay = 0.12},DNSLookupDelay {getDNSLookupDelay = 0.066},DNSLookupDelay {getDNSLookupDelay = 0.104},DNSLookupDelay {getDNSLookupDelay = 0.132},DNSLookupDelay {getDNSLookupDelay = 0.098},DNSLookupDelay {getDNSLookupDelay = 0.031},DNSLookupDelay {getDNSLookupDelay = 0.074}]))
                             Nothing
                             False
                             (Script (FetchModeDeadline :| [FetchModeBulkSync,FetchModeBulkSync,FetchModeBulkSync,FetchModeBulkSync,FetchModeBulkSync,FetchModeBulkSync,FetchModeDeadline]))
                  , [JoinNetwork 4.261538461538,Reconfigure 1.638888888888 [(HotValency {getHotValency = 1},WarmValency {getWarmValency = 1},Map.fromList [(RelayAccessAddress "0.0.37.208" 65532,(DoAdvertisePeer,IsTrustable)),(RelayAccessAddress "0.0.104.38" 24,(DoNotAdvertisePeer,IsNotTrustable))])],Reconfigure 2.755555555555 [],Reconfigure 23.833333333333 [],Reconfigure 22.76 [],Kill 8.392156862745,JoinNetwork 12.303571428571,Kill 25.543859649122,JoinNetwork 17.1875,Reconfigure 7.827586206896 [],Reconfigure 6.953846153846 [],Reconfigure 2.143478260869 [(HotValency {getHotValency = 1},WarmValency {getWarmValency = 1},Map.fromList [])],Kill 10.8,JoinNetwork 16.442307692307,Reconfigure 19.222222222222 [(HotValency {getHotValency = 1},WarmValency {getWarmValency = 1},Map.fromList [])]])
               , ( NodeArgs (25) InitiatorAndResponderDiffusionMode (Just 269)
                            (Map.fromList [(RelayAccessAddress "0.0.37.208" 65532,DoAdvertisePeer)])
                            (Script (UseBootstrapPeers [RelayAccessDomain "test4" 27,RelayAccessAddress "0.0.42.147" 4] :| [UseBootstrapPeers [RelayAccessAddress "0.0.48.140" 65534,RelayAccessAddress "0.0.14.95" 65518,RelayAccessAddress "0:2f0d:0:d64:0:2027:0:1c22" 65534,RelayAccessDomain "test3" 1,RelayAccessAddress "0.0.2.16" 15,RelayAccessAddress "0:3714:0:3681:0:8d4:0:1acf" 23,RelayAccessDomain "test2" 3],UseBootstrapPeers [],UseBootstrapPeers [RelayAccessDomain "test2" 65508,RelayAccessDomain "test5" 10,RelayAccessAddress "0:2e24:0:77c:0:36e9:0:16cb" 25,RelayAccessAddress "0:6a2:0:1019:0:3acf:0:341f" 65508,RelayAccessAddress "0.0.54.209" 65509,RelayAccessDomain "test2" 4,RelayAccessAddress "0.0.22.83" 13,RelayAccessDomain "test3" 4,RelayAccessDomain "test3" 27,RelayAccessAddress "0:f8f:0:eb1:0:1a52:0:1636" 65528,RelayAccessDomain "test4" 0,RelayAccessAddress "0:c5e:0:793:0:485:0:1ee3" 65508,RelayAccessDomain "test3" 21,RelayAccessAddress "0:2eed:0:1862:0:1d56:0:ef2" 26,RelayAccessDomain "test1" 23,RelayAccessDomain "test3" 26,RelayAccessAddress "0.0.7.45" 65532,RelayAccessAddress "0:1f41:0:2f84:0:3e23:0:1ce3" 13,RelayAccessDomain "test4" 65513,RelayAccessDomain "test4" 28,RelayAccessAddress "0:2756:0:12db:0:7bf:0:3c29" 65514,RelayAccessAddress "0:8ae:0:2994:0:3998:0:3eea" 24,RelayAccessDomain "test5" 5,RelayAccessAddress "0:20a0:0:25a4:0:2ccb:0:2486" 65508,RelayAccessDomain "test4" 65508,RelayAccessAddress "0.0.12.45" 65535],DontUseBootstrapPeers,DontUseBootstrapPeers,UseBootstrapPeers [RelayAccessAddress "0:3ca8:0:b0c:0:e6a:0:157f" 5,RelayAccessAddress "0.0.26.194" 65528,RelayAccessAddress "0:2ce1:0:c47:0:15e2:0:3787" 2,RelayAccessAddress "0:3cfd:0:3590:0:662:0:1fa9" 65532,RelayAccessAddress "0.0.7.255" 65516,RelayAccessAddress "0:3937:0:1ae8:0:384c:0:3835" 13,RelayAccessDomain "test2" 65527,RelayAccessAddress "0:292b:0:100e:0:b8c:0:1c5d" 17,RelayAccessAddress "0.0.15.50" 16,RelayAccessAddress "0:24ce:0:8a5:0:353c:0:2813" 65516,RelayAccessDomain "test5" 18,RelayAccessAddress "0.0.46.130" 65511],UseBootstrapPeers [RelayAccessAddress "0.0.14.157" 65521,RelayAccessDomain "test2" 13,RelayAccessDomain "test1" 14,RelayAccessDomain "test3" 65515,RelayAccessDomain "test3" 65527,RelayAccessAddress "0:881:0:2990:0:e47:0:8d7" 26,RelayAccessAddress "0:3aa4:0:685:0:2be:0:1f7" 9,RelayAccessAddress "0:2af5:0:63d:0:5aa:0:307b" 65533,RelayAccessAddress "0:2969:0:30b1:0:3c44:0:c0a" 65512,RelayAccessAddress "0.0.58.229" 7,RelayAccessAddress "0.0.6.7" 65511,RelayAccessDomain "test4" 25],DontUseBootstrapPeers,UseBootstrapPeers [RelayAccessAddress "0.0.34.92" 24,RelayAccessDomain "test5" 25,RelayAccessDomain "test3" 15,RelayAccessAddress "0.0.15.48" 19,RelayAccessDomain "test2" 19,RelayAccessDomain "test4" 1,RelayAccessDomain "test2" 22,RelayAccessAddress "0.0.14.181" 65511],DontUseBootstrapPeers,DontUseBootstrapPeers,DontUseBootstrapPeers,DontUseBootstrapPeers,DontUseBootstrapPeers]))
                            (TestAddress (IPAddr (read "0.0.104.38") 24))
                            PeerSharingEnabled
                            [(HotValency {getHotValency = 1},WarmValency {getWarmValency = 1},Map.fromList [(RelayAccessDomain "test2" 65529,(DoNotAdvertisePeer,IsNotTrustable))]),(HotValency {getHotValency = 1},WarmValency {getWarmValency = 1},Map.fromList [(RelayAccessAddress "0.0.37.208" 65532,(DoNotAdvertisePeer,IsTrustable))])]
                            (Script (LedgerPools {getLedgerPools = [(PoolStake {unPoolStake = 150822 % 4032899},RelayAccessDomain "relay.iohk.example" 1074 :| [RelayAccessDomain "relay.iohk.example" 1054,RelayAccessAddress "1.1.1.1" 1048,RelayAccessDomain "relay.iohk.example" 1084,RelayAccessAddress "1.1.1.1" 1077,RelayAccessDomain "relay.iohk.example" 1031,RelayAccessDomain "relay.iohk.example" 1016,RelayAccessDomain "relay.iohk.example" 1083,RelayAccessDomain "relay.iohk.example" 1009,RelayAccessDomain "relay.iohk.example" 1005,RelayAccessDomain "relay.iohk.example" 1065,RelayAccessAddress "1.1.1.1" 1096,RelayAccessAddress "1.1.1.1" 1060,RelayAccessDomain "relay.iohk.example" 1070,RelayAccessDomain "relay.iohk.example" 1020,RelayAccessAddress "1.1.1.1" 1033,RelayAccessAddress "1.1.1.1" 1097,RelayAccessAddress "1.1.1.1" 1074,RelayAccessDomain "relay.iohk.example" 1080,RelayAccessDomain "relay.iohk.example" 1079,RelayAccessAddress "1.1.1.1" 1052,RelayAccessAddress "1.1.1.1" 1079,RelayAccessDomain "relay.iohk.example" 1081]),(PoolStake {unPoolStake = 75441 % 310223},RelayAccessDomain "relay.iohk.example" 1081 :| [RelayAccessAddress "1.1.1.1" 1003,RelayAccessDomain "relay.iohk.example" 1068,RelayAccessAddress "1.1.1.1" 1055,RelayAccessDomain "relay.iohk.example" 1004,RelayAccessAddress "1.1.1.1" 1058,RelayAccessDomain "relay.iohk.example" 1029,RelayAccessDomain "relay.iohk.example" 1027,RelayAccessDomain "relay.iohk.example" 1028,RelayAccessAddress "1.1.1.1" 1061]),(PoolStake {unPoolStake = 911578 % 4032899},RelayAccessAddress "1.1.1.1" 1093 :| [RelayAccessDomain "relay.iohk.example" 1031,RelayAccessAddress "1.1.1.1" 1052,RelayAccessDomain "relay.iohk.example" 1041,RelayAccessAddress "1.1.1.1" 1078]),(PoolStake {unPoolStake = 821622 % 4032899},RelayAccessAddress "1.1.1.1" 1061 :| [RelayAccessAddress "1.1.1.1" 1046,RelayAccessAddress "1.1.1.1" 1082,RelayAccessAddress "1.1.1.1" 1013,RelayAccessDomain "relay.iohk.example" 1018,RelayAccessAddress "1.1.1.1" 1019,RelayAccessDomain "relay.iohk.example" 1023,RelayAccessDomain "relay.iohk.example" 1043,RelayAccessDomain "relay.iohk.example" 1069,RelayAccessAddress "1.1.1.1" 1017,RelayAccessDomain "relay.iohk.example" 1089,RelayAccessDomain "relay.iohk.example" 1065,RelayAccessAddress "1.1.1.1" 1034,RelayAccessAddress "1.1.1.1" 1020,RelayAccessDomain "relay.iohk.example" 1099,RelayAccessAddress "1.1.1.1" 1035,RelayAccessAddress "1.1.1.1" 1039,RelayAccessDomain "relay.iohk.example" 1034,RelayAccessDomain "relay.iohk.example" 1012,RelayAccessDomain "relay.iohk.example" 1035,RelayAccessAddress "1.1.1.1" 1044,RelayAccessAddress "1.1.1.1" 1010,RelayAccessDomain "relay.iohk.example" 1039]),(PoolStake {unPoolStake = 761531 % 4032899},RelayAccessAddress "1.1.1.1" 1022 :| [RelayAccessAddress "1.1.1.1" 1064,RelayAccessDomain "relay.iohk.example" 1059,RelayAccessAddress "1.1.1.1" 1063,RelayAccessAddress "1.1.1.1" 1021,RelayAccessDomain "relay.iohk.example" 1056,RelayAccessAddress "1.1.1.1" 1033,RelayAccessAddress "1.1.1.1" 1078,RelayAccessAddress "1.1.1.1" 1002,RelayAccessAddress "1.1.1.1" 1027,RelayAccessDomain "relay.iohk.example" 1038,RelayAccessDomain "relay.iohk.example" 1029,RelayAccessAddress "1.1.1.1" 1052,RelayAccessAddress "1.1.1.1" 1082,RelayAccessAddress "1.1.1.1" 1077,RelayAccessDomain "relay.iohk.example" 1069,RelayAccessDomain "relay.iohk.example" 1054,RelayAccessAddress "1.1.1.1" 1071,RelayAccessAddress "1.1.1.1" 1018,RelayAccessDomain "relay.iohk.example" 1083,RelayAccessDomain "relay.iohk.example" 1084,RelayAccessAddress "1.1.1.1" 1037,RelayAccessDomain "relay.iohk.example" 1057,RelayAccessAddress "1.1.1.1" 1060,RelayAccessDomain "relay.iohk.example" 1015,RelayAccessAddress "1.1.1.1" 1097,RelayAccessDomain "relay.iohk.example" 1095]),(PoolStake {unPoolStake = 406613 % 4032899},RelayAccessAddress "1.1.1.1" 1094 :| [RelayAccessDomain "relay.iohk.example" 1007,RelayAccessAddress "1.1.1.1" 1092])]} :| [LedgerPools {getLedgerPools = [(PoolStake {unPoolStake = 1 % 1},RelayAccessDomain "relay.iohk.example" 1020 :| [RelayAccessDomain "test2" 65529])]},LedgerPools {getLedgerPools = [(PoolStake {unPoolStake = 1 % 1},RelayAccessDomain "relay.iohk.example" 1025 :| [])]},LedgerPools {getLedgerPools = [(PoolStake {unPoolStake = 1 % 1},RelayAccessDomain "relay.iohk.example" 1093 :| [])]},LedgerPools {getLedgerPools = [(PoolStake {unPoolStake = 1 % 1},RelayAccessDomain "relay.iohk.example" 1054 :| [RelayAccessDomain "test2" 65529])]},LedgerPools {getLedgerPools = [(PoolStake {unPoolStake = 1 % 1},RelayAccessAddress "1.1.1.1" 1049 :| [])]},LedgerPools {getLedgerPools = [(PoolStake {unPoolStake = 1 % 1},RelayAccessAddress "1.1.1.1" 1099 :| [RelayAccessDomain "test2" 65529])]},LedgerPools {getLedgerPools = [(PoolStake {unPoolStake = 1 % 1},RelayAccessAddress "1.1.1.1" 1002 :| [])]},LedgerPools {getLedgerPools = [(PoolStake {unPoolStake = 1 % 1},RelayAccessDomain "relay.iohk.example" 1021 :| [RelayAccessDomain "test2" 65529])]},LedgerPools {getLedgerPools = [(PoolStake {unPoolStake = 1 % 1},RelayAccessDomain "relay.iohk.example" 1034 :| [])]},LedgerPools {getLedgerPools = [(PoolStake {unPoolStake = 1 % 1},RelayAccessDomain "relay.iohk.example" 1098 :| [RelayAccessDomain "test2" 65529])]},LedgerPools {getLedgerPools = [(PoolStake {unPoolStake = 1 % 1},RelayAccessDomain "relay.iohk.example" 1076 :| [])]}]))
                            (PeerSelectionTargets {targetNumberOfRootPeers = 2, targetNumberOfKnownPeers = 6, targetNumberOfEstablishedPeers = 6, targetNumberOfActivePeers = 2, targetNumberOfKnownBigLedgerPeers = 6, targetNumberOfEstablishedBigLedgerPeers = 1, targetNumberOfActiveBigLedgerPeers = 1})
                            (Script (DNSTimeout {getDNSTimeout = 0.34} :| [DNSTimeout {getDNSTimeout = 0.281},DNSTimeout {getDNSTimeout = 0.232},DNSTimeout {getDNSTimeout = 0.289},DNSTimeout {getDNSTimeout = 0.32},DNSTimeout {getDNSTimeout = 0.18},DNSTimeout {getDNSTimeout = 0.168},DNSTimeout {getDNSTimeout = 0.112},DNSTimeout {getDNSTimeout = 0.121}]))
                            (Script (DNSLookupDelay {getDNSLookupDelay = 0.127} :| [DNSLookupDelay {getDNSLookupDelay = 0.102},DNSLookupDelay {getDNSLookupDelay = 0.131},DNSLookupDelay {getDNSLookupDelay = 0.056},DNSLookupDelay {getDNSLookupDelay = 0.036},DNSLookupDelay {getDNSLookupDelay = 0.022},DNSLookupDelay {getDNSLookupDelay = 0.091},DNSLookupDelay {getDNSLookupDelay = 0.137},DNSLookupDelay {getDNSLookupDelay = 0.072},DNSLookupDelay {getDNSLookupDelay = 0.106},DNSLookupDelay {getDNSLookupDelay = 0.064},DNSLookupDelay {getDNSLookupDelay = 0.049},DNSLookupDelay {getDNSLookupDelay = 0.074},DNSLookupDelay {getDNSLookupDelay = 0.095},DNSLookupDelay {getDNSLookupDelay = 0.054}]))
                            (Just (BlockNo 4))
                            True
                            (Script (FetchModeBulkSync :| [FetchModeDeadline,FetchModeBulkSync,FetchModeDeadline,FetchModeBulkSync]))
                , [JoinNetwork 20.676923076923,Reconfigure 24.806451612903 [(HotValency {getHotValency = 1},WarmValency {getWarmValency = 1},Map.fromList []),(HotValency {getHotValency = 1},WarmValency {getWarmValency = 1},Map.fromList [(RelayAccessAddress "0.0.37.208" 65532,(DoNotAdvertisePeer,IsTrustable))])],Reconfigure 20.823529411764 [(HotValency {getHotValency = 1},WarmValency {getWarmValency = 1},Map.fromList [])],Reconfigure 2.451162790697 [(HotValency {getHotValency = 1},WarmValency {getWarmValency = 1},Map.fromList [(RelayAccessDomain "test2" 65529,(DoNotAdvertisePeer,IsNotTrustable))])],Reconfigure 0.492727272727 [(HotValency {getHotValency = 1},WarmValency {getWarmValency = 1},Map.fromList [])],Reconfigure 0.176470588235 [],Reconfigure 12.615384615384 [(HotValency {getHotValency = 1},WarmValency {getWarmValency = 1},Map.fromList []),(HotValency {getHotValency = 1},WarmValency {getWarmValency = 1},Map.fromList [(RelayAccessAddress "0.0.37.208" 65532,(DoNotAdvertisePeer,IsTrustable))])],Reconfigure 1.222222222222 [(HotValency {getHotValency = 1},WarmValency {getWarmValency = 1},Map.fromList [(RelayAccessAddress "0.0.37.208" 65532,(DoNotAdvertisePeer,IsTrustable))])],Kill 14.875,JoinNetwork 0.25,Reconfigure 27.705882352941 [(HotValency {getHotValency = 1},WarmValency {getWarmValency = 1},Map.fromList []),(HotValency {getHotValency = 1},WarmValency {getWarmValency = 1},Map.fromList [])],Reconfigure 1.610344827586 [(HotValency {getHotValency = 1},WarmValency {getWarmValency = 1},Map.fromList [])],Kill 15])
              , ( NodeArgs (19) InitiatorAndResponderDiffusionMode (Just 135)
                           (Map.fromList [(RelayAccessAddress "0.0.104.38" 24,DoAdvertisePeer)])
                           (Script (UseBootstrapPeers [RelayAccessAddress "0:3fa5:0:3a3c:0:3bf6:0:87a" 65527,RelayAccessDomain "test4" 65513,RelayAccessDomain "test3" 65514,RelayAccessDomain "test1" 28,RelayAccessAddress "0:157c:0:99f:0:3fd9:0:3fa6" 65535,RelayAccessAddress "0:2a54:0:937:0:3590:0:2d0b" 65522,RelayAccessDomain "test1" 26,RelayAccessAddress "0:16eb:0:2db1:0:3ce8:0:1658" 65523,RelayAccessDomain "test2" 24,RelayAccessAddress "0:2420:0:155:0:16a4:0:1718" 65531,RelayAccessAddress "0.0.62.54" 65528,RelayAccessAddress "0:3214:0:2dde:0:3c1b:0:146f" 65521,RelayAccessDomain "test4" 10,RelayAccessDomain "test5" 65520,RelayAccessDomain "test4" 14,RelayAccessAddress "0.0.46.112" 26,RelayAccessDomain "test3" 65516,RelayAccessAddress "0.0.15.38" 21,RelayAccessDomain "test4" 25,RelayAccessAddress "0:7bd:0:1bea:0:14ff:0:14d2" 12,RelayAccessDomain "test5" 7,RelayAccessDomain "test4" 65528,RelayAccessDomain "test2" 65513,RelayAccessAddress "0.0.58.160" 65531] :| [DontUseBootstrapPeers,UseBootstrapPeers [RelayAccessAddress "0.0.39.170" 2,RelayAccessDomain "test2" 24,RelayAccessAddress "0.0.33.49" 2,RelayAccessDomain "test5" 65525,RelayAccessDomain "test5" 65516,RelayAccessAddress "0.0.2.172" 20,RelayAccessDomain "test4" 24,RelayAccessDomain "test5" 65518,RelayAccessDomain "test4" 65515,RelayAccessAddress "0.0.25.115" 1,RelayAccessDomain "test5" 65513],DontUseBootstrapPeers,UseBootstrapPeers [RelayAccessDomain "test4" 65517,RelayAccessDomain "test3" 65515,RelayAccessAddress "0.0.42.145" 65518,RelayAccessDomain "test2" 65516,RelayAccessDomain "test2" 65529,RelayAccessAddress "0:2dea:0:1ed2:0:1162:0:1011" 65533,RelayAccessDomain "test4" 65524,RelayAccessAddress "0.0.54.106" 65535,RelayAccessAddress "0.0.26.93" 65524,RelayAccessDomain "test1" 65514,RelayAccessDomain "test3" 65526,RelayAccessAddress "0.0.51.211" 65525,RelayAccessDomain "test5" 0,RelayAccessAddress "0.0.58.138" 25,RelayAccessDomain "test4" 5,RelayAccessAddress "0.0.57.174" 15,RelayAccessAddress "0:2895:0:1503:0:26ef:0:2402" 14,RelayAccessAddress "0.0.42.99" 65511,RelayAccessAddress "0:2c87:0:1def:0:863:0:1102" 14,RelayAccessDomain "test3" 65530,RelayAccessDomain "test2" 65516,RelayAccessDomain "test2" 3],DontUseBootstrapPeers,UseBootstrapPeers [RelayAccessDomain "test3" 14,RelayAccessAddress "0:3a4d:0:1ad0:0:3c09:0:3f78" 65513,RelayAccessDomain "test3" 65517,RelayAccessDomain "test5" 21,RelayAccessDomain "test3" 65525,RelayAccessAddress "0:21dd:0:31a4:0:7b0:0:192d" 6,RelayAccessAddress "0.0.46.183" 0,RelayAccessAddress "0.0.61.113" 10,RelayAccessDomain "test1" 4,RelayAccessAddress "0.0.18.249" 26,RelayAccessAddress "0:146c:0:2b04:0:2c7a:0:252a" 65514],DontUseBootstrapPeers]))
                           (TestAddress (IPAddr (read "0.0.37.208") 65532))
                           PeerSharingDisabled
                           [(HotValency {getHotValency = 2},WarmValency {getWarmValency = 2},Map.fromList [(RelayAccessDomain "test2" 65529,(DoNotAdvertisePeer,IsNotTrustable)),(RelayAccessAddress "0.0.104.38" 24,(DoNotAdvertisePeer,IsNotTrustable))])]
                           (Script (LedgerPools {getLedgerPools = [(PoolStake {unPoolStake = 11950 % 233741},RelayAccessDomain "relay.iohk.example" 1051 :| [RelayAccessAddress "1.1.1.1" 1048,RelayAccessDomain "relay.iohk.example" 1000,RelayAccessDomain "relay.iohk.example" 1059,RelayAccessAddress "1.1.1.1" 1075,RelayAccessAddress "1.1.1.1" 1081,RelayAccessDomain "relay.iohk.example" 1011,RelayAccessDomain "relay.iohk.example" 1038,RelayAccessDomain "relay.iohk.example" 1058]),(PoolStake {unPoolStake = 24071 % 2103669},RelayAccessAddress "1.1.1.1" 1020 :| [RelayAccessDomain "relay.iohk.example" 1097,RelayAccessDomain "relay.iohk.example" 1076,RelayAccessAddress "1.1.1.1" 1011,RelayAccessDomain "relay.iohk.example" 1061,RelayAccessDomain "relay.iohk.example" 1000,RelayAccessAddress "1.1.1.1" 1037,RelayAccessDomain "relay.iohk.example" 1009,RelayAccessAddress "1.1.1.1" 1061]),(PoolStake {unPoolStake = 496013 % 10518345},RelayAccessAddress "1.1.1.1" 1100 :| [RelayAccessAddress "1.1.1.1" 1061,RelayAccessAddress "1.1.1.1" 1083,RelayAccessAddress "1.1.1.1" 1057,RelayAccessAddress "1.1.1.1" 1059,RelayAccessAddress "1.1.1.1" 1001,RelayAccessAddress "1.1.1.1" 1058,RelayAccessDomain "relay.iohk.example" 1077,RelayAccessDomain "relay.iohk.example" 1030,RelayAccessAddress "1.1.1.1" 1090,RelayAccessAddress "1.1.1.1" 1091,RelayAccessAddress "1.1.1.1" 1033,RelayAccessDomain "relay.iohk.example" 1071,RelayAccessAddress "1.1.1.1" 1030,RelayAccessDomain "relay.iohk.example" 1016,RelayAccessAddress "1.1.1.1" 1085,RelayAccessAddress "1.1.1.1" 1072,RelayAccessDomain "relay.iohk.example" 1048]),(PoolStake {unPoolStake = 162146 % 3506115},RelayAccessDomain "relay.iohk.example" 1021 :| [RelayAccessAddress "1.1.1.1" 1057,RelayAccessAddress "1.1.1.1" 1055,RelayAccessDomain "relay.iohk.example" 1067,RelayAccessAddress "1.1.1.1" 1037,RelayAccessAddress "1.1.1.1" 1088,RelayAccessDomain "relay.iohk.example" 1063,RelayAccessAddress "1.1.1.1" 1077,RelayAccessDomain "relay.iohk.example" 1065,RelayAccessDomain "relay.iohk.example" 1010,RelayAccessDomain "relay.iohk.example" 1060,RelayAccessDomain "relay.iohk.example" 1013,RelayAccessAddress "1.1.1.1" 1089,RelayAccessAddress "1.1.1.1" 1013,RelayAccessAddress "1.1.1.1" 1028,RelayAccessDomain "relay.iohk.example" 1012,RelayAccessAddress "1.1.1.1" 1059,RelayAccessDomain "relay.iohk.example" 1001,RelayAccessAddress "1.1.1.1" 1023,RelayAccessDomain "relay.iohk.example" 1092,RelayAccessDomain "relay.iohk.example" 1053,RelayAccessDomain "relay.iohk.example" 1077,RelayAccessAddress "1.1.1.1" 1001,RelayAccessDomain "relay.iohk.example" 1024,RelayAccessAddress "1.1.1.1" 1061,RelayAccessDomain "relay.iohk.example" 1041]),(PoolStake {unPoolStake = 25537 % 1168705},RelayAccessDomain "relay.iohk.example" 1095 :| [RelayAccessAddress "1.1.1.1" 1092,RelayAccessDomain "relay.iohk.example" 1087,RelayAccessAddress "1.1.1.1" 1053,RelayAccessDomain "relay.iohk.example" 1084,RelayAccessAddress "1.1.1.1" 1089,RelayAccessAddress "1.1.1.1" 1047,RelayAccessDomain "relay.iohk.example" 1067,RelayAccessAddress "1.1.1.1" 1055,RelayAccessAddress "1.1.1.1" 1078,RelayAccessAddress "1.1.1.1" 1041,RelayAccessDomain "relay.iohk.example" 1004,RelayAccessAddress "1.1.1.1" 1007,RelayAccessDomain "relay.iohk.example" 1029,RelayAccessAddress "1.1.1.1" 1005,RelayAccessAddress "1.1.1.1" 1027,RelayAccessDomain "relay.iohk.example" 1081,RelayAccessAddress "1.1.1.1" 1088,RelayAccessAddress "1.1.1.1" 1029,RelayAccessAddress "1.1.1.1" 1034,RelayAccessAddress "1.1.1.1" 1096,RelayAccessAddress "1.1.1.1" 1068,RelayAccessDomain "relay.iohk.example" 1023,RelayAccessAddress "1.1.1.1" 1042,RelayAccessAddress "1.1.1.1" 1031,RelayAccessAddress "1.1.1.1" 1036,RelayAccessAddress "1.1.1.1" 1013]),(PoolStake {unPoolStake = 312418 % 10518345},RelayAccessAddress "1.1.1.1" 1066 :| [RelayAccessDomain "relay.iohk.example" 1016,RelayAccessAddress "1.1.1.1" 1070,RelayAccessDomain "relay.iohk.example" 1069,RelayAccessDomain "relay.iohk.example" 1100]),(PoolStake {unPoolStake = 921847 % 10518345},RelayAccessDomain "relay.iohk.example" 1082 :| [RelayAccessAddress "1.1.1.1" 1093,RelayAccessDomain "relay.iohk.example" 1005,RelayAccessDomain "relay.iohk.example" 1033,RelayAccessDomain "relay.iohk.example" 1056,RelayAccessDomain "relay.iohk.example" 1034,RelayAccessAddress "1.1.1.1" 1067,RelayAccessDomain "relay.iohk.example" 1081,RelayAccessAddress "1.1.1.1" 1092,RelayAccessAddress "1.1.1.1" 1074,RelayAccessAddress "1.1.1.1" 1027,RelayAccessDomain "relay.iohk.example" 1059,RelayAccessDomain "relay.iohk.example" 1072,RelayAccessDomain "relay.iohk.example" 1052,RelayAccessDomain "relay.iohk.example" 1061]),(PoolStake {unPoolStake = 406099 % 10518345},RelayAccessDomain "relay.iohk.example" 1084 :| [RelayAccessAddress "1.1.1.1" 1069,RelayAccessAddress "1.1.1.1" 1084,RelayAccessDomain "relay.iohk.example" 1001,RelayAccessDomain "relay.iohk.example" 1087,RelayAccessAddress "1.1.1.1" 1004,RelayAccessDomain "relay.iohk.example" 1031,RelayAccessAddress "1.1.1.1" 1033,RelayAccessAddress "1.1.1.1" 1049,RelayAccessDomain "relay.iohk.example" 1071,RelayAccessAddress "1.1.1.1" 1082,RelayAccessAddress "1.1.1.1" 1007]),(PoolStake {unPoolStake = 32624 % 1168705},RelayAccessDomain "relay.iohk.example" 1074 :| [RelayAccessDomain "relay.iohk.example" 1063,RelayAccessDomain "relay.iohk.example" 1017,RelayAccessDomain "relay.iohk.example" 1062,RelayAccessDomain "relay.iohk.example" 1013,RelayAccessAddress "1.1.1.1" 1022,RelayAccessDomain "relay.iohk.example" 1036,RelayAccessAddress "1.1.1.1" 1088,RelayAccessDomain "relay.iohk.example" 1051,RelayAccessDomain "relay.iohk.example" 1083,RelayAccessDomain "relay.iohk.example" 1037,RelayAccessAddress "1.1.1.1" 1054,RelayAccessDomain "relay.iohk.example" 1049,RelayAccessDomain "relay.iohk.example" 1071,RelayAccessAddress "1.1.1.1" 1002,RelayAccessDomain "relay.iohk.example" 1098]),(PoolStake {unPoolStake = 11706 % 233741},RelayAccessDomain "relay.iohk.example" 1002 :| [RelayAccessAddress "1.1.1.1" 1023,RelayAccessDomain "relay.iohk.example" 1040,RelayAccessAddress "1.1.1.1" 1092,RelayAccessDomain "relay.iohk.example" 1008,RelayAccessDomain "relay.iohk.example" 1021,RelayAccessDomain "relay.iohk.example" 1041,RelayAccessAddress "1.1.1.1" 1050,RelayAccessAddress "1.1.1.1" 1033,RelayAccessAddress "1.1.1.1" 1048,RelayAccessDomain "relay.iohk.example" 1065,RelayAccessAddress "1.1.1.1" 1067,RelayAccessAddress "1.1.1.1" 1064,RelayAccessAddress "1.1.1.1" 1035,RelayAccessAddress "1.1.1.1" 1068,RelayAccessDomain "relay.iohk.example" 1080,RelayAccessDomain "relay.iohk.example" 1066,RelayAccessAddress "1.1.1.1" 1060,RelayAccessDomain "relay.iohk.example" 1005,RelayAccessDomain "relay.iohk.example" 1099,RelayAccessAddress "1.1.1.1" 1053,RelayAccessDomain "relay.iohk.example" 1067,RelayAccessAddress "1.1.1.1" 1072]),(PoolStake {unPoolStake = 474809 % 10518345},RelayAccessDomain "relay.iohk.example" 1025 :| [RelayAccessAddress "1.1.1.1" 1056,RelayAccessDomain "relay.iohk.example" 1063,RelayAccessDomain "relay.iohk.example" 1039,RelayAccessAddress "1.1.1.1" 1098,RelayAccessDomain "relay.iohk.example" 1062,RelayAccessAddress "1.1.1.1" 1061,RelayAccessAddress "1.1.1.1" 1038,RelayAccessDomain "relay.iohk.example" 1029]),(PoolStake {unPoolStake = 501254 % 10518345},RelayAccessAddress "1.1.1.1" 1071 :| [RelayAccessAddress "1.1.1.1" 1013,RelayAccessAddress "1.1.1.1" 1008,RelayAccessDomain "relay.iohk.example" 1017,RelayAccessAddress "1.1.1.1" 1026,RelayAccessAddress "1.1.1.1" 1032,RelayAccessAddress "1.1.1.1" 1045,RelayAccessAddress "1.1.1.1" 1080,RelayAccessAddress "1.1.1.1" 1070,RelayAccessDomain "relay.iohk.example" 1066,RelayAccessAddress "1.1.1.1" 1059,RelayAccessAddress "1.1.1.1" 1062,RelayAccessDomain "relay.iohk.example" 1062,RelayAccessDomain "relay.iohk.example" 1005,RelayAccessDomain "relay.iohk.example" 1018,RelayAccessAddress "1.1.1.1" 1073,RelayAccessDomain "relay.iohk.example" 1004,RelayAccessDomain "relay.iohk.example" 1013,RelayAccessDomain "relay.iohk.example" 1088,RelayAccessAddress "1.1.1.1" 1047,RelayAccessAddress "1.1.1.1" 1011,RelayAccessAddress "1.1.1.1" 1023,RelayAccessAddress "1.1.1.1" 1029,RelayAccessDomain "relay.iohk.example" 1008]),(PoolStake {unPoolStake = 352726 % 10518345},RelayAccessDomain "relay.iohk.example" 1013 :| [RelayAccessAddress "1.1.1.1" 1017,RelayAccessAddress "1.1.1.1" 1051,RelayAccessAddress "1.1.1.1" 1093,RelayAccessAddress "1.1.1.1" 1019,RelayAccessDomain "relay.iohk.example" 1038,RelayAccessDomain "relay.iohk.example" 1052,RelayAccessAddress "1.1.1.1" 1071,RelayAccessDomain "relay.iohk.example" 1014,RelayAccessDomain "relay.iohk.example" 1080,RelayAccessDomain "relay.iohk.example" 1084]),(PoolStake {unPoolStake = 48416 % 3506115},RelayAccessDomain "relay.iohk.example" 1078 :| [RelayAccessDomain "relay.iohk.example" 1030,RelayAccessAddress "1.1.1.1" 1012,RelayAccessAddress "1.1.1.1" 1065,RelayAccessAddress "1.1.1.1" 1078,RelayAccessAddress "1.1.1.1" 1000,RelayAccessDomain "relay.iohk.example" 1000,RelayAccessAddress "1.1.1.1" 1013,RelayAccessAddress "1.1.1.1" 1071,RelayAccessDomain "relay.iohk.example" 1036,RelayAccessDomain "relay.iohk.example" 1057,RelayAccessDomain "relay.iohk.example" 1073,RelayAccessDomain "relay.iohk.example" 1004,RelayAccessDomain "relay.iohk.example" 1027,RelayAccessDomain "relay.iohk.example" 1080,RelayAccessAddress "1.1.1.1" 1040,RelayAccessDomain "relay.iohk.example" 1092,RelayAccessDomain "relay.iohk.example" 1013,RelayAccessAddress "1.1.1.1" 1035,RelayAccessAddress "1.1.1.1" 1084,RelayAccessDomain "relay.iohk.example" 1048,RelayAccessAddress "1.1.1.1" 1060,RelayAccessDomain "relay.iohk.example" 1094,RelayAccessDomain "relay.iohk.example" 1017]),(PoolStake {unPoolStake = 757192 % 10518345},RelayAccessAddress "1.1.1.1" 1004 :| [RelayAccessAddress "1.1.1.1" 1041,RelayAccessAddress "1.1.1.1" 1087,RelayAccessDomain "relay.iohk.example" 1067,RelayAccessAddress "1.1.1.1" 1043,RelayAccessAddress "1.1.1.1" 1096,RelayAccessAddress "1.1.1.1" 1018,RelayAccessAddress "1.1.1.1" 1026,RelayAccessAddress "1.1.1.1" 1009,RelayAccessDomain "relay.iohk.example" 1064,RelayAccessDomain "relay.iohk.example" 1032,RelayAccessAddress "1.1.1.1" 1002,RelayAccessAddress "1.1.1.1" 1051,RelayAccessAddress "1.1.1.1" 1048,RelayAccessAddress "1.1.1.1" 1033,RelayAccessAddress "1.1.1.1" 1084,RelayAccessDomain "relay.iohk.example" 1055]),(PoolStake {unPoolStake = 81244 % 10518345},RelayAccessDomain "relay.iohk.example" 1017 :| [RelayAccessAddress "1.1.1.1" 1078,RelayAccessAddress "1.1.1.1" 1028,RelayAccessDomain "relay.iohk.example" 1073,RelayAccessDomain "relay.iohk.example" 1060,RelayAccessAddress "1.1.1.1" 1084,RelayAccessDomain "relay.iohk.example" 1069,RelayAccessDomain "relay.iohk.example" 1042,RelayAccessAddress "1.1.1.1" 1057,RelayAccessAddress "1.1.1.1" 1036,RelayAccessDomain "relay.iohk.example" 1054,RelayAccessAddress "1.1.1.1" 1043,RelayAccessAddress "1.1.1.1" 1016,RelayAccessDomain "relay.iohk.example" 1080,RelayAccessAddress "1.1.1.1" 1077,RelayAccessDomain "relay.iohk.example" 1002,RelayAccessDomain "relay.iohk.example" 1079,RelayAccessAddress "1.1.1.1" 1074,RelayAccessAddress "1.1.1.1" 1024,RelayAccessAddress "1.1.1.1" 1097,RelayAccessAddress "1.1.1.1" 1085,RelayAccessDomain "relay.iohk.example" 1097]),(PoolStake {unPoolStake = 14237 % 10518345},RelayAccessAddress "1.1.1.1" 1055 :| [RelayAccessDomain "relay.iohk.example" 1070,RelayAccessDomain "relay.iohk.example" 1065,RelayAccessAddress "1.1.1.1" 1098,RelayAccessDomain "relay.iohk.example" 1090,RelayAccessAddress "1.1.1.1" 1016,RelayAccessDomain "relay.iohk.example" 1083,RelayAccessAddress "1.1.1.1" 1050,RelayAccessDomain "relay.iohk.example" 1016,RelayAccessDomain "relay.iohk.example" 1056,RelayAccessDomain "relay.iohk.example" 1085]),(PoolStake {unPoolStake = 7439 % 256545},RelayAccessAddress "1.1.1.1" 1040 :| [RelayAccessDomain "relay.iohk.example" 1061,RelayAccessAddress "1.1.1.1" 1099,RelayAccessDomain "relay.iohk.example" 1067,RelayAccessAddress "1.1.1.1" 1023,RelayAccessAddress "1.1.1.1" 1090,RelayAccessAddress "1.1.1.1" 1024,RelayAccessAddress "1.1.1.1" 1093,RelayAccessDomain "relay.iohk.example" 1001,RelayAccessDomain "relay.iohk.example" 1041,RelayAccessDomain "relay.iohk.example" 1026,RelayAccessDomain "relay.iohk.example" 1012,RelayAccessDomain "relay.iohk.example" 1002,RelayAccessAddress "1.1.1.1" 1012,RelayAccessDomain "relay.iohk.example" 1009,RelayAccessDomain "relay.iohk.example" 1095,RelayAccessAddress "1.1.1.1" 1041,RelayAccessAddress "1.1.1.1" 1083,RelayAccessAddress "1.1.1.1" 1062,RelayAccessDomain "relay.iohk.example" 1052,RelayAccessAddress "1.1.1.1" 1006,RelayAccessAddress "1.1.1.1" 1065,RelayAccessAddress "1.1.1.1" 1020]),(PoolStake {unPoolStake = 891892 % 10518345},RelayAccessAddress "1.1.1.1" 1037 :| [RelayAccessDomain "relay.iohk.example" 1082,RelayAccessAddress "1.1.1.1" 1029,RelayAccessAddress "1.1.1.1" 1028,RelayAccessDomain "relay.iohk.example" 1006,RelayAccessAddress "1.1.1.1" 1006,RelayAccessAddress "1.1.1.1" 1011,RelayAccessDomain "relay.iohk.example" 1074,RelayAccessAddress "1.1.1.1" 1088,RelayAccessDomain "relay.iohk.example" 1009,RelayAccessDomain "relay.iohk.example" 1010,RelayAccessDomain "relay.iohk.example" 1091,RelayAccessAddress "1.1.1.1" 1021,RelayAccessAddress "1.1.1.1" 1001,RelayAccessAddress "1.1.1.1" 1075,RelayAccessDomain "relay.iohk.example" 1049,RelayAccessDomain "relay.iohk.example" 1015,RelayAccessAddress "1.1.1.1" 1049,RelayAccessDomain "relay.iohk.example" 1093,RelayAccessAddress "1.1.1.1" 1092,RelayAccessAddress "1.1.1.1" 1099,RelayAccessDomain "relay.iohk.example" 1094,RelayAccessDomain "relay.iohk.example" 1047,RelayAccessDomain "relay.iohk.example" 1020,RelayAccessAddress "1.1.1.1" 1047]),(PoolStake {unPoolStake = 78021 % 1168705},RelayAccessDomain "relay.iohk.example" 1059 :| [RelayAccessDomain "relay.iohk.example" 1015,RelayAccessDomain "relay.iohk.example" 1021,RelayAccessAddress "1.1.1.1" 1083,RelayAccessDomain "relay.iohk.example" 1053,RelayAccessDomain "relay.iohk.example" 1065,RelayAccessAddress "1.1.1.1" 1034,RelayAccessAddress "1.1.1.1" 1005,RelayAccessAddress "1.1.1.1" 1013,RelayAccessDomain "relay.iohk.example" 1039,RelayAccessDomain "relay.iohk.example" 1060,RelayAccessAddress "1.1.1.1" 1063,RelayAccessAddress "1.1.1.1" 1093,RelayAccessAddress "1.1.1.1" 1048,RelayAccessDomain "relay.iohk.example" 1087,RelayAccessDomain "relay.iohk.example" 1078,RelayAccessDomain "relay.iohk.example" 1001,RelayAccessDomain "relay.iohk.example" 1045,RelayAccessDomain "relay.iohk.example" 1008,RelayAccessAddress "1.1.1.1" 1097]),(PoolStake {unPoolStake = 976529 % 10518345},RelayAccessDomain "relay.iohk.example" 1067 :| [RelayAccessDomain "relay.iohk.example" 1027,RelayAccessAddress "1.1.1.1" 1001,RelayAccessDomain "relay.iohk.example" 1008,RelayAccessAddress "1.1.1.1" 1056,RelayAccessDomain "relay.iohk.example" 1083,RelayAccessAddress "1.1.1.1" 1067,RelayAccessAddress "1.1.1.1" 1099,RelayAccessAddress "1.1.1.1" 1097,RelayAccessAddress "1.1.1.1" 1050,RelayAccessAddress "1.1.1.1" 1043,RelayAccessAddress "1.1.1.1" 1084,RelayAccessAddress "1.1.1.1" 1035,RelayAccessDomain "relay.iohk.example" 1058,RelayAccessDomain "relay.iohk.example" 1064,RelayAccessDomain "relay.iohk.example" 1009,RelayAccessAddress "1.1.1.1" 1032,RelayAccessDomain "relay.iohk.example" 1003,RelayAccessDomain "relay.iohk.example" 1100,RelayAccessDomain "relay.iohk.example" 1021,RelayAccessAddress "1.1.1.1" 1034,RelayAccessDomain "relay.iohk.example" 1095,RelayAccessAddress "1.1.1.1" 1016,RelayAccessDomain "relay.iohk.example" 1062,RelayAccessAddress "1.1.1.1" 1066,RelayAccessDomain "relay.iohk.example" 1052,RelayAccessDomain "relay.iohk.example" 1023]),(PoolStake {unPoolStake = 392641 % 10518345},RelayAccessDomain "relay.iohk.example" 1027 :| [RelayAccessAddress "1.1.1.1" 1047,RelayAccessDomain "relay.iohk.example" 1029,RelayAccessDomain "relay.iohk.example" 1040,RelayAccessAddress "1.1.1.1" 1016,RelayAccessAddress "1.1.1.1" 1024]),(PoolStake {unPoolStake = 592246 % 10518345},RelayAccessAddress "1.1.1.1" 1075 :| [RelayAccessAddress "1.1.1.1" 1060,RelayAccessDomain "relay.iohk.example" 1075,RelayAccessDomain "relay.iohk.example" 1070,RelayAccessAddress "1.1.1.1" 1086,RelayAccessDomain "relay.iohk.example" 1042,RelayAccessDomain "relay.iohk.example" 1094,RelayAccessAddress "1.1.1.1" 1019,RelayAccessAddress "1.1.1.1" 1045,RelayAccessDomain "relay.iohk.example" 1086,RelayAccessDomain "relay.iohk.example" 1061,RelayAccessAddress "1.1.1.1" 1025,RelayAccessAddress "1.1.1.1" 1058,RelayAccessDomain "relay.iohk.example" 1010,RelayAccessDomain "relay.iohk.example" 1065,RelayAccessAddress "1.1.1.1" 1029,RelayAccessDomain "relay.iohk.example" 1041,RelayAccessDomain "relay.iohk.example" 1033,RelayAccessAddress "1.1.1.1" 1073,RelayAccessDomain "relay.iohk.example" 1043,RelayAccessAddress "1.1.1.1" 1009,RelayAccessDomain "relay.iohk.example" 1005,RelayAccessDomain "relay.iohk.example" 1054,RelayAccessDomain "relay.iohk.example" 1035])]} :| [LedgerPools {getLedgerPools = [(PoolStake {unPoolStake = 1 % 1},RelayAccessDomain "relay.iohk.example" 1047 :| [])]},LedgerPools {getLedgerPools = [(PoolStake {unPoolStake = 1 % 1},RelayAccessAddress "1.1.1.1" 1002 :| [RelayAccessDomain "test2" 65529])]}]))
                           (PeerSelectionTargets {targetNumberOfRootPeers = 1, targetNumberOfKnownPeers = 4, targetNumberOfEstablishedPeers = 3, targetNumberOfActivePeers = 3, targetNumberOfKnownBigLedgerPeers = 1, targetNumberOfEstablishedBigLedgerPeers = 0, targetNumberOfActiveBigLedgerPeers = 0})
                           (Script (DNSTimeout {getDNSTimeout = 0.235} :| [DNSTimeout {getDNSTimeout = 0.26},DNSTimeout {getDNSTimeout = 0.175},DNSTimeout {getDNSTimeout = 0.221},DNSTimeout {getDNSTimeout = 0.143},DNSTimeout {getDNSTimeout = 0.346},DNSTimeout {getDNSTimeout = 0.378},DNSTimeout {getDNSTimeout = 0.297},DNSTimeout {getDNSTimeout = 0.322},DNSTimeout {getDNSTimeout = 0.343},DNSTimeout {getDNSTimeout = 0.174},DNSTimeout {getDNSTimeout = 0.249},DNSTimeout {getDNSTimeout = 0.353},DNSTimeout {getDNSTimeout = 0.404},DNSTimeout {getDNSTimeout = 0.204},DNSTimeout {getDNSTimeout = 0.372}]))
                           (Script (DNSLookupDelay {getDNSLookupDelay = 0.084} :| []))
                           Nothing
                           False
                           (Script (FetchModeBulkSync :| [FetchModeDeadline,FetchModeBulkSync,FetchModeDeadline,FetchModeBulkSync,FetchModeBulkSync,FetchModeBulkSync,FetchModeBulkSync,FetchModeDeadline,FetchModeDeadline,FetchModeBulkSync,FetchModeDeadline,FetchModeBulkSync,FetchModeBulkSync,FetchModeDeadline,FetchModeDeadline,FetchModeBulkSync,FetchModeBulkSync,FetchModeDeadline,FetchModeDeadline,FetchModeBulkSync]))
                , [JoinNetwork 2.516,Kill 9.857142857142,JoinNetwork 0.374358974358,Kill 0.85,JoinNetwork 20.043478260869,Kill 20.225806451612,JoinNetwork 22.307692307692,Reconfigure 2.481818181818 [],Reconfigure 7 [],Reconfigure 25.297297297297 [(HotValency {getHotValency = 2},WarmValency {getWarmValency = 2},Map.fromList [(RelayAccessDomain "test2" 65529,(DoNotAdvertisePeer,IsNotTrustable))])],Reconfigure 4.666666666666 [(HotValency {getHotValency = 2},WarmValency {getWarmValency = 2},Map.fromList [(RelayAccessDomain "test2" 65529,(DoNotAdvertisePeer,IsNotTrustable)),(RelayAccessAddress "0.0.104.38" 24,(DoNotAdvertisePeer,IsNotTrustable))])],Reconfigure 0.838461538461 [],Reconfigure 12.666666666666 [(HotValency {getHotValency = 2},WarmValency {getWarmValency = 2},Map.fromList [(RelayAccessDomain "test2" 65529,(DoNotAdvertisePeer,IsNotTrustable)),(RelayAccessAddress "0.0.104.38" 24,(DoNotAdvertisePeer,IsNotTrustable))])],Reconfigure 23.647058823529 [],Kill 1.318181818181,JoinNetwork 1.5575,Kill 6])]

-- | This test coverage of ConnectionManagerTrace constructors.
--
prop_connection_manager_trace_coverage :: AbsBearerInfo
                                       -> DiffusionScript
                                       -> Property
prop_connection_manager_trace_coverage defaultBearerInfo diffScript =

  let sim :: forall s . IOSim s Void
      sim = diffusionSimulation (toBearerInfo defaultBearerInfo)
                                diffScript
                                iosimTracer

      events :: [ConnectionManagerTrace
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

      events :: [InboundGovernorTrace NtNAddr]
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

      events :: [RemoteTransitionTrace NtNAddr]
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
            selectDiffusionPeerSelectionState (Governor.bootstrapPeersFlag) events

          govLedgerStateJudgement :: Signal LedgerStateJudgement
          govLedgerStateJudgement =
            selectDiffusionPeerSelectionState (Governor.ledgerStateJudgement) events

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
            Signal.keyedTimeoutTruncated
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
            selectDiffusionPeerSelectionState (Governor.bootstrapPeersFlag) events

          govLedgerStateJudgement :: Signal LedgerStateJudgement
          govLedgerStateJudgement =
            selectDiffusionPeerSelectionState (Governor.ledgerStateJudgement) events

          govKnownPeers :: Signal (Set NtNAddr)
          govKnownPeers =
            selectDiffusionPeerSelectionState (KnownPeers.toSet . Governor.knownPeers) events

          govTrustedPeers :: Signal (Set NtNAddr)
          govTrustedPeers =
            selectDiffusionPeerSelectionState
              (\st -> LocalRootPeers.keysSet (LocalRootPeers.clampToTrustable (Governor.localRootPeers st))
                   <> PublicRootPeers.getBootstrapPeers (Governor.publicRootPeers st)
              ) events

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
              (Script ((UseBootstrapPeers [RelayAccessDomain "bootstrap" 00000]) :| []))
              (TestAddress (IPAddr (read "0:7:0:7::") 65533))
              PeerSharingDisabled
              [ (1,1,Map.fromList [(RelayAccessDomain "test2" 65535,(DoNotAdvertisePeer, IsNotTrustable))
              , (RelayAccessAddress "0:6:0:3:0:6:0:5" 65530,(DoNotAdvertisePeer, IsNotTrustable))])
              ]
              (Script (LedgerPools [] :| []))
              nullPeerSelectionTargets {
                targetNumberOfKnownPeers = 2,
                targetNumberOfEstablishedPeers = 2,
                targetNumberOfActivePeers = 1,

                targetNumberOfKnownBigLedgerPeers = 0,
                targetNumberOfEstablishedBigLedgerPeers = 0,
                targetNumberOfActiveBigLedgerPeers = 0
              }
              (Script (DNSTimeout {getDNSTimeout = 0.239} :| [DNSTimeout {getDNSTimeout = 0.181},DNSTimeout {getDNSTimeout = 0.185},DNSTimeout {getDNSTimeout = 0.14},DNSTimeout {getDNSTimeout = 0.221}]))
              (Script (DNSLookupDelay {getDNSLookupDelay = 0.067} :| [DNSLookupDelay {getDNSLookupDelay = 0.097},DNSLookupDelay {getDNSLookupDelay = 0.101},DNSLookupDelay {getDNSLookupDelay = 0.096},DNSLookupDelay {getDNSLookupDelay = 0.051}]))
              Nothing
              False
              (Script (FetchModeDeadline :| []))
          , [JoinNetwork 1.742857142857
            ,Reconfigure 6.33333333333 [(1,1,Map.fromList [(RelayAccessDomain "test2" 65535,(DoAdvertisePeer, IsNotTrustable))]),
                                        (1,1,Map.fromList [(RelayAccessAddress "0:6:0:3:0:6:0:5" 65530,(DoAdvertisePeer, IsNotTrustable))
                                       ])]
            ,Reconfigure 23.88888888888 [(1,1,Map.fromList []),(1,1,Map.fromList [(RelayAccessAddress "0:6:0:3:0:6:0:5" 65530,(DoAdvertisePeer, IsNotTrustable))])]
            ,Reconfigure 4.870967741935 [(1,1,Map.fromList [(RelayAccessDomain "test2" 65535,(DoAdvertisePeer, IsNotTrustable))])]
            ]
          )
        , ( NodeArgs (1) InitiatorAndResponderDiffusionMode (Just 135)
             (Map.fromList [(RelayAccessAddress "0:7:0:7::" 65533, DoAdvertisePeer)])
              (Script ((UseBootstrapPeers [RelayAccessDomain "bootstrap" 00000]) :| []))
             (TestAddress (IPAddr (read "0:6:0:3:0:6:0:5") 65530))
             PeerSharingDisabled
             []
             (Script (LedgerPools [] :| []))
             nullPeerSelectionTargets {
               targetNumberOfRootPeers = 2,
               targetNumberOfKnownPeers = 5,
               targetNumberOfEstablishedPeers = 1,
               targetNumberOfActivePeers = 1
             }
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
            ,Reconfigure 4.533333333333 [(1,1,Map.fromList [])]
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
    verify_coolingToColdDemotions :: Events DiffusionTestTrace -> Property
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
                  Signal.keyedTimeoutTruncated
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

      events :: [ServerTrace NtNAddr]
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
      peerSelectionActionsTraceMap (PeerStatusChanged _)             =
        "PeerStatusChanged"
      peerSelectionActionsTraceMap (PeerStatusChangeFailure _ ft) =
        "PeerStatusChangeFailure " ++ show ft
      peerSelectionActionsTraceMap (PeerMonitoringError _ se)        =
        "PeerMonitoringError " ++ show se
      peerSelectionActionsTraceMap (PeerMonitoringResult _ wspt)     =
        "PeerMonitoringResult " ++ show wspt

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
               . fmap (\(WithTime t (WithName name b)) -> (WithName name (WithTime t b)))
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
    absInfo =
      AbsBearerInfo
        { abiConnectionDelay = SmallDelay,
          abiInboundAttenuation = NoAttenuation NormalSpeed,
          abiOutboundAttenuation = ErrorInterval NormalSpeed (Time 17.666666666666) 888,
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
            (Script ((UseBootstrapPeers [RelayAccessDomain "bootstrap" 00000]) :| []))
            (TestAddress (IPAddr (read "0.0.1.236") 65527))
            PeerSharingDisabled
            [ (2,2,Map.fromList [ (RelayAccessDomain "test2" 15,(DoNotAdvertisePeer, IsNotTrustable))
                                , (RelayAccessDomain "test3" 4,(DoAdvertisePeer, IsNotTrustable))])
            ]
            (Script (LedgerPools [] :| []))
            PeerSelectionTargets
              { targetNumberOfRootPeers = 6,
                targetNumberOfKnownPeers = 7,
                targetNumberOfEstablishedPeers = 7,
                targetNumberOfActivePeers = 6,

                targetNumberOfKnownBigLedgerPeers = 0,
                targetNumberOfEstablishedBigLedgerPeers = 0,
                targetNumberOfActiveBigLedgerPeers = 0
              }
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
              , Reconfigure 0.415384615384 [(1,1,Map.fromList [])
              , (1,1,Map.fromList [])]
              , Reconfigure 15.550561797752 [(1,1,Map.fromList [])
              , (1,1,Map.fromList [(RelayAccessDomain "test2" 15,(DoAdvertisePeer, IsNotTrustable))])]
              , Reconfigure 82.85714285714 []
              ])
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
    verify_target_established_public :: Events DiffusionTestTrace -> Property
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
        $ True

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
    verify_target_active_public :: Events DiffusionTestTrace -> Property
    verify_target_active_public events =
        let govPublicRootPeersSig :: Signal (Set NtNAddr)
            govPublicRootPeersSig =
              selectDiffusionPeerSelectionState
                (PublicRootPeers.toSet . Governor.publicRootPeers)
                events

            govActivePeersSig :: Signal (Set NtNAddr)
            govActivePeersSig =
              selectDiffusionPeerSelectionState Governor.activePeers events

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
          $ True


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
    verify_target_active_local :: Events DiffusionTestTrace -> Property
    verify_target_active_local events =
        let govLocalRootPeersSig :: Signal (Set NtNAddr)
            govLocalRootPeersSig =
              selectDiffusionPeerSelectionState
                (LocalRootPeers.keysSet . Governor.localRootPeers) events

            govActivePeersSig :: Signal (Set NtNAddr)
            govActivePeersSig =
              selectDiffusionPeerSelectionState Governor.activePeers events

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
          $ True

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
    verify_target_active_root :: Events DiffusionTestTrace -> Property
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
          $ True


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
    verify_target_established_local :: Events DiffusionTestTrace -> Property
    verify_target_established_local events =
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
            Signal.keyedTimeoutTruncated
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
    verify_target_active_below :: Events DiffusionTestTrace -> Property
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
                                                      (Set.singleton()))
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
          promotionOpportunity target local established active recentFailures isAlive inProgressDemoteToCold
            | isAlive && Set.size active < target
            = established Set.\\ active
                          Set.\\ LocalRootPeers.keysSet local
                          Set.\\ recentFailures
                          Set.\\ inProgressDemoteToCold

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

          promotionOpportunitiesIgnoredTooLong :: Signal (Set NtNAddr)
          promotionOpportunitiesIgnoredTooLong =
            Signal.keyedTimeoutTruncated
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
    verify_target_active_below :: Events DiffusionTestTrace -> Property
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
            (\local established active recentFailures isAlive inProgressDemoteToCold ->
              if isAlive then
                Set.unions
                  [ -- There are no opportunities if we're at or above target
                    if Set.size groupActive >= hotTarget
                       then Set.empty
                       else groupEstablished Set.\\ active
                                             Set.\\ recentFailures
                                             Set.\\ inProgressDemoteToCold
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

          promotionOpportunitiesIgnoredTooLong :: Signal (Set NtNAddr)
          promotionOpportunitiesIgnoredTooLong =
            Signal.keyedTimeoutTruncated
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
                   naLocalSelectionTargets = Governor.nullPeerSelectionTargets {
                                               targetNumberOfKnownPeers = 2,
                                               targetNumberOfEstablishedPeers = 2,
                                               targetNumberOfActivePeers = 2
                                             }
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
    common = NodeArgs {
        naSeed             = 10,
        naDiffusionMode    = InitiatorAndResponderDiffusionMode,
        naMbTime           = Just 1,
        naPublicRoots      = Map.empty,
        naBootstrapPeers   = (Script ((UseBootstrapPeers [RelayAccessDomain "bootstrap" 00000]) :| [])),
        naAddr             = undefined,
        naLocalRootPeers   = undefined,
        naLedgerPeers      = Script (LedgerPools [] :| []),
        naLocalSelectionTargets
                           = Governor.nullPeerSelectionTargets {
                               targetNumberOfKnownPeers = 1,
                               targetNumberOfEstablishedPeers = 1,
                               targetNumberOfActivePeers = 1
                             },
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
                (\case Right a -> a
                       _       -> Set.empty)
                (\case Left (Just a) -> a
                       _             -> Set.empty)
                (\case Left Nothing -> True
                       _            -> False)
            . Signal.fromEventsWith (Right Set.empty)
            . Signal.selectEvents
                (\case DiffusionPeerSelectionActionsTrace (PeerStatusChanged (HotToCooling connId)) ->
                           Just $ Right demotions
                         where
                           demotions = Set.singleton (remoteAddress connId)
                       DiffusionPeerSelectionActionsTrace (PeerStatusChanged (WarmToCooling connId)) ->
                           Just $ Right demotions
                         where
                           demotions = Set.singleton (remoteAddress connId)
                       DiffusionConnectionManagerTrace (TrConnectionCleanup connId) ->
                           Just $ Left failures
                         where
                           failures = Just $ Set.singleton (remoteAddress connId)
                       DiffusionPeerSelectionTrace (TraceDemoteAsynchronous status) ->
                           Just $ Left (Just failures)
                         where
                           failures = Map.keysSet (Map.filter ((==PeerCooling) . fst) status)
                       DiffusionPeerSelectionTrace (TraceDemoteBigLedgerPeersAsynchronous status) ->
                           Just $ Left (Just failures)
                         where
                           failures = Map.keysSet (Map.filter ((==PeerCooling) . fst) status)
                       DiffusionPeerSelectionTrace (TraceDemoteLocalAsynchronous status) ->
                           Just $ Left (Just failures)
                         where
                           failures = Map.keysSet (Map.filter ((==PeerCooling) . fst) status)
                       DiffusionPeerSelectionTrace (TraceDemoteHotFailed _ _ peeraddr _) ->
                           Just $ Left (Just failures)
                         where
                           failures = Set.singleton peeraddr
                       DiffusionPeerSelectionTrace (TraceDemoteWarmFailed _ _ peeraddr _) ->
                           Just $ Left (Just failures)
                         where
                           failures = Set.singleton peeraddr
                       DiffusionPeerSelectionTrace (TracePromoteColdFailed _ _ peeraddr _ _) ->
                           Just $ Left (Just failures)
                         where
                           failures = Set.singleton peeraddr
                       DiffusionPeerSelectionTrace (TracePromoteWarmFailed _ _ peeraddr _) ->
                           Just $ Left (Just failures)
                         where
                           failures = Set.singleton peeraddr
                       DiffusionPeerSelectionTrace (TraceDemoteWarmDone _ _ peeraddr) ->
                           Just $ Left (Just failures)
                         where
                           failures = Set.singleton peeraddr
                       DiffusionPeerSelectionTrace (TracePromoteColdBigLedgerPeerFailed _ _ peeraddr _ _) ->
                           Just $ Left (Just failures)
                         where
                           failures = Set.singleton peeraddr
                       DiffusionPeerSelectionTrace (TracePromoteWarmBigLedgerPeerFailed _ _ peeraddr _) ->
                           Just $ Left (Just failures)
                         where
                           failures = Set.singleton peeraddr
                       DiffusionPeerSelectionTrace (TraceDemoteHotBigLedgerPeerFailed _ _ peeraddr _) ->
                           Just $ Left (Just failures)
                         where
                           failures = Set.singleton peeraddr
                       DiffusionPeerSelectionTrace (TraceDemoteWarmBigLedgerPeerFailed _ _ peeraddr _) ->
                           Just $ Left (Just failures)
                         where
                           failures = Set.singleton peeraddr
                       DiffusionPeerSelectionTrace (TraceDemoteWarmBigLedgerPeerDone _ _ peeraddr) ->
                           Just $ Left (Just failures)
                         where
                           failures = Set.singleton peeraddr
                       DiffusionConnectionManagerTrace TrShutdown ->
                           Just $ Left Nothing

                       _ -> Nothing
                )
            $ events

          demotionOpportunitiesTooLong :: Signal (Set NtNAddr)
          demotionOpportunitiesTooLong =
              Signal.keyedTimeoutTruncated 1 id demotionOpportunities

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
    verify_target_active_above :: Events DiffusionTestTrace -> Property
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
            Signal.keyedTimeoutTruncated
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

          connectionManagerEvents :: [ConnectionManagerTrace
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
  let bearerInfo = AbsBearerInfo {
                     abiConnectionDelay = NormalDelay,
                     abiInboundAttenuation = NoAttenuation FastSpeed,
                     abiOutboundAttenuation = NoAttenuation FastSpeed,
                     abiInboundWriteFailure = Nothing,
                     abiOutboundWriteFailure = Nothing,
                     abiAcceptFailure = Just (SmallDelay,AbsIOErrResourceExhausted),
                     abiSDUSize = LargeSDU
                   }
      diffScript = DiffusionScript
        (SimArgs 1 10)
        (singletonTimedScript Map.empty)
        [( NodeArgs (-3) InitiatorAndResponderDiffusionMode (Just 224)
             (Map.fromList [])
             (Script ((UseBootstrapPeers [RelayAccessDomain "bootstrap" 00000]) :| []))
             (TestAddress (IPAddr (read "0.0.0.4") 9))
             PeerSharingDisabled
             [(1,1,Map.fromList [(RelayAccessAddress "0.0.0.8" 65531,(DoNotAdvertisePeer, IsNotTrustable))])]
             (Script (LedgerPools [] :| []))
             nullPeerSelectionTargets {
                 targetNumberOfRootPeers = 2,
                 targetNumberOfKnownPeers = 5,
                 targetNumberOfEstablishedPeers = 4,
                 targetNumberOfActivePeers = 1
               }
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
             (Script ((UseBootstrapPeers [RelayAccessDomain "bootstrap" 00000]) :| []))
             (TestAddress (IPAddr (read "0.0.0.8") 65531))
             PeerSharingDisabled
             [(1,1,Map.fromList [(RelayAccessAddress "0.0.0.4" 9,(DoNotAdvertisePeer, IsNotTrustable))])]
             (Script (LedgerPools [] :| []))
             nullPeerSelectionTargets {
                 targetNumberOfRootPeers = 4,
                 targetNumberOfKnownPeers = 5,
                 targetNumberOfEstablishedPeers = 3,
                 targetNumberOfActivePeers = 1,

                 targetNumberOfKnownBigLedgerPeers = 0,
                 targetNumberOfEstablishedBigLedgerPeers = 0,
                 targetNumberOfActiveBigLedgerPeers = 0
               }
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
              (Script (DontUseBootstrapPeers :| []))
              (TestAddress (IPAddr (read "0.0.0.0") 0))
              PeerSharingDisabled
              [ (2,2,Map.fromList [ (RelayAccessAddress "0.0.0.1" 0,(DoNotAdvertisePeer, IsNotTrustable))
                                  , (RelayAccessAddress "0.0.0.2" 0,(DoNotAdvertisePeer, IsNotTrustable))
                                  ])
              ]
              (Script (LedgerPools [] :| []))
              PeerSelectionTargets {
                targetNumberOfRootPeers = 1,
                targetNumberOfKnownPeers = 1,
                targetNumberOfEstablishedPeers = 1,
                targetNumberOfActivePeers = 1,

                targetNumberOfKnownBigLedgerPeers = 0,
                targetNumberOfEstablishedBigLedgerPeers = 0,
                targetNumberOfActiveBigLedgerPeers = 0
              }
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
               (Script (DontUseBootstrapPeers :| []))
               (TestAddress (IPAddr (read "0.0.0.1") 0))
               PeerSharingDisabled
               [(1,1,Map.fromList [(RelayAccessAddress "0.0.0.0" 0,(DoNotAdvertisePeer, IsNotTrustable))])]
               (Script (LedgerPools [] :| []))
               PeerSelectionTargets {
                 targetNumberOfRootPeers = 1,
                 targetNumberOfKnownPeers = 1,
                 targetNumberOfEstablishedPeers = 1,
                 targetNumberOfActivePeers = 1,

                 targetNumberOfKnownBigLedgerPeers = 0,
                 targetNumberOfEstablishedBigLedgerPeers = 0,
                 targetNumberOfActiveBigLedgerPeers = 0
               }
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
      let connectionManagerEvents :: [ConnectionManagerTrace
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
               TrConnectionExists {}    -> counterexample (show ev) False
               TrForbiddenConnection {} -> counterexample (show ev) False
               _                        -> property True
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
          $ ev
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
                $ False
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
                                                    $ ( promotionTime `diffTime` demotionTime
                                                     >= repromoteDelay config_REPROMOTE_DELAY
                                                      )
               g as@(WithTime demotionTime (PeerStatusChanged WarmToCooling{}) : as') =
                 case find (\case
                               WithTime _ (PeerStatusChanged ColdToWarm{}) -> True
                               _ -> False)
                           as' of

                   Nothing                         -> property True
                   Just (WithTime promotionTime _) -> counterexample (show as)
                                                    $ ( promotionTime `diffTime` demotionTime
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
                WithTime _ (PeerMonitoringResult connId _)   -> Just connId)
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

    targets x = PeerSelectionTargets {
        targetNumberOfRootPeers = x,
        targetNumberOfKnownPeers = x,
        targetNumberOfEstablishedPeers = x,
        targetNumberOfActivePeers = x,
        targetNumberOfKnownBigLedgerPeers = 0,
        targetNumberOfEstablishedBigLedgerPeers = 0,
        targetNumberOfActiveBigLedgerPeers = 0
      }

    defaultNodeArgs = NodeArgs {
        naSeed = 0,
        naDiffusionMode = InitiatorAndResponderDiffusionMode,
        naMbTime = Nothing,
        naPublicRoots = mempty,
        naBootstrapPeers = singletonScript DontUseBootstrapPeers,
        naAddr = undefined,
        naPeerSharing = PeerSharingEnabled,
        naLocalRootPeers = undefined,
        naLedgerPeers = singletonScript (LedgerPools []),
        naLocalSelectionTargets = undefined,
        naDNSTimeoutScript = singletonScript (DNSTimeout 300),
        naDNSLookupDelayScript = singletonScript (DNSLookupDelay 0.01),
        naChainSyncEarlyExit = False,
        naChainSyncExitOnBlockNo = Nothing,
        naFetchModeScript = singletonScript FetchModeDeadline
      }

    script = DiffusionScript
               (mainnetSimArgs 3)
               (singletonScript (mempty, ShortDelay))
               [ ( defaultNodeArgs { naAddr = ip_0,
                                     naLocalRootPeers = [(1, 1, Map.fromList [(ra_1, (DoNotAdvertisePeer, IsNotTrustable))])],
                                     naLocalSelectionTargets = targets 1
                                   }
                 , [JoinNetwork 0]
                 )
               , ( defaultNodeArgs { naAddr = ip_1,
                                     naLocalRootPeers = [],
                                     naLocalSelectionTargets = targets 2
                                   }
                 , [JoinNetwork 0]
                 )
               , ( defaultNodeArgs { naAddr = ip_2,
                                     naLocalRootPeers = [(1, 1, Map.fromList [(ra_1, (DoNotAdvertisePeer, IsNotTrustable))])],
                                     naLocalSelectionTargets = targets 2
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
      . concat
      . repeat
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
      let remoteTransitionTraceEvents :: Trace () (RemoteTransitionTrace NtNAddr)
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

      let remoteTransitionTraceEvents :: Trace () (RemoteTransitionTrace NtNAddr)
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
          $ ev
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
  . Signal.fromChangeEvents (f $ Governor.emptyPeerSelectionState (mkStdGen 42))
  . Signal.selectEvents
      (\case
        DiffusionDebugPeerSelectionTrace (TraceGovernorState _ _ st) -> Just (f st)
        _                                                            -> Nothing)

selectDiffusionPeerSelectionState' :: Eq a
                                  => (forall peerconn. Governor.PeerSelectionState NtNAddr peerconn -> a)
                                  -> Events DiffusionTestTrace
                                  -> Signal a
selectDiffusionPeerSelectionState' f =
  -- TODO: #3182 Rng seed should come from quickcheck.
    Signal.fromChangeEvents (f $ Governor.emptyPeerSelectionState (mkStdGen 42))
  . Signal.selectEvents
      (\case
        DiffusionDebugPeerSelectionTrace (TraceGovernorState _ _ st) -> Just (f st)
        _                                                            -> Nothing)

selectDiffusionConnectionManagerEvents
  :: Trace () DiffusionTestTrace
  -> Trace () (ConnectionManagerTrace NtNAddr
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
  -> Trace () (RemoteTransitionTrace NtNAddr)
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
        biAcceptFailures       = Nothing, -- TODO
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
              ++ show (length $ filter ((== InitiatorOnlyDiffusionMode) . naDiffusionMode . fst) $ nodes))
    . label ("Nº active peers: "
              ++ show (sum . map (targetNumberOfActivePeers . naLocalSelectionTargets . fst) $ nodes))
    . label ("Nº active big ledger peers: "
              ++ show (sum . map (targetNumberOfActiveBigLedgerPeers . naLocalSelectionTargets . fst) $ nodes))
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

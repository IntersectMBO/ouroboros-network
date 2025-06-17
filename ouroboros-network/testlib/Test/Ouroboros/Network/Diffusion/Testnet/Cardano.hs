{-# LANGUAGE CPP                 #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NumericUnderscores  #-}
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

module Test.Ouroboros.Network.Diffusion.Testnet.Cardano (tests) where

import Control.Exception (AssertionFailed (..), catch, evaluate, fromException)
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadTest (exploreRaces)
import Control.Monad.Class.MonadTime.SI
import Control.Monad.IOSim

import Data.Bifoldable (bifoldMap)
import Data.Bifunctor (first)
import Data.Dynamic (fromDynamic)
import Data.Foldable (fold)
import Data.IP qualified as IP
import Data.List qualified as List
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
import GHC.IO.Exception (IOErrorType (..), IOException (..))
import System.Random (mkStdGen)

import Network.DNS.Types qualified as DNS
import Network.Mux.Trace qualified as Mx

import Cardano.Network.ConsensusMode
import Cardano.Network.PeerSelection.Bootstrap (UseBootstrapPeers (..),
           requiresBootstrapPeers)
import Cardano.Network.PeerSelection.PeerTrustable (PeerTrustable (..))
import Cardano.Network.Types (LedgerStateJudgement, NumberOfBigLedgerPeers (..))

import Cardano.Network.PeerSelection.ExtraRootPeers qualified as Cardano
import Cardano.Network.PeerSelection.ExtraRootPeers qualified as Cardano.ExtraPeers
import Cardano.Network.PeerSelection.Governor.PeerSelectionState qualified as Cardano
import Cardano.Network.PeerSelection.Governor.PeerSelectionState qualified as Cardano.ExtraState

import Ouroboros.Network.Block (BlockNo (..))
import Ouroboros.Network.BlockFetch (PraosFetchMode (..),
           TraceFetchClientState (..))
import Ouroboros.Network.ConnectionHandler (ConnectionHandlerTrace (..))
import Ouroboros.Network.ConnectionId
import Ouroboros.Network.ConnectionManager.Core qualified as CM
import Ouroboros.Network.ConnectionManager.State qualified as CM
import Ouroboros.Network.ConnectionManager.Types
import Ouroboros.Network.Diffusion.Policies qualified as Diffusion
import Ouroboros.Network.ExitPolicy (RepromoteDelay (..))
import Ouroboros.Network.InboundGovernor qualified as IG
import Ouroboros.Network.Mock.ConcreteBlock (BlockHeader)
import Ouroboros.Network.NodeToNode (DiffusionMode (..))
import Ouroboros.Network.PeerSelection
import Ouroboros.Network.PeerSelection.Governor hiding (PeerSelectionState (..))
import Ouroboros.Network.PeerSelection.Governor qualified as Governor
import Ouroboros.Network.PeerSelection.PublicRootPeers qualified as PublicRootPeers
import Ouroboros.Network.PeerSelection.RootPeersDNS hiding (IOError)
import Ouroboros.Network.PeerSelection.State.EstablishedPeers qualified as EstablishedPeers
import Ouroboros.Network.PeerSelection.State.KnownPeers qualified as KnownPeers
import Ouroboros.Network.PeerSelection.State.LocalRootPeers (HotValency (..),
           LocalRootConfig (..), WarmValency (..))
import Ouroboros.Network.PeerSelection.State.LocalRootPeers qualified as LocalRootPeers
import Ouroboros.Network.PeerSharing (PeerSharingResult (..))
import Ouroboros.Network.Server qualified as Server

import Test.Ouroboros.Network.ConnectionManager.Timeouts
import Test.Ouroboros.Network.ConnectionManager.Utils
import Test.Ouroboros.Network.Data.AbsBearerInfo
import Test.Ouroboros.Network.Data.Script
import Test.Ouroboros.Network.Data.Signal
import Test.Ouroboros.Network.Data.Signal qualified as Signal
import Test.Ouroboros.Network.Diffusion.Node (config_REPROMOTE_DELAY)
import Test.Ouroboros.Network.Diffusion.Node.Kernel
import Test.Ouroboros.Network.Diffusion.Testnet.Cardano.Simulation
import Test.Ouroboros.Network.InboundGovernor.Utils
import Test.Ouroboros.Network.LedgerPeers (LedgerPools (..))
import Test.Ouroboros.Network.Utils hiding (SmallDelay, debugTracer)

import Simulation.Network.Snocket (BearerInfo (..))

import Cardano.Network.PeerSelection.PublicRootPeers qualified as PublicRootPeers
import Test.QuickCheck
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
                                 prop_diffusion_nofail_iosimpor
    , nightlyTest $ testProperty "no livelock"
                                 prop_diffusion_nolivelock_iosimpor
    , nightlyTest $ testProperty "dns can recover from fails"
                                 prop_diffusion_dns_can_recover_iosimpor
    , nightlyTest $ testProperty "target established public"
                                 prop_diffusion_target_established_public_iosimpor
    , nightlyTest $ testProperty "target active public"
                                 prop_diffusion_target_active_public_iosimpor
    , nightlyTest $ testProperty "target established local"
                                 prop_diffusion_target_established_local_iosimpor
    , nightlyTest $ testProperty "target active local"
                                 prop_diffusion_target_active_local_iosimpor
    , nightlyTest $ testProperty "target active root"
                                 prop_diffusion_target_active_root_iosimpor
    , nightlyTest $ testProperty "target active below"
                                 prop_diffusion_target_active_below_iosimpor
    , nightlyTest $ testProperty "target active local below"
                                 prop_diffusion_target_active_local_below_iosimpor
    , nightlyTest $ testProperty "async demotion"
                                 prop_diffusion_async_demotions_iosimpor
    , nightlyTest $ testProperty "target active local above"
                                 prop_diffusion_target_active_local_above_iosimpor
    , nightlyTest $ testProperty "connection manager valid transitions"
                                 prop_diffusion_cm_valid_transitions_iosimpor
    , nightlyTest $ testProperty "connection manager valid transition order"
                                 prop_diffusion_cm_valid_transition_order_iosimpor
    , nightlyTest $ testProperty "connection manager no dodgy traces"
                                 prop_diffusion_cm_no_dodgy_traces_iosimpor
    , nightlyTest $ testProperty "peer selection actions no dodgy traces"
                                 prop_diffusion_peer_selection_actions_no_dodgy_traces_iosimpor
    , nightlyTest $ testProperty "inbound governor valid transitions"
                                 prop_diffusion_ig_valid_transitions_iosimpor
    , nightlyTest $ testProperty "inbound governor valid transition order"
                                 prop_diffusion_ig_valid_transition_order_iosimpor
    , nightlyTest $ testProperty "cm & ig timeouts enforced"
                                 prop_diffusion_timeouts_enforced_iosimpor
    , nightlyTest $ testProperty "any Cold async demotion"
                                 prop_track_coolingToCold_demotions_iosimpor
    , nightlyTest $ testProperty "only bootstrap peers in fallback state"
                                 prop_only_bootstrap_peers_in_fallback_state_iosimpor
    , testGroup "Churn"
      [ nightlyTest $ testProperty "no timeouts"
                                   prop_churn_notimeouts_iosimpor
      , nightlyTest $ testProperty "steps"
                                   prop_churn_steps_iosimpor
      ]
    , testGroup "unit"
      [ nightlyTest $ testProperty "unit cm"
                                   unit_cm_valid_transitions
      ]
    ]
  , testGroup "IOSim"
    [ testProperty "no failure"
                   prop_diffusion_nofail_iosim
    , testProperty "no livelock"
                   prop_diffusion_nolivelock_iosim
    , testProperty "dns can recover from fails"
                   prop_diffusion_dns_can_recover_iosim
    , testProperty "unit #4191"
                   unit_4191
    , testProperty "target established public"
                   prop_diffusion_target_established_public_iosim
    , testProperty "target active public"
                   prop_diffusion_target_active_public_iosim
    , testProperty "target established local"
                   prop_diffusion_target_established_local_iosim
    , testProperty "unit reconnect"
                   prop_unit_reconnect
    , testProperty "target active local"
                   prop_diffusion_target_active_local_iosim
    , testProperty "target active root"
                   prop_diffusion_target_active_root_iosim
    , testProperty "target active below"
                   prop_diffusion_target_active_below_iosim
    , testProperty "target active local below"
                   prop_diffusion_target_active_local_below_iosim
    , testProperty "async demotion"
                   prop_diffusion_async_demotions_iosim
    , testProperty "async demotion (unit)"
                   unit_diffusion_async_demotions
    , testProperty "target active local above"
                   prop_diffusion_target_active_local_above_iosim
    , testProperty "connection manager valid transitions"
                   prop_diffusion_cm_valid_transitions_iosim
    , testProperty "connection manager valid transition order"
                   prop_diffusion_cm_valid_transition_order_iosim
    , testProperty "unit 4258"
                   prop_unit_4258
    , testProperty "connection manager no dodgy traces"
                   prop_diffusion_cm_no_dodgy_traces_iosim
    , testProperty "peer selection actions no dodgy traces"
                   prop_diffusion_peer_selection_actions_no_dodgy_traces_iosim
    , testProperty "inbound governor valid transitions"
                   prop_diffusion_ig_valid_transitions_iosim
    , testProperty "inbound governor valid transition order"
                   prop_diffusion_ig_valid_transition_order_iosim
    , testProperty "cm & ig timeouts enforced"
                   prop_diffusion_timeouts_enforced_iosim
    , testProperty "any Cold async demotion"
                   prop_track_coolingToCold_demotions_iosim
    , testProperty "unit #4177"
                   unit_4177
    , testProperty "connect failure"
                   prop_connect_failure
    , testProperty "accept failure"
                   prop_accept_failure
    , testProperty "only bootstrap peers in fallback state"
                   prop_only_bootstrap_peers_in_fallback_state_iosim
    , testGroup "local root diffusion mode"
        [ testProperty "InitiatorOnly"
          (unit_local_root_diffusion_mode InitiatorOnlyDiffusionMode)
        , testProperty "InitiatorAndResponder"
          (unit_local_root_diffusion_mode InitiatorAndResponderDiffusionMode)
        ]
    , testGroup "Peer Sharing"
      [ testProperty "share a peer"
                     unit_peer_sharing
      , testProperty "don't peershare the unwilling"
                      prop_no_peershare_unwilling_iosim
      ]
    , testGroup "Churn"
      [ testProperty "no timeouts" prop_churn_notimeouts_iosim
      , testProperty "steps" prop_churn_steps_iosim
      ]
    , testGroup "coverage"
      [ testProperty "server trace coverage"
                     prop_server_trace_coverage
      , testProperty "peer selection actions trace coverage"
                     prop_peer_selection_action_trace_coverage
      , testProperty "peer selection trace coverage"
                     prop_peer_selection_trace_coverage
      , testProperty "connection manager trace coverage"
                     unit_connection_manager_trace_coverage
      , testProperty "connection manager transitions coverage"
                     unit_connection_manager_transitions_coverage
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

--
-- Test execution utils & constants
--

-- | 125_000s events
long_trace :: Int
long_trace = 125_000

-- | 250_000 events
very_long_trace :: Int
very_long_trace = 2 * long_trace

-- | 10_000s events
short_trace :: Int
short_trace = 10_000


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
      trace = runSimTrace sim
   in labelDiffusionScript ds
    $ counterexample (Trace.ppTrace show (ppSimEvent 0 0 0) $ Trace.take traceNumber trace)
    $ f trace traceNumber


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


--
-- Properties
--


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
                  (Map.fromList [(RelayAccessAddress "0:71:0:1:0:1:0:1" 65_534,
                                  DoAdvertisePeer)])
                  GenesisMode
                  (Script (DontUseBootstrapPeers :| []))
                  (TestAddress (IPAddr (read "0:79::1:0:0") 3))
                  PeerSharingDisabled
                  [ (HotValency {getHotValency = 1},
                     WarmValency {getWarmValency = 1},
                     Map.fromList [(RelayAccessAddress "0:71:0:1:0:1:0:1" 65_534,
                                    LocalRootConfig DoAdvertisePeer InitiatorAndResponderDiffusionMode IsTrustable)])
                   ]
                  (Script (LedgerPools [] :| []))
                  (PeerSelectionTargets
                        { targetNumberOfRootPeers                 = 4
                        , targetNumberOfKnownPeers                = 4
                        , targetNumberOfEstablishedPeers          = 3
                        , targetNumberOfActivePeers               = 2
                        , targetNumberOfKnownBigLedgerPeers       = 4
                        , targetNumberOfEstablishedBigLedgerPeers = 1
                        , targetNumberOfActiveBigLedgerPeers      = 1
                        }
                  , PeerSelectionTargets
                        { targetNumberOfRootPeers                 = 0
                        , targetNumberOfKnownPeers                = 4
                        , targetNumberOfEstablishedPeers          = 0
                        , targetNumberOfActivePeers               = 0
                        , targetNumberOfKnownBigLedgerPeers       = 4
                        , targetNumberOfEstablishedBigLedgerPeers = 4
                        , targetNumberOfActiveBigLedgerPeers      = 3
                        }
                  )
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
                  (TestAddress (IPAddr (read "0:71:0:1:0:1:0:1") 65_534))
                  PeerSharingEnabled
                  [ (HotValency {getHotValency = 1},
                     WarmValency {getWarmValency = 1},
                     Map.fromList [(RelayAccessAddress "0:79::1:0:0" 3,
                                    LocalRootConfig DoNotAdvertisePeer InitiatorAndResponderDiffusionMode IsTrustable)])
                   ]
                  (Script (LedgerPools [] :| []))
                  ( PeerSelectionTargets
                        { targetNumberOfRootPeers                 = 1
                        , targetNumberOfKnownPeers                = 1
                        , targetNumberOfEstablishedPeers          = 1
                        , targetNumberOfActivePeers               = 1
                        , targetNumberOfKnownBigLedgerPeers       = 4
                        , targetNumberOfEstablishedBigLedgerPeers = 3
                        , targetNumberOfActiveBigLedgerPeers      = 3
                        }
                  , PeerSelectionTargets
                        { targetNumberOfRootPeers                 = 0
                        , targetNumberOfKnownPeers                = 1
                        , targetNumberOfEstablishedPeers          = 1
                        , targetNumberOfActivePeers               = 1
                        , targetNumberOfKnownBigLedgerPeers       = 4
                        , targetNumberOfEstablishedBigLedgerPeers = 2
                        , targetNumberOfActiveBigLedgerPeers      = 2
                        }
                  )
                  (Script (DNSTimeout {getDNSTimeout = 0.18} :| []))
                  (Script (DNSLookupDelay {getDNSLookupDelay = 0.125} :| []))
                  (Just (BlockNo 2))
                  False
                  (Script (FetchModeDeadline :| []))
                  , [JoinNetwork 1.484_848_484_848]
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
       prop_diffusion_cm_valid_transition_order' ioSimTrace short_trace

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
              $ [ assertPeerSelectionState Cardano.toSet Cardano.invariant st ()
                | (_, DiffusionDebugPeerSelectionTrace (TraceGovernorState _ _ st)) <- trace ]
              )
       `catch` \(AssertionFailed _) -> return False
     if r
     then return $ property True
     else do
       putStrLn $ List.intercalate "\n" $ map show trace
       -- the ioSimTrace is infinite, but it will terminate with `AssertionFailed`
       error "impossible!"

prop_diffusion_nofail_iosimpor
  :: AbsBearerInfo -> DiffusionScript -> Property
prop_diffusion_nofail_iosimpor
  = testWithIOSimPOR prop_diffusion_nofail short_trace

prop_diffusion_nofail_iosim
  :: AbsBearerInfo -> DiffusionScript -> Property
prop_diffusion_nofail_iosim
  = testWithIOSim prop_diffusion_nofail long_trace

-- | This test coverage of 'CM.Trace' constructors.
--
-- TODO: to turn this test into a property test requires to generate
-- `DiffusionScript` which have at least two nodes that connect to each other.
--
unit_connection_manager_trace_coverage :: Property
unit_connection_manager_trace_coverage =
  withMaxSuccess 1 $
  let sim :: forall s . IOSim s Void
      sim = diffusionSimulation (toBearerInfo absNoAttenuation)
                                script
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
             . Trace.take long_trace
             $ runSimTrace sim

      eventsSeenNames = map connectionManagerTraceMap events

   in tabulate "connection manager trace" eventsSeenNames
    $ label (showBucket 250 $ length events)
        (case events of [] | not . all (List.null . snd) $ nodes
                           -> False
                        _  -> True)
  where
    addr, addr' :: NtNAddr
    addr  = TestAddress (IPAddr (read "127.0.0.2") 1_000)
    addr' = TestAddress (IPAddr (read "127.0.0.1") 1_000)

    script@(DiffusionScript _ _ nodes) =
      DiffusionScript
        (SimArgs 1 20)
        (singletonTimedScript Map.empty)
        [ -- a relay node
          (NodeArgs {
             naSeed = 0,
             naDiffusionMode = InitiatorAndResponderDiffusionMode,
             naMbTime = Just 224,
             naPublicRoots = Map.empty,
             naConsensusMode = PraosMode,
             naBootstrapPeers = Script (DontUseBootstrapPeers :| []),
             naAddr = addr',
             naPeerSharing = PeerSharingDisabled,
             naLocalRootPeers = [],
             naLedgerPeers = Script (LedgerPools [] :| []),
             naPeerTargets = (PeerSelectionTargets
               { targetNumberOfRootPeers = 1,
                 targetNumberOfKnownPeers = 1,
                 targetNumberOfEstablishedPeers = 0,
                 targetNumberOfActivePeers = 0,

                 targetNumberOfKnownBigLedgerPeers = 0,
                 targetNumberOfEstablishedBigLedgerPeers = 0,
                 targetNumberOfActiveBigLedgerPeers = 0
               }, nullPeerSelectionTargets),
             naDNSTimeoutScript = Script (DNSTimeout {getDNSTimeout = 1} :| []),
             naDNSLookupDelayScript = Script (DNSLookupDelay {getDNSLookupDelay = 0.1} :| []),
             naChainSyncExitOnBlockNo = Nothing,
             naChainSyncEarlyExit = False,
             naFetchModeScript = Script (FetchModeDeadline :| [])
           }
          , [JoinNetwork 0]
          )
        , -- a relay, which has the BP as a local root
          (NodeArgs {
             naSeed = 0,
             naDiffusionMode = InitiatorAndResponderDiffusionMode,
             naMbTime = Just 224,
             naPublicRoots = Map.empty,
             naConsensusMode = PraosMode,
             naBootstrapPeers = Script (DontUseBootstrapPeers :| []),
             naAddr = addr,
             naPeerSharing = PeerSharingDisabled,
             naLocalRootPeers =
               [ (1,1,Map.fromList [ (RelayAccessAddress (read "127.0.0.1") 1000,
                                      LocalRootConfig DoNotAdvertisePeer InitiatorAndResponderDiffusionMode IsNotTrustable)
                                   ])
               ],
             naLedgerPeers = Script (LedgerPools [] :| []),
             naPeerTargets = (PeerSelectionTargets
               { targetNumberOfRootPeers = 6,
                 targetNumberOfKnownPeers = 7,
                 targetNumberOfEstablishedPeers = 7,
                 targetNumberOfActivePeers = 6,

                 targetNumberOfKnownBigLedgerPeers = 0,
                 targetNumberOfEstablishedBigLedgerPeers = 0,
                 targetNumberOfActiveBigLedgerPeers = 0
               }, nullPeerSelectionTargets),
             naDNSTimeoutScript = Script (DNSTimeout {getDNSTimeout = 1} :| []),
             naDNSLookupDelayScript = Script (DNSLookupDelay {getDNSLookupDelay = 0.1} :| []),
             naChainSyncExitOnBlockNo = Nothing,
             naChainSyncEarlyExit = False,
             naFetchModeScript = Script (FetchModeDeadline :| [])
           }
          , [JoinNetwork 0]
          )
        ]

-- | This tests coverage of ConnectionManager transitions.
--
-- TODO: to turn this test into a property test requires to generate
-- `DiffusionScript` which have at least two nodes that connect to each other.
--
unit_connection_manager_transitions_coverage :: Property
unit_connection_manager_transitions_coverage =
  withMaxSuccess 1 $
  let sim :: forall s . IOSim s Void
      sim = diffusionSimulation (toBearerInfo absNoAttenuation)
                                script
                                iosimTracer
      trace = runSimTrace sim

      -- events from `traceTVar` installed in `newMutableConnState`
      events :: [AbstractTransitionTrace CM.ConnStateId]
      events = fmap (\((WithName _ b)) -> b)
             . selectTraceEventsDynamic' @_ @(CM.ConnectionTransitionTrace NtNAddr)
             . Trace.take long_trace
             $ trace

      -- events from the transition tracer
      events' :: [AbstractTransitionTrace CM.ConnStateId]
      events' = Trace.toList
              . selectDiffusionConnectionManagerTransitionEvents
              . fmap (wnEvent . wtEvent)
              . withTimeNameTraceEvents
                 @DiffusionTestTrace
                 @NtNAddr
              . first (const ())
              . Trace.take long_trace
              $ trace

      transitionsSeenNames = map (snd . validTransitionMap . ttTransition)
                                 events

   in tabulate "connection manager transitions" transitionsSeenNames
    $ counterexample "traceTVar"
      (label ("traceTVar transitions: " ++ showBucket 250 (length events))
        (case events of [] | not . all (List.null . snd) $ nodes
                           -> False
                        _  -> True))
      .&&.
      counterexample "trace"
      (label ("tracer transitions: " ++ showBucket 250 (length events'))
        (case events' of [] | not . all (List.null . snd) $ nodes
                            -> False
                         _  -> True))

  where
    addr, addr' :: NtNAddr
    addr  = TestAddress (IPAddr (read "127.0.0.2") 1000)
    addr' = TestAddress (IPAddr (read "127.0.0.1") 1000)

    script@(DiffusionScript _ _ nodes) =
      DiffusionScript
        (SimArgs 1 20)
        (singletonTimedScript Map.empty)
        [ -- a relay node
          (NodeArgs {
             naSeed = 0,
             naDiffusionMode = InitiatorAndResponderDiffusionMode,
             naMbTime = Just 224,
             naPublicRoots = Map.empty,
             naConsensusMode = PraosMode,
             naBootstrapPeers = Script (DontUseBootstrapPeers :| []),
             naAddr = addr',
             naPeerSharing = PeerSharingDisabled,
             naLocalRootPeers = [],
             naLedgerPeers = Script (LedgerPools [] :| []),
             naPeerTargets = (PeerSelectionTargets
               { targetNumberOfRootPeers = 1,
                 targetNumberOfKnownPeers = 1,
                 targetNumberOfEstablishedPeers = 0,
                 targetNumberOfActivePeers = 0,

                 targetNumberOfKnownBigLedgerPeers = 0,
                 targetNumberOfEstablishedBigLedgerPeers = 0,
                 targetNumberOfActiveBigLedgerPeers = 0
               }, nullPeerSelectionTargets),
             naDNSTimeoutScript = Script (DNSTimeout {getDNSTimeout = 1} :| []),
             naDNSLookupDelayScript = Script (DNSLookupDelay {getDNSLookupDelay = 0.1} :| []),
             naChainSyncExitOnBlockNo = Nothing,
             naChainSyncEarlyExit = False,
             naFetchModeScript = Script (FetchModeDeadline :| [])
           }
          , [JoinNetwork 0]
          )
        , -- a relay, which has the BP as a local root
          (NodeArgs {
             naSeed = 0,
             naDiffusionMode = InitiatorAndResponderDiffusionMode,
             naMbTime = Just 224,
             naPublicRoots = Map.empty,
             naConsensusMode = PraosMode,
             naBootstrapPeers = Script (DontUseBootstrapPeers :| []),
             naAddr = addr,
             naPeerSharing = PeerSharingDisabled,
             naLocalRootPeers =
               [ (1,1,Map.fromList [ (RelayAccessAddress (read "127.0.0.1") 1000,
                                      LocalRootConfig DoNotAdvertisePeer InitiatorAndResponderDiffusionMode IsNotTrustable)
                                   ])
               ],
             naLedgerPeers = Script (LedgerPools [] :| []),
             naPeerTargets = (PeerSelectionTargets
               { targetNumberOfRootPeers = 6,
                 targetNumberOfKnownPeers = 7,
                 targetNumberOfEstablishedPeers = 7,
                 targetNumberOfActivePeers = 6,

                 targetNumberOfKnownBigLedgerPeers = 0,
                 targetNumberOfEstablishedBigLedgerPeers = 0,
                 targetNumberOfActiveBigLedgerPeers = 0
               }, nullPeerSelectionTargets),
             naDNSTimeoutScript = Script (DNSTimeout {getDNSTimeout = 1} :| []),
             naDNSLookupDelayScript = Script (DNSLookupDelay {getDNSLookupDelay = 0.1} :| []),
             naChainSyncExitOnBlockNo = Nothing,
             naChainSyncEarlyExit = False,
             naFetchModeScript = Script (FetchModeDeadline :| [])
           }
          , [JoinNetwork 0]
          )
        ]


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
             . Trace.take long_trace
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
             . Trace.take long_trace
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
             . Trace.take long_trace
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
            selectDiffusionPeerSelectionState
              (Cardano.bootstrapPeersFlag . Governor.extraState)
              events

          -- A signal which shows when bootstrap peers changed and are no
          -- longer a subset of previous bootstrap peers set.  This is a more
          -- refined version of simply observing `TraceUseBootstrapPeersChanged
          -- UseBootstrapPeers{}`
          useBootstrapPeersChangedSig :: Signal Bool
          useBootstrapPeersChangedSig =
              Signal.fromChangeEvents False
            . Signal.selectEvents
                (\case
                 TraceUseBootstrapPeersChanged UseBootstrapPeers{}
                   -> Just True
                 _ -> Nothing
                )
            . selectDiffusionPeerSelectionEvents
            $ events


          govLedgerStateJudgement :: Signal LedgerStateJudgement
          govLedgerStateJudgement =
            selectDiffusionPeerSelectionState
              (Cardano.ledgerStateJudgement . Governor.extraState)
              events

          trJoinKillSig :: Signal JoinedOrKilled
          trJoinKillSig =
              Signal.fromChangeEvents Terminated -- Default to TrKillingNode
            . Signal.selectEvents
                (\case TrJoiningNetwork -> Just Joined
                       TrTerminated     -> Just Terminated
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
            <$> Signal.keyedUntil (fromJoinedOrTerminated (Set.singleton ())
                                                           Set.empty)
                                  (fromJoinedOrTerminated  Set.empty
                                                          (Set.singleton ()))
                                  (const False)
                                  trJoinKillSig

          keepNonTrustablePeersTooLong :: Signal (Set NtNAddr)
          keepNonTrustablePeersTooLong =
            Signal.keyedTimeout
              -- Due to the possibilities of the node being reconfigured
              -- frequently and disconnection timeouts we have to increase
              -- this value
              Diffusion.closeConnectionTimeout
              (\( knownPeers
                , useBootstrapPeers
                , useBootstrapPeersChanged
                , trustedPeers
                , ledgerStateJudgement
                , isAlive
                ) ->
                if    isAlive
                   && not useBootstrapPeersChanged
                   && requiresBootstrapPeers useBootstrapPeers ledgerStateJudgement
                   then knownPeers `Set.difference` trustedPeers
                   else Set.empty
              )
              ((,,,,,) <$> govKnownPeers
                       <*> govUseBootstrapPeers
                       <*> useBootstrapPeersChangedSig
                       <*> govTrustedPeers
                       <*> govLedgerStateJudgement
                       <*> trIsNodeAlive
              )
       in counterexample (List.intercalate "\n" $ map show $ Signal.eventsToList events)
        $ signalProperty 20 show
            Set.null
            keepNonTrustablePeersTooLong

prop_only_bootstrap_peers_in_fallback_state_iosimpor
  :: AbsBearerInfo -> DiffusionScript -> Property
prop_only_bootstrap_peers_in_fallback_state_iosimpor
  = testWithIOSimPOR prop_only_bootstrap_peers_in_fallback_state short_trace

prop_only_bootstrap_peers_in_fallback_state_iosim
  :: AbsBearerInfo -> DiffusionScript -> Property
prop_only_bootstrap_peers_in_fallback_state_iosim
  = testWithIOSim prop_only_bootstrap_peers_in_fallback_state long_trace


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
              (Map.fromList [(RelayAccessDomain "test2" 65_535, DoAdvertisePeer)])
              PraosMode
              (Script (UseBootstrapPeers [RelayAccessDomain "bootstrap" 0] :| []))
              (TestAddress (IPAddr (read "0:7:0:7::") 65_533))
              PeerSharingDisabled
              [ (1,1,Map.fromList [(RelayAccessDomain "test2" 65_535,LocalRootConfig DoNotAdvertisePeer InitiatorAndResponderDiffusionMode IsNotTrustable)
              , (RelayAccessAddress "0:6:0:3:0:6:0:5" 65_530,LocalRootConfig DoNotAdvertisePeer InitiatorAndResponderDiffusionMode IsNotTrustable)])
              ]
              (Script (LedgerPools [] :| []))
              (nullPeerSelectionTargets {
                    targetNumberOfKnownPeers = 2,
                    targetNumberOfEstablishedPeers = 2,
                    targetNumberOfActivePeers = 1,
                    targetNumberOfKnownBigLedgerPeers = 0,
                    targetNumberOfEstablishedBigLedgerPeers = 0,
                    targetNumberOfActiveBigLedgerPeers = 0 }
              , nullPeerSelectionTargets)
              (Script (DNSTimeout {getDNSTimeout = 0.239} :| [DNSTimeout {getDNSTimeout = 0.181},DNSTimeout {getDNSTimeout = 0.185},DNSTimeout {getDNSTimeout = 0.14},DNSTimeout {getDNSTimeout = 0.221}]))
              (Script (DNSLookupDelay {getDNSLookupDelay = 0.067} :| [DNSLookupDelay {getDNSLookupDelay = 0.097},DNSLookupDelay {getDNSLookupDelay = 0.101},DNSLookupDelay {getDNSLookupDelay = 0.096},DNSLookupDelay {getDNSLookupDelay = 0.051}]))
              Nothing
              False
              (Script (FetchModeDeadline :| []))
          , [JoinNetwork 1.742_857_142_857
            ,Reconfigure 6.333_333_333_33 [(1,1,Map.fromList [(RelayAccessDomain "test2" 65_535,LocalRootConfig DoAdvertisePeer InitiatorAndResponderDiffusionMode IsNotTrustable)]),
                                        (1,1,Map.fromList [(RelayAccessAddress "0:6:0:3:0:6:0:5" 65_530,LocalRootConfig DoAdvertisePeer InitiatorAndResponderDiffusionMode IsNotTrustable)
                                       ])]
            ,Reconfigure 23.888_888_888_88 [(1,1,Map.empty),(1,1,Map.fromList [(RelayAccessAddress "0:6:0:3:0:6:0:5" 65_530,LocalRootConfig DoAdvertisePeer InitiatorAndResponderDiffusionMode IsNotTrustable)])]
            ,Reconfigure 4.870_967_741_935 [(1,1,Map.fromList [(RelayAccessDomain "test2" 65_535,LocalRootConfig DoAdvertisePeer InitiatorAndResponderDiffusionMode IsNotTrustable)])]
            ]
          )
        , ( NodeArgs 1 InitiatorAndResponderDiffusionMode (Just 135)
             (Map.fromList [(RelayAccessAddress "0:7:0:7::" 65_533, DoAdvertisePeer)])
             PraosMode
              (Script (UseBootstrapPeers [RelayAccessDomain "bootstrap" 0] :| []))
             (TestAddress (IPAddr (read "0:6:0:3:0:6:0:5") 65_530))
             PeerSharingDisabled
             []
             (Script (LedgerPools [] :| []))
             (nullPeerSelectionTargets {
                   targetNumberOfRootPeers = 2,
                   targetNumberOfKnownPeers = 5,
                   targetNumberOfEstablishedPeers = 1,
                   targetNumberOfActivePeers = 1 }
             , nullPeerSelectionTargets)
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
          , [JoinNetwork 0.183_783_783_783
            ,Reconfigure 4.533_333_333_333 [(1,1,Map.empty)]
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
              Signal.fromChangeEvents Terminated -- Default to TrKillingNode
            . Signal.selectEvents
                (\case TrJoiningNetwork -> Just Joined
                       TrTerminated     -> Just Terminated
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
            <$> Signal.keyedUntil (fromJoinedOrTerminated (Set.singleton ())
                                                           Set.empty)
                                  (fromJoinedOrTerminated  Set.empty
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

prop_track_coolingToCold_demotions_iosimpor
  :: AbsBearerInfo -> DiffusionScript -> Property
prop_track_coolingToCold_demotions_iosimpor
  = testWithIOSimPOR prop_track_coolingToCold_demotions short_trace

prop_track_coolingToCold_demotions_iosim
  :: AbsBearerInfo -> DiffusionScript -> Property
prop_track_coolingToCold_demotions_iosim
  = testWithIOSim prop_track_coolingToCold_demotions long_trace


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
             . Trace.take long_trace
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
             . Trace.take long_trace
             $ runSimTrace sim

      -- we need to avoid leaking addresses, otherwise the report becomes not
      -- very useful
      peerSelectionActionsTraceMap :: PeerSelectionActionsTrace NtNAddr NtNVersion
                                   -> String
      peerSelectionActionsTraceMap (PeerStatusChanged tp)         =
        "PeerStatusChanged " ++ renderPeerStatusChangeType tp
      peerSelectionActionsTraceMap (PeerStatusChangeFailure tp _) =
          "PeerStatusChangeFailure "
        ++ renderPeerStatusChangeType tp
      peerSelectionActionsTraceMap (PeerMonitoringError _ e)      =
        "PeerMonitoringError " ++ show e
      peerSelectionActionsTraceMap (PeerMonitoringResult _ _res)  =
        "PeerMonitoringResult"
      peerSelectionActionsTraceMap (AcquireConnectionError e)
        | Just ioe <- fromException e
        = "AcquireConnectionError: " ++ show (ioe_type ioe)
        | otherwise
        = "AcquireConnectionError: " ++ show e

      eventsSeenNames = map peerSelectionActionsTraceMap events

      renderPeerStatusChangeType :: PeerStatusChangeType NtNAddr -> String
      renderPeerStatusChangeType = \case
        ColdToWarm{}    -> "ColdToWarm"
        WarmToHot{}     -> "WarmToHot"
        HotToWarm{}     -> "HotToWarm"
        WarmToCooling{} -> "WarmToCooling"
        HotToCooling{}  -> "HotToCooling"
        CoolingToCold{} -> "CoolingToCold"

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

      events :: [TracePeerSelection Cardano.ExtraState PeerTrustable (Cardano.ExtraPeers NtNAddr) NtNAddr]
      events = mapMaybe (\case DiffusionPeerSelectionTrace st -> Just st
                               _                              -> Nothing
                        )
             . Trace.toList
             . fmap (\(WithTime _ (WithName _ b)) -> b)
             . withTimeNameTraceEvents
                @DiffusionTestTrace
                @NtNAddr
             . Trace.take long_trace
             $ runSimTrace sim

      peerSelectionTraceMap
        :: ( Show extraDebugState
           , Show extraFlags
           , Show extraPeers
           ) => TracePeerSelection extraDebugState extraFlags extraPeers NtNAddr -> String
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

          numberOfEvents = 10_000 * 5 -- 5 is the maximum number of nodes in a simulation

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

prop_diffusion_nolivelock_iosimpor
  :: AbsBearerInfo -> DiffusionScript -> Property
prop_diffusion_nolivelock_iosimpor
  = testWithIOSimPOR prop_diffusion_nolivelock short_trace

prop_diffusion_nolivelock_iosim
  :: AbsBearerInfo -> DiffusionScript -> Property
prop_diffusion_nolivelock_iosim
  = testWithIOSim prop_diffusion_nolivelock long_trace


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
    ttlForDnsError DNS.NameError _ = 10_800
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
    clipTTLAbove = min 86_400  -- and 24hrs

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
          (TraceLocalRootFailure rap (DNSError err)) ->
            let dns = extractDomainName rap
                ttl = fromMaybe 0 $ Map.lookup dns ttlMap
                ttl' = ttlForDnsError err ttl
                ttlMap' = Map.insert dns ttl' ttlMap
             in verify (Map.insert dns (addTime ttl' t) toRecover)
                        ttlMap'
                        recovered t evs
        DiffusionLocalRootPeerTrace
          (TraceLocalRootReconfigured _ _) ->
            verify Map.empty ttlMap recovered t evs
        DiffusionDNSTrace (DNSResult DNSLocalPeer domain srvDomain ipsttls) ->
          let primaryDomain = fromMaybe domain srvDomain
              ttls = map getTTLs ipsttls
              ttlMap' = Map.insert primaryDomain (ttlForResults ttls) ttlMap
           in case Map.lookup primaryDomain toRecover of
                Nothing -> verify toRecover ttlMap' recovered t evs
                Just _  -> verify (Map.delete primaryDomain toRecover)
                                  ttlMap'
                                  (recovered + 1)
                                  t
                                  evs
        DiffusionSimulationTrace TrReconfiguringNode ->
          verify Map.empty ttlMap recovered t evs
        _ -> verify toRecover ttlMap recovered time evs

    extractDomainName (RelayAccessDomain d _)  = d
    extractDomainName (RelayAccessSRVDomain d) = d
    extractDomainName _                        = error "impossible!"

    getTTLs (_, _, it) = it

prop_diffusion_dns_can_recover_iosimpor
  :: AbsBearerInfo -> DiffusionScript -> Property
prop_diffusion_dns_can_recover_iosimpor
  = testWithIOSimPOR prop_diffusion_dns_can_recover short_trace

prop_diffusion_dns_can_recover_iosim
  :: AbsBearerInfo -> DiffusionScript -> Property
prop_diffusion_dns_can_recover_iosim
  = testWithIOSim prop_diffusion_dns_can_recover long_trace

-- | Unit test which covers issue #4191
--
unit_4191 :: Property
unit_4191 = testWithIOSim prop_diffusion_dns_can_recover long_trace absInfo script
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
          abiOutboundAttenuation = ErrorInterval NormalSpeed (Time 17.666_666_666_666) 888 ioerr,
          abiInboundWriteFailure = Nothing,
          abiOutboundWriteFailure = Just 2,
          abiAcceptFailure = Nothing, abiSDUSize = LargeSDU
        }
    script =
      DiffusionScript
        (SimArgs 1 20)
        (singletonTimedScript $
           Map.fromList
             [ (("test2", DNS.A), Left [ (read "810b:4c8a:b3b5:741:8c0c:b437:64cf:1bd9", 300)
                                       , (read "254.167.216.215", 300)
                                       , (read "27.173.29.254", 300)
                                       , (read "61.238.34.238", 300)
                                       , (read "acda:b62d:6d7d:50f7:27b6:7e34:2dc6:ee3d", 300)
                                       ])
             , (("test3", DNS.A), Left [ (read "903e:61bc:8b2f:d98f:b16e:5471:c83d:4430", 300)
                                       , (read "19.40.90.161", 300)
                                       ])
             ])
        [(NodeArgs
            16
            InitiatorAndResponderDiffusionMode
            (Just 224)
            Map.empty
            PraosMode
            (Script (UseBootstrapPeers [RelayAccessDomain "bootstrap" 0] :| []))
            (TestAddress (IPAddr (read "0.0.1.236") 65_527))
            PeerSharingDisabled
            [ (2,2,Map.fromList [ (RelayAccessDomain "test2" 15,LocalRootConfig DoNotAdvertisePeer InitiatorAndResponderDiffusionMode IsNotTrustable)
                                , (RelayAccessDomain "test3" 4,LocalRootConfig DoAdvertisePeer InitiatorAndResponderDiffusionMode IsNotTrustable)])
            ]
            (Script (LedgerPools [] :| []))
            (PeerSelectionTargets
                { targetNumberOfRootPeers = 6,
                  targetNumberOfKnownPeers = 7,
                  targetNumberOfEstablishedPeers = 7,
                  targetNumberOfActivePeers = 6,

                  targetNumberOfKnownBigLedgerPeers = 0,
                  targetNumberOfEstablishedBigLedgerPeers = 0,
                  targetNumberOfActiveBigLedgerPeers = 0
                }
            , nullPeerSelectionTargets)
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
            , [ JoinNetwork 6.710_144_927_536
              , Kill 7.454_545_454_545
              , JoinNetwork 10.763_157_894_736
              , Reconfigure 0.415_384_615_384 [(1,1,Map.empty)
              , (1,1,Map.empty)]
              , Reconfigure 15.550_561_797_752 [(1,1,Map.empty)
              , (1,1,Map.fromList [(RelayAccessDomain "test2" 15,LocalRootConfig DoAdvertisePeer InitiatorAndResponderDiffusionMode IsNotTrustable)])]
              , Reconfigure 82.857_142_857_14 []
              ])
        ]


-- | Verify that some connect failures are fatal.
--
prop_connect_failure :: AbsIOError -> Property
prop_connect_failure (AbsIOError ioerr) =
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
         . -- verify that the node was not killed by the `IOError`
           all (\case
                   TrErrored {} -> False
                   _            -> True)
         . map snd
         $ evs
    ) noEvents absInfo script
  where
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
            naLocalRootPeers = [(1,1,Map.fromList [(RelayAccessAddress relayIP relayPort,LocalRootConfig DoNotAdvertisePeer InitiatorAndResponderDiffusionMode IsNotTrustable)])],
            naLedgerPeers = Script (LedgerPools [] :| []),
            naPeerTargets = (PeerSelectionTargets
              { targetNumberOfRootPeers = 1,
                targetNumberOfKnownPeers = 1,
                targetNumberOfEstablishedPeers = 1,
                targetNumberOfActivePeers = 1,

                targetNumberOfKnownBigLedgerPeers = 0,
                targetNumberOfEstablishedBigLedgerPeers = 0,
                targetNumberOfActiveBigLedgerPeers = 0
              }, nullPeerSelectionTargets),
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
            naPeerTargets = (PeerSelectionTargets
              { targetNumberOfRootPeers = 0,
                targetNumberOfKnownPeers = 0,
                targetNumberOfEstablishedPeers = 0,
                targetNumberOfActivePeers = 0,

                targetNumberOfKnownBigLedgerPeers = 0,
                targetNumberOfEstablishedBigLedgerPeers = 0,
                targetNumberOfActiveBigLedgerPeers = 0
              }, nullPeerSelectionTargets),
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
  label (if isFatalAccept ioerr then "fatal IOError" else "non-fatal IOError") $
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
         . (if isFatalAccept ioerr
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
    isFatalAccept :: IOError -> Bool
    isFatalAccept = not . Server.isECONNABORTED

    noEvents = short_trace
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
            naLocalRootPeers = [(1,1,Map.fromList [(RelayAccessAddress relayIP relayPort,LocalRootConfig DoNotAdvertisePeer InitiatorAndResponderDiffusionMode IsNotTrustable)])],
            naLedgerPeers = Script (LedgerPools [] :| []),
            naPeerTargets = (PeerSelectionTargets
              { targetNumberOfRootPeers = 1,
                targetNumberOfKnownPeers = 1,
                targetNumberOfEstablishedPeers = 1,
                targetNumberOfActivePeers = 1,

                targetNumberOfKnownBigLedgerPeers = 0,
                targetNumberOfEstablishedBigLedgerPeers = 0,
                targetNumberOfActiveBigLedgerPeers = 0
            }, nullPeerSelectionTargets),
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
            naPeerTargets = (PeerSelectionTargets
              { targetNumberOfRootPeers = 0,
                targetNumberOfKnownPeers = 0,
                targetNumberOfEstablishedPeers = 0,
                targetNumberOfActivePeers = 0,

                targetNumberOfKnownBigLedgerPeers = 0,
                targetNumberOfEstablishedBigLedgerPeers = 0,
                targetNumberOfActiveBigLedgerPeers = 0
              }, nullPeerSelectionTargets),
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
              (PublicRootPeers.toSet Cardano.ExtraPeers.toSet . Governor.publicRootPeers)
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

prop_diffusion_target_established_public_iosimpor
  :: AbsBearerInfo -> DiffusionScript -> Property
prop_diffusion_target_established_public_iosimpor
  = testWithIOSimPOR prop_diffusion_target_established_public short_trace

prop_diffusion_target_established_public_iosim
  :: AbsBearerInfo -> DiffusionScript -> Property
prop_diffusion_target_established_public_iosim
  = testWithIOSim prop_diffusion_target_established_public long_trace

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
                (PublicRootPeers.toSet Cardano.ExtraPeers.toSet . Governor.publicRootPeers)
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

prop_diffusion_target_active_public_iosimpor
  :: AbsBearerInfo -> DiffusionScript -> Property
prop_diffusion_target_active_public_iosimpor
  = testWithIOSimPOR prop_diffusion_target_active_public short_trace

prop_diffusion_target_active_public_iosim
  :: AbsBearerInfo -> DiffusionScript -> Property
prop_diffusion_target_active_public_iosim
  = testWithIOSim prop_diffusion_target_active_public long_trace


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

prop_diffusion_target_active_local_iosimpor
  :: AbsBearerInfo -> DiffusionScript -> Property
prop_diffusion_target_active_local_iosimpor
  = testWithIOSimPOR prop_diffusion_target_active_local short_trace

prop_diffusion_target_active_local_iosim
  :: AbsBearerInfo -> DiffusionScript -> Property
prop_diffusion_target_active_local_iosim
  = testWithIOSim prop_diffusion_target_active_local long_trace


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
                (PublicRootPeers.toSet Cardano.ExtraPeers.toSet . Governor.publicRootPeers) events

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

prop_diffusion_target_active_root_iosimpor
  :: AbsBearerInfo -> DiffusionScript -> Property
prop_diffusion_target_active_root_iosimpor
  = testWithIOSimPOR prop_diffusion_target_active_root short_trace

prop_diffusion_target_active_root_iosim
  :: AbsBearerInfo -> DiffusionScript -> Property
prop_diffusion_target_active_root_iosim
  = testWithIOSim prop_diffusion_target_active_root long_trace


-- | This test checks the percentage of public root peers that, at some point,
-- become active, when using the 'HotDiffusionScript' generator.
--
prop_hot_diffusion_target_active_public :: NonFailingAbsBearerInfo
                                        -> HotDiffusionScript
                                        -> Property
prop_hot_diffusion_target_active_public defaultBearerInfo (HotDiffusionScript sa dns hds) =
  testWithIOSim prop_diffusion_target_active_public long_trace (unNFBI defaultBearerInfo) (DiffusionScript sa dns hds)

-- | This test checks the percentage of local root peers that, at some point,
-- become active, when using the 'HotDiffusionScript' generator.
--
prop_hot_diffusion_target_active_local :: NonFailingAbsBearerInfo
                                       -> HotDiffusionScript
                                       -> Property
prop_hot_diffusion_target_active_local defaultBearerInfo (HotDiffusionScript sa dns hds) =
  testWithIOSim prop_diffusion_target_active_local long_trace (unNFBI defaultBearerInfo) (DiffusionScript sa dns hds)

-- | This test checks the percentage of root peers that, at some point,
-- become active, when using the 'HotDiffusionScript' generator.
--
prop_hot_diffusion_target_active_root :: NonFailingAbsBearerInfo
                                      -> HotDiffusionScript
                                      -> Property
prop_hot_diffusion_target_active_root defaultBearerInfo (HotDiffusionScript sa dns hds) =
  testWithIOSim prop_diffusion_target_active_root long_trace (unNFBI defaultBearerInfo) (DiffusionScript sa dns hds)

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
      let govLocalRootPeersSig :: Signal (LocalRootPeers.LocalRootPeers PeerTrustable NtNAddr)
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
              Signal.fromChangeEvents Terminated -- Default to TrKillingNode
            . Signal.selectEvents
                (\case TrJoiningNetwork -> Just Joined
                       TrKillingNode    -> Just Terminated
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
            <$> Signal.keyedUntil (fromJoinedOrTerminated (Set.singleton ())
                                                           Set.empty)
                                  (fromJoinedOrTerminated  Set.empty
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
               List.intercalate "\n" (map show $ eventsToList events)
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

prop_diffusion_target_established_local_iosimpor
  :: AbsBearerInfo -> DiffusionScript -> Property
prop_diffusion_target_established_local_iosimpor
  = testWithIOSimPOR prop_diffusion_target_established_local short_trace

prop_diffusion_target_established_local_iosim
  :: AbsBearerInfo -> DiffusionScript -> Property
prop_diffusion_target_established_local_iosim
  = testWithIOSim prop_diffusion_target_established_local long_trace


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
      let govLocalRootPeersSig :: Signal (LocalRootPeers.LocalRootPeers PeerTrustable NtNAddr)
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
              Signal.fromChangeEvents Terminated -- Default to TrKillingNode
            . Signal.selectEvents
                (\case TrJoiningNetwork -> Just Joined
                       TrTerminated     -> Just Terminated
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
            <$> Signal.keyedUntil (fromJoinedOrTerminated (Set.singleton ())
                                                           Set.empty)
                                  (fromJoinedOrTerminated  Set.empty
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
            (List.intercalate "\n" $ map show $ Signal.eventsToList events) $

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

prop_diffusion_target_active_below_iosimpor
  :: AbsBearerInfo -> DiffusionScript -> Property
prop_diffusion_target_active_below_iosimpor
  = testWithIOSimPOR prop_diffusion_target_active_below short_trace

prop_diffusion_target_active_below_iosim
  :: AbsBearerInfo -> DiffusionScript -> Property
prop_diffusion_target_active_below_iosim
  = testWithIOSim prop_diffusion_target_active_below long_trace


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
      let govLocalRootPeersSig :: Signal (LocalRootPeers.LocalRootPeers PeerTrustable NtNAddr)
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
              Signal.fromChangeEvents Terminated -- Default to TrKillingNode
            . Signal.selectEvents
                    (\case DiffusionSimulationTrace TrJoiningNetwork
                            -> Just Joined
                           DiffusionSimulationTrace TrTerminated
                            -> Just Terminated
                           DiffusionConnectionManagerTrace CM.TrShutdown
                            -> Just Terminated
                           _ -> Nothing
                    )
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
            <$> Signal.keyedUntil (fromJoinedOrTerminated (Set.singleton ())
                                                           Set.empty)
                                  (fromJoinedOrTerminated  Set.empty
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
            (List.intercalate "\n" $ map show $ Signal.eventsToList events) $

          signalProperty 20 show
            (\(_,_,_,_,_,toolong) -> Set.null toolong)
            ((,,,,,) <$> (LocalRootPeers.toGroupSets <$> govLocalRootPeersSig)
                     <*> govEstablishedPeersSig
                     <*> govActivePeersSig
                     <*> govActiveFailuresSig
                     <*> promotionOpportunities
                     <*> promotionOpportunitiesIgnoredTooLong)

prop_diffusion_target_active_local_below_iosimpor
  :: AbsBearerInfo -> DiffusionScript -> Property
prop_diffusion_target_active_local_below_iosimpor
  = testWithIOSimPOR prop_diffusion_target_active_local_below short_trace

prop_diffusion_target_active_local_below_iosim
  :: AbsBearerInfo -> DiffusionScript -> Property
prop_diffusion_target_active_local_below_iosim
  = testWithIOSim prop_diffusion_target_active_local_below very_long_trace


--_iosim
--
async_demotion_network_script :: DiffusionScript
async_demotion_network_script =
    DiffusionScript
      simArgs
      (singletonTimedScript Map.empty)
      [ ( common { naAddr                  = addr1,
                   naLocalRootPeers        = localRoots1,
                   naPeerTargets =
                     (Governor.nullPeerSelectionTargets {
                         targetNumberOfKnownPeers = 2,
                           targetNumberOfEstablishedPeers = 2,
                           targetNumberOfActivePeers = 2
                         }
                     , peerTargets)
                 }
        , [ JoinNetwork 0
            -- reconfigure the peer to trigger the outbound governor log
          , Reconfigure 240 localRoots1'
          ]
        )
      , ( common { naAddr           = addr2,
                   naLocalRootPeers = [(1,1, Map.fromList [(ra_addr1, LocalRootConfig DoNotAdvertisePeer InitiatorAndResponderDiffusionMode IsNotTrustable)])] }
        , [JoinNetwork 0, Kill 5, JoinNetwork 20]
        )
      , ( common { naAddr           = addr3,
                   naLocalRootPeers = [(1,1, Map.fromList [(ra_addr1, LocalRootConfig DoNotAdvertisePeer InitiatorAndResponderDiffusionMode IsNotTrustable)])] }
        , [JoinNetwork 0]
        )
      ]
  where
    addr1    = TestAddress (IPAddr (read "10.0.0.1") 3000)
    ra_addr1 = RelayAccessAddress (read "10.0.0.1") 3000
    localRoots1  = [(2,2, Map.fromList [(ra_addr2, LocalRootConfig DoNotAdvertisePeer InitiatorAndResponderDiffusionMode IsNotTrustable)
                                       ,(ra_addr3, LocalRootConfig DoNotAdvertisePeer InitiatorAndResponderDiffusionMode IsNotTrustable)])]
    localRoots1' = [(2,2, Map.fromList [(ra_addr2, LocalRootConfig DoAdvertisePeer InitiatorAndResponderDiffusionMode IsNotTrustable)
                                       ,(ra_addr3, LocalRootConfig DoAdvertisePeer InitiatorAndResponderDiffusionMode IsNotTrustable)])]

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
        naBootstrapPeers   = Script (UseBootstrapPeers [RelayAccessDomain "bootstrap" 0] :| []),
        naAddr             = undefined,
        naLocalRootPeers   = undefined,
        naLedgerPeers      = Script (LedgerPools [] :| []),
        naPeerTargets      = (peerTargets, peerTargets),
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
                          failures = Map.keysSet (Map.filter (\case
                                                                 (PeerCold, _   ) -> True
                                                                 (PeerCooling, _) -> True
                                                                 _                -> False
                                                             ) status)
                      TraceDemoteBigLedgerPeersAsynchronous status ->
                          Just $ Stop failures
                        where
                          failures = Map.keysSet (Map.filter (\case
                                                                 (PeerCold, _   ) -> True
                                                                 (PeerCooling, _) -> True
                                                                 _                -> False
                                                             ) status)
                      TraceDemoteLocalAsynchronous status ->
                          Just $ Stop failures
                        where
                          failures = Map.keysSet (Map.filter (\case
                                                                 (PeerCold, _   ) -> True
                                                                 (PeerCooling, _) -> True
                                                                 _                -> False
                                                             ) status)
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
              Signal.keyedTimeout 10 id demotionOpportunities

      in signalProperty
            20 show Set.null
            demotionOpportunitiesTooLong

prop_diffusion_async_demotions_iosimpor
  :: AbsBearerInfo -> DiffusionScript -> Property
prop_diffusion_async_demotions_iosimpor
  = testWithIOSimPOR prop_diffusion_async_demotions short_trace

prop_diffusion_async_demotions_iosim
  :: AbsBearerInfo -> DiffusionScript -> Property
prop_diffusion_async_demotions_iosim
  = testWithIOSim prop_diffusion_async_demotions long_trace


unit_diffusion_async_demotions :: Property
unit_diffusion_async_demotions =
    testWithIOSim
      prop_diffusion_async_demotions
      long_trace
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
      let govLocalRootPeersSig :: Signal (LocalRootPeers.LocalRootPeers PeerTrustable NtNAddr)
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
              Signal.fromChangeEvents Terminated -- Default to TrKillingNode
            . Signal.selectEvents
                (\case TrJoiningNetwork -> Just Joined
                       TrTerminated     -> Just Terminated
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
            <$> Signal.keyedUntil (fromJoinedOrTerminated (Set.singleton ())
                                                           Set.empty)
                                  (fromJoinedOrTerminated  Set.empty
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
          counterexample (List.intercalate "\n" $ map show $ Signal.eventsToList events) $

          signalProperty 20 show
            (\(_,_,_,_,toolong) -> Set.null toolong)
            ((,,,,) <$> (LocalRootPeers.toGroupSets <$> govLocalRootPeersSig)
                    <*> govActivePeersSig
                    <*> trIsNodeAlive
                    <*> demotionOpportunities
                    <*> demotionOpportunitiesIgnoredTooLong)

prop_diffusion_target_active_local_above_iosimpor
  :: AbsBearerInfo -> DiffusionScript -> Property
prop_diffusion_target_active_local_above_iosimpor
  = testWithIOSimPOR prop_diffusion_target_active_local_above short_trace

prop_diffusion_target_active_local_above_iosim
  :: AbsBearerInfo -> DiffusionScript -> Property
prop_diffusion_target_active_local_above_iosim
  = testWithIOSim prop_diffusion_target_active_local_above long_trace


-- | A variant of ouroboros-network-framework
-- 'Test.Ouroboros.Network.Server.prop_connection_manager_valid_transitions'
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
      let abstractTransitionEvents :: Trace () (AbstractTransitionTrace CM.ConnStateId)
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
                       ++ List.intercalate "\n" (map ppTransition trs))
                       )
                   . foldMap ( \ tr
                              -> Every
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

prop_diffusion_cm_valid_transitions_iosimpor
  :: AbsBearerInfo -> DiffusionScript -> Property
prop_diffusion_cm_valid_transitions_iosimpor
  = testWithIOSimPOR prop_diffusion_cm_valid_transitions short_trace

prop_diffusion_cm_valid_transitions_iosim
  :: AbsBearerInfo -> DiffusionScript -> Property
prop_diffusion_cm_valid_transitions_iosim
  = testWithIOSim prop_diffusion_cm_valid_transitions long_trace


-- | A variant of ouroboros-network-framework
-- 'Test.Ouroboros.Network.Server.prop_connection_manager_valid_transition_order'
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
prop_diffusion_cm_valid_transition_order' :: SimTrace Void
                                          -> Int
                                          -> Property
prop_diffusion_cm_valid_transition_order' ioSimTrace traceNumber =
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
         in counterexample (List.intercalate "\n" $ map show $ Trace.toList ev)
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
          (verifyAbstractTransitionOrder id False)
       . fmap (map ttTransition)
       . groupConns id abstractStateIsFinalTransitionTVarTracing

prop_diffusion_cm_valid_transition_order_iosimpor
  :: AbsBearerInfo -> DiffusionScript -> Property
prop_diffusion_cm_valid_transition_order_iosimpor
  = testWithIOSimPOR prop_diffusion_cm_valid_transition_order' short_trace


-- | A variant of ouroboros-network-framework
-- 'Test.Ouroboros.Network.Server.prop_connection_manager_valid_transition_order'
-- but for running on Diffusion. This means it has to have in consideration the
-- the logs for all nodes running will all appear in the trace and the test
-- property should only be valid while a given node is up and running.
--
prop_diffusion_cm_valid_transition_order'' :: SimTrace Void
                                           -> Int
                                           -> Property
prop_diffusion_cm_valid_transition_order'' ioSimTrace traceNumber =
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
          . classifyNumberOfEvents (length evsList)
          . verify_cm_valid_transition_order
          $ ev
        )
      <$> events
  where
    verify_cm_valid_transition_order :: Trace () (WithName NtNAddr (WithTime DiffusionTestTrace)) -> Property
    verify_cm_valid_transition_order events =
      let abstractTransitionEvents :: Trace () (WithName NtNAddr (WithTime (AbstractTransitionTrace CM.ConnStateId)))
          abstractTransitionEvents =
            selectDiffusionConnectionManagerTransitionEvents' events

       in  property
         . bifoldMap
            (const mempty)
            (verifyAbstractTransitionOrder (wtEvent . wnEvent) False)
         . fmap (map (fmap (fmap ttTransition)))
         . groupConns (wtEvent . wnEvent) abstractStateIsFinalTransition
         $ abstractTransitionEvents

prop_diffusion_cm_valid_transition_order_iosim
  :: AbsBearerInfo -> DiffusionScript -> Property
prop_diffusion_cm_valid_transition_order_iosim
  = testWithIOSim prop_diffusion_cm_valid_transition_order'' long_trace


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
             (Script (UseBootstrapPeers [RelayAccessDomain "bootstrap" 0] :| []))
             (TestAddress (IPAddr (read "0.0.0.4") 9))
             PeerSharingDisabled
             [(1,1,Map.fromList [(RelayAccessAddress "0.0.0.8" 65_531,LocalRootConfig DoNotAdvertisePeer InitiatorAndResponderDiffusionMode IsNotTrustable)])]
             (Script (LedgerPools [] :| []))
             (nullPeerSelectionTargets {
                 targetNumberOfRootPeers = 2,
                 targetNumberOfKnownPeers = 5,
                 targetNumberOfEstablishedPeers = 4,
                 targetNumberOfActivePeers = 1 }
             , nullPeerSelectionTargets)
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
         , [ JoinNetwork 4.166_666_666_666,
             Kill 0.3,
             JoinNetwork 1.517_857_142_857,
             Reconfigure 0.245_238_095_238 [],
             Reconfigure 4.190_476_190_476 []
           ]
         ),
         ( NodeArgs (-5) InitiatorAndResponderDiffusionMode (Just 269)
             (Map.fromList [(RelayAccessAddress "0.0.0.4" 9, DoAdvertisePeer)])
             PraosMode
             (Script (UseBootstrapPeers [RelayAccessDomain "bootstrap" 0] :| []))
             (TestAddress (IPAddr (read "0.0.0.8") 65_531))
             PeerSharingDisabled
             [(1,1,Map.fromList [(RelayAccessAddress "0.0.0.4" 9,LocalRootConfig DoNotAdvertisePeer InitiatorAndResponderDiffusionMode IsNotTrustable)])]
             (Script (LedgerPools [] :| []))
             (nullPeerSelectionTargets {
                 targetNumberOfRootPeers = 4,
                 targetNumberOfKnownPeers = 5,
                 targetNumberOfEstablishedPeers = 3,
                 targetNumberOfActivePeers = 1,

                 targetNumberOfKnownBigLedgerPeers = 0,
                 targetNumberOfEstablishedBigLedgerPeers = 0,
                 targetNumberOfActiveBigLedgerPeers = 0
               }
             , nullPeerSelectionTargets)
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
         , [ JoinNetwork 3.384_615_384_615,
             Reconfigure 3.583_333_333_333 [(1,1,Map.fromList [(RelayAccessAddress "0.0.0.4" 9,LocalRootConfig DoNotAdvertisePeer InitiatorAndResponderDiffusionMode IsNotTrustable)])],
             Kill 15.555_555_555_55,
             JoinNetwork 30.533_333_333_33,
             Kill 71.111_111_111_11
            ]
         )]
   in prop_diffusion_cm_valid_transition_order_iosim bearerInfo diffScript

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
-- patched to explore more schedules or IOSim is made more efficient) add long_trace
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
              [ (2,2,Map.fromList [ (RelayAccessAddress "0.0.0.1" 0,LocalRootConfig DoNotAdvertisePeer InitiatorAndResponderDiffusionMode IsNotTrustable)
                                  , (RelayAccessAddress "0.0.0.2" 0,LocalRootConfig DoNotAdvertisePeer InitiatorAndResponderDiffusionMode IsNotTrustable)
                                  ])
              ]
              (Script (LedgerPools [] :| []))
              (PeerSelectionTargets {
                    targetNumberOfRootPeers = 1,
                    targetNumberOfKnownPeers = 1,
                    targetNumberOfEstablishedPeers = 1,
                    targetNumberOfActivePeers = 1,

                    targetNumberOfKnownBigLedgerPeers = 0,
                    targetNumberOfEstablishedBigLedgerPeers = 0,
                    targetNumberOfActiveBigLedgerPeers = 0 }
              , nullPeerSelectionTargets)
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
               [(1,1,Map.fromList [(RelayAccessAddress "0.0.0.0" 0,LocalRootConfig DoNotAdvertisePeer InitiatorAndResponderDiffusionMode IsNotTrustable)])]
               (Script (LedgerPools [] :| []))
               (PeerSelectionTargets {
                     targetNumberOfRootPeers = 1,
                     targetNumberOfKnownPeers = 1,
                     targetNumberOfEstablishedPeers = 1,
                     targetNumberOfActivePeers = 1,

                     targetNumberOfKnownBigLedgerPeers = 0,
                     targetNumberOfEstablishedBigLedgerPeers = 0,
                     targetNumberOfActiveBigLedgerPeers = 0 }
               , nullPeerSelectionTargets)
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

      events :: [Events (WithName NtNAddr DiffusionTestTrace)]
      events = Trace.toList
             . fmap ( Signal.eventsFromList
                    . fmap (\(WithName addr (WithTime t b)) -> (t, WithName addr b))
                    )
             . splitWithNameTrace
             . fmap (\(WithTime t (WithName name b)) -> WithName name (WithTime t b))
             . withTimeNameTraceEvents
                @DiffusionTestTrace
                @NtNAddr
             . Trace.take long_trace
             $ runSimTrace sim

   in conjoin
    $ verify_consistency
   <$> events

  where
    verify_consistency :: Events (WithName NtNAddr DiffusionTestTrace) -> Property
    verify_consistency events =
      let govEstablishedPeersSig :: Signal (Set NtNAddr)
          govEstablishedPeersSig =
            selectDiffusionPeerSelectionState'
              (EstablishedPeers.toSet . Governor.establishedPeers)
              (wnEvent <$> events)

          govConnectionManagerTransitionsSig :: [E (WithName NtNAddr (AbstractTransitionTrace CM.ConnStateId))]
          govConnectionManagerTransitionsSig =
              Signal.eventsToListWithId
            $ Signal.selectEvents
                (\case
                   WithName addr (DiffusionConnectionManagerTransitionTrace tr)
                     -> Just (WithName addr tr)
                   _ -> Nothing
                ) events

       in conjoin
        $ map (\(E ts (WithName addr a)) -> case a of
                TransitionTrace _ (Transition _ TerminatedSt) ->
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

prop_diffusion_cm_no_dodgy_traces_iosimpor
  :: AbsBearerInfo -> DiffusionScript -> Property
prop_diffusion_cm_no_dodgy_traces_iosimpor
  = testWithIOSimPOR prop_diffusion_cm_no_dodgy_traces short_trace

prop_diffusion_cm_no_dodgy_traces_iosim
  :: AbsBearerInfo -> DiffusionScript -> Property
prop_diffusion_cm_no_dodgy_traces_iosim
  = testWithIOSim prop_diffusion_cm_no_dodgy_traces long_trace


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
         conjoin (zipWith (curry (\case
             ev@( WithTime _ (PeerStatusChangeFailure (HotToWarm _) TimeoutError)
                , WithTime _ (PeerStatusChangeFailure (HotToWarm _) ActiveCold)
                )
               -> counterexample (show ev)
                $ counterexample (unlines $ map show peerSelectionActionsEvents)
                  False
             _ -> property True
             )) peerSelectionActionsEvents (tail peerSelectionActionsEvents))
         .&&.
         ( let f :: [WithTime (PeerSelectionActionsTrace NtNAddr NtNVersion)] -> Property
               f as = conjoin $ g <$> List.tails as

               g :: [WithTime (PeerSelectionActionsTrace NtNAddr NtNVersion)] -> Property
               g as@(WithTime demotionTime (PeerStatusChanged HotToCooling{}) : as') =
                 case List.find
                           (\case
                               WithTime _ (PeerStatusChanged ColdToWarm{}) -> True
                               _ -> False)
                           as' of

                   Nothing                         -> property True
                   Just (WithTime promotionTime _) -> counterexample (show as)
                                                      ( promotionTime `diffTime` demotionTime
                                                     >= repromoteDelay config_REPROMOTE_DELAY
                                                      )
               g as@(WithTime demotionTime (PeerStatusChanged WarmToCooling{}) : as') =
                 case List.find
                           (\case
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

prop_diffusion_peer_selection_actions_no_dodgy_traces_iosimpor
  :: AbsBearerInfo -> DiffusionScript -> Property
prop_diffusion_peer_selection_actions_no_dodgy_traces_iosimpor
  = testWithIOSimPOR prop_diffusion_peer_selection_actions_no_dodgy_traces short_trace

prop_diffusion_peer_selection_actions_no_dodgy_traces_iosim
  :: AbsBearerInfo -> DiffusionScript -> Property
prop_diffusion_peer_selection_actions_no_dodgy_traces_iosim
  = testWithIOSim prop_diffusion_peer_selection_actions_no_dodgy_traces long_trace


unit_peer_sharing :: Property
unit_peer_sharing =
    let sim :: forall s. IOSim s Void
        sim = diffusionSimulation (toBearerInfo absNoAttenuation)
                                  script
                                  iosimTracer

        events :: Map NtNAddr [TracePeerSelection Cardano.ExtraState PeerTrustable (Cardano.ExtraPeers NtNAddr) NtNAddr]
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
               -> [TracePeerSelection extraDebugState extraFlags extraPeers NtNAddr]
               -> Every
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
          in Every $
             counterexample (concat [ show ip_0
                                    , " is not a member of received peers "
                                    , show receivedPeers
                                    ]) $
             ip_0 `Set.member` receivedPeers
        verify _ _ = Every True

    in
      -- counterexample (ppEvents trace) $
      counterexample (Map.foldrWithKey (\addr evs s -> concat [ "\n\n===== "
                                                              , show addr
                                                              , " =====\n\n"
                                                              ]
                                                          ++ List.intercalate "\n" (map show evs)
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
                in (t, t)


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
        naPeerTargets = (nullPeerSelectionTargets, nullPeerSelectionTargets),
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
                                     naLocalRootPeers = [(1, 1, Map.fromList [(ra_1, LocalRootConfig DoNotAdvertisePeer InitiatorAndResponderDiffusionMode IsNotTrustable)])],
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
                                     naLocalRootPeers = [(1, 1, Map.fromList [(ra_1, LocalRootConfig DoNotAdvertisePeer InitiatorAndResponderDiffusionMode IsNotTrustable)])],
                                     naPeerTargets = targets 2
                                   }
                 , [JoinNetwork 0]
                 )
               ]


-- | This property verifies that when nodes are running without network
-- attenuation, decreasing numbers by churn never timeouts.
--
--
-- This test revealed a scenario where the governor can become blocked due to
-- the way `EstablishedPeers` enforces connection constraints. Specifically,
-- each `EstablishedPeers` action has a guard that prevents further peer
-- selection until a new connection can be established. In a real-world
-- network with many peers, this is unlikely to be an issue. However, in our
-- controlled test environment with a limited number of peers, all of them may
-- be in a timeout state. Because the governor selects the peer with the
-- minimum `connectTime`, and exponential backoff increases this value
-- significantly after multiple failed attempts, the governor can end up
-- blocking for an extended period (e.g., ~150s). This blocking behavior
-- disrupts churn, as the governor cannot proceed with peer selection while
-- waiting. To address this, the test takes into consideration when such a
-- blocking occurs and identifies whether the governor is stuck due to a high
-- `minConnectTime`, and bypass the churn timeout in such cases. While this is
-- a workaround, it ensures the test remains meaningful without being
-- invalidated by an artificial limitation of the test environment.
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
            let psSig :: Signal (TracePeerSelection Cardano.ExtraState PeerTrustable (Cardano.ExtraPeers NtNAddr) NtNAddr)
                psSig = fromChangeEvents TraceGovernorWakeup (selectDiffusionPeerSelectionEvents evs)

                -- We have 'True' when the governor is blocked
                -- waiting for peers to be available to connect.
                isGovernorStuck :: Signal Bool
                isGovernorStuck = fmap (not . Set.null)
                                . keyedLinger'
                                    (\d -> if d > 0
                                          then (Set.singleton (), d)
                                          else (Set.empty, d)
                                    )
                                . Signal.fromChangeEvents 0
                                . Signal.selectEvents
                                   (\case
                                       DiffusionDebugPeerSelectionTrace (TraceGovernorState _ (Just dt) _)
                                         -> Just dt
                                       _ -> Nothing)
                                $ evs

                -- We can't decrease peers that are in progress sets
                noChurnTimeoutSig :: Signal Bool
                noChurnTimeoutSig =
                  (\peerSelection ->
                    case peerSelection of
                      TraceChurnTimeout _ DecreasedActivePeers _               -> False
                      TraceChurnTimeout _ DecreasedActiveBigLedgerPeers _      -> False
                      TraceChurnTimeout _ DecreasedEstablishedPeers _          -> False
                      TraceChurnTimeout _ DecreasedEstablishedBigLedgerPeers _ -> False
                      TraceChurnTimeout _ DecreasedKnownPeers _                -> False
                      TraceChurnTimeout _ DecreasedKnownBigLedgerPeers _       -> False
                      _                                                        -> True
                  ) <$> psSig

            in signalProperty 20 show
                  (\(churnTimeout, governorStuck)
                      -> governorStuck || churnTimeout)
                  ((,) <$> noChurnTimeoutSig <*> isGovernorStuck)
        )
     <$> events

prop_churn_notimeouts_iosimpor
  :: AbsBearerInfo -> DiffusionScript -> Property
prop_churn_notimeouts_iosimpor
  = testWithIOSimPOR prop_churn_notimeouts short_trace

prop_churn_notimeouts_iosim
  :: AbsBearerInfo -> DiffusionScript -> Property
prop_churn_notimeouts_iosim
  = testWithIOSim prop_churn_notimeouts long_trace


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
           let evsList :: [(Time, TracePeerSelection Cardano.ExtraState PeerTrustable (Cardano.ExtraPeers NtNAddr) NtNAddr)]
               evsList = eventsToList (selectDiffusionPeerSelectionEvents evs)
           in  counterexample (List.intercalate "\n" (show <$> evsList))
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
        all (uncurry (==))
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

prop_churn_steps_iosimpor
  :: AbsBearerInfo -> DiffusionScript -> Property
prop_churn_steps_iosimpor
  = testWithIOSimPOR prop_churn_steps short_trace

prop_churn_steps_iosim
  :: AbsBearerInfo -> DiffusionScript -> Property
prop_churn_steps_iosim
  = testWithIOSim prop_churn_steps 5000



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
-- 'Test.Ouroboros.Network.Server.prop_inbound_governor_valid_transitions'
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
            ( \ _ -> Every True )
            ( \ TransitionTrace {ttPeerAddr = peerAddr, ttTransition = tr} ->
                  Every
                . counterexample (concat [ "Unexpected transition: "
                                         , show peerAddr
                                         , " "
                                         , show tr
                                         ])
                . verifyRemoteTransition
                $ tr
            )
         $ remoteTransitionTraceEvents

prop_diffusion_ig_valid_transitions_iosimpor
  :: AbsBearerInfo -> DiffusionScript -> Property
prop_diffusion_ig_valid_transitions_iosimpor
  = testWithIOSimPOR prop_diffusion_ig_valid_transitions short_trace

prop_diffusion_ig_valid_transitions_iosim
  :: AbsBearerInfo -> DiffusionScript -> Property
prop_diffusion_ig_valid_transitions_iosim
  = testWithIOSim prop_diffusion_ig_valid_transitions long_trace


-- | A variant of ouroboros-network-framework
-- 'Test.Ouroboros.Network.Server.prop_inbound_governor_valid_transition_order'
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

prop_diffusion_ig_valid_transition_order_iosimpor
  :: AbsBearerInfo -> DiffusionScript -> Property
prop_diffusion_ig_valid_transition_order_iosimpor
  = testWithIOSimPOR prop_diffusion_ig_valid_transition_order short_trace

prop_diffusion_ig_valid_transition_order_iosim
  :: AbsBearerInfo -> DiffusionScript -> Property
prop_diffusion_ig_valid_transition_order_iosim
  = testWithIOSim prop_diffusion_ig_valid_transition_order long_trace


-- | A variant of ouroboros-network-framework
-- 'Test.Ouroboros.Network.Server.prop_timeouts_enforced'
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
      let transitionSignal :: Trace (SimResult ()) [(Time, AbstractTransitionTrace CM.ConnStateId)]
          transitionSignal = Trace.fromList (MainReturn (Time 0) (Labelled (ThreadId []) (Just "main")) () [])
                           . Trace.toList
                           . groupConns snd abstractStateIsFinalTransition
                           . selectDiffusionConnectionManagerTransitionEventsTime
                           $ events

       in property
        $ verifyAllTimeouts True transitionSignal

prop_diffusion_timeouts_enforced_iosimpor
  :: AbsBearerInfo -> DiffusionScript -> Property
prop_diffusion_timeouts_enforced_iosimpor
  = testWithIOSimPOR prop_diffusion_timeouts_enforced short_trace

prop_diffusion_timeouts_enforced_iosim
  :: AbsBearerInfo -> DiffusionScript -> Property
prop_diffusion_timeouts_enforced_iosim
  = testWithIOSim prop_diffusion_timeouts_enforced long_trace


newtype ArbDiffusionMode = ArbDiffusionMode { getDiffusionMode :: DiffusionMode }
  deriving (Eq, Show)

-- | Verify that local root can negotiate the right diffusion mode.
--
unit_local_root_diffusion_mode :: DiffusionMode
                               -> Property
unit_local_root_diffusion_mode diffusionMode =
    -- this is a unit test
    withMaxSuccess 1 $
    let sim = diffusionSimulation (toBearerInfo absNoAttenuation) script iosimTracer

        -- list of negotiated version data
        events :: [NtNVersionData]
        events =
            mapMaybe (\case
                       DiffusionConnectionManagerTrace (CM.TrConnectionHandler ConnectionId { remoteAddress } (TrHandshakeSuccess _ versionData))
                         | remoteAddress == addr'
                         -> Just versionData
                       _ -> Nothing
                     )
          . fmap wnEvent
          . filter (\WithName { wnName } -> wnName == addr)
          . fmap wtEvent
          . Trace.toList
          . withTimeNameTraceEvents
              @DiffusionTestTrace
              @NtNAddr
          . Trace.take long_trace
          $ runSimTrace sim
    in property $ foldMap (\versionData -> Every $ ntnDiffusionMode versionData === diffusionMode) events
  where
    addr, addr' :: NtNAddr
    addr  = TestAddress (IPAddr (read "127.0.0.2") 1000)
    addr' = TestAddress (IPAddr (read "127.0.0.1") 1000)

    script =
      DiffusionScript
        (SimArgs 1 20)
        (singletonTimedScript Map.empty)
        [ -- a relay node
          (NodeArgs {
             naSeed = 0,
             naDiffusionMode = InitiatorAndResponderDiffusionMode,
             naMbTime = Just 224,
             naPublicRoots = Map.empty,
             naConsensusMode = PraosMode,
             naBootstrapPeers = Script (DontUseBootstrapPeers :| []),
             naAddr = addr',
             naPeerSharing = PeerSharingDisabled,
             naLocalRootPeers = [],
             naLedgerPeers = Script (LedgerPools [] :| []),
             naPeerTargets = (PeerSelectionTargets
               { targetNumberOfRootPeers = 1,
                 targetNumberOfKnownPeers = 1,
                 targetNumberOfEstablishedPeers = 0,
                 targetNumberOfActivePeers = 0,

                 targetNumberOfKnownBigLedgerPeers = 0,
                 targetNumberOfEstablishedBigLedgerPeers = 0,
                 targetNumberOfActiveBigLedgerPeers = 0
               }, nullPeerSelectionTargets),
             naDNSTimeoutScript = Script (DNSTimeout {getDNSTimeout = 1} :| []),
             naDNSLookupDelayScript = Script (DNSLookupDelay {getDNSLookupDelay = 0.1} :| []),
             naChainSyncExitOnBlockNo = Nothing,
             naChainSyncEarlyExit = False,
             naFetchModeScript = Script (FetchModeDeadline :| [])
           }
          , [JoinNetwork 0]
          )
        , -- a relay, which has the BP as a local root
          (NodeArgs {
             naSeed = 0,
             naDiffusionMode = InitiatorAndResponderDiffusionMode,
             naMbTime = Just 224,
             naPublicRoots = Map.empty,
             naConsensusMode = PraosMode,
             naBootstrapPeers = Script (DontUseBootstrapPeers :| []),
             naAddr = addr,
             naPeerSharing = PeerSharingDisabled,
             naLocalRootPeers =
               [ (1,1,Map.fromList [ (RelayAccessAddress (read "127.0.0.1") 1000,
                                      LocalRootConfig DoNotAdvertisePeer diffusionMode IsNotTrustable)
                                   ])
               ],
             naLedgerPeers = Script (LedgerPools [] :| []),
             naPeerTargets = (PeerSelectionTargets
               { targetNumberOfRootPeers = 6,
                 targetNumberOfKnownPeers = 7,
                 targetNumberOfEstablishedPeers = 7,
                 targetNumberOfActivePeers = 6,

                 targetNumberOfKnownBigLedgerPeers = 0,
                 targetNumberOfEstablishedBigLedgerPeers = 0,
                 targetNumberOfActiveBigLedgerPeers = 0
               }, nullPeerSelectionTargets),
             naDNSTimeoutScript = Script (DNSTimeout {getDNSTimeout = 1} :| []),
             naDNSLookupDelayScript = Script (DNSLookupDelay {getDNSLookupDelay = 0.1} :| []),
             naChainSyncExitOnBlockNo = Nothing,
             naChainSyncEarlyExit = False,
             naFetchModeScript = Script (FetchModeDeadline :| [])
           }
          , [JoinNetwork 0]
          )
        ]

prop_no_peershare_unwilling:: SimTrace Void
                           -> Int
                           -> Property
prop_no_peershare_unwilling ioSimTrace traceNumber =
  let events = Trace.toList
             . fmap (\(WithTime t (WithName _ b)) -> (t, b))
             . withTimeNameTraceEvents
                @DiffusionTestTrace
                @NtNAddr
             . Trace.take traceNumber
             $ ioSimTrace
  in  counterexample (List.intercalate "\n" $ map show events)
    $ foldMap
        (\case
          (_, DiffusionInboundGovernorTrace (IG.TrMuxErrored _ err)) ->
            case fromException err of
              -- Technically we fail on more than the peersharing protocol.
              -- Which is fine.
              Just (Mx.UnknownMiniProtocol num) -> Every
                                                 $ counterexample (show num) False
              Just _                            -> Every True
              Nothing                           -> Every True
          _                                     -> Every True
          )
          events

prop_no_peershare_unwilling_iosim
  :: AbsBearerInfo -> DiffusionScript -> Property
prop_no_peershare_unwilling_iosim
  = testWithIOSim prop_no_peershare_unwilling long_trace


--
-- Utils
--

data JoinedOrKilled = Joined | Terminated
  deriving (Eq, Show)

-- Similar to 'either' but for 'JoinedOrKilled'
fromJoinedOrTerminated :: c -> c -> JoinedOrKilled -> c
fromJoinedOrTerminated j _ Joined     = j
fromJoinedOrTerminated _ k Terminated = k

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
        classify (nEvents <=    100) "Nº Events <=    100"
      . classify (nEvents >=  1_000) "Nº Events >=  1_000"
      . classify (nEvents >= 10_000) "Nº Events >= 10_000"
      . classify (nEvents >= 50_000) "Nº Events >= 50_000"

withTimeNameTraceEvents :: forall b name r. (Typeable b, Typeable name)
                        => Trace r SimEvent
                        -> Trace r (WithTime (WithName name b))
withTimeNameTraceEvents = traceSelectTraceEventsDynamic
                            @r
                            @(WithTime (WithName name b))

selectDiffusionPeerSelectionEvents :: Events DiffusionTestTrace
                                   -> Events (TracePeerSelection Cardano.ExtraState PeerTrustable (Cardano.ExtraPeers NtNAddr) NtNAddr)
selectDiffusionPeerSelectionEvents = Signal.selectEvents
                    (\case DiffusionPeerSelectionTrace e -> Just e
                           _                             -> Nothing)

selectDiffusionSimulationTrace :: Events DiffusionTestTrace
                               -> Events DiffusionSimulationTrace
selectDiffusionSimulationTrace = Signal.selectEvents
                    (\case DiffusionSimulationTrace e -> Just e
                           _                                   -> Nothing)

selectDiffusionPeerSelectionState :: Eq a
                                  => (forall peerconn. Governor.PeerSelectionState Cardano.ExtraState PeerTrustable (Cardano.ExtraPeers NtNAddr) NtNAddr peerconn -> a)
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
        DiffusionDebugPeerSelectionTrace (TraceGovernorState _ _ st) ->
          Just (Cardano.consensusMode (Governor.extraState st), f st)
        _                                                            ->
          Nothing)
  where
    initialState consensusMode =
      f $ Governor.emptyPeerSelectionState
            (mkStdGen 42)
            (Cardano.ExtraState.empty consensusMode (NumberOfBigLedgerPeers 0)) -- ^ todo: fix
            Cardano.ExtraPeers.empty

selectDiffusionPeerSelectionState' :: (forall peerconn. Governor.PeerSelectionState Cardano.ExtraState PeerTrustable (Cardano.ExtraPeers NtNAddr) NtNAddr peerconn -> a)
                                  -> Events DiffusionTestTrace
                                  -> Signal a
selectDiffusionPeerSelectionState' f =
  -- TODO: #3182 Rng seed should come from quickcheck.
    Signal.fromChangeEvents
      (f $ Governor.emptyPeerSelectionState
             (mkStdGen 42)
             (Cardano.ExtraState.empty PraosMode (NumberOfBigLedgerPeers 0))
             Cardano.ExtraPeers.empty)
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
  -> Trace () (AbstractTransitionTrace CM.ConnStateId)
selectDiffusionConnectionManagerTransitionEvents =
  Trace.fromList ()
  . mapMaybe
     (\case DiffusionConnectionManagerTransitionTrace e -> Just e
            _                                           -> Nothing)
  . Trace.toList

selectDiffusionConnectionManagerTransitionEvents'
  :: Trace () (WithName NtNAddr (WithTime DiffusionTestTrace))
  -> Trace () (WithName NtNAddr (WithTime (AbstractTransitionTrace CM.ConnStateId)))
selectDiffusionConnectionManagerTransitionEvents' =
    Trace.fromList ()
  . mapMaybe
     (\case
       (WithName addr (WithTime time (DiffusionConnectionManagerTransitionTrace e)))
         -> Just (WithName addr (WithTime time e))
       _ -> Nothing)
  . Trace.toList

selectDiffusionConnectionManagerTransitionEventsTime
  :: Trace () (Time, DiffusionTestTrace)
  -> Trace () (Time, AbstractTransitionTrace CM.ConnStateId)
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
              ++ show (sum . map (targetNumberOfActivePeers . fst . naPeerTargets . fst) $ nodes))
    . label ("Nº active big ledger peers: "
              ++ show (sum . map (targetNumberOfActiveBigLedgerPeers . fst . naPeerTargets . fst) $ nodes))
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
    :: (Governor.PeerSelectionState extraState extraFlags extraPeers NtNAddr peerconn -> Set NtNAddr)
    ->  Governor.PeerSelectionState extraState extraFlags extraPeers NtNAddr peerconn -> Set NtNAddr
dropBigLedgerPeers f =
  \st -> f st Set.\\ PublicRootPeers.getBigLedgerPeers (Governor.publicRootPeers st)


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

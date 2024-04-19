{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}

{-# OPTIONS_GHC -Wno-orphans #-}
-- TODO: remove it once #3601 is fixed
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-x-partial #-}
#endif

module Test.Ouroboros.Network.PeerSelection
  ( tests
  , unfHydra
  , takeBigLedgerPeers
  , dropBigLedgerPeers
  ) where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Exception (AssertionFailed (..), catch, evaluate)
import Control.Monad (when)
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import Control.Tracer (Tracer (..))

import Data.Bifoldable (bitraverse_)
import Data.ByteString.Char8 qualified as BS
import Data.Foldable (traverse_)
import Data.Function (on)
import Data.IP qualified as IP
import Data.List (foldl', groupBy, intercalate)
import Data.List.Trace qualified as Trace
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isNothing, listToMaybe)
import Data.OrdPSQ qualified as PSQ
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Void (Void)
import System.Random (mkStdGen)

import Network.DNS qualified as DNS (defaultResolvConf)
import Network.Socket (SockAddr)

import Ouroboros.Network.ExitPolicy (RepromoteDelay (..))
import Ouroboros.Network.PeerSelection.Bootstrap (UseBootstrapPeers (..),
           requiresBootstrapPeers)
import Ouroboros.Network.PeerSelection.Governor hiding (PeerSelectionState (..),
           peerSharing)
import Ouroboros.Network.PeerSelection.Governor qualified as Governor
import Ouroboros.Network.PeerSelection.LedgerPeers
import Ouroboros.Network.PeerSelection.LocalRootPeers (OutboundConnectionsState)
import Ouroboros.Network.PeerSelection.PeerAdvertise
import Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing (..))
import Ouroboros.Network.PeerSelection.PeerTrustable (PeerTrustable (..))
import Ouroboros.Network.PeerSelection.PublicRootPeers (PublicRootPeers)
import Ouroboros.Network.PeerSelection.PublicRootPeers qualified as PublicRootPeers
import Ouroboros.Network.PeerSelection.RootPeersDNS.DNSActions
import Ouroboros.Network.PeerSelection.RootPeersDNS.DNSSemaphore
import Ouroboros.Network.PeerSelection.RootPeersDNS.PublicRootPeers
import Ouroboros.Network.PeerSelection.State.EstablishedPeers qualified as EstablishedPeers
import Ouroboros.Network.PeerSelection.State.KnownPeers qualified as KnownPeers
import Ouroboros.Network.PeerSelection.State.LocalRootPeers (HotValency (..),
           LocalRootPeers (..), WarmValency (..))
import Ouroboros.Network.PeerSelection.State.LocalRootPeers qualified as LocalRootPeers
import Ouroboros.Network.Protocol.PeerSharing.Type (PeerSharingResult (..))

import Ouroboros.Network.Testing.Data.Script
import Ouroboros.Network.Testing.Data.Signal (E (E), Events, Signal, TS (TS),
           signalProperty)
import Ouroboros.Network.Testing.Data.Signal qualified as Signal
import Ouroboros.Network.Testing.Utils (disjointSetsProperty, isSubsetProperty,
           nightlyTest)
import Test.Ouroboros.Network.PeerSelection.Instances
import Test.Ouroboros.Network.PeerSelection.MockEnvironment hiding (tests)
import Test.Ouroboros.Network.PeerSelection.PeerGraph

import Control.Monad.IOSim

import Test.QuickCheck
import Test.QuickCheck.Monoids
import Test.Tasty
import Test.Tasty.QuickCheck
import Text.Pretty.Simple


-- Exactly as named.
unfHydra :: Int
unfHydra = 1

tests :: TestTree
tests =
  testGroup "Ouroboros.Network.PeerSelection"
  [ testGroup "PeerSelectionView"
    [ testProperty "sizes" prop_peerSelectionView_sizes
    ]
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
    , testProperty "event number coverage" prop_governor_events_coverage
    ]

    -- The no livelock property is needed to ensure other tests terminate
  , after AllSucceed "Ouroboros.Network.PeerSelection.basic" $
    testGroup "progress"
    [ testGroup "ledger peers"
      [ testProperty "progresses towards known target (from below)"
                     prop_governor_target_known_below
      , testProperty "progresses towards known target (from above)"
                     prop_governor_target_known_above

      , testProperty "progresses towards established target (from below)"
                     prop_governor_target_established_below
      , testProperty "progresses towards established target (from above)"
                     prop_governor_target_established_above

      , testProperty "progresses towards active target (from below)"
                     prop_governor_target_active_below
      , testProperty "progresses towards active target (from above)"
                     prop_governor_target_active_above
      ]

    , testGroup "public root peers"
      [ testProperty "progresses towards target (from below)"
                     prop_governor_target_root_below
      , testProperty "progresses towards established peers"
                     prop_governor_target_established_public
      , testProperty "progresses towards active peers"
                     prop_governor_target_active_public
      ]

    , testGroup "local root peers"
      [ testProperty "progresses towards established target"
                     prop_governor_target_established_local
      , testProperty "progresses towards active target (from below)"
                     prop_governor_target_active_local_below
      , testProperty "progresses towards active target (from above)"
                     prop_governor_target_active_local_above
      ]

    , testGroup "big ledger peers"
      [ testProperty "progresses towards known target (from below)"
        prop_governor_target_known_big_ledger_peers_below
      , testProperty "progresses towards known target (from above)"
        prop_governor_target_known_big_ledger_peers_above

      , testProperty "progresses towards established target"
                      prop_governor_target_established_big_ledger_peers
      , testProperty "progresses towards established target (from below)"
                      prop_governor_target_established_big_ledger_peers_below
      , testProperty "progresses towards established target (from above)"
                      prop_governor_target_established_big_ledger_peers_above

      , testProperty "progresses towards active target (from below)"
                     prop_governor_target_active_big_ledger_peers_below
      , testProperty "progresses towards active target (from above)"
                     prop_governor_target_active_big_ledger_peers_above
      ]
    , testGroup "bootstrap peers"
      [ testProperty "progress towards only bootstrap peers after changing to fallback state"
                     prop_governor_only_bootstrap_peers_in_fallback_state
      , testProperty "node does not learn about non trustable peers when in fallback state"
                     prop_governor_no_non_trustable_peers_before_caught_up_state
      , testProperty "node only use bootstrap peers if in sensitive state"
                     prop_governor_stops_using_bootstrap_peers
      , testProperty "node never uses non-trustable peers in clean state"
                     prop_governor_only_bootstrap_peers_in_clean_state
      , testProperty "node uses ledger peers in non-sensitive mode"
                     prop_governor_uses_ledger_peers
      ]
    , testProperty "association mode" prop_governor_association_mode
    ]
  , testGroup "issues"
    [ testProperty "3233" prop_issue_3233
    , testProperty "3494" prop_issue_3494
    , testProperty "3515" prop_issue_3515
    , testProperty "3550" prop_issue_3550
    ]
  , testProperty "governor repromote delay with fuzz"   prop_governor_repromote_delay
  , testProperty "governor peer share reachable in 1hr" prop_governor_peershare_1hr
  , testProperty "governor connection status"           prop_governor_connstatus
  , testProperty "governor no livelock"                 prop_governor_nolivelock

  , testGroup "races"
    [ nightlyTest $ testProperty "governor no livelock"       $ prop_explore_governor_nolivelock
    , nightlyTest $ testProperty "governor connection status" $ prop_explore_governor_connstatus
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

prop_peerSelectionView_sizes :: GovernorMockEnvironment -> Property
prop_peerSelectionView_sizes env =
    let trace = runGovernorInMockEnvironment env
        evs   = selectGovernorStateEvents
              $ selectPeerSelectionTraceEventsUntil (Time (10 * 3600)) trace
    in property $
       foldMap (\(_, TraceGovernorState _ _ st) ->
                     let view = peerSelectionStateToView st in
                        All (viewInvariant (fst <$> view))
                     <> All (viewSizeInvariant view))
               evs
  where
    viewInvariant :: PeerSelectionView (Set PeerAddr)
                  -> Property
    viewInvariant PeerSelectionView {..} =
           isSubsetProperty "viewActivePeersDemotions" viewActivePeersDemotions viewActivePeers
      .&&. isSubsetProperty "viewActivePeers" viewActivePeers viewEstablishedPeers
      .&&. isSubsetProperty "viewEstablishedPeers" viewEstablishedPeers viewKnownPeers
      .&&. isSubsetProperty "viewColdPeersPromotions" viewColdPeersPromotions viewKnownPeers
      .&&. isSubsetProperty "viewAvailableToConnectPeers" viewAvailableToConnectPeers viewKnownPeers
      .&&. isSubsetProperty "viewWarmPeersDemotions" viewWarmPeersDemotions (viewEstablishedPeers Set.\\ viewActivePeers)
      .&&. isSubsetProperty "viewWarmPeersPromotions" viewWarmPeersPromotions (viewEstablishedPeers Set.\\ viewActivePeers)

      .&&. isSubsetProperty "viewActiveBigLedgerPeersDemotions" viewActiveBigLedgerPeersDemotions viewActiveBigLedgerPeers
      .&&. isSubsetProperty "viewActiveBigLedgerPeers" viewActiveBigLedgerPeers viewEstablishedBigLedgerPeers
      .&&. isSubsetProperty "viewEstablishedBigLedgerPeers" viewEstablishedBigLedgerPeers viewKnownBigLedgerPeers
      .&&. isSubsetProperty "viewColdBigLedgerPeersPromotions" viewColdBigLedgerPeersPromotions viewKnownBigLedgerPeers
      .&&. isSubsetProperty "viewAvailableToConnectBigLedgerPeers" viewAvailableToConnectBigLedgerPeers viewKnownBigLedgerPeers
      .&&. isSubsetProperty "viewWarmBigLedgerPeersDemotions" viewWarmBigLedgerPeersDemotions (viewEstablishedBigLedgerPeers Set.\\ viewActiveBigLedgerPeers)
      .&&. isSubsetProperty "viewWarmBigLedgerPeersPromotions" viewWarmBigLedgerPeersPromotions (viewEstablishedBigLedgerPeers Set.\\ viewActiveBigLedgerPeers)

      .&&. isSubsetProperty "viewActiveLocalRootPeersDemotions" viewActiveLocalRootPeersDemotions viewActiveLocalRootPeers
      .&&. isSubsetProperty "viewActiveLocalRootPeers" viewActiveLocalRootPeers viewEstablishedLocalRootPeers
      .&&. isSubsetProperty "viewEstablishedLocalRootPeers" viewEstablishedLocalRootPeers viewKnownLocalRootPeers
      .&&. isSubsetProperty "viewColdLocalRootPeersPromotions" viewColdLocalRootPeersPromotions viewKnownLocalRootPeers
      .&&. isSubsetProperty "viewAvailableToConnectLocalRootPeers" viewAvailableToConnectLocalRootPeers viewKnownLocalRootPeers
      .&&. isSubsetProperty "viewWarmLocalRootPeersPromotions" viewWarmLocalRootPeersPromotions (viewEstablishedLocalRootPeers Set.\\ viewActiveLocalRootPeers)

      .&&. isSubsetProperty "viewActiveNonRootPeersDemotions" viewActiveNonRootPeersDemotions viewActiveNonRootPeers
      .&&. isSubsetProperty "viewActiveNonRootPeers" viewActiveNonRootPeers viewEstablishedNonRootPeers
      .&&. isSubsetProperty "viewEstablishedNonRootPeers" viewEstablishedNonRootPeers viewKnownNonRootPeers
      .&&. isSubsetProperty "viewColdNonRootPeersPromotions" viewColdNonRootPeersPromotions viewKnownNonRootPeers
      .&&. isSubsetProperty "viewWarmNonRootPeersPromotions" viewWarmNonRootPeersPromotions (viewEstablishedNonRootPeers Set.\\ viewActiveNonRootPeers)
      .&&. isSubsetProperty "viewWarmNonRootPeersDemotions" viewWarmNonRootPeersDemotions (viewEstablishedNonRootPeers Set.\\ viewActiveNonRootPeers)

      .&&. isSubsetProperty "viewActiveBootstrapPeersDemotions" viewActiveBootstrapPeersDemotions viewActiveBootstrapPeers
      .&&. isSubsetProperty "viewActiveBootstrapPeers" viewActiveBootstrapPeers viewEstablishedBootstrapPeers
      .&&. isSubsetProperty "viewEstablishedBootstrapPeers" viewEstablishedBootstrapPeers viewKnownBootstrapPeers
      .&&. isSubsetProperty "viewColdBootstrapPeersPromotions" viewColdBootstrapPeersPromotions viewKnownBootstrapPeers
      .&&. isSubsetProperty "viewWarmBootstrapPeersPromotions" viewWarmBootstrapPeersPromotions (viewEstablishedBootstrapPeers Set.\\ viewActiveBootstrapPeers)
      .&&. isSubsetProperty "viewWarmBootstrapPeersDemotions" viewWarmBootstrapPeersDemotions (viewEstablishedBootstrapPeers Set.\\ viewActiveBootstrapPeers)

      .&&. disjointSetsProperty "viewKnownPeers viewKnownBigLedgerPeers" viewKnownPeers viewKnownBigLedgerPeers
      .&&. isSubsetProperty "viewKnownLocalRootPeers" viewKnownLocalRootPeers viewKnownPeers
      .&&. isSubsetProperty "viewKnownNonRootPeers" viewKnownNonRootPeers viewKnownPeers
      .&&. isSubsetProperty "viewKnownBootstrapPeers" viewKnownBootstrapPeers viewKnownPeers

      .&&. disjointSetsProperty "viewKnownLocalRootPeers-viewKnownBigLedgerPeers" viewKnownLocalRootPeers viewKnownBigLedgerPeers
      .&&. disjointSetsProperty "viewKnownLocalRootPeers-viewKnownNonRootPeers" viewKnownLocalRootPeers viewKnownNonRootPeers
      .&&. disjointSetsProperty "viewKnownLocalRootPeers-viewKnownBootstrapPeers" viewKnownLocalRootPeers viewKnownBootstrapPeers

      .&&. disjointSetsProperty "viewKnownNonRootPeers-viewKnownBigLedgerPeers" viewKnownNonRootPeers viewKnownBigLedgerPeers
      .&&. disjointSetsProperty "viewKnownBootstrapPeers-viewKnownBigLedgerPeers" viewKnownBootstrapPeers viewKnownBigLedgerPeers

    viewSizeInvariant :: PeerSelectionSetsWithSizes PeerAddr
                      -> Property
    viewSizeInvariant PeerSelectionView {..} =
            counterexample "viewRootPeers"
            (Set.size (fst viewRootPeers) === snd viewRootPeers)

      .&&.  counterexample "viewKnownPeers"
            (Set.size (fst viewKnownPeers) === snd viewKnownPeers)

      .&&. counterexample "viewAvailableToConnectPeers"
           (Set.size (fst viewAvailableToConnectPeers) === snd viewAvailableToConnectPeers)
      .&&. counterexample "viewColdPeersPromotions"
           (Set.size (fst viewColdPeersPromotions) === snd viewColdPeersPromotions)
      .&&. counterexample "viewEstablishedPeers"
           (Set.size (fst viewEstablishedPeers) === snd viewEstablishedPeers)
      .&&. counterexample "viewWarmPeersDemotions"
           (Set.size (fst viewWarmPeersDemotions) === snd viewWarmPeersDemotions)
      .&&. counterexample "viewWarmPeersPromotions"
           (Set.size (fst viewWarmPeersPromotions) === snd viewWarmPeersPromotions)
      .&&. counterexample "viewActivePeers"
           (Set.size (fst viewActivePeers) === snd viewActivePeers)
      .&&. counterexample "viewActivePeersDemotions"
           (Set.size (fst viewActivePeersDemotions) === snd viewActivePeersDemotions)

      .&&. counterexample "viewKnownBigLedgerPeers"
           (Set.size (fst viewKnownBigLedgerPeers) === snd viewKnownBigLedgerPeers)
      .&&. counterexample "viewAvailableToConnectBigLedgerPeers"
           (Set.size (fst viewAvailableToConnectBigLedgerPeers) === snd viewAvailableToConnectBigLedgerPeers)
      .&&. counterexample "viewColdBigLedgerPeersPromotions"
           (Set.size (fst viewColdBigLedgerPeersPromotions) === snd viewColdBigLedgerPeersPromotions)
      .&&. counterexample "viewEstablishedBigLedgerPeers"
           (Set.size (fst viewEstablishedBigLedgerPeers) === snd viewEstablishedBigLedgerPeers)
      .&&. counterexample "viewWarmBigLedgerPeersDemotions"
           (Set.size (fst viewWarmBigLedgerPeersDemotions) === snd viewWarmBigLedgerPeersDemotions)
      .&&. counterexample "viewWarmBigLedgerPeersPromotions"
           (Set.size (fst viewWarmBigLedgerPeersPromotions) === snd viewWarmBigLedgerPeersPromotions)
      .&&. counterexample "viewActiveBigLedgerPeers"
           (Set.size (fst viewActiveBigLedgerPeers) === snd viewActiveBigLedgerPeers)
      .&&. counterexample "viewActiveBigLedgerPeersDemotions"
           (Set.size (fst viewActiveBigLedgerPeersDemotions) === snd viewActiveBigLedgerPeersDemotions)

      .&&. counterexample "viewKnownLocalRootPeers"
           (Set.size (fst viewKnownLocalRootPeers) === snd viewKnownLocalRootPeers)
      .&&. counterexample "viewAvailableToConnectLocalRootPeers"
           (Set.size (fst viewAvailableToConnectLocalRootPeers) === snd viewAvailableToConnectLocalRootPeers)
      .&&. counterexample "viewColdLocalRootPeersPromotions"
           (Set.size (fst viewColdLocalRootPeersPromotions) === snd viewColdLocalRootPeersPromotions)
      .&&. counterexample "viewEstablishedLocalRootPeers"
           (Set.size (fst viewEstablishedLocalRootPeers) === snd viewEstablishedLocalRootPeers)
      .&&. counterexample "viewWarmLocalRootPeersPromotions"
           (Set.size (fst viewWarmLocalRootPeersPromotions) === snd viewWarmLocalRootPeersPromotions)
      .&&. counterexample "viewActiveLocalRootPeers"
           (Set.size (fst viewActiveLocalRootPeers) === snd viewActiveLocalRootPeers)
      .&&. counterexample "viewActiveLocalRootPeersDemotions"
           (Set.size (fst viewActiveLocalRootPeersDemotions) === snd viewActiveLocalRootPeersDemotions)

      .&&. counterexample "viewKnownNonRootPeers"
           (Set.size (fst viewKnownNonRootPeers) === snd viewKnownNonRootPeers)
      .&&. counterexample "viewColdNonRootPeersPromotions"
           (Set.size (fst viewColdNonRootPeersPromotions) === snd viewColdNonRootPeersPromotions)
      .&&. counterexample "viewEstablishedNonRootPeers"
           (Set.size (fst viewEstablishedNonRootPeers) === snd viewEstablishedNonRootPeers)
      .&&. counterexample "viewWarmNonRootPeersDemotions"
           (Set.size (fst viewWarmNonRootPeersDemotions) === snd viewWarmNonRootPeersDemotions)
      .&&. counterexample "viewWarmNonRootPeersPromotions"
           (Set.size (fst viewWarmNonRootPeersPromotions) === snd viewWarmNonRootPeersPromotions)
      .&&. counterexample "viewActiveNonRootPeers"
           (Set.size (fst viewActiveNonRootPeers) === snd viewActiveNonRootPeers)
      .&&. counterexample "viewActiveNonRootPeersDemotions"
           (Set.size (fst viewActiveNonRootPeersDemotions) === snd viewActiveNonRootPeersDemotions)

      .&&. counterexample "viewKnownBootstrapPeers"
           (Set.size (fst viewKnownBootstrapPeers) === snd viewKnownBootstrapPeers)
      .&&. counterexample "viewColdBootstrapPeersPromotions"
           (Set.size (fst viewColdBootstrapPeersPromotions) === snd viewColdBootstrapPeersPromotions)
      .&&. counterexample "viewEstablishedBootstrapPeers"
           (Set.size (fst viewEstablishedBootstrapPeers) === snd viewEstablishedBootstrapPeers)
      .&&. counterexample "viewWarmBootstrapPeersDemotions"
           (Set.size (fst viewWarmBootstrapPeersDemotions) === snd viewWarmBootstrapPeersDemotions)
      .&&. counterexample "viewWarmBootstrapPeersPromotions"
           (Set.size (fst viewWarmBootstrapPeersPromotions) === snd viewWarmBootstrapPeersPromotions)
      .&&. counterexample "viewActiveBootstrapPeers"
           (Set.size (fst viewActiveBootstrapPeers) === snd viewActiveBootstrapPeers)
      .&&. counterexample "viewActiveBootstrapPeersDemotions"
           (Set.size (fst viewActiveBootstrapPeersDemotions) === snd viewActiveBootstrapPeersDemotions)


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
-- * A cold peer peer sharing "reachable" property: that the governor either hits
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
prop_governor_hasoutput :: GovernorMockEnvironment -> Property
prop_governor_hasoutput env =
    let trace = runGovernorInMockEnvironment env
        evs   = selectPeerSelectionTraceEvents trace

     in counterexample (unlines ["\nSIM TRACE", ppTrace trace])
      $ counterexample (unlines . ("EVENTS" :) . map show $ evs)
      $ hasOutput env (selectGovernorEvents evs)

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
 && (PublicRootPeers.null publicRootPeers
      || all (\(t,_) -> targetNumberOfRootPeers  t == 0) targets)


-- | As a basic property we run the governor to explore its state space a bit
-- and check it does not throw any exceptions (assertions such as invariant
-- violations).
--
-- We do /not/ assume freedom from livelock for this property, so we run the
-- governor for a maximum number of trace events rather than for a fixed
-- simulated time.
--
prop_governor_nofail :: GovernorMockEnvironment -> Property
prop_governor_nofail env =
    let ioSimTrace = runGovernorInMockEnvironment env
        trace = take 5000
              . selectPeerSelectionTraceEvents
              $ ioSimTrace

    -- run in `IO` so we can catch the pure 'AssertionFailed' exception
    in ioProperty $ do
      r <-
        evaluate ( foldl' (flip seq) True
               $ [ assertPeerSelectionState st ()
                 | (_, GovernorDebug (TraceGovernorState _ _ st)) <- trace ]
               )
        `catch` \(AssertionFailed _) -> return False
      case r of
        True  -> return $ property True
        False -> do
          bitraverse_ (putStrLn . show)
                      (putStrLn . ppSimEvent 20 20 20)
                      ioSimTrace
          -- the ioSimTrace is infinite, but it will terminate with `AssertionFailed`
          error "impossible!"



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
    check_governor_nolivelock 5000 $ runGovernorInMockEnvironment env

prop_explore_governor_nolivelock :: GovernorMockEnvironment -> Property
prop_explore_governor_nolivelock =

    -- Simulation steps take longer and longer as simulated time
    -- advances; running time for this property is quadratic in the
    -- number of events explored. We limit the test to 500 governor
    -- events to avoid unreasonably slow tests. This may be because
    -- the governor becomes slower over time with priority-based
    -- scheduling, or because IOSimPOR's data structures grow.

    -- This test currently fails, because of a broken assertion in
    -- Ouroboros.Network.PeerSelection.Governor.ActivePeers, which
    -- checks that a newly promoted warm peer is a member of the set
    -- of established peers. This may not be true if the promotion is
    -- delayed by a race condition.

    prop'_explore_governor_nolivelock id 500

prop'_explore_governor_nolivelock :: ExplorationSpec -> Int -> GovernorMockEnvironment -> Property
prop'_explore_governor_nolivelock spec len env =
    exploreGovernorInMockEnvironment spec env $ \_ trace ->
      -- counterexample (showTrace trace) $
      -- whenfail (pPrint env) $
      check_governor_nolivelock len trace

check_governor_nolivelock :: Int -> SimTrace a -> Property
check_governor_nolivelock n trace0 =
    let trace = take n .
                selectGovernorEvents .
                selectPeerSelectionTraceEvents $
                  trace0
     in case tooManyEventsBeforeTimeAdvances 1000 trace of
          Nothing -> property True
          Just (t, es) ->
            counterexample
              ("over 1000 events at time: " ++ show t ++ "\n" ++
               "first 50 events: " ++ (unlines . map show . take 50 $ es)) $
            property False

{-
showTrace = break . show
  where break s | "(Trace " `isPrefixOf` s = "\n" ++ breaks
                | otherwise                = breaks
         where breaks | null s    = []
                      | otherwise = head s : break (tail s)
-}

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
-- (quite reasonably) start with a big burst of activity (e.g. as it peer shares
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
    busy !busyStartTime !credits ((busyEndTime, _dt, GovernorEvent{}) : trace')
      | busySpanLength > endCredits credits trace'=
        Left (busyEndTime, endCredits credits trace')
      where
        busySpanLength = diffTime busyEndTime busyStartTime

        -- If the governor wakes up due to an action that gives us new credits
        -- we take those credits into account before failing.
        endCredits !c [] = c
        endCredits !c ((t, _, MockEnvEvent e) : tr) | t == busyEndTime =
          endCredits (c + fromIntegral (envEventCredits e)) tr
        endCredits !c ((t, _, _) : tr) | t == busyEndTime =
          endCredits c tr
        endCredits !c _ = c

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

envEventCredits (TraceEnvSetLocalRoots  peers)  = LocalRootPeers.size peers
envEventCredits (TraceEnvSetPublicRoots peers)  = PublicRootPeers.size peers
envEventCredits  TraceEnvRequestPublicRootPeers = 0
envEventCredits  TraceEnvRequestBigLedgerPeers  = 0
envEventCredits  TraceEnvPublicRootTTL          = 60
envEventCredits  TraceEnvBigLedgerPeersTTL      = 60

envEventCredits (TraceEnvSetTargets PeerSelectionTargets {
                   targetNumberOfRootPeers = _,
                   targetNumberOfKnownPeers,
                   targetNumberOfEstablishedPeers,
                   targetNumberOfActivePeers
                 }) = 80
                    + 10 * (targetNumberOfKnownPeers
                          + targetNumberOfEstablishedPeers
                          + targetNumberOfActivePeers)

envEventCredits (TraceEnvPeersDemote Noop   _)    = 10
envEventCredits (TraceEnvPeersDemote ToWarm _)    = 30
envEventCredits (TraceEnvPeersDemote ToCooling _) = 30
envEventCredits (TraceEnvPeersDemote ToCold _)    = 30

envEventCredits  TraceEnvPeersStatus{}          = 0

-- These events can return emty results which will actually result in the governor
-- to issue more since it wasn't able to make progress towards the target.
-- However, the governor won't be making infinite requests, and ending up on a
-- livelock. Given this, in the case it doesn't manage to make
-- progress we give it a little bit of credits in order to account for the
-- next request.
--
envEventCredits  TraceEnvPeerShareResult{}      = 10
envEventCredits  TraceEnvRootsResult{}          = 10
envEventCredits  TraceEnvBigLedgerPeersResult{} = 10

-- These events are visible in the environment but are the result of actions
-- initiated by the governor, hence they get no credit.
envEventCredits  TraceEnvPeerShareRequest{}     = 0

envEventCredits  TraceEnvPeerShareTTL   {}      = 0

envEventCredits  TraceEnvEstablishConn {}       = 0
envEventCredits  TraceEnvActivatePeer {}        = 0
envEventCredits  TraceEnvDeactivatePeer {}      = 0
envEventCredits  TraceEnvCloseConn {}           = 0

envEventCredits  TraceEnvUseLedgerPeers {}      = 30
envEventCredits  TraceEnvSetLedgerStateJudgement {} = 30

envEventCredits  TraceEnvSetUseBootstrapPeers {} = 30


-- | A coverage property that checks how many events are analysed when taking
-- up to 10h of execution time.
--
prop_governor_events_coverage :: GovernorMockEnvironment -> Property
prop_governor_events_coverage env =
    let trace = Signal.eventsToList
              . Signal.eventsFromListUpToTime (Time (10 * 60 * 60))
              . selectPeerSelectionTraceEvents
              . runGovernorInMockEnvironment
              $ env

        printLength x
          | x < 10 = "# events < 10"
          | x >= 10 && x < 100 = "# events >= 10 && < 100"
          | x >= 100 && x < 1000 = "# events >= 100 && < 1000"
          | x >= 1000 && x < 10000 = "# events >= 1000 && < 10000"
          | x >= 10000 && x < 100000 = "# events >= 10000 && < 100000"
          | x >= 100000 && x < 1000000 = "# events >= 100000 && < 1000000"
          | otherwise = "# events >= 1000000"
     in tabulate   "# events" [printLength (length trace)]
        True

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
traceNum TraceLocalRootPeersChanged{}                         = 00
traceNum TraceTargetsChanged{}                                = 01
traceNum TracePublicRootsRequest{}                            = 02
traceNum TracePublicRootsResults{}                            = 03
traceNum TracePublicRootsFailure{}                            = 04
traceNum TracePeerShareRequests{}                             = 05
traceNum TracePeerShareResults{}                              = 06
traceNum TracePeerShareResultsFiltered{}                      = 07
traceNum TraceForgetColdPeers{}                               = 08
traceNum TracePromoteColdPeers{}                              = 09
traceNum TracePromoteColdLocalPeers{}                         = 10
traceNum TracePromoteColdFailed{}                             = 11
traceNum TracePromoteColdDone{}                               = 12
traceNum TracePromoteWarmPeers{}                              = 13
traceNum TracePromoteWarmLocalPeers{}                         = 14
traceNum TracePromoteWarmFailed{}                             = 15
traceNum TracePromoteWarmDone{}                               = 16
traceNum TraceDemoteWarmPeers{}                               = 17
traceNum TraceDemoteWarmFailed{}                              = 18
traceNum TraceDemoteWarmDone{}                                = 19
traceNum TraceDemoteHotPeers{}                                = 20
traceNum TraceDemoteLocalHotPeers{}                           = 21
traceNum TraceDemoteHotFailed{}                               = 22
traceNum TraceDemoteHotDone{}                                 = 23
traceNum TraceDemoteAsynchronous{}                            = 24
traceNum TraceGovernorWakeup{}                                = 25
traceNum TraceChurnWait{}                                     = 26
traceNum TraceChurnMode{}                                     = 27
traceNum TracePromoteWarmAborted{}                            = 28
traceNum TraceDemoteLocalAsynchronous{}                       = 29
traceNum TraceBigLedgerPeersRequest{}                         = 30
traceNum TraceBigLedgerPeersResults{}                         = 31
traceNum TraceBigLedgerPeersFailure{}                         = 32
traceNum TraceForgetBigLedgerPeers{}                          = 33
traceNum TracePromoteColdBigLedgerPeers{}                     = 34
traceNum TracePromoteColdBigLedgerPeerFailed{}                = 35
traceNum TracePromoteColdBigLedgerPeerDone{}                  = 36
traceNum TracePromoteWarmBigLedgerPeers{}                     = 37
traceNum TracePromoteWarmBigLedgerPeerFailed{}                = 38
traceNum TracePromoteWarmBigLedgerPeerDone{}                  = 39
traceNum TracePromoteWarmBigLedgerPeerAborted{}               = 40
traceNum TraceDemoteWarmBigLedgerPeers{}                      = 41
traceNum TraceDemoteWarmBigLedgerPeerFailed{}                 = 42
traceNum TraceDemoteWarmBigLedgerPeerDone{}                   = 43
traceNum TraceDemoteHotBigLedgerPeers{}                       = 44
traceNum TraceDemoteHotBigLedgerPeerFailed{}                  = 45
traceNum TraceDemoteHotBigLedgerPeerDone{}                    = 46
traceNum TraceKnownInboundConnection{}                        = 47
traceNum TraceDemoteBigLedgerPeersAsynchronous{}              = 48
traceNum TraceLedgerStateJudgementChanged{}                   = 49
traceNum TraceOnlyBootstrapPeers{}                            = 50
traceNum TraceBootstrapPeersFlagChangedWhilstInSensitiveState = 51
traceNum TraceUseBootstrapPeersChanged {}                     = 52
traceNum TraceOutboundGovernorCriticalFailure {}              = 53
traceNum TraceDebugState {}                                   = 54
traceNum TraceChurnAction {}                                  = 55
traceNum TraceChurnTimeout {}                                 = 56

allTraceNames :: Map Int String
allTraceNames =
  Map.fromList
   [ (00, "TraceLocalRootPeersChanged")
   , (01, "TraceTargetsChanged")
   , (02, "TracePublicRootsRequest")
   , (03, "TracePublicRootsResults")
   , (04, "TracePublicRootsFailure")
   , (05, "TracePeerShareRequests")
   , (06, "TracePeerShareResults")
   , (07, "TracePeerShareResultsFiltered")
   , (08, "TraceForgetColdPeers")
   , (09, "TracePromoteColdPeers")
   , (10, "TracePromoteColdLocalPeers")
   , (11, "TracePromoteColdFailed")
   , (12, "TracePromoteColdDone")
   , (13, "TracePromoteWarmPeers")
   , (14, "TracePromoteWarmLocalPeers")
   , (15, "TracePromoteWarmFailed")
   , (16, "TracePromoteWarmDone")
   , (17, "TraceDemoteWarmPeers")
   , (18, "TraceDemoteWarmFailed")
   , (19, "TraceDemoteWarmDone")
   , (20, "TraceDemoteHotPeers")
   , (21, "TraceDemoteLocalHotPeers")
   , (22, "TraceDemoteHotFailed")
   , (23, "TraceDemoteHotDone")
   , (24, "TraceDemoteAsynchronous")
   , (25, "TraceGovernorWakeup")
   , (26, "TraceChurnWait")
   , (27, "TraceChurnMode")
   , (28, "TracePromoteWarmAborted")
   , (29, "TraceDemoteAsynchronous")
   , (30, "TraceBigLedgerPeersRequest")
   , (31, "TraceBigLedgerPeersResults")
   , (32, "TraceBigLedgerPeersFailure")
   , (33, "TraceForgetBigLedgerPeers")
   , (34, "TracePromoteColdBigLedgerPeers")
   , (35, "TracePromoteColdBigLedgerPeerFailed")
   , (36, "TracePromoteColdBigLedgerPeerDone")
   , (37, "TracePromoteWarmBigLedgerPeers")
   , (38, "TracePromoteWarmBigLedgerPeerFailed")
   , (39, "TracePromoteWarmBigLedgerPeerDone")
   , (40, "TracePromoteWarmBigLedgerPeerAborted")
   , (41, "TraceDemoteWarmBigLedgerPeers")
   , (42, "TraceDemoteWarmBigLedgerPeerFailed")
   , (43, "TraceDemoteWarmBigLedgerPeerDone")
   , (44, "TraceDemoteHotBigLedgerPeers")
   , (45, "TraceDemoteHotBigLedgerPeerFailed")
   , (46, "TraceDemoteHotBigLedgerPeerDone")
   , (47, "TraceKnownInboundConnection")
   , (48, "TraceDemoteBigLedgerPeersAsynchronous")
   , (49, "TraceLedgerStateJudgementChanged")
   , (50, "TraceOnlyBootstrapPeers")
   , (51, "TraceBootstrapPeersFlagChangedWhilstInSensitiveState")
   , (52, "TraceUseBootstrapPeersChanged")
   , (53, "TraceOutboundGovernorCriticalFailure")
   , (54, "TraceDebugState")
   , (55, "TraceChurnAction")
   , (56, "TraceChurnTimeout")
   ]


-- | Run the governor for up to 1 hour (simulated obviously) and look at the
-- set of known peers it has selected. This uses static targets and root peers.
--
-- As a basic correctness property, the peers the governor selects must be a
-- subset of those that are in principle reachable in the mock network
-- environment.
--
-- More interestingly, we expect the governor to find enough peers. However,
-- one can not test that it will find all reachable addresses, since we only
-- peer share with established peers and the mock environment might never promote
-- the peer that would allow us to reach every other peer.
--
prop_governor_peershare_1hr :: GovernorMockEnvironment -> Property
prop_governor_peershare_1hr env@GovernorMockEnvironment {
                               peerGraph,
                               localRootPeers,
                               publicRootPeers,
                               targets
                             } =
    let ioSimTrace = runGovernorInMockEnvironment env {
                         targets = singletonScript (targets', NoDelay)
                       }
        trace      = selectPeerSelectionTraceEvents ioSimTrace
        Just found = knownPeersAfter1Hour trace
        reachable  = peerShareReachablePeers peerGraph
                       (LocalRootPeers.keysSet localRootPeers <> PublicRootPeers.toSet publicRootPeers)
     in counterexample ( intercalate "\n"
                       . map (ppSimEvent 20 20 20)
                       . takeWhile (\e -> seTime e <= Time (60*60))
                       . Trace.toList
                       $ ioSimTrace) $
        subsetProperty    found reachable
  where
    -- This test is only about testing peer sharing,
    -- so do not try to establish connections:
    targets' :: PeerSelectionTargets
    targets' = fst (scriptHead targets)

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

-- | Check the governor's view of connection status does not lag behind reality
-- by too much.
--

prop_governor_connstatus :: GovernorMockEnvironment -> Property
prop_governor_connstatus env =
  check_governor_connstatus Nothing (runGovernorInMockEnvironment env)

-- The explore version of this property fails, I think because the
-- assumption made in the check that status events appear in the trace
-- in the correct order is broken by a race.

prop_explore_governor_connstatus :: GovernorMockEnvironment -> Property
prop_explore_governor_connstatus = prop'_explore_governor_connstatus id

prop'_explore_governor_connstatus :: ExplorationSpec -> GovernorMockEnvironment -> Property
prop'_explore_governor_connstatus opts env =
  whenFail (pPrint env) $
  exploreGovernorInMockEnvironment opts env check_governor_connstatus

check_governor_connstatus :: Maybe (SimTrace a) -> SimTrace a -> Property
check_governor_connstatus _ trace0 =
    let trace = takeFirstNHours 1
              . selectPeerSelectionTraceEvents $ trace0
        --TODO: check any actually get a true status output and try some deliberate bugs
     in
     whenFail (traverse_ print trace) $
     conjoin $ map ok (groupBy ((==) `on` fst) trace)
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
            [ Map.filter (not . isPeerCooling) status
            | (_, MockEnvEvent (TraceEnvPeersStatus status)) <- reverse trace ]

        isPeerCooling PeerCooling = True
        isPeerCooling _           = False

        lastGovStatus =
          listToMaybe
            [ Governor.establishedPeersStatus st
            | (_, GovernorDebug (TraceGovernorState _ _ st)) <- reverse trace ]


--
-- Progress properties
--

-- | A variant of 'prop_governor_target_established_below' but for the target
-- number of root peers.
--
-- Check that the governor can hit (but not overshoot) its target for the
-- number of root peers. This has to be bounded by what is possible: we cannot
-- always find enough peers, and when we can, some of them fail.
--
prop_governor_target_root_below :: GovernorMockEnvironment -> Property
prop_governor_target_root_below env =
    let events = Signal.eventsFromListUpToTime (Time (10 * 60 * 60))
               . selectPeerSelectionTraceEvents
               . runGovernorInMockEnvironment
               $ env

        govTargetsSig :: Signal Int
        govTargetsSig =
          selectGovState (targetNumberOfRootPeers . Governor.targets) events

        govLocalRootPeersSig :: Signal (Set PeerAddr)
        govLocalRootPeersSig =
          selectGovState (LocalRootPeers.keysSet . Governor.localRootPeers) events

        govPublicRootPeersSig :: Signal (Set PeerAddr)
        govPublicRootPeersSig =
          selectGovState (PublicRootPeers.toSet . Governor.publicRootPeers) events

        govRootPeersSig :: Signal (Set PeerAddr)
        govRootPeersSig = Set.union <$> govLocalRootPeersSig <*> govPublicRootPeersSig

        -- There are no opportunities if we're at or above target
        --
        requestOpportunity target public roots
          | Set.size roots >= target
          = Set.empty

          | otherwise
          = public Set.\\ roots

        requestOpportunities :: Signal (Set PeerAddr)
        requestOpportunities =
          requestOpportunity
            <$> govTargetsSig
            <*> govPublicRootPeersSig
            <*> govRootPeersSig

        requestOpportunitiesIgnoredTooLong :: Signal (Set PeerAddr)
        requestOpportunitiesIgnoredTooLong =
          Signal.keyedTimeout
            10 -- seconds
            id
            requestOpportunities

     in counterexample
          ("\nSignal key: (target, local peers, public peers, root peers, " ++
           "opportunities, ignored too long)") $

        signalProperty 20 show
          (\(_,_,_,_,_,toolong) -> Set.null toolong)
          ((,,,,,) <$> govTargetsSig
                   <*> govLocalRootPeersSig
                   <*> govPublicRootPeersSig
                   <*> govRootPeersSig
                   <*> requestOpportunities
                   <*> requestOpportunitiesIgnoredTooLong)

-- | A variant of 'prop_governor_target_established_below' but for the target
-- that any public root peers should become established.
--
-- We do not need separate above and below variants of this property since it
-- is not possible to exceed the target.
--
prop_governor_target_established_public :: MaxTime -> GovernorMockEnvironment -> Property
prop_governor_target_established_public (MaxTime maxTime) env =
    let events = Signal.eventsFromListUpToTime maxTime
               . selectPeerSelectionTraceEvents
               . runGovernorInMockEnvironment
               $ env

        govPublicRootPeersSig :: Signal (Set PeerAddr)
        govPublicRootPeersSig =
          selectGovState (PublicRootPeers.toSet . Governor.publicRootPeers)
                         events

        govEstablishedPeersSig :: Signal (Set PeerAddr)
        govEstablishedPeersSig =
          selectGovState
            (EstablishedPeers.toSet . Governor.establishedPeers)
            events

        govInProgressPromoteColdSig :: Signal (Set PeerAddr)
        govInProgressPromoteColdSig =
          selectGovState
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


-- | A variant of 'prop_governor_target_established_public' but for big ledger
-- peers.
--
prop_governor_target_established_big_ledger_peers
    :: MaxTime
    -> GovernorMockEnvironment
    -> Property
prop_governor_target_established_big_ledger_peers (MaxTime maxTime) env =
    let events = Signal.eventsFromListUpToTime maxTime
               . selectPeerSelectionTraceEvents
               . runGovernorInMockEnvironment
               $ env

        govBigLedgerPeersSig :: Signal (Set PeerAddr)
        govBigLedgerPeersSig =
          selectGovState (PublicRootPeers.getBigLedgerPeers . Governor.publicRootPeers)
                         events

        govLedgerStateJudgement :: Signal LedgerStateJudgement
        govLedgerStateJudgement =
          selectGovState (Governor.ledgerStateJudgement)
                         events

        govEstablishedPeersSig :: Signal (Set PeerAddr)
        govEstablishedPeersSig =
          selectGovState
            (EstablishedPeers.toSet . Governor.establishedPeers)
            events

        govInProgressPromoteColdSig :: Signal (Set PeerAddr)
        govInProgressPromoteColdSig =
          selectGovState
            Governor.inProgressPromoteCold
            events

        bigLedgerPeersInEstablished :: Signal Bool
        bigLedgerPeersInEstablished =
          (\bigLedgerPeers established inProgressPromoteCold lsj ->
            case lsj of
              YoungEnough ->
                not . Set.null $
                (bigLedgerPeers `Set.intersection`
                  (established `Set.union` inProgressPromoteCold))
              TooOld -> True
          ) <$> govBigLedgerPeersSig
            <*> govEstablishedPeersSig
            <*> govInProgressPromoteColdSig
            <*> govLedgerStateJudgement

        meaning :: Bool -> String
        meaning False = "No BigLedgerPeers in Established Set"
        meaning True  = "BigLedgerPeers in Established Set"

        valuesList :: [String]
        valuesList = map (meaning . snd)
                   . Signal.eventsToList
                   . Signal.toChangeEvents
                   $ bigLedgerPeersInEstablished

     in checkCoverage
      $ coverTable "established big ledger peers"
                   [("BigLedgerPeers in Established Set", 1)]
      $ tabulate "established big ledger peers" valuesList
      $ True


-- | A variant of 'prop_governor_target_active_below' but for checking if any
-- number of public root peers becomes active, since there's no target for
-- how many public root peers should be active.
--
prop_governor_target_active_public :: MaxTime -> GovernorMockEnvironment -> Property
prop_governor_target_active_public (MaxTime maxTime) env =
    let events = Signal.eventsFromListUpToTime maxTime
               . selectPeerSelectionTraceEvents
               . runGovernorInMockEnvironment
               $ env

        govPublicRootPeersSig :: Signal (Set PeerAddr)
        govPublicRootPeersSig =
          selectGovState (PublicRootPeers.toSet . Governor.publicRootPeers) events

        govActivePeersSig :: Signal (Set PeerAddr)
        govActivePeersSig =
          selectGovState Governor.activePeers events

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

-- | The main progress property for known peers: that we make progress towards
-- the target for known peers from below. See 'prop_governor_target_known_above'
-- for the (simpler) corresponding property for hitting the target from above.
--
-- Intuitively the property we want is that the governor either hits its target
-- for the number of known peers, or gets as close as reasonably possible. The
-- environment may be such that it prevents the governor from reaching its
-- target, e.g. because the target is too high, or not all peers may be
-- reachable by the peer share graph.
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
-- 2. If the governor is below target and has the opportunity to peer share then
--    within a bounded time it should perform a share request with one of its
--    established peers.
--
--    This is the primary progress property. It is a relatively weak property:
--    we do not require that progress is actually made, just that opportunities
--    for progress are taken when available. We cannot always demand actual
--    progress since there are environments where it is not possible to make
--    progress, even though opportunities for peer sharing remain available.
--    Examples include environments where the total set of peers in the graph
--    is less than the target for known peers.
--
-- 3. The governor should not peer share too frequently with any individual peer,
--    except when the governor forgets known peers.
--
--    This is both useful in its own right, but it also helps to strengthen the
--    primary property by helping to ensure that the choices of which peers to
--    ask to are reasonable. In the primary property we do not require that
--    the peer the governor chooses to peer share with is one of the opportunities
--    as defined by the property. We do not require this because the set of
--    opportunities is a lower bound not an upper bound, and trying to make it a
--    tight bound becomes complex and over-specifies behaviour.
--    There is the danger however that the governor could appear to try to make
--    progress by peer sharing but always picking useless choices that avoid
--    making actual progress. By requiring that the governor not peer share with
--    any individual peer too often we can shrink the set of peers the governor can
--    choose and thus force the governor to eventually pick other peers to
--    peer share with, which should mean the governor eventually picks peers that
--    can enable progress.
--
-- 4. When the governor does perform a peer sharing request, within a bounded
--    time it should include the results into its known peer set, or the known
--    peer set should reach its target size.
--
--    This helps to strengthen the primary progress property by ensuring the
--    results of peer sharing are used to make progress when that is possible.
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
-- Property 2 tells us that we eventually peer share with some peer, but
-- does not by itself establish that we make progress in a bounded measure.
-- Property 3 gives us the bounded measure. Property 3 gives us a set of peers
-- that we have not peer shared with recently. When the governor does peer share
-- with a peer then it is removed from this set (but scheduled to be added back
-- some time later). So the measure is the size of this set of peers. It is
-- clearly bounded below by the empty set. So the combination of 2 and 3 tells
-- us we make progress in this bounded measure, but that does not directly
-- translate into increasing the size of the known peers set. Properties 4 and 5
-- tell us that progress with peer sharing will eventually translate into
-- increasing the size of the known peers set if that is possible.
--
-- There is one known wrinkle to this argument to do with property 3 that when
-- the governor peer shares with a peer it is removed from the tracking set
-- however it gets added back some time later. If they get added back too soon
-- then it would undermine the progress argument because it would break the
-- argument about decreasing the bounded measure. This is readily solved
-- however: we simply need to make sure the time scale for peer sharing
-- frequency is relatively long, and the other progress bounds are relatively
-- short.
--
prop_governor_target_known_below :: MaxTime -> GovernorMockEnvironment -> Property
prop_governor_target_known_below maxTime env =
      counterexample "invalid subset"
      (prop_governor_target_known_1_valid_subset      maxTime env)
 .&&. counterexample "opportunity not taken"
      (prop_governor_target_known_2_opportunity_taken maxTime env)
 .&&. counterexample "too chatty"
      (prop_governor_target_known_3_not_too_chatty    maxTime env)
 .&&. counterexample "not used results"
      (prop_governor_target_known_4_results_used      maxTime env)
 .&&. counterexample "shrinked below"
      (prop_governor_target_known_5_no_shrink_below   maxTime env)

prop_governor_target_known_big_ledger_peers_below :: MaxTime -> GovernorMockEnvironment -> Property
prop_governor_target_known_big_ledger_peers_below maxTime env =
      counterexample "shrinked big ledger peers below"
      (prop_governor_target_known_5_no_shrink_big_ledger_peers_below maxTime env)

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
prop_governor_target_known_1_valid_subset :: MaxTime
                                          -> GovernorMockEnvironment
                                          -> Property
prop_governor_target_known_1_valid_subset (MaxTime maxTime) env =
    let events = Signal.eventsFromListUpToTime maxTime
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
                  TraceLocalRootPeersChanged _ x   -> Just (LocalRootPeers.keysSet x)
                  TracePublicRootsResults x _ _    -> Just (PublicRootPeers.toSet x)
                  TraceBigLedgerPeersResults x _ _ -> Just x
                  TracePeerShareResultsFiltered x  -> Just (Set.fromList x)
                  _                                -> Nothing
              )
          . selectGovEvents
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


-- | If the governor is below target and has the opportunity to peer share then
-- within a bounded time it should perform a peer sharing request with one of its
-- established peers, unless there isn't any available.
--
-- We derive a number of signals:
--
-- 1. A signal of the target for known peers from the environment
--
-- 2. A signal of the set of established peers in the governor state.
--
-- 3. A signal of the set of established peers in the governor state.
--
-- 4. A signal of the environment peer sharing request events.
--
-- 5. A signal of the set of peers with which the governor has peer shared
--    recently, based on the requests to the environment
--
-- 6. Based on 2 and 3, a signal of the set of peer sharing opportunities: the
--    current established peers that are not in the recent peer share set.
--
-- 7. Based on 1, 2, 4 and 5, a signal that becomes False if for 30 seconds:
--    the number of known peers is below target; the set of opportunities is
--    non empty; and no peer share request event has occurred.
--
-- Based on these signals we check:
--
-- * That the signal 6 remains True at all times.
--
prop_governor_target_known_2_opportunity_taken :: MaxTime
                                               -> GovernorMockEnvironment
                                               -> Property
prop_governor_target_known_2_opportunity_taken (MaxTime maxTime) env =

    let events = Signal.eventsFromListUpToTime maxTime
               . selectPeerSelectionTraceEvents
               . runGovernorInMockEnvironment
               $ env

        govTargetsSig :: Signal Int
        govTargetsSig =
          selectGovState (targetNumberOfKnownPeers . Governor.targets) events

        govKnownPeersSig :: Signal (Set PeerAddr)
        govKnownPeersSig =
          selectGovState (KnownPeers.toSet . Governor.knownPeers) events

        -- Available Established Peers are those who have correct PeerSharing
        -- permissions
        govAvailableEstablishedPeersSig :: Signal (Set PeerAddr)
        govAvailableEstablishedPeersSig =
          selectGovState
            (\x ->
              KnownPeers.getPeerSharingRequestPeers
                (EstablishedPeers.availableForPeerShare
                                    (Governor.establishedPeers x)
                Set.\\ (Governor.inProgressDemoteToCold x))
                (Governor.knownPeers x))
                events

        -- Note that we only require that the governor try to peer share, it does
        -- not have to succeed.
        envPeerSharesEventsAsSig :: Signal (Maybe PeerAddr)
        envPeerSharesEventsAsSig =
            Signal.fromEvents
          . Signal.selectEvents
              (\case TraceEnvPeerShareRequest addr _ -> Just addr
                     _                               -> Nothing)
          . selectEnvEvents
          $ events

        envPeerShareUnavailableSig :: Signal (Set PeerAddr)
        envPeerShareUnavailableSig =
            Signal.keyedLinger
              -- peers are unavailable for peer sharing for at least an
              -- hour after each peer sharing interaction
              (60 * 60)
              (maybe Set.empty Set.singleton)
              envPeerSharesEventsAsSig

        govLedgerStateJudgementSig :: Signal LedgerStateJudgement
        govLedgerStateJudgementSig =
          selectGovState Governor.ledgerStateJudgement events

        govUseBootstrapPeersSig :: Signal UseBootstrapPeers
        govUseBootstrapPeersSig =
          selectGovState Governor.bootstrapPeersFlag events

        -- We define the governor's peer sharing opportunities at any point in time
        -- to be the governor's set of established peers, less the ones we can see
        -- that it has peer shared with recently.
        --
        peerShareOpportunitiesSig :: Signal (Set PeerAddr)
        peerShareOpportunitiesSig =
          (Set.\\) <$> govAvailableEstablishedPeersSig
                   <*> envPeerShareUnavailableSig

        -- The signal of all the things of interest for this property.
        -- This is used to compute the final predicate, and is also what
        -- we want to report if there is a property violation.
        combinedSig :: Signal (Int,
                               Set PeerAddr,
                               Set PeerAddr,
                               Maybe PeerAddr,
                               LedgerStateJudgement,
                               UseBootstrapPeers
                              )
        combinedSig =
          (,,,,,) <$> govTargetsSig
                  <*> govKnownPeersSig
                  <*> peerShareOpportunitiesSig
                  <*> envPeerSharesEventsAsSig
                  <*> govLedgerStateJudgementSig
                  <*> govUseBootstrapPeersSig

        -- This is the ultimate predicate signal
        peerShareOpportunitiesOkSig :: Signal Bool
        peerShareOpportunitiesOkSig =
          Signal.truncateAt (Time (60 * 60 * 10)) $
          governorEventuallyTakesPeerShareOpportunities (peerSharingFlag env) combinedSig

     in counterexample
          "Signal key: (target, known peers, opportunities, peer share event)" $

        -- Check the predicate signal but for failures report the input signal
        signalProperty 20 (show . snd) fst $
          (,) <$> peerShareOpportunitiesOkSig
              <*> combinedSig


governorEventuallyTakesPeerShareOpportunities
  :: PeerSharing
  -> Signal (Int, Set PeerAddr, Set PeerAddr, Maybe PeerAddr, LedgerStateJudgement, UseBootstrapPeers)
  -> Signal Bool
governorEventuallyTakesPeerShareOpportunities peerSharing =
    -- Time out and fail after 30 seconds if we enter and remain in a bad state
    fmap not
  . Signal.timeout timeLimit badState
  where
    timeLimit :: DiffTime
    timeLimit = 30

    badState (target, govKnownPeers, peerShareOpportunities, peerShareEvent, ledgerState, useBootstrapPeersFlag) =

        -- A bad state is one where we are below target;
        Set.size govKnownPeers < target

        -- where we do have opportunities; and
     && not (Set.null peerShareOpportunities)

        -- are not performing an action to take the opportunity.
     && isNothing peerShareEvent

        -- Peer Sharing must be enabled
     && peerSharing /= PeerSharingDisabled

       -- ledger state should be caught up state
     && not (requiresBootstrapPeers useBootstrapPeersFlag ledgerState)

        -- Note that if a peer share does take place, we do /not/ require
        -- the peer sharing target to be a member of the peerShareOpportunities.
        -- This is because the peer sharing opportunities set is a lower bound
        -- not an upper bound. There is a separate property to check that we do
        -- not peer share too frequently with any individual peer.



-- | The governor should not peer share too frequently with any individual peer,
-- except when the governor demotes an established peer or there's an
-- asynchronous demotion.
--
-- We derive a number of signals:
--
-- Based on these signals we check:
--
prop_governor_target_known_3_not_too_chatty :: MaxTime
                                            -> GovernorMockEnvironment
                                            -> Property
prop_governor_target_known_3_not_too_chatty (MaxTime maxTime) env =
    let events = Signal.eventsFromListUpToTime maxTime
               . selectPeerSelectionTraceEvents
               . runGovernorInMockEnvironment
               $ env

        peerShareOk Nothing      _           = True
        peerShareOk (Just peers) unavailable =
          Set.null (peers `Set.intersection` unavailable)

     in signalProperty 20 show (uncurry peerShareOk) $
          recentPeerShareActivity 3600 events


recentPeerShareActivity :: DiffTime
                        -> Events TestTraceEvent
                        -> Signal (Maybe (Set PeerAddr), Set PeerAddr)
recentPeerShareActivity d =
    Signal.fromChangeEvents (Nothing, Set.empty)
  . Signal.primitiveTransformEvents (go Set.empty PSQ.empty)
    --TODO: we should be able to avoid primitiveTransformEvents and express
    -- this as some combo of keyed linger and keyed until.
  where
    go :: Set PeerAddr -- ^ Recently shared with peers
       -> PSQ.OrdPSQ PeerAddr Time () -- ^ PSQ with next time to request to peers
       -> [E TestTraceEvent]
       -> [E (Maybe (Set PeerAddr), Set PeerAddr)]
    go !recentSet !recentPSQ txs@(E (TS t _) _ : _)
      | Just (k, t', _, recentPSQ') <- PSQ.minView recentPSQ
      , t' <= t
      , let recentSet' = Set.delete k recentSet
      = E (TS t' 0) (Nothing, recentSet')
      : go recentSet' recentPSQ' txs

    -- When we see a peer sharing request we add it to the recent set and
    -- schedule it to be removed again at time d+t. We arrange for the change in
    -- the recent set to happen after the peer sharing event.
    go !recentSet !recentPSQ
        (E (TS t i) (GovernorEvent (TracePeerShareRequests _ _ _ _ addrs)) : txs) =
      let recentSet' = recentSet <> addrs
          recentPSQ' = foldl' (\q a -> PSQ.insert a t' () q) recentPSQ addrs
          t'         = d `addTime` t
       in E (TS t i)     (Just addrs, recentSet)
        : E (TS t (i+1)) (Nothing, recentSet') -- updated in next change at same time
        : go recentSet' recentPSQ' txs

    -- When the governor demotes an established peer, we drop it from
    -- the recent activity tracking, which means if it is added back again
    -- later then we can peer share with it again earlier than the normal limit.
    --
    -- Alternatively we could track this more coarsely by dropping all tracking
    -- when the targets are adjusted downwards, but we use small target
    -- adjustments to perform churn.
    --
    -- There is a separate property to check that the governor does not demote
    -- peers unnecessarily.
    --
    go !recentSet !recentPSQ
        (E t (GovernorEvent (TraceDemoteWarmDone _ _ addr)) : txs) =
      let recentSet' = Set.delete addr recentSet
          recentPSQ' = PSQ.delete addr recentPSQ
       in E t (Nothing, recentSet')
        : go recentSet' recentPSQ' txs

    -- Like above but for big ledger peers.
    go !recentSet !recentPSQ
        (E t (GovernorEvent (TraceDemoteWarmBigLedgerPeerDone _ _ addr)) : txs) =
      let recentSet' = Set.delete addr recentSet
          recentPSQ' = PSQ.delete addr recentPSQ
       in E t (Nothing, recentSet')
        : go recentSet' recentPSQ' txs

    -- When the governor demotes a local established peer, we drop it from
    -- the recent activity tracking, which means if it is added back again
    -- later then we can peer share with it again earlier than the normal limit.
    --
    -- Alternatively we could track this more coarsely by dropping all tracking
    -- when the targets are adjusted downwards, but we use small target
    -- adjustments to perform churn.
    --
    -- There is a separate property to check that the governor does not demote
    -- peers unnecessarily.
    --
    go !recentSet !recentPSQ
        (E t (GovernorEvent (TraceDemoteLocalAsynchronous m)) : txs) =
      let peersDemotedToCold = Map.foldrWithKey'
                                (\k v r -> case v of
                                  (PeerCold, _)    -> k : r
                                  (PeerCooling, _) -> k : r
                                  _                -> r
                                ) [] m
          recentSet' = foldl' (flip Set.delete) recentSet peersDemotedToCold
          recentPSQ' = foldl' (flip PSQ.delete) recentPSQ peersDemotedToCold
       in E t (Nothing, recentSet')
        : go recentSet' recentPSQ' txs

    -- When the governor demotes an non-local established peer, we drop it from
    -- the recent activity tracking, which means if it is added back again
    -- later then we can peer share with it again earlier than the normal limit.
    --
    -- Alternatively we could track this more coarsely by dropping all tracking
    -- when the targets are adjusted downwards, but we use small target
    -- adjustments to perform churn.
    --
    -- There is a separate property to check that the governor does not demote
    -- peers unnecessarily.
    --
    go !recentSet !recentPSQ
        (E t (GovernorEvent (TraceDemoteAsynchronous m)) : txs) =
      let peersDemotedToCold = Map.foldrWithKey'
                                (\k v r -> case v of
                                  (PeerCold, _)    -> k : r
                                  (PeerCooling, _) -> k : r
                                  _                -> r
                                ) [] m
          recentSet' = foldl' (flip Set.delete) recentSet peersDemotedToCold
          recentPSQ' = foldl' (flip PSQ.delete) recentPSQ peersDemotedToCold
       in E t (Nothing, recentSet')
        : go recentSet' recentPSQ' txs

    -- As above but a big ledger peer
    go !recentSet !recentPSQ
        (E t (GovernorEvent (TraceDemoteBigLedgerPeersAsynchronous m)) : txs) =
      let peersDemotedToCold = Map.foldrWithKey'
                                (\k v r -> case v of
                                  (PeerCold, _)    -> k : r
                                  (PeerCooling, _) -> k : r
                                  _                -> r
                                ) [] m
          recentSet' = foldl' (flip Set.delete) recentSet peersDemotedToCold
          recentPSQ' = foldl' (flip PSQ.delete) recentPSQ peersDemotedToCold
       in E t (Nothing, recentSet')
        : go recentSet' recentPSQ' txs

    go !recentSet !recentPSQ
        (E t (GovernorEvent (TracePromoteWarmBigLedgerPeerFailed _ _ addr _)) : txs) =
      let recentSet' = Set.delete addr recentSet
          recentPSQ' = PSQ.delete addr recentPSQ
       in E t (Nothing, recentSet')
        : go recentSet' recentPSQ' txs

    go !recentSet !recentPSQ
        (E t (GovernorEvent (TracePromoteWarmFailed _ _ addr _)) : txs) =
      let recentSet' = Set.delete addr recentSet
          recentPSQ' = PSQ.delete addr recentPSQ
       in E t (Nothing, recentSet')
        : go recentSet' recentPSQ' txs

    go !recentSet !recentPSQ
        (E t (GovernorEvent (TraceDemoteHotFailed _ _ addr _)) : txs) =
      let recentSet' = Set.delete addr recentSet
          recentPSQ' = PSQ.delete addr recentPSQ
       in E t (Nothing, recentSet')
        : go recentSet' recentPSQ' txs

    go !recentSet !recentPSQ
        (E t (GovernorEvent (TraceDemoteWarmFailed _ _ addr _)) : txs) =
      let recentSet' = Set.delete addr recentSet
          recentPSQ' = PSQ.delete addr recentPSQ
       in E t (Nothing, recentSet')
        : go recentSet' recentPSQ' txs

    go !recentSet !recentPSQ
        (E t (GovernorEvent (TraceDemoteHotBigLedgerPeerFailed _ _ addr _)) : txs) =
      let recentSet' = Set.delete addr recentSet
          recentPSQ' = PSQ.delete addr recentPSQ
       in E t (Nothing, recentSet')
        : go recentSet' recentPSQ' txs

    go !recentSet !recentPSQ
        (E t (GovernorEvent (TraceDemoteWarmBigLedgerPeerFailed _ _ addr _)) : txs) =
      let recentSet' = Set.delete addr recentSet
          recentPSQ' = PSQ.delete addr recentPSQ
       in E t (Nothing, recentSet')
        : go recentSet' recentPSQ' txs

    go !recentSet !recentPSQ (_ : txs) =
      go recentSet recentPSQ txs

    go !_ !_ [] = []


-- | When the governor does perform a peer sharing request, within a bounded time
-- it should include the results into its known peer set, or the known peer set
-- should reach its target size.
--
-- We derive a number of signals:
--
-- 1. A signal of the target for known peers from the environment
--
-- 2. A signal of the set of known peers in the governor state.
--
-- 3. A signal of the environment peer sharing result events, as the set of
--    results at any point in time.
--
-- 4. Based on 1, 2 and 3, a signal that tracks a set of peers that we have
--    peer shared with, such that the peers remain in the set until either
--    they appear in the governor known peers set or until the known peer set
--    reaches its target size.
--
-- 5. Based on 4, a signal of the subset of elements that have been a member
--    continuously for at least X seconds duration.
--
-- Based on these signals we assert:
--
-- * That the signal 4 above is always empty.
--
prop_governor_target_known_4_results_used :: MaxTime -> GovernorMockEnvironment -> Property
prop_governor_target_known_4_results_used (MaxTime maxTime) env =
    let events = Signal.eventsFromListUpToTime maxTime
               . selectPeerSelectionTraceEvents
               . runGovernorInMockEnvironment
               $ env

        govTargetsSig :: Signal Int
        govTargetsSig =
          selectGovState (targetNumberOfKnownPeers . Governor.targets) events

        govKnownPeersSig :: Signal (Set PeerAddr)
        govKnownPeersSig =
          selectGovState (KnownPeers.toSet . Governor.knownPeers) events

        envPeerShareResultsSig :: Signal (Set PeerAddr)
        envPeerShareResultsSig =
            fmap (maybe Set.empty Set.fromList)
          . Signal.fromEvents
          . Signal.selectEvents
              (\case TracePeerShareResultsFiltered addrs -> Just addrs
                     _                                   -> Nothing)
          . selectGovEvents
          $ events

        peerShareResultsUntilKnown :: Signal (Set PeerAddr)
        peerShareResultsUntilKnown =
          Signal.keyedUntil
            (\(_, _, peerShares)    -> peerShares) -- start set
            (\(_, known, _)      -> known)   -- stop set
            (\(target, known, _) -> Set.size known <= target) -- reset condition
            ((,,) <$> govTargetsSig
                  <*> govKnownPeersSig
                  <*> envPeerShareResultsSig)

        peerShareResultsUnknownTooLong :: Signal (Set PeerAddr)
        peerShareResultsUnknownTooLong =
          Signal.keyedTimeout
            (10 + 1) -- policyPeerShareOverallTimeout
            id
            peerShareResultsUntilKnown

     in counterexample
          ("\nSignal key: (known peers, peer share result, results unknown, " ++
           "results unknown too long)") $

        signalProperty 20 show
          (\(_,_,_,_,x) -> Set.null x) $
          (,,,,) <$> govTargetsSig
                 <*> govKnownPeersSig
                 <*> envPeerShareResultsSig
                 <*> peerShareResultsUntilKnown
                 <*> peerShareResultsUnknownTooLong


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
prop_governor_target_known_5_no_shrink_below :: MaxTime -> GovernorMockEnvironment -> Property
prop_governor_target_known_5_no_shrink_below (MaxTime maxTime) env =
    let events = Signal.eventsFromListUpToTime maxTime
               . selectPeerSelectionTraceEvents
               . runGovernorInMockEnvironment
               $ env

        govTargetsSig :: Signal Int
        govTargetsSig =
          selectGovState (targetNumberOfKnownPeers . Governor.targets) events

        govKnownPeersSig :: Signal (Set PeerAddr)
        govKnownPeersSig =
          selectGovState (KnownPeers.toSet . Governor.knownPeers) events

        bigLedgerPeersSig :: Signal (Set PeerAddr)
        bigLedgerPeersSig =
          selectGovState (PublicRootPeers.getBigLedgerPeers . Governor.publicRootPeers)
                         events

        bootstrapPeersSig :: Signal (Set PeerAddr)
        bootstrapPeersSig =
          selectGovState (PublicRootPeers.getBootstrapPeers . Governor.publicRootPeers)
                         events

        knownPeersShrinksSig :: Signal (Set PeerAddr)
        knownPeersShrinksSig =
            Signal.nub
          . fmap (fromMaybe Set.empty)
          . Signal.difference
              -- We subtract all big ledger peers.  This is because we might
              -- first satisfy the target of known peers, and then learn that
              -- one of them was a big ledger peers. We also subtract
              -- bootstrap peers. This would be a fake shrink of known non
              -- big ledger peers.
              --
              -- By subtracting a sum of `y` and `y'` we also do not account
              -- forgetting big ledger peers.
              (\(x,y,z) (x',y',z') -> x Set.\\ x' Set.\\ y Set.\\ y' Set.\\ z Set.\\ z')
          $ (,,) <$> govKnownPeersSig
                 <*> bigLedgerPeersSig
                 <*> bootstrapPeersSig

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
          ) <$> govTargetsSig
            <*> govKnownPeersSig
            <*> knownPeersShrinksSig

     in counterexample
          "\nSignal key: (target, known peers, shrinks, unexpected)" $

        signalProperty 20 show
          (\(_,_,_,unexpected) -> not unexpected)
          ((,,,) <$> govTargetsSig
                 <*> govKnownPeersSig
                 <*> knownPeersShrinksSig
                 <*> unexpectedShrink)

-- | Like 'prop_governor_target_known_5_no_shrink_below' but for big ledger
-- peers.
--
prop_governor_target_known_5_no_shrink_big_ledger_peers_below :: MaxTime -> GovernorMockEnvironment -> Property
prop_governor_target_known_5_no_shrink_big_ledger_peers_below (MaxTime maxTime) env =
    let events = Signal.eventsFromListUpToTime maxTime
               . selectPeerSelectionTraceEvents
               . runGovernorInMockEnvironment
               $ env

        govTargetsSig :: Signal Int
        govTargetsSig =
          selectGovState (targetNumberOfKnownBigLedgerPeers . Governor.targets) events

        govKnownPeersSig :: Signal (Set PeerAddr)
        govKnownPeersSig =
          selectGovState (takeBigLedgerPeers $
                            KnownPeers.toSet . Governor.knownPeers) events

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
          ) <$> govTargetsSig
            <*> govKnownPeersSig
            <*> knownPeersShrinksSig

     in counterexample
          "\nSignal key: (target, known peers, shrinks, unexpected)" $

        signalProperty 20 show
          (\(_,_,_,unexpected) -> not unexpected)
          ((,,,) <$> govTargetsSig
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
prop_governor_target_known_above :: MaxTime -> GovernorMockEnvironment -> Property
prop_governor_target_known_above (MaxTime maxTime) env =
    let events = Signal.eventsFromListUpToTime maxTime
               . selectPeerSelectionTraceEvents
               . runGovernorInMockEnvironment
               $ env

        govTargetsSig :: Signal PeerSelectionTargets
        govTargetsSig =
          selectGovState Governor.targets events

        govLocalRootPeersSig :: Signal (Set PeerAddr)
        govLocalRootPeersSig =
          selectGovState (LocalRootPeers.keysSet . Governor.localRootPeers)
                         events

        govPublicRootPeersSig :: Signal (Set PeerAddr)
        govPublicRootPeersSig =
          selectGovState (PublicRootPeers.toSet . Governor.publicRootPeers) events

        govKnownPeersSig :: Signal (Set PeerAddr)
        govKnownPeersSig =
          selectGovState (dropBigLedgerPeers $
                            KnownPeers.toSet . Governor.knownPeers)
                         events

        govEstablishedPeersSig :: Signal (Set PeerAddr)
        govEstablishedPeersSig =
          selectGovState (dropBigLedgerPeers $
                            EstablishedPeers.toSet . Governor.establishedPeers)
                         events

        -- There are no demotion opportunities if we're at or below target.
        -- Otherwise, the opportunities for demotion are known peers that
        -- are not currently established and are not local.
        --
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

        demotionOpportunities :: Signal (Set PeerAddr)
        demotionOpportunities =
          demotionOpportunity
            <$> govTargetsSig
            <*> govLocalRootPeersSig
            <*> govPublicRootPeersSig
            <*> govKnownPeersSig
            <*> govEstablishedPeersSig

        demotionOpportunitiesIgnoredTooLong :: Signal (Set PeerAddr)
        demotionOpportunitiesIgnoredTooLong =
          Signal.keyedTimeout
            10 -- seconds
            id
            demotionOpportunities

     in counterexample
          ("\nSignal key: (target (root, known), local peers, public peers, known peers, " ++
           "established peers, demotion opportunities, ignored too long)") $

        signalProperty 20 show
          (\(_,_,_,_,_,_,toolong) -> Set.null toolong)
          ((,,,,,,) <$> ((\t -> (targetNumberOfRootPeers t,
                                 targetNumberOfKnownPeers t)) <$> govTargetsSig)
                    <*> govLocalRootPeersSig
                    <*> govPublicRootPeersSig
                    <*> govKnownPeersSig
                    <*> govEstablishedPeersSig
                    <*> demotionOpportunities
                    <*> demotionOpportunitiesIgnoredTooLong)


-- | Like 'prop_governor_target_known_above' but for big ledger peers.
--
prop_governor_target_known_big_ledger_peers_above
  :: MaxTime -> GovernorMockEnvironment -> Property
prop_governor_target_known_big_ledger_peers_above (MaxTime maxTime) env =
    let events = Signal.eventsFromListUpToTime maxTime
               . selectPeerSelectionTraceEvents
               . runGovernorInMockEnvironment
               $ env

        govTargetsSig :: Signal PeerSelectionTargets
        govTargetsSig =
          selectGovState Governor.targets events

        govKnownPeersSig :: Signal (Set PeerAddr)
        govKnownPeersSig =
          selectGovState (takeBigLedgerPeers $
                            KnownPeers.toSet . Governor.knownPeers)
                         events

        govEstablishedPeersSig :: Signal (Set PeerAddr)
        govEstablishedPeersSig =
          selectGovState (takeBigLedgerPeers $
                            EstablishedPeers.toSet . Governor.establishedPeers)
                         events

        -- There are no demotion opportunities if we're at or below target.
        -- Otherwise, the opportunities for demotion are known peers that
        -- are not currently established and are not local.
        --
        demotionOpportunity targets known established
          | Set.size known <= targetNumberOfKnownBigLedgerPeers targets
          = Set.empty

         | otherwise
         = known Set.\\ established

        demotionOpportunities :: Signal (Set PeerAddr)
        demotionOpportunities =
          demotionOpportunity
            <$> govTargetsSig
            <*> govKnownPeersSig
            <*> govEstablishedPeersSig

        demotionOpportunitiesIgnoredTooLong :: Signal (Set PeerAddr)
        demotionOpportunitiesIgnoredTooLong =
          Signal.keyedTimeout
            10 -- seconds
            id
            demotionOpportunities

     in counterexample
          ("\nSignal key: (target (root, known), local peers, public peers, known peers, " ++
           "established peers, demotion opportunities, ignored too long)") $

        signalProperty 20 show
          (\(_,_,_,_,toolong) -> Set.null toolong)
          ((,,,,) <$> ((\t -> (targetNumberOfRootPeers t,
                                 targetNumberOfKnownPeers t)) <$> govTargetsSig)
                    <*> govKnownPeersSig
                    <*> govEstablishedPeersSig
                    <*> demotionOpportunities
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
prop_governor_target_established_below :: MaxTime -> GovernorMockEnvironment -> Property
prop_governor_target_established_below (MaxTime maxTime) env =
    let events = Signal.eventsFromListUpToTime maxTime
               . selectPeerSelectionTraceEvents
               . runGovernorInMockEnvironment
               $ env

        govTargetsSig :: Signal Int
        govTargetsSig =
          selectGovState (targetNumberOfEstablishedPeers . Governor.targets) events

        govKnownPeersSig :: Signal (Set PeerAddr)
        govKnownPeersSig =
          selectGovState (dropBigLedgerPeers $
                            KnownPeers.toSet . Governor.knownPeers) events

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
                       Just $! Set.singleton peer
                     --TODO: what about TraceDemoteWarmDone ?
                     -- these are also not immediate candidates
                     -- why does the property not fail for not tracking these?
                     TraceDemoteAsynchronous status
                       | Set.null failures -> Nothing
                       | otherwise         -> Just failures
                       where
                         !failures = Map.keysSet (Map.filter (==PeerCooling) . fmap fst $ status)
                     TraceDemoteLocalAsynchronous status
                       | Set.null failures -> Nothing
                       | otherwise         -> Just failures
                       where
                         !failures = Map.keysSet (Map.filter (==PeerCooling) . fmap fst $ status)
                     TracePromoteWarmFailed _ _ peer _ ->
                       Just $! Set.singleton peer
                     TraceDemoteWarmFailed _ _ peer _ ->
                       Just $! Set.singleton peer
                     TraceDemoteHotFailed _ _ peer _ ->
                       Just $! Set.singleton peer
                     _ -> Nothing
              )
          . selectGovEvents
          $ events

        -- There are no opportunities if we're at or above target
        --
        promotionOpportunity target known established recentFailures
          | Set.size established >= target
          = Set.empty

          | otherwise
          = known Set.\\ established
                  Set.\\ recentFailures

        promotionOpportunities :: Signal (Set PeerAddr)
        promotionOpportunities =
          promotionOpportunity
            <$> govTargetsSig
            <*> govKnownPeersSig
            <*> govEstablishedPeersSig
            <*> govEstablishedFailuresSig

        promotionOpportunitiesIgnoredTooLong :: Signal (Set PeerAddr)
        promotionOpportunitiesIgnoredTooLong =
          Signal.keyedTimeout
            (repromoteDelay config_REPROMOTE_DELAY + 20) -- seconds
            id
            promotionOpportunities

     in counterexample
          ("\nSignal key: (target, known peers, established peers, recent failures, " ++
           "opportunities, ignored too long)") $

        signalProperty 20 show
          (\(_,_,_,_,_,toolong) -> Set.null toolong)
          ((,,,,,) <$> govTargetsSig
                   <*> govKnownPeersSig
                   <*> govEstablishedPeersSig
                   <*> govEstablishedFailuresSig
                   <*> promotionOpportunities
                   <*> promotionOpportunitiesIgnoredTooLong)

-- | A version of the `prop_governor_target_established_below` for big ledger
-- peers.
--
prop_governor_target_established_big_ledger_peers_below
    :: MaxTime -> GovernorMockEnvironment -> Property
prop_governor_target_established_big_ledger_peers_below (MaxTime maxTime) env =
    let events = Signal.eventsFromListUpToTime maxTime
               . selectPeerSelectionTraceEvents
               . runGovernorInMockEnvironment
               $ env

        govTargetsSig :: Signal Int
        govTargetsSig =
          selectGovState (targetNumberOfEstablishedBigLedgerPeers . Governor.targets) events

        govKnownPeersSig :: Signal (Set PeerAddr)
        govKnownPeersSig =
          selectGovState (takeBigLedgerPeers $
                           KnownPeers.toSet . Governor.knownPeers)
                         events

        govEstablishedPeersSig :: Signal (Set PeerAddr)
        govEstablishedPeersSig =
          selectGovState
            (takeBigLedgerPeers $
              EstablishedPeers.toSet . Governor.establishedPeers)
            events

        govEstablishedFailuresSig :: Signal (Set PeerAddr)
        govEstablishedFailuresSig =
            Signal.keyedLinger
              180 -- 3 minutes  -- TODO: too eager to reconnect?
              (fromMaybe Set.empty)
          . Signal.fromEvents
          . Signal.selectEvents
              (\case TracePromoteColdBigLedgerPeerFailed _ _ peer _ _ ->
                       --TODO: the environment does not yet cause this to happen
                       -- it requires synchronous failure in the establish action
                       Just (Set.singleton peer)
                     TracePromoteWarmBigLedgerPeerFailed _ _ peer _ ->
                       Just (Set.singleton peer)
                     --TODO: what about TraceDemoteWarmDone ?
                     -- these are also not immediate candidates
                     -- why does the property not fail for not tracking these?
                     TraceDemoteBigLedgerPeersAsynchronous status
                       | Set.null failures -> Nothing
                       | otherwise         -> Just failures
                       where
                         failures = Map.keysSet (Map.filter (==PeerCooling) . fmap fst $ status)
                     TraceDemoteLocalAsynchronous status
                       | Set.null failures -> Nothing
                       | otherwise         -> Just failures
                       where
                         failures = Map.keysSet (Map.filter (==PeerCooling) . fmap fst $ status)
                     TracePromoteWarmFailed _ _ peer _ ->
                       Just (Set.singleton peer)
                     TraceDemoteWarmBigLedgerPeerFailed _ _ peer _ ->
                       Just (Set.singleton peer)
                     TraceDemoteHotBigLedgerPeerFailed _ _ peer _ ->
                       Just (Set.singleton peer)
                     _ -> Nothing
              )
          . selectGovEvents
          $ events

        -- There are no opportunities if we're at or above target
        --
        promotionOpportunity target known established recentFailures
          | Set.size established >= target
          = Set.empty

          | otherwise
          = known Set.\\ established
                  Set.\\ recentFailures

        promotionOpportunities :: Signal (Set PeerAddr)
        promotionOpportunities =
          promotionOpportunity
            <$> govTargetsSig
            <*> govKnownPeersSig
            <*> govEstablishedPeersSig
            <*> govEstablishedFailuresSig

        promotionOpportunitiesIgnoredTooLong :: Signal (Set PeerAddr)
        promotionOpportunitiesIgnoredTooLong =
          Signal.keyedTimeout
            (repromoteDelay config_REPROMOTE_DELAY + 20) -- seconds
            id
            promotionOpportunities

     in counterexample
          ("\nSignal key: (target, known big ledger peers, established big ledger peers, recent failures, " ++
           "opportunities, ignored too long)") $

        -- counterexample (unlines $ fmap show $ Signal.eventsToList events) $

        signalProperty 20 show
          (\(_,_,_,_,_,toolong) -> Set.null toolong)
          ((,,,,,) <$> govTargetsSig
                   <*> govKnownPeersSig
                   <*> govEstablishedPeersSig
                   <*> govEstablishedFailuresSig
                   <*> promotionOpportunities
                   <*> promotionOpportunitiesIgnoredTooLong)


prop_governor_target_active_below :: MaxTime -> GovernorMockEnvironment -> Property
prop_governor_target_active_below (MaxTime maxTime) env =
    let events = Signal.eventsFromListUpToTime maxTime
               . selectPeerSelectionTraceEvents
               . runGovernorInMockEnvironment
               $ env

        govTargetsSig :: Signal Int
        govTargetsSig =
          selectGovState (targetNumberOfActivePeers . Governor.targets) events

        govLocalRootPeersSig :: Signal (LocalRootPeers.LocalRootPeers PeerAddr)
        govLocalRootPeersSig =
          selectGovState Governor.localRootPeers events

        govInProgressDemoteToColdSig :: Signal (Set PeerAddr)
        govInProgressDemoteToColdSig =
          selectGovState Governor.inProgressDemoteToCold events

        govEstablishedPeersSig :: Signal (Set PeerAddr)
        govEstablishedPeersSig =
          selectGovState
            (dropBigLedgerPeers $
               EstablishedPeers.toSet . Governor.establishedPeers)
            events

        govActivePeersSig :: Signal (Set PeerAddr)
        govActivePeersSig =
          selectGovState (dropBigLedgerPeers Governor.activePeers) events

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
                       Just $! Set.singleton peer
                     TraceDemoteWarmFailed _ _ peer _ ->
                       Just $! Set.singleton peer
                     TraceDemoteHotFailed _ _ peer _ ->
                       Just $! Set.singleton peer
                     --TODO
                     TraceDemoteAsynchronous status
                       | Set.null failures -> Nothing
                       | otherwise         -> Just failures
                       where
                         !failures = Map.keysSet (Map.filter (==PeerWarm) . fmap fst $ status)
                     TraceDemoteLocalAsynchronous status
                       | Set.null failures -> Nothing
                       | otherwise         -> Just failures
                       where
                         !failures = Map.keysSet (Map.filter (==PeerWarm) . fmap fst $ status)
                     _ -> Nothing
              )
          . selectGovEvents
          $ events

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
        promotionOpportunity target local established active recentFailures inProgressDemoteToCold
          | Set.size active >= target
          = Set.empty

          | otherwise
          = established Set.\\ active
                        Set.\\ LocalRootPeers.keysSet local
                        Set.\\ recentFailures
                        Set.\\ inProgressDemoteToCold

        promotionOpportunities :: Signal (Set PeerAddr)
        promotionOpportunities =
          promotionOpportunity
            <$> govTargetsSig
            <*> govLocalRootPeersSig
            <*> govEstablishedPeersSig
            <*> govActivePeersSig
            <*> govActiveFailuresSig
            <*> govInProgressDemoteToColdSig

        promotionOpportunitiesIgnoredTooLong :: Signal (Set PeerAddr)
        promotionOpportunitiesIgnoredTooLong =
          Signal.keyedTimeout
            15 -- seconds
            id
            promotionOpportunities

     in counterexample
          ("\nSignal key: (target, local peers, established peers, " ++
           "active peers, recent failures, opportunities, ignored too long)") $

        signalProperty 20 show
          (\(_,_,_,_,_,_,toolong) -> Set.null toolong)
          ((,,,,,,) <$> govTargetsSig
                    <*> govLocalRootPeersSig
                    <*> govEstablishedPeersSig
                    <*> govActivePeersSig
                    <*> govActiveFailuresSig
                    <*> promotionOpportunities
                    <*> promotionOpportunitiesIgnoredTooLong)


-- | A variant of 'prop_governor_target_active_below' but for big ledger peers.
--
prop_governor_target_active_big_ledger_peers_below
  :: MaxTime -> GovernorMockEnvironment -> Property
prop_governor_target_active_big_ledger_peers_below (MaxTime maxTime) env =
    let events = Signal.eventsFromListUpToTime maxTime
               . selectPeerSelectionTraceEvents
               . runGovernorInMockEnvironment
               $ env

        govTargetsSig :: Signal Int
        govTargetsSig =
          selectGovState (targetNumberOfActiveBigLedgerPeers . Governor.targets) events

        govEstablishedPeersSig :: Signal (Set PeerAddr)
        govEstablishedPeersSig =
          selectGovState
            (takeBigLedgerPeers $
              EstablishedPeers.toSet . Governor.establishedPeers)
            events

        govInProgressDemoteToColdSig :: Signal (Set PeerAddr)
        govInProgressDemoteToColdSig =
          selectGovState Governor.inProgressDemoteToCold events

        govActivePeersSig :: Signal (Set PeerAddr)
        govActivePeersSig =
          selectGovState (takeBigLedgerPeers Governor.activePeers) events

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
                     TraceDemoteBigLedgerPeersAsynchronous status
                       | Set.null failures -> Nothing
                       | otherwise         -> Just failures
                       where
                         failures = Map.keysSet (Map.filter (==PeerWarm) . fmap fst $ status)
                     _ -> Nothing
              )
          . selectGovEvents
          $ events

        -- There are no opportunities if we're at or above target.
        --
        promotionOpportunity target established active recentFailures inProgressDemoteToCold
          | Set.size active >= target
          = Set.empty

          | otherwise
          = established Set.\\ active
                        Set.\\ recentFailures
                        Set.\\ inProgressDemoteToCold

        promotionOpportunities :: Signal (Set PeerAddr)
        promotionOpportunities =
          promotionOpportunity
            <$> govTargetsSig
            <*> govEstablishedPeersSig
            <*> govActivePeersSig
            <*> govActiveFailuresSig
            <*> govInProgressDemoteToColdSig

        promotionOpportunitiesIgnoredTooLong :: Signal (Set PeerAddr)
        promotionOpportunitiesIgnoredTooLong =
          Signal.keyedTimeout
            (repromoteDelay config_REPROMOTE_DELAY + 20) -- seconds
            id
            promotionOpportunities

     in counterexample
          ("\nSignal key: (target, established big ledger peers, " ++
           "active peers, recent failures, opportunities, ignored too long)") $

        signalProperty 20 show
          (\(_,_,_,_,_,toolong) -> Set.null toolong)
          ((,,,,,) <$> govTargetsSig
                    <*> govEstablishedPeersSig
                    <*> govActivePeersSig
                    <*> govActiveFailuresSig
                    <*> promotionOpportunities
                    <*> promotionOpportunitiesIgnoredTooLong)


prop_governor_target_established_above :: MaxTime -> GovernorMockEnvironment -> Property
prop_governor_target_established_above (MaxTime maxTime) env =
    let events = Signal.eventsFromListUpToTime maxTime
               . selectPeerSelectionTraceEvents
               . runGovernorInMockEnvironment
               $ env

        govTargetsSig :: Signal Int
        govTargetsSig =
          selectGovState (targetNumberOfEstablishedPeers . Governor.targets) events

        govInProgressDemoteToColdSig :: Signal (Set PeerAddr)
        govInProgressDemoteToColdSig =
          selectGovState Governor.inProgressDemoteToCold events

        govLocalRootPeersSig :: Signal (LocalRootPeers.LocalRootPeers PeerAddr)
        govLocalRootPeersSig =
          selectGovState Governor.localRootPeers events

        govEstablishedPeersSig :: Signal (Set PeerAddr)
        govEstablishedPeersSig =
          selectGovState
            (dropBigLedgerPeers $
               EstablishedPeers.toSet . Governor.establishedPeers)
            events

        govActivePeersSig :: Signal (Set PeerAddr)
        govActivePeersSig =
          selectGovState (dropBigLedgerPeers Governor.activePeers) events

        -- There are no demotion opportunities if we're at or below target.
        -- Otherwise the demotion opportunities are the established peers that
        -- are not active and not local root peers.
        --
        demotionOpportunity target local established active inProgressDemoteToCold
          | Set.size established <= target
          = Set.empty

          | otherwise
          = established Set.\\ active
                        Set.\\ LocalRootPeers.keysSet local
                        Set.\\ inProgressDemoteToCold
        demotionOpportunities :: Signal (Set PeerAddr)
        demotionOpportunities =
          demotionOpportunity
            <$> govTargetsSig
            <*> govLocalRootPeersSig
            <*> govEstablishedPeersSig
            <*> govActivePeersSig
            <*> govInProgressDemoteToColdSig

        demotionOpportunitiesIgnoredTooLong :: Signal (Set PeerAddr)
        demotionOpportunitiesIgnoredTooLong =
          Signal.keyedTimeout
            10 -- seconds
            id
            demotionOpportunities

     in counterexample
          ("\nSignal key: (target, local peers, established peers, active peers, " ++
           "demotion opportunities, ignored too long)") $

        signalProperty 20 show
          (\(_,_,_,_,_,_,toolong) -> Set.null toolong)
          ((,,,,,,) <$> govTargetsSig
                    <*> (LocalRootPeers.toGroupSets <$> govLocalRootPeersSig)
                    <*> govEstablishedPeersSig
                    <*> govActivePeersSig
                    <*> demotionOpportunities
                    <*> govInProgressDemoteToColdSig
                    <*> demotionOpportunitiesIgnoredTooLong)


-- | Like 'prop_governor_target_established_above' but for big ledger peers.
--
prop_governor_target_established_big_ledger_peers_above
    :: MaxTime -> GovernorMockEnvironment -> Property
prop_governor_target_established_big_ledger_peers_above (MaxTime maxTime) env =
    let events = Signal.eventsFromListUpToTime maxTime
               . selectPeerSelectionTraceEvents
               . runGovernorInMockEnvironment
               $ env

        govTargetsSig :: Signal Int
        govTargetsSig =
          selectGovState (targetNumberOfEstablishedBigLedgerPeers . Governor.targets) events

        govEstablishedPeersSig :: Signal (Set PeerAddr)
        govEstablishedPeersSig =
          selectGovState
            (takeBigLedgerPeers $
              EstablishedPeers.toSet . Governor.establishedPeers)
            events

        govInProgressDemoteToColdSig :: Signal (Set PeerAddr)
        govInProgressDemoteToColdSig =
          selectGovState Governor.inProgressDemoteToCold events

        govActivePeersSig :: Signal (Set PeerAddr)
        govActivePeersSig =
          selectGovState (takeBigLedgerPeers Governor.activePeers) events

        -- There are no demotion opportunities if we're at or below target.
        -- Otherwise the demotion opportunities are the established peers that
        -- are not active and not local root peers.
        --
        demotionOpportunity target established active inProgressDemoteToCold
          | Set.size established <= target
          = Set.empty

          | otherwise
          = established Set.\\ active
                        Set.\\ inProgressDemoteToCold
        demotionOpportunities :: Signal (Set PeerAddr)
        demotionOpportunities =
          demotionOpportunity
            <$> govTargetsSig
            <*> govEstablishedPeersSig
            <*> govActivePeersSig
            <*> govInProgressDemoteToColdSig

        demotionOpportunitiesIgnoredTooLong :: Signal (Set PeerAddr)
        demotionOpportunitiesIgnoredTooLong =
          Signal.keyedTimeout
            10 -- seconds
            id
            demotionOpportunities

     in counterexample
          ("\nSignal key: (target, established big ledger peers, active big ledger peers, " ++
           "demotion opportunities, ignored too long)") $

        signalProperty 20 show
          (\(_,_,_,_,toolong) -> Set.null toolong)
          ((,,,,) <$> govTargetsSig
                  <*> govEstablishedPeersSig
                  <*> govActivePeersSig
                  <*> demotionOpportunities
                  <*> demotionOpportunitiesIgnoredTooLong)


prop_governor_target_active_above :: MaxTime -> GovernorMockEnvironment -> Property
prop_governor_target_active_above (MaxTime maxTime) env =
    let events = Signal.eventsFromListUpToTime maxTime
               . selectPeerSelectionTraceEvents
               . runGovernorInMockEnvironment
               $ env

        govTargetsSig :: Signal Int
        govTargetsSig =
          selectGovState (targetNumberOfActivePeers . Governor.targets) events

        govLocalRootPeersSig :: Signal (LocalRootPeers.LocalRootPeers PeerAddr)
        govLocalRootPeersSig =
          selectGovState Governor.localRootPeers events

        govActivePeersSig :: Signal (Set PeerAddr)
        govActivePeersSig =
          selectGovState (dropBigLedgerPeers Governor.activePeers) events

        govInProgressDemoteToColdSig :: Signal (Set PeerAddr)
        govInProgressDemoteToColdSig =
          selectGovState Governor.inProgressDemoteToCold events

        demotionOpportunity target local active inProgressDemoteToCold
          | (Set.size active - Set.size inProgressDemoteToCold) <= target
          = Set.empty

          | otherwise
          = active Set.\\ LocalRootPeers.keysSet local
                   Set.\\ inProgressDemoteToCold

        demotionOpportunities :: Signal (Set PeerAddr)
        demotionOpportunities =
          demotionOpportunity
            <$> govTargetsSig
            <*> govLocalRootPeersSig
            <*> govActivePeersSig
            <*> govInProgressDemoteToColdSig

        demotionOpportunitiesIgnoredTooLong :: Signal (Set PeerAddr)
        demotionOpportunitiesIgnoredTooLong =
          Signal.keyedTimeout
            15 -- seconds
            id
            demotionOpportunities

     in counterexample
          ("\nSignal key: (target, local peers, active peers, " ++
           "demotion opportunities, ignored too long)") $

        signalProperty 20 show
          (\(_,_,_,_,toolong) -> Set.null toolong)
          ((,,,,) <$> govTargetsSig
                  <*> (LocalRootPeers.toGroupSets <$> govLocalRootPeersSig)
                  <*> govActivePeersSig
                  <*> demotionOpportunities
                  <*> demotionOpportunitiesIgnoredTooLong)


-- | Like 'prop_governor_target_active_above' but for big ledger peers.
--
prop_governor_target_active_big_ledger_peers_above
    :: MaxTime -> GovernorMockEnvironment -> Property
prop_governor_target_active_big_ledger_peers_above (MaxTime maxTime) env =
    let events = Signal.eventsFromListUpToTime maxTime
               . selectPeerSelectionTraceEvents
               . runGovernorInMockEnvironment
               $ env

        govTargetsSig :: Signal Int
        govTargetsSig =
          selectGovState (targetNumberOfActiveBigLedgerPeers . Governor.targets) events

        govActivePeersSig :: Signal (Set PeerAddr)
        govActivePeersSig =
          selectGovState (takeBigLedgerPeers Governor.activePeers) events

        demotionOpportunity target active
          | Set.size active <= target
          = Set.empty

          | otherwise
          = active

        demotionOpportunities :: Signal (Set PeerAddr)
        demotionOpportunities =
          demotionOpportunity
            <$> govTargetsSig
            <*> govActivePeersSig

        demotionOpportunitiesIgnoredTooLong :: Signal (Set PeerAddr)
        demotionOpportunitiesIgnoredTooLong =
          Signal.keyedTimeout
            10 -- seconds
            id
            demotionOpportunities

     in counterexample
          ("\nSignal key: (target, active big ledger peers, " ++
           "demotion opportunities, ignored too long)") $

        signalProperty 20 show
          (\(_,_,_,toolong) -> Set.null toolong)
          ((,,,) <$> govTargetsSig
                 <*> govActivePeersSig
                 <*> demotionOpportunities
                 <*> demotionOpportunitiesIgnoredTooLong)


-- | A variant of 'prop_governor_target_established_below' but for the target
-- that all local root peers should become established.
--
-- We do not need separate above and below variants of this property since it
-- is not possible to exceed the target.
--
prop_governor_target_established_local :: MaxTime -> GovernorMockEnvironment -> Property
prop_governor_target_established_local (MaxTime maxTime) env =
    let events = Signal.eventsFromListUpToTime maxTime
               . selectPeerSelectionTraceEvents
               . runGovernorInMockEnvironment
               $ env

        govLocalRootPeersSig :: Signal (LocalRootPeers PeerAddr)
        govLocalRootPeersSig =
          selectGovState Governor.localRootPeers
                         events

        govEstablishedPeersSig :: Signal (Set PeerAddr)
        govEstablishedPeersSig =
          selectGovState
            (EstablishedPeers.toSet . Governor.establishedPeers)
            events

        govInProgressPromoteColdSig :: Signal (Set PeerAddr)
        govInProgressPromoteColdSig =
          selectGovState
            Governor.inProgressPromoteCold
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
                         failures = Map.keysSet (Map.filter (==PeerCooling) . fmap fst $ status)
                     TraceDemoteLocalAsynchronous status
                       | Set.null failures -> Nothing
                       | otherwise         -> Just failures
                       where
                         failures = Map.keysSet (Map.filter (==PeerCooling) . fmap fst $ status)
                     TracePromoteWarmFailed _ _ peer _ ->
                       Just (Set.singleton peer)
                     _ -> Nothing
              )
          . selectGovEvents
          $ events

        promotionOpportunities :: Signal (Set PeerAddr)
        promotionOpportunities =
          (\local established recentFailures inProgressPromoteCold ->
              Set.unions
                [ -- There are no opportunities if we're at or above target
                  if Set.size groupEstablished >= warmTarget'
                     then Set.empty
                     else group Set.\\ established
                                Set.\\ recentFailures
                                Set.\\ inProgressPromoteCold
                | (_, WarmValency warmTarget', group) <- LocalRootPeers.toGroupSets local
                , let groupEstablished = group `Set.intersection` established
                ]
          ) <$> govLocalRootPeersSig
            <*> govEstablishedPeersSig
            <*> govEstablishedFailuresSig
            <*> govInProgressPromoteColdSig

        promotionOpportunitiesIgnoredTooLong :: Signal (Set PeerAddr)
        promotionOpportunitiesIgnoredTooLong =
          Signal.keyedTimeout
            15 -- seconds
            id
            promotionOpportunities

     in counterexample
          ("\nSignal key: (local root peers, established peers, " ++
           "recent failures, opportunities, ignored too long)") $

        signalProperty 20 show
          (\(_,_,_,_,_,toolong) -> Set.null toolong)
          ((,,,,,) <$> govLocalRootPeersSig
                  <*> govEstablishedPeersSig
                  <*> govEstablishedFailuresSig
                  <*> govInProgressPromoteColdSig
                  <*> promotionOpportunities
                  <*> promotionOpportunitiesIgnoredTooLong)


-- | A variant of 'prop_governor_target_active_below' but for the target that
-- certain numbers out of groups of local root peers should become active.
--
-- We do not need separate above and below variants of this property because
-- the target for active local root peers is one-sided: it is ok if we are
-- above target for any individual group. It is the overall active peers target
-- that can cause us to demote local roots if that's possible for any group
-- without going under target.
--
-- TODO: perhaps we do need a below property that we do not demote active peers
-- causing us to undershoot the target for local root peers being active.
--
prop_governor_target_active_local_below :: MaxTime -> GovernorMockEnvironment -> Property
prop_governor_target_active_local_below (MaxTime maxTime) env =
    let events = Signal.eventsFromListUpToTime maxTime
               . selectPeerSelectionTraceEvents
               . runGovernorInMockEnvironment
               $ env

        govLocalRootPeersSig :: Signal (LocalRootPeers.LocalRootPeers PeerAddr)
        govLocalRootPeersSig =
          selectGovState Governor.localRootPeers events

        govEstablishedPeersSig :: Signal (Set PeerAddr)
        govEstablishedPeersSig =
          selectGovState
            (EstablishedPeers.toSet . Governor.establishedPeers)
            events

        govActivePeersSig :: Signal (Set PeerAddr)
        govActivePeersSig =
          selectGovState Governor.activePeers events

        govInProgressDemoteToColdSig :: Signal (Set PeerAddr)
        govInProgressDemoteToColdSig =
          selectGovState Governor.inProgressDemoteToCold events

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
                         failures = Map.keysSet (Map.filter (==PeerWarm) . fmap fst $ status)
                     TraceDemoteLocalAsynchronous status
                       | Set.null failures -> Nothing
                       | otherwise         -> Just failures
                       where
                         failures = Map.keysSet (Map.filter (==PeerWarm) . fmap fst $ status)
                     _ -> Nothing
              )
          . selectGovEvents
          $ events

        promotionOpportunities :: Signal (Set PeerAddr)
        promotionOpportunities =
          (\local established active recentFailures inProgressDemoteToCold ->
              Set.unions
                [ -- There are no opportunities if we're at or above target
                  if Set.size groupActive >= hotTarget'
                     then Set.empty
                     else groupEstablished Set.\\ active
                                           Set.\\ recentFailures
                                           Set.\\ inProgressDemoteToCold
                | (HotValency hotTarget', _, group) <- LocalRootPeers.toGroupSets local
                , let groupActive      = group `Set.intersection` active
                      groupEstablished = group `Set.intersection` established
                ]
          ) <$> govLocalRootPeersSig
            <*> govEstablishedPeersSig
            <*> govActivePeersSig
            <*> govActiveFailuresSig
            <*> govInProgressDemoteToColdSig

        promotionOpportunitiesIgnoredTooLong :: Signal (Set PeerAddr)
        promotionOpportunitiesIgnoredTooLong =
          Signal.keyedTimeout
            (repromoteDelay config_REPROMOTE_DELAY + 20) -- seconds
            id
            promotionOpportunities

     in counterexample
          ("\nSignal key: (local, established peers, active peers, " ++
           "recent failures, opportunities, ignored too long)") $

        signalProperty 20 show
          (\(_,_,_,_,_,toolong) -> Set.null toolong)
          ((,,,,,) <$> (LocalRootPeers.toGroupSets <$> govLocalRootPeersSig)
                   <*> govEstablishedPeersSig
                   <*> govActivePeersSig
                   <*> govActiveFailuresSig
                   <*> promotionOpportunities
                   <*> promotionOpportunitiesIgnoredTooLong)

prop_governor_target_active_local_above :: MaxTime -> GovernorMockEnvironment -> Property
prop_governor_target_active_local_above (MaxTime maxTime) env =
    let events = Signal.eventsFromListUpToTime maxTime
               . selectPeerSelectionTraceEvents
               . runGovernorInMockEnvironment
               $ env

        govLocalRootPeersSig :: Signal (LocalRootPeers.LocalRootPeers PeerAddr)
        govLocalRootPeersSig =
          selectGovState Governor.localRootPeers events

        govActivePeersSig :: Signal (Set PeerAddr)
        govActivePeersSig =
          selectGovState Governor.activePeers events

        deomotionOpportunities :: Signal (Set PeerAddr)
        deomotionOpportunities =
          (\local active ->
              Set.unions
                [ -- There are no opportunities if we're at or below target
                  if Set.size groupActive <= hotTarget'
                     then Set.empty
                     else groupActive
                | (HotValency hotTarget', _, group) <- LocalRootPeers.toGroupSets local
                , let groupActive = group `Set.intersection` active
                ]
          ) <$> govLocalRootPeersSig
            <*> govActivePeersSig

        demotionOpportunitiesIgnoredTooLong :: Signal (Set PeerAddr)
        demotionOpportunitiesIgnoredTooLong =
          Signal.keyedTimeout
            10 -- seconds
            id
            deomotionOpportunities

     in counterexample
          ("\nSignal key: (local peers, active peers, " ++
           "demotion opportunities, ignored too long)") $

        signalProperty 20 show
          (\(_,_,_,toolong) -> Set.null toolong)
          ((,,,) <$> (LocalRootPeers.toGroupSets <$> govLocalRootPeersSig)
                 <*> govActivePeersSig
                 <*> deomotionOpportunities
                 <*> demotionOpportunitiesIgnoredTooLong)

-- | When in 'TooOld' state make sure we don't stay connected to non trustable
-- peers for too long
prop_governor_only_bootstrap_peers_in_fallback_state :: GovernorMockEnvironment -> Property
prop_governor_only_bootstrap_peers_in_fallback_state env =
    let events = Signal.eventsFromListUpToTime (Time (10 * 60 * 60))
               . selectPeerSelectionTraceEvents
               . runGovernorInMockEnvironment
               $ env

        govUseBootstrapPeers :: Signal UseBootstrapPeers
        govUseBootstrapPeers =
          selectGovState Governor.bootstrapPeersFlag events

        govLedgerStateJudgement :: Signal LedgerStateJudgement
        govLedgerStateJudgement =
          selectGovState (Governor.ledgerStateJudgement) events

        govKnownPeers :: Signal (Set PeerAddr)
        govKnownPeers =
          selectGovState (KnownPeers.toSet . Governor.knownPeers) events

        govTrustedPeers :: Signal (Set PeerAddr)
        govTrustedPeers =
          selectGovState
            (\st -> LocalRootPeers.keysSet (LocalRootPeers.clampToTrustable (Governor.localRootPeers st))
                 <> PublicRootPeers.getBootstrapPeers (Governor.publicRootPeers st)
            ) events

        keepNonTrustablePeersTooLong :: Signal (Set PeerAddr)
        keepNonTrustablePeersTooLong =
          Signal.keyedTimeout
            10 -- seconds
            (\(knownPeers, useBootstrapPeers, trustedPeers, lsj) ->
              if requiresBootstrapPeers useBootstrapPeers lsj
                 then knownPeers `Set.difference` trustedPeers
                 else Set.empty
            )
            ((,,,) <$> govKnownPeers
                   <*> govUseBootstrapPeers
                   <*> govTrustedPeers
                   <*> govLedgerStateJudgement
            )

     in signalProperty 20 show
          Set.null
          keepNonTrustablePeersTooLong

-- | When in 'TooOld' state make sure that after we disconnected from all non
-- trustable peers, we don't get any non trustable peers in our known set
-- until we are in caught up state
prop_governor_no_non_trustable_peers_before_caught_up_state :: GovernorMockEnvironment -> Property
prop_governor_no_non_trustable_peers_before_caught_up_state env =
    let events = Signal.eventsFromListUpToTime (Time (10 * 60 * 60))
               . selectPeerSelectionTraceEvents
               . runGovernorInMockEnvironment
               $ env

        govUseBootstrapPeers :: Signal UseBootstrapPeers
        govUseBootstrapPeers =
          selectGovState Governor.bootstrapPeersFlag events

        govLedgerStateJudgement :: Signal LedgerStateJudgement
        govLedgerStateJudgement =
          selectGovState (Governor.ledgerStateJudgement) events

        govKnownPeers :: Signal (Set PeerAddr)
        govKnownPeers =
          selectGovState (KnownPeers.toSet . Governor.knownPeers) events

        govTrustedPeers :: Signal (Set PeerAddr)
        govTrustedPeers =
          selectGovState
            (\st -> LocalRootPeers.keysSet (LocalRootPeers.clampToTrustable (Governor.localRootPeers st))
                 <> PublicRootPeers.getBootstrapPeers (Governor.publicRootPeers st)
            ) events

        govHasOnlyBootstrapPeers :: Signal Bool
        govHasOnlyBootstrapPeers =
          selectGovState Governor.hasOnlyBootstrapPeers events

        keepNonTrustablePeersTooLong :: Signal (Set PeerAddr)
        keepNonTrustablePeersTooLong =
          Signal.keyedTimeout
            10 -- seconds
            (\( knownPeers, trustedPeers
              , useBootstrapPeers, lsj, hasOnlyBootstrapPeers) ->
                if hasOnlyBootstrapPeers && requiresBootstrapPeers useBootstrapPeers lsj
                   then Set.difference knownPeers trustedPeers
                   else Set.empty
            )
            ((,,,,) <$> govKnownPeers
                    <*> govTrustedPeers
                    <*> govUseBootstrapPeers
                    <*> govLedgerStateJudgement
                    <*> govHasOnlyBootstrapPeers
            )

     in signalProperty 20 show
          Set.null
          keepNonTrustablePeersTooLong

-- NOTE: the clean state is defined as a state in which we require bootstrap
-- peers and the governor set the `hasOnlyBootstrapPeers` flag.
--
prop_governor_only_bootstrap_peers_in_clean_state :: GovernorMockEnvironment -> Property
prop_governor_only_bootstrap_peers_in_clean_state env =
    let events = Signal.eventsFromListUpToTime (Time (10 * 60 * 60))
               . selectPeerSelectionTraceEvents
               . runGovernorInMockEnvironment
               $ env

        govUseBootstrapPeers :: Signal UseBootstrapPeers
        govUseBootstrapPeers =
          selectGovState Governor.bootstrapPeersFlag events

        govLedgerStateJudgement :: Signal LedgerStateJudgement
        govLedgerStateJudgement =
          selectGovState (Governor.ledgerStateJudgement) events

        govKnownAndTrustedPeers :: Signal (Set PeerAddr, Set PeerAddr)
        govKnownAndTrustedPeers =
          selectGovState
            (\st ->
                ( KnownPeers.toSet (Governor.knownPeers st)
                ,
                      LocalRootPeers.keysSet (LocalRootPeers.clampToTrustable (Governor.localRootPeers st))
                   <> PublicRootPeers.getBootstrapPeers (Governor.publicRootPeers st)
                )
            ) events

        govHasOnlyBootstrapPeers :: Signal Bool
        govHasOnlyBootstrapPeers =
          selectGovState Governor.hasOnlyBootstrapPeers events

        govTargets :: Signal PeerSelectionTargets
        govTargets =
          selectGovState Governor.targets events

        isInCleanState :: Signal Bool
        isInCleanState =
          fmap (not . Set.null)
          $ Signal.keyedUntil
             (\(_, ubp, lsj, hp) ->
               if hp && requiresBootstrapPeers ubp lsj
                  then Set.singleton ()
                  else Set.empty
             )
             (\(_, ubp, lsj, _hp) ->
               if not (requiresBootstrapPeers ubp lsj)
                  then Set.singleton ()
                  else Set.empty
             )
             (const False)
             ((,,,) <$> govKnownAndTrustedPeers
                    <*> govUseBootstrapPeers
                    <*> govLedgerStateJudgement
                    <*> govHasOnlyBootstrapPeers
             )

     in signalProperty 20 show
          (\(b, (kp, tp), t) -> (b && (Set.null (Set.difference kp tp) || targetNumberOfKnownPeers t <= 0))
                             || not b)
          ((,,) <$> isInCleanState
                <*> govKnownAndTrustedPeers
                <*> govTargets
          )

-- | This test checks that if the node is not in a sensitive state it will not
-- stay connected to _only_ bootstrap peers for too long.
--
prop_governor_stops_using_bootstrap_peers :: GovernorMockEnvironment -> Property
prop_governor_stops_using_bootstrap_peers env =
    let events = Signal.eventsFromListUpToTime (Time (10 * 60 * 60))
               . selectPeerSelectionTraceEvents
               . runGovernorInMockEnvironment
               $ env

        govUseBootstrapPeers :: Signal UseBootstrapPeers
        govUseBootstrapPeers =
          selectGovState Governor.bootstrapPeersFlag events

        govLedgerStateJudgement :: Signal LedgerStateJudgement
        govLedgerStateJudgement =
          selectGovState (Governor.ledgerStateJudgement) events

        govKnownPeers :: Signal (Set PeerAddr)
        govKnownPeers =
          selectGovState (KnownPeers.toSet . Governor.knownPeers) events

        govBootstrapPeers :: Signal (Set PeerAddr)
        govBootstrapPeers =
          selectGovState (PublicRootPeers.getBootstrapPeers . Governor.publicRootPeers) events

        govTrustableLocalRootPeers :: Signal (Set PeerAddr)
        govTrustableLocalRootPeers =
          selectGovState
            (\st -> LocalRootPeers.keysSet (LocalRootPeers.clampToTrustable (Governor.localRootPeers st))
            ) events

        keepBootstrapPeersTooLong :: Signal (Set ())
        keepBootstrapPeersTooLong =
          Signal.keyedTimeout
            10 -- seconds
            (\(knownPeers, trustableLocalRootPeers, bootstrapPeers, useBootstrapPeers, lsj) ->
            if requiresBootstrapPeers useBootstrapPeers lsj
               then if    not (Set.null (knownPeers Set.\\ bootstrapPeers Set.\\ trustableLocalRootPeers))
                       || Set.null knownPeers
                       then Set.empty
                       else Set.singleton ()
               else Set.empty
            )
            ((,,,,) <$> govKnownPeers
                    <*> govTrustableLocalRootPeers
                    <*> govBootstrapPeers
                    <*> govUseBootstrapPeers
                    <*> govLedgerStateJudgement
            )

     in signalProperty 20 show
          Set.null
          keepBootstrapPeersTooLong

-- | This test checks that if the node is not in a sensitive state it will use
-- ledger peers
--
prop_governor_uses_ledger_peers :: GovernorMockEnvironment -> Property
prop_governor_uses_ledger_peers env =
    let events = Signal.eventsFromListUpToTime (Time (10 * 60 * 60))
               . selectPeerSelectionTraceEvents
               . runGovernorInMockEnvironment
               $ env

        govUseBootstrapPeers :: Signal UseBootstrapPeers
        govUseBootstrapPeers =
          selectGovState Governor.bootstrapPeersFlag events

        govLedgerStateJudgement :: Signal LedgerStateJudgement
        govLedgerStateJudgement =
          selectGovState (Governor.ledgerStateJudgement) events

        govPublicRootPeersResultsSig :: Signal (PublicRootPeers PeerAddr)
        govPublicRootPeersResultsSig =
            Signal.fromEventsWith (PublicRootPeers.empty)
          . Signal.selectEvents
              (\case
                  TracePublicRootsResults prp _ _ -> Just prp
                  _ -> Nothing
              )
          . selectGovEvents
          $ events

        usesLedgerPeers =
            Signal.eventsToList
          $ Signal.toChangeEvents
          $ ((\ubp lsj prp ->
                if not (requiresBootstrapPeers ubp lsj)
                   then Set.null (PublicRootPeers.getBootstrapPeers prp)
                   else True)
            <$> govUseBootstrapPeers
            <*> govLedgerStateJudgement
            <*> govPublicRootPeersResultsSig
            )

     in counterexample (intercalate "\n" $ map show $ usesLedgerPeers)
      $ all snd usesLedgerPeers


prop_governor_association_mode :: GovernorMockEnvironment -> Property
prop_governor_association_mode env =
    let events = Signal.eventsFromListUpToTime (Time (10 * 60 * 60))
               . selectPeerSelectionTraceEvents
               . runGovernorInMockEnvironment
               $ env

        counters :: Signal (PeerSelectionSetsWithSizes PeerAddr)
        counters =
          selectGovState peerSelectionStateToView events

        -- accumulate local roots
        localRoots :: Signal (Set PeerAddr)
        localRoots =
            Signal.keyedUntil
              (\case
                Just (GovernorEvent (TraceLocalRootPeersChanged a _)) -> LocalRootPeers.keysSet a
                Just (MockEnvEvent (TraceEnvSetLocalRoots a)) -> LocalRootPeers.keysSet a
                _ -> Set.empty
              )
              (\_ -> Set.empty)
              (\_ -> False)
          . Signal.fromChangeEvents Nothing
          . fmap Just
          $ events

        publicRoots :: Signal (Set PeerAddr)
        publicRoots =
            Signal.keyedUntil
              PublicRootPeers.toSet
              (\_ -> Set.empty)
              (\_ -> False)
          . selectGovState Governor.publicRootPeers
          $ events

        associationMode :: Signal AssociationMode
        associationMode =
            Signal.fromChangeEvents Unrestricted
          $ selectGovAssociationMode events

    in counterexample (intercalate "\n" $ show <$> Signal.eventsToList events)
     $ signalProperty 20 show
        (\(cs, localRootSet, publicRootSet, am) ->
          case am of
            LocalRootsOnly ->
              -- we need to remove local and public roots.  They are changing
              -- over time, and a node might keep using them, event though the
              -- node is configured as an `Unrestricted` node.
              --
              -- This makes this test only effective if a node starts in
              -- `LocalRootsOnly` mode, until it is reconfigured.  This can
              -- discover some bugs in `readAssociationMode` but certainly not
              -- all.
              --
              -- TODO: write a more effective test.
                 Set.null (fst (viewKnownBootstrapPeers cs)
                            Set.\\ localRootSet
                            Set.\\ publicRootSet)
              && Set.null (fst (viewKnownBigLedgerPeers cs)
                            Set.\\ localRootSet
                            Set.\\ publicRootSet)
              && Set.null (fst (viewKnownNonRootPeers cs)
                            Set.\\ localRootSet
                            Set.\\ publicRootSet)

            Unrestricted -> True
        )
        ((,,,) <$> counters
               <*> localRoots
               <*> publicRoots
               <*> associationMode)

--
-- Utils for properties
--

takeFirstNHours :: DiffTime -> [(Time, a)] -> [(Time, a)]
takeFirstNHours h = takeWhile (\(t,_) -> t < Time (60*60*h))

selectEnvEvents :: Events TestTraceEvent -> Events TraceMockEnv
selectEnvEvents = Signal.selectEvents
                    (\case MockEnvEvent e -> Just $! e
                           _              -> Nothing)

selectGovEvents :: Events TestTraceEvent
                -> Events (TracePeerSelection PeerAddr)
selectGovEvents = Signal.selectEvents
                    (\case GovernorEvent e -> Just $! e
                           _               -> Nothing)

selectGovCounters :: Events TestTraceEvent
                  -> Events PeerSelectionCounters
selectGovCounters = Signal.selectEvents
                      (\case GovernorCounters e -> Just $! e
                             _                  -> Nothing)

selectGovAssociationMode :: Events TestTraceEvent
                         -> Events AssociationMode
selectGovAssociationMode = Signal.selectEvents
                             (\case GovernorAssociationMode e -> Just $! e
                                    _                         -> Nothing)

selectGovState :: Eq a
               => (forall peerconn. Governor.PeerSelectionState PeerAddr peerconn -> a)
               -> Events TestTraceEvent
               -> Signal a
selectGovState f =
    Signal.nub
  -- TODO: #3182 Rng seed should come from quickcheck.
  . Signal.fromChangeEvents (f $! Governor.emptyPeerSelectionState (mkStdGen 42))
  . Signal.selectEvents
      (\case GovernorDebug (TraceGovernorState _ _ st) -> Just $! f st
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
      (\case TraceEnvSetTargets targets -> Just $! targets
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
_governorFindingPublicRoots :: Int
                            -> STM IO (Map RelayAccessPoint PeerAdvertise)
                            -> STM IO UseBootstrapPeers
                            -> STM IO LedgerStateJudgement
                            -> PeerSharing
                            -> StrictTVar IO OutboundConnectionsState
                            -> IO Void
_governorFindingPublicRoots targetNumberOfRootPeers readDomains readUseBootstrapPeers readLedgerStateJudgement peerSharing olocVar = do
    countersVar <- newTVarIO emptyPeerSelectionCounters
    publicStateVar <- makePublicPeerSelectionStateVar
    debugStateVar <- newTVarIO $ emptyPeerSelectionState (mkStdGen 42)
    dnsSemaphore <- newLedgerAndPublicRootDNSSemaphore
    let interfaces = PeerSelectionInterfaces {
            countersVar,
            publicStateVar,
            debugStateVar,
            readUseLedgerPeers = return DontUseLedgerPeers
          }
    publicRootPeersProvider
      tracer
      (curry IP.toSockAddr)
      dnsSemaphore
      DNS.defaultResolvConf
      readDomains
      (ioDNSActions LookupReqAAndAAAA) $ \requestPublicRootPeers -> do
        peerSelectionGovernor
          tracer tracer tracer
          -- TODO: #3182 Rng seed should come from quickcheck.
          (mkStdGen 42)
          actions
            { requestPublicRootPeers = \_ ->
                transformPeerSelectionAction requestPublicRootPeers }
          policy
          interfaces
  where
    tracer :: Show a => Tracer IO a
    tracer  = Tracer (BS.putStrLn . BS.pack . show)

    actions :: PeerSelectionActions SockAddr PeerSharing IO
    actions = PeerSelectionActions {
                readLocalRootPeers       = return [],
                peerSharing              = peerSharing,
                readPeerSelectionTargets = return targets,
                requestPeerShare         = \_ _ -> return (PeerSharingResult []),
                peerConnToPeerSharing    = \ps -> ps,
                requestPublicRootPeers   = \_ _ -> return (PublicRootPeers.empty, 0),
                peerStateActions         = PeerStateActions {
                  establishPeerConnection  = error "establishPeerConnection",
                  monitorPeerConnection    = error "monitorPeerConnection",
                  activatePeerConnection   = error "activatePeerConnection",
                  deactivatePeerConnection = error "deactivatePeerConnection",
                  closePeerConnection      = error "closePeerConnection"
                },
                readUseBootstrapPeers,
                readLedgerStateJudgement,
                readInboundPeers = pure Map.empty,
                updateOutboundConnectionsState = \a -> do
                  a' <- readTVar olocVar
                  when (a /= a') $
                    writeTVar olocVar a
              }

    targets :: PeerSelectionTargets
    targets = nullPeerSelectionTargets {
                targetNumberOfRootPeers  = targetNumberOfRootPeers,
                targetNumberOfKnownPeers = targetNumberOfRootPeers
              }

    policy :: PeerSelectionPolicy SockAddr IO
    policy  = PeerSelectionPolicy {
                policyPickKnownPeersForPeerShare = \_ _ _ -> pickTrivially,
                policyPickColdPeersToForget   = \_ _ _ -> pickTrivially,
                policyPickColdPeersToPromote  = \_ _ _ -> pickTrivially,
                policyPickWarmPeersToPromote  = \_ _ _ -> pickTrivially,
                policyPickHotPeersToDemote    = \_ _ _ -> pickTrivially,
                policyPickWarmPeersToDemote   = \_ _ _ -> pickTrivially,
                policyFindPublicRootTimeout   = 5,
                policyMaxInProgressPeerShareReqs = 0,
                policyPeerShareRetryTime         = 0, -- seconds
                policyPeerShareBatchWaitTime     = 0, -- seconds
                policyPeerShareOverallTimeout    = 0, -- seconds
                policyPeerShareActivationDelay   = 2, -- seconds
                policyErrorDelay              = 0  -- seconds
              }
    pickTrivially :: Applicative m => Set SockAddr -> Int -> m (Set SockAddr)
    pickTrivially m n = pure . Set.take n $ m

    transformPeerSelectionAction = fmap (fmap (\(a, b) -> (PublicRootPeers.fromMapAndSet a Set.empty Set.empty Set.empty, b)))

prop_issue_3550 :: Property
prop_issue_3550 = prop_governor_target_established_below defaultMaxTime $
  GovernorMockEnvironment {
      peerGraph = PeerGraph
        [ (PeerAddr 4,[],GovernorScripts {peerShareScript = Script (Just ([],PeerShareTimeSlow) :| []), peerSharingScript = Script (PeerSharingDisabled :| []), connectionScript = Script ((Noop,NoDelay) :| [])}),
          (PeerAddr 14,[],GovernorScripts {peerShareScript = Script (Nothing :| []), peerSharingScript = Script (PeerSharingDisabled :| []), connectionScript = Script ((Noop,NoDelay) :| [])}),
          (PeerAddr 16,[],GovernorScripts {peerShareScript = Script (Nothing :| []), peerSharingScript = Script (PeerSharingDisabled :| []), connectionScript = Script ((Noop,NoDelay) :| [])}),
          (PeerAddr 29,[],GovernorScripts {peerShareScript = Script (Nothing :| []), peerSharingScript = Script (PeerSharingDisabled :| []), connectionScript = Script ((ToWarm,NoDelay) :| [(ToCold,NoDelay),(Noop,NoDelay)])})
        ],
      localRootPeers = LocalRootPeers.fromGroups
        [ (1, 1, Map.fromList [(PeerAddr 16,(DoAdvertisePeer, IsNotTrustable))])
        , (1, 1, Map.fromList [(PeerAddr 4,(DoAdvertisePeer, IsNotTrustable))])
        ],
      publicRootPeers = PublicRootPeers.fromPublicRootPeers
        (Map.fromList [ (PeerAddr 14, DoNotAdvertisePeer)
                      , (PeerAddr 29, DoNotAdvertisePeer)
                      ]
        ),
      targets = Script
        ((nullPeerSelectionTargets {
           targetNumberOfRootPeers = 1,
           targetNumberOfKnownPeers = 4,
           targetNumberOfEstablishedPeers = 4,
           targetNumberOfActivePeers = 3
         },NoDelay) :| []),
      pickKnownPeersForPeerShare = Script (PickFirst :| []),
      pickColdPeersToPromote = Script (PickFirst :| []),
      pickWarmPeersToPromote = Script (PickFirst :| []),
      pickHotPeersToDemote = Script (PickSome (Set.fromList [PeerAddr 29]) :| []),
      pickWarmPeersToDemote = Script (PickFirst :| []),
      pickColdPeersToForget = Script (PickFirst :| []),
      peerSharingFlag = PeerSharingEnabled,
      useBootstrapPeers = Script ((DontUseBootstrapPeers, NoDelay) :| []),
      useLedgerPeers = Script ((UseLedgerPeers Always, NoDelay) :| []),
      ledgerStateJudgement = Script ((YoungEnough, NoDelay) :| [])
    }

-- | issue #3515
--
-- ```
-- Exception:
--   Assertion failed
--   CallStack (from HasCallStack):
--     assert, called at src/Ouroboros/Network/PeerSelection/Governor/Types.hs:396:5 in ouroboros-network-0.1.0.0-inplace:Ouroboros.Network.PeerSelection.Governor.Types
-- ```
prop_issue_3515 :: Property
prop_issue_3515 = prop_governor_nolivelock $
  GovernorMockEnvironment {
      peerGraph = PeerGraph
        [(PeerAddr 10,[],GovernorScripts {
                           peerShareScript = Script (Nothing :| []),
                           peerSharingScript = Script (PeerSharingDisabled :| []),
                           connectionScript = Script ((ToCold,NoDelay) :| [(Noop,NoDelay)])
                         })],
      localRootPeers = LocalRootPeers.fromGroups [(1,1,Map.fromList [(PeerAddr 10,(DoAdvertisePeer, IsNotTrustable))])],
      publicRootPeers = PublicRootPeers.empty,
      targets = Script
        (( nullPeerSelectionTargets { targetNumberOfKnownPeers = 1 }, ShortDelay)
        :| [ ( nullPeerSelectionTargets { targetNumberOfKnownPeers = 1 }, ShortDelay),
             ( nullPeerSelectionTargets, NoDelay),
             ( nullPeerSelectionTargets { targetNumberOfKnownPeers = 1 }, NoDelay)
           ]),
      pickKnownPeersForPeerShare = Script (PickFirst :| []),
      pickColdPeersToPromote = Script (PickFirst :| []),
      pickWarmPeersToPromote = Script (PickFirst :| []),
      pickHotPeersToDemote = Script (PickFirst :| []),
      pickWarmPeersToDemote = Script (PickFirst :| []),
      pickColdPeersToForget = Script (PickFirst :| []),
      peerSharingFlag = PeerSharingEnabled,
      useBootstrapPeers = Script ((DontUseBootstrapPeers, NoDelay) :| []),
      useLedgerPeers = Script ((UseLedgerPeers Always, NoDelay) :| []),
      ledgerStateJudgement = Script ((YoungEnough, NoDelay) :| [])
    }

-- | issue #3494
--
-- ```
-- *** Exception: Assertion failed
-- CallStack (from HasCallStack):
--   assert, called at src/Ouroboros/Network/PeerSelection/Governor/Types.hs:396:5 in ouroboros-network-0.1.0.0-inplace:Ouroboros.Network.PeerSelection.Governor.Types
-- ```
prop_issue_3494 :: Property
prop_issue_3494 = prop_governor_nofail $
  GovernorMockEnvironment {
      peerGraph = PeerGraph [(PeerAddr 64,[],GovernorScripts {
                                                peerShareScript = Script (Nothing :| []),
                                                peerSharingScript = Script (PeerSharingDisabled :| []),
                                                connectionScript = Script ((ToCold,NoDelay) :| [(Noop,NoDelay)])
                                              })],
      localRootPeers = LocalRootPeers.fromGroups [(1,1,Map.fromList [(PeerAddr 64, (DoAdvertisePeer, IsNotTrustable))])],
      publicRootPeers = PublicRootPeers.empty,
      targets = Script
        (( nullPeerSelectionTargets,NoDelay)
        :| [ (nullPeerSelectionTargets { targetNumberOfKnownPeers = 1 },ShortDelay),
             (nullPeerSelectionTargets { targetNumberOfKnownPeers = 1 },ShortDelay),
             (nullPeerSelectionTargets,NoDelay),
             (nullPeerSelectionTargets,NoDelay),
             (nullPeerSelectionTargets { targetNumberOfKnownPeers = 1 },NoDelay)
           ]),
      pickKnownPeersForPeerShare = Script (PickFirst :| []),
      pickColdPeersToPromote = Script (PickFirst :| []),
      pickWarmPeersToPromote = Script (PickFirst :| []),
      pickHotPeersToDemote = Script (PickFirst :| []),
      pickWarmPeersToDemote = Script (PickFirst :| []),
      pickColdPeersToForget = Script (PickFirst :| []),
      peerSharingFlag = PeerSharingEnabled,
      useBootstrapPeers = Script ((DontUseBootstrapPeers, NoDelay) :| []),
      useLedgerPeers = Script ((UseLedgerPeers Always, NoDelay) :| []),
      ledgerStateJudgement = Script ((YoungEnough, NoDelay) :| [])
    }

-- | issue #3233
--
prop_issue_3233 :: Property
prop_issue_3233 = prop_governor_nolivelock $
  GovernorMockEnvironment {
      peerGraph = PeerGraph
        [(PeerAddr 4,[],GovernorScripts {
                          peerShareScript = Script (Nothing :| []),
                          peerSharingScript = Script (PeerSharingDisabled :| []),
                          connectionScript = Script ((ToCold,NoDelay)
                                                 :| [(ToCold,NoDelay),
                                                     (Noop,NoDelay),
                                                     (ToWarm,NoDelay),
                                                     (ToCold,NoDelay),
                                                     (Noop,NoDelay)
                                                    ])
                        }),
         (PeerAddr 13,[],GovernorScripts {peerShareScript = Script (Nothing :| []), peerSharingScript = Script (PeerSharingDisabled :| []), connectionScript = Script ((Noop,NoDelay) :| [])}),
         (PeerAddr 15,[],GovernorScripts {peerShareScript = Script (Just ([],PeerShareTimeSlow) :| []), peerSharingScript = Script (PeerSharingDisabled :| []), connectionScript = Script ((Noop,NoDelay) :| [])})
        ],
      localRootPeers = LocalRootPeers.fromGroups
        [ (1, 1, Map.fromList [(PeerAddr 15, (DoAdvertisePeer, IsNotTrustable))])
        , (1, 1, Map.fromList [(PeerAddr 13, (DoAdvertisePeer, IsNotTrustable))])
        ],
      publicRootPeers = PublicRootPeers.fromPublicRootPeers
        (Map.fromList [(PeerAddr 4, DoNotAdvertisePeer)]),
      targets = Script
        ((nullPeerSelectionTargets,NoDelay)
        :| [(nullPeerSelectionTargets {
               targetNumberOfRootPeers = 1,
               targetNumberOfKnownPeers = 3,
               targetNumberOfEstablishedPeers = 3
             },LongDelay),
            (nullPeerSelectionTargets,NoDelay),
            (nullPeerSelectionTargets,NoDelay),
            (nullPeerSelectionTargets {
                targetNumberOfRootPeers = 1,
                targetNumberOfKnownPeers = 3,
                targetNumberOfEstablishedPeers = 3,
                targetNumberOfActivePeers = 2
             },NoDelay)]),
      pickKnownPeersForPeerShare = Script (PickFirst :| []),
      pickColdPeersToPromote = Script (PickFirst :| []),
      pickWarmPeersToPromote = Script (PickFirst :| []),
      pickHotPeersToDemote = Script (PickFirst :| []),
      pickWarmPeersToDemote = Script (PickFirst :| []),
      pickColdPeersToForget = Script (PickFirst :| []),
      peerSharingFlag = PeerSharingEnabled,
      useBootstrapPeers = Script ((DontUseBootstrapPeers, NoDelay) :| []),
      useLedgerPeers = Script ((UseLedgerPeers Always, NoDelay) :| []),
      ledgerStateJudgement = Script ((YoungEnough, NoDelay) :| [])
    }


-- | Verify that re-promote delay is applied with a fuzz.
--
prop_governor_repromote_delay :: MaxTime -> GovernorMockEnvironment -> Property
prop_governor_repromote_delay (MaxTime maxTime) env =
    let evs = Signal.eventsFromListUpToTime maxTime
            . selectPeerSelectionTraceEvents
            . runGovernorInMockEnvironment
            $ env
    in  property
      . foldMap (\case
                   TraceDemoteAsynchronous m ->
                     foldMap
                       (\(st, mx) -> maybe mempty (\x -> All
                                                   $ counterexample (show st)
                                                   $ x =/= config_REPROMOTE_DELAY) mx)
                       m
                   TraceDemoteLocalAsynchronous m ->
                     foldMap
                       (\(st, mx) -> maybe mempty (\x -> All
                                                   $ counterexample (show st)
                                                   $ x =/= config_REPROMOTE_DELAY) mx)
                       m
                   TraceDemoteBigLedgerPeersAsynchronous m ->
                     foldMap
                       (\(st, mx) -> maybe mempty (\x -> All
                                                   $ counterexample (show st)
                                                   $ x =/= config_REPROMOTE_DELAY) mx)
                       m
                   _ -> mempty
                )
      . selectGovEvents
      $ evs

--
-- Utils
--


-- | Max simulation time.  We start with 10hrs, and shrink it to smaller values
-- if needed.
--
newtype MaxTime = MaxTime { getTime :: Time }
  deriving (Show)

defaultMaxTime :: MaxTime
defaultMaxTime = MaxTime (Time (10 * 3600))

instance Arbitrary MaxTime where
    arbitrary = pure defaultMaxTime
    shrink (MaxTime (Time t)) =
      [ MaxTime (Time (microsecondsAsIntToDiffTime t'))
      | t' <- shrink  (diffTimeToMicrosecondsAsInt t)
      ]


-- | filter big ledger peers
--
takeBigLedgerPeers
    :: (Governor.PeerSelectionState PeerAddr peerconn -> Set PeerAddr)
    ->  Governor.PeerSelectionState PeerAddr peerconn -> Set PeerAddr
takeBigLedgerPeers f =
  \st -> f st `Set.intersection` (PublicRootPeers.getBigLedgerPeers . Governor.publicRootPeers) st

-- | filter out big ledger peers
--
dropBigLedgerPeers
    :: (Governor.PeerSelectionState PeerAddr peerconn -> Set PeerAddr)
    ->  Governor.PeerSelectionState PeerAddr peerconn -> Set PeerAddr
dropBigLedgerPeers f =
  \st -> f st Set.\\ (PublicRootPeers.getBigLedgerPeers . Governor.publicRootPeers) st

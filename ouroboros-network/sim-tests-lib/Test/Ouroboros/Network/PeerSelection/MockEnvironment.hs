{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.Ouroboros.Network.PeerSelection.MockEnvironment
  ( PeerGraph (..)
  , GovernorMockEnvironment (..)
  , GovernorPraosMockEnvironment (..)
  , GovernorMockEnvironmentWithoutAsyncDemotion (..)
  , runGovernorInMockEnvironment
  , exploreGovernorInMockEnvironment
  , TraceMockEnv (..)
  , TestTraceEvent (..)
  , selectGovernorEvents
  , selectGovernorStateEvents
  , selectPeerSelectionTraceEvents
  , selectPeerSelectionTraceEventsUntil
  , peerShareReachablePeers
  , module Ouroboros.Network.Testing.Data.Script
  , module Ouroboros.Network.PeerSelection.Types
  , tests
  , prop_shrink_nonequal_GovernorMockEnvironment
  , config_REPROMOTE_DELAY
  ) where

import Data.Bifunctor (first, bimap)
import Data.Dynamic (fromDynamic)
import Data.Functor ((<&>))
import Data.List (nub)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Typeable (Typeable)
import Data.Void (Void)
import System.Random (mkStdGen)

import Control.Concurrent.Class.MonadSTM
import Control.Concurrent.Class.MonadSTM.Strict qualified as StrictTVar
import Control.Exception (throw)
import Control.Monad (when, forM)
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadSay
import Control.Monad.Class.MonadTest
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI hiding (timeout)
import Control.Monad.Fail qualified as Fail
import Control.Monad.IOSim
import Control.Tracer (Tracer (..), contramap, traceWith)

import Ouroboros.Network.ExitPolicy
import Ouroboros.Network.PeerSelection.Governor hiding (PeerSelectionState (..))
import Ouroboros.Network.PeerSelection.Governor qualified as Governor
import Ouroboros.Network.PeerSelection.State.LocalRootPeers qualified as LocalRootPeers

import Ouroboros.Network.Testing.Data.Script (PickScript, Script (..),
           ScriptDelay (..), TimedScript, arbitraryPickScript,
           arbitraryScriptOf, initScript, initScript', interpretPickScript,
           playTimedScript, prop_shrink_Script, singletonScript,
           singletonTimedScript, stepScript, stepScriptSTM, stepScriptSTM', shrinkScriptWith)
import Ouroboros.Network.Testing.Utils
    ( ShrinkCarefully,
      arbitrarySubset,
      prop_shrink_nonequal,
      prop_shrink_valid,
      nightlyTest )

import Test.Ouroboros.Network.PeerSelection.Instances
import Test.Ouroboros.Network.PeerSelection.LocalRootPeers as LocalRootPeers hiding
           (tests)
import Test.Ouroboros.Network.PeerSelection.PeerGraph

import Ouroboros.Network.ConsensusMode
import Ouroboros.Network.PeerSelection.Bootstrap (UseBootstrapPeers (..),
           requiresBootstrapPeers)
import Ouroboros.Network.PeerSelection.LedgerPeers
import Ouroboros.Network.PeerSelection.LocalRootPeers
           (OutboundConnectionsState (..))
import Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing (..))
import Ouroboros.Network.PeerSelection.PublicRootPeers (PublicRootPeers (..))
import Ouroboros.Network.PeerSelection.PublicRootPeers qualified as PublicRootPeers
import Ouroboros.Network.PeerSelection.Types (PeerStatus (..))
import Ouroboros.Network.Protocol.PeerSharing.Type (PeerSharingAmount,
           PeerSharingResult (..))
import Test.Ouroboros.Network.LedgerPeers (ArbitraryLedgerStateJudgement (..))
import Test.Ouroboros.Network.PeerSelection.PublicRootPeers ()
import Test.QuickCheck
import Test.Tasty (TestTree, localOption, testGroup)
import Test.Tasty.QuickCheck (QuickCheckMaxSize (..), testProperty)


tests :: TestTree
tests =
  testGroup "Ouroboros.Network.PeerSelection"
    [ testGroup "MockEnvironment"
      [ testProperty "shrink for Script"                     prop_shrink_Script
      , testProperty "shrink for GovernorScripts"            prop_shrink_GovernorScripts
      , testProperty "arbitrary for PeerSelectionTargets"    prop_arbitrary_PeerSelectionTargets
      , testProperty "shrink for PeerSelectionTargets"       prop_shrink_PeerSelectionTargets
      , testProperty "arbitrary for PeerGraph"               prop_arbitrary_PeerGraph
      , localOption (QuickCheckMaxSize 30) $
        testProperty "shrink for PeerGraph"                  prop_shrink_PeerGraph
      , testProperty "arbitrary for GovernorMockEnvironment" prop_arbitrary_GovernorMockEnvironment
      , localOption (QuickCheckMaxSize 30) $
        testProperty "shrink for GovernorMockEnvironment"    prop_shrink_GovernorMockEnvironment
      , nightlyTest $
        testProperty "shrink nonequal GovernorMockEnvironment"
                                                             prop_shrink_nonequal_GovernorMockEnvironment
      ]
    ]


--
-- Mock environment types
--

-- | The data needed to execute the peer selection governor in a test with a
-- mock network environment. It contains the data needed to provide the
-- 'PeerSelectionActions' and 'PeerSelectionPolicy' to run the governor.
--
-- The representations are chosen to be easily shrinkable. See the @Arbitrary@
-- instances.
--
data GovernorMockEnvironment = GovernorMockEnvironment {
       peerGraph                  :: !PeerGraph,
       localRootPeers             :: !(LocalRootPeers PeerAddr),
       publicRootPeers            :: !(PublicRootPeers PeerAddr),
       targets                    :: !(TimedScript ConsensusModePeerTargets),
       pickKnownPeersForPeerShare :: !(PickScript PeerAddr),
       pickColdPeersToPromote     :: !(PickScript PeerAddr),
       pickWarmPeersToPromote     :: !(PickScript PeerAddr),
       pickHotPeersToDemote       :: !(PickScript PeerAddr),
       pickWarmPeersToDemote      :: !(PickScript PeerAddr),
       pickColdPeersToForget      :: !(PickScript PeerAddr),
       pickInboundPeers           :: !(PickScript PeerAddr),
       peerSharingFlag            :: !PeerSharing,
       useBootstrapPeers          :: !(TimedScript UseBootstrapPeers),
       consensusMode              :: !ConsensusMode,
       useLedgerPeers             :: !(TimedScript UseLedgerPeers),
       ledgerStateJudgement       :: !(TimedScript LedgerStateJudgement)
     }
  deriving (Show, Eq)

-- | This instance is used to generate test cases for properties
-- which rely on peer selection prior to introduction of Genesis
--
newtype GovernorPraosMockEnvironment = GovernorPraosMockEnvironment { getMockEnv :: GovernorMockEnvironment }
  deriving (Eq, Show)

data PeerConn m = PeerConn !PeerAddr !PeerSharing !(TVar m PeerStatus)

instance Show (PeerConn m) where
    show (PeerConn peeraddr peerSharing _) =
      "PeerConn " ++ show peeraddr ++ " " ++ show peerSharing


-- | 'GovernorMockEnvironment' which does not do any asynchronous demotions.
--
newtype GovernorMockEnvironmentWithoutAsyncDemotion =
    GovernorMockEnvironmentWAD GovernorMockEnvironment
  deriving Show

instance Arbitrary GovernorMockEnvironmentWithoutAsyncDemotion where
    arbitrary = GovernorMockEnvironmentWAD . fixGraph <$> arbitrary
      where
        fixGraph g@GovernorMockEnvironment { peerGraph = PeerGraph peerGraph } =
          g { peerGraph = PeerGraph (map fixNode peerGraph) }
        fixNode (addr, addrs, peerInfo) =
          (addr, addrs, peerInfo { connectionScript = singletonScript (Noop, ShortDelay) })
    shrink (GovernorMockEnvironmentWAD env) = map GovernorMockEnvironmentWAD (shrink env)


-- | Invariant. Used to check the QC generator and shrinker.
--
-- NOTE: Local and Public Root Peers sets should be disjoint.
-- However we do not check for that invariant here. The goal
-- is to check if the actual Governor takes care of this and enforces
-- the invariant.
validGovernorMockEnvironment :: GovernorMockEnvironment -> Property
validGovernorMockEnvironment GovernorMockEnvironment {
                               peerGraph,
                               localRootPeers,
                               publicRootPeers,
                               targets
                             } =
   conjoin [ counterexample "invalid peer graph"
              (validPeerGraph peerGraph)
           , counterexample "local roots not a subset of all peers"
              (LocalRootPeers.keysSet localRootPeers `Set.isSubsetOf` allPeersSet)
           , counterexample "public root peers not a subset of  all peers" $
             property (PublicRootPeers.toSet publicRootPeers `Set.isSubsetOf` allPeersSet)
           , counterexample "failed peer selection targets sanity check" $ 
             property (foldl (\ !p (ConsensusModePeerTargets {..},_) ->
                                p && all sanePeerSelectionTargets [praosTargets, genesisSyncTargets])
                        True
                        targets)
           , counterexample "big ledger peers not a subset of public roots"
                (PublicRootPeers.invariant publicRootPeers)
           ]
  where
    allPeersSet = allPeers peerGraph


--
-- Execution in the mock environment
--

-- | Run the 'peerSelectionGovernor' in the mock environment dictated by the
-- data in the 'GovernorMockEnvironment'.
--
-- The result is an execution trace.
--
runGovernorInMockEnvironment :: GovernorMockEnvironment -> SimTrace Void
runGovernorInMockEnvironment mockEnv =
    runSimTrace $ governorAction mockEnv

governorAction :: GovernorMockEnvironment -> IOSim s Void
governorAction mockEnv@GovernorMockEnvironment {
                 consensusMode,
                 targets = Script targets',
                 ledgerStateJudgement = Script ledgerStateJudgement'} = do
    publicStateVar <- makePublicPeerSelectionStateVar
    lpVar <- playTimedScript (contramap TraceEnvUseLedgerPeers tracerMockEnv)
                             (useLedgerPeers mockEnv)
    usbVar <- playTimedScript (contramap TraceEnvSetUseBootstrapPeers tracerMockEnv)
                              (useBootstrapPeers mockEnv)
    debugStateVar <- StrictTVar.newTVarIO (emptyPeerSelectionState (mkStdGen 42) consensusMode)
    countersVar <- StrictTVar.newTVarIO emptyPeerSelectionCounters
    policy  <- mockPeerSelectionPolicy mockEnv
    let initialPeerTargets = fst . NonEmpty.head $ targets'

    actions <-
      case consensusMode of
        PraosMode -> do
          lsjVar <- playTimedScript (contramap TraceEnvSetLedgerStateJudgement tracerMockEnv)
                                    (ledgerStateJudgement mockEnv)
          targetsVar <- playTimedScript (contramap TraceEnvSetTargets tracerMockEnv)
                                        (first praosTargets <$> targets mockEnv)
          mockPeerSelectionActions tracerMockEnv mockEnv
                                   initialPeerTargets
                                   (readTVar usbVar)
                                   (readTVar lpVar)
                                   (readTVar lsjVar)
                                   (readTVar targetsVar)
                                   policy
        GenesisMode -> do
          let tandemLsjAndTargets =
                Script $ NonEmpty.zipWith (\(lsj, delay) (ConsensusModePeerTargets {
                                                             praosTargets,
                                                             genesisSyncTargets}, _) ->
                                             let pickTargets =
                                                   case lsj of
                                                     TooOld -> genesisSyncTargets
                                                     YoungEnough -> praosTargets
                                             in ((lsj, pickTargets), delay))
                                          ledgerStateJudgement'
                                          targets'
          tandemVar <- playTimedScript (contramap TraceEnvGenesisLsjAndTargets tracerMockEnv)
                                       tandemLsjAndTargets
          mockPeerSelectionActions tracerMockEnv mockEnv
                                   initialPeerTargets
                                   (readTVar usbVar)
                                   (readTVar lpVar)
                                   (fst <$> readTVar tandemVar)
                                   (snd <$> readTVar tandemVar)
                                   policy


    let interfaces = PeerSelectionInterfaces {
            countersVar,
            publicStateVar,
            debugStateVar,
            -- TODO: peer selection tests are not relying on `UseLedgerPeers`
            readUseLedgerPeers = return DontUseLedgerPeers
          }

    exploreRaces      -- explore races within the governor
    _ <- forkIO $ do  -- races with the governor should be explored
      labelThisThread "outbound-governor"
      _ <- peerSelectionGovernor
        tracerTracePeerSelection
        (tracerDebugPeerSelection <> traceAssociationMode interfaces actions)
        tracerTracePeerSelectionCounters
        (mkStdGen 42)
        consensusMode
        actions
        policy
        interfaces
      atomically retry
    atomically retry  -- block to allow the governor to run

exploreGovernorInMockEnvironment :: Testable test
                                 => (ExplorationOptions->ExplorationOptions)
                                 -> GovernorMockEnvironment
                                 -> (Maybe (SimTrace Void) -> SimTrace Void -> test)
                                 -> Property
exploreGovernorInMockEnvironment optsf mockEnv k =
    exploreSimTrace optsf (governorAction mockEnv) k

data TraceMockEnv = TraceEnvAddPeers       !PeerGraph
                  | TraceEnvSetLocalRoots  !(LocalRootPeers PeerAddr)
                  | TraceEnvRequestPublicRootPeers
                  | TraceEnvRequestBigLedgerPeers
                  | TraceEnvSetPublicRoots !(PublicRootPeers PeerAddr)
                  | TraceEnvPublicRootTTL
                  | TraceEnvBigLedgerPeersTTL
                  | TraceEnvPeerShareTTL   !PeerAddr
                  | TraceEnvSetTargets     !PeerSelectionTargets
                  | TraceEnvPeersDemote    !AsyncDemotion !PeerAddr
                  | TraceEnvEstablishConn  !PeerAddr
                  | TraceEnvActivatePeer   !PeerAddr
                  | TraceEnvDeactivatePeer !PeerAddr
                  | TraceEnvCloseConn      !PeerAddr
                  | TraceEnvRootsResult      ![PeerAddr]
                  | TraceEnvBigLedgerPeersResult !(Set PeerAddr)
                  | TraceEnvPeerShareRequest !PeerAddr !(Maybe ([PeerAddr], PeerShareTime))
                  | TraceEnvPeerShareResult  !PeerAddr ![PeerAddr]
                  | TraceEnvPeersStatus      !(Map PeerAddr PeerStatus)
                  | TraceEnvSetUseBootstrapPeers !UseBootstrapPeers
                  | TraceEnvSetLedgerStateJudgement !LedgerStateJudgement
                  | TraceEnvUseLedgerPeers !UseLedgerPeers
                  | TraceEnvGenesisLsjAndTargets !(LedgerStateJudgement, PeerSelectionTargets)
  deriving Show

mockPeerSelectionActions :: forall m.
                            (MonadAsync m, MonadDelay m, MonadTimer m,
                             Fail.MonadFail m, MonadThrow (STM m), MonadTraceSTM m)
                         => Tracer m TraceMockEnv
                         -> GovernorMockEnvironment
                         -> ConsensusModePeerTargets
                         -> STM m UseBootstrapPeers
                         -> STM m UseLedgerPeers
                         -> STM m LedgerStateJudgement
                         -> STM m PeerSelectionTargets
                         -> PeerSelectionPolicy PeerAddr m
                         -> m (PeerSelectionActions PeerAddr (PeerConn m) m)
mockPeerSelectionActions tracer
                         env@GovernorMockEnvironment {
                           peerGraph,
                           localRootPeers,
                           publicRootPeers
                         }
                         initialPeerTargets
                         readUseBootstrapPeers
                         readUseLedgerPeers
                         getLedgerStateJudgement
                         readTargets
                         policy = do
    scripts <- Map.fromList <$>
                 sequence
                   [ (\a b c -> (addr, (a, b, c)))
                     <$> initScript peerShareScript
                     <*> initScript peerSharingScript
                     <*> initScript connectionScript
                   | let PeerGraph adjacency = peerGraph
                   , (addr, _, GovernorScripts {
                                 peerShareScript,
                                 peerSharingScript,
                                 connectionScript
                               }) <- adjacency
                   ]
    churnMutex <- StrictTVar.newEmptyTMVarIO
    peerConns  <- atomically $ do
      v <- newTVar Map.empty
      traceTVar proxy
                v (\_ a -> TraceDynamic . TraceEnvPeersStatus
                       <$> snapshotPeersStatus proxy a)
      return v

    onlyLocalOutboundConnsVar <- newTVarIO UntrustedState
    traceWith tracer (TraceEnvAddPeers peerGraph)
    traceWith tracer (TraceEnvSetLocalRoots localRootPeers)   --TODO: make dynamic
    traceWith tracer (TraceEnvSetPublicRoots publicRootPeers) --TODO: make dynamic
    return $ mockPeerSelectionActions'
               tracer env initialPeerTargets policy
               scripts readTargets
               readUseBootstrapPeers
               readUseLedgerPeers
               getLedgerStateJudgement
               peerConns
               onlyLocalOutboundConnsVar
               churnMutex
  where
    proxy :: Proxy m
    proxy = Proxy


data TransitionError
  = ActivationError
  | DeactivationError
  deriving (Show, Typeable)

instance Exception TransitionError where


mockPeerSelectionActions' :: forall m.
                             (MonadAsync m, MonadDelay m, MonadSTM m, MonadTimer m, Fail.MonadFail m,
                              MonadThrow (STM m))
                          => Tracer m TraceMockEnv
                          -> GovernorMockEnvironment
                          -> ConsensusModePeerTargets
                          -> PeerSelectionPolicy PeerAddr m
                          -> Map PeerAddr (TVar m PeerShareScript, TVar m PeerSharingScript, TVar m ConnectionScript)
                          -> STM m PeerSelectionTargets
                          -> STM m UseBootstrapPeers
                          -> STM m UseLedgerPeers
                          -> STM m LedgerStateJudgement
                          -> TVar m (Map PeerAddr (TVar m PeerStatus))
                          -> TVar m OutboundConnectionsState
                          -> StrictTVar.StrictTMVar m ()
                          -> PeerSelectionActions PeerAddr (PeerConn m) m
mockPeerSelectionActions' tracer
                          GovernorMockEnvironment {
                            localRootPeers,
                            publicRootPeers,
                            peerSharingFlag
                          }
                          peerTargets
                          _
                          scripts
                          readTargets
                          readUseBootstrapPeers
                          readUseLedgerPeers
                          readLedgerStateJudgement
                          connsVar
                          outboundConnectionsStateVar
                          churnMutex =
    PeerSelectionActions {
      readLocalRootPeers       = return (LocalRootPeers.toGroups localRootPeers),
      peerSharing              = peerSharingFlag,
      peerConnToPeerSharing    = \(PeerConn _ ps _) -> ps,
      requestPublicRootPeers,
      readPeerSelectionTargets = readTargets,
      requestPeerShare,
      peerStateActions         = PeerStateActions {
          establishPeerConnection,
          monitorPeerConnection,
          activatePeerConnection,
          deactivatePeerConnection,
          closePeerConnection
        },
      readUseBootstrapPeers,
      readLedgerStateJudgement,
      readInboundPeers = pure Map.empty,
      updateOutboundConnectionsState = \a -> do
        a' <- readTVar outboundConnectionsStateVar
        when (a /= a') $
          writeTVar outboundConnectionsStateVar a,
      churnMutex,
      peerTargets
    }
  where
    -- TODO: make this dynamic
    requestPublicRootPeers ledgerPeersKind _n = do
      traceWith tracer TraceEnvRequestPublicRootPeers
      let ttl :: DiffTime
          ttl = 60
      _ <- async $ do
        threadDelay ttl
        traceWith tracer TraceEnvPublicRootTTL

      -- Read the current ledger state judgement
      usingBootstrapPeers <- atomically
                           $ requiresBootstrapPeers <$> readUseBootstrapPeers
                                                    <*> readLedgerStateJudgement
      useLedgerPeers <- atomically readUseLedgerPeers
      -- If the ledger state is YoungEnough we should get ledger peers.
      -- Otherwise we should get bootstrap peers
      let publicConfigPeers = PublicRootPeers.getPublicConfigPeers publicRootPeers
          bootstrapPeers    = PublicRootPeers.getBootstrapPeers publicRootPeers
          ledgerPeers       = PublicRootPeers.getLedgerPeers publicRootPeers
          bigLedgerPeers    = PublicRootPeers.getBigLedgerPeers publicRootPeers
          result =
            if usingBootstrapPeers
               then PublicRootPeers.fromBootstrapPeers bootstrapPeers
               else case useLedgerPeers of
                 DontUseLedgerPeers -> PublicRootPeers.empty
                 UseLedgerPeers _ -> case ledgerPeersKind of
                   AllLedgerPeers
                     | Set.null ledgerPeers ->
                       PublicRootPeers.fromPublicRootPeers publicConfigPeers
                     | otherwise            ->
                       PublicRootPeers.fromLedgerPeers ledgerPeers
                   BigLedgerPeers
                     | Set.null ledgerPeers ->
                       PublicRootPeers.fromPublicRootPeers publicConfigPeers
                     | otherwise            ->
                       PublicRootPeers.fromBigLedgerPeers bigLedgerPeers

      traceWith tracer (TraceEnvRootsResult (Set.toList (PublicRootPeers.toSet result)))
      return (result, ttl)

    requestPeerShare :: PeerSharingAmount -> PeerAddr -> m (PeerSharingResult PeerAddr)
    requestPeerShare _ addr = do
      let Just (peerShareScript, _, _) = Map.lookup addr scripts
      mPeerShare <- stepScript peerShareScript
      traceWith tracer (TraceEnvPeerShareRequest addr mPeerShare)
      case mPeerShare of
        Nothing                -> do
          threadDelay 1
          traceWith tracer (TraceEnvPeerShareResult addr [])
          fail "no peers"
        Just (peeraddrs, time) -> do
          threadDelay (interpretPeerShareTime time)
          traceWith tracer (TraceEnvPeerShareResult addr peeraddrs)
          return (PeerSharingResult peeraddrs)

    establishPeerConnection :: IsBigLedgerPeer -> PeerAddr -> m (PeerConn m)
    establishPeerConnection _ peeraddr = do
      --TODO: add support for variable delays and synchronous failure
      traceWith tracer (TraceEnvEstablishConn peeraddr)
      threadDelay 1
      let Just (_, peerSharingScript, connectScript) = Map.lookup peeraddr scripts
      conn@(PeerConn _ _ v) <- atomically $ do
        conn  <- newTVar PeerWarm
        conns <- readTVar connsVar
        let !conns' = Map.insert peeraddr conn conns
        writeTVar connsVar conns'
        remotePeerSharing <- stepScriptSTM peerSharingScript
        return (PeerConn peeraddr (peerSharingFlag <> remotePeerSharing) conn)
      _ <- async $
        -- monitoring loop which does asynchronous demotions. It will terminate
        -- as soon as either of the events:
        --
        -- + the script returns 'Noop'
        -- + peer demoted to 'PeerCold'
        --
        let loop = do
              (demotion, delay) <- stepScript connectScript
              let interpretScriptDelay NoDelay    = 1
                  interpretScriptDelay ShortDelay = 60
                  interpretScriptDelay LongDelay  = 600
                  interpretScriptDelay (Delay a)  = a -- not used by the generator
              done <-
                case demotion of
                  Noop   -> return True
                  ToWarm -> do
                    threadDelay (interpretScriptDelay delay)
                    atomically $ do
                      s <- readTVar v
                      case s of
                        PeerHot  -> writeTVar v PeerWarm
                                 >> return False
                        PeerCold -> return True
                        _        -> return False
                  ToCooling -> do
                    threadDelay (interpretScriptDelay delay)
                    atomically $ do
                      s <- readTVar v
                      case s of
                        PeerCooling -> return False
                        PeerCold -> return True
                        _        -> writeTVar v PeerCooling
                                 >> return False
                  ToCold -> do
                    threadDelay (interpretScriptDelay delay)
                    atomically $ do
                      s <- readTVar v
                      case s of
                        PeerCold -> return True
                        _        -> writeTVar v PeerCold
                                 >> return True

              traceWith tracer (TraceEnvPeersDemote demotion peeraddr)

              if done
                then return ()
                else loop
        in loop
      return conn

    activatePeerConnection :: IsBigLedgerPeer -> PeerConn m -> m ()
    activatePeerConnection _ (PeerConn peeraddr _ conn) = do
      traceWith tracer (TraceEnvActivatePeer peeraddr)
      threadDelay 1
      atomically $ do
        status <- readTVar conn
        case status of
          PeerHot     -> error "activatePeerConnection of hot peer"
          PeerWarm    -> writeTVar conn PeerHot
          --TODO: check it's just a race condition and not just wrong:
          --
          -- We throw 'ActivationError' for the following reason:
          -- 'PeerCold' can be set by the monitoring loop started by
          -- 'establishedPeerConnection' above.  However if that happens we
          -- want to signal the governor that the warm -> hot transition
          -- errored.  Otherwise 'jobPromoteWarmPeer' will try to update the
          -- state as if the transition went fine which will violate
          -- 'invariantPeerSelectionState'.
          PeerCooling -> throwIO ActivationError
          PeerCold    -> throwIO ActivationError

    deactivatePeerConnection :: PeerConn m -> m ()
    deactivatePeerConnection (PeerConn peeraddr _ conn) = do
      traceWith tracer (TraceEnvDeactivatePeer peeraddr)
      atomically $ do
        status <- readTVar conn
        case status of
          PeerHot     -> writeTVar conn PeerWarm
          --TODO: check it's just a race condition and not just wrong:
          PeerWarm    -> return ()
          -- See the note in 'activatePeerConnection' why we throw an exception
          -- here.
          PeerCooling -> throwIO DeactivationError
          PeerCold    -> throwIO DeactivationError

    closePeerConnection :: PeerConn m -> m ()
    closePeerConnection (PeerConn peeraddr _ conn) = do
      traceWith tracer (TraceEnvCloseConn peeraddr)
      atomically $ do
        status <- readTVar conn
        case status of
          PeerHot     -> writeTVar conn PeerCold
          --TODO: check it's just a race condition and not just wrong:
          PeerWarm    -> writeTVar conn PeerCold
          PeerCooling -> writeTVar conn PeerCold
          PeerCold    -> return ()
        conns <- readTVar connsVar
        let !conns' = Map.delete peeraddr conns
        writeTVar connsVar conns'

    monitorPeerConnection :: PeerConn m -> STM m (PeerStatus, Maybe RepromoteDelay)
    monitorPeerConnection (PeerConn _peeraddr _ conn) = do
      st <- readTVar conn
      pure $ case st of
        PeerCooling -> (st, Nothing)
        _           -> (st, Just config_REPROMOTE_DELAY)


config_REPROMOTE_DELAY :: RepromoteDelay
config_REPROMOTE_DELAY = 10


snapshotPeersStatus :: MonadInspectSTM m
                    => proxy m
                    -> Map PeerAddr (TVar m PeerStatus)
                    -> InspectMonad m (Map PeerAddr PeerStatus)
snapshotPeersStatus p conns = traverse (inspectTVar p) conns


mockPeerSelectionPolicy  :: MonadSTM m
                         => GovernorMockEnvironment
                         -> m (PeerSelectionPolicy PeerAddr m)
mockPeerSelectionPolicy GovernorMockEnvironment {
                          pickKnownPeersForPeerShare,
                          pickColdPeersToPromote,
                          pickWarmPeersToPromote,
                          pickHotPeersToDemote,
                          pickWarmPeersToDemote,
                          pickColdPeersToForget,
                          pickInboundPeers
                        } = do
    pickKnownPeersForPeerShareVar <- initScript' pickKnownPeersForPeerShare
    pickColdPeersToPromoteVar  <- initScript' pickColdPeersToPromote
    pickWarmPeersToPromoteVar  <- initScript' pickWarmPeersToPromote
    pickHotPeersToDemoteVar    <- initScript' pickHotPeersToDemote
    pickWarmPeersToDemoteVar   <- initScript' pickWarmPeersToDemote
    pickColdPeersToForgetVar   <- initScript' pickColdPeersToForget
    pickInboundPeersVar        <- initScript' pickInboundPeers
    return PeerSelectionPolicy {
      policyPickKnownPeersForPeerShare = \_ _ _ -> interpretPickScript pickKnownPeersForPeerShareVar,
      policyPickColdPeersToPromote  = \_ _ _ -> interpretPickScript pickColdPeersToPromoteVar,
      policyPickWarmPeersToPromote  = \_ _ _ -> interpretPickScript pickWarmPeersToPromoteVar,
      policyPickHotPeersToDemote    = \_ _ _ -> interpretPickScript pickHotPeersToDemoteVar,
      policyPickWarmPeersToDemote   = \_ _ _ -> interpretPickScript pickWarmPeersToDemoteVar,
      policyPickColdPeersToForget   = \_ _ _ -> interpretPickScript pickColdPeersToForgetVar,
      policyPickInboundPeers        = \_ _ _ -> interpretPickScript pickInboundPeersVar,
      policyFindPublicRootTimeout   = 5,    -- seconds
      policyMaxInProgressPeerShareReqs = 2,
      policyPeerShareRetryTime         = 3600, -- seconds
      policyPeerShareBatchWaitTime     = 3,    -- seconds
      policyPeerShareOverallTimeout    = 10,   -- seconds
      policyPeerShareActivationDelay   = 300,  -- seconds
      policyErrorDelay                 = 10    -- seconds
    }

--
-- Utils for properties
--

data TestTraceEvent = GovernorDebug           !(DebugPeerSelection PeerAddr)
                    | GovernorEvent           !(TracePeerSelection PeerAddr)
                    | GovernorCounters        !PeerSelectionCounters
                    | GovernorAssociationMode !AssociationMode
                    | MockEnvEvent            !TraceMockEnv
                   -- Warning: be careful with writing properties that rely
                   -- on trace events from both the governor and from the
                   -- environment. These events typically occur in separate
                   -- threads and so are not casually ordered. It is ok to use
                   -- them for timeout/eventually properties, but not for
                   -- properties that check conditions synchronously.
                   -- The governor debug vs other events are fully ordered.
  deriving Show

tracerTracePeerSelection :: Tracer (IOSim s) (TracePeerSelection PeerAddr)
tracerTracePeerSelection = contramap f tracerTestTraceEvent
  where
    -- make the tracer strict
    f :: TracePeerSelection PeerAddr -> TestTraceEvent
    f a@(TraceLocalRootPeersChanged !_ !_)                   = GovernorEvent a
    f a@(TraceTargetsChanged !_ !_)                          = GovernorEvent a
    f a@(TracePublicRootsRequest !_ !_)                      = GovernorEvent a
    f a@(TracePublicRootsResults !_ !_ !_)                   = GovernorEvent a
    f a@(TracePublicRootsFailure !_ !_ !_)                   = GovernorEvent a
    f a@(TraceForgetColdPeers !_ !_ !_)                      = GovernorEvent a
    f a@(TraceBigLedgerPeersRequest !_ !_)                   = GovernorEvent a
    f a@(TraceBigLedgerPeersResults !_ !_ !_)                = GovernorEvent a
    f a@(TraceBigLedgerPeersFailure !_ !_ !_)                = GovernorEvent a
    f a@(TraceForgetBigLedgerPeers !_ !_ !_)                 = GovernorEvent a
    f a@(TracePickInboundPeers !_ !_ !_ !_)                  = GovernorEvent a
    f a@(TracePeerShareRequests !_ !_ !_ !_ !_)              = GovernorEvent a
    f a@(TracePeerShareResults !_)                           = GovernorEvent a
    f a@(TracePeerShareResultsFiltered !_)                   = GovernorEvent a
    f a@(TracePromoteColdPeers !_ !_ !_)                     = GovernorEvent a
    f a@(TracePromoteColdLocalPeers !_ !_)                   = GovernorEvent a
    f a@(TracePromoteColdFailed !_ !_ !_ !_ !_)              = GovernorEvent a
    f a@(TracePromoteColdDone !_ !_ !_)                      = GovernorEvent a
    f a@(TracePromoteColdBigLedgerPeers !_ !_ !_)            = GovernorEvent a
    f a@(TracePromoteColdBigLedgerPeerFailed !_ !_ !_ !_ !_) = GovernorEvent a
    f a@(TracePromoteColdBigLedgerPeerDone !_ !_ !_)         = GovernorEvent a
    f a@(TracePromoteWarmPeers !_ !_ !_)                     = GovernorEvent a
    f a@(TracePromoteWarmLocalPeers !_ !_)                   = GovernorEvent a
    f a@(TracePromoteWarmFailed !_ !_ !_ !_)                 = GovernorEvent a
    f a@(TracePromoteWarmDone !_ !_ !_)                      = GovernorEvent a
    f a@(TracePromoteWarmAborted !_ !_ !_)                   = GovernorEvent a
    f a@(TracePromoteWarmBigLedgerPeers !_ !_ !_)            = GovernorEvent a
    f a@(TracePromoteWarmBigLedgerPeerFailed !_ !_ !_ !_)    = GovernorEvent a
    f a@(TracePromoteWarmBigLedgerPeerDone !_ !_ !_)         = GovernorEvent a
    f a@(TracePromoteWarmBigLedgerPeerAborted !_ !_ !_)      = GovernorEvent a
    f a@(TraceDemoteWarmPeers !_ !_ !_)                      = GovernorEvent a
    f a@(TraceDemoteWarmFailed !_ !_ !_ !_)                  = GovernorEvent a
    f a@(TraceDemoteWarmDone !_ !_ !_)                       = GovernorEvent a
    f a@(TraceDemoteWarmBigLedgerPeers !_ !_ !_)             = GovernorEvent a
    f a@(TraceDemoteWarmBigLedgerPeerFailed !_ !_ !_ !_)     = GovernorEvent a
    f a@(TraceDemoteWarmBigLedgerPeerDone !_ !_ !_)          = GovernorEvent a
    f a@(TraceDemoteHotPeers !_ !_ !_)                       = GovernorEvent a
    f a@(TraceDemoteLocalHotPeers !_ !_)                     = GovernorEvent a
    f a@(TraceDemoteHotFailed !_ !_ !_ !_)                   = GovernorEvent a
    f a@(TraceDemoteHotDone !_ !_ !_)                        = GovernorEvent a
    f a@(TraceDemoteHotBigLedgerPeers !_ !_ !_)              = GovernorEvent a
    f a@(TraceDemoteHotBigLedgerPeerFailed !_ !_ !_ !_)      = GovernorEvent a
    f a@(TraceDemoteHotBigLedgerPeerDone !_ !_ !_)           = GovernorEvent a
    f a@(TraceDemoteAsynchronous !_)                         = GovernorEvent a
    f a@(TraceDemoteLocalAsynchronous !_)                    = GovernorEvent a
    f a@(TraceDemoteBigLedgerPeersAsynchronous !_)           = GovernorEvent a
    f a@TraceGovernorWakeup                                  = GovernorEvent a
    f a@(TraceChurnWait !_)                                  = GovernorEvent a
    f a@(TraceChurnMode !_)                                  = GovernorEvent a
    f a@(TraceLedgerStateJudgementChanged !_)                = GovernorEvent a
    f a@TraceOnlyBootstrapPeers                              = GovernorEvent a
    f a@TraceBootstrapPeersFlagChangedWhilstInSensitiveState = GovernorEvent a
    f a@(TraceUseBootstrapPeersChanged !_)                   = GovernorEvent a
    f a@(TraceOutboundGovernorCriticalFailure !_)            = GovernorEvent a
    f a@(TraceDebugState !_ !_)                              = GovernorEvent a
    f a@(TraceChurnAction !_ !_ !_)                          = GovernorEvent a
    f a@(TraceChurnTimeout !_ !_ !_)                         = GovernorEvent a

tracerDebugPeerSelection :: Tracer (IOSim s) (DebugPeerSelection PeerAddr)
tracerDebugPeerSelection = GovernorDebug `contramap` tracerTestTraceEvent

traceAssociationMode :: PeerSelectionInterfaces PeerAddr (PeerConn (IOSim s)) (IOSim s)
                     -> PeerSelectionActions PeerAddr (PeerConn (IOSim s)) (IOSim s)
                     -> Tracer (IOSim s) (DebugPeerSelection PeerAddr)
traceAssociationMode interfaces actions = Tracer $ \(TraceGovernorState _ _ st) -> do
    associationMode <- atomically $ readAssociationMode
                                           (readUseLedgerPeers interfaces)
                                           (Governor.peerSharing actions)
                                           (Governor.bootstrapPeersFlag st)
    traceWith tracerTestTraceEvent (GovernorAssociationMode associationMode)

tracerTracePeerSelectionCounters :: Tracer (IOSim s) PeerSelectionCounters
tracerTracePeerSelectionCounters = contramap GovernorCounters tracerTestTraceEvent

tracerMockEnv :: Tracer (IOSim s) TraceMockEnv
tracerMockEnv = contramap MockEnvEvent tracerTestTraceEvent

tracerTestTraceEvent :: Tracer (IOSim s) TestTraceEvent
tracerTestTraceEvent = dynamicTracer <> Tracer (say . show)

dynamicTracer :: Typeable a => Tracer (IOSim s) a
dynamicTracer = Tracer traceM

selectPeerSelectionTraceEvents :: SimTrace a -> [(Time, TestTraceEvent)]
selectPeerSelectionTraceEvents = go
  where
    go (SimTrace t _ _ (EventLog e) trace)
     | Just x <- fromDynamic e       = (t,x) : go trace
    go (SimPORTrace t _ _ _ (EventLog e) trace)
     | Just x <- fromDynamic e       = (t,x) : go trace
    go (SimTrace _ _ _ _ trace)      =         go trace
    go (SimPORTrace _ _ _ _ _ trace) =         go trace
    go (TraceRacesFound _ trace)     =         go trace
    go (TraceMainException _ _ e _)  = throw e
    go (TraceDeadlock      _   _)    = [] -- expected result in many cases
    go  TraceMainReturn {}           = []
    go (TraceInternalError e)        = error ("IOSim: " ++ e)
    go TraceLoop                     = error "Step time limit exceeded"

selectPeerSelectionTraceEventsUntil :: Time -> SimTrace a -> [(Time, TestTraceEvent)]
selectPeerSelectionTraceEventsUntil tmax = go
  where
    go (SimTrace t _ _ _ _)
     | t > tmax                      = []
    go (SimTrace t _ _ (EventLog e) trace)
     | Just x <- fromDynamic e       = (t,x) : go trace
    go (SimPORTrace t _ _ _ _ _)
     | t > tmax                      = []
    go (SimPORTrace t _ _ _ (EventLog e) trace)
     | Just x <- fromDynamic e       = (t,x) : go trace
    go (SimTrace _ _ _ _ trace)      =         go trace
    go (SimPORTrace _ _ _ _ _ trace) =         go trace
    go (TraceRacesFound _ trace)     =         go trace
    go (TraceMainException _ _ e _)  = throw e
    go (TraceDeadlock      _   _)    = [] -- expected result in many cases
    go  TraceMainReturn {}           = []
    go (TraceInternalError e)        = error ("IOSim: " ++ e)
    go TraceLoop                     = error "Step time limit exceeded"

selectGovernorEvents :: [(Time, TestTraceEvent)]
                     -> [(Time, TracePeerSelection PeerAddr)]
selectGovernorEvents trace = [ (t, e) | (t, GovernorEvent e) <- trace ]

selectGovernorStateEvents :: [(Time, TestTraceEvent)]
                          -> [(Time, DebugPeerSelection PeerAddr)]
selectGovernorStateEvents trace = [ (t, e) | (t, GovernorDebug e) <- trace ]



--
-- QuickCheck instances
--

instance Arbitrary GovernorPraosMockEnvironment where
  arbitrary = do
    mockEnv <- arbitrary
    bootstrapScript <- arbitrary
    return $ GovernorPraosMockEnvironment mockEnv {
      consensusMode = PraosMode,
      useBootstrapPeers = bootstrapScript }

  shrink env = GovernorPraosMockEnvironment <$> shrink (getMockEnv env)

instance Arbitrary GovernorMockEnvironment where
  arbitrary = do
      -- Dependency of the root set on the graph
      peerGraph         <- arbitrary
      let peersSet       = allPeers peerGraph
      (localRootPeers,
       publicRootPeers) <- arbitraryRootPeers peersSet

      let arbitrarySubsetOfPeers = arbitrarySubset peersSet
      pickKnownPeersForPeerShare <- arbitraryPickScript arbitrarySubsetOfPeers
      pickColdPeersToPromote  <- arbitraryPickScript arbitrarySubsetOfPeers
      pickWarmPeersToPromote  <- arbitraryPickScript arbitrarySubsetOfPeers
      pickHotPeersToDemote    <- arbitraryPickScript arbitrarySubsetOfPeers
      pickWarmPeersToDemote   <- arbitraryPickScript arbitrarySubsetOfPeers
      pickColdPeersToForget   <- arbitraryPickScript arbitrarySubsetOfPeers
      pickInboundPeers        <- arbitraryPickScript arbitrarySubsetOfPeers
      peerSharingFlag         <- arbitrary
      consensusMode           <- arbitrary
      useBootstrapPeers       <- case consensusMode of
                                   GenesisMode -> pure $ singletonTimedScript DontUseBootstrapPeers
                                   PraosMode   -> arbitrary
      useLedgerPeers          <- arbitrary
      ledgerStateJudgement0   <- listOf arbitrary
     
      (ledgerStateJudgement, targets) <-
        let wrap = Script . NonEmpty.fromList
            -- we want to generate targets which respect the number of local roots
            -- and locally configured public roots which we plucked from the peer
            -- graph
            (HotValency localHot) = LocalRootPeers.hotTarget localRootPeers
            (WarmValency localWarm) = LocalRootPeers.warmTarget localRootPeers
            publicConfiguredRootSize = Set.size . PublicRootPeers.toPublicConfigPeerSet $ publicRootPeers
            knownOffset = localWarm + publicConfiguredRootSize
            knownGen = (knownOffset +) <$> resize (1000 - min 1000 knownOffset) arbitrarySizedNatural
            rootKnownGen knownMax = choose (0, min 100 (knownMax - localWarm))
            estGen knownMax = choose (localWarm, min 1000 knownMax)
            actGen estMax = choose (localHot, min 100 estMax)
            
        in forM (   ledgerStateJudgement0
                 ++ [ArbitraryLedgerStateJudgement YoungEnough])
             (\(ArbitraryLedgerStateJudgement lsj) -> do
                  (praosKnown, genesisKnown) <- (,) <$> knownGen <*> knownGen
                  (praosRootKnown, genesisRootKnown) <- (,) <$> rootKnownGen praosKnown <*> rootKnownGen genesisKnown
                  (praosEst, genesisEst) <- (,) <$> estGen praosKnown <*> estGen genesisKnown
                  (praosAct, genesisAct) <- (,) <$> actGen praosEst <*> actGen genesisEst
                  (praosBigKnown, Positive genesisBigKnown)
                    <- (,) <$> resize 1000 arbitrarySizedNatural
                           <*> resize 1000 arbitrary `suchThat` ((>= 10) . getPositive)
                  (praosBigEst, genesisBigEst) <- (,) <$> choose (0, min 1000 praosBigKnown)
                                                      <*> choose (1, min 1000 genesisBigKnown)
                  (praosBigAct, genesisBigAct) <- (,) <$> choose (0, min 100 praosBigEst)
                                                      <*> choose (1, min 100 genesisBigEst)

                  let targets0 =
                        ConsensusModePeerTargets {
                          praosTargets = PeerSelectionTargets {
                              targetNumberOfRootPeers = praosRootKnown,
                              targetNumberOfKnownPeers = praosKnown,
                              targetNumberOfEstablishedPeers = praosEst,
                              targetNumberOfActivePeers = praosAct,
                              targetNumberOfKnownBigLedgerPeers = praosBigKnown,
                              targetNumberOfEstablishedBigLedgerPeers = praosBigEst,
                              targetNumberOfActiveBigLedgerPeers = praosBigAct },
                          genesisSyncTargets = PeerSelectionTargets {
                              targetNumberOfRootPeers = genesisRootKnown,
                              targetNumberOfKnownPeers = genesisKnown,
                              targetNumberOfEstablishedPeers = genesisEst,
                              targetNumberOfActivePeers = genesisAct,
                              targetNumberOfKnownBigLedgerPeers = genesisBigKnown,
                              targetNumberOfEstablishedBigLedgerPeers = genesisBigKnown,
                              targetNumberOfActiveBigLedgerPeers = genesisBigAct } }
                  let lsjWithDelay = (,) lsj <$> elements [ShortDelay, NoDelay]
                      -- synchronize target basis with ledger state judgement
                      -- so we can use tests which check if the right targets
                      -- are selected
                      targetsWithDelay = (,) targets0 <$> case consensusMode of
                                                     PraosMode -> elements [ShortDelay, NoDelay]
                                                     GenesisMode -> snd <$> lsjWithDelay
                  (,) <$> lsjWithDelay <*> targetsWithDelay)
           <&> bimap wrap wrap . unzip

      return GovernorMockEnvironment{..}
    where
      arbitraryRootPeers :: Set PeerAddr
                         -> Gen (LocalRootPeers PeerAddr, PublicRootPeers PeerAddr)
      arbitraryRootPeers peers | Set.null peers =
        return (LocalRootPeers.empty, PublicRootPeers.empty)

      arbitraryRootPeers peers = do
        -- We decide how many we want and then pick randomly.
        sz <- getSize
        let minroots
              | sz >= 10  = 1
              | otherwise = 0
            maxroots      = ceiling
                          . sqrt
                          . (fromIntegral :: Int -> Double)
                          . length
                          $ peers
        numroots  <- choose (minroots, maxroots)
        ixs       <- vectorOf numroots (getNonNegative <$> arbitrary)
        let pick n    = Set.elemAt i peers where i = n `mod` Set.size peers
            rootPeers = nub (map pick ixs)
        -- divide into local and public, but with a bit of overlap:
        local <- vectorOf (length rootPeers) (choose (0, 10 :: Int))
        -- Deliberately asking for a small intersection in order to test if
        -- the Governor actually takes care of this invariant
        let localRootsSet  = Set.fromList [ x | (x, v) <- zip rootPeers local
                                              , v <= 5 ]
            publicRootsSet = nub [ x | (x, v) <- zip rootPeers local
                                     , v >= 5 ]
        pAdvPLedger <- vectorOf (length publicRootsSet)
                               ((,) <$> arbitrary <*> arbitrary)
        let publicRoots = Map.fromList (zip publicRootsSet pAdvPLedger)

        numBigLedgerPeers <- choose (minroots, numroots)
        -- `publicRoots` might be empty
        ixs' <- vectorOf numBigLedgerPeers (getNonNegative <$> arbitrary)
        let bigLedgerPeers = (Set.\\ localRootsSet)
                           . Set.fromList
                           . map (\(_,_,a) -> a)
                           . filter (\(ix, ix', _) ->
                                       ix == ix' `mod` Map.size publicRoots)
                           $ zip3 [0..] ixs' (Map.keys publicRoots)

        let (publicConfigPeers, otherPeers) =
              span (\case (_, (x, _)) -> not x)
                   (zip publicRootsSet pAdvPLedger)
            (publicConfigPeersMap, (boostrapPeers, ledgerPeers)) =
              ( Map.fromList $ map (\(p, (_, pa)) -> (p, pa)) publicConfigPeers
              , let otherPeers' = map fst otherPeers
                 in splitAt (length otherPeers' `div` 2) otherPeers'
              )

        localRoots <- arbitraryLocalRootPeers localRootsSet
        return ( localRoots
               , PublicRootPeers.fromMapAndSet
                  publicConfigPeersMap
                  (Set.fromList boostrapPeers)
                  (Set.fromList ledgerPeers)
                  bigLedgerPeers
               )

  shrink env@GovernorMockEnvironment {
           peerGraph,
           localRootPeers,
           publicRootPeers,
           targets,
           pickKnownPeersForPeerShare,
           pickColdPeersToPromote,
           pickWarmPeersToPromote,
           pickHotPeersToDemote,
           pickWarmPeersToDemote,
           pickColdPeersToForget,
           pickInboundPeers,
           peerSharingFlag,
           useBootstrapPeers,
           consensusMode,
           useLedgerPeers,
           ledgerStateJudgement
         } =
      -- Special rule for shrinking the peerGraph because the localRootPeers
      -- depends on it so has to be updated too.
      [ env {
          peerGraph       = peerGraph',
          localRootPeers  = LocalRootPeers.restrictKeys localRootPeers nodes',
          publicRootPeers = publicRootPeers `PublicRootPeers.intersection` nodes'
        }
      | peerGraph' <- shrink peerGraph
      , let nodes' = allPeers peerGraph' ]
      -- All the others are generic.
   ++ [ env { localRootPeers = localRootPeers' }
      | localRootPeers' <- shrinkLocalRootPeers localRootPeers
      ]
   ++ [ env { publicRootPeers = publicRootPeers' }
      | publicRootPeers' <- shrinkPublicRootPeers publicRootPeers
      ]
   ++ [ env { targets = targets' }
      | targets' <- shrinkScriptWith shrinkTargets targets
      ]
   ++ [ env { pickKnownPeersForPeerShare = pickKnownPeersForPeerShare' }
      | pickKnownPeersForPeerShare' <- shrink pickKnownPeersForPeerShare
      ]
   ++ [ env { pickColdPeersToPromote = pickColdPeersToPromote' }
      | pickColdPeersToPromote' <- shrink pickColdPeersToPromote
      ]
   ++ [ env { pickWarmPeersToPromote = pickWarmPeersToPromote' }
      | pickWarmPeersToPromote' <- shrink pickWarmPeersToPromote
      ]
   ++ [ env { pickWarmPeersToDemote = pickWarmPeersToDemote' }
      | pickWarmPeersToDemote' <- shrink pickWarmPeersToDemote
      ]
   ++ [ env { pickHotPeersToDemote = pickHotPeersToDemote' }
      | pickHotPeersToDemote' <- shrink pickHotPeersToDemote
      ]
   ++ [ env { pickColdPeersToForget = pickColdPeersToForget' }
      | pickColdPeersToForget' <- shrink pickColdPeersToForget
      ]
   ++ [ env { pickInboundPeers = pickInboundPeers' }
      | pickInboundPeers' <- shrink pickInboundPeers
      ]
   ++ [ env { useBootstrapPeers = useBootstrapPeers' }
      | useBootstrapPeers' <- shrink useBootstrapPeers
      ]
   ++ [ env { useLedgerPeers = useLedgerPeers' }
      | useLedgerPeers' <- shrink useLedgerPeers
      ]
   ++ [ env { ledgerStateJudgement = fmap (first getArbitraryLedgerStateJudgement) ledgerStateJudgement' }
      | ledgerStateJudgement' <- shrink (fmap (first ArbitraryLedgerStateJudgement) ledgerStateJudgement)
      ]
   ++ [ env { peerSharingFlag = peerSharingFlag' }
      | peerSharingFlag' <- shrink peerSharingFlag
      ]
   ++ [ env { consensusMode = consensusMode' }
      | consensusMode' <- shrink consensusMode
      ]
    where
      -- A sensible shrink will not decrease target of known peers below
      -- the minimum number of local root peers that we need from configuration (hot+warm)
      -- and the number of publicly configured root peers. A similar treatment with
      -- appropriate conditions is necessary for established and active peers.
      shrinkTargets targetsWithDelay =
        let publicConfiguredRootSize = Set.size . PublicRootPeers.toPublicConfigPeerSet $ publicRootPeers
            (HotValency hotLocalRootsSize) = LocalRootPeers.hotTarget localRootPeers
            (WarmValency warmLocalRootsSize) = LocalRootPeers.warmTarget localRootPeers
            shrunkScript = shrink targetsWithDelay
            checkTargets t = 
                 targetNumberOfKnownPeers t >= publicConfiguredRootSize + warmLocalRootsSize
              && targetNumberOfEstablishedPeers t >= warmLocalRootsSize
              && targetNumberOfEstablishedPeers t <= targetNumberOfKnownPeers t
              && targetNumberOfActivePeers t >= hotLocalRootsSize
              && targetNumberOfActivePeers t <= targetNumberOfEstablishedPeers t
              && targetNumberOfRootPeers t <=   targetNumberOfKnownPeers t
                                              - warmLocalRootsSize            
        in
          [shrunk
          | shrunk@(shrunkTarget, _) <- shrunkScript,
            let ConsensusModePeerTargets {
                  praosTargets,
                  genesisSyncTargets = genesisSyncTargets@PeerSelectionTargets {
                      targetNumberOfKnownBigLedgerPeers = genesisBigKnown,
                      targetNumberOfEstablishedBigLedgerPeers = genesisBigEst,
                      targetNumberOfActiveBigLedgerPeers = genesisBigAct } } = shrunkTarget,
            all checkTargets [praosTargets, genesisSyncTargets],
            genesisBigKnown >= 10 && genesisBigEst <= genesisBigKnown && genesisBigAct <= genesisBigEst,
            genesisBigEst * genesisBigAct /= 0]
        
      shrinkLocalRootPeers a =
        [ LocalRootPeers.fromGroups g
          | g <- shrink (LocalRootPeers.toGroups a)
        ]
      shrinkPublicRootPeers (PublicRootPeers pp bsp lp blp) =
        [ PublicRootPeers pp' bsp' lp' blp'
          | (pp', bsp', lp', blp') <- shrink (pp, bsp, lp, blp)
        ]

--
-- Tests for the QC Arbitrary instances
--

prop_arbitrary_GovernorMockEnvironment :: GovernorMockEnvironment -> Property
prop_arbitrary_GovernorMockEnvironment env =
    tabulate "num root peers"        [show (LocalRootPeers.size (localRootPeers env)
                                          + PublicRootPeers.size (publicRootPeers env))] $
    tabulate "num local root peers"  [show (LocalRootPeers.size (localRootPeers env))] $
    tabulate "num public root peers" [show (PublicRootPeers.size (publicRootPeers env))] $
    tabulate "empty root peers" [show $ not emptyGraph && emptyRootPeers]  $
    tabulate "overlapping local/public roots" [show overlappingRootPeers]  $
    tabulate "num big ledger peers"  [show (Set.size bigLedgerPeersSet)] $

    validGovernorMockEnvironment env
  where
    bigLedgerPeersSet = PublicRootPeers.getBigLedgerPeers (publicRootPeers env)
    emptyGraph     = null g where PeerGraph g = peerGraph env
    emptyRootPeers = LocalRootPeers.null (localRootPeers env)
                  && PublicRootPeers.null (publicRootPeers env)
    overlappingRootPeers =
      not $ PublicRootPeers.null $
        PublicRootPeers.intersection
          (publicRootPeers env)
          (LocalRootPeers.keysSet (localRootPeers env))

prop_shrink_GovernorMockEnvironment :: ShrinkCarefully GovernorMockEnvironment -> Property
prop_shrink_GovernorMockEnvironment x =
      prop_shrink_valid validGovernorMockEnvironment x
 .&&. prop_shrink_nonequal x

prop_shrink_nonequal_GovernorMockEnvironment ::
  ShrinkCarefully GovernorMockEnvironment -> Property
prop_shrink_nonequal_GovernorMockEnvironment = prop_shrink_nonequal

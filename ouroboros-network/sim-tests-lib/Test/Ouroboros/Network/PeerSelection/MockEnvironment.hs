{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}


{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.Ouroboros.Network.PeerSelection.MockEnvironment
  ( PeerGraph (..)
  , GovernorMockEnvironment (..)
  , GovernorMockEnvironmentWithoutAsyncDemotion (..)
  , runGovernorInMockEnvironment
  , exploreGovernorInMockEnvironment
  , TraceMockEnv (..)
  , TestTraceEvent (..)
  , selectGovernorEvents
  , selectPeerSelectionTraceEvents
  , selectPeerSelectionTraceEventsUntil
  , peerShareReachablePeers
  , module Ouroboros.Network.Testing.Data.Script
  , module Ouroboros.Network.PeerSelection.Types
  , tests
  , prop_shrink_nonequal_GovernorMockEnvironment
  ) where

import           Data.Dynamic (fromDynamic)
import           Data.List (nub)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Proxy (Proxy (..))
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Typeable (Typeable)
import           Data.Void (Void)
import           System.Random (mkStdGen)

import           Control.Concurrent.Class.MonadSTM
import qualified Control.Concurrent.Class.MonadSTM.Strict as StrictTVar
import           Control.Exception (throw)
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadTest
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime.SI
import           Control.Monad.Class.MonadTimer.SI hiding (timeout)
import qualified Control.Monad.Fail as Fail
import           Control.Monad.IOSim
import           Control.Tracer (Tracer (..), contramap, traceWith)

import           Ouroboros.Network.ExitPolicy
import           Ouroboros.Network.PeerSelection.Governor hiding
                     (PeerSelectionState (..))
import qualified Ouroboros.Network.PeerSelection.State.LocalRootPeers as LocalRootPeers

import           Ouroboros.Network.Testing.Data.Script (PickScript, Script (..),
                     ScriptDelay (..), TimedScript, arbitraryPickScript,
                     arbitraryScriptOf, initScript', interpretPickScript,
                     playTimedScript, prop_shrink_Script, singletonScript,
                     stepScript, stepScriptSTM')
import           Ouroboros.Network.Testing.Utils (ShrinkCarefully,
                     arbitrarySubset, nightlyTest, prop_shrink_nonequal,
                     prop_shrink_valid)

import           Test.Ouroboros.Network.PeerSelection.Instances
import           Test.Ouroboros.Network.PeerSelection.LocalRootPeers as LocalRootPeers hiding
                     (tests)
import           Test.Ouroboros.Network.PeerSelection.PeerGraph

import           Ouroboros.Network.PeerSelection.LedgerPeers (IsBigLedgerPeer,
                     IsLedgerPeer)
import           Ouroboros.Network.PeerSelection.PeerAdvertise (PeerAdvertise)
import           Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing (..))
import           Ouroboros.Network.PeerSelection.Types (PeerStatus (..))
import           Ouroboros.Network.Protocol.PeerSharing.Type (PeerSharingAmount,
                     PeerSharingResult (..))
import           Test.QuickCheck
import           Test.Tasty (TestTree, localOption, testGroup)
import           Test.Tasty.QuickCheck (QuickCheckMaxSize (..), testProperty)


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
       peerGraph                  :: PeerGraph,
       localRootPeers             :: LocalRootPeers PeerAddr,
       publicRootPeers            :: Map PeerAddr (PeerAdvertise, IsLedgerPeer),
       bigLedgerPeers             :: Set PeerAddr,
       targets                    :: TimedScript PeerSelectionTargets,
       pickKnownPeersForPeerShare :: PickScript PeerAddr,
       pickColdPeersToPromote     :: PickScript PeerAddr,
       pickWarmPeersToPromote     :: PickScript PeerAddr,
       pickHotPeersToDemote       :: PickScript PeerAddr,
       pickWarmPeersToDemote      :: PickScript PeerAddr,
       pickColdPeersToForget      :: PickScript PeerAddr,
       peerSharing                :: PeerSharing
     }
  deriving (Show, Eq)

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
                               bigLedgerPeers,
                               targets
                             } =
        counterexample "invalid peer graph"
        (validPeerGraph peerGraph)
   .&&. counterexample "local roots not a subset of all peers"
        (LocalRootPeers.keysSet localRootPeers `Set.isSubsetOf` allPeersSet)
   .&&. counterexample "public roots not a subset of all peers"
        (Map.keysSet publicRootPeers `Set.isSubsetOf` allPeersSet)
   .&&. foldl (\p (a,_) -> p .&&. counterexample ("in sane targets: " ++ show a) (sanePeerSelectionTargets a))
              (property True) targets
   .&&. counterexample "big ledger peers not a subset of public roots"
        (bigLedgerPeers `Set.isSubsetOf` Map.keysSet publicRootPeers)
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
governorAction mockEnv = do
    publicStateVar <- StrictTVar.newTVarIO emptyPublicPeerSelectionState
    policy  <- mockPeerSelectionPolicy                mockEnv
    actions <- mockPeerSelectionActions tracerMockEnv mockEnv policy
    exploreRaces      -- explore races within the governor
    _ <- forkIO $ do  -- races with the governor should be explored
      labelThisThread "outbound-governor"
      _ <- peerSelectionGovernor
        tracerTracePeerSelection
        tracerDebugPeerSelection
        tracerTracePeerSelectionCounters
        (mkStdGen 42)
        publicStateVar
        actions
        policy
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
                  | TraceEnvSetPublicRoots !(Map PeerAddr (PeerAdvertise, IsLedgerPeer))
                  | TraceEnvPublicRootTTL
                  | TraceEnvBigLedgerPeersTTL
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
  deriving Show

mockPeerSelectionActions :: forall m.
                            (MonadAsync m, MonadDelay m, MonadTimer m,
                             Fail.MonadFail m, MonadThrow (STM m), MonadTraceSTM m)
                         => Tracer m TraceMockEnv
                         -> GovernorMockEnvironment
                         -> PeerSelectionPolicy PeerAddr m
                         -> m (PeerSelectionActions PeerAddr (PeerConn m) m)
mockPeerSelectionActions tracer
                         env@GovernorMockEnvironment {
                           peerGraph,
                           localRootPeers,
                           publicRootPeers,
                           targets
                         }
                         policy = do
    scripts <- Map.fromList <$>
                 sequence
                   [ (\a b c -> (addr, (a, b, c)))
                     <$> initScript' peerShareScript
                     <*> initScript' peerSharingScript
                     <*> initScript' connectionScript
                   | let PeerGraph adjacency = peerGraph
                   , (addr, _, GovernorScripts {
                                 peerShareScript,
                                 peerSharingScript,
                                 connectionScript
                               }) <- adjacency
                   ]
    targetsVar <- playTimedScript (contramap TraceEnvSetTargets tracer) targets
    peerConns  <- atomically $ do
      v <- newTVar Map.empty
      traceTVar proxy
                v (\_ a -> TraceDynamic . TraceEnvPeersStatus
                       <$> snapshotPeersStatus proxy a)
      return v
    traceWith tracer (TraceEnvAddPeers peerGraph)
    traceWith tracer (TraceEnvSetLocalRoots localRootPeers)   --TODO: make dynamic
    traceWith tracer (TraceEnvSetPublicRoots publicRootPeers) --TODO: make dynamic
    return $ mockPeerSelectionActions'
               tracer env policy
               scripts targetsVar peerConns
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
                          -> PeerSelectionPolicy PeerAddr m
                          -> Map PeerAddr (TVar m PeerShareScript, TVar m PeerSharingScript, TVar m ConnectionScript)
                          -> TVar m PeerSelectionTargets
                          -> TVar m (Map PeerAddr (TVar m PeerStatus))
                          -> PeerSelectionActions PeerAddr (PeerConn m) m
mockPeerSelectionActions' tracer
                          GovernorMockEnvironment {
                            localRootPeers,
                            publicRootPeers,
                            bigLedgerPeers,
                            peerSharing
                          }
                          _
                          scripts
                          targetsVar
                          connsVar =
    PeerSelectionActions {
      readLocalRootPeers       = return (LocalRootPeers.toGroups localRootPeers),
      peerSharing              = peerSharing,
      peerConnToPeerSharing    = \(PeerConn _ ps _) -> ps,
      requestPublicRootPeers,
      requestBigLedgerPeers,
      readPeerSelectionTargets = readTVar targetsVar,
      readNewInboundConnection = retry,
      requestPeerShare,
      peerStateActions         = PeerStateActions {
          establishPeerConnection,
          monitorPeerConnection,
          activatePeerConnection,
          deactivatePeerConnection,
          closePeerConnection
        }
    }
  where
    -- TODO: make this dynamic
    requestPublicRootPeers _n = do
      traceWith tracer TraceEnvRequestPublicRootPeers
      let ttl :: DiffTime
          ttl = 60
      _ <- async $ do
        threadDelay ttl
        traceWith tracer TraceEnvPublicRootTTL
      traceWith tracer (TraceEnvRootsResult (Map.keys publicRootPeers))
      return (publicRootPeers, ttl)

    -- TODO: make this dynamic
    requestBigLedgerPeers _n = do
      traceWith tracer TraceEnvRequestBigLedgerPeers
      let ttl :: DiffTime
          ttl = 60
      _ <- async $ do
        threadDelay ttl
        traceWith tracer TraceEnvBigLedgerPeersTTL
      traceWith tracer (TraceEnvBigLedgerPeersResult bigLedgerPeers)
      return (bigLedgerPeers, ttl)

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
        remotePeerSharing <- stepScriptSTM' peerSharingScript
        return (PeerConn peeraddr (peerSharing <> remotePeerSharing) conn)
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

    monitorPeerConnection :: PeerConn m -> STM m (PeerStatus, Maybe ReconnectDelay)
    monitorPeerConnection (PeerConn _peeraddr _ conn) = (,) <$> readTVar conn
                                                              <*> pure Nothing


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
                          pickColdPeersToForget
                        } = do
    pickKnownPeersForPeerShareVar <- initScript' pickKnownPeersForPeerShare
    pickColdPeersToPromoteVar  <- initScript' pickColdPeersToPromote
    pickWarmPeersToPromoteVar  <- initScript' pickWarmPeersToPromote
    pickHotPeersToDemoteVar    <- initScript' pickHotPeersToDemote
    pickWarmPeersToDemoteVar   <- initScript' pickWarmPeersToDemote
    pickColdPeersToForgetVar   <- initScript' pickColdPeersToForget
    return PeerSelectionPolicy {
      policyPickKnownPeersForPeerShare = \_ _ _ -> interpretPickScript pickKnownPeersForPeerShareVar,
      policyPickColdPeersToPromote  = \_ _ _ -> interpretPickScript pickColdPeersToPromoteVar,
      policyPickWarmPeersToPromote  = \_ _ _ -> interpretPickScript pickWarmPeersToPromoteVar,
      policyPickHotPeersToDemote    = \_ _ _ -> interpretPickScript pickHotPeersToDemoteVar,
      policyPickWarmPeersToDemote   = \_ _ _ -> interpretPickScript pickWarmPeersToDemoteVar,
      policyPickColdPeersToForget   = \_ _ _ -> interpretPickScript pickColdPeersToForgetVar,
      policyFindPublicRootTimeout   = 5,    -- seconds
      policyMaxInProgressPeerShareReqs = 2,
      policyPeerShareRetryTime         = 3600, -- seconds
      policyPeerShareBatchWaitTime     = 3,    -- seconds
      policyPeerShareOverallTimeout    = 10,   -- seconds
      policyPeerShareActivationDelay   = 300,  -- seconds
      policyPeerShareMaxPeers          = 10,
      policyErrorDelay              = 10    -- seconds
    }


--
-- Utils for properties
--

data TestTraceEvent = GovernorDebug    !(DebugPeerSelection PeerAddr)
                    | GovernorEvent    !(TracePeerSelection PeerAddr)
                    | GovernorCounters !PeerSelectionCounters
                    | MockEnvEvent     !TraceMockEnv
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
    f a@(TracePeerShareRequests !_ !_ !_ !_)                 = GovernorEvent a
    f a@(TracePeerShareResults !_)                           = GovernorEvent a
    f a@(TracePeerShareResultsFiltered !_)                   = GovernorEvent a
    f a@(TraceKnownInboundConnection !_ !_)                  = GovernorEvent a
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

tracerDebugPeerSelection :: Tracer (IOSim s) (DebugPeerSelection PeerAddr)
tracerDebugPeerSelection = GovernorDebug `contramap` tracerTestTraceEvent

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


--
-- QuickCheck instances
--

instance Arbitrary GovernorMockEnvironment where
  arbitrary = do
      -- Dependency of the root set on the graph
      peerGraph         <- arbitrary
      let peersSet       = allPeers peerGraph
      (localRootPeers,
       publicRootPeers,
       bigLedgerPeers) <- arbitraryRootPeers peersSet

      -- But the others are independent
      targets                 <- arbitrary

      let arbitrarySubsetOfPeers = arbitrarySubset peersSet
      pickKnownPeersForPeerShare <- arbitraryPickScript arbitrarySubsetOfPeers
      pickColdPeersToPromote  <- arbitraryPickScript arbitrarySubsetOfPeers
      pickWarmPeersToPromote  <- arbitraryPickScript arbitrarySubsetOfPeers
      pickHotPeersToDemote    <- arbitraryPickScript arbitrarySubsetOfPeers
      pickWarmPeersToDemote   <- arbitraryPickScript arbitrarySubsetOfPeers
      pickColdPeersToForget   <- arbitraryPickScript arbitrarySubsetOfPeers
      peerSharing             <- arbitrary
      return GovernorMockEnvironment{..}
    where
      arbitraryRootPeers :: Set PeerAddr
                         -> Gen (LocalRootPeers PeerAddr, Map PeerAddr (PeerAdvertise, IsLedgerPeer), Set PeerAddr)
      arbitraryRootPeers peers | Set.null peers =
        return (LocalRootPeers.empty, Map.empty, Set.empty)

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

        localRoots <- arbitraryLocalRootPeers localRootsSet
        return (localRoots, publicRoots, bigLedgerPeers)

  shrink env@GovernorMockEnvironment {
           peerGraph,
           localRootPeers,
           publicRootPeers,
           bigLedgerPeers,
           targets,
           pickKnownPeersForPeerShare,
           pickColdPeersToPromote,
           pickWarmPeersToPromote,
           pickHotPeersToDemote,
           pickWarmPeersToDemote,
           pickColdPeersToForget,
           peerSharing
         } =
      -- Special rule for shrinking the peerGraph because the localRootPeers
      -- depends on it so has to be updated too.
      [ env {
          peerGraph       = peerGraph',
          localRootPeers  = LocalRootPeers.restrictKeys localRootPeers nodes',
          publicRootPeers = publicRootPeers `Map.restrictKeys` nodes',
          bigLedgerPeers  = bigLedgerPeers `Set.intersection` nodes'
        }
      | peerGraph' <- shrink peerGraph
      , let nodes' = allPeers peerGraph' ]
      -- All the others are generic.
   ++ [ GovernorMockEnvironment {
          peerGraph,
          localRootPeers          = localRootPeers',
          publicRootPeers         = publicRootPeers',
          bigLedgerPeers          = bigLedgerPeers'',
          targets                 = targets',
          pickKnownPeersForPeerShare = pickKnownPeersForPeerShare',
          pickColdPeersToPromote  = pickColdPeersToPromote',
          pickWarmPeersToPromote  = pickWarmPeersToPromote',
          pickHotPeersToDemote    = pickHotPeersToDemote',
          pickWarmPeersToDemote   = pickWarmPeersToDemote',
          pickColdPeersToForget   = pickColdPeersToForget',
          peerSharing
        }
      | (localRootPeers', publicRootPeers', bigLedgerPeers', targets',
         pickKnownPeersForPeerShare',
         pickColdPeersToPromote',
         pickWarmPeersToPromote',
         pickHotPeersToDemote',
         pickWarmPeersToDemote',
         pickColdPeersToForget')
          <- shrink (localRootPeers, publicRootPeers, bigLedgerPeers, targets,
                     pickKnownPeersForPeerShare,
                     pickColdPeersToPromote,
                     pickWarmPeersToPromote,
                     pickHotPeersToDemote,
                     pickWarmPeersToDemote,
                     pickColdPeersToForget)
      , let bigLedgerPeers'' = bigLedgerPeers'
                               `Set.intersection`
                               Map.keysSet publicRootPeers'
      ]


--
-- Tests for the QC Arbitrary instances
--

prop_arbitrary_GovernorMockEnvironment :: GovernorMockEnvironment -> Property
prop_arbitrary_GovernorMockEnvironment env =
    tabulate "num root peers"        [show (LocalRootPeers.size (localRootPeers env)
                                                     + Map.size (publicRootPeers env))] $
    tabulate "num local root peers"  [show (LocalRootPeers.size (localRootPeers env))] $
    tabulate "num public root peers" [show (Map.size (publicRootPeers env))] $
    tabulate "empty root peers" [show $ not emptyGraph && emptyRootPeers]  $
    tabulate "overlapping local/public roots" [show overlappingRootPeers]  $
    tabulate "num big ledger peers"  [show (Set.size (bigLedgerPeers env))] $

    validGovernorMockEnvironment env
  where
    emptyGraph     = null g where PeerGraph g = peerGraph env
    emptyRootPeers = LocalRootPeers.null (localRootPeers env)
                  && Map.null (publicRootPeers env)
    overlappingRootPeers =
      not $ Map.null $
        Map.restrictKeys
          (publicRootPeers env)
          (LocalRootPeers.keysSet (localRootPeers env))

prop_shrink_GovernorMockEnvironment :: ShrinkCarefully GovernorMockEnvironment -> Property
prop_shrink_GovernorMockEnvironment x =
      prop_shrink_valid validGovernorMockEnvironment x
 .&&. prop_shrink_nonequal x

prop_shrink_nonequal_GovernorMockEnvironment ::
  ShrinkCarefully GovernorMockEnvironment -> Property
prop_shrink_nonequal_GovernorMockEnvironment = prop_shrink_nonequal

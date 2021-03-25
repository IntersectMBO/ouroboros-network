{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}


{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Ouroboros.Network.PeerSelection.MockEnvironment (

    PeerGraph(..),
    GovernorMockEnvironment(..),
    GovernorMockEnvironmentWithoutAsyncDemotion(..),
    runGovernorInMockEnvironment,

    TraceMockEnv(..),
    TestTraceEvent(..),
    selectGovernorEvents,
    selectPeerSelectionTraceEvents,
    firstGossipReachablePeers,

    Script,
    ScriptDelay(..),
    TimedScript,
    scriptHead,
    singletonScript,

    tests,

  ) where

import           Data.Dynamic (fromDynamic)
import           Data.Functor (($>))
import           Data.List (nub)
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Typeable (Typeable)
import           Data.Void (Void)

import           Control.Exception (throw)
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadThrow
import qualified Control.Monad.Fail as Fail
import           Control.Tracer (Tracer (..), contramap, traceWith)

import           Control.Monad.Class.MonadTimer hiding (timeout)
import           Control.Monad.IOSim

import           Ouroboros.Network.PeerSelection.Governor hiding
                     (PeerSelectionState (..))
import qualified Ouroboros.Network.PeerSelection.LocalRootPeers as LocalRootPeers
import           Ouroboros.Network.PeerSelection.LocalRootPeers (LocalRootPeers)
import           Ouroboros.Network.PeerSelection.Types

import           Test.Ouroboros.Network.PeerSelection.Instances
import           Test.Ouroboros.Network.PeerSelection.Script
import           Test.Ouroboros.Network.PeerSelection.PeerGraph
import           Test.Ouroboros.Network.PeerSelection.LocalRootPeers
                   as LocalRootPeers hiding (tests)

import           Test.QuickCheck
import           Test.Tasty (TestTree, localOption, testGroup)
import           Test.Tasty.QuickCheck (QuickCheckMaxSize (..), testProperty)


tests :: TestTree
tests =
  testGroup "Mock environment"
    [ testProperty "arbitrary for PeerSelectionTargets"    prop_arbitrary_PeerSelectionTargets
    , testProperty "shrink for PeerSelectionTargets"       prop_shrink_PeerSelectionTargets
    , testProperty "arbitrary for PeerGraph"               prop_arbitrary_PeerGraph
    , localOption (QuickCheckMaxSize 30) $
      testProperty "shrink for PeerGraph"                  prop_shrink_PeerGraph
    , testProperty "arbitrary for GovernorMockEnvironment" prop_arbitrary_GovernorMockEnvironment
    , localOption (QuickCheckMaxSize 30) $
      testProperty "shrink for GovernorMockEnvironment"    prop_shrink_GovernorMockEnvironment
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
       peerGraph               :: PeerGraph,
       localRootPeers          :: LocalRootPeers PeerAddr,
       publicRootPeers         :: Set PeerAddr,
       targets                 :: TimedScript PeerSelectionTargets,
       pickKnownPeersForGossip :: PickScript,
       pickColdPeersToPromote  :: PickScript,
       pickWarmPeersToPromote  :: PickScript,
       pickHotPeersToDemote    :: PickScript,
       pickWarmPeersToDemote   :: PickScript,
       pickColdPeersToForget   :: PickScript
     }
  deriving Show

data PeerConn m = PeerConn !PeerAddr !(TVar m PeerStatus)

instance Show (PeerConn m) where
    show (PeerConn peeraddr _) = "PeerConn " ++ show peeraddr


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
          (addr, addrs, peerInfo { connectionScript = Script ((Noop, ShortDelay) :| []) })
    shrink (GovernorMockEnvironmentWAD env) = map GovernorMockEnvironmentWAD (shrink env)


-- | Invariant. Used to check the QC generator and shrinker.
--
-- NOTE: Local and Public Root Peers sets should be disjoint.
-- However we do not check for that invariant here. The goal
-- is to check if the actual Governor takes care of this and enforces
-- the invariant.
validGovernorMockEnvironment :: GovernorMockEnvironment -> Bool
validGovernorMockEnvironment GovernorMockEnvironment {
                               peerGraph,
                               localRootPeers,
                               publicRootPeers,
                               targets
                             } =
      validPeerGraph peerGraph
   && LocalRootPeers.keysSet localRootPeers `Set.isSubsetOf` allPeersSet
   &&                       publicRootPeers `Set.isSubsetOf` allPeersSet
   && all (sanePeerSelectionTargets . fst) targets
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
runGovernorInMockEnvironment :: GovernorMockEnvironment -> Trace Void
runGovernorInMockEnvironment mockEnv =
    runSimTrace $ do
      actions <- mockPeerSelectionActions tracerMockEnv mockEnv
      policy  <- mockPeerSelectionPolicy                mockEnv
      peerSelectionGovernor
        tracerTracePeerSelection
        tracerDebugPeerSelection
        actions
        policy

data TraceMockEnv = TraceEnvPeersStatus (Map PeerAddr PeerStatus)
  deriving Show

mockPeerSelectionActions :: (MonadAsync m, MonadTimer m, Fail.MonadFail m,
                             MonadThrow (STM m))
                         => Tracer m TraceMockEnv
                         -> GovernorMockEnvironment
                         -> m (PeerSelectionActions PeerAddr (PeerConn m) m)
mockPeerSelectionActions tracer
                         env@GovernorMockEnvironment {
                           peerGraph = PeerGraph adjacency,
                           targets
                         } = do
    scripts <- Map.fromList <$>
                       sequence [ (\a b -> (addr, (a, b)))
                                  <$> initScript gossipScript
                                  <*> initScript connectionScript
                                | (addr, _, GovernorScripts { gossipScript, connectionScript }) <- adjacency ]
    targetsVar <- playTimedScript targets
    peerConns  <- newTVarIO Map.empty
    return $ mockPeerSelectionActions'
               tracer env
               scripts targetsVar peerConns


data TransitionError
  = ActivationError
  | DeactivationError
  deriving (Show, Typeable)

instance Exception TransitionError where


mockPeerSelectionActions' :: forall m.
                             (MonadAsync m, MonadSTM m, MonadTimer m, Fail.MonadFail m,
                              MonadThrow (STM m))
                          => Tracer m TraceMockEnv
                          -> GovernorMockEnvironment
                          -> Map PeerAddr (TVar m GossipScript, TVar m ConnectionScript)
                          -> TVar m PeerSelectionTargets
                          -> TVar m (Map PeerAddr (TVar m PeerStatus))
                          -> PeerSelectionActions PeerAddr (PeerConn m) m
mockPeerSelectionActions' tracer
                          GovernorMockEnvironment {
                            localRootPeers,
                            publicRootPeers
                          }
                          scripts
                          targetsVar
                          connsVar =
    PeerSelectionActions {
      readLocalRootPeers       = return (LocalRootPeers.toGroups' localRootPeers),
      requestPublicRootPeers   = \_ -> return (publicRootPeers, 60),
      readPeerSelectionTargets = readTVar targetsVar,
      requestPeerGossip,
      peerStateActions         = PeerStateActions {
          establishPeerConnection,
          monitorPeerConnection,
          activatePeerConnection,
          deactivatePeerConnection,
          closePeerConnection
        }
    }
  where
    requestPeerGossip addr = do
      let Just (gossipScript, _) = Map.lookup addr scripts
      mgossip <- stepScript gossipScript
      case mgossip of
        Nothing                -> fail "no peers"
        Just (peeraddrs, time) -> do
          threadDelay (interpretGossipTime time)
          return peeraddrs

    establishPeerConnection :: PeerAddr -> m (PeerConn m)
    establishPeerConnection peeraddr = do
      threadDelay 1
      (conn@(PeerConn _ v), snapshot) <- atomically $ do
        conn  <- newTVar PeerWarm
        conns <- readTVar connsVar
        let !conns' = Map.insert peeraddr conn conns
        writeTVar connsVar conns'
        snapshot <- traverse readTVar conns'
        return (PeerConn peeraddr conn, snapshot)
      traceWith tracer (TraceEnvPeersStatus snapshot)
      let Just (_, connectScript) = Map.lookup peeraddr scripts
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
              done <-
                case demotion of
                  Noop   -> return True
                  ToWarm -> do
                    threadDelay (interpretScriptDelay delay)
                    atomically $ do
                      s <- readTVar v
                      case s of
                        PeerHot -> writeTVar v PeerWarm
                                $> False
                        _       -> return (PeerCold == s)
                  ToCold -> do
                    threadDelay (interpretScriptDelay delay)
                    atomically $  writeTVar v PeerCold
                               $> True

              if done
                then return ()
                else loop
        in loop
      return conn

    activatePeerConnection :: PeerConn m -> m ()
    activatePeerConnection (PeerConn _peeraddr conn) = do
      threadDelay 1
      snapshot <- atomically $ do
        status <- readTVar conn
        case status of
          PeerHot  -> error "activatePeerConnection of hot peer"
          PeerWarm -> writeTVar conn PeerHot
          --TODO: check it's just a race condition and not just wrong:
          --
          -- We throw 'ActivationError' for the following reason:
          -- 'PeerCold' can be set by the monitoring loop started by
          -- 'establishedPeerConnection' above.  However if that happens we
          -- want to signal the governor that the warm -> hot transition
          -- errored.  Otherwise 'jobPromoteWarmPeer' will try to update the
          -- state as if the transition went fine which will violate
          -- 'invariantPeerSelectionState'.
          PeerCold -> throwIO ActivationError
        conns <- readTVar connsVar
        traverse readTVar conns
      traceWith tracer (TraceEnvPeersStatus snapshot)

    deactivatePeerConnection :: PeerConn m -> m ()
    deactivatePeerConnection (PeerConn _peeraddr conn) = do
      snapshot <- atomically $ do
        status <- readTVar conn
        case status of
          PeerHot  -> writeTVar conn PeerWarm
          --TODO: check it's just a race condition and not just wrong:
          PeerWarm -> return ()
          -- See the note in 'activatePeerConnection' why we throw an exception
          -- here.
          PeerCold -> throwIO DeactivationError
        conns <- readTVar connsVar
        traverse readTVar conns
      traceWith tracer (TraceEnvPeersStatus snapshot)

    closePeerConnection :: PeerConn m -> m ()
    closePeerConnection (PeerConn peeraddr conn) = do
      snapshot <- atomically $ do
        status <- readTVar conn
        case status of
          PeerHot  -> writeTVar conn PeerCold
          --TODO: check it's just a race condition and not just wrong:
          PeerWarm -> writeTVar conn PeerCold
          PeerCold -> return ()
        conns <- readTVar connsVar
        let !conns' = Map.delete peeraddr conns
        writeTVar connsVar conns'
        traverse readTVar conns'
      traceWith tracer (TraceEnvPeersStatus snapshot)

    monitorPeerConnection :: PeerConn m -> STM m PeerStatus
    monitorPeerConnection (PeerConn _peeraddr conn) = readTVar conn


mockPeerSelectionPolicy  :: MonadSTM m
                         => GovernorMockEnvironment
                         -> m (PeerSelectionPolicy PeerAddr m)
mockPeerSelectionPolicy GovernorMockEnvironment {
                          pickKnownPeersForGossip,
                          pickColdPeersToPromote,
                          pickWarmPeersToPromote,
                          pickHotPeersToDemote,
                          pickWarmPeersToDemote,
                          pickColdPeersToForget
                        } = do
    pickKnownPeersForGossipVar <- initScript pickKnownPeersForGossip
    pickColdPeersToPromoteVar  <- initScript pickColdPeersToPromote
    pickWarmPeersToPromoteVar  <- initScript pickWarmPeersToPromote
    pickHotPeersToDemoteVar    <- initScript pickHotPeersToDemote
    pickWarmPeersToDemoteVar   <- initScript pickWarmPeersToDemote
    pickColdPeersToForgetVar   <- initScript pickColdPeersToForget
    return PeerSelectionPolicy {
      policyPickKnownPeersForGossip = interpretPickScript pickKnownPeersForGossipVar,
      policyPickColdPeersToPromote  = interpretPickScript pickColdPeersToPromoteVar,
      policyPickWarmPeersToPromote  = interpretPickScript pickWarmPeersToPromoteVar,
      policyPickHotPeersToDemote    = interpretPickScript pickHotPeersToDemoteVar,
      policyPickWarmPeersToDemote   = interpretPickScript pickWarmPeersToDemoteVar,
      policyPickColdPeersToForget   = interpretPickScript pickColdPeersToForgetVar,
      policyFindPublicRootTimeout   = 5,    -- seconds
      policyMaxInProgressGossipReqs = 2,
      policyGossipRetryTime         = 3600, -- seconds
      policyGossipBatchWaitTime     = 3,    -- seconds
      policyGossipOverallTimeout    = 10    -- seconds
    }


--
-- Utils for properties
--

data TestTraceEvent = GovernorDebug (DebugPeerSelection PeerAddr ())
                    | GovernorEvent (TracePeerSelection PeerAddr)
                    | MockEnvEvent   TraceMockEnv
  deriving Show

tracerTracePeerSelection :: Tracer (IOSim s) (TracePeerSelection PeerAddr)
tracerTracePeerSelection = contramap GovernorEvent tracerTestTraceEvent

tracerDebugPeerSelection :: Tracer (IOSim s) (DebugPeerSelection PeerAddr peerconn)
tracerDebugPeerSelection = contramap (GovernorDebug . fmap (const ()))
                                     tracerTestTraceEvent

tracerMockEnv :: Tracer (IOSim s) TraceMockEnv
tracerMockEnv = contramap MockEnvEvent tracerTestTraceEvent

tracerTestTraceEvent :: Tracer (IOSim s) TestTraceEvent
tracerTestTraceEvent = dynamicTracer

dynamicTracer :: Typeable a => Tracer (IOSim s) a
dynamicTracer = Tracer traceM

selectPeerSelectionTraceEvents :: Trace a -> [(Time, TestTraceEvent)]
selectPeerSelectionTraceEvents = go
  where
    go (Trace t _ _ (EventLog e) trace)
     | Just x <- fromDynamic e    = (t,x) : go trace
    go (Trace _ _ _ _ trace)      =         go trace
    go (TraceMainException _ e _) = throw e
    go (TraceDeadlock      _   _) = [] -- expected result in many cases
    go (TraceMainReturn    _ _ _) = []

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
      (localRootPeers,
       publicRootPeers) <- arbitraryRootPeers (allPeers peerGraph)

      -- But the others are independent
      targets                 <- arbitrary
      pickKnownPeersForGossip <- arbitrary
      pickColdPeersToPromote  <- arbitrary
      pickWarmPeersToPromote  <- arbitrary
      pickHotPeersToDemote    <- arbitrary
      pickWarmPeersToDemote   <- arbitrary
      pickColdPeersToForget   <- arbitrary
      return GovernorMockEnvironment{..}
    where
      arbitraryRootPeers :: Set PeerAddr
                         -> Gen (LocalRootPeers PeerAddr, Set PeerAddr)
      arbitraryRootPeers peers | Set.null peers =
        return (LocalRootPeers.empty, Set.empty)

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
        -- Deliberatly asking for a small intersection in order to test if
        -- the Governor actually takes care of this invariant
        let localRootsSet  = Set.fromList [ x | (x, v) <- zip rootPeers local
                                              , v <= 5 ]
            publicRootsSet = Set.fromList [ x | (x, v) <- zip rootPeers local
                                              , v >= 5 ]
        localRoots <- arbitraryLocalRootPeers localRootsSet
        return (localRoots, publicRootsSet)

  shrink env@GovernorMockEnvironment {
           peerGraph,
           localRootPeers,
           publicRootPeers,
           targets,
           pickKnownPeersForGossip,
           pickColdPeersToPromote,
           pickWarmPeersToPromote,
           pickHotPeersToDemote,
           pickWarmPeersToDemote,
           pickColdPeersToForget
         } =
      -- Special rule for shrinking the peerGraph because the localRootPeers
      -- depends on it so has to be updated too.
      [ env {
          peerGraph       = peerGraph',
          localRootPeers  = LocalRootPeers.restrictKeys localRootPeers nodes',
          publicRootPeers = publicRootPeers `Set.intersection` nodes'
        }
      | peerGraph' <- shrink peerGraph
      , let nodes' = allPeers peerGraph' ]
      -- All the others are generic.
   ++ [ GovernorMockEnvironment {
          peerGraph,
          localRootPeers          = localRootPeers',
          publicRootPeers         = publicRootPeers',
          targets                 = targets',
          pickKnownPeersForGossip = pickKnownPeersForGossip',
          pickColdPeersToPromote  = pickColdPeersToPromote',
          pickWarmPeersToPromote  = pickWarmPeersToPromote',
          pickHotPeersToDemote    = pickHotPeersToDemote',
          pickWarmPeersToDemote   = pickWarmPeersToDemote',
          pickColdPeersToForget   = pickColdPeersToForget'
        }
      | (localRootPeers', publicRootPeers', targets',
         pickKnownPeersForGossip',
         pickColdPeersToPromote',
         pickWarmPeersToPromote',
         pickHotPeersToDemote',
         pickWarmPeersToDemote',
         pickColdPeersToForget')
          <- shrink (localRootPeers, publicRootPeers, targets,
                     pickKnownPeersForGossip,
                     pickColdPeersToPromote,
                     pickWarmPeersToPromote,
                     pickHotPeersToDemote,
                     pickWarmPeersToDemote,
                     pickColdPeersToForget)
      ]


--
-- Tests for the QC Arbitrary instances
--

prop_arbitrary_GovernorMockEnvironment :: GovernorMockEnvironment -> Property
prop_arbitrary_GovernorMockEnvironment env =
    tabulate "num root peers"        [show (LocalRootPeers.size (localRootPeers env)
                                                     + Set.size (publicRootPeers env))] $
    tabulate "num local root peers"  [show (LocalRootPeers.size (localRootPeers env))] $
    tabulate "num public root peers" [show (Set.size (publicRootPeers env))] $
    tabulate "empty root peers" [show $ not emptyGraph && emptyRootPeers]  $
    tabulate "overlapping local/public roots" [show overlappingRootPeers]  $

    validGovernorMockEnvironment env
  where
    emptyGraph     = null g where PeerGraph g = peerGraph env
    emptyRootPeers = LocalRootPeers.null (localRootPeers env)
                  && Set.null (publicRootPeers env)
    overlappingRootPeers =
      not $ Set.null $
        Set.intersection
          (LocalRootPeers.keysSet (localRootPeers env))
          (publicRootPeers env)

prop_shrink_GovernorMockEnvironment :: GovernorMockEnvironment -> Bool
prop_shrink_GovernorMockEnvironment =
    all validGovernorMockEnvironment . shrink


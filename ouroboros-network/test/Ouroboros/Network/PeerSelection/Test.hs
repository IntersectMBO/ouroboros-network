{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}


{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Network.PeerSelection.Test (tests) where

import qualified Data.ByteString.Char8 as BS
import           Data.Dynamic (fromDynamic)
import           Data.Function (on)
import           Data.Functor (($>))
import           Data.Graph (Graph)
import qualified Data.Graph as Graph
import           Data.List (groupBy, nub)
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (listToMaybe)
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Tree as Tree
import           Data.Typeable (Typeable)
import           Data.Void (Void)

import           Control.Exception (Exception, throw)
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadThrow
import qualified Control.Monad.Fail as Fail
import           Control.Tracer (Tracer (..), contramap, traceWith)

import           Control.Monad.Class.MonadTimer hiding (timeout)
import           Control.Monad.IOSim

import qualified Network.DNS as DNS (defaultResolvConf)
import           Network.Socket (SockAddr)
import           Network.Mux.Timeout

import           Ouroboros.Network.PeerSelection.Governor hiding
                     (PeerSelectionState (..))
import qualified Ouroboros.Network.PeerSelection.Governor as Governor
import qualified Ouroboros.Network.PeerSelection.KnownPeers as KnownPeers
import           Ouroboros.Network.PeerSelection.RootPeersDNS
import           Ouroboros.Network.PeerSelection.Types

import           Test.QuickCheck
import           Test.Tasty (TestTree, localOption, testGroup)
import           Test.Tasty.QuickCheck (QuickCheckMaxSize (..), testProperty)


tests :: TestTree
tests =
  testGroup "Ouroboros.Network.PeerSelection"
  [ testGroup "generators"
    [ testProperty "arbitrary for PeerSelectionTargets"    prop_arbitrary_PeerSelectionTargets
    , testProperty "shrink for PeerSelectionTargets"       prop_shrink_PeerSelectionTargets
    , testProperty "arbitrary for PeerGraph"               prop_arbitrary_PeerGraph
    , localOption (QuickCheckMaxSize 30) $
      testProperty "shrink for PeerGraph"                  prop_shrink_PeerGraph
    , testProperty "arbitrary for GovernorMockEnvironment" prop_arbitrary_GovernorMockEnvironment
    , localOption (QuickCheckMaxSize 30) $
      testProperty "shrink for GovernorMockEnvironment"    prop_shrink_GovernorMockEnvironment
    ]
  , testProperty "governor no livelock"             prop_governor_nolivelock
  , testProperty "governor gossip reachable in 1hr" prop_governor_gossip_1hr
  , testProperty "governor connection status"       prop_governor_connstatus
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
       localRootPeers          :: Map PeerAddr PeerAdvertise,
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

-- | Simple address representation for the tests
--
newtype PeerAddr = PeerAddr Int
  deriving (Eq, Ord, Show)

data PeerConn m = PeerConn !PeerAddr !(TVar m PeerStatus)

instance Show (PeerConn m) where
    show (PeerConn peeraddr _) = "PeerConn " ++ show peeraddr

-- | The peer graph is the graph of all the peers in the mock p2p network, in
-- traditional adjacency representation.
--
newtype PeerGraph = PeerGraph [(PeerAddr, [PeerAddr], PeerInfo)]
  deriving (Eq, Show)

-- | For now the information associated with each node is just the gossip
-- script and connection script.
--
type PeerInfo = GovernorScripts

-- | The gossip script is the script we interpret to provide answers to gossip
-- requests that the governor makes. After each gossip request to a peer we
-- move on to the next entry in the script, unless we get to the end in which
-- case that becomes the reply for all remaining gossips.
--
-- A @Nothing@ indicates failure. The @[PeerAddr]@ is the list of peers to
-- return which must always be a subset of the actual edges in the p2p graph.
--
-- This representation was chosen because it allows easy shrinking.
--
type GossipScript = Script (Maybe ([PeerAddr], GossipTime))

-- | The gossp time is our simulation of elapsed time to respond to gossip
-- requests. This is important because the governor uses timeouts and behaves
-- differently in these three cases.
--
data GossipTime = GossipTimeQuick | GossipTimeSlow | GossipTimeTimeout
  deriving (Eq, Show)


data AsyncDemotion = ToWarm
                   | ToCold
                   | Noop
  deriving (Eq, Show)

instance Arbitrary AsyncDemotion where
    arbitrary = frequency [ (2, pure ToWarm)
                          , (2, pure ToCold)
                          , (6, pure Noop)
                          ]
    shrink ToWarm = [ToCold, Noop]
    shrink ToCold = [Noop]
    shrink Noop   = []


-- | Connection script is the script which provides asynchronous demotions
-- either to cold or warm peer.
--
type ConnectionScript = TimedScript AsyncDemotion


data GovernorScripts = GovernorScripts {
    gossipScript     :: GossipScript,
    connectionScript :: ConnectionScript
  }
  deriving (Eq, Show)

instance Arbitrary GovernorScripts where
    arbitrary = GovernorScripts
            <$> arbitrary
            <*> (fixConnectionScript <$> arbitrary)
    shrink GovernorScripts { gossipScript, connectionScript } =
      [ GovernorScripts gossipScript' connectionScript
      | gossipScript' <- shrink gossipScript
      ]
      ++
      [ GovernorScripts gossipScript (fixConnectionScript connectionScript')
      | connectionScript' <- shrink connectionScript
      ]

-- | We ensure that eventually the connection script will allow to connect to
-- a given peer.  This simplifies test conditions.
--
fixConnectionScript :: ConnectionScript -> ConnectionScript
fixConnectionScript (Script script) =
    case NonEmpty.last script of
      (Noop, _) -> Script   script
      _         -> Script $ script <> ((Noop, NoDelay) :| [])

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


-- | A pick script is used to interpret the 'policyPickKnownPeersForGossip' and
-- the 'policyPickColdPeersToForget'. It selects elements from the given
-- choices by their index (modulo the number of choices). This representation
-- was chosen because it allows easy shrinking.
--
type PickScript = Script (NonEmpty (NonNegative Int))

-- | Invariant. Used to check the QC generator and shrinker.
--
validGovernorMockEnvironment :: GovernorMockEnvironment -> Bool
validGovernorMockEnvironment GovernorMockEnvironment {
                               peerGraph,
                               localRootPeers,
                               publicRootPeers,
                               targets
                             } =
      validPeerGraph peerGraph
   && Map.keysSet localRootPeers `Set.isSubsetOf` allPeersSet
   &&            publicRootPeers `Set.isSubsetOf` allPeersSet
   && all (sanePeerSelectionTargets . fst) targets
  where
    allPeersSet = allPeers peerGraph

-- | Invariant. Used to check the QC generator and shrinker.
--
validPeerGraph :: PeerGraph -> Bool
validPeerGraph g@(PeerGraph adjacency) =
    and [ edgesSet  `Set.isSubsetOf` allpeersSet &&
          gossipSet `Set.isSubsetOf` edgesSet
        | let allpeersSet = allPeers g
        , (_, outedges, GovernorScripts { gossipScript = Script script }) <- adjacency
        , let edgesSet  = Set.fromList outedges
              gossipSet = Set.fromList
                            [ x | Just (xs, _) <- NonEmpty.toList script
                                , x <- xs ]
        ]

allPeers :: PeerGraph -> Set PeerAddr
allPeers (PeerGraph g) = Set.fromList [ addr | (addr, _, _) <- g ]


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
      readLocalRootPeers       = return localRootPeers,
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
        -- * the script returns 'Noop'
        -- * peer demoted to 'PeerCold'
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


interpretGossipTime :: GossipTime -> DiffTime
interpretGossipTime GossipTimeQuick   = 1
interpretGossipTime GossipTimeSlow    = 5
interpretGossipTime GossipTimeTimeout = 25

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

interpretPickScript :: (MonadSTMTx stm, Ord peeraddr)
                    => TVar_ stm PickScript
                    -> Map peeraddr a
                    -> Int
                    -> stm (Set peeraddr)
interpretPickScript scriptVar available pickNum
  | Map.null available
  = error "interpretPickScript: given empty map to pick from"
  | pickNum <= 0
  = error "interpretPickScript: given invalid pickNum"

  | Map.size available <= pickNum
  = return (Map.keysSet available)

  | otherwise
  = do offsets <- stepScriptSTM scriptVar
       return . pickMapKeys available
              . map getNonNegative
              . NonEmpty.take pickNum
              $ offsets

pickMapKeys :: Ord a => Map a b -> [Int] -> Set a
pickMapKeys m ns =
    Set.fromList (map pick ns)
  where
    pick n = fst (Map.elemAt i m) where i = n `mod` Map.size m


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
        [ Map.keysSet (KnownPeers.toMap (Governor.knownPeers st))
        | (_, GovernorDebug (TraceGovernorState _ _ st)) <- reverse (takeFirstNHours 1 trace) ]

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
            [ Governor.establishedStatus st
            | (_, GovernorDebug (TraceGovernorState _ _ st)) <- reverse trace ]


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

takeFirstNHours :: DiffTime -> [(Time, a)] -> [(Time, a)]
takeFirstNHours h = takeWhile (\(t,_) -> t < Time (60*60*h))

-- | The peers that are notionally reachable from the root set. It is notional
-- in the sense that it only takes account of the connectivity graph and not
-- the 'GossipScript's which determine what subset of edges the governor
-- actually sees when it tries to gossip.
--
_notionallyReachablePeers :: PeerGraph -> Set PeerAddr -> Set PeerAddr
_notionallyReachablePeers pg roots =
    Set.fromList
  . map vertexToAddr
  . concatMap Tree.flatten
  . Graph.dfs graph
  . map addrToVertex
  $ Set.toList roots
  where
    (graph, vertexToAddr, addrToVertex) = peerGraphAsGraph pg

firstGossipReachablePeers :: PeerGraph -> Set PeerAddr -> Set PeerAddr
firstGossipReachablePeers pg roots =
    Set.fromList
  . map vertexToAddr
  . concatMap Tree.flatten
  . Graph.dfs graph
  . map addrToVertex
  $ Set.toList roots
  where
    (graph, vertexToAddr, addrToVertex) = firstGossipGraph pg

peerGraphAsGraph :: PeerGraph
                 -> (Graph, Graph.Vertex -> PeerAddr, PeerAddr -> Graph.Vertex)
peerGraphAsGraph (PeerGraph adjacency) =
    simpleGraphRep $
      Graph.graphFromEdges [ ((), node, edges) | (node, edges, _) <- adjacency ]

firstGossipGraph :: PeerGraph
                 -> (Graph, Graph.Vertex -> PeerAddr, PeerAddr -> Graph.Vertex)
firstGossipGraph (PeerGraph adjacency) =
    simpleGraphRep $
      Graph.graphFromEdges
        [ ((), node, gossipScriptEdges gossipScript)
        | (node, _edges, GovernorScripts { gossipScript }) <- adjacency ]
  where
    gossipScriptEdges :: GossipScript -> [PeerAddr]
    gossipScriptEdges (Script (script :| _)) =
      case script of
        Nothing                     -> []
        Just (_, GossipTimeTimeout) -> []
        Just (edges, _)             -> edges

simpleGraphRep :: forall a n.
                  (Graph, Graph.Vertex -> (a, n, [n]), n -> Maybe Graph.Vertex)
               -> (Graph, Graph.Vertex -> n, n -> Graph.Vertex)
simpleGraphRep (graph, vertexInfo, lookupVertex) =
    (graph, vertexToAddr, addrToVertex)
  where
    vertexToAddr :: Graph.Vertex -> n
    vertexToAddr v = addr where (_,addr,_) = vertexInfo v

    addrToVertex :: n -> Graph.Vertex
    addrToVertex addr = v where Just v = lookupVertex addr


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
                         -> Gen (Map PeerAddr PeerAdvertise, Set PeerAddr)
      arbitraryRootPeers peers | Set.null peers = return (Map.empty, Set.empty)
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
        let localRoots  = [ x | (x, v) <- zip rootPeers local, v <= 5 ]
            publicRoots = [ x | (x, v) <- zip rootPeers local, v >= 5 ]
        peerinfos <- vectorOf (length localRoots) arbitrary
        let localRootsMap  = Map.fromList (zip localRoots peerinfos)
            publicRootsSet = Set.fromList publicRoots
        return (localRootsMap, publicRootsSet)

  shrink env@GovernorMockEnvironment {
           peerGraph,
           localRootPeers,
           publicRootPeers,
           targets,
           pickKnownPeersForGossip,
           pickColdPeersToForget
         } =
      -- Special rule for shrinking the peerGraph because the localRootPeers
      -- depends on it so has to be updated too.
      [ env {
          peerGraph       = peerGraph',
          localRootPeers  = Map.restrictKeys localRootPeers nodes',
          publicRootPeers = publicRootPeers `Set.intersection` nodes'
        }
      | peerGraph' <- shrink peerGraph
      , let nodes' = allPeers peerGraph' ]
      -- All the others are generic.
   ++ [ env {
          localRootPeers          = localRootPeers',
          publicRootPeers         = publicRootPeers',
          targets                 = targets',
          pickKnownPeersForGossip = pickKnownPeersForGossip',
          pickColdPeersToForget   = pickColdPeersToForget'
        }
      | (localRootPeers', publicRootPeers', targets',
         pickKnownPeersForGossip',
         pickColdPeersToForget')
          <- shrink (localRootPeers, publicRootPeers, targets,
                     pickKnownPeersForGossip,
                     pickColdPeersToForget)
      ]

instance Arbitrary PeerGraph where
  arbitrary = sized $ \sz -> do
      numNodes <- choose (0, sz)
      numEdges <- choose (numNodes, numNodes * numNodes `div` 2)
      edges <- vectorOf numEdges $
                 (,) <$> choose (0, numNodes-1)
                     <*> choose (0, numNodes-1)
      let adjacency = Map.fromListWith (<>)
                        [ (from, Set.singleton (PeerAddr to))
                        | (from, to) <- edges ]
      graph <- sequence [ do gossipScript <- arbitraryGossipScript outedges
                             connectionScript <- fixConnectionScript <$> arbitrary
                             let node = GovernorScripts { gossipScript, connectionScript }
                             return (PeerAddr n, outedges, node)
                        | n <- [0..numNodes-1]
                        , let outedges = maybe [] Set.toList
                                               (Map.lookup n adjacency) ]
      return (PeerGraph graph)

  shrink (PeerGraph graph) =
      [ PeerGraph (prunePeerGraphEdges graph')
      | graph' <- shrinkList shrinkNode graph ]
    where
      shrinkNode (nodeaddr, edges, script) =
          -- shrink edges before gossip script, and addr does not shrink
          [ (nodeaddr, edges', script)
          | edges' <- shrinkList shrinkNothing edges ]
       ++ [ (nodeaddr, edges, script')
          | script' <- shrink script ]

arbitraryGossipScript :: [PeerAddr] -> Gen GossipScript
arbitraryGossipScript peers =
    arbitraryShortScriptOf gossipResult
  where
    gossipResult :: Gen (Maybe ([PeerAddr], GossipTime))
    gossipResult =
      frequency [ (1, pure Nothing)
                , (4, Just <$> ((,) <$> selectHalfRandomly peers
                                    <*> arbitrary)) ]

    selectHalfRandomly :: [a] -> Gen [a]
    selectHalfRandomly xs = do
        picked <- vectorOf (length xs) arbitrary
        return [ x | (x, True) <- zip xs picked ]

-- | Remove dangling graph edges and gossip results.
--
prunePeerGraphEdges :: [(PeerAddr, [PeerAddr], PeerInfo)]
                    -> [(PeerAddr, [PeerAddr], PeerInfo)]
prunePeerGraphEdges graph =
    [ (nodeaddr, edges', node)
    | let nodes   = Set.fromList [ nodeaddr | (nodeaddr, _, _) <- graph ]
    , (nodeaddr, edges, GovernorScripts { gossipScript = Script gossip, connectionScript }) <- graph
    , let edges'  = pruneEdgeList nodes edges
          gossip' = pruneGossipScript (Set.fromList edges') gossip
          node    = GovernorScripts {
                        gossipScript = Script gossip',
                        connectionScript
                      }
    ]
  where
    pruneEdgeList :: Set PeerAddr -> [PeerAddr] -> [PeerAddr]
    pruneEdgeList nodes = filter (`Set.member` nodes)

    pruneGossipScript :: Set PeerAddr
                      -> NonEmpty (Maybe ([PeerAddr], GossipTime))
                      -> NonEmpty (Maybe ([PeerAddr], GossipTime))
    pruneGossipScript nodes =
      NonEmpty.map (fmap (\(es, t) -> (pruneEdgeList nodes es, t)))

-- Cheeky instance to make shrinking of other structures easier
instance Arbitrary PeerAddr where
  arbitrary = error "arbitrary: PeerAddr"
  shrink _  = []

instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = NonEmpty.fromList <$> listOf1 arbitrary

  shrink = shrinkMap from to
    where
      to :: NonEmpty a -> NonEmptyList a
      to xs = NonEmpty (NonEmpty.toList xs)

      from :: NonEmptyList a -> NonEmpty a
      from (NonEmpty xs) = NonEmpty.fromList xs

instance Arbitrary GossipTime where
  arbitrary = frequency [ (2, pure GossipTimeQuick)
                        , (2, pure GossipTimeSlow)
                        , (1, pure GossipTimeTimeout) ]

  shrink GossipTimeTimeout = [GossipTimeQuick, GossipTimeSlow]
  shrink GossipTimeSlow    = [GossipTimeQuick]
  shrink GossipTimeQuick   = []

instance Arbitrary PeerAdvertise where
  arbitrary = elements [ DoAdvertisePeer, DoNotAdvertisePeer ]

  shrink DoAdvertisePeer    = []
  shrink DoNotAdvertisePeer = [DoAdvertisePeer]

instance Arbitrary PeerSelectionTargets where
  arbitrary = do
    targetNumberOfKnownPeers       <-            min 10000 . getNonNegative <$> arbitrary
    targetNumberOfRootPeers        <- choose (0, min 100  targetNumberOfKnownPeers)
    targetNumberOfEstablishedPeers <- choose (0, min 1000 targetNumberOfKnownPeers)
    targetNumberOfActivePeers      <- choose (0, min 100  targetNumberOfEstablishedPeers)
    return PeerSelectionTargets {
      targetNumberOfRootPeers,
      targetNumberOfKnownPeers,
      targetNumberOfEstablishedPeers,
      targetNumberOfActivePeers
    }

  shrink (PeerSelectionTargets r k e a) =
    [ targets'
    | (r',k',e',a') <- shrink (r,k,e,a)
    , let targets' = PeerSelectionTargets r' k' e' a'
    , sanePeerSelectionTargets targets' ]


--
-- Test script abstraction
--

newtype Script a = Script (NonEmpty a)
  deriving (Eq, Show, Functor, Foldable, Traversable)
  deriving Arbitrary via NonEmpty a

singletonScript :: a -> Script a
singletonScript x = (Script (x :| []))

scriptHead :: Script a -> a
scriptHead (Script (x :| _)) = x

arbitraryShortScriptOf :: Gen a -> Gen (Script a)
arbitraryShortScriptOf a =
    sized $ \sz ->
      (Script . NonEmpty.fromList) <$> vectorOf (min 5 (sz+1)) a

initScript :: MonadSTM m => Script a -> m (TVar m (Script a))
initScript = newTVarIO

stepScript :: MonadSTM m => TVar m (Script a) -> m a
stepScript scriptVar = atomically (stepScriptSTM scriptVar)

stepScriptSTM :: MonadSTMTx stm => TVar_ stm (Script a) -> stm a
stepScriptSTM scriptVar = do
    Script (x :| xs) <- readTVar scriptVar
    case xs of
      []     -> return ()
      x':xs' -> writeTVar scriptVar (Script (x' :| xs'))
    return x

type TimedScript a = Script (a, ScriptDelay)

data ScriptDelay = NoDelay | ShortDelay | LongDelay
  deriving (Eq, Show)

instance Arbitrary ScriptDelay where
  arbitrary = frequency [ (1, pure NoDelay)
                        , (1, pure ShortDelay)
                        , (4, pure LongDelay) ]

  shrink LongDelay  = [NoDelay, ShortDelay]
  shrink ShortDelay = [NoDelay]
  shrink NoDelay    = []

playTimedScript :: (MonadAsync m, MonadTimer m)
                => TimedScript a -> m (TVar m a)
playTimedScript (Script ((x0,d0) :| script)) = do
    v <- newTVarIO x0
    _ <- async $ do
           threadDelay (interpretScriptDelay d0)
           sequence_ [ do atomically (writeTVar v x)
                          threadDelay (interpretScriptDelay d)
                     | (x,d) <- script ]
    return v
  where
    interpretScriptDelay NoDelay    = 0
    interpretScriptDelay ShortDelay = 1
    interpretScriptDelay LongDelay  = 3600


--
-- Tests for the QC Arbitrary instances
--

prop_arbitrary_PeerGraph :: PeerGraph -> Property
prop_arbitrary_PeerGraph pg =
    -- We are interested in the distribution of the graph size (in nodes)
    -- and the number of separate components so that we can see that we
    -- get some coverage of graphs that are not fully connected.
    tabulate  "graph size"       [graphSize] $
    tabulate  "graph components" [graphComponents] $
    validPeerGraph pg
  where
    graphSize       = renderGraphSize (length g) where PeerGraph g = pg
    graphComponents = renderNumComponents
                        (peerGraphNumStronglyConnectedComponents pg)

    renderGraphSize n
      | n == 0    = "0"
      | n <= 9    = "1 -- 9"
      | otherwise = renderRanges 10 n

    renderNumComponents n
      | n <= 4    = show n
      | otherwise = renderRanges 5 n

peerGraphNumStronglyConnectedComponents :: PeerGraph -> Int
peerGraphNumStronglyConnectedComponents pg =
    length (Graph.scc g)
  where
    (g,_,_) = peerGraphAsGraph pg

prop_shrink_PeerGraph :: PeerGraph -> Bool
prop_shrink_PeerGraph =
    all validPeerGraph . shrink

prop_arbitrary_PeerSelectionTargets :: PeerSelectionTargets -> Bool
prop_arbitrary_PeerSelectionTargets =
    sanePeerSelectionTargets

prop_shrink_PeerSelectionTargets :: PeerSelectionTargets -> Bool
prop_shrink_PeerSelectionTargets =
    all sanePeerSelectionTargets . shrink

prop_arbitrary_GovernorMockEnvironment :: GovernorMockEnvironment -> Property
prop_arbitrary_GovernorMockEnvironment env =
    tabulate "num root peers"        [show (Map.size (localRootPeers env)
                                          + Set.size (publicRootPeers env))] $
    tabulate "num local root peers"  [show (Map.size (localRootPeers env))] $
    tabulate "num public root peers" [show (Set.size (publicRootPeers env))] $
    tabulate "empty root peers" [show $ not emptyGraph && emptyRootPeers]  $
    tabulate "overlapping local/public roots" [show overlappingRootPeers]  $

    validGovernorMockEnvironment env
  where
    emptyGraph     = null g where PeerGraph g = peerGraph env
    emptyRootPeers = Map.null (localRootPeers env)
                  && Set.null (publicRootPeers env)
    overlappingRootPeers =
      not $ Set.null $ Set.intersection (Map.keysSet (localRootPeers env))
                                        (publicRootPeers env)

prop_shrink_GovernorMockEnvironment :: GovernorMockEnvironment -> Bool
prop_shrink_GovernorMockEnvironment =
    all validGovernorMockEnvironment . shrink

renderRanges :: Int -> Int -> String
renderRanges r n = show lower ++ " -- " ++ show upper
  where
    lower = n - n `mod` r
    upper = lower + (r-1)


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
    pickTrivially :: Applicative m => Map SockAddr a -> Int -> m (Set SockAddr)
    pickTrivially m n = pure . Set.take n . Map.keysSet $ m

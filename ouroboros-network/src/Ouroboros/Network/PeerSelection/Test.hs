{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Network.PeerSelection.Test where

import           Data.Void (Void)
import           Data.Typeable (Typeable)
import           Data.Dynamic (fromDynamic)
import           Data.Maybe (listToMaybe)
import           Data.List (nub)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import qualified Data.Set as Set
import           Data.Set (Set)
import qualified Data.Graph as Graph
import           Data.Graph (Graph)
import qualified Data.Tree as Tree

import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadTime
import           Control.Tracer (Tracer(..))
import           Control.Exception (throw)

import           Control.Monad.IOSim
import           Control.Monad.Class.MonadTimer

import           Ouroboros.Network.PeerSelection.Types
import           Ouroboros.Network.PeerSelection.Governor hiding (PeerSelectionState(..))
import qualified Ouroboros.Network.PeerSelection.Governor as Governor
import qualified Ouroboros.Network.PeerSelection.KnownPeers as KnownPeers

import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup, localOption)
import           Test.Tasty.QuickCheck (testProperty, QuickCheckMaxSize(..))


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
  , testProperty "governor sanity"             prop_governor_sanity
  , testProperty "governor reachable in 1hr"   prop_governor_reachable_1hr
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
       rootPeers               :: RootPeers PeerAddr,
       targets                 :: PeerSelectionTargets,
       pickKnownPeersForGossip :: PickScript,
       pickColdPeersToForget   :: PickScript
     }
  deriving Show

-- | Simple address representation for the tests
--
newtype PeerAddr = PeerAddr Int
  deriving (Eq, Ord, Show)

-- | The peer graph is the graph of all the peers in the mock p2p network, in
-- traditional adjacency representation.
--
newtype PeerGraph = PeerGraph [(PeerInfo, PeerAddr, [PeerAddr])]
  deriving (Eq, Show)

-- | For now the information associated with each node is just the gossip
-- script.
--
type PeerInfo = GossipScript

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
newtype GossipScript = GossipScript (NonEmpty (Maybe ([PeerAddr], GossipTime)))
  deriving (Eq, Show)

-- | The gossp time is our simulation of elapsed time to respond to gossip
-- requests. This is important because the governor uses timeouts and behaves
-- differently in these three cases.
--
data GossipTime = GossipTimeQuick | GossipTimeSlow | GossipTimeTimeout
  deriving (Eq, Show)

-- | A pick script is used to interpret the 'policyPickKnownPeersForGossip' and
-- the 'policyPickColdPeersToForget'. It selects elements from the given
-- choices by their index (modulo the number of choices). This representation
-- was chosen because it allows easy shrinking.
--
newtype PickScript = PickScript (NonEmpty (NonEmpty (NonNegative Int)))
  deriving (Eq, Show)

-- | Invariant. Used to check the QC generator and shrinker.
--
validGovernorMockEnvironment :: GovernorMockEnvironment -> Bool
validGovernorMockEnvironment GovernorMockEnvironment {
                               peerGraph,
                               rootPeers,
                               targets
                             } =
      validPeerGraph peerGraph
   && validRootPeers (allPeers peerGraph) rootPeers
   && sanePeerSelectionTargets targets
  where
    validRootPeers :: Set PeerAddr -> Map PeerAddr a -> Bool
    validRootPeers allpeers rootpeers =
        Map.keysSet rootpeers `Set.isSubsetOf` allpeers

-- | Invariant. Used to check the QC generator and shrinker.
--
validPeerGraph :: PeerGraph -> Bool
validPeerGraph g@(PeerGraph adjacency) =
    and [ edgesSet  `Set.isSubsetOf` allpeersSet &&
          gossipSet `Set.isSubsetOf` edgesSet
        | let allpeersSet = allPeers g
        , (GossipScript script, _, outedges) <- adjacency
        , let edgesSet  = Set.fromList outedges
              gossipSet = Set.fromList
                            [ x | Just (xs, _) <- NonEmpty.toList script
                                , x <- xs ]
        ]

allPeers :: PeerGraph -> Set PeerAddr
allPeers (PeerGraph g) = Set.fromList [ addr | (_, addr, _) <- g ]


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
      actions <- mockPeerSelectionActions mockEnv
      policy  <- mockPeerSelectionPolicy  mockEnv
      peerSelectionGovernor
        dynamicTracer
        actions
        policy

mockPeerSelectionActions :: (MonadSTM m, MonadTimer m)
                         => GovernorMockEnvironment
                         -> m (PeerSelectionActions PeerAddr m)
mockPeerSelectionActions GovernorMockEnvironment {
                           peerGraph = PeerGraph adjacency,
                           rootPeers,
                           targets
                         } = do
    scriptVars <-
      Map.fromList <$>
      sequence [ (,) addr <$> newTVarM script
               | (script, addr, _) <- adjacency ]
    let requestPeerGossip addr =
            stepGossipScript scriptVar
          where
            Just scriptVar = Map.lookup addr scriptVars
    return PeerSelectionActions {
      readRootPeers            = return rootPeers,
      readPeerSelectionTargets = return targets,
      requestPeerGossip
    }
  where
    stepGossipScript scriptVar = do
      mgossip <- atomically $ do
        GossipScript (mgossip :| script') <- readTVar scriptVar
        case script' of
          []   -> return ()
          x:xs -> writeTVar scriptVar (GossipScript (x :| xs))
        return mgossip
      case mgossip of
        Nothing        -> fail "no peers"
        Just (peeraddrs, time) -> do
          threadDelay (interpretGossipTime time)
          return peeraddrs

interpretGossipTime :: GossipTime -> DiffTime
interpretGossipTime GossipTimeQuick   = 1
interpretGossipTime GossipTimeSlow    = 5
interpretGossipTime GossipTimeTimeout = 25

mockPeerSelectionPolicy  :: MonadSTM m
                         => GovernorMockEnvironment
                         -> m (PeerSelectionPolicy PeerAddr m)
mockPeerSelectionPolicy GovernorMockEnvironment {
                          pickKnownPeersForGossip,
                          pickColdPeersToForget
                        } = do
    pickKnownPeersForGossipVar <- newTVarM pickKnownPeersForGossip
    pickColdPeersToForgetVar   <- newTVarM pickColdPeersToForget
    return PeerSelectionPolicy {
      policyPickKnownPeersForGossip = interpretPickScript pickKnownPeersForGossipVar,
      policyPickColdPeersToForget   = interpretPickScript pickColdPeersToForgetVar,
      policyMaxInProgressGossipReqs = 2,
      policyGossipRetryTime         = 3600, -- seconds
      policyGossipBatchWaitTime     = 3,    -- seconds
      policyGossipOverallTimeout    = 10    -- seconds
    }

interpretPickScript :: (MonadSTM m, Ord peeraddr)
                    => TVar m PickScript
                    -> Map peeraddr a
                    -> Int
                    -> STM m (NonEmpty peeraddr)
interpretPickScript scriptVar available pickNum
  | Map.null available
  = error "interpretPickScript: given empty map to pick from"
  | pickNum <= 0
  = error "interpretPickScript: given invalid pickNum"

  | Map.size available <= pickNum
  = return (NonEmpty.fromList (Map.keys available))

  | otherwise
  = do PickScript (offsets :| script') <- readTVar scriptVar
       case script' of
         []   -> return ()
         x:xs -> writeTVar scriptVar (PickScript (x :| xs))
       return . pickMapKeys available
              . NonEmpty.map getNonNegative
              . NonEmpty.fromList -- safe because pickNum > 0
              . NonEmpty.take pickNum
              $ offsets

pickMapKeys :: Ord a => Map a b -> NonEmpty Int -> NonEmpty a
pickMapKeys m ns =
    NonEmpty.nub (NonEmpty.map pick ns)
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


-- | Run the governor for up to 24 hours (simulated obviously) and see if it
-- throws any exceptions. This uses static targets and root peers.
--
-- This tells us that even for insane environments there are no invariant
-- violations.
--
prop_governor_sanity :: GovernorMockEnvironment -> Property
prop_governor_sanity env =
    let trace = selectPeerSelectionTraceEvents $
                  runGovernorInMockEnvironment env
     in      property (noFailures trace)
        .&&. if targetNumberOfKnownPeers (targets env) > 0
               then hasOutput trace
               else property True
  where
    hasOutput :: [(Time, TracePeerSelection PeerAddr)] -> Property
    hasOutput (_:_) = property True
    hasOutput []    = counterexample "no trace output" $
                      property False

    -- Just evaluate to force any exception
    noFailures :: [(Time, TracePeerSelection PeerAddr)] -> Bool
    noFailures = foldl const True . takeFirstNHours 24


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
prop_governor_reachable_1hr :: GovernorMockEnvironment -> Property
prop_governor_reachable_1hr env@GovernorMockEnvironment{
                              peerGraph,
                              rootPeers,
                              targets
                            } =
    let trace      = selectPeerSelectionTraceEvents $
                       runGovernorInMockEnvironment env
        Just found = knownPeersAfter1Hour trace
        reachable  = firstGossipReachablePeers peerGraph rootPeers
     in subsetProperty    found reachable
   .&&. bigEnoughProperty found reachable
  where
    knownPeersAfter1Hour trace =
      listToMaybe
        [ Map.keysSet (KnownPeers.toMap (Governor.knownPeers st))
        | (_, TraceGovernorLoopDebug st _) <- reverse (takeFirstNHours 1 trace) ]

    -- The ones we find should be a subset of the ones possible to find
    subsetProperty found reachable =
      counterexample ("reachable: " ++ show reachable ++ "\n" ++
                      "found:     " ++ show found) $
      property (found `Set.isSubsetOf` reachable)

    -- We expect to find enough of them, either the target number or the
    -- maximum reachable
    bigEnoughProperty found reachable =
      counterexample ("reachable : " ++ show reachable ++ "\n" ++
                      "found     : " ++ show found ++ "\n" ++
                      "found #   : " ++ show (Set.size found) ++ "\n" ++
                      "expected #: " ++ show expected) $
      property (Set.size found == expected)
      where
        expected = Set.size reachable `min` targetNumberOfKnownPeers targets


--
-- Utils for properties
--

dynamicTracer :: Typeable a => Tracer (SimM s) a
dynamicTracer = Tracer traceM

selectPeerSelectionTraceEvents :: Trace a -> [(Time, TracePeerSelection PeerAddr)]
selectPeerSelectionTraceEvents = go
  where
    go (Trace t _ _ (EventLog e) trace)
     | Just x <- fromDynamic e    = (t,x) : go trace
    go (Trace _ _ _ _ trace)      =         go trace
    go (TraceMainException _ e _) = throw e
    go (TraceDeadlock      _   _) = [] -- expected result in many cases
    go (TraceMainReturn    _ _ _) = []

takeFirstNHours :: DiffTime
                -> [(Time, TracePeerSelection PeerAddr)]
                -> [(Time, TracePeerSelection PeerAddr)]
takeFirstNHours h = takeWhile (\(t,_) -> t < Time (60*60*h))

-- | The peers that are notionally reachable from the root set. It is notional
-- in the sense that it only takes account of the connectivity graph and not
-- the 'GossipScript's which determine what subset of edges the governor
-- actually sees when it tries to gossip.
--
notionallyReachablePeers :: PeerGraph -> RootPeers PeerAddr -> Set PeerAddr
notionallyReachablePeers pg roots =
    Set.fromList
  . map vertexToAddr
  . concatMap Tree.flatten 
  . Graph.dfs graph
  . map addrToVertex
  $ Map.keys roots
  where
    (graph, vertexToAddr, addrToVertex) = peerGraphAsGraph pg

firstGossipReachablePeers :: PeerGraph -> RootPeers PeerAddr -> Set PeerAddr
firstGossipReachablePeers pg roots =
    Set.fromList
  . map vertexToAddr
  . concatMap Tree.flatten 
  . Graph.dfs graph
  . map addrToVertex
  $ Map.keys roots
  where
    (graph, vertexToAddr, addrToVertex) = firstGossipGraph pg

peerGraphAsGraph :: PeerGraph
                 -> (Graph, Graph.Vertex -> PeerAddr, PeerAddr -> Graph.Vertex)
peerGraphAsGraph (PeerGraph adjacency) =
    simpleGraphRep $
      Graph.graphFromEdges adjacency

firstGossipGraph :: PeerGraph
                 -> (Graph, Graph.Vertex -> PeerAddr, PeerAddr -> Graph.Vertex)
firstGossipGraph (PeerGraph adjacency) =
    simpleGraphRep $
      Graph.graphFromEdges
        [ ((), node, gossipScriptEdges gossip)
        | (gossip, node, _edges) <- adjacency ]
  where
    gossipScriptEdges :: GossipScript -> [PeerAddr]
    gossipScriptEdges (GossipScript (script :| _)) =
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
      peerGraph <- arbitrary
      rootPeers <- arbitraryRootPeers (allPeers peerGraph)

      -- But the others are independent
      targets                 <- arbitrary
      pickKnownPeersForGossip <- arbitrary
      pickColdPeersToForget   <- arbitrary
      return GovernorMockEnvironment{..}
    where
      arbitraryRootPeers :: Set PeerAddr -> Gen (RootPeers PeerAddr)
      arbitraryRootPeers peers | Set.null peers = return Map.empty
      arbitraryRootPeers peers = do
        -- We decide how many we want and then pick randomly.
        numroots  <- choose (1, ceiling . sqrt . (fromIntegral :: Int -> Double)
                                        . length $ peers)
        ixs       <- vectorOf numroots (getNonNegative <$> arbitrary)
        let pick n    = Set.elemAt i peers where i = n `mod` Set.size peers
            rootPeers = nub (map pick ixs)
        peerinfos <- vectorOf (length rootPeers) arbitrary
        return $ Map.fromList (zip rootPeers peerinfos)

  shrink env@GovernorMockEnvironment {
           peerGraph,
           rootPeers,
           targets,
           pickKnownPeersForGossip,
           pickColdPeersToForget
         } =
      -- Special rule for shrinking the peerGraph because the rootPeers
      -- depends on it so has to be updated too.
      [ env {
          peerGraph = peerGraph',
          rootPeers = Map.restrictKeys rootPeers (allPeers peerGraph')
        }
      | peerGraph' <- shrink peerGraph ]
      -- All the others are generic.
   ++ [ env {
          rootPeers               = rootPeers',
          targets                 = targets',
          pickKnownPeersForGossip = pickKnownPeersForGossip',
          pickColdPeersToForget   = pickColdPeersToForget'
        }
      | (rootPeers', targets',
         pickKnownPeersForGossip',
         pickColdPeersToForget')
          <- shrink (rootPeers, targets,
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
      graph <- sequence [ do node <- arbitraryGossipScript outedges
                             return (node, PeerAddr n, outedges)
                        | n <- [0..numNodes-1]
                        , let outedges = maybe [] Set.toList
                                               (Map.lookup n adjacency) ]
      return (PeerGraph graph)

  shrink (PeerGraph graph) =
      [ PeerGraph (prunePeerGraphEdges graph')
      | graph' <- shrinkList shrinkNode graph ]
    where
      shrinkNode (GossipScript script, nodeaddr, edges) =
          -- shrink edges before gossip script, and addr does not shrink
          [ (GossipScript script, nodeaddr, edges')
          | edges' <- shrinkList shrinkNothing edges ]
       ++ [ (GossipScript script', nodeaddr, edges)
          | script' <- shrink script ]

arbitraryGossipScript :: [PeerAddr] -> Gen GossipScript
arbitraryGossipScript peers =
    sized $ \sz ->
      (GossipScript . NonEmpty.fromList) <$>
        vectorOf (min 5 (sz+1)) gossipResult
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
prunePeerGraphEdges :: [(PeerInfo, PeerAddr, [PeerAddr])]
                    -> [(PeerInfo, PeerAddr, [PeerAddr])]
prunePeerGraphEdges graph =
    [ (GossipScript script', nodeaddr, edges')
    | let nodes   = Set.fromList [ nodeaddr | (_, nodeaddr, _) <- graph ]
    , (GossipScript script, nodeaddr, edges) <- graph
    , let edges'  = pruneEdgeList nodes edges
          script' = pruneGossipScript (Set.fromList edges') script
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

instance Arbitrary PickScript where
  arbitrary = PickScript <$> arbitrary

  shrink (PickScript xs) = map PickScript (shrink xs)

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

instance Arbitrary RootPeerInfo where
  arbitrary = RootPeerInfo <$> arbitrary
  shrink    = genericShrink

instance Arbitrary PeerSelectionTargets where
  arbitrary = do
    targetNumberOfKnownPeers       <-            min 10000 . getNonNegative <$> arbitrary
    targetNumberOfEstablishedPeers <- choose (0, min 1000 targetNumberOfKnownPeers)
    targetNumberOfActivePeers      <- choose (0, min 100  targetNumberOfEstablishedPeers)
    return PeerSelectionTargets {
      targetNumberOfKnownPeers,
      targetNumberOfEstablishedPeers,
      targetNumberOfActivePeers
    }

  shrink (PeerSelectionTargets k e a) =
    [ targets'
    | (k',e',a') <- shrink (k,e,a)
    , let targets' = PeerSelectionTargets k' e' a'
    , sanePeerSelectionTargets targets' ]


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
    classify (not emptyGraph && emptyRootPeers) "empty root peers" $
    tabulate "num root peers" [show (Map.size (rootPeers env))] $
    validGovernorMockEnvironment env
  where
    emptyGraph     = null g where PeerGraph g = peerGraph env
    emptyRootPeers = Map.null (rootPeers env)

prop_shrink_GovernorMockEnvironment :: GovernorMockEnvironment -> Bool
prop_shrink_GovernorMockEnvironment =
    all validGovernorMockEnvironment . shrink

renderRanges :: Int -> Int -> String
renderRanges r n = show lower ++ " -- " ++ show upper
  where
    lower = n - n `mod` r
    upper = lower + (r-1)


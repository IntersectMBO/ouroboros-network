{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Network.PeerSelection.Examples where

import           Data.Void (Void)
import           Data.Typeable (Typeable)
import           Data.Dynamic (fromDynamic)
import           Data.List (nub)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import qualified Data.Set as Set
import           Data.Set (Set)
import qualified Data.Graph as Graph
import           Data.Graph (Graph)

import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadTime
import           Control.Tracer (Tracer(..))
import           Control.Exception (throw)

import           Control.Monad.IOSim
import           Control.Monad.Class.MonadTimer

import           Ouroboros.Network.PeerSelection.Types
import           Ouroboros.Network.PeerSelection.Governor

import           Test.QuickCheck


-- Things we might like to test...
--
-- * for even insane environments, there are no invariant violations or insane behaviour
-- * for vaguely stable envs, we do stablise at our target number of cold peers
-- * we stabilise without going insane even if the available nodes are fewer than the target
-- * time to stabilise after a change is not crazy
-- * time to find new nodes after a graph change is ok

--TODO: this doesn't make the targets or root peer set dynamic.

--
-- Mock environment types
--

data GovernorMockEnvironment = GovernorMockEnvironment {
       peerGraph               :: PeerGraph,
       rootPeerSet             :: Map MockPeerAddr RootPeerInfo,
       targets                 :: PeerSelectionTargets,
       pickKnownPeersForGossip :: PickScript,
       pickColdPeersToForget   :: PickScript
     }
  deriving Show

newtype MockPeerAddr = MockPeerAddr Int
  deriving (Eq, Ord, Show)

newtype PeerGraph = PeerGraph [(GossipScript, MockPeerAddr, [MockPeerAddr])]
  deriving Show

data GossipScript = GossipScript
                      (Maybe [MockPeerAddr])
                      GossipTime
                      (Maybe GossipScript)
  deriving Show

data GossipTime = GossipTimeQuick | GossipTimeSlow | GossipTimeTimeout
  deriving Show

newtype PickScript = PickScript (NonEmpty (NonEmpty (NonNegative Int)))
  deriving Show

validGovernorMockEnvironment :: GovernorMockEnvironment -> Bool
validGovernorMockEnvironment GovernorMockEnvironment {
                               peerGraph,
                               rootPeerSet,
                               targets
                             } =
      validPeerGraph peerGraph
   && validRootPeerSet (Set.fromList (allPeers peerGraph)) rootPeerSet
   && sanePeerSelectionTargets targets

validPeerGraph :: PeerGraph -> Bool
validPeerGraph g@(PeerGraph adjacency) =
    and [ all (`Set.member` allpeersset) outedges
        | let allpeersset = Set.fromList (allPeers g)
        , (_, _, outedges) <- adjacency ]

validRootPeerSet :: Set MockPeerAddr -> Map MockPeerAddr a -> Bool
validRootPeerSet allpeers rootpeers =
    Map.keysSet rootpeers `Set.isSubsetOf` allpeers

allPeers :: PeerGraph -> [MockPeerAddr]
allPeers (PeerGraph g) = [ addr | (_, addr, _) <- g ]

peerGraphAsGraph :: PeerGraph
                 -> ( Graph
                    , Graph.Vertex -> (GossipScript,
                                       MockPeerAddr,
                                       [MockPeerAddr])
                    , MockPeerAddr -> Maybe Graph.Vertex
                    )
peerGraphAsGraph (PeerGraph adjacency) = Graph.graphFromEdges adjacency


--
-- Execution in the mock environment
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

dynamicTracer :: Typeable a => Tracer (SimM s) a
dynamicTracer = Tracer traceM


mockPeerSelectionActions :: (MonadSTM m, MonadTimer m)
                         => GovernorMockEnvironment
                         -> m (PeerSelectionActions MockPeerAddr m)
mockPeerSelectionActions GovernorMockEnvironment {
                           peerGraph = PeerGraph adjacency,
                           rootPeerSet,
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
      readRootPeers            = return rootPeerSet,
      readPeerSelectionTargets = return targets,
      requestPeerGossip
    }
  where
    stepGossipScript scriptVar = do
      (res, time) <- atomically $ do
        GossipScript res time mscript' <- readTVar scriptVar
        case mscript' of
          Nothing      -> return ()
          Just script' -> writeTVar scriptVar script'
        return (res, time)
      threadDelay (interpretGossipTime time)
      case res of
        Nothing        -> fail "no peers"
        Just peeraddrs -> return peeraddrs

interpretGossipTime :: GossipTime -> DiffTime
interpretGossipTime GossipTimeQuick   = 1
interpretGossipTime GossipTimeSlow    = 5
interpretGossipTime GossipTimeTimeout = 25

mockPeerSelectionPolicy  :: MonadSTM m
                         => GovernorMockEnvironment
                         -> m (PeerSelectionPolicy MockPeerAddr m)
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
-- Main properties, using mock environment
--

-- | Just run the governor and see if it throws any exceptions
--
prop_governor_basic :: GovernorMockEnvironment -> Property
prop_governor_basic env =
    let trace = runGovernorInMockEnvironment env
     in      eventuallyDeadlocks trace
        .&&. if targetNumberOfKnownPeers (targets env) > 0
               then hasOutput trace
               else property True
  where
    hasOutput :: Trace a -> Property
    hasOutput (Trace _ _ _ (EventLog e) _)
      | Just _ <- fromDynamic e :: Maybe (TracePeerSelection MockPeerAddr)
      = property True

    hasOutput (Trace _ _ _ _ t) = hasOutput t
    hasOutput  _                = counterexample "no trace output" $
                                  property False

    eventuallyDeadlocks :: Trace a -> Property
    eventuallyDeadlocks  TraceDeadlock{}           = property True
    eventuallyDeadlocks (Trace time _ _ _ _)
      | time >= Time (60*60*24)                    = property True
    eventuallyDeadlocks (Trace _ _ _ _ t)          = eventuallyDeadlocks t
    eventuallyDeadlocks (TraceMainException _ e _) = throw e
    eventuallyDeadlocks  TraceMainReturn{}         = property False


selectPeerSelectionTraceEvents :: Trace a -> [(Time, TracePeerSelection MockPeerAddr)]
selectPeerSelectionTraceEvents = go
  where
    go (Trace t _ _ (EventLog e) trace)
     | Just x <- fromDynamic e    = (t,x) : go trace
    go (Trace _ _ _ _ trace)      =         go trace
    go (TraceMainException _ e _) = throw (FailureException e)
    go (TraceDeadlock      _   _) = throw FailureDeadlock
    go (TraceMainReturn    _ _ _) = []


peerGraphNumStronglyConnectedComponents :: PeerGraph -> Int
peerGraphNumStronglyConnectedComponents pg =
    length (Graph.scc g)
  where
    (g,_,_) = peerGraphAsGraph pg


--
-- QuickCheck instances
--

instance Arbitrary GovernorMockEnvironment where
  arbitrary = do
      peerGraph@(PeerGraph adjacency) <- arbitrary
      rootPeerSet <- arbitraryRootPeers [ a | (_,a,_) <- adjacency ]
      targets <- arbitrary
      pickKnownPeersForGossip <- arbitrary
      pickColdPeersToForget   <- arbitrary
      return GovernorMockEnvironment{..}
    where
      arbitraryRootPeers :: [MockPeerAddr] -> Gen (Map MockPeerAddr RootPeerInfo)
      arbitraryRootPeers []    = return Map.empty
      arbitraryRootPeers peers = do
        -- We decide how many we want and then pick randomly.
        numroots  <- choose (1, ceiling . sqrt . (fromIntegral :: Int -> Double)
                                        . length $ peers)
        ixs       <- vectorOf numroots (getNonNegative <$> arbitrary)
        let peersSet  = Set.fromList peers
            pick n    = Set.elemAt i peersSet where i = n `mod` Set.size peersSet
            rootPeers = nub (map pick ixs)
        peerinfos <- vectorOf (length rootPeers) arbitrary
        return $ Map.fromList (zip rootPeers peerinfos)

  --TODO: shrink

instance Arbitrary PeerGraph where
  arbitrary = sized $ \sz -> do
      numNodes <- choose (0, sz)
      numEdges <- choose (numNodes, numNodes * numNodes `div` 2)
      edges <- vectorOf numEdges $
                 (,) <$> choose (0, numNodes-1)
                     <*> choose (0, numNodes-1)
      let addrs = map MockPeerAddr [0..numNodes-1]
      nodes <- vectorOf numNodes (gossipScript addrs 0)
      let adjacency = Map.fromListWith (<>)
                        [ (from, Set.singleton (MockPeerAddr to))
                        | (from, to) <- edges ]
          graph     = [ (node, MockPeerAddr n, outedges)
                      | (node, n) <- zip nodes [0..]
                      , let outedges = maybe [] Set.toList
                                             (Map.lookup n adjacency)
                      ]
      return (PeerGraph graph)
    where
      gossipScript :: [MockPeerAddr] -> Int -> Gen GossipScript
      gossipScript peers n =
        GossipScript <$> frequency [ (1, pure Nothing)
                                   , (4, Just <$> selectPeers 1 peers) ]
                     <*> arbitrary
                     <*> case n of
                           0 -> pure Nothing 
                           _ -> Just <$> gossipScript peers (n-1)

selectPeers :: Int -> [a] -> Gen [a]
selectPeers weight peers = do
    picked <- vectorOf (length peers)
                       (frequency [(1, pure True), (weight, pure False)])
    return [ peer | (peer, True) <- zip peers picked ]

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

instance Arbitrary RootPeerInfo where
  arbitrary = RootPeerInfo <$> arbitrary
  shrink    = genericShrink

instance Arbitrary PeerSelectionTargets where
  arbitrary = do
    targetNumberOfKnownPeers       <- getNonNegative <$> arbitrary
    targetNumberOfEstablishedPeers <- choose (0, targetNumberOfKnownPeers)
    targetNumberOfActivePeers      <- choose (0, targetNumberOfEstablishedPeers)
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

prop_arbitrary_PeerGraph :: PeerGraph -> Property
prop_arbitrary_PeerGraph pg =
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
    tabulate "num root peers" [show (Map.size (rootPeerSet env))] $
    validGovernorMockEnvironment env
  where
    emptyGraph     = null g where PeerGraph g = peerGraph env
    emptyRootPeers = Map.null (rootPeerSet env)

prop_shrink_GovernorMockEnvironment :: GovernorMockEnvironment -> Bool
prop_shrink_GovernorMockEnvironment =
    all validGovernorMockEnvironment . shrink

renderRanges :: Int -> Int -> String
renderRanges r n = show lower ++ " -- " ++ show upper
  where
    lower = n - n `mod` r
    upper = lower + (r-1)


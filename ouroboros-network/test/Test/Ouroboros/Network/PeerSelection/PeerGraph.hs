{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE ScopedTypeVariables        #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Ouroboros.Network.PeerSelection.PeerGraph (

    PeerGraph(..),
    validPeerGraph,
    allPeers,
    firstGossipReachablePeers,

    GovernorScripts(..),
    GossipScript,
    ConnectionScript,
    AsyncDemotion(..),
    GossipTime,
    interpretGossipTime,

    prop_shrink_GovernorScripts,
    prop_arbitrary_PeerGraph,
    prop_shrink_PeerGraph,

  ) where

import           Data.Graph (Graph)
import qualified Data.Graph as Graph
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Tree as Tree

import           Control.Monad.Class.MonadTime

import           Test.Ouroboros.Network.PeerSelection.Instances
import           Test.Ouroboros.Network.PeerSelection.Script

import           Test.QuickCheck
import           Test.QuickCheck.Utils


--
-- Mock environment types
--

-- | The peer graph is the graph of all the peers in the mock p2p network, in
-- traditional adjacency representation.
--
newtype PeerGraph = PeerGraph [(PeerAddr, [PeerAddr], PeerInfo)]
  deriving (Eq, Show)

-- | For now the information associated with each node is just the gossip
-- script and connection script.
--
type PeerInfo = GovernorScripts

data GovernorScripts = GovernorScripts {
    gossipScript     :: GossipScript,
    connectionScript :: ConnectionScript
  }
  deriving (Eq, Show)


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

interpretGossipTime :: GossipTime -> DiffTime
interpretGossipTime GossipTimeQuick   = 1
interpretGossipTime GossipTimeSlow    = 5
interpretGossipTime GossipTimeTimeout = 25


-- | Connection script is the script which provides asynchronous demotions
-- either to cold or warm peer.
--
type ConnectionScript = TimedScript AsyncDemotion

data AsyncDemotion = ToWarm
                   | ToCold
                   | Noop
  deriving (Eq, Show)



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


--
-- Utils for properties
--

allPeers :: PeerGraph -> Set PeerAddr
allPeers (PeerGraph g) = Set.fromList [ addr | (addr, _, _) <- g ]

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


instance Arbitrary AsyncDemotion where
    arbitrary = frequency [ (2, pure ToWarm)
                          , (2, pure ToCold)
                          , (6, pure Noop)
                          ]
    shrink ToWarm = [ToCold, Noop]
    shrink ToCold = [Noop]
    shrink Noop   = []


instance Arbitrary GovernorScripts where
    arbitrary = GovernorScripts
            <$> arbitrary
            <*> (fixConnectionScript <$> arbitrary)
    shrink GovernorScripts { gossipScript, connectionScript } =
      [ GovernorScripts gossipScript' connectionScript
      | gossipScript' <- shrink gossipScript
      ]
      ++
      [ GovernorScripts gossipScript connectionScript'
      | connectionScript' <- map fixConnectionScript (shrink connectionScript)
        -- fixConnectionScript can result in re-creating the same script
        -- which would cause shrinking to loop. Filter out such cases.
      , connectionScript' /= connectionScript
      ]

-- | We ensure that eventually the connection script will allow to connect to
-- a given peer.  This simplifies test conditions.
--
fixConnectionScript :: ConnectionScript -> ConnectionScript
fixConnectionScript (Script script) =
    case NonEmpty.last script of
      (Noop, _) -> Script   script
      _         -> Script $ script <> ((Noop, NoDelay) :| [])


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
    sized $ \sz ->
      arbitraryScriptOf (isqrt sz) gossipResult
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

isqrt :: Int -> Int
isqrt = floor . sqrt . (fromIntegral :: Int -> Double)

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


instance Arbitrary GossipTime where
  arbitrary = frequency [ (2, pure GossipTimeQuick)
                        , (2, pure GossipTimeSlow)
                        , (1, pure GossipTimeTimeout) ]

  shrink GossipTimeTimeout = [GossipTimeQuick, GossipTimeSlow]
  shrink GossipTimeSlow    = [GossipTimeQuick]
  shrink GossipTimeQuick   = []



--
-- Tests for the QC Arbitrary instances
--

prop_shrink_GovernorScripts :: Fixed GovernorScripts -> Property
prop_shrink_GovernorScripts =
    prop_shrink_nonequal

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

prop_shrink_PeerGraph :: Fixed PeerGraph -> Property
prop_shrink_PeerGraph x =
      prop_shrink_valid validPeerGraph x
 .&&. prop_shrink_nonequal x


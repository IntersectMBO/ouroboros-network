{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}

--module Sketch where

import Data.Bifunctor (Bifunctor (..))
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Graph (LEdge, LNode, mkGraph)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Time.Clock (DiffTime)
import Data.Word (Word64)
import Numeric.Natural (Natural)

type Network meta id = Map id (Labelled meta (Peer id))

data Labelled meta t = Labelled
  { label :: !meta
  , datum :: !t
  }

instance Functor (Labelled meta) where
  fmap f l = l { datum = f (datum l) }

-- | The hope is that this node datatype could actually be the very same
-- type used in production code. Throw it into a TVar and have other systems
-- update it with delta Q estimates.
data Peer id = Peer
  { coldPeers :: !(Map id ColdPeer)
  , warmPeers :: !(Map id WarmPeer)
  , hotPeers  :: !(Map id HotPeer)
  }

data ColdPeer = ColdPeer

deriving instance Show ColdPeer

data WarmPeer = WarmPeer
  { warmDeltaQ :: !DeltaQ
  }

deriving instance Show WarmPeer

data HotPeer = HotPeer
  { hotDeltaQ :: !DeltaQ
  }

deriving instance Show HotPeer

data DeltaQ where
  Bottom :: DeltaQ
  -- | G and S (per octet)
  DeltaQ :: !DiffTime -> (Natural -> DiffTime) -> DeltaQ

sum_deltaQ :: DeltaQ -> DeltaQ -> DeltaQ
sum_deltaQ Bottom         _              = Bottom
sum_deltaQ _              Bottom         = Bottom
sum_deltaQ (DeltaQ ga sa) (DeltaQ gb sb) = DeltaQ (ga + gb) (\x -> sa x + sb x)

-- DeltaQ is ordered _for a particular number of bytes_, which is given to the
-- S function and then the total DeltaQ compared.
--
-- TODO what happens when we introduce the V part? Then we have two axes.
-- Maybe we'd want to look at 95% confidence intervals for a given number of
-- bytes? In case the intervals are disjoint, it's obvious which one is better.
-- But if they overlap? If they're the same size, take the one with the lower
-- endpoint. If they're of different size and they overlap... then it depends
-- what you value more: certainty (smaller interval) or potential speed (smaller
-- lower endpoint).
ord_deltaQ :: Natural -> DeltaQ -> DeltaQ -> Ordering
ord_deltaQ _ Bottom         Bottom         = EQ
ord_deltaQ _ Bottom         _              = GT
ord_deltaQ _ _              Bottom         = LT
ord_deltaQ b (DeltaQ ga sa) (DeltaQ gb sb) = (ga + sa b) `compare` (gb + sb b)

-- Not such a great show instance. There's a function in the DeltaQ constructor
-- so what are we to do? If we assumed that S were linear in octets we could
-- print its slope.
instance Show DeltaQ where
  show Bottom       = "_|_"
  show (DeltaQ g s) = show g

data Edge where
  Cold :: Edge
  Warm :: !DeltaQ -> Edge
  Hot  :: !DeltaQ -> Edge

deriving instance Show Edge

data Node meta where
  Dead :: Node meta
  Live :: meta -> Node meta

deriving instance Show meta => Show (Node meta)

-- | From a network we can make a labelled graph where the edges are either
-- cold, warm, or hot. An encoding of identifiers as integers must be given,
-- and it needs to be injective.
--
-- Peers which are referenced in edges but do not appear in the key set of
-- the network are included as "dead" nodes. They carry no information but are
-- still useful to have in the graph, to show connections to nodes which have
-- vanished from the network. The existence of a hot or warm edge to one of
-- these nodes is of interest: 
toGraph :: forall meta id . Ord id => (id -> Int) -> Network meta id -> Gr (Node meta) Edge
toGraph mkIntId network = mkGraph nodes edges

  where

  -- The node set: use the identifier injection to get the Int identifier, and
  -- retain the metadata for the node label.
  nodes :: [LNode (Node meta)]
  nodes =
       fmap (\(id, lnode) -> (mkIntId id, Live (label lnode))) livePeers
    ++ fmap (\id          -> (mkIntId id, Dead              )) deadPeers

  -- Compute the edges using list monad do notation. Perhaps would be more
  -- readable as a list comprehension?
  edges :: [LEdge Edge]
  edges = do
    (id, Labelled _ node) <- livePeers
    let this  = mkIntId id
        colds :: [id]
        colds = Map.keys (coldPeers node)
        warms :: [(id, DeltaQ)]
        warms = fmap (\(id, warm) -> (id, warmDeltaQ warm)) (Map.toList (warmPeers node))
        hots  :: [(id, DeltaQ)]
        hots  = fmap (\(id, hot)  -> (id, hotDeltaQ hot))   (Map.toList (hotPeers  node))
        coldEdges :: [LEdge Edge]
        coldEdges = fmap (\id -> (this, mkIntId id, Cold)) colds
        warmEdges :: [LEdge Edge]
        warmEdges = fmap (\(id, deltaQ) -> (this, mkIntId id, Warm deltaQ)) warms
        hotEdges  :: [LEdge Edge]
        hotEdges  = fmap (\(id, deltaQ) -> (this, mkIntId id, Hot deltaQ))  hots
    coldEdges ++ warmEdges ++ hotEdges

  -- All node identifiers which appear as terminals of edges
  terminalPeers :: Set id
  terminalPeers = Set.fromList $ do
    (id, Labelled _ node) <- livePeers
    let colds = Map.keys (coldPeers node)
        warms = Map.keys (warmPeers node)
        hots  = Map.keys (hotPeers  node)
    colds ++ warms ++ hots

  -- All node identifiers which appear as initials of edges
  initialPeers :: Set id
  initialPeers = Set.fromList (fmap fst livePeers)

  -- Nodes which are referenced as terminals from other nodes, but do not
  -- appear in the key set of the network.
  deadPeers :: [id]
  deadPeers = Set.toList (Set.difference terminalPeers initialPeers)

  livePeers :: [(id, Labelled meta (Peer id))]
  livePeers = Map.toList network

-- | Create a network with no edges on a given set of identifier, metadata
-- pairs.
emptyNetworkOn :: Ord id => [(id, meta)] -> Network meta id
emptyNetworkOn = Map.fromList . fmap (\(id, meta) -> (id, Labelled meta emptyPeer))

emptyPeer :: Peer id
emptyPeer = Peer
  { coldPeers = Map.empty
  , warmPeers = Map.empty
  , hotPeers  = Map.empty
  }

-- Non-negative edge weight with infinity.
data Weight edge weight = Weight
  { edgeWeight   :: edge -> weight
  , weightOrder  :: weight -> weight -> Ordering
  -- Sum of weights, respecting the order
  --
  --   weightSum a b = weightSum b a
  --   forall c . (a >= b) ==> (weightSum a c >= b)
  --   weightSum a weightInf = weightInf
  --
  , weightSum    :: weight -> weight -> weight
  -- A weight that is greater than all other weights under weightOrder
  , weightInf    :: weight
  }

-- DeltaQ weight for a given number of bytes to be sent (DeltaQ doesn't have
-- a total order otherwise).
weight_deltaQ :: Natural -> Weight Edge DeltaQ
weight_deltaQ b = Weight
  { edgeWeight = \edge -> case edge of
      Cold    -> Bottom
      Warm dq -> dq
      Hot  dq -> dq
  , weightOrder = ord_deltaQ b
  , weightSum   = sum_deltaQ
  , weightInf   = Bottom
  }

--shortestPaths :: (Ord e, Monoid e) => Gr v e -> 

-- Next step: let's generate a graph pseudo-randomly for a given set of
-- identifiers. Graph generation involves:
-- 1. Randomly picking cold peers for each node from the set of identifiers.
-- 2. For each node, randomly selecting cold peers for which we randomly
--    generate DeltaQs.
--
-- Noo no no no that's the wrong way round!
-- We're not interested in pseudo0-randomly generating a graph; rather, we want
-- to pseudo-randomly generate node-local data.
-- We have given
--   - A set of node identifiers; these are all of the nodes that exist in the
--     world
-- Then we can, for each node, randomly pick cold peers from this set.
--
-- What changes can we make?
--
--   For a particular node:
--   - add/remove cold peers
--   - demote/promote hot/warm peers
--   - change delta Q for warm peers
--   The former 2 can be specified as part of a policy that is a parameter to
--   each node in the system. The random part is the delta Q variation, but
--   its distribution should be a parameter
--
-- Parameters:
--   - An initial set of a peers and a rule to determine how/when/if peers are
--     added/removed.
--   - For any node, its policy for altering its cold/warm/hot peers.
--   - A rule to make delta Q distributions for each pair of peers.
--   - When a peer is removed, how long before other peers realize it's not
--     responding and demote it... That's a part of the prior item let's say.
-- Observations:
--   - Characteristic path length of the graph over time.
--     Path length in terms of a delta Q? We're interested not only in the
--     number of hops, but also the cumulative delta Q for the entire path.
--   -  

type Time = Word64

-- Types for parameters:

-- | Local policies for each peer, identified by the @id@. NB: each policy
-- is universally quantified over the @id@ type, so you can't change the
-- policy based on the identifier of the _remote_ peers at that given id.
type LocalPolicies id = id -> LocalPolicy

-- | The true DeltaQ for each pair of peers, at any given time.
type LatencyDistributions id = Time -> id -> id -> DeltaQ

-- | Which peers is a given peer aware of at a given time (the cold peers).
type PeerAwareness id = Time -> id -> Set id

-- | Policy for cold/warm/hot juggling at a peer.
-- Does not depend upon time! Only upon the current peer set.
-- The latency distribution parameter will determine when the delta Qs in here
-- change.
--
-- What about the set of cold peers? The cold peers that each peer is
-- aware of is not a matter of a policy, it's a system-wide parameter like the
-- latency distributions.
-- Also, this type isn't so good: it's able to change the delta q!
-- Instead, we should give rules for demoting/promotion.
--
-- The Set id argument is the set of all peers that this peer is aware of,
-- as prescribed by a @PeerAwareness id@.
type LocalPolicy = forall id . Ord id => Set id -> Peer id -> Peer id -- DPeer id

{-
-- | Specify which identifiers are hot and which are warm.
-- If you have @forall id . Ord id => Peer id -> DPeer id@ then a new @Peer id@
-- can be given 
data DPeer id = DPeer
  { dhot  :: Set id
  , dwarm :: Set id
  }
-}

-- How about this: nodes are never added or removed from the network. They
-- don't need to be. A node going down can be expressed by setting the
-- DeltaQ to bottom for every pair involving that node.
--
--
-- Also, we don't want to represent them in this way, as functions. Instead,
-- it's more practical to use lists with diff times.
--
--   [(DiffTime, SomethingToDo)]
--
-- then we can merge all of these event lists to get graph mutations
--   type LocalPolicies id = id -> [(DiffTime, Peer id)]

network_at
  :: Map id meta
  -> PeerAwareness id
  -> LatencyDistributions id
  -> LocalPolicies id
  -> Time
  -> Network meta id
network_at peers awarneess latency policies time = undefined

-- | We will be interested in changes to a network...
type DNetwork id = forall meta . Network meta id -> Network meta id

-- | ... and also changes to a node
type DPeer id = Peer id -> Peer id

-- | Independent changes to a node can be applied uniformly to a network.
d_uniform :: DPeer id -> DNetwork id
d_uniform dnode = (fmap . fmap) dnode

-- | Change one node in a network
d_one :: Ord id => id -> DPeer id -> DNetwork id
d_one key f = Map.adjust (fmap f) key

-- We'll be interested in computing the characteristic path length: median of
-- the means of shortest paths between all nodes. "Shortest" in 2 different
-- senses:
-- - fewest hops over hot edges
-- - smallest delta q over hot edges
-- It would also be interesting to compare the CPL over warm and hot edges
-- versus only hot. Cold edges are always _|_ weight and so are not shorter
-- than any other path.
--
-- Not just the CPL, we should instead just take all pairs shortest paths
-- and then compute mean, min, max, median, std. dev., etc... We can even
-- plot it as a histogram? Sort of like a histogram... 
--
-- Sadly, the fgl shortest-path algorithms assume the edge type is an instance
-- of @Real@. But in fact all we need is a way to add them (DeltaQ semigroup
-- instance) and a way to compare them (DeltaQ ord instance).
-- Also, it gives a LRTree which is a list of lists of NODEs with their
-- labels. We want the edge labels too though.
characteristic_path_length_dq :: Gr (Node meta) Edge -> DeltaQ
characteristic_path_length_dq gr = undefined

-- We'll also want o determine, for each node pair, the CDF for time to diffuse
-- a block from A to B, assuming each peer sends the block to all of its
-- hot peers.
-- But how does that work with DeltaQ? If A sends the block to two peers B and
-- C, then we have 2 Delta Qs that must be combined in a kind of "OR" style...
--
-- For every pair of nodes that are directly connected, we know their CDF: its
-- given by the DeltaQ. We can throw those into a `Map NodePair CDF`.
-- The other elements of it are defined recursively on that same map, assuming
-- we have that combination of CDFs: just combine all CDFs for the terminus of
-- each hot edge of peer A, to peer B.
-- That would loop, though.


-- How's this for a simple goal for today?: make a graph where all nodes are
-- connected to all others by hot edges with uniform G and S (configurable),
-- then do all-pairs shortest paths by both
-- - hops (edge weight 1, sum monoid)
-- - delta q (edge weight at 2e6 bytes, sum monoid with _|_ as infinity, perfection as 0)

all_pairs_sp
  :: forall meta id m .
     ( Ord m, Monoid m )
  => (id -> Int)
  -> (Edge -> m)
  -> Network meta id
  -> Map (id, id) m
all_pairs_sp intId edgeWeight network = undefined
  where
  graph :: Gr (Node meta) m
  graph = second edgeWeight (toGraph intId network)

  sps_at :: id -> LRTree m
  sps_at id = spTree (intId id) graph








main :: IO ()
main = pure ()

{-# LANGUAGE ScopedTypeVariables #-}

module Algebra.Graph.Labelled.AdjacencyMap.ShortestPath where

import Algebra.Graph.Labelled.AdjacencyMap
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified Data.List.NonEmpty as NE ((!!), length, sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Monoid (Sum (..), mconcat)
import Data.PriorityQueue.FingerTree (PQueue)
import qualified Data.PriorityQueue.FingerTree as PQ
import Data.Semigroup (sconcat)
import Numeric.Natural

-- | Use this as your edge weight to calculate number of hops as path length.
newtype Hops = Hops
  { getHops :: Natural
  } deriving (Eq, Ord, Show)

hop :: a -> Hops
hop = const (Hops 1)

instance Semigroup Hops where
  left <> right = Hops (getHops left + getHops right)

instance Monoid Hops where
  mappend = (<>)
  mempty  = Hops 0

-- | All shortest paths for all vertices.
type AllShortestPaths vertex edge weight = Map vertex (ShortestPaths vertex edge weight)

-- | Shortest paths from a given vertex.
type ShortestPaths vertex edge weight = Map vertex (WeightedPath vertex edge weight)

-- | Full description of a weighted path, in reverse order from some vertex
-- (would be the key in a ShortestPaths). Each list element is one edge
-- traversal, and gives the edge label, its weight, and the initial vertex.
type WeightedPath vertex edge weight = [(edge, weight, vertex)]

path_weights :: WeightedPath vertex edge weight -> [weight]
path_weights = fmap (\(_, w, _) -> w)

map_weights :: (weight -> weight') -> WeightedPath vertex edge weight -> WeightedPath vertex edge weight'
map_weights f = fmap (\(e, w, v) -> (e, f w, v))

-- | An empty path has undefined weight, so it becomes Nothing.
total_weight :: Semigroup weight => WeightedPath vertex edge weight -> Maybe weight
total_weight = fmap sconcat . nonEmpty . fmap (\(_, w, _) -> w)

-- | The length of the path in hops; not necessarily the same as the total
-- weight of that path.
path_length :: WeightedPath vertex edge weight -> Natural
path_length = fromIntegral . length

-- | A shortest path candidate: its head and the path traversed so far.
data Candidate vertex edge weight = Candidate
  { candidateHead :: !vertex
  , candidateTail :: ![(edge, weight, vertex)]
  }

-- | Extend a candidate to a new vertex, traversing a given edge with a given
-- weight.
extend_candidate
  :: vertex
  -> edge
  -> weight
  -> Candidate vertex edge weight
  -> Candidate vertex edge weight
extend_candidate v e w candidate = candidate
  { candidateHead = v
  , candidateTail = (e, w, candidateHead candidate) : candidateTail candidate
  }

-- | All pairs shortest paths for a given weight.
-- Runs 'dijkstra' for each vertex in the graph.
-- For the monoid instance on the weight, its zero means "no distance". Must
-- have that the zero is less than every non-zero element.
all_pairs_sp
  :: forall vertex edge weight .
     ( Ord vertex, Ord weight, Monoid weight )
  => (vertex -> vertex -> edge -> weight)
  -> AdjacencyMap edge vertex
  -> Map vertex (ShortestPaths vertex edge weight)
all_pairs_sp mkWeight gr = Map.mapWithKey
  (\v _ -> dijkstra mkWeight v gr)
  (adjacencyMap gr)

-- | 'all_pairs_sp' but remove the entry for each node to itself.
all_pairs_sp_antireflexive
  :: forall vertex edge weight .
     ( Ord vertex, Ord weight, Monoid weight )
  => (vertex -> vertex -> edge -> weight)
  -> AdjacencyMap edge vertex
  -> Map vertex (ShortestPaths vertex edge weight)
all_pairs_sp_antireflexive mkWeight gr = remove_reflexives (all_pairs_sp mkWeight gr)
  where
  remove_reflexives = Map.mapWithKey Map.delete

-- | Shortest paths from a given vertex under a given weight. The Ord instance
-- determines "shorter-than": a min-priority-queue is used.
-- 0 in the monoid on weight must be no cost, less than everything under Ord.
--
-- The Ord vertex is needed because of the way AdjacencyMap is implemented.
dijkstra
  :: forall vertex edge weight .
     ( Ord vertex, Ord weight, Monoid weight )
  => (vertex -> vertex -> edge -> weight)
  -> vertex
  -> AdjacencyMap edge vertex
  -> ShortestPaths vertex edge weight
dijkstra mkWeight start gr = loop initialPQ gr Map.empty

  where

  -- The initial shortest paths has every vertex at infinite distance (weight).
  -- This will be refined by Map.insert for vertices which have a path of
  -- finite weight.
  initialMap :: ShortestPaths vertex edge weight
  initialMap = fmap (const []) (adjacencyMap gr)

  loop :: PQueue weight (Candidate vertex edge weight)
       -> AdjacencyMap edge vertex
       -> ShortestPaths vertex edge weight
       -> ShortestPaths vertex edge weight
  loop pq gr sps = case PQ.minViewWithKey pq of
    Nothing -> sps
    Just ((weight, candidate), pq') -> case Map.lookup (candidateHead candidate) (adjacencyMap gr) of
      -- If the candidate is not in the graph, we must have already reached it
      -- and included it in the shortest paths. Ignore this step.
      Nothing -> loop pq' gr sps
      Just neighbours ->
        let -- The frontier is all vertices reachable in the graph so far (the
            -- graph shrinks at each step to exclude the candidate that was
            -- just reached).
            frontier :: [(vertex, edge)]
            frontier = Map.toList neighbours
            -- Calculate the weights to the frontier and make candidates by
            -- extending this candidate
            weights :: [(vertex, edge, weight)]
            weights = fmap (\(v, e) -> (v, e, mkWeight (candidateHead candidate) v e)) frontier
            candidates :: [(weight, Candidate vertex edge weight)]
            candidates = fmap (\(v, e, w) -> (weight <> w, extend_candidate v e w candidate)) weights
            pq' :: PQueue weight (Candidate vertex edge weight)
            pq' = PQ.union pq (PQ.fromList candidates)
            gr' :: AdjacencyMap edge vertex
            gr' = removeVertex (candidateHead candidate) gr
            sps' :: ShortestPaths vertex edge weight
            sps' = Map.insert (candidateHead candidate) (candidateTail candidate) sps
        in  loop pq' gr' sps'

  initialPQ :: PQueue weight (Candidate vertex edge weight)
  initialPQ = PQ.singleton mempty initialCandidate

  initialCandidate :: Candidate vertex edge weight
  initialCandidate = Candidate start []

-- | The median of the mean shortest path length (here: weight, use a simple
-- 1 weight to get hop length).
--
-- This becomes undefined/infinite (Nothing) if any 2 peers are not reachable.
characteristic_path_length
  :: forall vertex edge weight fractional .
     ( Ord fractional, Fractional fractional )
  => (weight -> Maybe fractional)
  -> AllShortestPaths vertex edge weight
  -> Maybe fractional
characteristic_path_length mkFractional sps = do
  let means :: [Maybe fractional]
      means = fmap (mean_path_weight mkFractional) (Map.elems sps)
  -- If there are no means, the CPL is undefined.
  nonEmptyMeans :: NonEmpty (Maybe fractional) <- nonEmpty means
  -- If any of the means is Nothing, the CPL is undefined.
  finiteMeans   :: NonEmpty fractional         <- sequence nonEmptyMeans
  pure $ median finiteMeans
  where
  median :: NonEmpty fractional -> fractional
  median ne = case quotRem (NE.length ne) 2 of
    -- If the length is odd, there's a midpoint.
    (q, 1) -> NE.sort ne NE.!! (q+1)
    -- If the lenght is even, take the average of the two closest points
    (q, 0) -> let sorted = NE.sort ne
              in  (sorted NE.!! q + sorted NE.!! (q+1)) / 2
    _      -> error "median: impossible"

-- | The average path weight in the set of shortest paths from a given vertex.
--
-- The weight must be transformed to some fractional thing, so that we can
-- take the mean. It admits infinite weight by using Nothing.
--
-- If any of them are unreachable (empty weighted paths), this is Nothing.
-- If there are no paths, then this is Nothing.
mean_path_weight
  :: forall vertex edge weight fractional .
     ( Fractional fractional )
  => (weight -> Maybe fractional)
  -> ShortestPaths vertex edge weight
  -> Maybe fractional
mean_path_weight mkFractional sps = do
  let toFractionalSum :: weight -> Maybe (Sum fractional)
      toFractionalSum = fmap Sum . mkFractional
      -- For each path, make all of its weights fractional, and use sequence
      -- to ensure that any Nothing in the weight list makes the whole list
      -- Nothing
      finiteFractions :: [Maybe [Sum fractional]]
      finiteFractions = fmap (sequence . fmap toFractionalSum . path_weights) (Map.elems sps)
      finiteFractionalSums :: [Maybe (Sum fractional)]
      finiteFractionalSums = (fmap . fmap) mconcat finiteFractions
  -- There must be at least one path
  nePaths    :: NonEmpty (Maybe (Sum fractional)) <- nonEmpty finiteFractionalSums
  -- If any of the paths are Nothing, the mean is Nothing.
  finiteSums :: NonEmpty (Sum fractional)         <- sequence nePaths
  pure $ getSum (sconcat finiteSums) / fromIntegral (length finiteSums)

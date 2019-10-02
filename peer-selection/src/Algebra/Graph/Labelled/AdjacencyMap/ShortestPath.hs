{-# LANGUAGE ScopedTypeVariables #-}

module Algebra.Graph.Labelled.AdjacencyMap.ShortestPath where

import Algebra.Graph.Labelled.AdjacencyMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Monoid (mconcat)
import Data.PriorityQueue.FingerTree (PQueue)
import qualified Data.PriorityQueue.FingerTree as PQ
import Data.Word (Word32)

type WeightedPath vertex edge weight = [(edge, weight, vertex)]

map_weights :: (weight -> weight') -> WeightedPath vertex edge weight -> WeightedPath vertex edge weight'
map_weights f = fmap (\(e, w, v) -> (e, f w, v))

total_weight :: Monoid weight => WeightedPath vertex edge weight -> weight
total_weight = mconcat . fmap (\(_, w, _) -> w)

path_length :: WeightedPath vertex edge weight -> Word32
path_length = fromIntegral . length

type ShortestPaths vertex edge weight = Map vertex (WeightedPath vertex edge weight)

type AllShortestPaths vertex edge weight = Map vertex (ShortestPaths vertex edge weight)

data Candidate vertex edge weight = Candidate
  { candidateHead :: !vertex
  , candidateTail :: ![(edge, weight, vertex)]
  }

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

all_pairs_sp
  :: forall vertex edge weight .
     ( Ord vertex, Ord weight, Monoid weight )
  => (vertex -> vertex -> edge -> weight)
  -> AdjacencyMap edge vertex
  -> Map vertex (ShortestPaths vertex edge weight)
all_pairs_sp mkWeight gr = Map.mapWithKey
  (\v _ -> dijkstra mkWeight v gr)
  (adjacencyMap gr)

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

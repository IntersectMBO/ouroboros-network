{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DeltaQ.Examples.Gen where

import Algebra.Graph.Labelled.AdjacencyMap (AdjacencyMap (..), adjacencyMap)
import qualified Algebra.Graph.Labelled.AdjacencyMap as GR
import Data.Foldable (foldlM)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Semigroup (Last (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Numeric.Natural (Natural)

-- | A path on a non-empty list of vertices.
-- 
-- The continuation is given the final vertex in the list (and therefore
-- in the path), and the constructed path.
--
-- The edge is inside some @m@ to admit the possibility of generating it
-- pseudorandomly.
--
-- NB: if there are duplicate vertices in the list (under its Eq instance)
-- then this may contain cycles.
path
  :: ( Monad m, Ord vertex, Semigroup edge )
  => m edge
  -> NonEmpty vertex
  -> (vertex -> AdjacencyMap edge vertex -> m t)
  -> m t
path mkE vs k = go vs GR.empty
  where
  go (last NE.:| [])     !gr = k last gr
  go (v    NE.:| (u:vs)) !gr = mkE >>= \e -> go (u NE.:| vs) (GR.overlay gr (GR.edge e v u))

-- | A 'path' in which an edge from last vertex to the first vertex is included.
cycle
  :: ( Monad m, Ord vertex, Semigroup edge )
  => m edge
  -> NonEmpty vertex
  -> m (AdjacencyMap edge vertex)
cycle mkE vs = path mkE vs $ \lastVertex thePath -> mkE >>= \e ->
  pure (GR.overlay thePath (GR.edge e lastVertex (NE.head vs)))

-- To add "shortcuts" to a cycle, you can generate random a 1-regular graph on
-- a random subset of the vertex set, then overlay it.

-- How to express randomly selecting a vertex?
--
--   forall v. Ord v => Set v -> m v
--
-- that type guarantees that the @v@ is in the set.

type VertexSelector m = forall v . Ord v => Set v -> m v

-- | For every vertex in the set, generate a list of edges and put them in the
-- graph.
--
-- The type of the generator for edges ensures that every edge actually lies
-- within the vertex set.
gen_for_each
  :: ( Monad m, Ord vertex, Semigroup edge )
  => (forall v . Ord v => v -> Set v -> m [(edge, v)])
  -> Set vertex
  -> m (AdjacencyMap edge vertex)
gen_for_each genEdges vertices = foldlM includeEdges GR.empty (Set.toList vertices)
  where
  includeEdges gr v = do
    es <- genEdges v vertices
    let gr' = GR.edges (fmap (\(e, terminus) -> (e, v, terminus)) es)
    pure (GR.overlay gr gr')

-- | A regular graph of given degree. If the degree is greater than or equal
-- to one less the size of the vertex set, it's a complete graph.
--
-- It uses gen_for_each but takes a slightly different kind of edge generator:
-- given the current vertex and a vertex set, generate a vertex, an edge, and
-- another set. The new set must be derived from the given vertices. This
-- generator will be run degree times for each vertex.
--
-- This graph is not necessarily connected. To guarantee a connected graph, one
-- could start with an arbitrary cycle, and then overlay a regular graph.
regular
  :: forall m vertex edge .
     ( Monad m, Ord vertex, Semigroup edge )
  => (forall v . Ord v => v -> Set v -> m (edge, v, Set v))
  -> Natural -- ^ Degree
  -> Set vertex
  -> m (AdjacencyMap edge vertex)
regular genEdge degree vertices = gen_for_each genEdges vertices
  where
  genEdges :: forall v . Ord v => v -> Set v -> m [(edge, v)]
  genEdges = go degree []
  go :: forall v . Ord v => Natural -> [(edge, v)] -> v -> Set v -> m [(edge, v)]
  go 0 acc _ _  = pure acc
  go n acc v vs = do
    (edge, v', vs') <- genEdge v vs
    go (n-1) ((edge, v'):acc) v vs'

-- | Simple generator for a regular graph. Select an edge at random from
-- the set, favouring *not* the current vertex.
simple_edge_gen :: (Monad m, Ord v) => m Double -> v -> Set v -> m ((), v, Set v)
simple_edge_gen genDouble v vs = case Set.toList (Set.delete v vs) of
  []       -> pure ((), v, vs)
  (v':vs') -> do
    u <- random_list_element genDouble (v' NE.:| vs')
    pure ((), u, Set.delete u vs)

random_list_element
  :: ( Monad m )
  => m Double -- ^ in (0.0, 1.0)
  -> NonEmpty v
  -> m v
random_list_element genDouble lst = go (NE.length lst) lst
  where
  go len lst = do
    d <- genDouble
    case (d <= (1.0 / fromIntegral len), lst) of
      (True,  x NE.:| _)      -> pure x
      (False, _ NE.:| (x:xs)) -> go (len-1) (x NE.:| xs)
      -- The double is not less or equal to 1.0... oh well
      (False, x NE.:| [])     -> pure x

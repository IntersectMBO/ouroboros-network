{-# LANGUAGE ScopedTypeVariables #-}

module Algebra.Graph.Labelled.AdjacencyMap.Viz where

import Algebra.Graph.Labelled.AdjacencyMap
import qualified Data.Map as Map
import Data.GraphViz
import Data.GraphViz.Attributes
import Data.GraphViz.Attributes.Complete
import Data.Text.Lazy (Text)

-- | Visualize an edge-labelled graph as a graphviz dot program. You must
-- provide the node labels of type @nl@. Since, in order to make a dot program,
-- there must be a @PrintDot@ instance on the vertex type, this function also
-- makes it easy to re-define the vertex type of the graph, to some type which
-- _does_ have a @PrintDot@ instance.
toGraphvizDot
  :: forall vertex edge n nl el cl l .
     ( Ord cl, Ord n )
  => GraphvizParams n nl el cl l
  -> (vertex -> n) -- ^ Must be injective or else your graph will be mangled.
  -> (n -> nl)
  -> (edge -> el)
  -> AdjacencyMap edge vertex
  -> DotGraph n
toGraphvizDot params recode vlabel elabel gr = graphElemsToDot params vertices edges
  where
  --vertices :: [(n, nl)]
  vertices = Map.toList (Map.mapWithKey (const . vlabel) map)
  --edges :: [(n, n, el)]
  edges = [ (n1, n2, elabel e) | (n1, adj) <- Map.toList map, (n2, e) <- Map.toList adj ]
  map = fmap (Map.mapKeys recode) (Map.mapKeys recode (adjacencyMap gr))

-- | Just like 'Data.GraphViz.quickParams', but doesn't demand the ad-hoc
-- Labellable typeclass instance on node and edge labels.
simpleGraphvizParams
  :: (nl -> Text)
  -> (el -> Text)
  -> GraphvizParams n nl el () nl
simpleGraphvizParams nl el = nonClusteredParams { fmtNode = nodeFmt, fmtEdge = edgeFmt }
  where
  nodeFmt (_,l) = [Label (StrLabel (nl l))]
  edgeFmt (_,_,e) = [Label (StrLabel (el e))]

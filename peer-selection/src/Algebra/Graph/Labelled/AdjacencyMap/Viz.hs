{-# LANGUAGE ScopedTypeVariables #-}

module Algebra.Graph.Labelled.AdjacencyMap.Viz where

import Algebra.Graph.Labelled.AdjacencyMap
import qualified Data.Map as Map
import Data.GraphViz
import Data.GraphViz.Attributes.Complete
import Data.Text.Lazy (Text)

-- | Visualize an edge-labelled graph as a graphviz dot program.
-- Uses the Enum instance so that Int is the dot vertex type (has a PrintDot
-- instance).
toGraphvizDot
  :: forall nl el cl l .
     ( Ord cl, Enum nl )
  => GraphvizParams Int nl el cl l
  -> AdjacencyMap el nl
  -> DotGraph Int
toGraphvizDot params gr = graphElemsToDot params vertices' edges'
  where
  vertices' :: [(Int, nl)]
  vertices' = Map.toList (Map.mapWithKey (const . toEnum) map')
  edges' :: [(Int, Int, el)]
  edges' = [ (n1, n2, e) | (n1, adj) <- Map.toList map', (n2, e) <- Map.toList adj ]
  map' = fmap (Map.mapKeys fromEnum) (Map.mapKeys fromEnum (adjacencyMap gr))

viz_directed :: GraphvizParams n nl el cl nl -> GraphvizParams n nl el cl nl
viz_directed gp = gp { isDirected = True }

viz_undirected :: GraphvizParams n nl el cl nl -> GraphvizParams n nl el cl nl
viz_undirected gp = gp { isDirected = False }

-- | Just like 'Data.GraphViz.quickParams', but doesn't demand the ad-hoc
-- Labellable typeclass instance on node and edge labels.
simpleGraphvizParams
  :: (nl -> Text)
  -> (el -> Text)
  -> GraphvizParams n nl el () nl
simpleGraphvizParams nl el = nonClusteredParams
  { fmtNode = nodeFmt
  , fmtEdge = edgeFmt
  }
  where
  nodeFmt (_,l) = [Label (StrLabel (nl l))]
  edgeFmt (_,_,e) = [Label (StrLabel (el e))]

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module DeltaQ.Topography where

import Algebra.Graph.Labelled.AdjacencyMap

--import DeltaQ.OutcomesTypes

-- | A monoid suitable for use with algebraic-graphs, which treats mempty of
-- the edge label as "no edge". It's not OK to use, for instance, SimpleGS as
-- the edge label directly. If multiple edges are found, these will be combined,
-- so that one Bottom edge makes all edges Bottom. What's more, mempty is
-- perfection, which just does not correspond to "no edge".
newtype Edges e = Edges { getEdges :: [e] }
  deriving (Show)

instance Semigroup (Edges e) where
  left <> right = Edges (getEdges left <> getEdges right)

instance Monoid (Edges e) where
  mappend = (<>)
  mempty  = Edges mempty

instance Eq e => Eq (Edges e) where
  left == right = getEdges left == getEdges right

instance Ord e => Ord (Edges e) where
  left `compare` right = getEdges left `compare` getEdges right

instance Functor Edges where
  fmap f = Edges . fmap f . getEdges

edge :: e -> Edges e
edge = Edges . pure

-- | An adjacency map graph with 'Edges edge' as its edge label.
type Topography edge vertex = AdjacencyMap (Edges edge) vertex

{-
instance (QualityAttenuationMeasure a, Monoid a, Eq b, Ord b) => 
         TomographyGraph (Topography a b) where
  type QAMeasure (Topography a b) = a
  type NodeName  (Topography a b) = b

  linkAttenutation a n1 n2 = edgeLabel n1 n2 a
  nodes                    = vertexSet
-}

-- | remove the supplied bi-directional link from the connectivity,
--   ignore if link not present.
maskLinks :: (Ord b) => [(b,b)] -> Topography a b -> Topography a b
maskLinks  rm tg
  = foldr (\(x,y) -> removeEdge x y) tg (expand rm)
  where
    expand xs = concat [[(a,b), (b,a)] | (a,b) <- xs]

-- | transform the link annotation for a given topology
updateLinkAnnotation :: ( Ord a, Eq edge' )
                     => (a -> a -> edge -> edge')
                     -> Topography edge a -> Topography edge' a
updateLinkAnnotation f = edges . map f' . edgeList
  where
    f' = \(edges,a,a') -> (fmap (f a a') edges, a, a')

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module DeltaQ.Topography where

import Algebra.Graph.Labelled.AdjacencyMap
import Data.Semigroup (Last (..))

edge :: e -> Last e
edge = Last

-- | An adjacency map graph with at most one edge per pair of vertices. The
-- @Last@ semigroup is used to favour later graphs when one is constructed
-- compositionally.
type Topography edge vertex = AdjacencyMap (Last edge) vertex

-- | remove the supplied bi-directional link from the connectivity,
--   ignore if link not present.
maskLinks :: (Ord b) => [(b,b)] -> Topography a b -> Topography a b
maskLinks  rm tg
  = foldr (\(x,y) -> removeEdge x y) tg (expand rm)
  where
    expand xs = concat [[(a,b), (b,a)] | (a,b) <- xs]

-- | transform the link annotation for a given topology
updateLinkAnnotation :: ( Ord a )
                     => (a -> a -> edge -> edge')
                     -> Topography edge a -> Topography edge' a
updateLinkAnnotation f = edges . map f' . edgeList
  where
    f' = \(theedges,a,a') -> (fmap (f a a') theedges, a, a')

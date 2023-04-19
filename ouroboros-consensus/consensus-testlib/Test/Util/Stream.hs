{-# LANGUAGE DeriveTraversable #-}

module Test.Util.Stream (
    Stream (..)
  , nubOrdBy
  ) where

import qualified Data.Set as Set

data Stream a = a :< Stream a
  deriving (Foldable, Functor, Show, Traversable)

nubOrdBy :: Ord b => (a -> b) -> Set.Set b -> Stream a -> Stream a
nubOrdBy f = go
  where
    go acc (x :< xs)
      | Set.member (f x) acc = go acc xs
      | otherwise            = x :< go (Set.insert (f x) acc) xs

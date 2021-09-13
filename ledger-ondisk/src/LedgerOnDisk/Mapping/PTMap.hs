{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingVia #-}
module LedgerOnDisk.Mapping.PTMap where

import LedgerOnDisk.Mapping.Class
import LedgerOnDisk.Diff
import Prelude hiding (lookup)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Map.Monoidal (MonoidalMap(..))

-- note, not an instance of Mapping
newtype DiffMap k a where
  DiffMap :: Map k (Diff a) -> DiffMap k a
  deriving stock (Eq, Show)
  deriving (Semigroup, Monoid) via MonoidalMap k (Diff a)

data PTMap k a = PTMap !(Map k a) !(DiffMap k a)
  deriving stock (Eq, Show)

instance (Ord k, Semigroup a) => Semigroup (PTMap k a) where
  PTMap m1 d1 <> PTMap m2 d2 = PTMap (Map.unionWith (<>) m1 m2) (d1 <> d2)

instance (Ord k, Semigroup a) => Monoid (PTMap k a) where
  mempty = PTMap Map.empty mempty

instance Mapping PTMap where
  type MappingConstraint PTMap = Ord
  lookup k (PTMap m _) = Map.lookup k m
  insert k v (PTMap m (DiffMap d)) = PTMap (Map.insert k v m) (DiffMap $ Map.insertWith (<>) k (DChangeTo v) d)
  update k v (PTMap m (DiffMap d)) = PTMap (Map.insertWith (<>) k v m) (DiffMap $ Map.insertWith (<>) k (DMappend v) d)
  delete k (PTMap m (DiffMap d)) = PTMap (Map.delete k m) (DiffMap $ Map.insertWith (<>) k DRemove d)
  union = (<>)
  restrictKeys (PTMap m1 d) (PTMap m2 _) = PTMap (m1 `restrictKeys` m2) d
  partition f (PTMap m d) = let
    (ml, mr) = Map.partitionWithKey f m
    -- TODO this is a bit dodgy
    in (PTMap ml d, PTMap mr mempty)

ptMapFromMap :: (Ord k) => Map k v -> PTMap k v
ptMapFromMap hm = PTMap hm mempty

diffMapFromPTMap :: PTMap k v -> DiffMap k v
diffMapFromPTMap (PTMap _ d) = d

mapFromPTMap :: PTMap k v -> Map k v
mapFromPTMap (PTMap m _) = m

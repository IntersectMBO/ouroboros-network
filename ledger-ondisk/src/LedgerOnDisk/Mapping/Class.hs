-- |

{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module LedgerOnDisk.Mapping.Class where

import Data.Map (Map)
import Data.Kind
import qualified Data.Map.Strict as Map
import Data.HashMap.Strict (HashMap)
import Data.Hashable
import qualified Data.HashMap.Strict as HashMap
import Data.Monoid

-- TODO create quickcheck suite to validate instances
class Mapping (map :: Type -> Type -> Type) where
  type MappingConstraint map :: Type -> Constraint
  lookup :: MappingConstraint map k => k      -> map k a -> Maybe a
  insert :: MappingConstraint map k => k -> a -> map k a -> map k a
  update :: (MappingConstraint map k, Semigroup a) =>
                                       k -> a -> map k a -> map k a
  delete :: MappingConstraint map k => k      -> map k a -> map k a

  union :: (MappingConstraint map k, Semigroup a) => map k a -> map k a -> map k a
  restrictKeys :: (MappingConstraint map k) => map k a -> map k b -> map k a
  partition :: (MappingConstraint map k) => (k -> a -> Bool) -> map k a -> (map k a, map k a)


instance Mapping Map where
  type MappingConstraint Map = Ord
  lookup = Map.lookup
  insert = Map.insert
  update = Map.insertWith (<>)
  delete = Map.delete
  union = Map.unionWith (<>)
  restrictKeys m1 m2 = Map.restrictKeys m1 (Map.keysSet m2)
  partition = Map.partitionWithKey


class (Eq k, Hashable k) => HashMappingConstraint k
instance (Eq k, Hashable k) => HashMappingConstraint k

instance Mapping HashMap where
  type MappingConstraint HashMap = HashMappingConstraint
  lookup = HashMap.lookup
  insert = HashMap.insert
  update = HashMap.insertWith (<>)
  delete = HashMap.delete
  union = HashMap.unionWith (<>)
  restrictKeys = HashMap.intersectionWith (\x _ -> x)
  partition f m = let
    go k v
      | f k v = (Endo $ (:) (k, v), mempty)
      | otherwise = (mempty, Endo $ (:) (k, v))
    (left_endo, right_endo) = HashMap.foldMapWithKey go m
    in (HashMap.fromList $ appEndo left_endo [], HashMap.fromList $ appEndo right_endo [])

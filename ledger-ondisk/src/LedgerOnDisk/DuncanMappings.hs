{-# LANGUAGE RankNTypes #-}
-- |

{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
module LedgerOnDisk.DuncanMappings where

import Data.Kind
import LedgerOnDisk.Mapping.PTMap
import Data.Functor.Identity
class HasMappings (state :: (* -> * -> *) -> *) where
  type StateMappingKeyConstraint   state :: Type -> Constraint
  type StateMappingValueConstraint state :: Type -> Constraint

  traverseMappings :: Applicative f
                   => (forall k v. Ord k
                                => StateMappingKeyConstraint state k
                                => StateMappingValueConstraint state v
                                => map k v -> f (map' k v))
                   -> state map
                   -> f (state map')

class HasMappings state => HasOnlyMappings state where
  traverse0Mappings :: Applicative f
                       => (forall k v. Ord k
                                    => StateMappingKeyConstraint state k
                                    => StateMappingValueConstraint state v
                                    => f (map k v))
                       -> f (state map)

  traverse2Mappings :: Applicative f
                    => (forall k v. Ord k
                                 => StateMappingKeyConstraint state k
                                 => StateMappingValueConstraint state v
                                 => map k v -> map' k v -> f (map'' k v))
                    -> state map -> state map' -> f (state map'')

zipMappings :: HasOnlyMappings state
  => (forall k v. (Ord k, StateMappingKeyConstraint state k, StateMappingValueConstraint state v)
       => map1 k v -> map2 k v -> map3 k v
     )
  -> state map1 -> state map2 -> state map3
zipMappings f m1 m2 = runIdentity $ traverse2Mappings (\n1 n2 -> pure $ f n1 n2) m1 m2

constMappings :: HasOnlyMappings state
  => (forall k v. (Ord k, StateMappingKeyConstraint state k, StateMappingValueConstraint state v) => map k v)
  -> state map
constMappings f = runIdentity $ traverse0Mappings (pure f)

class (HasMappings state, HasOnlyMappings (OnDiskMappings state))
   => HasOnDiskMappings state where

  data OnDiskMappings state :: (* -> * -> *) -> *

  projectOnDiskMappings :: state map -> OnDiskMappings state map
  injectOnDiskMappings  :: OnDiskMappings state map -> state any -> state map

-- class MonoidialMapping map where
--     memptyMap   :: Ord k => map k v
--     mappendMaps :: Ord k => map k v -> map k v -> map k v

-- instance MonoidialMapping DiffMap where
--     memptyMap   = mempty
--     mappendMaps = (<>)

-- instance (HasOnlyMappings (OnDiskMappings state), MonoidialMapping map)
--       => Semigroup (OnDiskMappings state map) where
--    (<>) = zipMappings mappendMaps

-- instance (HasOnlyMappings (OnDiskMappings state), MonoidialMapping map)
--       => Monoid (OnDiskMappings state map) where
--    mempty = constMappings memptyMap


-------------------------------------

type KVConstraint = Type -> Type -> Constraint

class FunctorMappings (c :: KVConstraint) state where
  mapMappings :: ()
    => proxy c
    -> (forall k v. c k v => map1 k v -> map2 k v)
    -> state map1 -> state map2

class FunctorMappings c state => ApplicativeMappings (c :: KVConstraint) state where
  pureMappings :: Applicative f
    => proxy c
    -> (forall k v. c k v => f (map k v))
    -> f (state map)
  apMappings :: Applicative f
    => proxy c
    -> (forall k v. c k v => f (map1 k v) -> f (map2 k v) -> f (map3 k v))
    -> f (state map1) -> f (state map2) -> f (state map3)



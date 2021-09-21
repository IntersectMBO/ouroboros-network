{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}
-- |

module LedgerOnDisk.V2.Snapshot where

import Control.Monad.ST

import Control.Monad.State.Strict

import LedgerOnDisk.V2.OnDiskMappings
import LedgerOnDisk.V2.Diff
import Data.STRef.Strict
import Data.Coerce

data MappingSnapshotOps (state :: StateKind MapFlavour) (map1 :: MapKind MapFlavour) (map2 :: MapKind MapFlavour) = MappingSnapshotOps
  { snapshotMappings :: forall t k v. map1 t k v -> map2 'MK_RO k v
  , keepMapping :: forall t k v. map2 t k v
  , relabelMappings :: forall k v. map1 'MK_RO k v -> map2 'MK_RO k v
  }

data TaggedMap tag map t k v = TaggedMap tag


-- data FreeSnapshot t k v where
--   CopyFrom :: Int -> FreeSnapshot  map 'MK_RO k v
--   Keep :: FreeSnapshot  map t k v

-- instance Semigroup (FreeSnapshot t k v) where
--   CopyFrom _ <> CopyFrom y = CopyFrom y
--   Keep <> CopyFrom y = CopyFrom y
--   CopyFrom y <> Keep = CopyFrom y


mappingsWithTags :: forall ki (state :: StateKind ki) (map :: MapKind ki).
  HasOnDiskMappings state
  => OnDiskMappings state map -> OnDiskMappings state ((ConstMap Int `ProductMap` map))
mappingsWithTags = flip evalState 0 . traverseOnDiskMappings go
  where
    go m = do
      i <- get
      modify' (+1)
      pure (ProductMap (ConstMap i) m)

-- data SnapshotRef s map t k v where
--   SnapshotRef :: STRef s (map t k v) -> SnapshotRef s map t k v
--   SnapshotRefRO :: STRef s (map t k v) -> SnapshotRef s map 'MK_RO k v

data SnapshotFrom s map t k v where
  ROSnapshot :: map t k v -> SnapshotFrom s map 'MK_RO k v
  -- RORelabel :: STRef s (map 'MK_RO k v) -> SnapshotFrom s map 'MK_RO k v
  Keep :: SnapshotFrom s map t k v

swizzleSnapshotRefs :: forall (state :: StateKind MapFlavour) (map :: MapKind MapFlavour) s.
  HasOnDiskMappings  state
  => SnapshotInstructions state
  -> OnDiskMappings state map -> OnDiskMappings state map
swizzleSnapshotRefs (SnapshotInstructions snapshot) x = zipOnDiskMappings go (snapshot ops x) x
  where
    ops = MappingSnapshotOps
      { snapshotMappings = ROSnapshot
      , keepMapping = Keep
      , relabelMappings = ROSnapshot
      }
    go :: forall t k v. SnapshotFrom s map t k v -> map t k v -> map t k v
    go Keep = id
    go (ROSnapshot r) =  const r

-- data MapOrSnapshot state map t k v where
--   MMap :: map t k v -> MapOrSnapshot state map t k v
--   MSnapshot :: (OnDiskMappings state map -> map t k v) -> MapOrSnapshot state map 'MK_RO k v

-- initSnapshotRef :: OnDiskMappings state (MapOrSnapshot state map) -> ST s (OnDiskMappings state (SnapshotRef s map))
-- initSnapshotRef x = traverseOnDiskMappings go (x

-- finishShapshotRef :: forall state map s. OnDiskMappings state (MapOrSnapshot map) -> OnDiskMappings state (SnapshotFrom s map) -> ST s (OnDiskMappings state (MapOrSnapshot map))
-- finishShapshotRef orig swizzled = zipAOnDiskMappings go orig swizzled
--   where
--     go :: forall t k v. MapOrSnapshot map t k v -> SnapshotFrom s map t k v -> ST s (MapOrSnapshot map t k v)
--     go x = \case
--       ROSnapshot r -> MSnapshot <$> readSTRef r
--       RORelabel r -> MMap <$> readSTRef r
--       Keep -> pure x

-- tagMap :: OnDiskMappings state map -> OnDiskMappings state (ConstMap int)
-- tagMap m = flip runState 0 $ traverseOnDiskMappings go m
--   where
--     go _ = do
--       i <- get
--       modify' (+1)
--       pure (ConstMap i)

-- runFreeSnapshot :: OnDiskMappings state FreeSnapshot -> OnDiskMappings state map -> OnDiskMappings state map
-- runFreeSnapshot x y = zipOnDiskMappings go x (mappingsWithTags y)
--   where
--     go (CopyFrom i)k


-- swizzle :: SnapshotInstructions state -> OnDiskMappings state (MapOrSnapshot state map) -> OnDiskMappings state (MapOrSnapshot state map)
-- swizzle (SnapshotInstructions snapshot) x = snapshot ops x
--   where
--     ops = MappingSnapshotOps
--       { snapshotMappings = \case
--           MMap m ->


--       , keepMapping = MaybeMap Nothing
--       , relabelMappings = coerce . Just
--       }
--   i <- initSnapshotRef x
--   let y = snapshot ops i
--   finishSnapshotRef y




-- freeROMapOps = MappingSnapshotOps state (TaggedMap map) (FreeROMap map)
-- freeROMapOps = MappingSnapshotOps
--   { snapshotMappings = \case
--       TaggedMap tag m -> FreeAsRO tag m
--   , keepMappings = FreeAsIs
  -- }
-- data TaggedSnapshotOpMap map tag t k v where
--   SM_Snapshot :: tag -> map t k v -> TaggedSnapshotOpMap  map 'MK_RO k v
--   SM_Keep :: tag -> TaggedSnapshotOpMap map t k v
--   SM_Relabel :: tag -> map 'MK_RO k v -> TaggedSnapshotOpMap 'MK_RO k v

-- labelOnDiskMappings :: OnDiskMappings state (ConstMap Int)
-- labelOnDiskMappings = flip runState 0 $ traverseOnDiskMappings go nullMappings
--   where
--     go :: NullMap t k v-> State Int (ConstMap Int t k v)
--     go _ = do
--       i <- get
--       modify (+1)
--       pure $ ConstMap i


-- swizzleTags :: MappingSnapshotOps state (TaggedMap map) (TaggedMap map)
-- swizzleTags = MappingSnapshotOps
--   { snapshotMappings = \case
--       TaggedMap i m -> TaggedMap i m
--   , keepMapping = id
--   , relabelMappings = id
--   }

-- snapshotWithTags :: SnapshotInstructions state -> OnDiskMappings state map -> OnDiskMappings state (TaggedSnapshotOpMap map)
-- snapshotWithTags (SnapshotInstructions snapshot) m = snapshot ops (tagMap m)
--   where
--     ops = MappingSnapshotOps
--       { snapshotMappings = \case
--           TaggedMap i n -> SM_Snapshot i n
--       , keepMapping = \case
--           TaggedMap i _ -> SM_Keep i
--       , relabelMappings = \case
--           TaggedMap i m -> SM_Relabel i m
--       }

newtype SnapshotInstructions state where
  SnapshotInstructions :: (forall map1 map2. MappingSnapshotOps state map1 map2 -> OnDiskMappings state map1 -> OnDiskMappings state map2) -> SnapshotInstructions state

-- instance Semigroup (SnapshotInstructions (state :: StateKind MapFlavour)) where
--   SnapshotInstructions x <> SnapshotInstructions y = SnapshotInstructions z
--     where
--       z :: forall map1 map2 state. MappingSnapshotOps state map1 map2 -> OnDiskMappings state map1 -> OnDiskMappings state map2
--       z ops = x' . y swizzleTags . tagMap
--         where
--           x0 :: OnDiskMappings state (TaggedSnapshotOpMap map1) -> OnDiskMappings state map2
--           x0 = x MappingSnapshotOps
--             { snapshotMappings  = \case
--                 TaggedMap i m ->
--             , keepMapping = \case
--                 SM_AsRO m -> snapshotMappings ops m
--                 SM_AsIs m -> keepMapping ops m
--             , relabelMappings = \case
--                 SM_AsRO m -> snapshotMappings m

--             }

--           y0 :: OnDiskMappings state map1 -> OnDiskMappings state (SimpleMap map2)
--           y0 = y simpleMappingSnapshotOps

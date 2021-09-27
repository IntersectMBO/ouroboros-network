{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuantifiedConstraints #-}
-- |

module LedgerOnDisk.V2.Snapshot where

import Control.Monad.ST

import Control.Monad.State.Strict

import LedgerOnDisk.V2.OnDiskMappings
import LedgerOnDisk.V2.Diff
import Data.STRef.Strict
import Data.Coerce
import Data.Monoid
import qualified Data.Semigroup as Semi
import Test.QuickCheck
import GHC.Generics
import Data.Kind
import Data.Proxy
import Control.Lens
import Data.SOP
import Data.Foldable


-- TODO hide constructor, it's not safe, as demonstrated by semigroup instance
newtype SnapshotInstructions state where
  SnapshotInstructions :: (forall map. OnDiskMappings state map -> OnDiskMappings state (SnapshotOfTable map)) -> SnapshotInstructions state

data SnapshotOps map1 map2 = SnapshotOps
  { snapshotOfTable :: forall t k v. map1 t k v -> map2 'MK_RO k v
  , keepTable ::       forall t k v.               map2 t      k v
  }

takeSnapshot :: forall state. (forall map1 map2. SnapshotOps map1 map2 -> OnDiskMappings state map1 -> OnDiskMappings state map2) -> SnapshotInstructions state
takeSnapshot f = SnapshotInstructions $ f SnapshotOps { snapshotOfTable = SnapshotOfTable, keepTable = KeepTable }


runSnapshotInstructions :: forall (state :: StateKind MapFlavour) map1 map2. HasOnDiskMappings  state
  => (forall (t :: MapFlavour) k v. map1 t k v -> map2 'MK_RO k v)
  -> (forall (t :: MapFlavour) k v. map1 t k v -> map2 t k v)
  -> SnapshotInstructions state
  -> OnDiskMappings state map1
  -> OnDiskMappings state map2
runSnapshotInstructions snapshot keep (SnapshotInstructions si) x = zipOnDiskMappings go x (si x)  where
  go :: forall (t :: MapFlavour) k v. map1 t k v -> SnapshotOfTable map1 t k v -> map2 t k v
  go orig KeepTable = keep orig
  go _ (SnapshotOfTable t) = snapshot t

applySnapshotInstructionsOdm :: forall (state :: StateKind MapFlavour).HasOnDiskMappings state => SnapshotInstructions state -> OnDiskMappings state DataMap -> OnDiskMappings state DataMap
applySnapshotInstructionsOdm (SnapshotInstructions f) x = zipOnDiskMappings go x swizzled where
  swizzled = f x
  go :: DataMap t k v -> SnapshotOfTable DataMap t k v -> DataMap t k v
  go m1 = \case
    KeepTable -> m1
    SnapshotOfTable (DataMap m) -> DataMap m

applySnapshotInstructions :: HasOnDiskMappings state => SnapshotInstructions state -> state DataMap -> state DataMap
applySnapshotInstructions si x = x & onDiskMappingsLens %~ applySnapshotInstructionsOdm si

data SnapshotOfTable map t k v  where
  KeepTable :: SnapshotOfTable map t k v
  SnapshotOfTable :: map t k v -> SnapshotOfTable map 'MK_RO k v

data UnsafeSnapshotOfTable map t k v where
  UnsafeKeepTable :: map t k v -> UnsafeSnapshotOfTable  map t k v
  UnsafeSnapshotOfTable :: map t k v -> UnsafeSnapshotOfTable  map 'MK_RO k v


instance HasOnDiskMappings state => Semigroup (SnapshotInstructions state) where
   SnapshotInstructions g <> SnapshotInstructions f = SnapshotInstructions z
     where
       z :: forall map. OnDiskMappings state map -> OnDiskMappings state (SnapshotOfTable map)
       z ts = zipOnDiskMappings fixupg (g ts') ts'
         where
           ts' = zipOnDiskMappings fixupf (f ts) ts

       fixupg :: SnapshotOfTable (UnsafeSnapshotOfTable map) t k v -> UnsafeSnapshotOfTable map t k v -> SnapshotOfTable map t k v
       fixupg KeepTable (UnsafeKeepTable _) = KeepTable
       fixupg KeepTable (UnsafeSnapshotOfTable t) = SnapshotOfTable t
       fixupg (SnapshotOfTable (UnsafeKeepTable t)) _ = SnapshotOfTable t
       fixupg (SnapshotOfTable (UnsafeSnapshotOfTable t)) _ = SnapshotOfTable t

       fixupf :: SnapshotOfTable map t k v -> map t k v -> UnsafeSnapshotOfTable map t k v
       fixupf KeepTable x = UnsafeKeepTable x
       fixupf (SnapshotOfTable t) _ = UnsafeSnapshotOfTable t

instance HasOnDiskMappings state => Monoid (SnapshotInstructions state) where
  mempty = SnapshotInstructions $ const $ pureOnDiskMappings KeepTable

data ExampleState (map :: TableKind MapFlavour) = ExampleState
  { normal1 :: map 'MK_RW Int Int
  , normal2 :: map 'MK_RW Int Int
  , snapshot1 ::  map 'MK_RO Int Int
  , snapshot2 ::  map 'MK_RO Int Int
  } deriving stock (Generic)

instance HasOnDiskMappings ExampleState where
  -- newtype TableTag MapFlavour t k v = ExampleStateTag (MapTag t)
  -- tableTags = ExampleOdm (ExampleStateTag MapTagRW) (ExampleStateTag MapTagRW) (ExampleStateTag MapTagRO) (ExampleStateTag MapTagRO)

  type OnDiskMappingsTypes ExampleState =
    '[ '( 'MK_RW, Int, Int)
     , '( 'MK_RW, Int, Int)
     , '( 'MK_RO, Int, Int)
     , '( 'MK_RO, Int, Int)
     ]

  onDiskMappingsLens  = lens get_odm set_odm where
    get_odm ExampleState{..} = ExampleOdm normal1 normal2 snapshot1 snapshot2
    set_odm :: ExampleState map1 -> OnDiskMappings ExampleState map2 -> ExampleState map2
    set_odm ExampleState{} (ExampleOdm normal1 normal2 snapshot1 snapshot2) = ExampleState{..}

pattern ExampleOdm ::
  forall map. (HasOnDiskMappings ExampleState)
  => (HasOnDiskMappings ExampleState)
  => map 'MK_RW Int Int
  -> map 'MK_RW Int Int
  -> map 'MK_RO Int Int
  -> map 'MK_RO Int Int
  -> OnDiskMappings ExampleState map
pattern ExampleOdm n1 n2 s1 s2 = OnDiskMappings (CurryMap n1 :* CurryMap n2 :* CurryMap s1 :* CurryMap s2 :* Nil)

{-# COMPLETE ExampleOdm #-}

deriving stock instance Eq (ExampleState DataMap)
deriving stock instance Show (ExampleState DataMap)

instance Arbitrary (ExampleState DataMap) where
  arbitrary = ExampleState <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink

-- class CoArbitrary (map k t v) => CoArbitraryMap (map :: MapFlavour -> Type -> Type -> Type) k t v
-- instance CoArbitrary (map k t v) => CoArbitraryMap (map :: MapFlavour -> Type -> Type -> Type) k t v

-- class Function (map k t v) => FunctionMap (map :: MapFlavour -> Type -> Type -> Type) k t v
-- instance Function (map k t v) => FunctionMap (map :: MapFlavour -> Type -> Type -> Type) k t v

-- instance AllMap (CoArbitraryMap map) ExampleState => CoArbitrary (OnDiskMappings ExampleState map) where
--     coarbitrary odm = appEndo $ foldMapcOnDiskMappings (Proxy :: Proxy (CoArbitraryMap map)) (Endo . coarbitrary) odm

-- instance (AllMap (FunctionMap map) ExampleState) => Function (OnDiskMappings ExampleState map) where
--   function = functionMap to_tuple from_tuple where
--     to_tuple ::  OnDiskMappings ExampleState map -> (map 'MK_RW Int Int, map 'MK_RW Int Int, map 'MK_RO Int Int, map 'MK_RO Int Int)
--     to_tuple (ExampleOdm n1 n2 s1 s2) = (n1, n2, s1, s2)
--     from_tuple (n1, n2, s1, s2) = ExampleOdm n1 n2 s1 s2

data SnapshotBasis = Keep | SwapSnapshots | S1_N1 | S1_N2 | S1_S2 | S2_N1 | S2_N2 | S2_S1 | S1_Self | S2_Self
  deriving stock (Eq, Show, Ord, Bounded, Enum, Generic)

instance Arbitrary SnapshotBasis where
  shrink = genericShrink
  arbitrary = oneof . fmap pure $ [minBound .. maxBound]

newtype ArbitrarySnapshotInstructions = ArbitrarySnapshotInstructions [SnapshotBasis]
  deriving newtype (Eq, Show, Arbitrary)

runArbitrarySnapshotInstructions :: ArbitrarySnapshotInstructions -> SnapshotInstructions ExampleState
runArbitrarySnapshotInstructions (ArbitrarySnapshotInstructions ss) = foldMap go_ ss where
  go_ = \case
    Keep -> SnapshotInstructions $ go where
        go :: forall map. OnDiskMappings ExampleState map -> OnDiskMappings ExampleState (SnapshotOfTable map)
        go ExampleOdm {} = ExampleOdm KeepTable KeepTable KeepTable KeepTable
    SwapSnapshots -> SnapshotInstructions $ go where
        go :: forall map. OnDiskMappings ExampleState map -> OnDiskMappings ExampleState (SnapshotOfTable map)
        go (ExampleOdm _ _ s1 s2) = ExampleOdm KeepTable KeepTable (SnapshotOfTable s2) (SnapshotOfTable s1)
    S1_N1 -> SnapshotInstructions $ go where
        go :: forall map. OnDiskMappings ExampleState map -> OnDiskMappings ExampleState (SnapshotOfTable map)
        go (ExampleOdm n1 _ _ _) = ExampleOdm KeepTable KeepTable (SnapshotOfTable n1) KeepTable
    S1_N2 -> SnapshotInstructions $ go where
        go :: forall map. OnDiskMappings ExampleState map -> OnDiskMappings ExampleState (SnapshotOfTable map)
        go (ExampleOdm _ n2 _ _) = ExampleOdm KeepTable KeepTable (SnapshotOfTable n2) KeepTable
    S1_S2 -> SnapshotInstructions $ go where
        go :: forall map. OnDiskMappings ExampleState map -> OnDiskMappings ExampleState (SnapshotOfTable map)
        go (ExampleOdm _ _ _ s2) = ExampleOdm KeepTable KeepTable (SnapshotOfTable s2) KeepTable
    S2_N1 -> SnapshotInstructions $ go where
        go :: forall map. OnDiskMappings ExampleState map -> OnDiskMappings ExampleState (SnapshotOfTable map)
        go (ExampleOdm n1 _ _ _) = ExampleOdm KeepTable KeepTable KeepTable (SnapshotOfTable n1)
    S2_N2 -> SnapshotInstructions $ go where
        go :: forall map. OnDiskMappings ExampleState map -> OnDiskMappings ExampleState (SnapshotOfTable map)
        go (ExampleOdm _ n2 _ _) = ExampleOdm KeepTable KeepTable KeepTable (SnapshotOfTable n2)
    S2_S1 -> SnapshotInstructions $ go where
        go :: forall map. OnDiskMappings ExampleState map -> OnDiskMappings ExampleState (SnapshotOfTable map)
        go (ExampleOdm _ _ s1 _) = ExampleOdm KeepTable KeepTable KeepTable (SnapshotOfTable s1)
    S1_Self -> SnapshotInstructions $ go where
        go :: forall map. OnDiskMappings ExampleState map -> OnDiskMappings ExampleState (SnapshotOfTable map)
        go (ExampleOdm _ _ s1 _) = ExampleOdm KeepTable KeepTable (SnapshotOfTable s1) KeepTable
    S2_Self -> SnapshotInstructions $ go where
        go :: forall map. OnDiskMappings ExampleState map -> OnDiskMappings ExampleState (SnapshotOfTable map)
        go (ExampleOdm _ _ _ s2) = ExampleOdm KeepTable KeepTable KeepTable (SnapshotOfTable s2)

prop_SnapshotInstruction_Semigroup_associative ::
  ExampleState DataMap
  -> ArbitrarySnapshotInstructions
  -> ArbitrarySnapshotInstructions
  -> ArbitrarySnapshotInstructions
  -> Property
prop_SnapshotInstruction_Semigroup_associative s (runArbitrarySnapshotInstructions -> s1) (runArbitrarySnapshotInstructions -> s2) (runArbitrarySnapshotInstructions -> s3) =
  applySnapshotInstructions ((s1 <> s2) <> s3) s === applySnapshotInstructions (s1 <> (s2 <> s3)) s

prop_SnapshotInstruction_Semigroup_identity ::
  ExampleState DataMap
  -> ArbitrarySnapshotInstructions
  -> Property
prop_SnapshotInstruction_Semigroup_identity s (runArbitrarySnapshotInstructions -> si) =
  applySnapshotInstructions (si <> mempty) s === applySnapshotInstructions si s
  .&&.
  applySnapshotInstructions (mempty <> si) s === applySnapshotInstructions si s

{-
prop> prop_SnapshotInstruction_Semigroup_associative
+++ OK, passed 100 tests.


prop> prop_SnapshotInstruction_Semigroup_identity
+++ OK, passed 100 tests.
-}


-- midswizzleOps :: forall map1 map2. MappingSnapshotOps map1 map2 -> MappingSnapshotOps map1 (MidSwizzle map1 map2)
-- midswizzleOps MappingSnapshotOps{..} = MappingSnapshotOps
--   { snapshotMappings = sm
--   , keepMapping = km
--   }
--   where
--     sm :: forall t k v. map1 t k v -> MidSwizzle map1 map2 'MK_RO k v
--     sm m = TakeSnapshot $ snapshotMappings m
--     km :: forall t k v.  MidSwizzle map1 map2 t k v
--     km = Keep keepMapping

-- midswizzleOps :: Mappings

-- runMidswizzle :: OnDiskMappings state (MidSwizzle map map) -> OnDiskMappings state map -> OnDiskMappings state map
-- runMidswizzle = zip3OnDiskMappings go where
--   go msm m0 = case msm of
--     TakeSnapshot (Semi.Last (SomeMap m)) -> _


-- data SomeMap map k v = forall t. SomeMap (map t k v)

-- data MidSwizzle map1 map2 t k v where
--   TakeSnapshot :: map2 'MK_RO k v -> MidSwizzle map1 map2 'MK_RO k v
--   Keep :: map2 t k v -> MidSwizzle map1 map2 t k v

-- instance Semigroup (MidSwizzle map t k v) where
--   Keep _ <> Keep y = Keep  y
--   Keep x <> TakeSnapshot y = Keep $ y (SomeMap x)
--   TakeSnapshot x <> Keep y =  Keep y
--   x <> Keep = x
--   TakeSnapshot x <> TakeSnapshot y = TakeSnapshot (y <> x)

-- instance Monoid (MidSwizzle map t k v) where
--   mempty = Keep

-- data FreeSnapshot t k v where
--   CopyFrom :: Int -> FreeSnapshot  map 'MK_RO k v
--   Keep :: FreeSnapshot  map t k v

-- instance Semigroup (FreeSnapshot t k v) where
--   CopyFrom _ <> CopyFrom y = CopyFrom y
--   Keep <> CopyFrom y = CopyFrom y
--   CopyFrom y <> Keep = CopyFrom y


-- mappingsWithTags :: forall ki (state :: StateKind ki) (map :: TableKind ki).
--   HasOnDiskMappings state
--   => OnDiskMappings state map -> OnDiskMappings state (ConstMap Int `ProductMap` map)
-- mappingsWithTags = flip evalState 0 . traverseOnDiskMappings go
--   where
--     go m = do
--       i <- get
--       modify' (+1)
--       pure (ProductMap (ConstMap i) m)

-- data SnapshotRef s map t k v where
--   SnapshotRef :: STRef s (map t k v) -> SnapshotRef s map t k v
--   SnapshotRefRO :: STRef s (map t k v) -> SnapshotRef s map 'MK_RO k v

-- data SnapshotFrom s map t k v where
--   ROSnapshot :: map t k v -> SnapshotFrom s map 'MK_RO k v
--   -- RORelabel :: STRef s (map 'MK_RO k v) -> SnapshotFrom s map 'MK_RO k v
--   Keep :: SnapshotFrom s map t k v

-- swizzleSnapshotRefs :: forall (state :: StateKind MapFlavour) (map :: TableKind MapFlavour) s.
--   HasOnDiskMappings  state
--   => SnapshotInstructions state
--   -> OnDiskMappings state map -> OnDiskMappings state map
-- swizzleSnapshotRefs (SnapshotInstructions snapshot) x = zipOnDiskMappings go (snapshot ops x) x
--   where
--     ops = MappingSnapshotOps
--       { snapshotMappings = ROSnapshot
--       , keepMapping = Keep
--       , relabelMappings = ROSnapshot
--       }
--     go :: forall t k v. SnapshotFrom s map t k v -> map t k v -> map t k v
--     go Keep = id
--     go (ROSnapshot r) =  const r

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

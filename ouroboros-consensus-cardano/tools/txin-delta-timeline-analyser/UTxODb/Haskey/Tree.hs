{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE InstanceSigs #-}
-- |

module UTxODb.Haskey.Tree where

import qualified Database.Haskey.Alloc.Concurrent
import qualified Data.BTree.Primitives as Haskey
import qualified Data.BTree.Impure as Haskey
import qualified Database.Haskey.Alloc.Concurrent as Haskey
import Data.Binary
import Type.Reflection
import Data.Int
import Data.Word
import Data.Kind
import Data.Proxy
import Data.Monoid
import GHC.Generics (Generic)
import Text.Show (showListWith)

import UTxODb.Snapshots

data OnDiskTable (t :: TableType) k v where
  RW_ODT :: (Haskey.Key k, Haskey.Value v) => Haskey.Tree k v -> OnDiskTable TableTypeRW k v
  RWU_ODT :: (Haskey.Key k, Haskey.Value v, Semigroup v) => Haskey.Tree k v -> OnDiskTable TableTypeRWU k v
  RO_ODT :: (Haskey.Key k, Haskey.Value v) => Haskey.Tree k v -> OnDiskTable TableTypeRO k v

deriving stock instance Show (OnDiskTable t k v)

instance (HasTableTag t v, Haskey.Key k, Haskey.Value v) => Binary (OnDiskTable t k v) where
  put x = case (tableTag :: TableTag t v, x) of
    (TableTagRO, RO_ODT t) -> put t
    (TableTagRW, RW_ODT t) -> put t
    (TableTagRWU, RWU_ODT t) -> put t
  get = case tableTag :: TableTag t v of
    TableTagRO -> RO_ODT <$> get
    TableTagRW -> RW_ODT <$> get 
    TableTagRWU -> RWU_ODT <$> get

instance (Typeable k, Typeable v, Typeable t, Binary (OnDiskTable t k v)) => Haskey.Value (OnDiskTable t k v) where
  fixedSize _ = Nothing

data E t = E
  { t1 :: t TableTypeRW Int64 Int64
  , t2 :: t TableTypeRWU (Sum Word64) (Sum Word64)
  , t3 :: t TableTypeRO Int64 Int64
  , t4 :: Int
  }

deriving newtype instance Haskey.Key a => Haskey.Key (Sum a)
deriving newtype instance Haskey.Value a => Haskey.Value (Sum a)

class (Haskey.Key k, StateTableKeyConstraint state k) => HaskeyKeyConstraint state k
instance (Haskey.Key k, StateTableKeyConstraint state k) => HaskeyKeyConstraint state k

class (Haskey.Value v, StateTableValueConstraint state v) => HaskeyValueConstraint state v
instance (Haskey.Value v, StateTableValueConstraint state v) => HaskeyValueConstraint state v

class (Haskey.Key k, Haskey.Value v) => HaskeyKVConstraint k v
instance (Haskey.Key k, Haskey.Value v) => HaskeyKVConstraint k v

class (Typeable state, HasOnDiskTables state) => HasHaskeyOnDiskTables state where
  haskeyTraverseTables :: Applicative f
                   => (forall (t :: TableType) k v.
                              Ord k
                           => HaskeyKeyConstraint state k
                           => HaskeyValueConstraint state v
                           => TableTag t v -> table t k v -> f (table' t k v))
                   -> state table -> f (state table')


  haskeyTraverse0Tables :: Applicative f
                  => (forall (t :: TableType) k v.
                             Ord k
                          => HaskeyKeyConstraint state k
                          => HaskeyValueConstraint state v
                          => TableTag t v -> f (table t k v))
                  -> f (Tables state table)

  haskeyTraverse1Tables :: Applicative f
                   => (forall (t :: TableType) k v.
                              Ord k
                           => HaskeyKeyConstraint state k
                           => HaskeyValueConstraint state v
                           => TableTag t v -> table t k v -> f (table' t k v))
                   -> Tables state table -> f (Tables state table')

  haskeyTraverse2Tables :: Applicative f
                  => (forall (t :: TableType) k v.
                             Ord k
                          => HaskeyKeyConstraint state k
                          => HaskeyValueConstraint state v
                          => TableTag t v -> table t k v -> table' t k v -> f (table'' t k v))
                  -> Tables state table -> Tables state table' -> f (Tables state table'')

newtype HaskeyState state table = HaskeyState (state table)
  deriving stock Show

instance HasHaskeyOnDiskTables state => HasHaskeyOnDiskTables (HaskeyState state) where
  haskeyTraverseTables f (HaskeyState s) = HaskeyState <$> haskeyTraverseTables f s
  haskeyTraverse0Tables f  = HaskeyTables <$> haskeyTraverse0Tables f
  haskeyTraverse1Tables f (HaskeyTables t) = HaskeyTables <$> haskeyTraverse1Tables f t
  haskeyTraverse2Tables f (HaskeyTables s1) (HaskeyTables s2) = HaskeyTables <$> haskeyTraverse2Tables f s1 s2

instance HasHaskeyOnDiskTables state => HasTables (HaskeyState state) where
  type StateTableKeyConstraint (HaskeyState state) = HaskeyKeyConstraint state
  type StateTableValueConstraint (HaskeyState state) = HaskeyValueConstraint state

  traverseTables = haskeyTraverseTables

instance HasHaskeyOnDiskTables state => HasOnDiskTables (HaskeyState state) where
  newtype Tables (HaskeyState state) table = HaskeyTables (Tables state table)

  projectTables (HaskeyState s) = HaskeyTables (projectTables s)
  injectTables (HaskeyTables t) (HaskeyState s) = HaskeyState (injectTables t s)

instance HasHaskeyOnDiskTables state => Show (Tables (HaskeyState state) OnDiskTable) where
  showsPrec d t = showParen (d > 10) $ shows "Tables OnDiskTable" . showListWith shows table_strings where
    table_strings :: [String]
    table_strings = foldMapTables go t where
      go :: TableTag t v -> OnDiskTable t k v -> [String]
      go _tag odt = [show odt]

instance HasHaskeyOnDiskTables state => HasTables (Tables (HaskeyState state)) where
  type StateTableKeyConstraint (Tables (HaskeyState state)) = HaskeyKeyConstraint state
  type StateTableValueConstraint (Tables (HaskeyState state)) = HaskeyValueConstraint state

  traverseTables f (HaskeyTables s) = HaskeyTables <$> haskeyTraverse1Tables f s

instance HasHaskeyOnDiskTables state => HasOnlyTables (Tables (HaskeyState state)) where
  traverse0Tables = haskeyTraverse0Tables
  traverse2Tables = haskeyTraverse2Tables

class Top a
instance Top a

instance HasTables E where
  type StateTableKeyConstraint E = Top
  type StateTableValueConstraint E = Top

  traverseTables f E{t1,t2, t3, t4} = E <$> f TableTagRW t1 <*> f TableTagRWU t2 <*> f TableTagRO t3 <*> pure t4

instance HasOnDiskTables E where
  data Tables E t = OnDiskE
    { od1 :: t TableTypeRW Int64 Int64
    , od2 :: t TableTypeRWU (Sum Word64) (Sum Word64)
    , od3 :: t TableTypeRO Int64 Int64
    }

  projectTables E{..} = OnDiskE{od1 = t1, od2 = t2, od3 = t3}
  injectTables OnDiskE{..} s = s{t1 = od1, t2 = od2, t3 = od3 }

instance HasTables (Tables E) where
  type StateTableKeyConstraint (Tables E) = Top
  type StateTableValueConstraint (Tables E) = Top

  traverseTables f OnDiskE{od1,od2, od3} = OnDiskE <$> f TableTagRW od1 <*> f TableTagRWU od2 <*> f TableTagRO od3

instance HasOnlyTables (Tables E) where
  traverse0Tables f = OnDiskE <$> f TableTagRW <*> f TableTagRWU <*> f TableTagRO
  traverse2Tables f t1 t2 = OnDiskE <$> f TableTagRW (od1 t1) (od1 t2) <*> f TableTagRWU (od2 t1) (od2 t2) <*> f TableTagRO (od3 t1) (od3 t2)

instance HasHaskeyOnDiskTables E where
  haskeyTraverseTables f E{t1,t2, t3, t4} = E <$> f TableTagRW t1 <*> f TableTagRWU t2 <*> f TableTagRO t3 <*> pure t4
  haskeyTraverse0Tables f = OnDiskE <$> f TableTagRW <*> f TableTagRWU <*> f TableTagRO
  haskeyTraverse1Tables f t = OnDiskE <$> f TableTagRW (od1 t) <*> f TableTagRWU (od2 t) <*> f TableTagRO (od3 t)
  haskeyTraverse2Tables f t1 t2 = OnDiskE <$> f TableTagRW (od1 t1) (od1 t2) <*> f TableTagRWU (od2 t1) (od2 t2) <*> f TableTagRO (od3 t1) (od3 t2)

instance HasHaskeyOnDiskTables state => Binary (Tables (HaskeyState state) OnDiskTable) where
  get = traverse0Tables (\t -> withTableTag t get)
  put = traverseTables_ (\t x -> withTableTag t $ put x)

instance ShowTable OnDiskTable where
  showsPrecTable _ d t = showsPrec d t

instance (HasHaskeyOnDiskTables state) => Haskey.Value (Tables (HaskeyState state) OnDiskTable) where

instance (HasHaskeyOnDiskTables state) => Haskey.Root (Tables (HaskeyState state) OnDiskTable)

instance (ShowTable table) => Show (Tables E table) where
  showsPrec d = appEndo . foldMapTables (\tag table -> Endo (showsPrecTable tag d table)) . HaskeyTables

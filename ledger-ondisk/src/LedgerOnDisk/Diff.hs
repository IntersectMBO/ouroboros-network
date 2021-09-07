{-# LANGUAGE LambdaCase #-}
-- |

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RoleAnnotations #-}
module LedgerOnDisk.Diff where

import Data.Monoid
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Test.QuickCheck
import Test.QuickCheck.Instances
import Data.Functor.Identity
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.GADT.Compare
import Data.Coerce
import qualified Data.Semigroup as Semi
import Data.Functor ((<&>))
import qualified Data.Monoid as Monoid
import Data.Map
import qualified Data.Map.Strict as Map

-- | The semigroup dictionary in DMappend is unfortunate.
type role Diff nominal
data Diff v where
  DNoChange :: Diff v
  DRemove :: Diff v
  DChangeTo :: v -> Diff v
  DMappend :: Semigroup v => v -> Diff v


deriving stock instance Eq v => Eq (Diff v)
deriving stock instance Show v => Show (Diff v)

instance Semigroup (Diff v) where
  x <> DNoChange = x
  DMappend x <> DMappend y = DMappend $ x <> y
  DChangeTo x <> DMappend y = DChangeTo $ x <> y
  DRemove <> DMappend y = DChangeTo y
  _ <> y = y

instance Monoid (Diff v) where
  mempty = DNoChange

mapD :: Semigroup b => (a -> b) -> Diff a -> Diff b
mapD f = \case
  DChangeTo x -> DChangeTo $ f x
  DMappend x -> DChangeTo $ f x
  DRemove -> DRemove
  DNoChange -> DNoChange

applyD :: Semigroup v => Maybe v -> Diff v -> Maybe v
applyD x = \case
  DChangeTo v -> Just v
  DMappend v -> Just $ maybe v (<> v) x
  DRemove -> Nothing
  DNoChange -> x

applyDforK :: (Eq k, Hashable k) => k -> Diff v -> HashMap k v -> HashMap k v
applyDforK k = \case
  DChangeTo v -> HashMap.insert k v
  DMappend v -> HashMap.insertWith (<>) k v
  DRemove -> HashMap.delete k
  DNoChange -> id

applyDtoHashMap :: (Eq k, Hashable k) => HashMap k (Diff v) -> HashMap k v -> HashMap k v
applyDtoHashMap d = appEndo (HashMap.foldMapWithKey go d) where
  go k = Endo . applyDforK k

applyDifftoMap :: Ord k => Map k (Diff v) -> Map k v -> Map k v
applyDifftoMap d = appEndo (Map.foldMapWithKey go d) where
  go k = Endo . \case
    DChangeTo v -> Map.insert k v
    DMappend v -> Map.insertWith (<>) k v
    DRemove -> Map.delete k
    DNoChange -> id

shrinkD :: Arbitrary v => Diff v -> [Diff v]
shrinkD = \case
    DRemove -> [DNoChange]
    DChangeTo v -> [DNoChange] ++ (DChangeTo <$> shrink v)
    DNoChange -> []
    DMappend v -> [DNoChange] ++ (DChangeTo <$> shrink v) ++ [DChangeTo v] ++ (DMappend <$> shrink v)


instance (Semigroup v, Arbitrary v) => Arbitrary (Diff v) where
  arbitrary = oneof
    [ pure DNoChange
    , pure DRemove
    , coerce . DChangeTo <$> arbitrary
    , coerce . DMappend <$> arbitrary
    ]
  shrink = shrinkD

arbitraryNonmonoidalD :: Arbitrary v => Gen (Diff v)
arbitraryNonmonoidalD = arbitrary <&> \case
  DMappend (Semi.Last x) -> DChangeTo x
  DChangeTo (Semi.Last x) -> DChangeTo x
  DRemove -> DRemove
  DNoChange -> DNoChange
  
newtype ArbNonmonoidalD v = ArbNonmonoidalD { unArbNonmonoidalD :: Diff v }
  deriving stock (Eq, Show)
  deriving newtype (Semigroup, Monoid)

instance Arbitrary v => Arbitrary (ArbNonmonoidalD v) where
  arbitrary = coerce <$> arbitraryNonmonoidalD
  shrink = coerce shrinkD

-- we demand that domain keys (applyDToHashMaybeMap x y)  == keys y
applyDtoHashMaybeMap :: (Semigroup v, Eq k, Hashable k) => HashMap k (Diff v) -> HashMap k (Maybe v) -> HashMap k (Maybe v)
applyDtoHashMaybeMap d_map = HashMap.mapWithKey $ \k mb_v -> applyD mb_v (HashMap.lookupDefault mempty k d_map)

applyDforKDMap :: GCompare f => f x -> Diff x -> DMap f Identity -> DMap f Identity
applyDforKDMap k = \case
  DChangeTo x -> DMap.insert k (Identity x)
  -- DMappend x -> DMap.insertWith (<>) k (Identity x)
  DRemove -> DMap.delete k
  DNoChange -> id
  DMappend {} -> error "is this possible?"

applyDtoDMap :: GCompare f => DMap f Diff -> DMap f Identity -> DMap f Identity
applyDtoDMap dm_d = appEndo (DMap.foldrWithKey go mempty dm_d)
  where
    go k d e = Endo (applyDforKDMap k d) <> e

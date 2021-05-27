{-# LANGUAGE LambdaCase #-}
-- |

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

data D v where
  DChangeTo :: v -> D v
  DRemove :: D v
  DNoChange :: D v
  DMappend :: Semigroup v => v -> D v

deriving stock instance Eq v => Eq (D v)
deriving stock instance Show v => Show (D v)

instance Semigroup (D v) where
  x <> DNoChange = x
  DMappend x <> DMappend y = DMappend $ x <> y
  DChangeTo x <> DMappend y = DChangeTo $ x <> y
  DRemove <> DMappend y = DChangeTo y
  _ <> y = y

instance Monoid (D v) where
  mempty = DNoChange

mapD :: Monoid b => (a -> b) -> D a -> D b
mapD f = \case
  DChangeTo x -> DChangeTo $ f x
  DMappend x -> DChangeTo $ f x
  DRemove -> DRemove
  DNoChange -> DNoChange

applyD :: Maybe v -> D v -> Maybe v
applyD x = \case
  DChangeTo v -> Just v
  DMappend v -> Just $ maybe v (<> v) x
  DRemove -> Nothing
  DNoChange -> x

applyDforK :: (Eq k, Hashable k) => k -> D v -> HashMap k v -> HashMap k v
applyDforK k = \case
  DChangeTo v -> HashMap.insert k v
  DMappend v -> HashMap.insertWith (<>) k v
  DRemove -> HashMap.delete k
  DNoChange -> id

applyDtoHashMap :: (Eq k, Hashable k) => HashMap k (D v) -> HashMap k v -> HashMap k v
applyDtoHashMap d = appEndo (HashMap.foldMapWithKey go d) where
  go k = Endo . applyDforK k

shrinkD :: Arbitrary v => D v -> [D v]
shrinkD = \case
    DRemove -> [DNoChange]
    DChangeTo v -> [DNoChange] ++ (DChangeTo <$> shrink v)
    DNoChange -> []
    DMappend v -> [DNoChange] ++ (DChangeTo <$> shrink v) ++ [DChangeTo v] ++ (DMappend <$> shrink v)


instance (Semigroup v, Arbitrary v) => Arbitrary (D v) where
  arbitrary = oneof
    [ pure DNoChange
    , pure DRemove
    , coerce . DChangeTo <$> arbitrary
    , coerce . DMappend <$> arbitrary
    ]
  shrink = shrinkD

arbitraryNonmonoidalD :: Arbitrary v => Gen (D v)
arbitraryNonmonoidalD = arbitrary <&> \case
  DMappend (Semi.Last x) -> DChangeTo x
  DChangeTo (Semi.Last x) -> DChangeTo x
  DRemove -> DRemove
  DNoChange -> DNoChange
  
newtype ArbNonmonoidalD v = ArbNonmonoidalD { unArbNonmonoidalD :: D v }
  deriving stock (Eq, Show)
  deriving newtype (Semigroup, Monoid)

instance Arbitrary v => Arbitrary (ArbNonmonoidalD v) where
  arbitrary = coerce <$> arbitraryNonmonoidalD
  shrink = coerce shrinkD

-- we demand that domain keys (applyDToHashMaybeMap x y)  == keys y
applyDtoHashMaybeMap :: (Eq k, Hashable k) => HashMap k (D v) -> HashMap k (Maybe v) -> HashMap k (Maybe v)
applyDtoHashMaybeMap d_map = HashMap.mapWithKey $ \k mb_v -> applyD mb_v (HashMap.lookupDefault mempty k d_map)

applyDforKDMap :: GCompare f => f x -> D x -> DMap f Identity -> DMap f Identity
applyDforKDMap k = \case
  DChangeTo x -> DMap.insert k (Identity x)
  DMappend x -> DMap.insertWith (<>) k (Identity x)
  DRemove -> DMap.delete k
  DNoChange -> id

applyDtoDMap :: GCompare f => DMap f D -> DMap f Identity -> DMap f Identity
applyDtoDMap dm_d = appEndo (DMap.foldrWithKey go mempty dm_d)
  where
    go k d e = Endo (applyDforKDMap k d) <> e

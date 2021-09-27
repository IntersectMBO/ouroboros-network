{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

-- |

module LedgerOnDisk.V2.Diff where

import Test.QuickCheck
import Test.QuickCheck.Classes
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import GHC.Generics
import Data.Proxy
import Data.Functor
import Data.Monoid
import Data.Traversable
import Data.Foldable
import Control.Monad.Trans.Writer.CPS
import Control.Monad.IO.Class
import qualified Data.Map.Merge.Strict as Map
import Data.Set (Set)
import Data.Coerce

data DBDiff t k v

data MapFlavour = MK_RO | MK_RW | MK_RWU

data MapTag t where
  MapTagRO :: MapTag 'MK_RO
  MapTagRW :: MapTag 'MK_RW
  MapTagRWU :: MapTag 'MK_RWU

data Diff v = DNoChange | DRemove | DChangeTo !v
  deriving stock (Eq, Show, Generic)

inspectLawsOne :: Proxy a -> [Proxy a -> Laws] -> IO [String]
inspectLawsOne  p fs = flip appEndo [] . fold <$> for fs (\f -> go (f p))
  where
    tellString s = tell $ Endo (s :)
    go Laws{..} = execWriterT $ do
      tellString lawsTypeclass
      for lawsProperties $ \(name, prop) -> do
        r <- liftIO $ quickCheckResult prop
        tellString $  name <> ": " <> if isSuccess r then "succeeeded" else "FAILED"


instance Semigroup (Diff v) where
  DNoChange <> x = x
  x <> DNoChange = x
  x <> _y = x

instance Monoid (Diff v) where
  mempty = DNoChange

{-
>>> inspectLawsOne (Proxy :: Proxy (Diff Int)) [monoidLaws, semigroupMonoidLaws] >>= error . unlines
Monoid
Associative: succeeeded
Left Identity: succeeeded
Right Identity: succeeeded
Concatenation: succeeeded
Semigroup/Monoid
mappend == <>: succeeeded

-}

data DiffWithMappend v = DWMDiff (Diff v) | DMappend v
  deriving stock (Eq, Show, Generic)

instance Semigroup v => Semigroup (DiffWithMappend v) where
  (DWMDiff x) <> (DWMDiff y) = DWMDiff (x <> y)
  DWMDiff DNoChange <> x@DMappend {} = x
  x@(DWMDiff DRemove) <> DMappend y = x
  x@(DWMDiff DChangeTo {}) <> DMappend {} = x
  DMappend x <> DMappend y = DMappend $ y <> x
  x@DMappend {} <> DWMDiff DNoChange = x
  DMappend x <> DWMDiff (DChangeTo y) = DWMDiff . DChangeTo $ y <> x
  DMappend{} <> DWMDiff DRemove = DWMDiff DRemove

instance Semigroup v => Monoid (DiffWithMappend v) where
  mempty = DWMDiff mempty

{-
prop> \(x :: DiffWithMappend (Last Int)) -> x <> mempty === x .&&. mempty <> x === x
+++ OK, passed 100 tests.
prop> \(x :: DiffWithMappend (Last Int)) y z -> x <> (y <> z) === (x <> y) <> z
+++ OK, passed 100 tests.

>>> inspectLawsOne (Proxy :: Proxy (DiffWithMappend (Last Int))) [monoidLaws, semigroupMonoidLaws] >>= error . unlines
Monoid
Associative: succeeeded
Left Identity: succeeeded
Right Identity: succeeeded
Concatenation: succeeeded
Semigroup/Monoid
mappend == <>: succeeeded

-}

data DiffMap0 (t :: MapFlavour) k v where
  DiffMapRWU :: (Ord k, Semigroup v) => Map k (DiffWithMappend v) -> DiffMap0 'MK_RWU k v
  DiffMapRW :: Ord k => Map k (Diff v) -> DiffMap0 'MK_RW k v
  DiffMapRO :: DiffMap0 'MK_RO k v

deriving stock instance(Eq k, Eq v) => Eq (DiffMap0 t k v)
deriving stock instance(Show k, Show v) => Show (DiffMap0 t k v)

instance Semigroup (DiffMap0 t k v) where
  DiffMapRO <> DiffMapRO = DiffMapRO
  DiffMapRWU x <> DiffMapRWU y = DiffMapRWU $ Map.unionWith (<>) x y
  DiffMapRW x <> DiffMapRW y = DiffMapRW $ Map.unionWith (<>) x y

newtype DiffMap (t :: MapFlavour) k v = DiffMap (Maybe (DiffMap0 t k v))
  deriving stock (Eq, Show)
  deriving newtype (Semigroup, Monoid)

deriving newtype instance (Arbitrary (DiffMap0 t k v)) => Arbitrary (DiffMap t k v)

instance Arbitrary v => Arbitrary (Diff v) where
  shrink = genericShrink
  arbitrary = oneof [ pure DNoChange, pure DRemove, DChangeTo <$> arbitrary ]

instance (Semigroup v, Arbitrary v) => Arbitrary (DiffWithMappend v) where
  shrink = genericShrink
  arbitrary = oneof [ DWMDiff <$> arbitrary, DMappend <$> arbitrary ]

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (DiffMap0 'MK_RW k v) where
  arbitrary = DiffMapRW <$> arbitrary
  shrink = \case
    DiffMapRW m -> DiffMapRW <$> shrink m

instance (Ord k, Arbitrary k, Arbitrary v, Semigroup v) => Arbitrary (DiffMap0 'MK_RWU k v) where
  arbitrary = DiffMapRWU <$> arbitrary
  shrink = \case
    DiffMapRWU m -> DiffMapRWU <$> shrink m

instance Arbitrary (DiffMap0 'MK_RO k v) where
  arbitrary = pure DiffMapRO


restrictDiffMap :: Set k -> DiffMap t k v -> DiffMap t k v
restrictDiffMap s dm = case coerce dm of
  Nothing -> dm
  Just dm0
    | DiffMapRO <- dm0 -> dm
    | DiffMapRW m <- dm0 -> DiffMap . pure . DiffMapRW . Map.restrictKeys m $ s
    | DiffMapRWU m <- dm0 -> DiffMap . pure . DiffMapRWU . Map.restrictKeys m $ s

applyDiffMapToMap :: DiffMap t k v -> Map k v -> Map k v
applyDiffMapToMap dm x = case coerce dm of
  Nothing -> x
  Just dm0
    | DiffMapRO <- dm0 -> x
    | DiffMapRW m <- dm0 -> applyDiffToMap m x
    | DiffMapRWU m <- dm0 -> applyDiffWithMappendToMap m x

applyDiffToValue :: Diff v -> Maybe v -> Maybe v
applyDiffToValue d x = case d of
  DNoChange -> x
  DRemove -> Nothing
  DChangeTo y -> Just y

applyDiffWithMappendToValue :: Semigroup v => DiffWithMappend v -> Maybe v -> Maybe v
applyDiffWithMappendToValue d x = case d of
  DWMDiff d' -> applyDiffToValue d' x
  DMappend v -> (<> v) <$> x

applyDiffToMap :: Ord k => Map k (Diff v) -> Map k v -> Map k v
applyDiffToMap = Map.merge only_diff Map.preserveMissing diff_and_v
  where
    only_diff = Map.mapMaybeMissing $ \_ d -> applyDiffToValue d Nothing
    diff_and_v = Map.zipWithMaybeMatched $ \_ d x -> applyDiffToValue d (Just x)


applyDiffWithMappendToMap :: (Ord k, Semigroup v) => Map k (DiffWithMappend v) -> Map k v -> Map k v
applyDiffWithMappendToMap = Map.merge only_diff Map.preserveMissing diff_and_v
  where
    only_diff = Map.mapMaybeMissing $ \_ d -> applyDiffWithMappendToValue d Nothing
    diff_and_v = Map.zipWithMaybeMatched $ \_ d x -> applyDiffWithMappendToValue d (Just x)

prop_applyDiffMapToMap_is_homomorphism :: DiffMap 'MK_RW Int Int -> DiffMap 'MK_RW Int Int -> Map Int Int -> Property
prop_applyDiffMapToMap_is_homomorphism dm1 dm2 m = applyDiffMapToMap dm1 (applyDiffMapToMap dm2 m)  === applyDiffMapToMap (dm1 <> dm2) m

prop_applyDiffMapToMap_deletes :: Int -> Map Int Int -> Property
prop_applyDiffMapToMap_deletes k m = property $ Map.notMember k (applyDiffMapToMap (coerce . Just . DiffMapRW . Map.singleton k $ DRemove) m)

prop_applyDiffMapToMap_inserts :: Int -> Int -> Map Int Int -> Property
prop_applyDiffMapToMap_inserts k v m = property $ Map.member k (applyDiffMapToMap (coerce . Just . DiffMapRW . Map.singleton k . DChangeTo $ v) m)

{-
prop>  prop_applyDiffMapToMap_is_homomorphism
+++ OK, passed 100 tests.

prop>  prop_applyDiffMapToMap_deletes
+++ OK, passed 100 tests.

prop>  prop_applyDiffMapToMap_inserts
+++ OK, passed 100 tests.

-}

data PTMap (t :: MapFlavour) k v where
  PTMap ::
    { ptMap :: !(Map k v)
    , ptDiff :: !(DiffMap t k v)
    } -> PTMap t k v

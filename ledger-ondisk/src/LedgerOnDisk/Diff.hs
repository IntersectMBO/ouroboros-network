{-# LANGUAGE LambdaCase #-}
-- |

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
module LedgerOnDisk.Diff where

import Data.Monoid
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Test.QuickCheck
import Data.Functor.Identity
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.GADT.Compare

data D v where
  DChangeTo :: v -> D v
  DRemove :: D v
  DNoChange :: D v
  -- this is interesting, but it makes things more complicated:
  -- Inhibits functor instance (though I think this could be surmounted with a CoYoneda trick)
  -- Inhibits idempotency
  -- DIMappend :: Monoid v => v -> D v
  deriving stock (Show, Eq, Functor)

instance Semigroup (D v) where
  x <> DNoChange = x
  _ <> y = y

instance Monoid (D v) where
  mempty = DNoChange

applyD :: Maybe v -> D v -> Maybe v
applyD x = \case
  DChangeTo v -> Just v
  DRemove -> Nothing
  DNoChange -> x

applyDforK :: (Eq k, Hashable k) => k -> D v -> HashMap k v -> HashMap k v
applyDforK k = \case
  DChangeTo v -> HashMap.insert k v
  DRemove -> HashMap.delete k
  DNoChange -> id
  -- DIMappend v -> HashMap.insertWith (<>) k v

applyDtoHashMap :: (Eq k, Hashable k) => HashMap k (D v) -> HashMap k v -> HashMap k v
applyDtoHashMap d = appEndo (HashMap.foldMapWithKey go d) where
  go k = Endo . applyDforK k

instance Arbitrary v => Arbitrary (D v) where
  arbitrary = oneof [ pure DRemove, DChangeTo <$> arbitrary ]
  shrink di = case di of
    DRemove -> [DNoChange]
    DChangeTo v -> (DChangeTo <$> shrink v) ++ [ DNoChange ]
    DNoChange -> []

-- we demand that domain keys (applyDToHashMaybeMap x y)  == keys y
applyDtoHashMaybeMap :: (Eq k, Hashable k) => HashMap k (D v) -> HashMap k (Maybe v) -> HashMap k (Maybe v)
applyDtoHashMaybeMap d_map = HashMap.mapWithKey $ \k mb_v -> applyD mb_v (HashMap.lookupDefault mempty k d_map)

applyDforKDMap :: GCompare f => f x -> D x -> DMap f Identity -> DMap f Identity
applyDforKDMap k = \case
  DChangeTo x -> DMap.insert k (Identity x)
  DRemove -> DMap.delete k
  DNoChange -> id


applyDtoDMap :: GCompare f => DMap f D -> DMap f Identity -> DMap f Identity
applyDtoDMap dm_d = appEndo (DMap.foldrWithKey go mempty dm_d)
  where
    go k d e = Endo (applyDforKDMap k d) <> e

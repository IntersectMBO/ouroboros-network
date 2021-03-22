{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Test.Util.RefEnv (
    RefEnv
    -- opaque
  , elems
  , empty
  , filter
  , fromList
  , keys
  , lookup
  , null
  , reverseLookup
  , singleton
  , toList
  , union
  , (!)
  ) where

import           Prelude hiding (filter, lookup, null)
import qualified Prelude

import           Data.Bifunctor
import           Data.Functor.Classes
import           Data.List (intercalate)
import           Data.TreeDiff (ToExpr)
import           GHC.Generics (Generic)
import           GHC.Stack
import           Test.StateMachine (Reference)
import qualified Test.StateMachine.Types.Rank2 as Rank2

data RefEnv k a r = RefEnv { toList :: [(Reference k r, a)] }
  deriving (Generic, ToExpr, Show)

-- | Extend mapping
--
-- We don't insist that the keys are disjoint, but if the same key appears
-- twice, the value must agree.
extendMapping :: forall k v. (Eq k, Eq v, Show k, Show v, HasCallStack)
              => [(k, v)] -- Mapping known to have duplicate keys
              -> [(k, v)] -- With potential duplicates
              -> [(k, v)]
extendMapping acc []             = acc
extendMapping acc ((k, v) : kvs) =
    case Prelude.lookup k acc of
      Just v' | v /= v' -> error $ renderError v'
      _otherwise        -> extendMapping ((k, v) : acc) kvs
  where
    renderError :: v -> String
    renderError v' = intercalate " " [
          "Key"
        , show k
        , "with two different values"
        , show v
        , "and"
        , show v'
        ]

fromList :: (Eq k, Show k, Eq a, Show a, Eq1 r, Show1 r, HasCallStack)
         => [(Reference k r, a)] -> RefEnv k a r
fromList = RefEnv . extendMapping []

instance Rank2.Functor (RefEnv k a) where
  fmap f (RefEnv ras) = RefEnv $
      fmap (first (Rank2.fmap f)) ras

instance Rank2.Foldable (RefEnv k a) where
  foldMap f (RefEnv ras) =
      foldMap (Rank2.foldMap f . fst) ras

instance Rank2.Traversable (RefEnv k a) where
  traverse f (RefEnv ras) = RefEnv <$>
      traverse (\(r, a) -> (,a) <$> Rank2.traverse f r) ras

union :: (Eq k, Show k, Eq a, Show a, Eq1 r, Show1 r, HasCallStack)
      => RefEnv k a r -> RefEnv k a r -> RefEnv k a r
union (RefEnv ras1) (RefEnv ras2) = RefEnv (extendMapping ras1 ras2)

-- | Empty environment
--
-- In most context 'mempty' can be used, but the 'Monoid' instance requires
-- equality, which 'empty' does not.
empty :: RefEnv k a r
empty = RefEnv []

lookup :: (Eq k, Eq1 r) => Reference k r -> RefEnv k a r -> Maybe a
lookup r (RefEnv ras) = Prelude.lookup r ras

(!) :: (Eq k, Eq1 r) => RefEnv k a r -> Reference k r -> a
env ! r = case lookup r env of
            Just a  -> a
            Nothing -> error "(RefEnv.!): key not found"

keys :: RefEnv k a r -> [Reference k r]
keys (RefEnv ras) = map fst ras

elems :: RefEnv k a r -> [a]
elems (RefEnv ras) = map snd ras

null :: RefEnv k a r -> Bool
null (RefEnv ras) = Prelude.null ras

singleton :: Reference k r -> a -> RefEnv k a r
singleton r a = RefEnv [(r, a)]

filter :: (a -> Bool) -> RefEnv k a r -> RefEnv k a r
filter p (RefEnv ras) = RefEnv (Prelude.filter (p . snd) ras)

reverseLookup :: (a -> Bool) -> RefEnv k a r -> [Reference k r]
reverseLookup p = keys . filter p

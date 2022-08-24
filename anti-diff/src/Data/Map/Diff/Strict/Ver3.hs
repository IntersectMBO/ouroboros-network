{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE UndecidableInstances       #-}

module Data.Map.Diff.Strict.Ver3 (
    Act (..)
  , Diff (..)
  , PDiff
  , Partial (..)
    -- * Construction
  , fromList
  , fromPartialUnsafe
    -- * Values and keys
  , Keys (..)
  , Values (..)
  , diffKeys
  , restrictValues
  , valuesFromList
    -- * Forwarding values and keys
  , forwardValues
  , forwardValuesAndKeys
  ) where

import           Data.Group
import qualified Data.Map.Merge.Strict as Merge
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Semigroupoid
import           Data.Set (Set)
import qualified Data.Set as Set
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)
import           NoThunks.Class (NoThunks)

{------------------------------------------------------------------------------
  General-purposes diffs for key-value stores
------------------------------------------------------------------------------}

type PDiff k v = Partial (Diff k v)

-- | Redefine 'Maybe'
data Partial a = Defined !a | Undefined
  deriving (Generic, Show, Eq, Functor)
  deriving anyclass (NoThunks)

-- | A diff for key-value stores.
newtype Diff k v = Diff (Map k (Act v))
  deriving stock (Generic, Show, Eq, Functor)
  deriving anyclass (NoThunks)

-- | An action to apply to a value in a key-value store.
--
-- Note: This datatype froms a 'Groupoid' of insertions and deletions.
data Act v = Del !v | Ins !v | DelIns !v !v | InsDel
  deriving stock (Generic, Show, Eq, Functor)
  deriving anyclass (NoThunks)

{------------------------------------------------------------------------------
  Construction
------------------------------------------------------------------------------}

toPartial :: Maybe a -> Partial a
toPartial = maybe Undefined Defined

fromPartialUnsafe :: Partial a -> a
fromPartialUnsafe Undefined   = error "bad fromPartialUnsafe"
fromPartialUnsafe (Defined x) = x

fromList :: Ord k => [(k, Act v)] -> Diff k v
fromList = Diff . Map.fromList

{------------------------------------------------------------------------------
  Class instances for @'Act' v@
------------------------------------------------------------------------------}

instance Eq v => Semigroupoid (Act v) where
    l <>? r = case l of
        Del x -> case r of
            Del{}    -> Nothing   -- disallow double delete
            Ins y    -> Just $ DelIns x y

            DelIns{} -> Nothing   -- disallow double delete

            InsDel   -> Just $ Del x

        Ins x -> case r of
            Del y ->
                if x /= y then Nothing   -- disallow inaccurate delete
                else Just InsDel
            Ins{} -> Nothing   -- disallow overwrite

            DelIns y z ->
                if x /= y then Nothing   -- disallow inaccurate delete
                else Just $ Ins z

            InsDel{} -> Nothing   -- disallow overwrite

        DelIns x y -> case r of
            Del z ->
                if y /= z then Nothing   -- disallow inaccurate delete
                else Just $ Del x
            Ins{} -> Nothing   -- disallow overwrite

            DelIns z aa ->
                if y /= z then Nothing   -- disallow inaccurate delete
                else Just $ DelIns x aa

            InsDel{} -> Nothing   -- disallow overwrite

        InsDel -> case r of
            Del{}    -> Nothing   -- disallow double delete
            Ins x    -> Just $ Ins x

            DelIns{} -> Nothing   -- disallow double delete

            InsDel   -> Just InsDel

instance Eq v => Groupoid (Act v) where
  pinv = \case
      Del v      -> Ins v
      Ins v      -> Del v

      DelIns x y -> DelIns y x

      InsDel     -> InsDel

{------------------------------------------------------------------------------
  Class instances for @'Diff' k v@
------------------------------------------------------------------------------}

instance (Ord k, Eq v) => Semigroupoid (Diff k v) where
  (<>?) (Diff l) (Diff r) = Diff <$> sequenceA (
    Merge.merge
      (Merge.mapMissing (const Just))
      (Merge.mapMissing (const Just))
      (Merge.zipWithMatched f
      )
      l
      r)
    where
      f _ v1 v2 = v1 <>? v2

instance (Ord k, Eq v) => Groupoid (Diff k v) where
  pinv (Diff m) = Diff $ fmap pinv m

{------------------------------------------------------------------------------
  Class instances for @'PDiff' k v@ (partial diffs)
------------------------------------------------------------------------------}

instance (Ord k, Eq v) => Semigroup (PDiff k v) where
  Defined x <> Defined y = toPartial (x <>? y)
  _         <> _         = Undefined

instance (Ord k, Eq v) => Monoid (PDiff k v) where
  mempty = Defined . Diff $ Map.empty

instance (Ord k, Eq v) => Group (PDiff k v) where
  invert Undefined   = Undefined
  invert (Defined x) = Defined $ pinv x

{------------------------------------------------------------------------------
  Values and keys
------------------------------------------------------------------------------}

-- | A key-value store.
newtype Values k v = Values (Map k v)
  deriving stock (Generic, Show, Eq, Functor)
  deriving anyclass (NoThunks)

newtype Keys k v = Keys (Set k)
  deriving stock (Generic, Show, Eq, Functor)
  deriving anyclass (NoThunks)

valuesFromList :: Ord k => [(k, v)] -> Values k v
valuesFromList = Values . Map.fromList

diffKeys :: Diff k v -> Set k
diffKeys (Diff m) = Map.keysSet m

restrictValues :: Ord k => Values k v -> Keys k v -> Values k v
restrictValues (Values m) (Keys s) = Values (Map.restrictKeys m s)

{------------------------------------------------------------------------------
  Forwarding values and keys
------------------------------------------------------------------------------}

forwardValues ::
     (Ord k, Eq v, HasCallStack)
  => Values k v
  -> Diff k v
  -> Values k v
forwardValues (Values values) (Diff diffs) = Values $
    Merge.merge
      Merge.preserveMissing
      (Merge.mapMaybeMissing     newKeys)
      (Merge.zipWithMaybeMatched oldKeys)
      values
      diffs
  where
    -- Impossible cases:
    -- * Deletions of values that are not in the original @Map@.
    newKeys :: k -> Act v -> Maybe v
    newKeys _k = \case
      Del _x        -> error "impossible"
      Ins x         -> Just x
      DelIns _x _x' -> error "impossible"
      InsDel        -> Nothing

    -- Impossible cases:
    -- * Deletion of values that are not in the original @Map@.
    -- * Insertions of keys that are in the original @Map@.
    oldKeys :: Eq v => k -> v -> Act v -> Maybe v
    oldKeys _k x = \case
      Del y       | x == y    -> Nothing
                  | otherwise -> error "impossible"
      Ins _y                  -> error "impossible"
      DelIns y y' | x == y    -> Just y'
                  | otherwise -> error "impossible"
      InsDel                  -> error "impossible"

forwardValuesAndKeys ::
     (Ord k, Eq v, HasCallStack)
  => Values k v
  -> Keys k v
  -> Diff k v
  -> Values k v
forwardValuesAndKeys v@(Values values) (Keys keys) (Diff diffs) =
  forwardValues
    v
    (Diff $ diffs `Map.restrictKeys` (Map.keysSet values `Set.union` keys))

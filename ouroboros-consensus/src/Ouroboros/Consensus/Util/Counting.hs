{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

-- | Type-level counting
--
-- Intended for unqualified import.
module Ouroboros.Consensus.Util.Counting (
    Exactly
  , AtMost(..)
  , NonEmpty(..)
    -- * Working with 'Exactly'
  , exactlyOne
  , exactlyTwo
  , exactlyHead
  , exactlyTail
  , exactlyZip
  , exactlyZipFoldable
  , exactlyWeaken
  , exactlyWeakenNonEmpty
  , exactlyReplicate
    -- * Working with 'AtMost'
  , atMostOne
  , atMostInit
  , atMostHead
  , atMostLast
  , atMostZipFoldable
  , atMostNonEmpty
    -- * Working with 'NonEmpty'
  , nonEmptyHead
  , nonEmptyLast
  , nonEmptyInit
  , nonEmptyFromList
  ) where

import qualified Data.Foldable as Foldable
import           Data.SOP.Strict

import           Ouroboros.Consensus.Util.SOP

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

type Exactly xs a = NP (K a) xs

-- | At most one value for each type level index
data AtMost :: [*] -> * -> * where
  AtMostNil  :: AtMost xs a
  AtMostCons :: !a -> !(AtMost xs a) -> AtMost (x ': xs) a

-- | Non-empty variation on 'AtMost'
data NonEmpty :: [*] -> * -> * where
  NonEmptyOne  :: !a -> NonEmpty (x ': xs) a
  NonEmptyCons :: !a -> !(NonEmpty xs a) -> NonEmpty (x ': xs) a

deriving instance Eq a => Eq (AtMost   xs a)
deriving instance Eq a => Eq (NonEmpty xs a)

deriving instance Show a => Show (AtMost   xs a)
deriving instance Show a => Show (NonEmpty xs a)

deriving instance Functor     (AtMost xs)
deriving instance Foldable    (AtMost xs)
deriving instance Traversable (AtMost xs)

deriving instance Functor     (NonEmpty xs)
deriving instance Foldable    (NonEmpty xs)
deriving instance Traversable (NonEmpty xs)

{-------------------------------------------------------------------------------
  Working with 'Exactly'
-------------------------------------------------------------------------------}

-- | Singleton
exactlyOne :: a -> Exactly '[x] a
exactlyOne a = K a :* Nil

-- | From a pair
exactlyTwo :: a -> a -> Exactly '[x, y] a
exactlyTwo a1 a2 = K a1 :* K a2 :* Nil

-- | Analogue of 'head'
exactlyHead :: Exactly (x ': xs) a -> a
exactlyHead = unK . hd

-- | Analogue of 'tail'
exactlyTail :: Exactly (x ': xs) a -> Exactly xs a
exactlyTail = tl

-- | Analogue of 'zip'
exactlyZip :: Exactly xs a -> Exactly xs b -> Exactly xs (a, b)
exactlyZip np np' =
    npToSListI np $
      hzipWith (\(K x) (K y) -> K (x, y)) np np'

-- | Analogue of 'zip' where the length of second argument is unknown
exactlyZipFoldable :: Foldable t => Exactly xs a -> t b -> AtMost xs (a, b)
exactlyZipFoldable = \as bs -> go as (Foldable.toList bs)
  where
    go :: Exactly xs a -> [b] -> AtMost xs (a, b)
    go _           []     = AtMostNil
    go Nil         _      = AtMostNil
    go (K a :* as) (b:bs) = AtMostCons (a, b) $ go as bs

exactlyWeaken :: Exactly xs a -> AtMost xs a
exactlyWeaken = go
  where
    go :: Exactly xs a -> AtMost xs a
    go Nil         = AtMostNil
    go (K x :* xs) = AtMostCons x (go xs)

exactlyWeakenNonEmpty :: Exactly (x ': xs) a -> NonEmpty (x ': xs) a
exactlyWeakenNonEmpty = go
  where
    go :: Exactly (x ': xs) a -> NonEmpty (x ': xs) a
    go (K x :* Nil)         = NonEmptyOne x
    go (K x :* xs@(_ :* _)) = NonEmptyCons x (go xs)

-- | Analogue of 'replicate'
--
-- In CPS style because the @xs@ type parameter is not statically known.
exactlyReplicate :: forall a r. Word -> a -> (forall xs. Exactly xs a -> r) -> r
exactlyReplicate = go
  where
    go :: Word -> a -> (forall xs. Exactly xs a -> r) -> r
    go 0 _ k = k Nil
    go n a k = go (n - 1) a $ \xs -> k (K a :* xs)

{-------------------------------------------------------------------------------
  Working with 'AtMost'
-------------------------------------------------------------------------------}

-- | Singleton
atMostOne :: a -> AtMost (x ': xs) a
atMostOne x = AtMostCons x AtMostNil

-- | Analogue of 'init'
--
-- For simplicity we don't shrink the type-level index.
atMostInit :: AtMost xs a -> Maybe (AtMost xs a, a)
atMostInit = go
  where
    go :: AtMost xs a -> Maybe (AtMost xs a, a)
    go AtMostNil         = Nothing
    go (AtMostCons a as) = Just $
                             case go as of
                               Nothing        -> (AtMostNil, a)
                               Just (as', a') -> (AtMostCons a as', a')

-- | Analogue of 'head'
atMostHead :: AtMost xs a -> Maybe a
atMostHead AtMostNil        = Nothing
atMostHead (AtMostCons x _) = Just x

-- | Analogue of 'last'
atMostLast :: AtMost xs a -> Maybe a
atMostLast = fmap snd . atMostInit

atMostZipFoldable :: Foldable t => AtMost xs a -> t b -> AtMost xs (a, b)
atMostZipFoldable = \as bs -> go as (Foldable.toList bs)
  where
    go :: AtMost xs a -> [b] -> AtMost xs (a, b)
    go AtMostNil         _      = AtMostNil
    go _                 []     = AtMostNil
    go (AtMostCons a as) (b:bs) = AtMostCons (a, b) (go as bs)

atMostNonEmpty :: AtMost (x ': xs) a -> Maybe (NonEmpty (x ': xs) a)
atMostNonEmpty = \case
    AtMostNil       -> Nothing
    AtMostCons x xs -> Just $ go x xs
  where
    go :: a -> AtMost xs a -> NonEmpty (x ': xs) a
    go x AtMostNil         = NonEmptyOne  x
    go x (AtMostCons y zs) = NonEmptyCons x (go y zs)

{-------------------------------------------------------------------------------
  Working with 'NonEmpty'
-------------------------------------------------------------------------------}

-- | Analogue of 'head'
nonEmptyHead :: NonEmpty xs a -> a
nonEmptyHead (NonEmptyOne  x)   = x
nonEmptyHead (NonEmptyCons x _) = x

-- | Analogue of 'last'
nonEmptyLast :: NonEmpty xs a -> a
nonEmptyLast = snd . nonEmptyInit

-- | Analogue of 'init'
nonEmptyInit :: NonEmpty xs a -> (Maybe (NonEmpty xs a), a)
nonEmptyInit (NonEmptyOne  x)    = (Nothing, x)
nonEmptyInit (NonEmptyCons x xs) =
    case nonEmptyInit xs of
      (Nothing  , final) -> (Just (NonEmptyOne  x)     , final)
      (Just xs' , final) -> (Just (NonEmptyCons x xs') , final)

-- | Build a 'NonEmpty' from a list. Returns 'Nothing' when the list is empty
-- or when it's longer than @xs@.
nonEmptyFromList :: forall xs a. SListI xs => [a] -> Maybe (NonEmpty xs a)
nonEmptyFromList = go (sList @xs)
  where
    go :: forall xs'. SList xs' -> [a] -> Maybe (NonEmpty xs' a)
    go s ys = case (s, ys) of
        (SCons, [y])   -> Just $ NonEmptyOne y
        (SCons, y:ys') -> NonEmptyCons y <$> go sList ys'
        (SCons, [])    -> Nothing
        (SNil,  _)     -> Nothing

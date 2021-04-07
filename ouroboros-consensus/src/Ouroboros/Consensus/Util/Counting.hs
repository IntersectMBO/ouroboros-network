{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveFoldable       #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE EmptyCase            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}

-- | Type-level counting
--
-- Intended for unqualified import.
module Ouroboros.Consensus.Util.Counting (
    AtMost (..)
  , Exactly (.., ExactlyNil, ExactlyCons)
  , NonEmpty (..)
    -- * Working with 'Exactly'
  , exactlyHead
  , exactlyOne
  , exactlyReplicate
  , exactlyTail
  , exactlyTwo
  , exactlyWeaken
  , exactlyWeakenNonEmpty
  , exactlyZip
  , exactlyZipFoldable
    -- * Working with 'AtMost'
  , atMostHead
  , atMostInit
  , atMostLast
  , atMostNonEmpty
  , atMostOne
  , atMostZipFoldable
    -- * Working with 'NonEmpty'
  , nonEmptyFromList
  , nonEmptyHead
  , nonEmptyInit
  , nonEmptyLast
  , nonEmptyMapOne
  , nonEmptyMapTwo
  , nonEmptyStrictPrefixes
  , nonEmptyToList
  , nonEmptyWeaken
  ) where

import           Control.Applicative
import qualified Data.Foldable as Foldable
import           Data.Kind (Type)
import           Data.SOP.Dict
import           Data.SOP.Strict

import           Cardano.Binary
import           Data.Typeable (Typeable)
import           Ouroboros.Consensus.Util.SOP

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

newtype Exactly xs a = Exactly { getExactly :: NP (K a) xs }

-- NOTE: This could be a deriving newtype instance, but the constraints will be
-- a bit more complicated.
instance (Typeable xs, ToCBOR a, SListI xs) => ToCBOR (Exactly xs a) where
  toCBOR (Exactly xs) = hcfoldMap (Proxy @Top) (toCBOR . unK) xs

instance (Typeable xs, FromCBOR a, SListI xs) => FromCBOR (Exactly xs a) where
  fromCBOR = Exactly <$> htraverse' (\(K ()) -> K <$> fromCBOR) (hpure (K ()))

-- | At most one value for each type level index
data AtMost :: [Type] -> Type -> Type where
  AtMostNil  :: AtMost xs a
  AtMostCons :: !a -> !(AtMost xs a) -> AtMost (x ': xs) a

-- | Non-empty variation on 'AtMost'
data NonEmpty :: [Type] -> Type -> Type where
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
  Pattern synonyms for 'Exactly'
-------------------------------------------------------------------------------}

-- | Internal: view on 'Exactly'
--
-- Used for the pattern synonyms only.
data ExactlyView xs a where
  ENil  :: ExactlyView '[] a
  ECons :: a -> Exactly xs a -> ExactlyView (x : xs) a

-- | Internal: construct the view on 'Exactly'
--
-- Used for the pattern synonyms only.
exactlyView :: Exactly xs a -> ExactlyView xs a
exactlyView (Exactly (K x :* xs)) = ECons x (Exactly xs)
exactlyView (Exactly Nil)         = ENil

{-# COMPLETE ExactlyNil, ExactlyCons #-}

pattern ExactlyCons ::
     ()
  => xs' ~ (x ': xs)
  => a -> Exactly xs a -> Exactly xs' a
pattern ExactlyCons x xs <- (exactlyView -> ECons x xs)
  where
    ExactlyCons x xs = Exactly (K x :* (getExactly xs))

pattern ExactlyNil ::
     ()
  => xs ~ '[]
  => Exactly xs a
pattern ExactlyNil <- (exactlyView -> ENil)
  where
    ExactlyNil = Exactly Nil

{-------------------------------------------------------------------------------
  Type class instances for 'Exactly'

  For 'AtMost' and 'NonEmpty' we can just derive these, but for 'Exactly'
  we need to do a bit more work.
-------------------------------------------------------------------------------}

instance Functor (Exactly xs) where
  fmap f (Exactly xs) = npToSListI xs $ Exactly $
      hmap (mapKK f) xs

instance Foldable (Exactly xs) where
  foldMap f (Exactly xs) = npToSListI xs $
      foldMap f (hcollapse xs)

instance Traversable (Exactly xs) where
  traverse f (Exactly xs) = npToSListI xs $ fmap Exactly $
      hsequence' $ hmap (\(K x) -> Comp $ K <$> f x) xs

instance Show a => Show (Exactly xs a) where
  show (Exactly xs) = npToSListI xs $
      case dict of
        Dict -> show xs
    where
      dict :: SListI xs => Dict (All (Compose Show (K a))) xs
      dict = all_NP (hpure Dict)

instance Eq a => Eq (Exactly xs a) where
  Exactly xs == Exactly xs' = npToSListI xs $
      case dict of
        Dict -> xs == xs'
    where
      dict :: SListI xs => Dict (All (Compose Eq (K a))) xs
      dict = all_NP (hpure Dict)

{-------------------------------------------------------------------------------
  Working with 'Exactly'
-------------------------------------------------------------------------------}

-- | Singleton
exactlyOne :: a -> Exactly '[x] a
exactlyOne a = Exactly $ K a :* Nil

-- | From a pair
exactlyTwo :: a -> a -> Exactly '[x, y] a
exactlyTwo a1 a2 = Exactly $ K a1 :* K a2 :* Nil

-- | Analogue of 'head'
exactlyHead :: Exactly (x ': xs) a -> a
exactlyHead = unK . hd . getExactly

-- | Analogue of 'tail'
exactlyTail :: Exactly (x ': xs) a -> Exactly xs a
exactlyTail = Exactly . tl . getExactly

-- | Analogue of 'zip'
exactlyZip :: Exactly xs a -> Exactly xs b -> Exactly xs (a, b)
exactlyZip (Exactly np) (Exactly np') = Exactly $
    npToSListI np $
      hzipWith (\(K x) (K y) -> K (x, y)) np np'

-- | Analogue of 'zip' where the length of second argument is unknown
exactlyZipFoldable :: Foldable t => Exactly xs a -> t b -> AtMost xs (a, b)
exactlyZipFoldable = \(Exactly as) bs -> go as (Foldable.toList bs)
  where
    go :: NP (K a) xs -> [b] -> AtMost xs (a, b)
    go _           []     = AtMostNil
    go Nil         _      = AtMostNil
    go (K a :* as) (b:bs) = AtMostCons (a, b) $ go as bs

exactlyWeaken :: Exactly xs a -> AtMost xs a
exactlyWeaken = go . getExactly
  where
    go :: NP (K a) xs -> AtMost xs a
    go Nil         = AtMostNil
    go (K x :* xs) = AtMostCons x (go xs)

exactlyWeakenNonEmpty :: Exactly (x ': xs) a -> NonEmpty (x ': xs) a
exactlyWeakenNonEmpty = go . getExactly
  where
    go :: NP (K a) (x ': xs) -> NonEmpty (x ': xs) a
    go (K x :* Nil)         = NonEmptyOne x
    go (K x :* xs@(_ :* _)) = NonEmptyCons x (go xs)

-- | Analogue of 'replicate'
--
-- In CPS style because the @xs@ type parameter is not statically known.
exactlyReplicate :: forall a r. Word -> a -> (forall xs. Exactly xs a -> r) -> r
exactlyReplicate = \n a k -> go n a (k . Exactly)
  where
    go :: Word -> a -> (forall xs. NP (K a) xs -> r) -> r
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

instance IsNonEmpty xs => Applicative (NonEmpty xs) where
  pure x = case isNonEmpty (Proxy @xs) of
             ProofNonEmpty{} -> NonEmptyOne x
  (<*>) = go
    where
      go :: NonEmpty xs' (a -> b) -> NonEmpty xs' a -> NonEmpty xs' b
      go (NonEmptyOne  f)    (NonEmptyOne  x)    = NonEmptyOne  (f x)
      go (NonEmptyCons f _)  (NonEmptyOne  x)    = NonEmptyOne  (f x)
      go (NonEmptyOne  f)    (NonEmptyCons x _)  = NonEmptyOne  (f x)
      go (NonEmptyCons f fs) (NonEmptyCons x xs) = NonEmptyCons (f x) (go fs xs)

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

-- | Convert a 'NonEmpty' to a list.
nonEmptyToList :: forall xs a. NonEmpty xs a -> [a]
nonEmptyToList = go
  where
    go :: forall xs'. NonEmpty xs' a -> [a]
    go (NonEmptyOne  x)    = x : []
    go (NonEmptyCons x xs) = x : go xs

nonEmptyWeaken :: NonEmpty xs a -> AtMost xs a
nonEmptyWeaken = go
  where
    go :: NonEmpty xs a -> AtMost xs a
    go (NonEmptyOne  x)    = AtMostCons x AtMostNil
    go (NonEmptyCons x xs) = AtMostCons x (go xs)

-- | A strict prefixes
--
-- >    nonEmptyStrictPrefixes (fromJust (nonEmptyFromList [1..4]))
-- > == [ NonEmptyOne  1
-- >    , NonEmptyCons 1 $ NonEmptyOne  2
-- >    , NonEmptyCons 1 $ NonEmptyCons 2 $ NonEmptyOne 3
-- >    ]
nonEmptyStrictPrefixes :: NonEmpty xs a -> [NonEmpty xs a]
nonEmptyStrictPrefixes = go
  where
    go :: NonEmpty xs a -> [NonEmpty xs a]
    go (NonEmptyOne  _)    = []
    go (NonEmptyCons x xs) = NonEmptyOne x : map (NonEmptyCons x) (go xs)

-- | Apply the specified function to exactly one element
nonEmptyMapOne :: forall m xs a. Alternative m
               => (a -> m a) -> NonEmpty xs a -> m (NonEmpty xs a)
nonEmptyMapOne f = go
  where
    go :: NonEmpty xs' a -> m (NonEmpty xs' a)
    go (NonEmptyOne  x)    = NonEmptyOne <$> f x
    go (NonEmptyCons x xs) = Foldable.asum [
          (\x' -> NonEmptyCons x' xs) <$> f x
        , NonEmptyCons x <$> go xs
        ]

-- | Variation on 'nonEmptyMapOne' where we try to apply the function to
-- /pairs/ of elements
nonEmptyMapTwo :: forall m xs a. Alternative m
               => (a -> m a) -- Used when we reached the end of the list
               -> (a -> a -> m (a, a))
               -> NonEmpty xs a -> m (NonEmpty xs a)
nonEmptyMapTwo f g = go
  where
    go :: NonEmpty xs' a -> m (NonEmpty xs' a)
    go (NonEmptyOne x) =
        NonEmptyOne <$> f x
    go (NonEmptyCons x xs@(NonEmptyOne y)) = Foldable.asum [
          (\(x', y') -> NonEmptyCons x' (NonEmptyOne y')) <$> g x y
        , NonEmptyCons x <$> go xs
        ]
    go (NonEmptyCons x xs@(NonEmptyCons y zs)) = Foldable.asum [
          (\(x', y') -> NonEmptyCons x' (NonEmptyCons y' zs)) <$> g x y
        , NonEmptyCons x <$> go xs
        ]

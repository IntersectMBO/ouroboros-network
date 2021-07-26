{-# LANGUAGE DeriveFunctor #-}

module Data.List.Octopus
  ( Octopus (..)
  , ppOctopus
  , toList
  , fromList
  , head
  , tail
  , filter
  , length
  ) where

import           Prelude hiding (filter, head, length, tail)

import           Control.Applicative (Alternative (..))
import           Control.Monad (MonadPlus (..))
import           Control.Monad.Fix (MonadFix (..), fix)
import           Data.Bifunctor
import           Data.Bifoldable
import           Data.Bitraversable
import           Data.Functor.Classes

-- | A 'cons' list with polymorphic 'nil', thus an octopus.
--
-- * @'Octopus' Void a@ is an infinite stream
-- * @'Octopus' () a@ is isomorphic to @[a]@
--
-- Usually used with @a@ being a non empty sum type.
--
data Octopus a b
    = Cons b (Octopus a b)
    | Nil a
    deriving (Show, Eq, Ord, Functor)

head :: Octopus a b -> b
head (Cons b _) = b
head _ = error "Octopus.head: empty"

tail :: Octopus a b -> Octopus a b
tail (Cons _ o) = o
tail Nil {} = error "Octopus.tail: empty"

filter :: (b -> Bool) -> Octopus a b -> Octopus a b
filter _fn o@Nil {}   = o
filter  fn (Cons b o) =
    case fn b of
      True  -> Cons b (filter fn o)
      False ->         filter fn o

length :: Octopus a b -> Int
length (Cons _ o) = (+) 1 $! length o
length  Nil {}    = 0

toList :: Octopus a b -> [b]
toList = bifoldr (\_ bs -> bs) (:) []

fromList :: a -> [b] -> Octopus a b
fromList a = foldr Cons (Nil a)

-- | Pretty print an 'Octopus'.
--
ppOctopus :: (a -> String) -> (b -> String) ->  Octopus a b -> String
ppOctopus sa  sb (Cons b bs) = sb b ++ "\n" ++ ppOctopus sa sb bs
ppOctopus sa _sb (Nil a)     = sa a

instance Bifunctor Octopus where
    bimap f g (Cons b bs) = Cons (g b) (bimap f g bs)
    bimap f _ (Nil a)     = Nil (f a)

instance Bifoldable Octopus where
    bifoldMap f g (Cons b bs) = g b <> bifoldMap f g bs
    bifoldMap f _ (Nil a)     = f a

instance Bitraversable Octopus where
    bitraverse f g (Cons b bs) = Cons <$> g b <*> bitraverse f g bs
    bitraverse f _ (Nil a)     = Nil <$> f a

instance Semigroup a => Semigroup (Octopus a b) where
    Cons b o  <> o'          = Cons b (o <> o')
    o@Nil {}  <> (Cons b o') = Cons b (o <> o')
    Nil a     <> Nil a'      = Nil (a <> a')

instance Monoid a => Monoid (Octopus a b) where
    mempty = Nil mempty

instance Monoid a => Applicative (Octopus a) where
    pure b = Cons b (Nil mempty)
    Cons f fs <*> o = fmap f o <> (fs <*> o)
    Nil a <*> _     = Nil a

instance Monoid a => Monad (Octopus a) where
    return  = pure
    -- @bifoldMap Nil id@ is the @join@ of @Octopus a@
    o >>= f = bifoldMap Nil id $ fmap f o

instance Monoid a => MonadFail (Octopus a) where
    fail _ = mzero

instance Monoid a => Alternative (Octopus a) where
    empty = mempty
    (<|>) = (<>)

instance Monoid a => MonadPlus (Octopus a) where
    mzero = mempty
    mplus = (<>)

instance Monoid a => MonadFix (Octopus a) where
    mfix f = case fix (f . head) of
      o@Nil {} -> o
      Cons b _ -> Cons b (mfix (tail . f))

instance Eq a => Eq1 (Octopus a) where
    liftEq f (Cons b o) (Cons b' o') = f b b' && liftEq f o o'
    liftEq _ Nil  {}     Cons {}     = False
    liftEq _ Cons {}     Nil  {}     = False
    liftEq _ (Nil a)    (Nil a')     = a == a'

instance Ord a => Ord1 (Octopus a) where
    liftCompare f (Cons b o) (Cons b' o') = f b b' `compare` liftCompare f o o'
    liftCompare _  Nil  {}    Cons {}     = LT
    liftCompare _  Cons {}    Nil {}      = GT
    liftCompare _ (Nil a)    (Nil a')     = a `compare` a'

instance Show a => Show1 (Octopus a) where
    liftShowsPrec  showsPrec_ showsList_ prec (Cons b o)
      = showString "Cons "
      . showsPrec_ prec b
      . showChar ' '
      . showParen True (liftShowsPrec showsPrec_ showsList_ prec o)
    liftShowsPrec _showsPrec _showsList _prec (Nil a)
      = showString "Nil "
      . shows a

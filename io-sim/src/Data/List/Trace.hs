{-# LANGUAGE DeriveFunctor #-}

module Data.List.Trace
  ( Trace (..)
  , ppTrace
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
-- * @'Trace' Void a@ is an infinite stream
-- * @'Trace' () a@ is isomorphic to @[a]@
--
-- Usually used with @a@ being a non empty sum type.
--
data Trace a b
    = Cons b (Trace a b)
    | Nil a
    deriving (Show, Eq, Ord, Functor)

head :: Trace a b -> b
head (Cons b _) = b
head _ = error "Trace.head: empty"

tail :: Trace a b -> Trace a b
tail (Cons _ o) = o
tail Nil {} = error "Trace.tail: empty"

filter :: (b -> Bool) -> Trace a b -> Trace a b
filter _fn o@Nil {}   = o
filter  fn (Cons b o) =
    case fn b of
      True  -> Cons b (filter fn o)
      False ->         filter fn o

length :: Trace a b -> Int
length (Cons _ o) = (+) 1 $! length o
length  Nil {}    = 0

toList :: Trace a b -> [b]
toList = bifoldr (\_ bs -> bs) (:) []

fromList :: a -> [b] -> Trace a b
fromList a = foldr Cons (Nil a)

-- | Pretty print an 'Trace'.
--
ppTrace :: (a -> String) -> (b -> String) -> Trace a b -> String
ppTrace sa  sb (Cons b bs) = sb b ++ "\n" ++ ppTrace sa sb bs
ppTrace sa _sb (Nil a)     = sa a

instance Bifunctor Trace where
    bimap f g (Cons b bs) = Cons (g b) (bimap f g bs)
    bimap f _ (Nil a)     = Nil (f a)

instance Bifoldable Trace where
    bifoldMap f g (Cons b bs) = g b <> bifoldMap f g bs
    bifoldMap f _ (Nil a)     = f a

instance Bitraversable Trace where
    bitraverse f g (Cons b bs) = Cons <$> g b <*> bitraverse f g bs
    bitraverse f _ (Nil a)     = Nil <$> f a

instance Semigroup a => Semigroup (Trace a b) where
    Cons b o  <> o'          = Cons b (o <> o')
    o@Nil {}  <> (Cons b o') = Cons b (o <> o')
    Nil a     <> Nil a'      = Nil (a <> a')

instance Monoid a => Monoid (Trace a b) where
    mempty = Nil mempty

instance Monoid a => Applicative (Trace a) where
    pure b = Cons b (Nil mempty)
    Cons f fs <*> o = fmap f o <> (fs <*> o)
    Nil a <*> _     = Nil a

instance Monoid a => Monad (Trace a) where
    return  = pure
    -- @bifoldMap Nil id@ is the @join@ of @Trace a@
    o >>= f = bifoldMap Nil id $ fmap f o

instance Monoid a => MonadFail (Trace a) where
    fail _ = mzero

instance Monoid a => Alternative (Trace a) where
    empty = mempty
    (<|>) = (<>)

instance Monoid a => MonadPlus (Trace a) where
    mzero = mempty
    mplus = (<>)

instance Monoid a => MonadFix (Trace a) where
    mfix f = case fix (f . head) of
      o@Nil {} -> o
      Cons b _ -> Cons b (mfix (tail . f))

instance Eq a => Eq1 (Trace a) where
    liftEq f (Cons b o) (Cons b' o') = f b b' && liftEq f o o'
    liftEq _ Nil  {}     Cons {}     = False
    liftEq _ Cons {}     Nil  {}     = False
    liftEq _ (Nil a)    (Nil a')     = a == a'

instance Ord a => Ord1 (Trace a) where
    liftCompare f (Cons b o) (Cons b' o') = f b b' `compare` liftCompare f o o'
    liftCompare _  Nil  {}    Cons {}     = LT
    liftCompare _  Cons {}    Nil {}      = GT
    liftCompare _ (Nil a)    (Nil a')     = a `compare` a'

instance Show a => Show1 (Trace a) where
    liftShowsPrec  showsPrec_ showsList_ prec (Cons b o)
      = showString "Cons "
      . showsPrec_ prec b
      . showChar ' '
      . showParen True (liftShowsPrec showsPrec_ showsList_ prec o)
    liftShowsPrec _showsPrec _showsList _prec (Nil a)
      = showString "Nil "
      . shows a

{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE EmptyCase            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
-- | NP with optional values
--
--
-- Intended for qualified import
--
-- > import           Ouroboros.Consensus.Util.OptNP (OptNP (..), ViewOptNP (..))
-- > import qualified Ouroboros.Consensus.Util.OptNP as OptNP
module Ouroboros.Consensus.Util.OptNP (
    OptNP (..)
  , at
  , empty
  , fromNP
  , fromNonEmptyNP
  , fromSingleton
  , singleton
  , toNP
    -- * View
  , ViewOptNP (..)
  , view
    -- * Combining
  , combine
  , combineWith
  , zipWith
  ) where

import           Prelude hiding (zipWith)

import           Control.Monad (guard)
import           Data.Functor.These (These1 (..))
import           Data.Kind (Type)
import           Data.Maybe (isJust)
import           Data.SOP.Strict hiding (And)
import           Data.Type.Equality
import           GHC.Stack (HasCallStack)

import           Ouroboros.Consensus.Util.SOP

-- | Like an 'NP', but with optional values
data OptNP (empty :: Bool) (f :: k -> Type) (xs :: [k]) where
  OptNil  :: OptNP 'True f '[]
  OptCons :: !(f x) -> !(OptNP empty f xs) -> OptNP 'False f (x ': xs)
  OptSkip :: !(OptNP empty f xs) -> OptNP empty f (x ': xs)

type instance AllN    (OptNP empty) c = All c
type instance SListIN (OptNP empty)   = SListI
type instance Prod    (OptNP empty)   = NP

deriving instance All (Show `Compose` f) xs => Show (OptNP empty f xs)

eq ::
     All (Eq `Compose` f) xs
  => OptNP empty f xs
  -> OptNP empty' f xs -> Maybe (empty :~: empty')
eq OptNil         OptNil         = Just Refl
eq (OptSkip   xs) (OptSkip   ys) = eq xs ys
eq (OptCons x xs) (OptCons y ys) = do guard (x == y)
                                      Refl <- eq xs ys
                                      return Refl
eq _              _              = Nothing

instance All (Eq `Compose` f) xs => Eq (OptNP empty f xs) where
  xs == ys = isJust (eq xs ys)

empty :: forall f xs. SListI xs => OptNP 'True f xs
empty = case sList @xs of
    SNil  -> OptNil
    SCons -> OptSkip empty

fromNonEmptyNP :: forall f xs. IsNonEmpty xs => NP f xs -> OptNP 'False f xs
fromNonEmptyNP xs = case isNonEmpty (Proxy @xs) of
    ProofNonEmpty {} ->
      case xs of
        x :* xs' -> fromNP (OptCons x) xs'

fromNP :: (forall empty. OptNP empty f xs -> r) -> NP f xs -> r
fromNP k Nil       = k OptNil
fromNP k (x :* xs) = fromNP (k . OptCons x) xs

toNP :: OptNP empty f xs -> NP (Maybe :.: f) xs
toNP = go
  where
    go :: OptNP empty f xs -> NP (Maybe :.: f) xs
    go OptNil         = Nil
    go (OptCons x xs) = Comp (Just x) :* go xs
    go (OptSkip   xs) = Comp Nothing  :* go xs

at :: SListI xs => f x -> Index xs x -> OptNP 'False f xs
at x IZ         = OptCons x empty
at x (IS index) = OptSkip (at x index)

singleton :: f x -> OptNP 'False f '[x]
singleton x = OptCons x OptNil

-- | If 'OptNP' is not empty, it must contain at least one value
fromSingleton :: OptNP 'False f '[x] -> f x
fromSingleton (OptCons x _) = x

ap ::
     NP (f -.-> g) xs
  -> OptNP empty f xs
  -> OptNP empty g xs
ap = go
  where
    go :: NP (f -.-> g) xs -> OptNP empty f xs -> OptNP empty g xs
    go (f :* fs) (OptCons x xs) = OptCons (apFn f x) (go fs xs)
    go (_ :* fs) (OptSkip   xs) = OptSkip            (go fs xs)
    go Nil       OptNil         = OptNil

ctraverse' ::
     forall c proxy empty xs f f' g. (All c xs, Applicative g)
  => proxy c
  -> (forall a. c a => f a -> g (f' a))
  -> OptNP empty f xs  -> g (OptNP empty f' xs)
ctraverse' _ f = go
  where
    go :: All c ys => OptNP empty' f ys -> g (OptNP empty' f' ys)
    go (OptCons x xs) = OptCons <$> f x <*> go xs
    go (OptSkip   xs) = OptSkip <$>         go xs
    go OptNil         = pure OptNil

instance HAp (OptNP empty) where
  hap = ap

instance HSequence (OptNP empty) where
  hctraverse' = ctraverse'
  htraverse'  = hctraverse' (Proxy @Top)
  hsequence'  = htraverse' unComp

{-------------------------------------------------------------------------------
  View
-------------------------------------------------------------------------------}

data ViewOptNP f xs where
  OptNP_ExactlyOne :: f x -> ViewOptNP f '[x]
  OptNP_AtLeastTwo ::        ViewOptNP f (x ': y ': zs)

view :: forall f xs. OptNP 'False f xs -> ViewOptNP f xs
view = \case
    OptCons x  OptNil       -> OptNP_ExactlyOne x
    OptCons _ (OptCons _ _) -> OptNP_AtLeastTwo
    OptCons _ (OptSkip _)   -> OptNP_AtLeastTwo
    OptSkip   (OptCons _ _) -> OptNP_AtLeastTwo
    OptSkip   (OptSkip _)   -> OptNP_AtLeastTwo

{-------------------------------------------------------------------------------
  Combining
-------------------------------------------------------------------------------}

type family And (x :: Bool) (y :: Bool) :: Bool where
  And 'True  y      = y
  And 'False _      = 'False
  And _      'False = 'False

zipWith ::
     forall f g h empty1 empty2 xs.
     (forall a. These1 f g a -> h a)
  -> OptNP empty1              f xs
  -> OptNP empty2              g xs
  -> OptNP (And empty1 empty2) h xs
zipWith f = go
  where
    go :: OptNP empty1'               f xs'
       -> OptNP empty2'               g xs'
       -> OptNP (And empty1' empty2') h xs'
    go OptNil         OptNil         = OptNil
    go (OptCons x xs) (OptSkip   ys) = OptCons (f (This1  x  )) (go xs ys)
    go (OptSkip   xs) (OptCons y ys) = OptCons (f (That1    y)) (go xs ys)
    go (OptCons x xs) (OptCons y ys) = OptCons (f (These1 x y)) (go xs ys)
    go (OptSkip   xs) (OptSkip   ys) = OptSkip                  (go xs ys)

combineWith ::
     SListI xs
  => (forall a. These1 f g a -> h a)
  -> Maybe (OptNP 'False f xs)
  -> Maybe (OptNP 'False g xs)
  -> Maybe (OptNP 'False h xs)
combineWith _ Nothing   Nothing   = Nothing
combineWith f (Just xs) Nothing   = Just $ zipWith f xs    empty
combineWith f Nothing   (Just ys) = Just $ zipWith f empty ys
combineWith f (Just xs) (Just ys) = Just $ zipWith f xs    ys

-- | Precondition: there is no overlap between the two given lists: if there is
-- a 'Just' at a given position in one, it must be 'Nothing' at the same
-- position in the other.
combine ::
     forall (f :: Type -> Type) xs.
     -- 'These1' is not kind-polymorphic
     (SListI xs, HasCallStack)
  => Maybe (OptNP 'False f xs)
  -> Maybe (OptNP 'False f xs)
  -> Maybe (OptNP 'False f xs)
combine = combineWith $ \case
    This1 x   -> x
    That1 y   -> y
    These1 {} -> error "combine: precondition violated"

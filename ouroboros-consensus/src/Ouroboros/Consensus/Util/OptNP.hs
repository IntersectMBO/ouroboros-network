{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE EmptyCase            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
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
-- > import           Ouroboros.Consensus.Util.OptNP (OptNP (..))
-- > import qualified Ouroboros.Consensus.Util.OptNP as OptNP
module Ouroboros.Consensus.Util.OptNP (
    OptNP(..)
  , empty
  , toNP
  , singleton
  , fromSingleton
  ) where

import           Control.Monad (guard)
import           Data.Kind (Type)
import           Data.Maybe (isJust)
import           Data.SOP.Strict
import           Data.Type.Equality

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

toNP :: OptNP empty f xs -> NP (Maybe :.: f) xs
toNP = go
  where
    go :: OptNP empty f xs -> NP (Maybe :.: f) xs
    go OptNil         = Nil
    go (OptCons x xs) = Comp (Just x) :* go xs
    go (OptSkip   xs) = Comp Nothing  :* go xs

singleton :: f x -> OptNP 'False f '[x]
singleton x = OptCons x OptNil

-- | If 'OptNP' is not empty, it must contain at least one value
fromSingleton :: OptNP 'False f '[x] -> f x
fromSingleton = go
  where
    go :: OptNP 'False f '[x] -> f x
    go (OptCons x _) = x
    go (OptSkip xs)  = case xs of {}

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

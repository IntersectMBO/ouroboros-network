{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE EmptyCase            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Util.SOP (
    -- * Minor variations on standard SOP operators
    sequence_NS'
  , map_NP'
  , partition_NS
  , npWithIndices
  , nsToIndex
  , nsFromIndex
  , Lens(..)
  , lenses_NP
  , npToSListI
  , allComposeShowK
  , fn_5
    -- * Type-level non-empty lists
  , IsNonEmpty(..)
  , ProofNonEmpty(..)
  , checkIsNonEmpty
    -- * NP with optional values
  , OptNP(..)
  , fromOptNP
  , singletonOptNP
  , fromSingletonOptNP
  ) where

import           Data.SOP.Dict
import           Data.SOP.Strict
import           Data.Word

{-------------------------------------------------------------------------------
  Minor variations on standard SOP operators
-------------------------------------------------------------------------------}

-- | Version of 'sequence_NS' that requires only 'Functor'
--
-- The version in the library requires 'Applicative', which is unnecessary.
sequence_NS' :: forall xs f g. Functor f
             => NS (f :.: g) xs -> f (NS g xs)
sequence_NS' = go
  where
    go :: NS (f :.: g) xs' -> f (NS g xs')
    go (Z (Comp fx)) = Z <$> fx
    go (S r)         = S <$> go r

-- | Version of 'map_NP' that does not require a singleton
map_NP' :: forall f g xs. (forall a. f a -> g a) -> NP f xs -> NP g xs
map_NP' f = go
  where
    go :: NP f xs' -> NP g xs'
    go Nil       = Nil
    go (x :* xs) = f x :* go xs

partition_NS :: forall xs f. SListI xs => [NS f xs] -> NP ([] :.: f) xs
partition_NS =
      foldr (hzipWith append) (hpure nil)
    . map (hexpand nil . hmap singleton)
  where
    nil :: ([] :.: f) a
    nil = Comp []

    singleton :: f a -> ([] :.: f) a
    singleton = Comp . (:[])

    append :: ([] :.: f) a -> ([] :.: f) a -> ([] :.: f) a
    append (Comp as) (Comp as') = Comp (as ++ as')

-- | We only allow up to 23 (so counting from 0, 24 elements in @xs@), because
-- CBOR stores a 'Word8' in the range 0-23 as a single byte equal to the value
-- of the 'Word8'. We rely on this in 'reconstructNestedCtxt' and other
-- places.
npWithIndices :: SListI xs => NP (K Word8) xs
npWithIndices = go 0 sList
  where
    go :: Word8 -> SList xs' -> NP (K Word8) xs'
    go !_ SNil  = Nil
    go 24 SCons = error "npWithIndices out of range"
    go !i SCons = K i :* go (i + 1) sList

nsToIndex :: SListI xs => NS f xs -> Word8
nsToIndex = hcollapse . hzipWith const npWithIndices

-- | We only allow up to 23, see 'npWithIndices'.
nsFromIndex :: SListI xs => Word8 -> Maybe (NS (K ()) xs)
nsFromIndex n = go 0 sList
  where
    go :: Word8 -> SList xs' -> Maybe (NS (K ()) xs')
    go !i SCons
      | i == 24   = error "nsFromIndex out of range"
      | i == n    = Just $ Z $ K ()
      | otherwise = S <$> go (i + 1) sList
    go !_ SNil    = Nothing

-- | Simple lens to access an element of an n-ary product.
data Lens f xs a = Lens {
      getter :: NP f xs -> f a
    , setter :: f a -> NP f xs -> NP f xs
    }

-- | Generate all lenses to access the element of an n-ary product.
lenses_NP :: forall f xs. SListI xs => NP (Lens f xs) xs
lenses_NP = go sList
  where
    go :: SList xs' -> NP (Lens f xs') xs'
    go SNil  = Nil
    go SCons = lensFirst :* hmap shiftLens (go sList)

    lensFirst :: Lens f (x ': xs') x
    lensFirst = Lens {
          getter = hd
        , setter = \a' (_ :* as) -> a' :* as
        }

    shiftLens :: Lens f xs' a -> Lens f (x ': xs') a
    shiftLens l = Lens {
          getter = getter l . tl
        , setter = \a' (a :* as) -> a :* setter l a' as
        }

-- | Conjure up an 'SListI' constraint from an 'NP'
npToSListI :: NP a xs -> (SListI xs => r) -> r
npToSListI = sListToSListI . npToSList
  where
    sListToSListI :: SList xs -> (SListI xs => r) -> r
    sListToSListI SNil  k = k
    sListToSListI SCons k = k

    npToSList :: NP a xs -> SList xs
    npToSList Nil       = SNil
    npToSList (_ :* xs) = sListToSListI (npToSList xs) SCons

allComposeShowK :: (SListI xs, Show a)
                => Proxy xs -> Proxy a -> Dict (All (Compose Show (K a))) xs
allComposeShowK _ _ = all_NP $ hpure Dict

fn_5 :: (f0 a -> f1 a -> f2 a -> f3 a -> f4 a -> f5 a)
     -> (f0 -.-> f1 -.-> f2 -.-> f3 -.-> f4 -.-> f5) a
fn_5 f = Fn $ \x0 ->
         Fn $ \x1 ->
         Fn $ \x2 ->
         Fn $ \x3 ->
         Fn $ \x4 ->
         f x0 x1 x2 x3 x4

{-------------------------------------------------------------------------------
  Type-level non-empty lists
-------------------------------------------------------------------------------}

data ProofNonEmpty :: [*] -> * where
  ProofNonEmpty :: Proxy x -> Proxy xs -> ProofNonEmpty (x ': xs)

class IsNonEmpty xs where
  isNonEmpty :: proxy xs -> ProofNonEmpty xs

instance IsNonEmpty (x ': xs) where
  isNonEmpty _ = ProofNonEmpty (Proxy @x) (Proxy @xs)

checkIsNonEmpty :: forall xs. SListI xs => Proxy xs -> Maybe (ProofNonEmpty xs)
checkIsNonEmpty _ = case sList @xs of
    SNil  -> Nothing
    SCons -> Just $ ProofNonEmpty Proxy Proxy

{-------------------------------------------------------------------------------
  NP with optional values
-------------------------------------------------------------------------------}

-- | Like an 'NP', but with optional values
data OptNP (allowedEmpty :: Bool) (f :: k -> *) (xs :: [k]) where
  OptNil  :: OptNP 'True f '[]
  OptCons :: !(f x) -> !(OptNP 'True f xs) -> OptNP allowedEmpty f (x ': xs)
  OptSkip :: !(OptNP allowedEmpty f xs) -> OptNP allowedEmpty f (x ': xs)

type instance AllN    (OptNP allowedEmpty) c = All c
type instance SListIN (OptNP allowedEmpty)   = SListI
type instance Prod    (OptNP allowedEmpty)   = NP

deriving instance All (Show `Compose` f) xs => Show (OptNP allowedEmpty f xs)
deriving instance All (Eq   `Compose` f) xs => Eq   (OptNP allowedEmpty f xs)
deriving instance ( All (Eq   `Compose` f) xs
                  , All (Ord  `Compose` f) xs
                  ) => Ord (OptNP allowedEmpty f xs)

fromOptNP :: OptNP allowedEmpty f xs -> NP (Maybe :.: f) xs
fromOptNP = go
  where
    go :: OptNP allowedEmpty f xs -> NP (Maybe :.: f) xs
    go OptNil         = Nil
    go (OptCons x xs) = Comp (Just x) :* go xs
    go (OptSkip   xs) = Comp Nothing  :* go xs

singletonOptNP :: f x -> OptNP allowedEmpty f '[x]
singletonOptNP x = OptCons x OptNil

-- | If 'OptNP' is not allowed to be empty, it must contain at least one value
fromSingletonOptNP :: OptNP 'False f '[x] -> f x
fromSingletonOptNP = go
  where
    go :: OptNP 'False f '[x] -> f x
    go (OptCons x _) = x
    go (OptSkip xs)  = case xs of {}

ap_OptNP :: NP (f -.-> g) xs
         -> OptNP allowedEmpty f xs
         -> OptNP allowedEmpty g xs
ap_OptNP = go
  where
    go :: NP (f -.-> g) xs -> OptNP allowedEmpty f xs -> OptNP allowedEmpty g xs
    go (f :* fs) (OptCons x xs) = OptCons (apFn f x) (go fs xs)
    go (_ :* fs) (OptSkip   xs) = OptSkip            (go fs xs)
    go Nil       OptNil         = OptNil

ctraverse'_OptNP ::
     forall c proxy allowedEmpty xs f f' g. (All c xs, Applicative g)
  => proxy c
  -> (forall a. c a => f a -> g (f' a))
  -> OptNP allowedEmpty f xs  -> g (OptNP allowedEmpty f' xs)
ctraverse'_OptNP _ f = go
  where
    go :: All c ys => OptNP allowedEmpty' f ys -> g (OptNP allowedEmpty' f' ys)
    go (OptCons x xs) = OptCons <$> f x <*> go xs
    go (OptSkip   xs) = OptSkip <$>         go xs
    go OptNil         = pure OptNil

instance HAp (OptNP allowedEmpty) where
  hap = ap_OptNP

instance HSequence (OptNP allowedEmpty) where
  hctraverse' = ctraverse'_OptNP
  htraverse'  = hctraverse' (Proxy @Top)
  hsequence'  = htraverse' unComp

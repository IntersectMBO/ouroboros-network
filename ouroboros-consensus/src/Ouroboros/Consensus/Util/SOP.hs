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
    Lens (..)
  , allComposeShowK
  , fn_5
  , lenses_NP
  , map_NP'
  , npToSListI
  , npWithIndices
  , nsFromIndex
  , nsToIndex
  , partition_NS
  , sequence_NS'
    -- * Type-level non-empty lists
  , IsNonEmpty (..)
  , ProofNonEmpty (..)
  , checkIsNonEmpty
    -- * Indexing SOP types
  , Index (..)
  , dictIndexAll
  , indices
  , injectNS
  , injectNS'
  , projectNP
    -- * Zipping with indices
  , hcimap
  , hcizipWith
  , hcizipWith3
  , hcizipWith4
  , himap
  , hizipWith
  , hizipWith3
  , hizipWith4
  ) where

import           Data.Coerce
import           Data.Kind (Type)
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
npToSListI np = sListToSListI $ npToSList np
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

data ProofNonEmpty :: [Type] -> Type where
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
  Indexing SOP types
-------------------------------------------------------------------------------}

data Index xs x where
  IZ ::               Index (x ': xs) x
  IS :: Index xs x -> Index (y ': xs) x

indices :: forall xs. SListI xs => NP (Index xs) xs
indices = case sList @xs of
    SNil  -> Nil
    SCons -> IZ :* hmap IS indices

dictIndexAll :: All c xs => Proxy c -> Index xs x -> Dict c x
dictIndexAll p = \case
    IZ      -> Dict
    IS idx' -> dictIndexAll p idx'

injectNS :: forall f x xs. Index xs x -> f x -> NS f xs
injectNS idx x = case idx of
    IZ      -> Z x
    IS idx' -> S (injectNS idx' x)

injectNS' ::
     forall f a b x xs. (Coercible a (f x), Coercible b (NS f xs))
  => Proxy f -> Index xs x -> a -> b
injectNS' _ idx = coerce . injectNS @f idx . coerce

projectNP :: Index xs x -> NP f xs -> f x
projectNP IZ        (x :* _) = x
projectNP (IS idx) (_ :* xs) = projectNP idx xs

{-------------------------------------------------------------------------------
  Zipping with indices
-------------------------------------------------------------------------------}

hcimap ::
     (HAp h, All c xs, Prod h ~ NP)
  => proxy c
  -> (forall a. c a => Index xs a -> f1 a -> f2 a)
  -> h f1 xs
  -> h f2 xs
hcimap p f xs1 =
    hcpure p (fn_2 f)
      `hap` indices
      `hap` xs1

himap ::
     (HAp h, SListI xs, Prod h ~ NP)
  => (forall a. Index xs a -> f1 a -> f2 a)
  -> h f1 xs
  -> h f2 xs
himap = hcimap (Proxy @Top)

hcizipWith ::
     (HAp h, All c xs, Prod h ~ NP)
  => proxy c
  -> (forall a. c a => Index xs a -> f1 a -> f2 a -> f3 a)
  -> NP f1 xs
  -> h  f2 xs
  -> h  f3 xs
hcizipWith p f xs1 xs2 =
    hcpure p (fn_3 f)
      `hap` indices
      `hap` xs1
      `hap` xs2

hizipWith ::
     (HAp h, SListI xs, Prod h ~ NP)
  => (forall a. Index xs a -> f1 a -> f2 a -> f3 a)
  -> NP f1 xs
  -> h  f2 xs
  -> h  f3 xs
hizipWith = hcizipWith (Proxy @Top)

hcizipWith3 ::
     (HAp h, All c xs, Prod h ~ NP)
  => proxy c
  -> (forall a. c a => Index xs a -> f1 a -> f2 a -> f3 a -> f4 a)
  -> NP f1 xs
  -> NP f2 xs
  -> h  f3 xs
  -> h  f4 xs
hcizipWith3 p f xs1 xs2 xs3 =
    hcpure p (fn_4 f)
      `hap` indices
      `hap` xs1
      `hap` xs2
      `hap` xs3

hizipWith3 ::
     (HAp h, SListI xs, Prod h ~ NP)
  => (forall a. Index xs a -> f1 a -> f2 a -> f3 a -> f4 a)
  -> NP f1 xs
  -> NP f2 xs
  -> h  f3 xs
  -> h  f4 xs
hizipWith3 = hcizipWith3 (Proxy @Top)

hcizipWith4 ::
     (HAp h, All c xs, Prod h ~ NP)
  => proxy c
  -> (forall a. c a => Index xs a -> f1 a -> f2 a -> f3 a -> f4 a -> f5 a)
  -> NP f1 xs
  -> NP f2 xs
  -> NP f3 xs
  -> h  f4 xs
  -> h  f5 xs
hcizipWith4 p f xs1 xs2 xs3 xs4 =
    hcpure p (fn_5 f)
      `hap` indices
      `hap` xs1
      `hap` xs2
      `hap` xs3
      `hap` xs4

hizipWith4 ::
     (HAp h, SListI xs, Prod h ~ NP)
  => (forall a. Index xs a -> f1 a -> f2 a -> f3 a -> f4 a -> f5 a)
  -> NP f1 xs
  -> NP f2 xs
  -> NP f3 xs
  -> h  f4 xs
  -> h  f5 xs
hizipWith4 = hcizipWith4 (Proxy @Top)

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
-- |

module Data.SOP.Index (
    -- * Indexing SOP types
    Index (..)
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
    -- * Indices with Word
  , npWithIndices
  , nsFromIndex
  , nsToIndex
  ) where

import           Data.Coerce
import           Data.SOP.Dict
import           Data.SOP.Strict
import           Data.Word

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

{-------------------------------------------------------------------------------
 Indices with Word
-------------------------------------------------------------------------------}

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

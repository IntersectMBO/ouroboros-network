{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Test.Ouroboros.Consensus.ChainGenerator.Slot (
  -- * Counting
  E (ActiveSlotE, EmptySlotE, SlotE),
  complementActive,
  complementEmpty,
  -- * Slot
  S,
  genAsc,
  Test.Ouroboros.Consensus.ChainGenerator.Slot.showS,
  -- * Reuse
  POL (mkActive, test),
  Pol (Inverted, NotInverted),
  PreImage,
  inverted,
  notInverted,
  ) where

import           Data.Coerce (coerce)
import           Data.Proxy (Proxy (Proxy))
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as MVG
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import qualified System.Random.Stateful as R
import qualified Test.Ouroboros.Consensus.ChainGenerator.Counting as C
import           Test.Ouroboros.Consensus.ChainGenerator.Params (Asc, ascVal)
import qualified Test.QuickCheck as QC

-- | The activeness of some slot
newtype S = S Bool
  deriving (QC.Arbitrary, Eq, Ord, Read, Show)

-- these instances adapted from https://github.com/minoki/unboxing-vector/blob/3a152014b9660ef1e2885d6b9c66423064223f63/test/Foo.hs#L36-L63
--
-- vector 0.13 lets us derive the two big instances; see the top of https://hackage.haskell.org/package/vector-0.13.0.0/docs/Data-Vector-Unboxed.html
--
-- TODO do so once we eventually bump our dependency on vector to include that feature
newtype instance MV.MVector s S = MV_S (MV.MVector s Bool)
newtype instance V.Vector     S = V_S (V.Vector Bool)
instance MVG.MVector MV.MVector S where
    basicLength (MV_S mv)                = MVG.basicLength mv
    basicUnsafeSlice i l (MV_S mv)       = MV_S (MVG.basicUnsafeSlice i l mv)
    basicOverlaps (MV_S mv) (MV_S mv')   = MVG.basicOverlaps mv mv'
    basicUnsafeNew l                     = MV_S <$> MVG.basicUnsafeNew l
    basicInitialize (MV_S mv)            = MVG.basicInitialize mv
    basicUnsafeReplicate i x             = MV_S <$> MVG.basicUnsafeReplicate i (coerce x)
    basicUnsafeRead (MV_S mv) i          = coerce <$> MVG.basicUnsafeRead mv i
    basicUnsafeWrite (MV_S mv) i x       = MVG.basicUnsafeWrite mv i (coerce x)
    basicClear (MV_S mv)                 = MVG.basicClear mv
    basicSet (MV_S mv) x                 = MVG.basicSet mv (coerce x)
    basicUnsafeCopy (MV_S mv) (MV_S mv') = MVG.basicUnsafeCopy mv mv'
    basicUnsafeMove (MV_S mv) (MV_S mv') = MVG.basicUnsafeMove mv mv'
    basicUnsafeGrow (MV_S mv) n          = MV_S <$> MVG.basicUnsafeGrow mv n
instance VG.Vector V.Vector S where
    basicUnsafeFreeze (MV_S mv)       = V_S <$> VG.basicUnsafeFreeze mv
    basicUnsafeThaw (V_S v)           = MV_S <$> VG.basicUnsafeThaw v
    basicLength (V_S v)               = VG.basicLength v
    basicUnsafeSlice i l (V_S v)      = V_S (VG.basicUnsafeSlice i l v)
    basicUnsafeIndexM (V_S v) i       = coerce <$> VG.basicUnsafeIndexM v i
    basicUnsafeCopy (MV_S mv) (V_S v) = VG.basicUnsafeCopy mv v
    elemseq (V_S v) x y               = VG.elemseq v (coerce x) y
instance V.Unbox S

-----

genAsc :: R.RandomGen g => Asc -> g -> (S, g)
genAsc asc g =
    bool `seq` (S bool, g')
  where
    (q, g') = R.random g   -- note 0 <= q <= 1

    bool = q < ascVal asc

showS :: S -> ShowS
showS (S bool) = showChar $ if bool then '1' else '0'

-----

-- | The different kinds of element counted in this library
data E = ActiveSlotE | EmptySlotE | SlotE

inverted :: Proxy Inverted
inverted = Proxy

notInverted :: Proxy NotInverted
notInverted = Proxy

-- | Many functions in this library are parameterized over a type of kind @Pol@
--
-- If the type is 'Inverted', then the function will treat all 'S' values as if they were first complemented.
--
-- The 'PreImage' family does the corresponding parameterization of 'ActiveSlotE' and 'EmptySlotE' at the type level.
--
-- NOTE: No 'S' value is ever actually complemented because of 'Inverted', but
-- the functions parameterized by 'POL' will treat them as if they were.
data Pol = Inverted | NotInverted

-- | Data for 'Pol'
class POL (pol :: Pol) where
    -- | Make on active slot
    mkActive :: proxy pol -> S
    -- | Test whether @pol@ maps the given bit to one
    test :: proxy pol -> S -> Bool

-- | Every slot is either active or empty
complementActive :: proxy pol -> C.Size base SlotE -> C.Count base (PreImage pol ActiveSlotE) which -> C.Count base (PreImage pol EmptySlotE) which
complementActive _pol (C.Count n) (C.Count i) = C.Count (n - i)

-- | Every slot is either active or empty
complementEmpty :: proxy pol -> C.Size base SlotE -> C.Count base (PreImage pol EmptySlotE) which -> C.Count base (PreImage pol ActiveSlotE) which
complementEmpty _pol (C.Count n) (C.Count i) = C.Count (n - i)

instance POL Inverted where
    mkActive _pol = coerce False
    test     _pol = coerce not

instance POL NotInverted where
    mkActive _pol = coerce True
    test     _pol = coerce

-- | @PreImage pol e@ is the complement of @e@ if @pol@ is 'Inverted' and simply @e@ if it's 'NotInverted'
type family PreImage (pol :: Pol) (e :: E) where
    PreImage Inverted    EmptySlotE  = ActiveSlotE
    PreImage Inverted    ActiveSlotE = EmptySlotE
    PreImage NotInverted e           = e

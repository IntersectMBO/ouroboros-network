{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}

-- Note: Parts of the documentation are based on/are directly copied from
-- documentation in the @Data.FingerTree.Strict@ module.
module Data.FingerTree.RootMeasured.Strict (
    -- * Strict finger trees with root measures
    StrictFingerTree
    -- * Measuring
  , Measured (..)
  , RootMeasured (..)
  , SuperMeasured
    -- * Construction
  , fromList
  , (|>)
    -- * Splitting
  , LR (..)
  , split
  , splitl
  , splitlr
  , splitr
    -- * Maps
  , fmap'
  , fmap''
  ) where

import           Data.Foldable
import           Data.Group
import           GHC.Generics (Generic)

import           NoThunks.Class (NoThunks (..), noThunksInValues)

import           Data.FingerTree.Strict (Measured)
import qualified Data.FingerTree.Strict as FT

{-------------------------------------------------------------------------------
  Strict finger trees with root measures
-------------------------------------------------------------------------------}

-- | A @StrictFingerTree@ with elements of type @a@, an internal measure of type
-- @vi@, and a root measure of type @vt@.
data StrictFingerTree vt vi a = SFT {
    tm       :: vt
  , elements :: !(FT.StrictFingerTree vi a)
  }
  deriving (Show, Eq, Ord, Generic)

instance Foldable (StrictFingerTree vt vi) where
  foldMap f = foldMap f . elements

instance NoThunks a => NoThunks (StrictFingerTree vt vi a) where
  showTypeOf _ = "StrictFingerTree'"
  wNoThunks ctxt = noThunksInValues ctxt . toList

instance (Semigroup vt, Measured vi a)
      => Semigroup (StrictFingerTree vt vi a) where
  SFT tm1 xs1 <> SFT tm2 xs2 = SFT (tm1 <> tm2) (xs1 FT.>< xs2)

instance (Monoid vt, Measured vi a) => Monoid (StrictFingerTree vt vi a) where
  mempty = SFT mempty FT.empty

{-------------------------------------------------------------------------------
  Measuring
-------------------------------------------------------------------------------}

-- | All @StrictFingerTree@s are internally measured.
instance Measured vi a => Measured vi (StrictFingerTree vt vi a) where
  measure = FT.measure . elements

-- | Re-iteration of @'Measured'@, but for root measures.
--
-- This re-iteration is necessary because we want to allow the root measure to
-- be distinct from the internal measure. For example, we can not create both of
-- these instances for distinct types @T@ and @T'@:
--
-- > @instance Measured T  a where -- ...@
--
-- > @instance Measured T' a where -- ...@
--
-- Furthermore, we want the root measure to be a @'Group'@ instead of a
-- @'Monoid'@.
class Monoid v => RootMeasured v a | a -> v where
  measureRoot :: a -> v

-- | All @StrictFingerTree@s are root measured.
instance RootMeasured vt a => RootMeasured vt (StrictFingerTree vt vi a) where
  measureRoot = tm

-- | Conjunction of @RootMeasured@ and @Measured@ constraints.
type SuperMeasured vt vi a = (RootMeasured vt a, Measured vi a)

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

infixl 5 |>

-- | /O(1)/. Add an element to the right end of a sequence.
--
-- Mnemonic: a triangle with the single element at the pointy end.
(|>) ::
     SuperMeasured vt vi a
  => StrictFingerTree vt vi a
  -> a
  -> StrictFingerTree vt vi a
SFT vt sft |> (!a) = SFT (vt <> measureRoot a) (sft FT.|> a)

-- | /O(n)/. Create a sequence from a finite list of elements. The opposite
-- operation 'toList' is supplied by the 'Foldable' instance.
fromList :: SuperMeasured vt vi a => [a] -> StrictFingerTree vt vi a
fromList !xs = SFT (foldMap measureRoot xs) (FT.fromList xs)

{-------------------------------------------------------------------------------
  Splitting
-------------------------------------------------------------------------------}

-- | /O(?)/. Split a sequence at a point where the predicate on the accumulated
-- /internal/ measure of the prefix changes from 'False' to 'True'.
--
-- For predictable results, one should ensure that there is only one such point,
-- i.e. that the predicate is /monotonic/.
--
-- A function @f@ should be provided that computes the root measures of the left
-- and right parts of the split. If the @vt@ type has a @'Group'@ instance, then
-- @f@ is defined for /free/: see the @'splitl'@ and @'splitr'@ variants of the
-- @'split'@ function.
--
-- TODO(jdral): Complexity analysis.
split ::
     SuperMeasured vt vi a
  => (vi -> Bool)
  -> (vt -> (vt, vt) -> (vt, vt))
  -> StrictFingerTree vt vi a
  -> ( StrictFingerTree vt vi a
     , StrictFingerTree vt vi a
     )
split p f (SFT vt sft) = (SFT vtLeft left, SFT vtRight right)
  where
    (left, right)     = FT.split p sft
    (vtLeft, vtRight) = f vt (foldMap measureRoot left, foldMap measureRoot right)

-- | Data type representing either /left/ or /right/.
data LR = L | R
  deriving (Show, Eq)

-- | Like @'split'@, but we compute to-measures for /free/ trough subtraction of
-- root measures.
--
-- Redirects to the @'splitl'@ and @'splitr'@ functions based on the @'LR'@
-- argument. Depending on which part of the split is shorter, redirecting to
-- @'splitl'@ or @'splitr'@ can be more performant. See @'splitl'@ and
-- @'splitr'@.
splitlr ::
     ( SuperMeasured vt vi a
     , Group vt
     )
  => LR
  -> (vi -> Bool)
  -> StrictFingerTree vt vi a
  -> ( StrictFingerTree vt vi a
     , StrictFingerTree vt vi a
     )
splitlr = \case
  L -> splitl
  R -> splitr

-- | Like @'split'@, but we compute root measures for /free/ through subtraction
-- of the left part's root measure.
--
-- This function is more performant than @'splitr'@ if the left part of the
-- split is shorter than the right part.
--
-- TODO(jdral): Complexity analysis.
splitl ::
     ( SuperMeasured vt vi a
     , Group vt
     )
  => (vi -> Bool)
  -> StrictFingerTree vt vi a
  -> ( StrictFingerTree vt vi a
     , StrictFingerTree vt vi a
     )
splitl p = split p f
  where
    f vt (vtLeft, _vtRight) = (vtLeft, invert vtLeft <> vt)

-- | Like @'split'@, but we compute root measures for /free/ through subtraction
-- of the right part's root measure.
--
-- This function is more performant than @'splitl'@ if the right part of the
-- split is shorter than the left part.
--
-- TODO(jdral): Complexity analysis.
splitr ::
     ( SuperMeasured vt vi a
     , Group vt
     )
  => (vi -> Bool)
  -> StrictFingerTree vt vi a
  -> ( StrictFingerTree vt vi a
     , StrictFingerTree vt vi a
     )
splitr p = split p f
  where
    f vt (_vtLeft, vtRight) = (vt <> invert vtRight, vtRight)


{-------------------------------------------------------------------------------
  Maps
-------------------------------------------------------------------------------}

-- | Like @'fmap'@, but with constraints on the element types.
--
-- Note: @vt2@ is reconstructed in time linear in the size of the finger tree.
fmap' ::
     ( SuperMeasured vt1 vi1 a1
     , SuperMeasured vt2 vi2 a2
     )
  => (a1 -> a2)
  -> StrictFingerTree vt1 vi1 a1
  -> StrictFingerTree vt2 vi2 a2
fmap' f (SFT _ sft) = SFT vt' sft'
  where
    sft' = FT.fmap' f sft
    vt' = foldMap measureRoot sft'

-- | Like @'fmap''@, but without the linear-time reconstruction of the root
-- level measure.
--
-- Though similar to @'fmap''@, this function also requires a function parameter
-- of root measures to root measures. This function ensures that we do not have
-- to reconstruct @vt2@ from the elements of the finger tree.
fmap'' ::
     ( SuperMeasured vt1 vi1 a1
     , SuperMeasured vt2 vi2 a2
     )
  => (a1 -> a2)
  -> (vt1 -> vt2)
  -> StrictFingerTree vt1 vi1 a1
  -> StrictFingerTree vt2 vi2 a2
fmap'' f g (SFT vt sft) = SFT vt' sft'
  where
    sft' = FT.fmap' f sft
    vt' = g vt

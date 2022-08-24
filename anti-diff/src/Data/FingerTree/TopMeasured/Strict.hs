{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}

-- Note: Parts of the documentation are based on/are directly copied from
-- documentation in the @Data.FingerTree.Strict@ module.
module Data.FingerTree.TopMeasured.Strict (
    -- * Strict finger trees with top-level measures
    StrictFingerTree
    -- * Measuring
  , Measured (..)
  , SuperMeasured
  , TopMeasured (..)
    -- * Construction
  , fromList
  , (|>)
    -- * Splitting
  , split
  , split'
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
  Strict finger trees with top-level measures
-------------------------------------------------------------------------------}

-- | A @StrictFingerTree@ with elements of type @a@, an internal measure
-- of type @vi@, and a top-level measure of type @vt@.
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
  SFT tm1 xs1 <> SFT tm2 xs2 = SFT (tm1 <> tm2) (xs1 <> xs2)

instance (Monoid vt, Measured vi a) => Monoid (StrictFingerTree vt vi a) where
  mempty = SFT mempty mempty

{-------------------------------------------------------------------------------
  Measuring
-------------------------------------------------------------------------------}

-- | All @StrictFingerTree@s are internal-measured.
instance Measured vi a => Measured vi (StrictFingerTree vt vi a) where
  measure = FT.measure . elements

-- | Re-iteration of @'Measured'@, but for top-level measures.
--
-- This re-iteration is necessary because we want to allow the top-level measure
-- to be distinct from the internal measure. For example, we can not create both
-- of these instances:
-- > @instance Measured T  a where -- ...@
-- > @instance Measured T' a where -- ...@
-- Furthermore, we want the top-level measure to be a @'Group'@ instead of a
-- @'Monoid'@.
class Monoid v => TopMeasured v a | a -> v where
  measureTop :: a -> v

-- | All @StrictFingerTree@s are top-measured.
instance TopMeasured vt a => TopMeasured vt (StrictFingerTree vt vi a) where
  measureTop = tm

-- | Conjunction of @TopMeasured@ and @InternalMeasured@ constraints.
type SuperMeasured vt vi a = (TopMeasured vt a, Measured vi a)

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

infixl 5 |>

-- | /O(1)/. Add an element to the right end of a sequence.
-- Mnemonic: a triangle with the single element at the pointy end.
(|>) ::
     SuperMeasured vt vi a
  => StrictFingerTree vt vi a
  -> a
  -> StrictFingerTree vt vi a
SFT vt sft |> (!a) = SFT (vt <> measureTop a) (sft FT.|> a)

-- | /O(n)/. Create a sequence from a finite list of elements.
-- The opposite operation 'toList' is supplied by the 'Foldable' instance.
fromList :: SuperMeasured vt vi a => [a] -> StrictFingerTree vt vi a
fromList !xs = SFT (foldMap measureTop xs) (FT.fromList xs)

{-------------------------------------------------------------------------------
  Splitting
-------------------------------------------------------------------------------}

-- | /O(?)/. Split a sequence at a point where the predicate on the accumulated
-- /internal/ measure of the prefix changes from 'False' to 'True'.
--
-- For predictable results, one should ensure that there is only one such
-- point, i.e. that the predicate is /monotonic/.
--
-- TODO(jdral): Complexity analysis.
split ::
     ( SuperMeasured vt vi a
     , Group vt
     )
  => (vi -> Bool)
  -> StrictFingerTree vt vi a
  -> ( StrictFingerTree vt vi a
     , StrictFingerTree vt vi a
     )
split p = split' p f
  where
    -- TODO(jdral): There are two options for constructing the results from
    -- @left@ and @right@ parts that are obtained here. Either we invert the
    -- top-level measure of the @left@ part and use that in our construction,
    -- or we invert the top-level measure of the @right@ part and use that in
    -- our construction. The choice depends on the sizes of @left@ and
    -- @right@. Should the @split@ function take this into account, or should
    -- we introduce variants of the @split@ function?
    f vt (vtLeft, _vtRight) = (vtLeft, invert vtLeft <> vt)

-- | Like 'split', but does not require a 'Group' instance. Instead, the
-- function should be provided a function that computes the top-measures of
-- the left and right parts of the split.
split' ::
     SuperMeasured vt vi a
  => (vi -> Bool)
  -> (vt -> (vt, vt) -> (vt, vt))
  -> StrictFingerTree vt vi a
  -> ( StrictFingerTree vt vi a
     , StrictFingerTree vt vi a
     )
split' p f (SFT vt sft) = (SFT vtLeft left, SFT vtRight right)
  where
    (left, right)     = FT.split p sft
    (vtLeft, vtRight) = f vt (foldMap measureTop left, foldMap measureTop right)

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
    vt' = foldMap measureTop sft'

-- | Like @'fmap''@, but without the linear-time reconstruction of the top-level
-- measure.
--
-- Though similar to @'fmap''@, this function also requires a function parameter
-- of top-level measures to top-level measures. This function ensures that we
-- do not have to reconstruct @vt2@ from the elements of the finger tree.
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

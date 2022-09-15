{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}

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
  , SplitRootMeasure (..)
  , split
  , splitl
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
-- @vi@, and a root measure of type @vr@.
data StrictFingerTree vr vi a = SFT {
    rm       :: vr
  , elements :: !(FT.StrictFingerTree vi a)
  }
  deriving (Show, Eq, Ord, Generic)

instance Foldable (StrictFingerTree vr vi) where
  foldMap f = foldMap f . elements

instance NoThunks a => NoThunks (StrictFingerTree vr vi a) where
  showTypeOf _ = "StrictFingerTree'"
  wNoThunks ctxt = noThunksInValues ctxt . toList

instance (Semigroup vr, Measured vi a)
      => Semigroup (StrictFingerTree vr vi a) where
  SFT tm1 xs1 <> SFT tm2 xs2 = SFT (tm1 <> tm2) (xs1 FT.>< xs2)

instance (Monoid vr, Measured vi a) => Monoid (StrictFingerTree vr vi a) where
  mempty = SFT mempty FT.empty

{-------------------------------------------------------------------------------
  Measuring
-------------------------------------------------------------------------------}

-- | All @'StrictFingerTree'@s are internally measured.
instance Measured vi a => Measured vi (StrictFingerTree vr vi a) where
  measure = FT.measure . elements

-- | Re-iteration of @'Measured'@, but for root measures.
--
-- This re-iteration is necessary because we want to allow the root measure to
-- be distinct from the internal measure. For example, we can not create both of
-- these instances for distinct types @T@ and @T'@:
--
-- > instance Measured T  a where -- ...
--
-- > instance Measured T' a where -- ...
--
-- Furthermore, we want the root measure to be a @'Group'@ instead of a
-- @'Monoid'@.
class Group v => RootMeasured v a | a -> v where
  measureRoot :: a -> v

-- | All @'StrictFingerTree'@s are root measured.
instance RootMeasured vr a => RootMeasured vr (StrictFingerTree vr vi a) where
  measureRoot = rm

-- | Conjunction of @'RootMeasured'@ and @'Measured'@ constraints.
type SuperMeasured vr vi a = (RootMeasured vr a, Measured vi a)

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

infixl 5 |>

-- | /O(1)/. Add an element to the right end of a sequence.
--
-- Mnemonic: a triangle with the single element at the pointy end.
(|>) ::
     SuperMeasured vr vi a
  => StrictFingerTree vr vi a
  -> a
  -> StrictFingerTree vr vi a
SFT vr sft |> (!a) = SFT (vr <> measureRoot a) (sft FT.|> a)

-- | /O(n)/. Create a sequence from a finite list of elements. The opposite
-- operation @'toList'@ is supplied by the @'Foldable'@ instance.
fromList :: SuperMeasured vr vi a => [a] -> StrictFingerTree vr vi a
fromList !xs = SFT (foldMap measureRoot xs) (FT.fromList xs)

{-------------------------------------------------------------------------------
  Splitting
-------------------------------------------------------------------------------}

-- | /O(log(min(i,n-i))) + O(f(l, r))/. Split a sequence at a point where the
-- predicate on the accumulated /internal/ measure of the prefix changes from
-- 'False' to 'True'.
--
-- For predictable results, one should ensure that there is only one such point,
-- i.e. that the predicate is /monotonic/.
--
-- A @'SplitRootMeasure'@ function @f@ should be provided that computes the root
-- measures of the left and right parts of the split. Since the @vr@ type has a
-- @'Group'@ instance, we can use the inversion operation from the @'Group'@
-- class to compute the root measures: see @'splitl'@ and @'splitr'@.
--
-- Note on time complexity: the @log@ factor comes from @'FT.split'@. Moreover,
-- the @log@ factor of the time complexity is determined by the smallest part of
-- the split: the result of /(min(i, n-i))/ is either @i@ or @n-i@, which are
-- the lengths of the split parts.
--
-- Denotations for time complexity: @n@ denotes the length of the input of the
-- split. @i@ denotes the length of left part of the split result. @l@ denotes
-- the left part of the split result. @r@ denotes the right part of the split
-- result. @f@ denotes a @'SplitRootMeasure'@ function. @length@ denotes a
-- function that computes the length of a finger tree.
--
-- TODO(jdral): Not only the length of the left and right parts of the split
-- determine the complexity of the split function, but also the time complexity
-- of monoidal sums for the type of root measures @vr@. Under the assumption
-- that the time complexity of monoidal sums does not vary too much (or is
-- constant), this heuristic is probably sufficient. In the future, we might
-- want to experiment with different heuristics (that are not too costly to
-- compute), which let us split in even more efficient ways.
split ::
     SuperMeasured vr vi a
  => (vi -> Bool)
  -> SplitRootMeasure vr vi a
  -> StrictFingerTree vr vi a
  -> ( StrictFingerTree vr vi a
     , StrictFingerTree vr vi a
     )
split p f (SFT vr sft) = (SFT vrLeft left, SFT vrRight right)
  where
    (left, right)     = FT.split p sft
    (vrLeft, vrRight) = unSplitRootMeasure f vr (left, right)

-- | A function that computes the root measures of the left and right parts of a
-- split.
--
-- The function's arguments are:
-- * The root measure of the input of the split function, and
-- * The left and right parts of the split.
newtype SplitRootMeasure vr vi a = SplitRootMeasure {
    unSplitRootMeasure ::
         vr
      -> ( FT.StrictFingerTree vi a
         , FT.StrictFingerTree vi a
         )
      -> (vr, vr)
  }

-- | /O(log(min(i,n-i))) + O(f(l, r)) = O(log(min(i,n-i))) + O(length(l))/.
-- Specialisation of @'split'@ that computes root measures by sutraction of the
-- left part's root measure.
--
-- Note: The @l@ suffix of the function name indicates that its time complexity
-- depends on the length of the left part of the split.
splitl ::
     SuperMeasured vr vi a
  => (vi -> Bool)
  -> StrictFingerTree vr vi a
  -> ( StrictFingerTree vr vi a
     , StrictFingerTree vr vi a
     )
splitl p = split p $ SplitRootMeasure $ \vr (left, _right) ->
  let vrLeft = foldMap measureRoot left
  in  (vrLeft, invert vrLeft <> vr)

-- | /O(log(min(i,n-i))) + O(f(l, r)) = O(log(min(i,n-i))) + O(length(r))/.
-- Specialisation of @'split'@ that computes root measures by sutraction of the
-- right part's root measure.
--
-- Note: The @r@ suffix of the function name indicates that its time complexity
-- depends on the length of the right part of the split.
splitr ::
     SuperMeasured vr vi a
  => (vi -> Bool)
  -> StrictFingerTree vr vi a
  -> ( StrictFingerTree vr vi a
     , StrictFingerTree vr vi a
     )
splitr p = split p $ SplitRootMeasure $ \vr (_left, right) ->
  let vrRight = foldMap measureRoot right
  in  (vr <> invert vrRight, vrRight)

{-------------------------------------------------------------------------------
  Maps
-------------------------------------------------------------------------------}

-- | Like @'fmap'@, but with constraints on the element types.
--
-- Note: @vr2@ is reconstructed in time linear in the size of the finger tree.
fmap' ::
     ( SuperMeasured vr1 vi1 a1
     , SuperMeasured vr2 vi2 a2
     )
  => (a1 -> a2)
  -> StrictFingerTree vr1 vi1 a1
  -> StrictFingerTree vr2 vi2 a2
fmap' f (SFT _ sft) = SFT vr' sft'
  where
    sft' = FT.fmap' f sft
    vr' = foldMap measureRoot sft'

-- | Like @'fmap''@, but without the linear-time reconstruction of the root
-- level measure.
--
-- Though similar to @'fmap''@, this function also requires a function parameter
-- of root measures to root measures. This function ensures that we do not have
-- to reconstruct @vr2@ from the elements of the finger tree.
fmap'' ::
     ( SuperMeasured vr1 vi1 a1
     , SuperMeasured vr2 vi2 a2
     )
  => (a1 -> a2)
  -> (vr1 -> vr2)
  -> StrictFingerTree vr1 vi1 a1
  -> StrictFingerTree vr2 vi2 a2
fmap'' f g (SFT vr sft) = SFT vr' sft'
  where
    sft' = FT.fmap' f sft
    vr' = g vr

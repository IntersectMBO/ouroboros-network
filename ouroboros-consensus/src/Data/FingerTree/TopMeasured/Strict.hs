{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Data.FingerTree.TopMeasured.Strict (
    StrictFingerTree
    -- * Top-measuring
  , InternalMeasured
  , Measured (..)
  , TopMeasured (..)
  , measureInternal
    -- * API
  , empty
  , fmap'
  , fmap''
  , fromList
  , split
  , (|>)
  ) where

import           Data.Foldable
import           Data.Group
import           GHC.Generics (Generic)

import           NoThunks.Class (NoThunks (..), noThunksInValues)

import           Data.FingerTree.Strict (Measured)
import qualified Data.FingerTree.Strict as FT

-- TODO(jdral): Should we force strictness anywhere? Or more generally, where
-- should we guide evaluation?

{-------------------------------------------------------------------------------
  Strict finger trees with top-level measures
-------------------------------------------------------------------------------}

-- | A @StrictFingerTree@ with elements of type @a@, an internal measure
-- of type @vi@, and a top-level measure of type @vt@.
data StrictFingerTree vt vi a = StrictFingerTree  {
    tm       :: vt
  , elements :: !(FT.StrictFingerTree vi a)
  }
  deriving (Show, Eq, Ord, Generic)

instance Foldable (StrictFingerTree vt vi) where
  foldMap f = foldMap f . elements

instance NoThunks a => NoThunks (StrictFingerTree vt vi a) where
  showTypeOf _ = "Alt"
  wNoThunks ctxt = noThunksInValues ctxt . toList

instance (Semigroup vt, Measured vi a)
      => Semigroup (StrictFingerTree vt vi a) where
  StrictFingerTree tm1 xs1 <> StrictFingerTree tm2 xs2 =
    StrictFingerTree (tm1 <> tm2) (xs1 <> xs2)

instance (Monoid vt, Measured vi a) => Monoid (StrictFingerTree vt vi a) where
  mempty = StrictFingerTree mempty mempty

{-------------------------------------------------------------------------------
  Measuring
-------------------------------------------------------------------------------}

-- | Internal measures are used by the internal @'StrictFingerTree'@.
type InternalMeasured v a = Measured v a

measureInternal :: Measured v a => a -> v
measureInternal = FT.measure

-- | All values of type @Alt@ are internal-measured.
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
class Group v => TopMeasured v a | a -> v where
  measureTop :: a -> v

-- | All values of type @Alt@ are top-measured.
instance TopMeasured vt a => TopMeasured vt (StrictFingerTree vt vi a) where
  measureTop = tm

-- | Wrapper for when we need both @TopMeasured@ and @InternalMeasured@
type SuperMeasured vt vi a = (TopMeasured vt a, InternalMeasured vi a)

{-------------------------------------------------------------------------------
  API
-------------------------------------------------------------------------------}

infixl 5 |>

(|>) ::
     SuperMeasured v0 v a
  => StrictFingerTree v0 v a
  -> a
  -> StrictFingerTree v0 v a
StrictFingerTree v0 sft |> (!a) =
  StrictFingerTree (v0 <> measureTop a) (sft FT.|> a)

fromList :: SuperMeasured v0 v a => [a] -> StrictFingerTree v0 v a
fromList !xs = StrictFingerTree (foldMap measureTop xs) (FT.fromList xs)

empty :: SuperMeasured v0 v a => StrictFingerTree v0 v a
empty = StrictFingerTree mempty mempty

-- | Note: linear time reconstruction of @v02@ from @v01@.
fmap' ::
     ( SuperMeasured v01 v1 a1
     , SuperMeasured v02 v2 a2
     )
  => (a1 -> a2)
  -> StrictFingerTree v01 v1 a1
  -> StrictFingerTree v02 v2 a2
fmap' f (StrictFingerTree _ sft) = StrictFingerTree v0' sft'
  where
    sft' = FT.fmap' f sft
    v0' = foldMap measureTop sft'

-- | Version of @fmap'@ that also requires function of top-level measure to
-- top-level measure.
fmap'' ::
     ( SuperMeasured v01 v1 a1
     , SuperMeasured v02 v2 a2
     )
  => (a1 -> a2)
  -> (v01 -> v02)
  -> StrictFingerTree v01 v1 a1
  -> StrictFingerTree v02 v2 a2
fmap'' f g (StrictFingerTree v0 sft) = StrictFingerTree v0' sft'
  where
    sft' = FT.fmap' f sft
    v0' = g v0


-- TODO: Choice of left- or right-Delta should depend on sizes.
split ::
     SuperMeasured v0 v a
  => (v -> Bool)
  -> StrictFingerTree v0 v a
  -> ( StrictFingerTree v0 v a
     , StrictFingerTree v0 v a
     )
split p (StrictFingerTree v0 sft) =
    ( StrictFingerTree v0Delta left
    , StrictFingerTree (invert v0Delta <> v0) right
    )
  where
      (left, right) = FT.split p sft
      -- TODO(jdral): Should we invert inside the definition of @v0Delta@ instead?
      -- > @v0Delta = foldMap (invert . measureTop) left
      v0Delta = foldMap measureTop left

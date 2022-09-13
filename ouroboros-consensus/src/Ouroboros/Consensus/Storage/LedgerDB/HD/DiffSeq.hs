{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

{-# LANGUAGE UndecidableInstances       #-}

module Ouroboros.Consensus.Storage.LedgerDB.HD.DiffSeq (
    -- * Sequences of diffs
    DiffSeq (..)
  , Element (..)
  , InternalMeasure (..)
  , Length (..)
  , RootMeasure (..)
  , SlotNo (..)
    -- * Short-hands for type-class constraints
  , SM
    -- * API: derived functions
  , cumulativeDiff
  , extend
  , extend'
  , length
  , mapDiffSeq
  , maxSlot
  , splitl
  , splitlAt
  , splitlAtFromEnd
  , splitr
  , splitrAt
  , splitrAtFromEnd
  ) where

import           Prelude hiding (length, splitAt)

import qualified Control.Exception as Exn
import           Data.Bifunctor (Bifunctor (bimap))
import           Data.Group
import           Data.Monoid (Sum (..))
import           Data.Semigroup (Max (..))
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)

import           Data.FingerTree.RootMeasured.Strict hiding (split, splitl,
                     splitlr, splitr)
import qualified Data.FingerTree.RootMeasured.Strict as RMFT (splitlr)
import           Data.Map.Diff.Strict (Diff)

import qualified Cardano.Slotting.Slot as Slot

{-------------------------------------------------------------------------------
  Sequences of diffs
-------------------------------------------------------------------------------}

newtype DiffSeq k v =
  DiffSeq
    (StrictFingerTree
      (RootMeasure k v)
      (InternalMeasure k v)
      (Element k v)
    )
  deriving stock (Generic, Show, Eq)
  deriving newtype (Semigroup, Monoid)
  deriving anyclass (NoThunks)


-- The @'SlotNo'@ is not included in the root measure, since it is
-- not a @'Group'@ instance.
data RootMeasure k v = RootMeasure {
    -- | Cumulative length
    tmLength :: {-# UNPACK #-} !Length
    -- | Cumulative diff
  , tmDiff   :: !(Diff k v)
  }
  deriving stock (Generic, Show, Eq, Functor)
  deriving anyclass (NoThunks)

data InternalMeasure k v = InternalMeasure {
    -- | Cumulative length
    imLength :: {-# UNPACK #-} !Length
    -- | Right-most slot number
  , imSlotNo ::                !(Maybe SlotNo)
  }
  deriving stock (Generic, Show, Eq, Functor)
  deriving anyclass (NoThunks)

data Element k v = Element {
    elSlotNo :: {-# UNPACK #-} !SlotNo
  , elDiff   ::                !(Diff k v)
  }
  deriving stock (Generic, Show, Eq, Functor)
  deriving anyclass (NoThunks)


-- | Length as a @'Sum'@.
newtype Length = Length {unLength :: Int}
  deriving stock (Generic, Show, Eq, Ord)
  deriving newtype (Num)
  deriving anyclass (NoThunks)
  deriving Semigroup via Sum Int
  deriving Monoid via Sum Int
  deriving Group via Sum Int

-- | Right-most slot number as a @'Max'@.
newtype SlotNo = SlotNo {unSlotNo :: Slot.SlotNo}
  deriving stock (Generic, Show, Eq, Ord)
  deriving newtype (Num)
  deriving anyclass (NoThunks)
  deriving Semigroup via Max Slot.SlotNo
  deriving Monoid via Max Slot.SlotNo

{-------------------------------------------------------------------------------
  Root measuring
-------------------------------------------------------------------------------}

instance (Ord k, Eq v) => RootMeasured (RootMeasure k v) (Element k v) where
  measureRoot (Element _ d) = RootMeasure 1 d

instance (Ord k, Eq v) => Semigroup (RootMeasure k v) where
  RootMeasure len1 d1 <> RootMeasure len2 d2 =
      RootMeasure (len1 <> len2) (d1 <> d2)

instance (Ord k, Eq v) => Monoid (RootMeasure k v) where
  mempty = RootMeasure mempty mempty

instance (Ord k, Eq v) => Group (RootMeasure k v) where
  invert (RootMeasure len d) =
    RootMeasure (invert len) (invert d)

{-------------------------------------------------------------------------------
  Internal measuring
-------------------------------------------------------------------------------}

instance Measured (InternalMeasure k v) (Element k v) where
  measure (Element slotNo _d) = InternalMeasure 1 (Just slotNo)

instance Semigroup (InternalMeasure k v) where
  InternalMeasure len1 sl1 <> InternalMeasure len2 sl2 =
    InternalMeasure (len1 <> len2) (sl1 <> sl2)

instance Monoid (InternalMeasure k v) where
  mempty = InternalMeasure mempty mempty

{-------------------------------------------------------------------------------
  Short-hands for type-class constraints
-------------------------------------------------------------------------------}

-- | Short-hand for @'SuperMeasured'@.
type SM k v = SuperMeasured (RootMeasure k v) (InternalMeasure k v) (Element k v)

{-------------------------------------------------------------------------------
  API: derived functions
-------------------------------------------------------------------------------}

cumulativeDiff ::
     SM k v
  => DiffSeq k v
  -> Diff k v
cumulativeDiff (DiffSeq ft) = tmDiff $ measureRoot ft

length ::
     SM k v
  => DiffSeq k v -> Int
length (DiffSeq ft) = unLength . tmLength $ measureRoot ft

extend ::
     SM k v
  => DiffSeq k v
  -> Element k v
  -> DiffSeq k v
extend (DiffSeq ft) el = DiffSeq $ ft |> el

extend' ::
     SM k v
  => DiffSeq k v
  -> Element k v
  -> DiffSeq k v
extend' (DiffSeq ft) el =
    Exn.assert invariant $ DiffSeq $ ft |> el
  where
    invariant = case imSlotNo $ measure el of
      Nothing  -> True
      Just sl0 -> sl0 <= elSlotNo el

maxSlot ::
     SM k v
  => DiffSeq k v
  -> Maybe Slot.SlotNo
maxSlot (DiffSeq ft) =
    unwrapInner $ imSlotNo $ measure ft
  where
    -- We care about /real/ slot numbers, so we should return a
    -- @'Slot.SlotNo'@.
    unwrapInner :: Maybe SlotNo -> Maybe Slot.SlotNo
    unwrapInner Nothing            = Nothing
    unwrapInner (Just (SlotNo sl)) = Just sl

splitlr ::
     (SM k v, Ord k, Eq v)
  => LR
  -> (InternalMeasure k v -> Bool)
  -> DiffSeq k v
  -> (DiffSeq k v, DiffSeq k v)
splitlr lr p (DiffSeq ft) = bimap DiffSeq DiffSeq $ RMFT.splitlr lr p ft

splitl ::
     (SM k v, Ord k, Eq v)
  => (InternalMeasure k v -> Bool)
  -> DiffSeq k v
  -> (DiffSeq k v, DiffSeq k v)
splitl = splitlr L

splitr ::
     (SM k v, Ord k, Eq v)
  => (InternalMeasure k v -> Bool)
  -> DiffSeq k v
  -> (DiffSeq k v, DiffSeq k v)
splitr = splitlr R

splitlrAt ::
     (SM k v, Ord k, Eq v)
  => LR
  -> Int
  -> DiffSeq k v
  -> (DiffSeq k v, DiffSeq k v)
splitlrAt lr n = splitlr lr ((Length n<) . imLength)

splitlAt ::
     (SM k v, Ord k, Eq v)
  => Int
  -> DiffSeq k v
  -> (DiffSeq k v, DiffSeq k v)
splitlAt = splitlrAt L

splitrAt ::
     (SM k v, Ord k, Eq v)
  => Int
  -> DiffSeq k v
  -> (DiffSeq k v, DiffSeq k v)
splitrAt = splitlrAt R

splitlrAtFromEnd ::
     (SM k v, Ord k, Eq v)
  => LR
  -> Int
  -> DiffSeq k v
  -> (DiffSeq k v, DiffSeq k v)
splitlrAtFromEnd lr n dseq =
    Exn.assert (n <= len) $ splitlrAt lr (len - n) dseq
  where
    len = length dseq

splitlAtFromEnd ::
     (SM k v, Ord k, Eq v)
  => Int
  -> DiffSeq k v
  -> (DiffSeq k v, DiffSeq k v)
splitlAtFromEnd = splitlrAtFromEnd L

splitrAtFromEnd ::
     (SM k v, Ord k, Eq v)
  => Int
  -> DiffSeq k v
  -> (DiffSeq k v, DiffSeq k v)
splitrAtFromEnd = splitlrAtFromEnd R

mapDiffSeq ::
       ( SM k v
       , SM k v'
       )
    => (v -> v')
    -> DiffSeq k v
    -> DiffSeq k v'
mapDiffSeq f (DiffSeq ft) = DiffSeq $ fmap' (fmap f) ft

{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MonoLocalBinds             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

{- | Sequences of diffs for ledger tables.

   These diff sequences are an instantiation of a strict finger tree with root
   measures. The tree/sequence itself contains diffs and slot information, while
   the root measure is the total sum of all diffs in the sequence. The internal
   measure is used to keep track of sequence length and maximum slot numbers.

   The diff datatype that we use forms a @'Group'@, which allows for relatively
   efficient splitting of finger trees with respect to recomputing measures by
   means of the @'invert'@ operation that the @'Group'@ type class requires.
   Namely, if either the left or right part of the split is small in comparison
   with the input sequence, then we can subtract the diffs in the smaller part
   from the root measure of the input to (quickly) compute the root measure of
   the /other/ part of the split. This is much faster than computing the root
   measures from scratch by doing a linear-time pass over the elements of the
   split parts, or a logarithmic-time pass over intermediate sums of diffs in
   case we store cumulative diffs in the nodes of the finger tree.

   === Example of fast splits

   As an analogy, consider this example: we have a sequence of consecutive
   integer numbers @xs = [1..n]@ where @n@ is large, and we define the root
   measure of the sequence to be the total sum of these numbers, @rmxs = sum
   [1..n]@ (we assume @rmxs@ is fully evaluated). Say we split this sequence of
   integer numbers at the index @2@, then we get /left/ and /right/ parts of the
   split @ys@ and @zs respectively.

   > splitAt 2 xs = (ys, zs) = ([1..2], [3..n])

   How should we compute we the root measure @rmys@ of @ys@? Since @ys@ is
   small, we can just compute @rmys = sum [1..2]@. How should we compute the
   root measure @rmzs@ of @zs@? We should not compute @rmzs = sum [3..n]@ in
   this case, since @n@ is large. Instead, we compute @rmzs = rmxs - rmys@,
   which evaluates to its result in time that is linear in the length of @ys@,
   in this case @O(1)@.

   === Why not store sums of diffs in the internal measure instead of the root
   measure?

   We could also have used the interal measure of the strict finger tree to
   store intermediate sums of diffs for all subtrees of the node. The subtree
   rooted at the root of the tree would then store the total sum of diffs.
   However, we would have now to recompute a possibly logarithmic number of sums
   of diffs when we split or extend the sequence. Given that in @consensus@ we
   use the total sum of diffs nearly as often as we split or extend the diff
   sequence, this proved to be too costly. The single-instance root measure
   reduces the overhead of this "caching" of intermediate sums of diffs by only
   using a single total sum of diffs, though augmented with an @'invert'@
   operation to facilitate computing updated root measures.

-}
module Ouroboros.Consensus.Storage.LedgerDB.DiffSeq (
    -- * Sequences of diffs
    DiffSeq (..)
  , Element (..)
  , InternalMeasure (..)
  , Length (..)
  , RootMeasure (..)
  , SlotNoLB (..)
  , SlotNoUB (..)
    -- * Short-hands for type-class constraints
  , SM
    -- * API: derived functions
  , append
  , cumulativeDiff
  , empty
  , extend
  , length
    -- * Slots
  , maxSlot
  , minSlot
    -- * Splitting
  , split
  , splitAt
  , splitAtFromEnd
    -- * Maps
  , mapDiffSeq
  ) where

import qualified Cardano.Slotting.Slot as Slot
import qualified Control.Exception as Exn
import           Data.Bifunctor (Bifunctor (bimap))
import           Data.FingerTree.RootMeasured.Strict hiding (split)
import qualified Data.FingerTree.RootMeasured.Strict as RMFT (splitSized)
import           Data.Group
import           Data.Map.Diff.Strict as MapDiff
import           Data.Maybe.Strict
import           Data.Monoid (Sum (..))
import           Data.Semigroup (Max (..), Min (..))
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)
import           Prelude hiding (length, splitAt)

{-------------------------------------------------------------------------------
  Sequences of diffs
-------------------------------------------------------------------------------}

-- | A sequence of key-value store differences.
--
-- INVARIANT: The slot numbers of consecutive elements should be strictly
-- increasing. Manipulating the underlying @'StrictFingerTree'@ directly may
-- break this invariant.
newtype DiffSeq k v =
  UnsafeDiffSeq
    (StrictFingerTree
      (RootMeasure k v)
      (InternalMeasure k v)
      (Element k v)
    )
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NoThunks)

-- The @'SlotNo'@ is not included in the root measure, since it is
-- not a @'Group'@ instance.
data RootMeasure k v = RootMeasure {
    -- | Cumulative length
    rmLength :: {-# UNPACK #-} !Length
    -- | Cumulative diff
  , rmDiff   :: !(Diff k v)
  }
  deriving stock (Generic, Show, Eq, Functor)
  deriving anyclass (NoThunks)

data InternalMeasure k v = InternalMeasure {
    -- | Cumulative length
    imLength  :: {-# UNPACK #-} !Length
    -- | Leftmost slot number (or lower bound)
    --
    -- Empty diff sequences have no rightmost slot number, so in that case
    -- @imSlotNo == Nothing@.
  , imSlotNoL ::                !(StrictMaybe SlotNoLB)
    -- | Rightmost slot number (or upper bound)
    --
    -- Empty diff sequences have no leftmost slot number, so in that case
    -- @imSlotNo == Nothing@.
  , imSlotNoR ::                !(StrictMaybe SlotNoUB)
  }
  deriving stock (Generic, Show, Eq, Functor)
  deriving anyclass (NoThunks)

data Element k v = Element {
    elSlotNo :: {-# UNPACK #-} !Slot.SlotNo
  , elDiff   ::                !(Diff k v)
  }
  deriving stock (Generic, Show, Eq, Functor)
  deriving anyclass (NoThunks)

-- | Length of a sequence of differences.
newtype Length = Length { unLength :: Int }
  deriving stock (Generic, Show, Eq, Ord)
  deriving newtype (Num)
  deriving anyclass (NoThunks)
  deriving Semigroup via Sum Int
  deriving Monoid via Sum Int
  deriving Group via Sum Int

-- | An upper bound on slot numbers.
newtype SlotNoUB = SlotNoUB {unSlotNoUB :: Slot.SlotNo}
  deriving stock (Generic, Show, Eq, Ord)
  deriving newtype (Num)
  deriving anyclass (NoThunks)
  deriving Semigroup via Max Slot.SlotNo
  deriving Monoid via Max Slot.SlotNo

-- | A lower bound on slot numbers.
newtype SlotNoLB = SlotNoLB {unSlotNoLB :: Slot.SlotNo}
  deriving stock (Generic, Show, Eq, Ord)
  deriving newtype (Num)
  deriving anyclass (NoThunks)
  deriving Semigroup via Min Slot.SlotNo
  deriving Monoid via Min Slot.SlotNo

noSlotBoundsIntersect :: SlotNoUB -> SlotNoLB -> Bool
noSlotBoundsIntersect (SlotNoUB sl1) (SlotNoLB sl2) = sl1 <= sl2

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
  measure (Element sl _d) = InternalMeasure {
      imLength  = 1
    , imSlotNoL = SJust $ SlotNoLB sl
    , imSlotNoR = SJust $ SlotNoUB sl
    }

instance Semigroup (InternalMeasure k v) where
  InternalMeasure len1 sl1L sl1R <> InternalMeasure len2 sl2L sl2R =
    InternalMeasure (len1 <> len2) (sl1L <> sl2L) (sl1R <> sl2R)

instance Monoid (InternalMeasure k v) where
  mempty = InternalMeasure mempty mempty mempty

{-------------------------------------------------------------------------------
  Short-hands types and constraints
-------------------------------------------------------------------------------}

-- | Short-hand for @'SuperMeasured'@.
type SM k v =
  SuperMeasured (RootMeasure k v) (InternalMeasure k v) (Element k v)

{-------------------------------------------------------------------------------
  Queries
-------------------------------------------------------------------------------}

cumulativeDiff ::
     SM k v
  => DiffSeq k v
  -> Diff k v
cumulativeDiff (UnsafeDiffSeq ft) = rmDiff $ measureRoot ft

length ::
     SM k v
  => DiffSeq k v -> Int
length (UnsafeDiffSeq ft) = unLength . rmLength $ measureRoot ft

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

extend ::
     SM k v
  => DiffSeq k v
  -> Slot.SlotNo
  -> Diff k v
  -> DiffSeq k v
extend (UnsafeDiffSeq ft) sl d =
    Exn.assert invariant $ UnsafeDiffSeq $ ft |> Element sl d
  where
    invariant = case imSlotNoR $ measure ft of
      SNothing  -> True
      SJust slR -> noSlotBoundsIntersect slR (SlotNoLB sl)

append ::
     (Ord k, Eq v)
  => DiffSeq k v
  -> DiffSeq k v
  -> DiffSeq k v
append (UnsafeDiffSeq ft1) (UnsafeDiffSeq ft2) =
    Exn.assert invariant $ UnsafeDiffSeq (ft1 <> ft2)
  where
    sl1R      = imSlotNoR $ measure ft1
    sl2L      = imSlotNoL $ measure ft2
    invariant = case noSlotBoundsIntersect <$> sl1R <*> sl2L of
      SNothing -> True
      SJust v  -> v

empty ::
     (Ord k, Eq v)
  => DiffSeq k v
empty = UnsafeDiffSeq mempty

{-------------------------------------------------------------------------------
  Slots
-------------------------------------------------------------------------------}

maxSlot ::
     SM k v
  => DiffSeq k v
  -> StrictMaybe Slot.SlotNo
maxSlot (UnsafeDiffSeq ft) = unSlotNoUB <$> imSlotNoR (measure ft)

minSlot ::
     SM k v
  => DiffSeq k v
  -> StrictMaybe Slot.SlotNo
minSlot (UnsafeDiffSeq ft) = unSlotNoLB <$> imSlotNoL (measure ft)

{-------------------------------------------------------------------------------
  Splitting
-------------------------------------------------------------------------------}

instance Sized (InternalMeasure k v) where
  size = unLength . imLength

split ::
     SM k v
  => (InternalMeasure k v -> Bool)
  -> DiffSeq k v
  -> (DiffSeq k v, DiffSeq k v)
split p (UnsafeDiffSeq ft) = bimap UnsafeDiffSeq UnsafeDiffSeq $ RMFT.splitSized p ft

splitAt ::
     SM k v
  => Int
  -> DiffSeq k v
  -> (DiffSeq k v, DiffSeq k v)
splitAt n = split ((Length n<) . imLength)

splitAtFromEnd ::
     SM k v
  => Int
  -> DiffSeq k v
  -> (DiffSeq k v, DiffSeq k v)
splitAtFromEnd n dseq =
    Exn.assert (n <= len) $ splitAt (len - n) dseq
  where
    len = length dseq

{-------------------------------------------------------------------------------
  Maps
-------------------------------------------------------------------------------}

mapDiffSeq ::
       ( SM k v
       , SM k v'
       )
    => (v -> v')
    -> DiffSeq k v
    -> DiffSeq k v'
mapDiffSeq f (UnsafeDiffSeq ft) = UnsafeDiffSeq $ fmap' (fmap f) ft

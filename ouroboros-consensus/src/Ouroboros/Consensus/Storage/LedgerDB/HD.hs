{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- | How to mediate access to an on-disk UTxO table
--
-- Except for the backing store, this interface would also be required for any
-- other kind of table. Some definitions would change depending on that table's
-- specifics.
--
-- TODO: Previously, we had a deprecation warning pragma on this module, but
-- this made CI fail. However, since we should remove this module at some point,
-- we add this TODO here: remove this module.
module Ouroboros.Consensus.Storage.LedgerDB.HD (
    -- * Values
    UtxoValues (..)
  , emptyUtxoValues
  , mapUtxoValues
    -- * Keys
  , UtxoKeys (..)
  , castUtxoKeys
  , emptyUtxoKeys
    -- * Differences
  , UtxoDiff (..)
  , differenceUtxoValues
  , emptyUtxoDiff
  , keysUtxoDiff
  , mapUtxoDiff
    -- ** Internals
  , UtxoEntryDiff (..)
  , UtxoEntryDiffState (..)
    -- * Combinators
  , forwardValues
  , forwardValuesAndKeys
  , restrictValues
    -- * Sequence of differences
  , SeqUtxoDiff (..)
  , cumulativeDiffSeqUtxoDiff
  , emptySeqUtxoDiff
  , extendSeqUtxoDiff
  , lengthSeqUtxoDiff
  , mapSeqUtxoDiff
  , slotSeqUtxoDiff
  , splitAfterSlotSeqUtxoDiff
  , splitAtFromEndSeqUtxoDiff
  , splitAtSeqUtxoDiff
    -- ** Internals
  , SudElement (..)
  , SudMeasure (..)
  ) where

import qualified Control.Exception as Exn
import           Data.Map (Map)
import qualified Data.Map.Merge.Strict as MapMerge
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)
import           NoThunks.Class (NoThunks)

import           Cardano.Binary (FromCBOR (..), ToCBOR (..))
import           Cardano.Slotting.Slot (SlotNo)

import           Data.FingerTree.Strict (StrictFingerTree)
import qualified Data.FingerTree.Strict as FT

import           Ouroboros.Consensus.Util.CBOR.Simple

{-------------------------------------------------------------------------------
  Map of values
-------------------------------------------------------------------------------}

-- | An mapping of tx inputs (ie a transaction id and an output index)
-- to tx outputs (eg an address and an amount)
--
-- The map must be a /functional/ (if a key is present, its value is uniquely
-- determined by context). Moreover, any specific key must only be inserted at
-- most once.
--
-- TODO should we use the bespoke @compact-map@ that the ledger team recently
-- developed? We don't need to, if this type is only every use for tests.
newtype UtxoValues k v = UtxoValues (Map k v)
  deriving (Eq, Generic, NoThunks, Show)

instance Ord k => Monoid (UtxoValues k v) where
  mempty = UtxoValues Map.empty

-- | Note that this fails via 'error' on collisions
instance Ord k => Semigroup (UtxoValues k v) where
  UtxoValues m1 <> UtxoValues m2 =
      UtxoValues $ Map.unionWith err m1 m2
    where
      err = error "impossible! Semigroup UtxoValues collision"

instance (Ord k, ToCBOR k, ToCBOR v) => ToCBOR (UtxoValues k v) where
  toCBOR (UtxoValues m) = versionZeroProductToCBOR [toCBOR m]

instance (Ord k, FromCBOR k, FromCBOR v) => FromCBOR (UtxoValues k v) where
  fromCBOR = versionZeroProductFromCBOR "UtxoValues" 1 $ UtxoValues <$> fromCBOR

emptyUtxoValues :: UtxoValues k v
emptyUtxoValues = UtxoValues Map.empty

mapUtxoValues :: (v -> v') -> UtxoValues k v -> UtxoValues k v'
mapUtxoValues f (UtxoValues vs) = UtxoValues $ Map.map f vs

{-------------------------------------------------------------------------------
  Difference of maps
-------------------------------------------------------------------------------}

-- | The differences that could be applied to a 'UtxoValues'
newtype UtxoDiff k v = UtxoDiff (Map k (UtxoEntryDiff v))
  deriving (Eq, Generic, NoThunks, Show)

-- | The key's value and how it changed
data UtxoEntryDiff v = UtxoEntryDiff !v !UtxoEntryDiffState
  deriving (Eq, Generic, NoThunks, Show)

-- | Whether an entry was deleted, inserted, or inserted-and-then-deleted
data UtxoEntryDiffState = UedsDel | UedsIns | UedsInsAndDel
  deriving (Eq, Generic, NoThunks, Show)

-- | Assumes the colliding value is equivalent, since UTxO map is functional
--
-- Note that this fails via 'error' if a UTxO is inserted twice, deleted twice,
-- or inserted after being deleted.
instance Semigroup (UtxoEntryDiff v) where
  UtxoEntryDiff v s1 <> UtxoEntryDiff _v s2 =
      UtxoEntryDiff v $ case (s1, s2) of
        (UedsIns, UedsDel) -> UedsInsAndDel
        o                  ->
          error $ "impossible! Semigroup UtxoEntryDiff " <> show o

instance Ord k => Monoid (UtxoDiff k v) where
  mempty = UtxoDiff Map.empty

instance Ord k => Semigroup (UtxoDiff k v) where
  UtxoDiff m1 <> UtxoDiff m2 = UtxoDiff $ Map.unionWith (<>) m1 m2

emptyUtxoDiff :: UtxoDiff k v
emptyUtxoDiff = UtxoDiff Map.empty

-- | The function must preserve the /functionality/ (if a key is present, its
-- value is uniquely determined by context).
mapUtxoDiff :: (v -> v') -> UtxoDiff k v -> UtxoDiff k v'
mapUtxoDiff f (UtxoDiff m) =
    UtxoDiff $ Map.map g m
  where
    g (UtxoEntryDiff v diffstate) = UtxoEntryDiff (f v) diffstate

keysUtxoDiff :: UtxoDiff k v -> UtxoKeys k v
keysUtxoDiff (UtxoDiff m) = UtxoKeys $ Map.keysSet m

-- | Given values before and after, compute the diff.
--
-- Note that this diff will not include any 'UedsInsAndDel'.
differenceUtxoValues :: Ord k => UtxoValues k v -> UtxoValues k v -> UtxoDiff k v
differenceUtxoValues (UtxoValues m1) (UtxoValues m2) =
      UtxoDiff
    $ MapMerge.merge
        (MapMerge.mapMissing $ \_k v -> UtxoEntryDiff v UedsDel)
        (MapMerge.mapMissing $ \_k v -> UtxoEntryDiff v UedsIns)
        (MapMerge.zipWithMaybeMatched $ \ _ _ _ -> Nothing)
        m1
        m2

{-------------------------------------------------------------------------------
  Set of keys
-------------------------------------------------------------------------------}

-- | Just the keys
newtype UtxoKeys k v = UtxoKeys (Set k)
  deriving (Eq, Generic, NoThunks, Show)

instance Ord k => Monoid (UtxoKeys k v) where
  mempty = UtxoKeys Set.empty

instance Ord k => Semigroup (UtxoKeys k v) where
  UtxoKeys s1 <> UtxoKeys s2 = UtxoKeys $ Set.union s1 s2

emptyUtxoKeys :: UtxoKeys k v
emptyUtxoKeys = UtxoKeys Set.empty

castUtxoKeys :: UtxoKeys k v -> UtxoKeys k v'
castUtxoKeys (UtxoKeys ks) = UtxoKeys ks

{-------------------------------------------------------------------------------
  Combinators
-------------------------------------------------------------------------------}

restrictValues :: Ord k => UtxoValues k v -> UtxoKeys k v -> UtxoValues k v
restrictValues (UtxoValues m) (UtxoKeys s) =
    UtxoValues (Map.restrictKeys m s)

-- | Transport a set of values (eg 'rewindPresent' unioned with the fetch of
-- 'rewoundUnknown' from backing store) by applying a valid difference
--
-- Note that this fails via 'error' if the diff is invalid, eg it deletes a key
-- that is not present in the argument or inserts a key that is already in the
-- argument.
forwardValues :: (Ord k, HasCallStack) => UtxoValues k v -> UtxoDiff k v -> UtxoValues k v
forwardValues (UtxoValues values) (UtxoDiff diffs) =
      UtxoValues
    $ MapMerge.merge
        MapMerge.preserveMissing
        (MapMerge.mapMaybeMissing     newKeys)
        (MapMerge.zipWithMaybeMatched oldKeys)
        values
        diffs
  where
    newKeys :: k -> UtxoEntryDiff v -> Maybe v
    newKeys _k (UtxoEntryDiff v diffState) = case diffState of
      UedsIns       -> Just v
      UedsInsAndDel -> Nothing
      UedsDel       -> Nothing -- TODO error "impossible! delete of missing key"

    oldKeys :: k -> v -> UtxoEntryDiff v -> Maybe v
    oldKeys _k _v1 (UtxoEntryDiff _v2 diffState) = case diffState of
      UedsDel       -> Nothing
      UedsIns       -> error "impossible! duplicate insert of key"
      UedsInsAndDel -> error "impossible! duplicate insert of key"

-- | Transport a set of values by applying a valid difference as well as
-- creating the values for keys that must be created in said differences.
--
-- The provided values come from reading a BackingStore, the differences come
-- from the current 'DbChangelog' and the keys are the values that are created
-- or created and consumed along the 'DbChangelog' and therefore we know are not
-- present on the 'BackingStore'. Note that in particular, a key cannot appear
-- in both arguments at the same time.
--
-- Note that this fails via 'error' if the diff is invalid, eg it deletes a key
-- that is not present in the argument or inserts a key that is already in the
-- argument.
forwardValuesAndKeys :: forall k v. (Ord k, HasCallStack) => UtxoValues k v -> UtxoKeys k v -> UtxoDiff k v -> UtxoValues k v
forwardValuesAndKeys values@(UtxoValues v) (UtxoKeys keys) (UtxoDiff diffs) =
  forwardValues values (UtxoDiff $ diffs `Map.restrictKeys` (Map.keysSet v `Set.union` keys))

{-------------------------------------------------------------------------------
  Sequence of diffs
-------------------------------------------------------------------------------}

-- | A sequence of 'UtxoDiff'
--
-- Each is labeled by a 'SlotNo'.
--
-- See 'SudElement' and 'SudMeasure'.
newtype SeqUtxoDiff k v =
    SeqUtxoDiff (StrictFingerTree (SudMeasure k v) (SudElement k v))
  deriving (Eq, Generic, NoThunks, Show)

-- TODO no Semigroup instance just because I don't think we need it

emptySeqUtxoDiff :: Ord k => SeqUtxoDiff k v
emptySeqUtxoDiff = SeqUtxoDiff FT.empty

-- | The measure of a possibly-empty sequence
data SudMeasure k v =
    -- | The measure of an empty sequence
    SudMeasureNothing
    -- | The measure of a non-empty sequence
  | SudMeasureJust
      {-# UNPACK #-} !Int   -- ^ cumulative size
      {-# UNPACK #-} !SlotNo   -- ^ rightmost, ie maximum
                     !(UtxoDiff k v)   -- ^ cumulative diff
  deriving (Eq, Generic, Show)

sizeSudMeasure :: SudMeasure k v -> Int
sizeSudMeasure = \case
    SudMeasureNothing               -> 0
    SudMeasureJust size _slot _diff -> size

slotSudMeasure :: SudMeasure k v -> Maybe SlotNo
slotSudMeasure = \case
    SudMeasureNothing               -> Nothing
    SudMeasureJust _size slot _diff -> Just slot

instance Ord k => Monoid (SudMeasure k v) where
  mempty = SudMeasureNothing

instance Ord k => Semigroup (SudMeasure k v) where
  SudMeasureNothing                <> r                                = r
  l                                <> SudMeasureNothing                = l
  SudMeasureJust size1 slot1 diff1 <> SudMeasureJust size2 slot2 diff2 =
        Exn.assert (slot1 <= slot2)
      $ SudMeasureJust
          (size1 + size2)
          slot2
          (diff1 <> diff2)

-- | An element of the sequence
data SudElement k v = SudElement {-# UNPACK #-} !SlotNo !(UtxoDiff k v)
  deriving (Eq, Generic, NoThunks, Show)

instance
     Ord k
  => FT.Measured (SudMeasure k v) (SudElement k v)
    where
  measure (SudElement slot diff) = SudMeasureJust 1 slot diff

cumulativeDiffSeqUtxoDiff :: Ord k => SeqUtxoDiff k v -> UtxoDiff k v
cumulativeDiffSeqUtxoDiff (SeqUtxoDiff ft) = case FT.measure ft of
    SudMeasureNothing               -> mempty
    SudMeasureJust _size _slot diff -> diff

lengthSeqUtxoDiff :: Ord k => SeqUtxoDiff k v -> Int
lengthSeqUtxoDiff (SeqUtxoDiff ft) = sizeSudMeasure $ FT.measure ft

slotSeqUtxoDiff :: Ord k => SeqUtxoDiff k v -> Maybe SlotNo
slotSeqUtxoDiff (SeqUtxoDiff ft) = slotSudMeasure $ FT.measure ft

-- | The function must preserve the /functionality/ (if a key is present, its
-- value is uniquely determined by context).
mapSeqUtxoDiff :: Ord k => (v -> v') -> SeqUtxoDiff k v -> SeqUtxoDiff k v'
mapSeqUtxoDiff f (SeqUtxoDiff ft) =
    SeqUtxoDiff $ FT.fmap' g ft
  where
    g (SudElement slot diff) = SudElement slot (mapUtxoDiff f diff)

-- | Append a diff to the end of the sequence
extendSeqUtxoDiff ::
  Ord k => SeqUtxoDiff k v -> SlotNo -> UtxoDiff k v -> SeqUtxoDiff k v
extendSeqUtxoDiff (SeqUtxoDiff ft) slot diff =
      Exn.assert invariant
    $ SeqUtxoDiff
    $ ft FT.|> SudElement slot diff
  where
    invariant = case FT.measure ft of
      SudMeasureNothing                -> True
      SudMeasureJust _size slot0 _diff -> slot0 <= slot

splitAtSeqUtxoDiff ::
  Ord k => Int -> SeqUtxoDiff k v -> (SeqUtxoDiff k v, SeqUtxoDiff k v)
splitAtSeqUtxoDiff n (SeqUtxoDiff ft) =
    (SeqUtxoDiff l, SeqUtxoDiff r)
  where
    (l, r)      = FT.split predicate ft
    predicate m = n < sizeSudMeasure m

-- | Isolate the given number of differences at the end of the sequence
splitAtFromEndSeqUtxoDiff ::
  Ord k => Int -> SeqUtxoDiff k v -> (SeqUtxoDiff k v, SeqUtxoDiff k v)
splitAtFromEndSeqUtxoDiff n sq =
      Exn.assert (n <= len)
    $ splitAtSeqUtxoDiff (len - n) sq
  where
    len = lengthSeqUtxoDiff sq

-- | Isolate the diffs that are labeled @<= slot@
--
-- TODO How to handle EBBs? Or else how to justify ignoring them? It's possible
-- for an EBB to have the same slot number as its successor. Suppose that's the
-- case. If the EBB was the last thing flushed to the database, then the seqno
-- of the database and the seqno of the block we'd flush next are already
-- equivalent!
--
-- Should the seqno be the block number instead? That'd resolve this. Or, it
-- could be the point instead of just the slot, EG.
splitAfterSlotSeqUtxoDiff ::
     Ord k
  => SlotNo
  -> SeqUtxoDiff k v
  -> (SeqUtxoDiff k v, SeqUtxoDiff k v)
splitAfterSlotSeqUtxoDiff slot (SeqUtxoDiff ft) =
    (SeqUtxoDiff l, SeqUtxoDiff r)
  where
    (l, r)      = FT.split predicate ft
    predicate m = case slotSudMeasure m of
      Nothing    -> True
      Just slot' -> slot < slot'

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
  , RewoundKeys (..)
  , forwardValues
  , mapRewoundKeys
  , restrictValues
  , rewindKeys
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

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Control.Exception as Exn
import           Data.Foldable (toList)
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
mapUtxoValues f (UtxoValues vs) = UtxoValues $ fmap f vs

{-------------------------------------------------------------------------------
  Difference of maps
-------------------------------------------------------------------------------}

-- | The differences that could be applied to a 'UtxoValues'
newtype UtxoDiff k v = UtxoDiff (Map k (UtxoEntryDiff v))
  deriving (Eq, Generic, NoThunks, Show)

instance (Ord k, ToCBOR k, ToCBOR v) => ToCBOR (UtxoDiff k v) where
  toCBOR (UtxoDiff m) = versionZeroProductToCBOR [toCBOR m]

instance (Ord k, FromCBOR k, FromCBOR v) => FromCBOR (UtxoDiff k v) where
  fromCBOR = versionZeroProductFromCBOR "UtxoDiff" 1 $ UtxoDiff <$> fromCBOR

-- | The key's value and how it changed
data UtxoEntryDiff v = UtxoEntryDiff !v !UtxoEntryDiffState
  deriving (Eq, Generic, NoThunks, Show)

instance ToCBOR v => ToCBOR (UtxoEntryDiff v) where
  toCBOR (UtxoEntryDiff v diffstate) =
      versionZeroProductToCBOR [toCBOR v, toCBOR diffstate]

instance FromCBOR v => FromCBOR (UtxoEntryDiff v) where
  fromCBOR =
        versionZeroProductFromCBOR "UtxoEntryDiff" 2
      $ UtxoEntryDiff <$> fromCBOR <*> fromCBOR

-- | Whether an entry was deleted, inserted, or inserted-and-then-deleted
data UtxoEntryDiffState = UedsDel | UedsIns | UedsInsAndDel
  deriving (Eq, Generic, NoThunks, Show)

instance ToCBOR UtxoEntryDiffState where
  toCBOR = (CBOR.encodeListLen 1 <>) . \case
    UedsDel       -> CBOR.encodeWord 0
    UedsIns       -> CBOR.encodeWord 1
    UedsInsAndDel -> CBOR.encodeWord 2

instance FromCBOR UtxoEntryDiffState where
  fromCBOR = do
      CBOR.decodeListLenOf 1
      CBOR.decodeWord >>= \case
        0 -> pure UedsDel
        1 -> pure UedsIns
        2 -> pure UedsInsAndDel
        o -> fail $ "UtxoEntryDiffState unknown tag: " <> show o

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
    UtxoDiff $ fmap g m
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

instance (Ord k, ToCBOR k, ToCBOR v) => ToCBOR (UtxoKeys k v) where
  toCBOR (UtxoKeys m) = versionZeroProductToCBOR [toCBOR m]

instance (Ord k, FromCBOR k, FromCBOR v) => FromCBOR (UtxoKeys k v) where
  fromCBOR = versionZeroProductFromCBOR "UtxoKeys" 1 $ UtxoKeys <$> fromCBOR

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

-- | The result of rewinding keys through a valid 'UtxoDiff'
data RewoundKeys k v = RewoundKeys {
    -- | Keys inserted by the diff
    --
    -- Because the diff is valid, these keys are absent in the argument of the
    -- diff.
    rkAbsent  :: UtxoKeys k v
    -- | The UTxO deleted by the diff
    --
    -- Because the diff is valid, these mappings are present in the argument of
    -- the diff.
  , rkPresent :: UtxoValues k v
    -- | Keys whose presence or absence in the argument of the diff is not
    -- determined by the diff
  , rkUnknown :: UtxoKeys k v
  }
  deriving (Eq, Generic, NoThunks, Show)

instance (Ord k, ToCBOR k, ToCBOR v) => ToCBOR (RewoundKeys k v) where
  toCBOR rew =
        versionZeroProductToCBOR
      $ map ($ rew)
      $ [toCBOR . rkAbsent, toCBOR . rkPresent, toCBOR . rkUnknown]

instance (Ord k, FromCBOR k, FromCBOR v) => FromCBOR (RewoundKeys k v) where
  fromCBOR =
        versionZeroProductFromCBOR "RewoundKeys" 3
      $ RewoundKeys <$> fromCBOR <*> fromCBOR <*> fromCBOR

mapRewoundKeys :: (v -> v') -> RewoundKeys k v -> RewoundKeys k v'
mapRewoundKeys f rew =
    RewoundKeys {
        rkAbsent  = castUtxoKeys    (rkAbsent  rew)
      , rkPresent = mapUtxoValues f (rkPresent rew)
      , rkUnknown = castUtxoKeys    (rkUnknown rew)
      }

-- | Transport a set of keys backwards through a difference
--
-- Suppose @vs2 = 'forwardValues' vs1 diff@ and @rew = 'rewindKeys' ks diff@.
--
-- Then all:
--
-- * @'rkPresent' rew@ and @restrictKeys vs1 ('rkUnknown' rew)@ partition @vs1@.
--
-- * @'rkAbsent' rew `disjoint` keysSet vs1@.
--
-- * @'rkAbsent' rew@, @keysSet ('rkPresent' rew)@, and @('rkUnknown' rew)@
--   partition @ks@.
--
-- The practical benefit is that @'rkUnknown' rew@ is a possibly-empty subset of
-- @ks@, and so could avoid unnecessary reads from the backing store containing
-- @vs1@.
rewindKeys :: forall k v. Ord k => UtxoKeys k v -> UtxoDiff k v -> RewoundKeys k v
rewindKeys (UtxoKeys query) (UtxoDiff diffs) =
    RewoundKeys {
        rkAbsent  = UtxoKeys   $ Map.keysSet $ Map.filter isIns hits
      , rkPresent = UtxoValues $ Map.mapMaybe justIfDel hits
      , rkUnknown = UtxoKeys   misses
      }
  where
    misses :: Set k
    misses = query `Set.difference` Map.keysSet diffs

    hits :: Map k (UtxoEntryDiff v)
    hits = diffs `Map.restrictKeys` query

    justIfDel :: UtxoEntryDiff v -> Maybe v
    justIfDel (UtxoEntryDiff v diffstate) = case diffstate of
      UedsIns       -> Nothing
      UedsDel       -> Just v
      UedsInsAndDel -> Nothing

    isIns :: UtxoEntryDiff v -> Bool
    isIns (UtxoEntryDiff _v diffstate) = case diffstate of
      UedsIns       -> True
      UedsDel       -> False
      UedsInsAndDel -> True

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

instance (Ord k, ToCBOR k, ToCBOR v) => ToCBOR (SeqUtxoDiff k v) where
  toCBOR (SeqUtxoDiff ft) = versionZeroProductToCBOR [toCBOR (toList ft)]

instance (Ord k, FromCBOR k, FromCBOR v) => FromCBOR (SeqUtxoDiff k v) where
  fromCBOR =
        versionZeroProductFromCBOR "SeqUtxoDiff" 1
      $ (SeqUtxoDiff . FT.fromList) <$> fromCBOR

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

instance (Ord k, ToCBOR k, ToCBOR v) => ToCBOR (SudMeasure k v) where
  toCBOR = \case
    SudMeasureNothing             -> CBOR.encodeListLen 1 <> CBOR.encodeWord 0
    SudMeasureJust size slot diff ->
         CBOR.encodeListLen 4
      <> CBOR.encodeWord 1
      <> toCBOR size
      <> toCBOR slot
      <> toCBOR diff

instance (Ord k, FromCBOR k, FromCBOR v) => FromCBOR (SudMeasure k v) where
  fromCBOR = do
      len <- CBOR.decodeListLen
      i   <- CBOR.decodeWord
      case (len, i) of
        (1, 0) -> pure SudMeasureNothing
        (4, 1) -> SudMeasureJust <$> fromCBOR <*> fromCBOR <*> fromCBOR
        o      -> fail $ "SudMeasure unknown len and constructor index: " <> show o

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

instance (Ord k, ToCBOR k, ToCBOR v) => ToCBOR (SudElement k v) where
  toCBOR (SudElement slot diff) = versionZeroProductToCBOR [toCBOR slot, toCBOR diff]

instance (Ord k, FromCBOR k, FromCBOR v) => FromCBOR (SudElement k v) where
  fromCBOR =
        versionZeroProductFromCBOR "SudElement" 1
      $ SudElement <$> fromCBOR <*> fromCBOR

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

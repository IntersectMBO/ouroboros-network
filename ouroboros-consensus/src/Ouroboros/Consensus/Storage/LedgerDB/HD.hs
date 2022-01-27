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
  , mapUtxoDiff
    -- ** Internals
  , UtxoEntryDiffState (..)
  , UtxoEntryDiff (..)
    -- * Combinators
  , RewoundKeys (..)
  , forwardValues
  , mapRewoundKeys
  , rewindKeys
    -- * Backing store interface
  , BackingStore (..)
  , BackingStorePath (..)
  , BackingStoreValueHandle (..)
  , bsRead
  , withBsValueHandle
    -- ** An in-memory backing store
  , newTVarBackingStore
  , TVarBackingStoreClosedExn (..)
  , TVarBackingStoreDeserialiseExn (..)
  , TVarBackingStoreValueHandleClosedExn (..)
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
  , splitImmutableAtSeqUtxoDiff
    -- ** Internals
  , SudElement (..)
  , SudMeasure (..)
  ) where

import           Codec.Serialise (DeserialiseFailure, Serialise,
                     deserialiseOrFail, serialise)
import qualified Control.Exception as Exn
import           Control.Monad (join, void)
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as MapMerge
import           Data.Set (Set)
import qualified Data.Set as Set
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)

import           Cardano.Slotting.Slot (SlotNo, WithOrigin (..))

import           Data.FingerTree.Strict (StrictFingerTree)
import qualified Data.FingerTree.Strict as FT

import           Ouroboros.Consensus.Config.SecurityParam (SecurityParam (..))
import qualified Ouroboros.Consensus.Storage.FS.API as FS
import qualified Ouroboros.Consensus.Storage.FS.API.Types as FS
import           Ouroboros.Consensus.Util.IOLike (IOLike)
import qualified Ouroboros.Consensus.Util.IOLike as IOLike

{-------------------------------------------------------------------------------
  Map of values
-------------------------------------------------------------------------------}

-- | An mapping of tx inputs (ie a transaction id and an output index)
-- to tx outputs (eg an address and an amount)
--
-- The map must be a /functional/ (if a key is present, its value is uniquely
-- determined by context). Moreover, any specific key must only be inserted at
-- most once.
newtype UtxoValues k v = UtxoValues (Map k v)
  deriving (Generic, NoThunks)

instance Ord k => Monoid (UtxoValues k v) where
  mempty = UtxoValues Map.empty
  
-- | Note that this fails via 'error' on collisions
instance Ord k => Semigroup (UtxoValues k v) where
  UtxoValues m1 <> UtxoValues m2 =
      UtxoValues $ Map.unionWith err m1 m2
    where
      err = error "impossible! Semigroup UtxoValues collision"

emptyUtxoValues :: UtxoValues k v
emptyUtxoValues = UtxoValues Map.empty

-- | The function should be determined by the @v@ and @v'@ types
mapUtxoValues :: (v -> v') -> UtxoValues k v -> UtxoValues k v'
mapUtxoValues f (UtxoValues vs) = UtxoValues $ fmap f vs

{-------------------------------------------------------------------------------
  Difference of maps
-------------------------------------------------------------------------------}

-- | The differences that could be applied to a 'UtxoValues'
newtype UtxoDiff k v = UtxoDiff (Map k (UtxoEntryDiff v))
  deriving (Generic, NoThunks)

-- | The key's value and how it changed
data UtxoEntryDiff v = UtxoEntryDiff !v !UtxoEntryDiffState
  deriving (Generic, NoThunks)

-- | Whether an entry was deleted, inserted, or inserted-and-then-deleted
data UtxoEntryDiffState = UedsDel | UedsIns | UedsInsAndDel
  deriving (Generic, NoThunks, Show)

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
  deriving (Generic, NoThunks)

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
  deriving (Generic, NoThunks)

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
forwardValues :: Ord k => UtxoValues k v -> UtxoDiff k v -> UtxoValues k v
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
      UedsDel       -> error "impossible! delete of missing key"

    oldKeys :: k -> v -> UtxoEntryDiff v -> Maybe v
    oldKeys _k _v1 (UtxoEntryDiff _v2 diffState) = case diffState of
      UedsDel       -> Nothing
      UedsIns       -> error "impossible! duplicate insert of key"
      UedsInsAndDel -> error "impossible! duplicate insert of key"

{-------------------------------------------------------------------------------
  Backing store interface
-------------------------------------------------------------------------------}

-- | A backing store containing a 'UtxoValues'
--
-- TODO Do we also need a backup and restore mechanism? Or does any disk
-- corruption mean the node will always need to sync entirely from scratch?
data BackingStore m k v = BackingStore {
    -- | Close the backing store
    --
    -- Other methods throw exceptions if called on a closed store.
    bsClose       :: m ()
    -- | Create a persistent copy
    --
    -- Each backing store implementation will offer a way to initialize itself
    -- from such a path.
  , bsCopy        :: FS.SomeHasFS m -> BackingStorePath -> m ()
    -- | Open a 'BackingStoreValueHandle' capturing the current value of the
    -- entire database
  , bsValueHandle :: m (WithOrigin SlotNo, BackingStoreValueHandle m k v)
    -- | Apply a valid diff to the contents of the backing store
  , bsWrite       :: SlotNo -> UtxoDiff k v -> m ()
  }

newtype BackingStorePath = BackingStorePath FS.FsPath

-- | An ephemeral handle to an immutable value of the entire database
--
-- The performance cost is usually minimal unless this handle is held open too
-- long.
data BackingStoreValueHandle m k v = BackingStoreValueHandle {
    -- | Close the handle
    --
    -- Other methods throw exceptions if called on a closed handle.
    bsvhClose :: m ()
    -- | Read the given keys from the handle
    --
    -- Absent keys will merely not be present in the result instead of causing a
    -- failure or an exception.
  , bsvhRead  :: UtxoKeys k v -> m (UtxoValues k v)
  }

-- | A combination of 'bsValueHandle' and 'bsvhRead'
bsRead ::
     IOLike m
  => BackingStore m k v
  -> UtxoKeys k v
  -> m (WithOrigin SlotNo, UtxoValues k v)
bsRead store keys = withBsValueHandle store $ \slot vh -> do
    values <- bsvhRead vh keys
    pure (slot, values)

-- | A 'IOLike.bracket'ed 'bsValueHandle'
withBsValueHandle ::
     IOLike m
  => BackingStore m k v
  -> (WithOrigin SlotNo -> BackingStoreValueHandle m k v -> m a)
  -> m a
withBsValueHandle store kont =
    IOLike.bracket
      (bsValueHandle store)
      (bsvhClose . snd)
      (uncurry kont)

{-------------------------------------------------------------------------------
  An in-memory backing store
-------------------------------------------------------------------------------}

data TVarBackingStoreContents m k v =
    TVarBackingStoreContentsClosed
  | TVarBackingStoreContents
      !(WithOrigin SlotNo)
      !(UtxoValues k v)
  deriving (Generic, NoThunks)

data TVarBackingStoreClosedExn = TVarBackingStoreClosedExn
  deriving (Exn.Exception, Show)

data TVarBackingStoreValueHandleClosedExn = TVarBackingStoreValueHandleClosedExn
  deriving (Exn.Exception, Show)

newtype TVarBackingStoreDeserialiseExn = TVarBackingStoreDeserialiseExn DeserialiseFailure
  deriving (Exn.Exception, Show)

-- | Use a 'TVar' as a trivial backing store
newTVarBackingStore :: forall m k v.
     (IOLike m, NoThunks k, NoThunks v, Ord k, Serialise k, Serialise v)
  => Either
       (FS.SomeHasFS m, BackingStorePath)
       (WithOrigin SlotNo, UtxoValues k v)   -- ^ initial seqno and contents
  -> m (BackingStore m k v)
newTVarBackingStore initialization = do
    ref <- do
      (slot, vs) <- case initialization of
        Left (FS.SomeHasFS fs, BackingStorePath path) -> do
          FS.withFile fs path FS.ReadMode $ \h -> do
            bs <- FS.hGetAll fs h
            case deserialiseOrFail bs of
              Left  err        -> Exn.throw $ TVarBackingStoreDeserialiseExn err
              Right (slot, vs) -> pure (slot, UtxoValues vs :: UtxoValues k v)
        Right x -> pure x
      IOLike.newTVarIO $ TVarBackingStoreContents slot vs
    pure BackingStore {
        bsClose    = IOLike.atomically $ do
          IOLike.writeTVar ref TVarBackingStoreContentsClosed
      , bsCopy = \(FS.SomeHasFS fs) (BackingStorePath path) ->
          join $ IOLike.atomically $ do
            IOLike.readTVar ref >>= \case
              TVarBackingStoreContentsClosed                ->
                pure $ Exn.throw TVarBackingStoreClosedExn
              TVarBackingStoreContents slot (UtxoValues vs) -> pure $ do
                FS.withFile fs path (FS.WriteMode FS.MustBeNew) $ \h -> do
                  void $ FS.hPutAll fs h $ serialise (slot, vs)
      , bsValueHandle = join $ IOLike.atomically $ do
          IOLike.readTVar ref >>= \case
            TVarBackingStoreContentsClosed                ->
              pure $ Exn.throw TVarBackingStoreClosedExn
            TVarBackingStoreContents slot (UtxoValues vs) -> pure $ do
              refHandleClosed <- IOLike.newTVarIO False
              pure $ (,) slot $ BackingStoreValueHandle {
                  bsvhClose = IOLike.atomically $ do
                    IOLike.writeTVar refHandleClosed True
                , bsvhRead  = \(UtxoKeys ks) -> join $ IOLike.atomically $ do
                    isClosed <- IOLike.readTVar refHandleClosed
                    pure $
                      if isClosed
                      then Exn.throw TVarBackingStoreValueHandleClosedExn
                      else pure $ UtxoValues $ Map.restrictKeys vs ks
                }
      , bsWrite    = \slot2 diff -> join $ IOLike.atomically $ do
          IOLike.readTVar ref >>= \case
            TVarBackingStoreContentsClosed        ->
              pure $ Exn.throw TVarBackingStoreClosedExn
            TVarBackingStoreContents slot1 values ->
              Exn.assert (slot1 <= At slot2) $ do
                IOLike.writeTVar ref $
                  TVarBackingStoreContents
                    (At slot2)
                    (forwardValues values diff)
                pure $ pure ()
      }

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
  deriving (Generic, NoThunks)

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
  deriving (Generic, NoThunks)

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

-- | Isolate up to @n@ of the oldest immutable diffs
--
-- TODO is block count really what we want to us? Or maybe instead something
-- like the size of the cumulative diff?
splitImmutableAtSeqUtxoDiff ::
     Ord k
  => SecurityParam
  -> Int
  -> SeqUtxoDiff k v
  -> (SeqUtxoDiff k v, SeqUtxoDiff k v)
splitImmutableAtSeqUtxoDiff k n sq =
    splitAtSeqUtxoDiff (min n numImmutable) sq
  where
    k'           = fromEnum $ maxRollbacks k
    numImmutable = max k' (lengthSeqUtxoDiff sq) - k'

-- | Isolate the diffs that are labeled @<= slot@
--
-- TODO How to handle EBBs? Or else how to justify ignoring them?
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

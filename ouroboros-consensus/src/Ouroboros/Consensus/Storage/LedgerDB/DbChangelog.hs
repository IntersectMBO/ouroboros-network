{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

-- | A 'DbChangelog' is a data structure that holds a sequence of "virtual"
-- ledger states by internally maintaining:
--
-- - A sequence of in-memory ledger states the volatile part of the chain.
--
-- - A ledger state that is the last flushed state. Usually this will coincide
-- - with the immutable tip, but this is not necessarily the case.
--
-- - A sequence of differences that are associated with each ledger state and
--   represent the delta between the associated ledger state and its predecesor.
--   These differences are defined with respect to a 'BackingStore' that
--   provides the set of values at the anchor of the sequence, i.e. at the last
--   flushed state.
--
-- This design is based on the technical report "Storing the Cardano ledger
-- state on disk: analysis and design options" by Duncan Coutts and Douglas
-- Wilson.
--
-- See 'DbChangelog' for more information.
module Ouroboros.Consensus.Storage.LedgerDB.DbChangelog (
    -- * The DbChangelog
    DbChangelog (..)
    -- * Construction
  , empty
    -- * Updates
  , extend
  , immutableTipSlot
  , pruneVolatilePart
  , rollbackN
  , rollbackToAnchor
  , rollbackToPoint
    -- * Flush
  , DbChangelogToFlush (..)
  , FlushPolicy (..)
  , flush
  , flushIntoBackingStore
  , flushableLength
    -- * Lock
  , LedgerDBLock (..)
  , mkLedgerDBLock
  , withReadLock
  , withWriteLock
  ) where

import           Cardano.Slotting.Slot
import qualified Control.Exception as Exn
import           Data.Bifunctor (bimap, first)
import           Data.Semigroup (Sum (..))
import           Data.SOP.Functors (Product2 (..))
import           Data.SOP.Strict (K, unK)
import           Data.Word
import           GHC.Generics (Generic)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Tables.Utils
import           Ouroboros.Consensus.Storage.LedgerDB.BackingStore
import qualified Ouroboros.Consensus.Storage.LedgerDB.BackingStore as BackingStore
import           Ouroboros.Consensus.Storage.LedgerDB.DiffSeq hiding (empty,
                     extend)
import qualified Ouroboros.Consensus.Storage.LedgerDB.DiffSeq as DiffSeq
import qualified Ouroboros.Consensus.Storage.LedgerDB.DiffSeq as DS
import           Ouroboros.Consensus.Util.IOLike
import qualified Ouroboros.Consensus.Util.MonadSTM.RAWLock as Lock
import           Ouroboros.Network.AnchoredSeq (Anchorable (..),
                     AnchoredSeq (..))
import qualified Ouroboros.Network.AnchoredSeq as AS
import           Prelude hiding (splitAt)

-- | Holds a sequence of split ledger states, where the in-memory part is in a
-- sequence and the on-disk part is represented by a sequence of differences
-- that need a @BackingStore@ as an anchor point.
--
-- We illustrate its contents below, where @k = 3@ (for a state @Li@, the
-- corresponding set of differences is @Di@):
--
-- > stateAnchor | diskAnchor | states                     | tableDiffs
-- > --------------------------------------------------------------------------
-- >      0      |      0     | [ L0 ]                     | [ ]
-- >      0      |      0     | [ L0, L1 ]                 | [ D1 ]
-- >      0      |      0     | [ L0, L1, L2 ]             | [ D1, D2 ]
-- >      0      |      0     | [ L0, L1, L2, L3 ]         | [ D1, D2, D3 ]
-- >      1      |      0     | [ L0, L1, L2, L3, L4 ]     | [ D1, D2, D3, D4 ]
-- >      2      |      0     | [ L0, L1, L2, L3, L4, L5 ] | [ D1, D2, D3, D4, D5 ]    (*)
-- >      2      |      2     | [ L2, L3, L4, L5 ]         | [ D3, D4, D5 ]   -- flush (**)
-- >      3      |      2     | [ L2, L3, L4, L5, L6 ]     | [ D3, D4, D5, D6 ]
--
-- The disk anchor moves when we flush data to disk, and the state anchor points
-- always to the state that represents the tip of the logical immutable
-- database. Notice that @seqNo (last states) - stateAnchor@ is usually @k@
-- except when rollbacks or data corruption take place and will be less than @k@
-- when we just loaded a snapshot. We cannot roll back more than @k@ blocks.
-- This means that after a rollback of @k@ blocks at (*), the changelog will
-- look something like this:
--
-- >      2      |      0     | [ L0, L1, L2 ]             | [ D1, D2 ]
--
-- And a rollback of @k@ blocks at (**) will look something like this:
--
-- >      2      |      0     | [ L2 ]                     | [ ]
--
-- Notice how the states list always contains the in-memory state of the anchor,
-- but the table differences might not contain the differences for that anchor
-- if they have been flushed to the backend.
--
-- As said above, this @DbChangelog@ has to be coupled with a @BackingStore@
-- which provides the pointers to the on-disk data.
data DbChangelog l = DbChangelog {
    -- | The last flushed ledger state.
    --
    -- We need to keep track of this one as this will be the state written to
    -- disk when we make a snapshot
    changelogAnchor         :: !(l EmptyMK)

    -- | The sequence of differences between the last flushed state
    -- ('changelogAnchor') and the tip of the volatile sequence
    -- ('changelogVolatileStates').
  , changelogDiffs          :: !(LedgerTables l SeqDiffMK)

    -- | The volatile sequence of states.
    --
    -- The anchor of this sequence is the immutable tip, so whenever we flush,
    -- we should do so up until that point. The length of this sequence will be
    -- @k@ except in abnormal circumstances like rollbacks or data corruption.
  , changelogVolatileStates ::
      !(AnchoredSeq
          (WithOrigin SlotNo)
          (l EmptyMK)
          (l EmptyMK)
       )
  }
  deriving (Generic)

deriving instance (Eq       (LedgerTables l SeqDiffMK), Eq       (l EmptyMK))
               =>  Eq       (DbChangelog l)
deriving instance (NoThunks (LedgerTables l SeqDiffMK), NoThunks (l EmptyMK))
               =>  NoThunks (DbChangelog l)
deriving instance (Show     (LedgerTables l SeqDiffMK), Show     (l EmptyMK))
               =>  Show     (DbChangelog l)

instance GetTip l => AS.Anchorable (WithOrigin SlotNo) (l EmptyMK) (l EmptyMK) where
  asAnchor = id
  getAnchorMeasure _ = getTipSlot

instance ( IsLedger l
         , HeaderHash (K @MapKind (DbChangelog l)) ~ HeaderHash l
         ) => GetTip (K (DbChangelog l)) where
  getTip = castPoint
         . getTip
         . either id id
         . AS.head
         . changelogVolatileStates
         . unK

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

empty ::
     (HasLedgerTables l, GetTip l)
  => l EmptyMK -> DbChangelog l
empty anchor =
    DbChangelog {
        changelogAnchor         = anchor
      , changelogDiffs          = pureLedgerTables (SeqDiffMK DS.empty)
      , changelogVolatileStates = AS.Empty anchor
      }

{-------------------------------------------------------------------------------
  Updates
-------------------------------------------------------------------------------}

extend ::
     (HasLedgerTables l, GetTip l)
  => DbChangelog l -> l DiffMK -> DbChangelog l
extend dblog newState =
    DbChangelog {
        changelogAnchor
      , changelogDiffs          =
          zipLedgerTables ext changelogDiffs tablesDiff
      , changelogVolatileStates =
          changelogVolatileStates AS.:> l'
      }
  where
    DbChangelog {
        changelogAnchor
      , changelogDiffs
      , changelogVolatileStates
      } = dblog

    l'         = forgetLedgerTables  newState
    tablesDiff = projectLedgerTables newState

    slot =
      fromWithOrigin
        (error "impossible! extending with an state at Origin")
        (getTipSlot l')

    ext ::
         (Ord k, Eq v)
      => SeqDiffMK k v
      -> DiffMK    k v
      -> SeqDiffMK k v
    ext (SeqDiffMK sq) (DiffMK d) =
      SeqDiffMK $ DS.extend sq slot d

pruneVolatilePart ::
     GetTip l
  => SecurityParam -> DbChangelog l -> DbChangelog l
pruneVolatilePart (SecurityParam k) dblog =
    DbChangelog {
        changelogAnchor
      , changelogDiffs
      , changelogVolatileStates = vol'
      }
  where
    DbChangelog {
        changelogAnchor
      , changelogDiffs
      , changelogVolatileStates = vol
      } = dblog

    nvol = AS.length vol

    vol' =
      if toEnum nvol <= k
      then vol
      else snd $ AS.splitAt (nvol - fromEnum k) vol

-- | Roll back the volatile states up to the specified point.
rollbackToPoint ::
     ( StandardHash l
     , GetTip l
     , HasLedgerTables l
     )
  => Point l -> DbChangelog l -> Maybe (DbChangelog l)
rollbackToPoint pt dblog = do
    vol' <-
      AS.rollback
        (pointSlot pt)
        ((== pt) . getTip . either id id)
        vol
    let ndropped = AS.length vol - AS.length vol'
        diffs'   = mapLedgerTables (trunc ndropped) changelogDiffs
    Exn.assert (ndropped >= 0) $ pure DbChangelog {
          changelogAnchor
        , changelogDiffs           = diffs'
        , changelogVolatileStates  = vol'
        }
  where
    DbChangelog {
        changelogAnchor
      , changelogDiffs
      , changelogVolatileStates = vol
      } = dblog

rollbackToAnchor ::
     (GetTip l, HasLedgerTables l)
  => DbChangelog l -> DbChangelog l
rollbackToAnchor dblog =
    DbChangelog {
        changelogAnchor
      , changelogDiffs           = diffs'
      , changelogVolatileStates  = AS.Empty (AS.anchor vol)
      }
  where
    DbChangelog {
        changelogAnchor
      , changelogDiffs
      , changelogVolatileStates = vol
      } = dblog

    ndropped = AS.length vol
    diffs'   =
      mapLedgerTables (trunc ndropped) changelogDiffs

trunc ::
     (Ord k, Eq v)
  => Int -> SeqDiffMK k v -> SeqDiffMK k v
trunc n (SeqDiffMK sq) =
  SeqDiffMK $ fst $ splitAtFromEnd n sq

rollbackN ::
     (GetTip l, HasLedgerTables l)
  => Int -> DbChangelog l -> DbChangelog l
rollbackN n dblog =
    DbChangelog {
        changelogAnchor
      , changelogDiffs          = mapLedgerTables (trunc n) changelogDiffs
      , changelogVolatileStates = AS.dropNewest n changelogVolatileStates
      }
  where
    DbChangelog {
        changelogAnchor
      , changelogDiffs
      , changelogVolatileStates
      } = dblog

immutableTipSlot ::
     GetTip l
  => DbChangelog l -> WithOrigin SlotNo
immutableTipSlot =
      getTipSlot
    . AS.anchor
    . changelogVolatileStates

{-------------------------------------------------------------------------------
  Flushing
-------------------------------------------------------------------------------}

flushableLength :: HasLedgerTables l => SecurityParam -> DbChangelog l -> Word64
flushableLength (SecurityParam k) =
    (\(Sum x) -> x - k)
  . foldLedgerTables f
  . changelogDiffs
 where
   f :: (Ord k, Eq v)
     => SeqDiffMK k v
     -> Sum Word64
   f (SeqDiffMK sq) = Sum $ fromIntegral $ DiffSeq.length sq

-- | The flush policy
newtype FlushPolicy =
    -- | Always flush everything older than the immutable tip
    FlushAllImmutable SecurityParam

-- | "Flush" the 'DbChangelog' by splitting it into two 'DbChangelogs', one that
-- contains the diffs that should be flushed into the Backing store (see
-- 'flushIntoBackingStore') and one to be considered as the new 'DbChangelog'.
flush ::
     (GetTip l, HasLedgerTables l)
  => FlushPolicy
  -> DbChangelog l
  -> (Maybe (DbChangelogToFlush l), DbChangelog l)
flush (FlushAllImmutable (SecurityParam k)) dblog =
      (ldblog, rdblog)
  where
    DbChangelog {
        changelogDiffs
      , changelogVolatileStates
      } = dblog

    immTip = AS.anchor changelogVolatileStates

    -- TODO: #4371 by point, not by count, so sequences can be ragged
    splitSeqDiff ::
         (Ord k, Eq v)
      => SeqDiffMK k v
      -> (SeqDiffMK k v, SeqDiffMK k v)
    splitSeqDiff (SeqDiffMK sq) =
        bimap (maybe emptyMK SeqDiffMK) SeqDiffMK
      $ if DiffSeq.length sq > fromIntegral k
        then first Just $ splitAtFromEnd (fromIntegral k) sq
        else (Nothing, sq)

    lr = mapLedgerTables (uncurry Pair2 . splitSeqDiff) changelogDiffs
    l = mapLedgerTables (\(Pair2 x _) -> x) lr
    r = mapLedgerTables (\(Pair2 _ y) -> y) lr

    prj ::
         (Ord k, Eq v)
      => SeqDiffMK k v
      -> DiffMK k v
    prj (SeqDiffMK sq) = DiffMK (DS.cumulativeDiff sq)

    ldblog =
      if foldLedgerTables (\(SeqDiffMK sq) -> Sum $ DiffSeq.length sq) l == 0
      then Nothing
      else Just $ DbChangelogToFlush {
        toFlushDiffs = mapLedgerTables prj l
      , toFlushSlot  =
            fromWithOrigin (error "Flushing a DbChangelog at origin should never happen")
          $ getTipSlot immTip
      }

    rdblog = DbChangelog {
        changelogAnchor = immTip
      , changelogDiffs  = r
      , changelogVolatileStates
      }

-- | A simplified 'DbChangelog' that should be used for flushing.
data DbChangelogToFlush l = DbChangelogToFlush {
    -- | The set of differences that should be flushed into the 'BackingStore'
    toFlushDiffs :: !(LedgerTables l DiffMK)
    -- | At which slot the diffs were split. This must be the slot of the state
    -- considered as "last flushed" in the kept 'DbChangelog'
  , toFlushSlot  :: !SlotNo
  }

-- | Flush **all the changes in this DbChangelog** into the backing store
--
-- Note that 'flush' must have been called to split the 'DbChangelog' on the
-- immutable tip and produce two 'DbChangelog's, one to flush and one to keep.
--
-- The 'Ouroboros.Consensus.Storage.ChainDB.Impl.LgrDB.LgrDb'
-- 'Ouroboros.Consensus.Storage.ChainDB.Impl.LgrDB.flushLock' write lock must be
-- held before calling this function.
--
-- PRECONDITION: @dblog@ should only contain the diffs for the immutable part
-- of the changelog. If not, the @slot@ that we flush to the backing store will
-- not match the actual tip of the diffs that we flush to the backing store.
flushIntoBackingStore ::
  LedgerBackingStore m l -> DbChangelogToFlush l -> m ()
flushIntoBackingStore (LedgerBackingStore backingStore) dblog =
  BackingStore.bsWrite
    backingStore
    (toFlushSlot dblog)
    (toFlushDiffs dblog)

{-------------------------------------------------------------------------------
  LedgerDB lock
-------------------------------------------------------------------------------}

-- | A lock to prevent the LedgerDB (i.e. a 'DbChangelog') from getting out of
-- sync with the 'BackingStore'.
--
-- We rely on the capability of the @BackingStore@s of providing
-- 'BackingStoreValueHandles' that can be used to hold a persistent view of the
-- database as long as the handle is open. Assuming this functionality, the lock
-- is used in three ways:
--
-- - Read lock to acquire a value handle: we do this when acquiring a view of the
--   'LedgerDB' (which lives in a 'StrictTVar' at the 'ChainDB' level) and of
--   the 'BackingStore'. We momentarily acquire a read lock, consult the
--   transactional variable and also open a 'BackingStoreValueHandle'. This is
--   the case for ledger state queries and for the forging loop.
--
-- - Read lock to ensure two operations are in sync: in the above situation, we
--   relied on the 'BackingStoreValueHandle' functionality, but sometimes we
--   won't access the values through a value handle, and instead we might use
--   the LMDB environment (as it is the case for 'lmdbCopy'). In these cases, we
--   acquire a read lock until we ended the copy, so that writers are blocked
--   until this process is completed. This is the case when taking a snapshot.
--
-- - Write lock when flushing differences.
newtype LedgerDBLock m = LedgerDBLock (Lock.RAWLock m ())
  deriving newtype NoThunks

mkLedgerDBLock :: IOLike m => m (LedgerDBLock m)
mkLedgerDBLock = LedgerDBLock <$> Lock.new ()

-- | Acquire the ledger DB read lock and hold it while performing an action
withReadLock :: IOLike m => LedgerDBLock m -> m a -> m a
withReadLock (LedgerDBLock lock) m =
    Lock.withReadAccess lock (\() -> m)

-- | Acquire the ledger DB write lock and hold it while performing an action
withWriteLock :: IOLike m => LedgerDBLock m -> m a -> m a
withWriteLock (LedgerDBLock lock) m =
    Lock.withWriteAccess lock (\() -> (,) () <$> m)

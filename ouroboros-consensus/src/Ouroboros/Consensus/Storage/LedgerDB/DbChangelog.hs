{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DeriveAnyClass           #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UndecidableInstances     #-}

-- | A 'DbChangelog' is a data structure that holds a sequence of "virtual"
-- ledger states by internally maintaining 3 sequences:
--
-- - Two sequences of in-memory ledger states, the volatile and the immutable
--   parts of the chain.
--
-- - A sequence of differences that are associated with each ledger state. These
--   differences are defined with respect to a 'BackingStore' that provides the
--   set of values at the anchor of the sequence.
--
-- This design is based on the technical report "Storing the Cardano ledger
-- state on disk: analysis and design options" by Duncan Coutts and Douglas
-- Wilson.
--
-- See 'DbChangelog' for more information.
module Ouroboros.Consensus.Storage.LedgerDB.DbChangelog (
    -- * The DbChangelog
    DbChangelog (..)
  , DbChangelogState (..)
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
  , DbChangelogFlushPolicy (..)
  , flush
  , flushIntoBackingStore
  ) where

import           Cardano.Slotting.Slot
import qualified Control.Exception as Exn
import           Data.Bifunctor (bimap)
import           Data.SOP.Functors (Product2 (..))
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Tables
import           Ouroboros.Consensus.Ledger.Tables.Utils
import           Ouroboros.Consensus.Storage.LedgerDB.BackingStore
import qualified Ouroboros.Consensus.Storage.LedgerDB.BackingStore as BackingStore
import           Ouroboros.Consensus.Storage.LedgerDB.DiffSeq hiding (empty,
                     extend)
import qualified Ouroboros.Consensus.Storage.LedgerDB.DiffSeq as DS
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
--
-- INVARIANT: the head of 'changelogImmutableStates' is the anchor of
-- 'changelogVolatileStates'.
data DbChangelog l = DbChangelog {
    changelogDiffAnchor      :: !(WithOrigin SlotNo)
  , changelogDiffs           :: !(LedgerTables l SeqDiffMK)
  , changelogImmutableStates ::
      !(AnchoredSeq
          (WithOrigin SlotNo)
          (DbChangelogState l)
          (DbChangelogState l)
       )
  , changelogVolatileStates  ::
      !(AnchoredSeq
          (WithOrigin SlotNo)
          (DbChangelogState l)
          (DbChangelogState l)
       )
  }
  deriving (Generic)

deriving instance (Eq       (LedgerTables l SeqDiffMK), Eq       (l EmptyMK))
               =>  Eq       (DbChangelog l)
deriving instance (NoThunks (LedgerTables l SeqDiffMK), NoThunks (l EmptyMK))
               =>  NoThunks (DbChangelog l)
deriving instance (Show     (LedgerTables l SeqDiffMK), Show     (l EmptyMK))
               =>  Show     (DbChangelog l)

newtype DbChangelogState l = DbChangelogState {unDbChangelogState :: l EmptyMK}
  deriving (Generic)

deriving instance Eq       (l EmptyMK) => Eq       (DbChangelogState l)
deriving instance NoThunks (l EmptyMK) => NoThunks (DbChangelogState l)
deriving instance Show     (l EmptyMK) => Show     (DbChangelogState l)

instance GetTip (l EmptyMK) => AS.Anchorable (WithOrigin SlotNo) (DbChangelogState l) (DbChangelogState l) where
  asAnchor = id
  getAnchorMeasure _ = getTipSlot . unDbChangelogState

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

empty ::
     (HasLedgerTables l, GetTip (l EmptyMK))
  => l EmptyMK -> DbChangelog l
empty anchor =
    DbChangelog {
        changelogDiffAnchor      = getTipSlot anchor
      , changelogDiffs           = pureLedgerTables (SeqDiffMK DS.empty)
      , changelogImmutableStates = AS.Empty (DbChangelogState anchor)
      , changelogVolatileStates  = AS.Empty (DbChangelogState anchor)
      }

{-------------------------------------------------------------------------------
  Updates
-------------------------------------------------------------------------------}

extend ::
     (HasLedgerTables l, GetTip (l EmptyMK))
  => DbChangelog l -> l DiffMK -> DbChangelog l
extend dblog newState =
    DbChangelog {
        changelogDiffAnchor
      , changelogDiffs           =
          zipLedgerTables ext changelogDiffs tablesDiff
      , changelogImmutableStates
      , changelogVolatileStates  =
          changelogVolatileStates AS.:> DbChangelogState l'
      }
  where
    DbChangelog {
        changelogDiffAnchor
      , changelogDiffs
      , changelogImmutableStates
      , changelogVolatileStates
      } = dblog

    l'         = forgetLedgerTables  newState
    tablesDiff = projectLedgerTables newState

    slot = case getTipSlot l' of
      Origin -> error "impossible! extendDbChangelog"
      At s   -> s

    ext ::
         (Ord k, Eq v)
      => SeqDiffMK k v
      -> DiffMK    k v
      -> SeqDiffMK k v
    ext (SeqDiffMK sq) (DiffMK d) =
      SeqDiffMK $ DS.extend sq slot d

pruneVolatilePart ::
     (GetTip (l EmptyMK), StandardHash (l EmptyMK))
  => SecurityParam -> DbChangelog l -> DbChangelog l
pruneVolatilePart (SecurityParam k) dblog =
    Exn.assert (AS.length imm' + AS.length vol' == AS.length imm + AS.length vol) $
    DbChangelog {
        changelogDiffAnchor
      , changelogDiffs
      , changelogImmutableStates = imm'
      , changelogVolatileStates  = vol'
      }
  where
    DbChangelog {
        changelogDiffAnchor
      , changelogDiffs
      , changelogImmutableStates
      , changelogVolatileStates
      } = dblog

    imm = changelogImmutableStates
    vol = changelogVolatileStates

    nvol = AS.length vol

    (imm', vol') =
      if toEnum nvol <= k then (imm, vol) else
      let (l, r) = AS.splitAt (nvol - fromEnum k) vol
      in case AS.join (\e a -> getTip (unDbChangelogState a) ==
                               either (getTip . unDbChangelogState)
                                      (getTip . unDbChangelogState) e) imm l of
        Nothing     -> error "Critical inconsistency! The immutable and volatile ledger databases don't fit together"
        Just joined -> (joined, r)

-- | Roll back the volatile states up to the specified point.
rollbackToPoint ::
     ( StandardHash (l EmptyMK)
     , GetTip (l EmptyMK)
     , HasLedgerTables l
     )
  => Point (l EmptyMK) -> DbChangelog l -> Maybe (DbChangelog l)
rollbackToPoint pt dblog = do
    let vol = changelogVolatileStates
    vol' <-
      AS.rollback
        (pointSlot pt)
        ((== pt) . getTip . unDbChangelogState . either id id)
        vol
    let ndropped                  = AS.length vol - AS.length vol'
        diffs'                    =
          mapLedgerTables (trunc ndropped) changelogDiffs
    Exn.assert (ndropped >= 0) $ pure DbChangelog {
          changelogDiffAnchor
        , changelogDiffs           = diffs'
        , changelogImmutableStates
        , changelogVolatileStates  = vol'
        }
  where
    DbChangelog {
        changelogDiffAnchor
      , changelogDiffs
      , changelogImmutableStates
      , changelogVolatileStates
      } = dblog

rollbackToAnchor ::
     (GetTip (l EmptyMK), HasLedgerTables l)
  => DbChangelog l -> DbChangelog l
rollbackToAnchor dblog =
    DbChangelog {
        changelogDiffAnchor
      , changelogDiffs           = diffs'
      , changelogImmutableStates
      , changelogVolatileStates  = AS.Empty (AS.anchor vol)
      }
  where
    DbChangelog {
        changelogDiffAnchor
      , changelogDiffs
      , changelogImmutableStates
      , changelogVolatileStates
      } = dblog

    vol                       = changelogVolatileStates
    ndropped                  = AS.length vol
    diffs'                    =
      mapLedgerTables (trunc ndropped) changelogDiffs

trunc ::
     (Ord k, Eq v)
  => Int -> SeqDiffMK k v -> SeqDiffMK k v
trunc n (SeqDiffMK sq) =
  SeqDiffMK $ fst $ splitAtFromEnd n sq

rollbackN ::
     (GetTip (l EmptyMK), HasLedgerTables l)
  => Int -> DbChangelog l -> DbChangelog l
rollbackN n dblog =
    DbChangelog {
        changelogDiffAnchor
      , changelogDiffs           = mapLedgerTables (trunc n) changelogDiffs
      , changelogImmutableStates
      , changelogVolatileStates  = AS.dropNewest n changelogVolatileStates
      }
  where
    DbChangelog {
        changelogDiffAnchor
      , changelogDiffs
      , changelogImmutableStates
      , changelogVolatileStates
      } = dblog

immutableTipSlot ::
     GetTip (l EmptyMK)
  => DbChangelog l -> WithOrigin SlotNo
immutableTipSlot =
      getTipSlot
    . either unDbChangelogState unDbChangelogState
    . AS.head
    . changelogImmutableStates

{-------------------------------------------------------------------------------
  Flushing
-------------------------------------------------------------------------------}

-- | The flush policy
data DbChangelogFlushPolicy =
    -- | Always flush everything older than the immutable tip
    DbChangelogFlushAllImmutable

-- | "Flush" the 'DbChangelog' by splitting it into two 'DbChangelogs', one that
-- contains the diffs that should be flushed into the Backing store (see
-- 'flushIntoBackingStore') and one to be considered as the new 'DbChangelog'.
flush ::
     forall l.
     (GetTip (l EmptyMK), HasLedgerTables l)
  => DbChangelogFlushPolicy
  -> DbChangelog l
  -> (DbChangelog l, DbChangelog l)
flush DbChangelogFlushAllImmutable dblog =
      (ldblog, rdblog)
  where
    DbChangelog {
        changelogDiffAnchor
      , changelogDiffs
      , changelogImmutableStates
      , changelogVolatileStates
      } = dblog

    imm = changelogImmutableStates
    vol = changelogVolatileStates

    immTip = AS.anchor vol

    -- TODO: #4371 by point, not by count, so sequences can be ragged
    splitSeqDiff ::
         (Ord k, Eq v)
      => SeqDiffMK k v
      -> (SeqDiffMK k v, SeqDiffMK k v)
    splitSeqDiff (SeqDiffMK sq) =
        bimap SeqDiffMK SeqDiffMK
      $ splitAt (AS.length imm) sq

    lr :: LedgerTables l (Product2 SeqDiffMK SeqDiffMK)
    lr = mapLedgerTables (uncurry Pair2 . splitSeqDiff) changelogDiffs

    l = mapLedgerTables (\(Pair2 x _) -> x) lr
    r = mapLedgerTables (\(Pair2 _ y) -> y) lr

    ldblog = DbChangelog {
        changelogDiffAnchor
      , changelogDiffs           = l
      , changelogImmutableStates = imm
      , changelogVolatileStates  = AS.Empty immTip
      }

    rdblog = DbChangelog {
        changelogDiffAnchor      = getTipSlot (unDbChangelogState immTip)
      , changelogDiffs           = r
      , changelogImmutableStates = AS.Empty immTip
      , changelogVolatileStates  = vol
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
     (Applicative m, HasLedgerTables l, GetTip (l EmptyMK))
  => LedgerBackingStore m l -> DbChangelog l -> m ()
flushIntoBackingStore (LedgerBackingStore backingStore) dblog =
    case immutableTipSlot dblog of
      Origin  -> pure ()   -- the diff is necessarily empty
      At slot ->
        BackingStore.bsWrite
          backingStore
          slot
          (mapLedgerTables prj $ changelogDiffs dblog)
  where
    prj ::
         (Ord k, Eq v)
      => SeqDiffMK k v
      -> DiffMK k v
    prj (SeqDiffMK sq) = DiffMK (DS.cumulativeDiff sq)

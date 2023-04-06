
-- | How to rewind, read and forward a set of keys through a db changelog,
-- and use it to apply a function that expects a hydrated state as input.
module Ouroboros.Consensus.Storage.LedgerDB.ReadsKeySets (
    -- * Rewind
    RewoundTableKeySets (..)
  , rewindTableKeySets
    -- * Read
  , KeySetsReader
  , PointNotFound (..)
  , getLedgerTablesFor
  , readKeySets
  , readKeySetsWith
  , trivialKeySetsReader
  , withKeysReadSets
    -- * Forward
  , UnforwardedReadSets (..)
  , forwardTableKeySets
  , forwardTableKeySets'
  ) where

import           Cardano.Slotting.Slot
import           Data.Map.Diff.Strict.Internal (unsafeApplyDiffForKeys)
import           GHC.Stack (HasCallStack)
import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Ledger.Basics (GetTip, getTipSlot)
import           Ouroboros.Consensus.Ledger.Tables
import           Ouroboros.Consensus.Storage.LedgerDB.BackingStore
import           Ouroboros.Consensus.Storage.LedgerDB.DbChangelog
import           Ouroboros.Consensus.Storage.LedgerDB.DiffSeq
import           Ouroboros.Consensus.Storage.LedgerDB.LedgerDB
import           Ouroboros.Consensus.Util.IOLike

{-------------------------------------------------------------------------------
  Rewind
-------------------------------------------------------------------------------}

data RewoundTableKeySets l =
    RewoundTableKeySets
      !(WithOrigin SlotNo)   -- ^ the slot to which the keys were rewound
      !(LedgerTables l KeysMK)

rewindTableKeySets ::
     GetTip l
  => DbChangelog l -> LedgerTables l KeysMK -> RewoundTableKeySets l
rewindTableKeySets =
      RewoundTableKeySets . getTipSlot . changelogAnchor

{-------------------------------------------------------------------------------
  Read
-------------------------------------------------------------------------------}

type KeySetsReader m l = RewoundTableKeySets l -> m (UnforwardedReadSets l)

readKeySets ::
     IOLike m
  => LedgerBackingStore m l
  -> KeySetsReader m l
readKeySets (LedgerBackingStore backingStore) rew = do
    readKeySetsWith (bsRead backingStore) rew

readKeySetsWith ::
     Monad m
  => (LedgerTables l KeysMK -> m (WithOrigin SlotNo, LedgerTables l ValuesMK))
  -> RewoundTableKeySets l
  -> m (UnforwardedReadSets l)
readKeySetsWith readKeys (RewoundTableKeySets _seqNo rew) = do
    (slot, values) <- readKeys rew
    pure UnforwardedReadSets {
        ursSeqNo  = slot
      , ursValues = values
      , ursKeys   = rew
    }

withKeysReadSets ::
  (HasLedgerTables l, GetTip l, Monad m, HasCallStack)
  => l mk1
  -> KeySetsReader m l
  -> DbChangelog l
  -> LedgerTables l KeysMK
  -> (l ValuesMK -> m a)
  -> m a
withKeysReadSets st ksReader dbch ks f = do
      let aks = rewindTableKeySets dbch ks
      urs <- ksReader aks
      case withHydratedLedgerState st dbch urs f of
        Left err ->
          -- We performed the rewind;read;forward sequence in this function. So
          -- the forward operation should not fail. If this is the case we're in
          -- the presence of a problem that we cannot deal with at this level,
          -- so we throw an error.
          --
          -- When we introduce pipelining, if the forward operation fails it
          -- could be because the DB handle was modified by a DB flush that took
          -- place when __after__ we read the unforwarded keys-set from disk.
          -- However, performing rewind;read;forward with the same __locked__
          -- changelog should always succeed.
          error $ "Changelog rewind;read;forward sequence failed, " <> show err
        Right res -> res

withHydratedLedgerState ::
     (GetTip l, HasLedgerTables l)
  => l mk1
  -> DbChangelog l
  -> UnforwardedReadSets l
  -> (l ValuesMK -> a)
  -> Either RewindReadFwdError a
withHydratedLedgerState st dbch urs f =
          f
      .   withLedgerTables st
      <$> forwardTableKeySets dbch urs

-- | The requested point is not found on the ledger db
newtype PointNotFound blk = PointNotFound (Point blk) deriving (Eq, Show)

-- | Read and forward the values up to the tip of the given ledger db. Returns
-- Left if the anchor moved. If Left is returned, then the caller was just
-- unlucky and scheduling of events happened to move the backing store. Reading
-- again the LedgerDB and calling this function must eventually succeed.
getLedgerTablesFor ::
     (Monad m, HasLedgerTables l, GetTip l)
  => LedgerDB l
  -> LedgerTables l KeysMK
  -> KeySetsReader m l
  -> m (Either RewindReadFwdError (LedgerTables l ValuesMK))
getLedgerTablesFor db keys ksRead = do
  let aks = rewindTableKeySets db keys
  urs <- ksRead aks
  pure $ forwardTableKeySets db urs

trivialKeySetsReader :: (Monad m, LedgerTablesAreTrivial l) => KeySetsReader m l
trivialKeySetsReader (RewoundTableKeySets s _) =
  pure $ UnforwardedReadSets s trivialLedgerTables trivialLedgerTables

{-------------------------------------------------------------------------------
  Forward
-------------------------------------------------------------------------------}

data UnforwardedReadSets l = UnforwardedReadSets {
    -- | The Slot number of the anchor of the 'DbChangelog' that was used when
    -- rewinding and reading.
    ursSeqNo  :: !(WithOrigin SlotNo)
    -- | The values that were found in the 'BackingStore'.
  , ursValues :: !(LedgerTables l ValuesMK)
    -- | All the requested keys, being or not present in the 'BackingStore'.
  , ursKeys   :: !(LedgerTables l KeysMK)
  }

-- | The DbChangelog and the BackingStore got out of sync. This is a critical
-- error, we cannot recover from this.
data RewindReadFwdError = RewindReadFwdError {
    rrfBackingStoreAt :: !(WithOrigin SlotNo)
  , rrfDbChangelogAt  :: !(WithOrigin SlotNo)
  } deriving Show

forwardTableKeySets' ::
     HasLedgerTables l
  => WithOrigin SlotNo
  -> LedgerTables l SeqDiffMK
  -> UnforwardedReadSets l
  -> Either RewindReadFwdError
            (LedgerTables l ValuesMK)
forwardTableKeySets' seqNo chdiffs = \(UnforwardedReadSets seqNo' values keys) ->
    if seqNo /= seqNo'
    then Left $ RewindReadFwdError seqNo' seqNo
    else Right $ zipLedgerTables3 forward values keys chdiffs
  where
    forward ::
         (Ord k, Eq v)
      => ValuesMK  k v
      -> KeysMK    k v
      -> SeqDiffMK k v
      -> ValuesMK  k v
    forward (ValuesMK values) (KeysMK keys) (SeqDiffMK diffs) =
      ValuesMK $ unsafeApplyDiffForKeys values keys (cumulativeDiff diffs)

forwardTableKeySets ::
     (GetTip l, HasLedgerTables l)
  => DbChangelog l
  -> UnforwardedReadSets l
  -> Either RewindReadFwdError
            (LedgerTables l ValuesMK)
forwardTableKeySets dblog =
  forwardTableKeySets'
    (getTipSlot $ changelogAnchor dblog)
    (changelogDiffs dblog)

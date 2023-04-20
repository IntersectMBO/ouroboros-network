{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | How to rewind, read and forward a set of keys through a db changelog,
-- and use it to apply a function that expects a hydrated state as input.
module Ouroboros.Consensus.Storage.LedgerDB.ReadsKeySets (
    -- * Rewind
    RewoundTableKeySets (..)
  , rewindTableKeySets
    -- * Read
  , KeySetsReader
  , PointNotFound (..)
  , getLedgerTablesAtFor
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
import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Ledger.Basics (IsLedger)
import           Ouroboros.Consensus.Ledger.Tables
import           Ouroboros.Consensus.Storage.LedgerDB.BackingStore
import           Ouroboros.Consensus.Storage.LedgerDB.DbChangelog
import           Ouroboros.Consensus.Storage.LedgerDB.DiffSeq
import           Ouroboros.Consensus.Storage.LedgerDB.LedgerDB (LedgerDB (..))
import           Ouroboros.Consensus.Storage.LedgerDB.Query (rollback)
import           Ouroboros.Consensus.Util.IOLike

{-------------------------------------------------------------------------------
  Rewind
-------------------------------------------------------------------------------}

data RewoundTableKeySets l =
    RewoundTableKeySets
      !(WithOrigin SlotNo)   -- ^ the slot to which the keys were rewound
      !(LedgerTables l KeysMK)

rewindTableKeySets ::
     DbChangelog l -> LedgerTables l KeysMK -> RewoundTableKeySets l
rewindTableKeySets dblog =
      RewoundTableKeySets
        (changelogDiffAnchor dblog)

{-------------------------------------------------------------------------------
  Read
-------------------------------------------------------------------------------}

type KeySetsReader m l = RewoundTableKeySets l -> m (UnforwardedReadSets l)

readKeySets :: forall m l.
     IOLike m
  => LedgerBackingStore m l
  -> KeySetsReader m l
readKeySets (LedgerBackingStore backingStore) rew = do
    readKeySetsWith (bsRead backingStore) rew

readKeySetsWith :: forall m l.
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
  forall m l mk1 a.
  ( HasLedgerTables l, Monad m
  )
  => l mk1
  -> KeySetsReader m l
  -> DbChangelog l
  -> LedgerTables l KeysMK
  -> (l ValuesMK -> m a)
  -> m a
withKeysReadSets st ksReader dbch ks f = do
      let aks = rewindTableKeySets dbch ks :: RewoundTableKeySets l
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
     HasLedgerTables l
  => l mk1
  -> DbChangelog l
  -> UnforwardedReadSets l
  -> (l ValuesMK -> a)
  -> Either (WithOrigin SlotNo, WithOrigin SlotNo) a
withHydratedLedgerState st dbch urs f =
          f
      .   withLedgerTables st
      <$> forwardTableKeySets dbch urs

-- | The requested point is not found on the ledger db
data PointNotFound blk = PointNotFound !(Point blk) deriving (Eq, Show)

getLedgerTablesAtFor ::
     ( HeaderHash l ~ HeaderHash blk
     , HasHeader blk
     , IsLedger l
     , HasTickedLedgerTables l
     , IOLike m
     , StandardHash l
     )
  => Point blk
  -> LedgerTables l KeysMK
  -> LedgerDB l
  -> LedgerBackingStore m l
  -> m (Either (PointNotFound blk) (LedgerTables l ValuesMK))
getLedgerTablesAtFor pt keys lgrDb lbs =
  case rollback pt lgrDb of
    Nothing -> pure $ Left $ PointNotFound pt
    Just l  -> do
      eValues <-
        getLedgerTablesFor l keys (readKeySets lbs)
      case eValues of
        Right v -> pure $ Right v
        Left _  -> getLedgerTablesAtFor pt keys lgrDb lbs

-- | Read and forward the values up to the tip of the given ledger db. Returns
-- Left if the anchor moved. If Left is returned, then the caller was just
-- unlucky and scheduling of events happened to move the backing store. Reading
-- again the LedgerDB and calling this function must eventually succeed.
getLedgerTablesFor ::
     (Monad m, HasLedgerTables l)
  => LedgerDB l
  -> LedgerTables l KeysMK
  -> KeySetsReader m l
  -> m (Either (WithOrigin SlotNo, WithOrigin SlotNo) (LedgerTables l ValuesMK))
getLedgerTablesFor db keys ksRead = do
  let aks = rewindTableKeySets (ledgerDbChangelog db) keys
  urs <- ksRead aks
  pure $ forwardTableKeySets (ledgerDbChangelog db) urs

trivialKeySetsReader :: (Monad m, LedgerTablesAreTrivial l) => KeySetsReader m l
trivialKeySetsReader (RewoundTableKeySets s _) = pure $ UnforwardedReadSets s trivialLedgerTables trivialLedgerTables

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

forwardTableKeySets' ::
     HasLedgerTables l
  => WithOrigin SlotNo
  -> LedgerTables l SeqDiffMK
  -> UnforwardedReadSets l
  -> Either (WithOrigin SlotNo, WithOrigin SlotNo)
            (LedgerTables l ValuesMK)
forwardTableKeySets' seqNo chdiffs = \(UnforwardedReadSets seqNo' values keys) ->
    if seqNo /= seqNo' then Left (seqNo, seqNo') else
    Right
      $ zipLedgerTables3 forward values keys chdiffs
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
     HasLedgerTables l
  => DbChangelog l
  -> UnforwardedReadSets l
  -> Either (WithOrigin SlotNo, WithOrigin SlotNo)
            (LedgerTables l ValuesMK)
forwardTableKeySets dblog =
  forwardTableKeySets' (changelogDiffAnchor dblog) (changelogDiffs dblog)

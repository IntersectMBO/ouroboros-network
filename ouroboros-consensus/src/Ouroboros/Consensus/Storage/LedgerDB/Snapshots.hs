{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE MultiWayIf             #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}

{-|
  = Snapshot managing and contents

  Snapshotting a ledger state means saving a copy of the in-memory part of the
  ledger state serialized as a file on disk, as well as flushing differences on
  the ledger tables between the last snapshotted ledger state and the one that
  we are snapshotting now and making a copy of that resulting on-disk state.

  == Startup

  On startup, the node will:

  1. Find the latest snapshot which will be a directory identified by the slot
     number of the snapshot:

        > <cardano-node data dir>
        > ├── volatile
        > ├── immutable
        > └── ledger
        >     ├── <slotNumber1>
        >     │   ├── tables
        >     │   └── state
        >     ├── <slotNumber2>
        >     │   ├── tables
        >     │   └── state
        >     └── <slotNumber3>
        >         ├── tables
        >         └── state

  2. Depending on the snapshots found, there are two possibilities:

       - If there is no snapshot to load, create a new @'BackingStore'@ with the
         contents of the Genesis UTxO and finish.

       - If there is a snapshot found, then deserialize the @state@ file, which
         contains a serialization of the in-memory part of the LedgerState, with
         an empty UTxO map (i.e. a @ExtLedgerState blk EmptyMK@).

        In case we found an snapshot, we will overwrite (either literally
        overwriting it or using some feature the specific backend used) the
        @BackingStore@ tables with the @tables@ file from said snapshot as it
        was left in whatever state it was when the node shut down.

  3. The deserialized ledger state will be then used as the anchor for the
     ledger database.

  4. Replay on top of this ledger database all blocks up to the immutable
     database tip.

  At this point, the node carries a @LedgerDB@ that is initialized and ready to
  be applied blocks on the volatile database.

  == Taking snapshots during normal operation

  Snapshots are taken by the @'copyAndSnapshotRunner'@ when the disk policy
  dictates to do so. Whenever the chain grows past @k@ blocks, said runner will
  copy the blocks which are more than @k@ blocks from the tip (i.e. the ones
  that must be considered immutable) to the immutable database and then:

  1. Every time we have processed a specific amount of blocks since the last
     flush (currently statically set to 100, but ideally will be lifted into
     some configuration parameter), perform a flush of differences in the DB up
     to the immutable db tip.

  2. If dictated by the disk policy, flush immediately all the differences up to
     the immutable db tip and serialize the ledger database anchor
     @ExtLedgerState blk EmptyMK@.

     A directory is created named after the slot number of the ledger state
     being snapshotted, and the serialization from above is written into the
     @\<slotNumber\>/state@ file and the @BackingStore@ tables are copied into
     the @\<slotNumber\>/tables@ file.

  3. There is a maximum number of snapshots that should exist in the disk at any
     time, dictated by the @DiskPolicy@, so if needed, we will trim out old
     snapshots.

  == Flush during startup and snapshot at the end of startup

  Due to the nature of the database having to carry around all the differences
  between the last snapshotted state and the current tip, there is a need to
  flush when replaying the chain as otherwise, for example on a replay from
  genesis to the tip, we would carry millions of differences in memory.

  Because of this, when we are replaying blocks we will flush regularly. As the
  last snapshot that was taken lives in a @\<slotNumber\>/tables@ file, there is
  no risk of destroying it (overwriting tables at another earlier snapshot) by
  flushing. Only when we finish replaying blocks and start the background
  threads (and specifically the @copyAndSnapshotRunner@), we will take a
  snapshot of the current immutable database anchor as described above.
-}
module Ouroboros.Consensus.Storage.LedgerDB.Snapshots (
    DiskSnapshot (..)
    -- * Read from disk
  , SnapshotFailure (..)
  , diskSnapshotIsTemporary
  , listSnapshots
  , readSnapshot
    -- * Write to disk
  , takeSnapshot
  , trimSnapshots
  , writeSnapshot
    -- * Delete
  , deleteSnapshot
    -- * Paths
  , snapshotToStatePath
  , snapshotToTablesPath
    -- * Trace
  , TraceSnapshotEvent (..)
    -- * Test
  , decodeSnapshotBackwardsCompatible
  , encodeSnapshot
  ) where

import qualified Codec.CBOR.Write as CBOR
import           Codec.Serialise.Decoding (Decoder)
import qualified Codec.Serialise.Decoding as Dec
import           Codec.Serialise.Encoding (Encoding)
import           Control.Monad.Except
import           Control.Tracer
import qualified Data.List as List
import           Data.Maybe (isJust, mapMaybe)
import           Data.Ord (Down (..))
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word
import           GHC.Generics (Generic)
import           GHC.Stack
import           Text.Read (readMaybe)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Util.CBOR (ReadIncrementalErr,
                     decodeWithOrigin, readIncremental)
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.Versioned

import           Ouroboros.Consensus.Storage.FS.API
import           Ouroboros.Consensus.Storage.FS.API.Types
import qualified Ouroboros.Consensus.Storage.LedgerDB.BackingStore as BackingStore
import           Ouroboros.Consensus.Storage.LedgerDB.DiskPolicy

data DiskSnapshot = DiskSnapshot {
      -- | Snapshots are numbered. We will try the snapshots with the highest
      -- number first.
      --
      -- When creating a snapshot, we use the slot number of the ledger state it
      -- corresponds to as the snapshot number. This gives an indication of how
      -- recent the snapshot is.
      --
      -- Note that the snapshot names are only indicative, we don't rely on the
      -- snapshot number matching the slot number of the corresponding ledger
      -- state. We only use the snapshots numbers to determine the order in
      -- which we try them.
      dsNumber :: Word64

      -- | Snapshots can optionally have a suffix, separated by the snapshot
      -- number with an underscore, e.g., @4492799_last_Byron@. This suffix acts
      -- as metadata for the operator of the node. Snapshots with a suffix will
      -- /not be trimmed/.
    , dsSuffix :: Maybe String
    }
  deriving (Show, Eq, Ord, Generic)

{-------------------------------------------------------------------------------
  Read from disk
-------------------------------------------------------------------------------}

data SnapshotFailure blk =
    -- | We failed to deserialise the snapshot
    --
    -- This can happen due to data corruption in the ledger DB.
    InitFailureRead ReadIncrementalErr

    -- | This snapshot is too recent (ahead of the tip of the chain)
  | InitFailureTooRecent (RealPoint blk)

    -- | This snapshot was of the ledger state at genesis, even though we never
    -- take snapshots at genesis, so this is unexpected.
  | InitFailureGenesis
  deriving (Show, Eq, Generic)

-- | Named snapshot are permanent, they will never be deleted when trimming.
diskSnapshotIsPermanent :: DiskSnapshot -> Bool
diskSnapshotIsPermanent = isJust . dsSuffix

-- | The snapshots that are periodically created are temporary, they will be
-- deleted when trimming
diskSnapshotIsTemporary :: DiskSnapshot -> Bool
diskSnapshotIsTemporary = not . diskSnapshotIsPermanent

-- | Read snapshot from disk
readSnapshot ::
     forall m blk. IOLike m
  => SomeHasFS m
  -> (forall s. Decoder s (ExtLedgerState blk EmptyMK))
  -> (forall s. Decoder s (HeaderHash blk))
  -> DiskSnapshot
  -> ExceptT ReadIncrementalErr m (ExtLedgerState blk EmptyMK)
readSnapshot hasFS decLedger decHash = do
      ExceptT
    . readIncremental hasFS decoder
    . snapshotToStatePath
  where
    decoder :: Decoder s (ExtLedgerState blk EmptyMK)
    decoder = decodeSnapshotBackwardsCompatible (Proxy @blk) decLedger decHash

-- | List on-disk snapshots, highest number first.
listSnapshots :: Monad m => SomeHasFS m -> m [DiskSnapshot]
listSnapshots (SomeHasFS HasFS{..}) =
    aux <$> listDirectory (mkFsPath [])
  where
    aux :: Set String -> [DiskSnapshot]
    aux = List.sortOn (Down . dsNumber) . mapMaybe snapshotFromPath . Set.toList

{-------------------------------------------------------------------------------
  Write to disk
-------------------------------------------------------------------------------}

-- | Take a snapshot of the /oldest ledger state/ in the ledger DB
--
-- We write the /oldest/ ledger state to disk because the intention is to only
-- write ledger states to disk that we know to be immutable. Primarily for
-- testing purposes, 'takeSnapshot' returns the block reference corresponding
-- to the snapshot that we wrote.
--
-- If a snapshot with the same number already exists on disk or if the tip is at
-- genesis, no snapshot is taken.
--
-- Note that an EBB can have the same slot number and thus snapshot number as
-- the block after it. This doesn't matter. The one block difference in the
-- ledger state doesn't warrant an additional snapshot. The number in the name
-- of the snapshot is only indicative, we don't rely on it being correct.
--
-- NOTE: This is a lower-level API that takes a snapshot independent from
-- whether this snapshot corresponds to a state that is more than @k@ back.
--
-- TODO: Should we delete the file if an error occurs during writing?
takeSnapshot ::
     forall m blk. (MonadThrow m, IsLedger (LedgerState blk))
  => Tracer m (TraceSnapshotEvent blk)
  -> SomeHasFS m
  -> BackingStore.LedgerBackingStore' m blk
  -> (ExtLedgerState blk EmptyMK -> Encoding)
  -> ExtLedgerState blk EmptyMK
  -> m (Maybe (DiskSnapshot, RealPoint blk))
takeSnapshot tracer hasFS backingStore encLedger oldest =
    case pointToWithOriginRealPoint (castPoint (getTip oldest)) of
      Origin ->
        return Nothing
      NotOrigin tip -> do
        let number   = unSlotNo (realPointSlot tip)
            snapshot = DiskSnapshot number Nothing
        snapshots <- listSnapshots hasFS
        if List.any ((== number) . dsNumber) snapshots then
          return Nothing
        else do
          writeSnapshot hasFS backingStore encLedger snapshot oldest
          traceWith tracer $ TookSnapshot snapshot tip
          return $ Just (snapshot, tip)

-- | Trim the number of on disk snapshots so that at most 'onDiskNumSnapshots'
-- snapshots are stored on disk. The oldest snapshots are deleted.
--
-- The deleted snapshots are returned.
trimSnapshots ::
     Monad m
  => Tracer m (TraceSnapshotEvent r)
  -> SomeHasFS m
  -> DiskPolicy
  -> m [DiskSnapshot]
trimSnapshots tracer hasFS DiskPolicy{..} = do
    -- We only trim temporary snapshots
    snapshots <- filter diskSnapshotIsTemporary <$> listSnapshots hasFS
    -- The snapshot are most recent first, so we can simply drop from the
    -- front to get the snapshots that are "too" old.
    forM (drop (fromIntegral onDiskNumSnapshots) snapshots) $ \snapshot -> do
      deleteSnapshot hasFS snapshot
      traceWith tracer $ DeletedSnapshot snapshot
      return snapshot

-- | Write snapshot to disk
writeSnapshot ::
     forall m blk. MonadThrow m
  => SomeHasFS m
  -> BackingStore.LedgerBackingStore' m blk
  -> (ExtLedgerState blk EmptyMK -> Encoding)
  -> DiskSnapshot
  -> ExtLedgerState blk EmptyMK
  -> m ()
writeSnapshot (SomeHasFS hasFS) backingStore encLedger snapshot cs = do
    createDirectory hasFS (snapshotToDirPath snapshot)
    withFile hasFS (snapshotToStatePath snapshot) (WriteMode MustBeNew) $ \h ->
      void $ hPut hasFS h $ CBOR.toBuilder (encode cs)
    BackingStore.bsCopy
      (let BackingStore.LedgerBackingStore store = backingStore in store)
      (SomeHasFS hasFS)
      (BackingStore.BackingStorePath (snapshotToTablesPath snapshot))
  where
    encode :: ExtLedgerState blk EmptyMK -> Encoding
    encode = encodeSnapshot encLedger

{-------------------------------------------------------------------------------
  Delete
-------------------------------------------------------------------------------}

-- | Delete snapshot from disk
deleteSnapshot :: HasCallStack => SomeHasFS m -> DiskSnapshot -> m ()
deleteSnapshot (SomeHasFS HasFS{..}) = removeDirectoryRecursive . snapshotToDirPath

{-------------------------------------------------------------------------------
  Paths
-------------------------------------------------------------------------------}

snapshotToDirName :: DiskSnapshot -> String
snapshotToDirName DiskSnapshot { dsNumber, dsSuffix } =
    show dsNumber <> suffix
  where
    suffix = case dsSuffix of
      Nothing -> ""
      Just s  -> "_" <> s

-- | The path within the LgrDB's filesystem to the snapshot's directory
snapshotToDirPath :: DiskSnapshot -> FsPath
snapshotToDirPath = mkFsPath . (:[]) . snapshotToDirName

-- | The path within the LgrDB's filesystem to the file that contains the
-- snapshot's serialized ledger state
snapshotToStatePath :: DiskSnapshot -> FsPath
snapshotToStatePath = mkFsPath . (\x -> [x, "state"]) . snapshotToDirName

-- | The path within the LgrDB's filesystem to the directory that contains a
-- snapshot's backing store
snapshotToTablesPath :: DiskSnapshot -> FsPath
snapshotToTablesPath = mkFsPath . (\x -> [x, "tables"]) . snapshotToDirName

-- | The path within the LgrDB's filesystem to the directory that contains the
-- backing store
_tablesPath :: FsPath
_tablesPath = mkFsPath ["tables"]

snapshotFromPath :: String -> Maybe DiskSnapshot
snapshotFromPath fileName = do
    number <- readMaybe prefix
    return $ DiskSnapshot number suffix'
  where
    (prefix, suffix) = break (== '_') fileName

    suffix' :: Maybe String
    suffix' = case suffix of
      ""      -> Nothing
      _ : str -> Just str

{-------------------------------------------------------------------------------
  Tracing
-------------------------------------------------------------------------------}

data TraceSnapshotEvent blk
  = InvalidSnapshot DiskSnapshot (SnapshotFailure blk)
    -- ^ An on disk snapshot was skipped because it was invalid.
  | TookSnapshot DiskSnapshot (RealPoint blk)
    -- ^ A snapshot was written to disk.
  | DeletedSnapshot DiskSnapshot
    -- ^ An old or invalid on-disk snapshot was deleted
  deriving (Generic, Eq, Show)

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

-- | Version 1: uses versioning ('Ouroboros.Consensus.Util.Versioned') and only
-- encodes the ledger state @l@.
snapshotEncodingVersion1 :: VersionNumber
snapshotEncodingVersion1 = 1

-- | Encoder to be used in combination with 'decodeSnapshotBackwardsCompatible'.
encodeSnapshot :: (l -> Encoding) -> l -> Encoding
encodeSnapshot encodeLedger l =
    encodeVersion snapshotEncodingVersion1 (encodeLedger l)

-- | To remain backwards compatible with existing snapshots stored on disk, we
-- must accept the old format as well as the new format.
--
-- The old format:
-- * The tip: @WithOrigin (RealPoint blk)@
-- * The chain length: @Word64@
-- * The ledger state: @l@
--
-- The new format is described by 'snapshotEncodingVersion1'.
--
-- This decoder will accept and ignore them. The encoder ('encodeSnapshot') will
-- no longer encode them.
decodeSnapshotBackwardsCompatible ::
     forall l blk.
     Proxy blk
  -> (forall s. Decoder s l)
  -> (forall s. Decoder s (HeaderHash blk))
  -> forall s. Decoder s l
decodeSnapshotBackwardsCompatible _ decodeLedger decodeHash =
    decodeVersionWithHook
      decodeOldFormat
      [(snapshotEncodingVersion1, Decode decodeVersion1)]
  where
    decodeVersion1 :: forall s. Decoder s l
    decodeVersion1 = decodeLedger

    decodeOldFormat :: Maybe Int -> forall s. Decoder s l
    decodeOldFormat (Just 3) = do
        _ <- withOriginRealPointToPoint <$>
               decodeWithOrigin (decodeRealPoint @blk decodeHash)
        _ <- Dec.decodeWord64
        decodeLedger
    decodeOldFormat mbListLen =
        fail $
          "decodeSnapshotBackwardsCompatible: invalid start " <>
          show mbListLen

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
    -- * Low-level API (primarily exposed for testing)
  , decodeSnapshotBackwardsCompatible
  , deleteSnapshot
  , encodeSnapshot
  , snapshotToFileName
  , snapshotToPath
    -- * Trace
  , TraceSnapshotEvent (..)
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
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Storage.LedgerDB.DiskPolicy
import           Ouroboros.Consensus.Util.CBOR (ReadIncrementalErr,
                     decodeWithOrigin, readIncremental)
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.Versioned
import           System.FS.API
import           System.FS.API.Types
import           Text.Read (readMaybe)

{-------------------------------------------------------------------------------
  Write to disk
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

data TraceSnapshotEvent blk
  = InvalidSnapshot DiskSnapshot (SnapshotFailure blk)
    -- ^ An on disk snapshot was skipped because it was invalid.
  | TookSnapshot DiskSnapshot (RealPoint blk)
    -- ^ A snapshot was written to disk.
  | DeletedSnapshot DiskSnapshot
    -- ^ An old or invalid on-disk snapshot was deleted
  deriving (Generic, Eq, Show)

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
  -> (ExtLedgerState blk -> Encoding)
  -> ExtLedgerState blk -> m (Maybe (DiskSnapshot, RealPoint blk))
takeSnapshot tracer hasFS encLedger oldest =
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
          writeSnapshot hasFS encLedger snapshot oldest
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

{-------------------------------------------------------------------------------
  Internal: reading from disk
-------------------------------------------------------------------------------}

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
  -> (forall s. Decoder s (ExtLedgerState blk))
  -> (forall s. Decoder s (HeaderHash blk))
  -> DiskSnapshot
  -> ExceptT ReadIncrementalErr m (ExtLedgerState blk)
readSnapshot hasFS decLedger decHash =
      ExceptT
    . readIncremental hasFS decoder
    . snapshotToPath
  where
    decoder :: Decoder s (ExtLedgerState blk)
    decoder = decodeSnapshotBackwardsCompatible (Proxy @blk) decLedger decHash

-- | Write snapshot to disk
writeSnapshot ::
     forall m blk. MonadThrow m
  => SomeHasFS m
  -> (ExtLedgerState blk -> Encoding)
  -> DiskSnapshot
  -> ExtLedgerState blk -> m ()
writeSnapshot (SomeHasFS hasFS) encLedger ss cs = do
    withFile hasFS (snapshotToPath ss) (WriteMode MustBeNew) $ \h ->
      void $ hPut hasFS h $ CBOR.toBuilder (encode cs)
  where
    encode :: ExtLedgerState blk -> Encoding
    encode = encodeSnapshot encLedger

-- | Delete snapshot from disk
deleteSnapshot :: HasCallStack => SomeHasFS m -> DiskSnapshot -> m ()
deleteSnapshot (SomeHasFS HasFS{..}) = removeFile . snapshotToPath

-- | List on-disk snapshots, highest number first.
listSnapshots :: Monad m => SomeHasFS m -> m [DiskSnapshot]
listSnapshots (SomeHasFS HasFS{..}) =
    aux <$> listDirectory (mkFsPath [])
  where
    aux :: Set String -> [DiskSnapshot]
    aux = List.sortOn (Down . dsNumber) . mapMaybe snapshotFromPath . Set.toList

snapshotToFileName :: DiskSnapshot -> String
snapshotToFileName DiskSnapshot { dsNumber, dsSuffix } =
    show dsNumber <> suffix
  where
    suffix = case dsSuffix of
      Nothing -> ""
      Just s  -> "_" <> s

snapshotToPath :: DiskSnapshot -> FsPath
snapshotToPath = mkFsPath . (:[]) . snapshotToFileName

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

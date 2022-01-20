{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ouroboros.Consensus.Storage.LedgerDB.OnDisk (
    -- * Opening the database
    InitFailure (..)
  , InitLog (..)
  , initLedgerDB
    -- ** Instantiate in-memory to @blk@
  , AnnLedgerError'
  , LedgerDB'
    -- ** Abstraction over the stream API
  , NextBlock (..)
  , StreamAPI (..)
    -- * Abstraction over the ledger-state HD interface
  , OnDiskLedgerStDb (..)
  , mkOnDiskLedgerStDb
    -- * FS wrappers
  , OldLedgerFS (..)
  , NewLedgerFS (..)
    -- * Read from disk
  , readSnapshot
    -- * Write to disk
  , takeSnapshot
  , trimOldSnapshots
  , trimNewSnapshots
  , writeSnapshot
    -- * Low-level API (primarily exposed for testing)
  , deleteSnapshot
  , snapshotToFileName
  , snapshotToPath
    -- ** opaque
  , DiskSnapshot (..)
    -- * Trace events
  , ReplayGoal (..)
  , ReplayStart (..)
  , TraceEvent (..)
  , TraceReplayEvent (..)
  , decorateReplayTracerWithGoal
  ) where

import qualified Codec.CBOR.Write as CBOR
import           Codec.Serialise.Decoding (Decoder, decodeWord64)
import           Codec.Serialise.Encoding (Encoding)
import           Control.Monad.Except
import           Control.Tracer
import           Data.Bifunctor (bimap)
import qualified Data.List as List
import           Data.Maybe (isJust, mapMaybe)
import           Data.Ord (Down (..))
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word
import           GHC.Generics (Generic)
import           GHC.Stack
import           Text.Read (readMaybe)

import           Ouroboros.Network.Block (Point (Point))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Inspect
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Util.CBOR (ReadIncrementalErr,
                     readIncremental, decodeWithOrigin)
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.Versioned

import           Ouroboros.Consensus.Storage.FS.API
import           Ouroboros.Consensus.Storage.FS.API.Types

import           Ouroboros.Consensus.Storage.LedgerDB.DiskPolicy
import           Ouroboros.Consensus.Storage.LedgerDB.InMemory
import           Ouroboros.Consensus.Storage.LedgerDB.UTxOHD

{-------------------------------------------------------------------------------
  Instantiate the in-memory DB to @blk@
-------------------------------------------------------------------------------}

type LedgerDB'       blk = LedgerDB       (ExtLedgerState blk)
type AnnLedgerError' blk = AnnLedgerError (ExtLedgerState blk) blk

{-------------------------------------------------------------------------------
  Newtype wrappers for not mixing up old and new filesystems
-------------------------------------------------------------------------------}

newtype OldLedgerFS m = OldLedgerFS (SomeHasFS m) deriving newtype (NoThunks)
newtype NewLedgerFS m = NewLedgerFS (SomeHasFS m) deriving newtype (NoThunks)

{-------------------------------------------------------------------------------
  Abstraction over the streaming API provided by the Chain DB
-------------------------------------------------------------------------------}

-- | Next block returned during streaming
data NextBlock blk = NoMoreBlocks | NextBlock blk

-- | Stream blocks from the immutable DB
--
-- When we initialize the ledger DB, we try to find a snapshot close to the
-- tip of the immutable DB, and then stream blocks from the immutable DB to its
-- tip to bring the ledger up to date with the tip of the immutable DB.
--
-- In CPS form to enable the use of 'withXYZ' style iterator init functions.
data StreamAPI m blk = StreamAPI {
  -- | Start streaming after the specified block up to the end of the
  -- immutable database.
  streamAfter :: forall a. HasCallStack
              => Point blk
              -- Reference to the block corresponding to the snapshot we found
              -- (or 'Point Origin' if we didn't find any)

              -> (Either (RealPoint blk) (m (NextBlock blk)) -> m a)
              -- Get the next block (by value)
              --
              -- Should be @Left pt@ if the snapshot we found is more recent
              -- than the tip of the immutable DB. Since we only store snapshots
              -- to disk for blocks in the immutable DB, this can only happen if
              -- the immutable DB got truncated due to disk corruption. The
              -- returned @pt@ is a 'RealPoint', not a 'Point', since it must
              -- always be possible to stream after genesis.
              -> m a

  , -- | Start streaming after the given block up to the second given block.
    streamAfterUpTo :: forall a. HasCallStack
                    => Point blk
                    -- Reference to the block corresponding to the snapshot we found (or
                    -- 'Point Origin' if we didn't find any)
                    -> Point blk
                    -- Reference to the block we want to reach.
                    -> (Either (RealPoint blk) (m (NextBlock blk)) -> m a)
                    -- Get the next block (by value)
                    --
                    -- Should be @Left pt@ if the snapshot we found is more
                    -- recent than the tip of the immutable DB. Since we only
                    -- store snapshots to disk for blocks in the immutable DB,
                    -- this can only happen if the immutable DB got truncated
                    -- due to disk corruption. The returned @pt@ is a
                    -- 'RealPoint', not a 'Point', since it must always be
                    -- possible to stream after genesis.
                    -> m a
  }

-- | Stream all blocks
streamAll ::
     forall m blk e a. (Monad m, HasCallStack)
  => StreamAPI m blk
  -> Point blk             -- ^ Starting point for streaming
  -> (RealPoint blk -> e)  -- ^ Error when tip not found
  -> a                     -- ^ Starting point when tip /is/ found
  -> (blk -> a -> m a)     -- ^ Update function for each block
  -> ExceptT e m a
streamAll StreamAPI{..} tip notFound e f = ExceptT $
    streamAfter tip $ \case
      Left tip' -> return $ Left (notFound tip')

      Right getNext -> do
        let go :: a -> m a
            go a = do mNext <- getNext
                      case mNext of
                        NoMoreBlocks -> return a
                        NextBlock b  -> go =<< f b a
        Right <$> go e

-- | Stream all blocks up to the specified block
streamUpTo ::
     forall m blk e a. (Monad m, HasCallStack)
  => StreamAPI m blk
  -> Point blk             -- ^ Starting point for streaming
  -> Point blk             -- ^ Ending point for streaming
  -> (RealPoint blk -> e)  -- ^ Error when tip not found
  -> a                     -- ^ Starting point when tip /is/ found
  -> (blk -> a -> m a)     -- ^ Update function for each block
  -> ExceptT e m a
streamUpTo StreamAPI{..} tip goal notFound e f = ExceptT $
    streamAfterUpTo tip goal $ \case
      Left tip' -> return $ Left (notFound tip')

      Right getNext -> do
        let go :: a -> m a
            go a = do mNext <- getNext
                      case mNext of
                        NoMoreBlocks -> return a
                        NextBlock b  -> go =<< f b a
        Right <$> go e

{-------------------------------------------------------------------------------
  Initialize the DB
-------------------------------------------------------------------------------}

-- | Initialization log
--
-- The initialization log records which snapshots from disk were considered,
-- in which order, and why some snapshots were rejected. It is primarily useful
-- for monitoring purposes.
data InitLog blk =
    -- | Defaulted to initialization from genesis
    --
    -- NOTE: Unless the blockchain is near genesis, we should see this /only/
    -- if data corrupted occurred.
    InitFromGenesis

    -- | Used a snapshot corresponding to the specified tip
  | InitFromSnapshot DiskSnapshot (RealPoint blk)

    -- | Initialization skipped a snapshot
    --
    -- We record the reason why it was skipped.
    --
    -- NOTE: We should /only/ see this if data corrupted occurred.
  | InitFailure DiskSnapshot (InitFailure blk) (InitLog blk)
  deriving (Show, Eq, Generic)

-- | Initialize the ledger DB
--
-- This will:
-- 1. deserialize the most recent found snapshot for each of the internal databases
-- 2. bring up the database that is behind until they are at the same tip
-- 3. replay the remaining blocks on the immutable database on both databases
initLedgerDB ::
     ( IOLike m
     , LedgerSupportsProtocol blk
     , InspectLedger blk
     , HasCallStack
     )
  => Tracer m (ReplayGoal blk -> TraceReplayEvent blk)   -- ^ A replay tracer
  -> Tracer m (TraceEvent blk)                           -- ^ A general tracer
  -> OldLedgerFS m                                       -- ^ The filesystem with the old-style snapshots
  -> (forall s. Decoder s (ExtLedgerState blk ValuesMK)) -- ^ A decoder for the old-style snapshots
  -> NewLedgerFS m                                       -- ^ The filesystem with the new-style snapshots
  -> OnDiskLedgerStDb m (ExtLedgerState blk) blk         -- ^ The backend handle
  -> (forall s. Decoder s (ExtLedgerState blk EmptyMK))  -- ^ A decoder for the new-style snapshots
  -> (forall s. Decoder s (HeaderHash blk))              -- ^ A decoder for header hashes
  -> LedgerDbCfg (ExtLedgerState blk)                    -- ^ The ledger database configuration
  -> m (ExtLedgerState blk ValuesMK)                     -- ^ Genesis ledger state
  -> StreamAPI m blk                                     -- ^ An streaming API for the immutable database
  -> Bool                                                -- ^ Whether to run both databases TODO: This is not yet implemented
  -> m ((InitLog blk, InitLog blk), LedgerDB' blk, (Word64, Word64))
initLedgerDB replayTracer
             tracer
             hasFS
             decLedger
             newHasFS
             onDiskLedgerDbSt
             decNewLedger
             decHash
             cfg
             getGenesisLedger
             streamAPI
             runDual = do
    (initLog, ledgerDb, tipPoint)                    <- oldInitLedgerDB
                                                          getGenesisLedger
                                                          hasFS
                                                          decLedger
                                                          decHash
    (initLog', ledgerDb', tipPoint', replayTracer') <- newInitLedgerDB
                                                          replayTracer
                                                          tracer
                                                          getGenesisLedger
                                                          newHasFS
                                                          decNewLedger
                                                          decHash
                                                          onDiskLedgerDbSt

    (lgrDB, w) <- case compare tipPoint tipPoint' of
        LT -> bringUpOldLedgerDB
                runDual
                cfg
                ledgerDb'
                (streamUpTo streamAPI tipPoint tipPoint' InitFailureTooRecent (ledgerDb, 0))
        GT -> bringUpNewLedgerDB
                runDual
                (\b e -> traceWith replayTracer' (ReplayedBlock b e))
                cfg
                getExtLedgerCfg
                ledgerState
                onDiskLedgerDbSt
                ledgerDb
                (streamUpTo streamAPI tipPoint' tipPoint InitFailureTooRecent (ledgerDb', 0))
        EQ -> return $ combineLedgerDBs
                runDual
                ledgerDb
                ledgerDb'

    ml <- runExceptT $ replayStartingWith replayTracer' cfg onDiskLedgerDbSt streamAPI lgrDB
    case ml of
      Left err -> error $  "invariant violation: invalid current chain:" <> show err
      Right (ledgerDB, w64) -> return ((initLog, initLog'), ledgerDB, bimap (+ w64) (+ w64) w)

{-------------------------------------------------------------------------------
 Load snapshots from the disk
-------------------------------------------------------------------------------}

-- | Load a snapshot from the disk. Depending on the decoder, the snapshot is
-- expected to be of the @mk@ that the decoder knows how to decode.
--
-- This will throw an exception @InitFailureRead@ if it can't deserialize a
-- snapshot, and an @InitFailureGenesis@ if we are trying to load a snapshot
-- that corresponds to Genesis.
loadSnapshot ::
  ( IOLike m
  , LedgerSupportsProtocol blk
  )
  => Maybe (Tracer m (ReplayGoal blk -> TraceReplayEvent blk)) -- ^ The replay tracer to be annotated with starting point.
  -> SomeHasFS m                                               -- ^ The filesystem with the snapshots
  -> (forall s. Decoder s (ExtLedgerState blk mk))             -- ^ A decoder for the snapshots
  -> (forall s. Decoder s (HeaderHash blk))                    -- ^ A decoder for header hashes
  -> DiskSnapshot                                              -- ^ Which snapshot to load on the filesystem
  -> ExceptT (InitFailure blk)
       m
       ( ExtLedgerState blk mk                     -- The resulting ledger state
       , RealPoint blk                             -- The real point corresponding to the ledger state
       , Maybe (Tracer m (  ReplayStart blk
                         -> ReplayGoal blk
                         -> TraceReplayEvent blk)) -- The annotated tracer
       )
loadSnapshot tracer fs dec decHash ss  = do
    initSS <- withExceptT InitFailureRead $ readSnapshot fs dec decHash ss
    let initialPoint = castPoint (getTip initSS)
    case pointToWithOriginRealPoint initialPoint of
      Origin -> throwError InitFailureGenesis
      NotOrigin tip -> do
        maybe (return ()) (\t -> lift $ traceWith t $ ReplayFromSnapshot ss tip (ReplayStart initialPoint)) tracer
        return (initSS, tip, decorateReplayTracerWithStart initialPoint <$> tracer)

{-------------------------------------------------------------------------------
 Old initialization of LedgerDB
-------------------------------------------------------------------------------}

-- | Initialize the ledger DB in the old way from the most recent snapshot on disk
--
-- If no such snapshot can be found, use the genesis ledger DB. Returns the
-- initialized old DB as well as the block reference corresponding to the snapshot
-- we found on disk (the latter primarily for testing/monitoring purposes).
--
-- We only discard snapshots if we cannot deserialise them.
--
-- It is possible that the Ledger DB will not be able to roll back @k@ blocks
-- after initialization if the chain has been truncated (data corruption).
--
-- We do /not/ attempt to use multiple ledger states from disk to construct the
-- ledger DB. Instead we load only a /single/ ledger state from disk, and
-- /compute/ all subsequent ones. This is important, because the ledger states
-- obtained in this way will (hopefully) share much of their memory footprint
-- with their predecessors.
oldInitLedgerDB ::
  ( IOLike m
  , LedgerSupportsProtocol blk
  , HasCallStack
  )
  => m (ExtLedgerState blk ValuesMK)                     -- ^ Action that gives the Genesis ledger state
  -> OldLedgerFS m                                       -- ^ Filesystem containing the old ledger snapshots
  -> (forall s. Decoder s (ExtLedgerState blk ValuesMK)) -- ^ A decoder for the old ledger snapshots
  -> (forall s. Decoder s (HeaderHash blk))              -- ^ A decoder for header hashes
  -> m ( InitLog blk                      -- The initialization log
       , OldLedgerDB (ExtLedgerState blk) -- The resulting old ledgerDB
       , Point blk                        -- The point corresponding to the adopted ledger state
       )
oldInitLedgerDB getGenesisLedger (OldLedgerFS hasFS) decLedger decHash = do
    listSnapshots hasFS >>= tryNewestFirst id
  where
    tryNewestFirst acc [] = do
      -- We're out of snapshots. Start at genesis
      initSS <- getGenesisLedger
      return ( acc InitFromGenesis
             , oldLedgerDbWithAnchor initSS
             , Point Origin
             )
    tryNewestFirst acc (s:ss) = do
      ml <- runExceptT $ loadSnapshot Nothing hasFS decLedger decHash s
      case ml of
        Left err -> do
          when (diskSnapshotIsTemporary s) $
            -- We don't delete permanent snapshots, even if we couldn't parse
            -- them
            deleteSnapshot hasFS s
          tryNewestFirst (acc . InitFailure s err) ss
        Right (ls, pt, _) -> do
          return ( acc (InitFromSnapshot s pt)
                 , oldLedgerDbWithAnchor ls
                 , realPointToPoint pt
                 )

{-------------------------------------------------------------------------------
 New initialization of LedgerDB
-------------------------------------------------------------------------------}

-- | Initialize a DbChangelog from the snapshot in the disk.
--
-- If there are multiple snapshots we can only be sure that the latest one
-- corresponds to what was flushed to the disk and therefore we can only try to
-- deserialize that one. If the point on said snapshot and the point on the disk
-- data is not the same one, this will throw an assertion failure.
--
-- If we fail to deserialize the last snapshot, we cannot try previous ones
-- because they will be misaligned with the on disk data, so we can just revert
-- to Genesis.
newInitLedgerDB ::
  ( IOLike m
  , LedgerSupportsProtocol blk
  , HasCallStack
  )
  => Tracer m (ReplayGoal blk -> TraceReplayEvent blk)  -- ^ A tracer for the replay events
  -> Tracer m (TraceEvent blk)                          -- ^ A tracer for general events
  -> m (ExtLedgerState blk ValuesMK)                    -- ^ An action to get the Genesis ledger state
  -> NewLedgerFS m                                      -- ^ The filesystem with the new snapshots
  -> (forall s. Decoder s (ExtLedgerState blk EmptyMK)) -- ^ A decoder for new snapshots
  -> (forall s. Decoder s (HeaderHash blk))             -- ^ A decoder for header hashes
  -> OnDiskLedgerStDb m (ExtLedgerState blk) blk
  -> m ( InitLog blk                       -- The initialization log
       , NewLedgerDB (ExtLedgerState blk)  -- The new ledger database
       , Point blk                         -- The point corresponding to the adopted ledger state
       , Tracer m (  ReplayStart blk
                  -> ReplayGoal blk
                  -> TraceReplayEvent blk) -- An annotated tracer for replay events
       )
newInitLedgerDB replayTracer tracer getGenesisLedger (NewLedgerFS hasFS) decLedger decHash onDiskLedgerDbSt = do
    snapshots <- listSnapshots hasFS
    case snapshots of
      []  -> initUsingGenesis
      s:_ -> do
        ml <- runExceptT $ loadSnapshot (Just replayTracer) hasFS decLedger decHash s
        case ml of
          Left err -> do
            -- The snapshot failed to deserialize, delete it and start from genesis
            deleteSnapshot hasFS s
            traceWith tracer $ InvalidSnapshot s err
            (l, db, pt, tr) <- initUsingGenesis
            return (InitFailure s err l, db, pt, tr)
          Right (initSS, pt', replayTracer') -> do
            -- The snapshot succeeded in deserialization
            pt'' <- odlsGetPt onDiskLedgerDbSt
            if realPointToPoint pt' == pt''
              then
                -- The snapshot is in sync with the UTxO backend
                return ( InitFromSnapshot s pt'
                       , initialDbChangelogWithEmptyState (getTipSlot initSS) initSS
                       , realPointToPoint pt'
                       , maybe (error "unreachable as we provided a Just to loadSnapshot") id replayTracer'
                       )
              else do
                deleteSnapshot hasFS s
                traceWith tracer $ InvalidSnapshot s InitFailureNotInSyncWithUTxO
                (l, db, pt, tr) <- initUsingGenesis
                return (InitFailure s InitFailureNotInSyncWithUTxO l, db, pt, tr)
  where
    initUsingGenesis = do
      initSS <- getGenesisLedger
      -- No matter if this was called because there are no snapshots, the
      -- snapshot failed to deserialize or it was out of sync with the on-disk
      -- UTxO, we have to initialize the on-disk UTxO back to Genesis.
      writeGenesisUTxO onDiskLedgerDbSt initSS
      let replayTracer' = decorateReplayTracerWithStart (Point Origin) replayTracer
      return ( InitFromGenesis
             , initialDbChangelog (getTipSlot initSS) initSS
             , Point Origin
             , replayTracer'
             )

{-------------------------------------------------------------------------------
  Internal: initialize using the given snapshot
-------------------------------------------------------------------------------}

data InitFailure blk =
    -- | We failed to deserialise the snapshot
    --
    -- This can happen due to data corruption in the ledger DB.
    InitFailureRead ReadIncrementalErr

    -- | This snapshot is too recent (ahead of the tip of the chain)
  | InitFailureTooRecent (RealPoint blk)

    -- | This snapshot was of the ledger state at genesis, even though we never
    -- take snapshots at genesis, so this is unexpected.
  | InitFailureGenesis

    -- | The new-style snapshot is not in sync with the on-disk UTxO
  | InitFailureNotInSyncWithUTxO
  deriving (Show, Eq, Generic)

-- | Attempt to initialize the ledger DB starting from the given ledger DB by
-- streaming blocks from the immutable database up to the immutable database
-- tip.
replayStartingWith :: forall m blk.
  ( Monad m
  , LedgerSupportsProtocol blk
  , InspectLedger blk
  , HasCallStack
  )
  => Tracer m (  ReplayStart blk
              -> ReplayGoal blk
              -> TraceReplayEvent blk)           -- ^ A replay tracer
  -> LedgerDbCfg (ExtLedgerState blk)            -- ^ The LedgerDB configuration
  -> OnDiskLedgerStDb m (ExtLedgerState blk) blk -- ^ The backend handle
  -> StreamAPI m blk                             -- ^ An API to stream blocks from the Immutable DB
  -> LedgerDB' blk                               -- ^ The LedgerDB we want to push blocks into
  -> ExceptT (InitFailure blk) m (LedgerDB' blk, Word64)
replayStartingWith tracer cfg onDiskLedgerDbSt streamAPI initDb = do
    streamAll streamAPI (castPoint (ledgerDbTip initDb))
      InitFailureTooRecent
      (initDb, 0)
      push
  where
    push :: blk -> (LedgerDB' blk, Word64) -> m (LedgerDB' blk, Word64)
    push blk !(!db, !replayed) = do
        !db' <- defaultReadKeySets (readKeySets onDiskLedgerDbSt) $
                  ledgerDbPush cfg (ReapplyVal blk) db

        db'' <- if replayed `mod` 2160 == 0
                then ledgerDbFlush onDiskLedgerDbSt undefined db'
                else return db'

        let !replayed' = replayed + 1

            events = inspectLedger
                       (getExtLedgerCfg (ledgerDbCfg cfg))
                       (ledgerState (ledgerDbCurrent db))
                       (ledgerState (ledgerDbCurrent db''))

        traceWith tracer (ReplayedBlock (blockRealPoint blk) events)
        return (db'', replayed')

{-------------------------------------------------------------------------------
  DiskSnapshots
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
  Snapshots: serialisation
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
        _ <- decodeWord64
        decodeLedger
    decodeOldFormat mbListLen =
        fail $
          "decodeSnapshotBackwardsCompatible: invalid start " <>
          show mbListLen

{-------------------------------------------------------------------------------
  Internal: Write to disk
-------------------------------------------------------------------------------}

-- | Take a snapshot of the given Ledger state
--
-- We expect this to be the /oldest/ ledger state in the database because the
-- intention is to only write ledger states to disk that we know to be
-- immutable. Primarily for testing purposes, 'takeSnapshot' returns the block
-- reference corresponding to the snapshot that we wrote.
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
     (MonadThrow m, IsLedger (LedgerState blk))
  => Tracer m (TraceEvent blk)
  -> SomeHasFS m
  -> (ExtLedgerState blk ValuesMK -> Encoding)
  -> (ExtLedgerState blk EmptyMK -> Encoding)
  -> LedgerDB' blk
  -> Bool
  -> m (Maybe (DiskSnapshot, RealPoint blk))
takeSnapshot tracer hasFS oldEncLedger newEncLedger ledgerDb isNew =
    if isNew
    then takeSnapshot' newEncLedger . ledgerDbAnchor $ ledgerDb
    else takeSnapshot' oldEncLedger . oldLedgerDbAnchor $ ledgerDb
  where
    takeSnapshot' encLedger oldest =
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
trimOldSnapshots ::
     Monad m
  => Tracer m (TraceEvent r)
  -> OldLedgerFS m
  -> DiskPolicy
  -> m [DiskSnapshot]
trimOldSnapshots tracer (OldLedgerFS hasFS) DiskPolicy{..} = do
    -- We only trim temporary snapshots
    snapshots <- filter diskSnapshotIsTemporary <$> listSnapshots hasFS
    -- The snapshot are most recent first, so we can simply drop from the
    -- front to get the snapshots that are "too" old.
    forM (drop (fromIntegral onDiskNumOldSnapshots) snapshots) $ \snapshot -> do
      deleteSnapshot hasFS snapshot
      traceWith tracer $ DeletedSnapshot snapshot
      return snapshot

-- | Remove any snapshot in the disk that is not the one we just wrote as in the
-- new-style LedgerDB there can only be one snapshot which must be in sync with
-- the UTxO HD backend.
--
-- The deleted snapshots are returned.
trimNewSnapshots ::
     Monad m
  => Tracer m (TraceEvent r)
  -> NewLedgerFS m
  -> DiskSnapshot
  -> m [DiskSnapshot]
trimNewSnapshots tracer (NewLedgerFS newHasFS) s = do
    snapshots <- filter (not . (== s)) <$> listSnapshots newHasFS
    forM snapshots $ \snapshot -> do
      deleteSnapshot newHasFS snapshot
      traceWith tracer $ DeletedSnapshot snapshot
      return snapshot

-- | Write snapshot to disk
writeSnapshot ::
     forall m blk mk. MonadThrow m
  => SomeHasFS m
  -> (ExtLedgerState blk mk -> Encoding)
  -> DiskSnapshot
  -> ExtLedgerState blk mk -> m ()
writeSnapshot (SomeHasFS hasFS) encLedger ss cs = do
    withFile hasFS (snapshotToPath ss) (WriteMode MustBeNew) $ \h ->
      void $ hPut hasFS h $ CBOR.toBuilder (encode cs)
  where
    encode :: ExtLedgerState blk mk -> Encoding
    encode = encodeSnapshot encLedger

-- | Delete snapshot from disk
deleteSnapshot :: HasCallStack => SomeHasFS m -> DiskSnapshot -> m ()
deleteSnapshot (SomeHasFS HasFS{..}) = removeFile . snapshotToPath

{-------------------------------------------------------------------------------
  Internal: reading from disk
-------------------------------------------------------------------------------}

-- | Read snapshot from disk
readSnapshot ::
     forall m blk mk. IOLike m
  => SomeHasFS m
  -> (forall s. Decoder s (ExtLedgerState blk mk))
  -> (forall s. Decoder s (HeaderHash blk))
  -> DiskSnapshot
  -> ExceptT ReadIncrementalErr m (ExtLedgerState blk mk)
readSnapshot hasFS decLedger decHash =
      ExceptT
    . readIncremental hasFS decoder
    . snapshotToPath
  where
    decoder :: Decoder s (ExtLedgerState blk mk)
    decoder = decodeSnapshotBackwardsCompatible (Proxy @blk) decLedger decHash

-- | List on-disk snapshots, highest number first.
listSnapshots :: Monad m => SomeHasFS m -> m [DiskSnapshot]
listSnapshots (SomeHasFS HasFS{..}) =
    aux <$> listDirectory (mkFsPath [])
  where
    aux :: Set String -> [DiskSnapshot]
    aux = List.sortOn (Down . dsNumber) . mapMaybe snapshotFromPath . Set.toList

{-------------------------------------------------------------------------------
  Replay trace events
-------------------------------------------------------------------------------}

-- | Add the tip of the Immutable DB to the trace event
--
-- Between the tip of the immutable DB and the point of the starting block,
-- the node could (if it so desired) easily compute a "percentage complete".
decorateReplayTracerWithGoal
  :: Point blk -- ^ Tip of the ImmutableDB
  -> Tracer m (TraceReplayEvent blk)
  -> Tracer m (ReplayGoal blk -> TraceReplayEvent blk)
decorateReplayTracerWithGoal immTip = contramap ($ (ReplayGoal immTip))

-- | Add the block at which a replay started.
--
-- This allows to compute a "percentage complete" when tracing the events.
decorateReplayTracerWithStart
  :: Point blk -- ^ Starting point of the replay
  -> Tracer m (ReplayGoal blk -> TraceReplayEvent blk)
  -> Tracer m (ReplayStart blk -> ReplayGoal blk -> TraceReplayEvent blk)
decorateReplayTracerWithStart start = contramap ($ (ReplayStart start))

data TraceEvent blk
  = InvalidSnapshot DiskSnapshot (InitFailure blk)
    -- ^ An on disk snapshot was skipped because it was invalid.
  | TookSnapshot DiskSnapshot (RealPoint blk)
    -- ^ A snapshot was written to disk.
  | DeletedSnapshot DiskSnapshot
    -- ^ An old or invalid on-disk snapshot was deleted
  deriving (Generic, Eq, Show)

-- | Which point the replay started from
newtype ReplayStart blk = ReplayStart (Point blk) deriving (Eq, Show)

-- | Which point the replay is expected to end at
newtype ReplayGoal blk = ReplayGoal (Point blk) deriving (Eq, Show)

-- | Events traced while replaying blocks against the ledger to bring it up to
-- date w.r.t. the tip of the ImmutableDB during initialisation. As this
-- process takes a while, we trace events to inform higher layers of our
-- progress.
data TraceReplayEvent blk
  = -- | There were no LedgerDB snapshots on disk, so we're replaying all blocks
    -- starting from Genesis against the initial ledger.
    ReplayFromGenesis
        (ReplayGoal blk)  -- ^ the block at the tip of the ImmutableDB
    -- | There was a LedgerDB snapshot on disk corresponding to the given tip.
    -- We're replaying more recent blocks against it.
  | ReplayFromSnapshot
        DiskSnapshot
        (RealPoint blk)
        (ReplayStart blk) -- ^ the block at which this replay started
        (ReplayGoal blk)  -- ^ the block at the tip of the ImmutableDB
  -- | We replayed the given block (reference) on the genesis snapshot during
  -- the initialisation of the LedgerDB. Used during ImmutableDB replay.
  | ReplayedBlock
        (RealPoint blk)   -- ^ the block being replayed
        [LedgerEvent blk]
        (ReplayStart blk) -- ^ the block at which this replay started
        (ReplayGoal blk)  -- ^ the block at the tip of the ImmutableDB
  deriving (Generic, Eq, Show)

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}

module Ouroboros.Consensus.Storage.LedgerDB.OnDisk (
    -- * Opening the database
    initLedgerDB
  , InitLog(..)
  , InitFailure(..)
    -- ** Abstraction over the stream API
  , NextBlock(..)
  , StreamAPI(..)
    -- * Write to disk
  , takeSnapshot
  , trimSnapshots
  , mkDiskSnapshot
    -- * Low-level API (primarily exposed for testing)
  , DiskSnapshot -- opaque
  , deleteSnapshot
  , snapshotToPath
    -- * Trace events
  , TraceEvent(..)
  , TraceReplayEvent(..)
  ) where

import qualified Codec.CBOR.Write as CBOR
import           Codec.Serialise.Decoding (Decoder)
import           Codec.Serialise.Encoding (Encoding)
import           Control.Monad.Except
import           Control.Tracer
import qualified Data.Bifunctor.TH as TH
import qualified Data.List as List
import           Data.Maybe (isNothing, mapMaybe)
import           Data.Ord
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word
import           GHC.Generics (Generic)
import           GHC.Stack
import           Text.Read (readMaybe)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Util.CBOR (ReadIncrementalErr,
                     readIncremental)
import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Consensus.Storage.FS.API
import           Ouroboros.Consensus.Storage.FS.API.Types

import           Ouroboros.Consensus.Storage.LedgerDB.DiskPolicy
import           Ouroboros.Consensus.Storage.LedgerDB.InMemory

{-------------------------------------------------------------------------------
  Abstraction over the streaming API provided by the Chain DB
-------------------------------------------------------------------------------}

-- | Next block returned during streaming
data NextBlock r b = NoMoreBlocks | NextBlock (r, b)

-- | Stream blocks from the immutable DB
--
-- When we initialize the ledger DB, we try to find a snapshot close to the
-- tip of the immutable DB, and then stream blocks from the immutable DB to its
-- tip to bring the ledger up to date with the tip of the immutable DB.
--
-- In CPS form to enable the use of 'withXYZ' style iterator init functions.
data StreamAPI m r b = StreamAPI {
      -- | Start streaming after the specified block
      streamAfter :: forall a. HasCallStack
        => WithOrigin r
        -- Reference to the block corresponding to the snapshot we found
        -- (or 'TipGen' if we didn't find any)

        -> (Maybe (m (NextBlock r b)) -> m a)
        -- Get the next block (by value)
        --
        -- Should be 'Nothing' if the snapshot we found is more recent than
        -- the tip of the immutable DB; since we only store snapshots to disk
        -- for blocks in the immutable DB, this can only happen if the
        -- immutable DB got truncated due to disk corruption.
        -> m a
    }

-- | Stream all blocks
streamAll :: forall m r b e a. (Monad m, HasCallStack)
          => StreamAPI m r b
          -> WithOrigin r         -- ^ Starting point for streaming
          -> (WithOrigin r -> e)  -- ^ Error when tip not found
          -> a                    -- ^ Starting point when tip /is/ found
          -> ((r, b) -> a -> m a) -- ^ Update function for each block
          -> ExceptT e m a
streamAll StreamAPI{..} tip notFound e f = ExceptT $
    streamAfter tip $ \case
      Nothing      -> return $ Left (notFound tip)
      Just getNext -> do
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
data InitLog r =
    -- | Defaulted to initialization from genesis
    --
    -- NOTE: Unless the blockchain is near genesis, we should see this /only/
    -- if data corrupted occurred.
    InitFromGenesis

    -- | Used a snapshot corresponding to the specified tip
  | InitFromSnapshot DiskSnapshot (WithOrigin r)

    -- | Initialization skipped a snapshot
    --
    -- We record the reason why it was skipped.
    --
    -- NOTE: We should /only/ see this if data corrupted occurred.
  | InitFailure DiskSnapshot (InitFailure r) (InitLog r)
  deriving (Show, Eq, Generic)

-- | Initialize the ledger DB from the most recent snapshot on disk
--
-- If no such snapshot can be found, use the genesis ledger DB. Returns the
-- initialized DB as well as the block reference corresponding to the snapshot
-- we found on disk (the latter primarily for testing/monitoring purposes).
--
-- We do /not/ catch any exceptions thrown during streaming; should any be
-- thrown, it is the responsibility of the 'ChainDB' to catch these
-- and trigger (further) validation. We only discard snapshots if
--
-- * We cannot deserialise them, or
-- * they are /ahead/ of the chain
--
-- It is possible that the Ledger DB will not be able to roll back @k@ blocks
-- after initialization if the chain has been truncated (data corruption).

-- We do /not/ attempt to use multiple ledger states from disk to construct the
-- ledger DB. Instead we load only a /single/ ledger state from disk, and
-- /compute/ all subsequent ones. This is important, because the ledger states
-- obtained in this way will (hopefully) share much of their memory footprint
-- with their predecessors.
initLedgerDB :: forall m h l r b. (IOLike m, ApplyBlock l b, HasCallStack)
             => Tracer m (TraceReplayEvent r ())
             -> Tracer m (TraceEvent r)
             -> HasFS m h
             -> (forall s. Decoder s l)
             -> (forall s. Decoder s r)
             -> LedgerDbParams
             -> FullBlockConfig l b
             -> m l -- ^ Genesis ledger state
             -> StreamAPI m r b
             -> m (InitLog r, LedgerDB l r, Word64)
initLedgerDB replayTracer
             tracer
             hasFS
             decLedger
             decRef
             params
             conf
             getGenesisLedger
             streamAPI = do
    snapshots <- listSnapshots hasFS
    tryNewestFirst id snapshots
  where
    tryNewestFirst :: (InitLog r -> InitLog r)
                   -> [DiskSnapshot]
                   -> m (InitLog r, LedgerDB l r, Word64)
    tryNewestFirst acc [] = do
        -- We're out of snapshots. Start at genesis
        traceWith replayTracer $ ReplayFromGenesis ()
        initDb <- ledgerDbFromGenesis params <$> getGenesisLedger
        ml     <- runExceptT $ initStartingWith replayTracer conf streamAPI initDb
        case ml of
          Left _  -> error "invariant violation: invalid current chain"
          Right (l, replayed) -> return (acc InitFromGenesis, l, replayed)
    tryNewestFirst acc (s:ss) = do
        -- If we fail to use this snapshot, delete it and try an older one
        ml <- runExceptT $ initFromSnapshot
                             replayTracer
                             hasFS
                             decLedger
                             decRef
                             params
                             conf
                             streamAPI
                             s
        case ml of
          Left err -> do
            when (isNothing (dsSuffix s)) $
              -- We don't delete named snapshots, even if we couldn't parse them
              deleteSnapshot hasFS s
            traceWith tracer $ InvalidSnapshot s err
            tryNewestFirst (acc . InitFailure s err) ss
          Right (r, l, replayed) ->
            return (acc (InitFromSnapshot s r), l, replayed)

{-------------------------------------------------------------------------------
  Internal: initialize using the given snapshot
-------------------------------------------------------------------------------}

data InitFailure r =
    -- | We failed to deserialise the snapshot
    --
    -- This can happen due to data corruption in the ledger DB.
    InitFailureRead ReadIncrementalErr

    -- | This snapshot is too recent (ahead of the tip of the chain)
  | InitFailureTooRecent (WithOrigin r)
  deriving (Show, Eq, Generic)

-- | Attempt to initialize the ledger DB from the given snapshot
--
-- If the chain DB or ledger layer reports an error, the whole thing is aborted
-- and an error is returned. This should not throw any errors itself (ignoring
-- unexpected exceptions such as asynchronous exceptions, of course).
initFromSnapshot :: forall m h l r b. (IOLike m, ApplyBlock l b, HasCallStack)
                 => Tracer m (TraceReplayEvent r ())
                 -> HasFS m h
                 -> (forall s. Decoder s l)
                 -> (forall s. Decoder s r)
                 -> LedgerDbParams
                 -> FullBlockConfig l b
                 -> StreamAPI m r b
                 -> DiskSnapshot
                 -> ExceptT (InitFailure r) m (WithOrigin r, LedgerDB l r, Word64)
initFromSnapshot tracer hasFS decLedger decRef params conf streamAPI ss = do
    initSS <- withExceptT InitFailureRead $
                readSnapshot hasFS decLedger decRef ss
    lift $ traceWith tracer $ ReplayFromSnapshot ss (csTip initSS) ()
    (initDB, replayed) <- initStartingWith tracer conf streamAPI (ledgerDbWithAnchor params initSS)
    return (csTip initSS, initDB, replayed)

-- | Attempt to initialize the ledger DB starting from the given ledger DB
initStartingWith :: forall m l r b. (Monad m, ApplyBlock l b, HasCallStack)
                 => Tracer m (TraceReplayEvent r ())
                 -> FullBlockConfig l b
                 -> StreamAPI m r b
                 -> LedgerDB l r
                 -> ExceptT (InitFailure r) m (LedgerDB l r, Word64)
initStartingWith tracer conf streamAPI initDb = do
    streamAll streamAPI (ledgerDbTip initDb)
      InitFailureTooRecent
      (initDb, 0)
      push
  where
    push :: (r, b) -> (LedgerDB l r, Word64) -> m (LedgerDB l r, Word64)
    push (r, b) !(!db, !replayed) = do
        traceWith tracer (ReplayedBlock r ())
        (, replayed + 1) <$> ledgerDbPush conf (ReapplyVal r b) db

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
-- NOTE: This is a lower-level API that takes a snapshot independent from
-- whether this snapshot corresponds to a state that is more than @k@ back. It
-- doesn't take a snapshot if a snapshot at the same slot already exists on
-- disk or if the tip is at genesis.
--
-- Note that an EBB can have the same slot number as the block after it. This
-- doesn't matter. The one block difference in the ledger state doesn't warrant
-- an additional snapshot. The slot number in the name of the snapshot is only
-- indicative, we don't rely on it being correct.
--
-- TODO: Should we delete the file if an error occurs during writing?
takeSnapshot :: forall m l r h. MonadThrow m
             => Tracer m (TraceEvent r)
             -> HasFS m h
             -> (l -> Encoding)
             -> (r -> Encoding)
             -> (r -> DiskSnapshot)
             -> LedgerDB l r
             -> m (Maybe DiskSnapshot, WithOrigin r)
takeSnapshot tracer hasFS encLedger encRef toSnap db = case tip of
    Origin      -> return (Nothing, tip)
    NotOrigin r -> do
      let snapshot = toSnap r
      snaps <- listSnapshots hasFS
      if List.any (sameSlot snapshot) snaps
        then return (Nothing, tip)
        else do
          writeSnapshot hasFS encLedger encRef snapshot oldest
          traceWith tracer $ TookSnapshot snapshot tip
          return (Just snapshot, tip)
  where
    oldest :: ChainSummary l r
    oldest = ledgerDbAnchor db

    tip = csTip oldest

-- | Trim the number of on disk snapshots so that at most 'onDiskNumSnapshots'
-- snapshots are stored on disk. The oldest snapshots are deleted. Snapshots
-- with a suffix are not deleted.
trimSnapshots :: Monad m
              => Tracer m (TraceEvent r)
              -> HasFS m h
              -> DiskPolicy
              -> m ()
trimSnapshots tracer hasFS DiskPolicy{..} = do
    -- We ignore named snaspshots
    snapshots <- filter (isNothing . dsSuffix) <$> listSnapshots hasFS
    -- The snapshot are most recent first, so we can simply drop from the
    -- front to get the snapshots that are "too" old.
    forM_ (drop (fromIntegral onDiskNumSnapshots) snapshots) $ \snapshot -> do
      deleteSnapshot hasFS snapshot
      traceWith tracer $ DeletedSnapshot snapshot

{-------------------------------------------------------------------------------
  Internal: reading from disk
-------------------------------------------------------------------------------}

data DiskSnapshot = DiskSnapshot {
      -- | We name snapshots after the slot number the ledger state corresponds
      -- to. This gives an indication of how recent the snapshot is.
      --
      -- Note that the snapshot names are only indicative, we don't rely on them
      -- having the correct slot number. We only use the names to determine the
      -- order in which we try them. If a user chooses to rename their
      -- snapshots, we'll just try them in a different order. That order might
      -- be suboptimal, i.e., trying older snapshots requiring more replay
      -- first.
      dsSlotNo :: SlotNo
      -- | Snapshots can optionally have a suffix, separated by the slot number
      -- with an underscore, e.g., @4492799_last_Byron@. This suffix acts as
      -- metadata for the operator of the node. Snapshots with a suffix will
      -- /not be trimmed/.
    , dsSuffix :: Maybe String
    }
  deriving (Show, Eq, Ord, Generic)

mkDiskSnapshot :: (r -> SlotNo) -> r -> DiskSnapshot
mkDiskSnapshot toSlot r = DiskSnapshot (toSlot r) Nothing

sameSlot :: DiskSnapshot -> DiskSnapshot -> Bool
sameSlot ds1 ds2 = dsSlotNo ds1 == dsSlotNo ds2

-- | Read snapshot from disk
readSnapshot :: forall m l r h. (IOLike m)
             => HasFS m h
             -> (forall s. Decoder s l)
             -> (forall s. Decoder s r)
             -> DiskSnapshot
             -> ExceptT ReadIncrementalErr m (ChainSummary l r)
readSnapshot hasFS decLedger decRef =
      ExceptT
    . readIncremental hasFS decoder
    . snapshotToPath
  where
    decoder :: Decoder s (ChainSummary l r)
    decoder = decodeChainSummary decLedger decRef

-- | Write snapshot to disk
writeSnapshot :: forall m l r h. MonadThrow m
              => HasFS m h
              -> (l -> Encoding)
              -> (r -> Encoding)
              -> DiskSnapshot -> ChainSummary l r -> m ()
writeSnapshot hasFS encLedger encRef ss cs = do
    withFile hasFS (snapshotToPath ss) (WriteMode MustBeNew) $ \h ->
      void $ hPut hasFS h $ CBOR.toBuilder (encode cs)
  where
    encode :: ChainSummary l r -> Encoding
    encode = encodeChainSummary encLedger encRef

-- | Delete snapshot from disk
deleteSnapshot :: HasCallStack => HasFS m h -> DiskSnapshot -> m ()
deleteSnapshot HasFS{..} = removeFile . snapshotToPath

-- | List on-disk snapshots.
--
-- Snapshots are sorted based on their slot.
listSnapshots :: Monad m => HasFS m h -> m [DiskSnapshot]
listSnapshots HasFS{..} =
    aux <$> listDirectory (mkFsPath [])
  where
    aux :: Set String -> [DiskSnapshot]
    aux = List.sortOn (Down . dsSlotNo) . mapMaybe snapshotFromPath . Set.toList

-- | Snapshot files are named by their slot number and they can possibly have an
-- underscore `_` followed by a suffix.
snapshotToPath :: DiskSnapshot -> FsPath
snapshotToPath DiskSnapshot {..} = mkFsPath [pref ++ suff]
  where
    pref = show $ unSlotNo dsSlotNo
    suff = case dsSuffix of
      Nothing  -> ""
      Just str -> "_" ++ str

snapshotFromPath :: String -> Maybe DiskSnapshot
snapshotFromPath fileName = do
    slot <- readSlot pref
    return $ DiskSnapshot slot fixedSuff
  where
    (pref, suff) = break (== '_') fileName

    readSlot :: String -> Maybe SlotNo
    readSlot str = SlotNo <$> readMaybe str

    fixedSuff :: Maybe String
    fixedSuff = case suff of
      ""      -> Nothing
      _ : str -> Just str

{-------------------------------------------------------------------------------
  Trace events
-------------------------------------------------------------------------------}

data TraceEvent r
  = InvalidSnapshot DiskSnapshot (InitFailure r)
    -- ^ An on disk snapshot was skipped because it was invalid.
  | TookSnapshot DiskSnapshot (WithOrigin r)
    -- ^ A snapshot was written to disk.
  | DeletedSnapshot DiskSnapshot
    -- ^ An old or invalid on-disk snapshot was deleted
  deriving (Generic, Eq, Show)

-- | Events traced while replaying blocks against the ledger to bring it up to
-- date w.r.t. the tip of the ImmutableDB during initialisation. As this
-- process takes a while, we trace events to inform higher layers of our
-- progress.
--
-- The @replayTo@ parameter is meant to be filled in by a higher layer,
-- i.e., the ChainDB.
data TraceReplayEvent r replayTo
  = ReplayFromGenesis replayTo
    -- ^ There were no LedgerDB snapshots on disk, so we're replaying all
    -- blocks starting from Genesis against the initial ledger.
    --
    -- The @replayTo@ parameter corresponds to the block at the tip of the
    -- ImmutableDB, i.e., the last block to replay.
  | ReplayFromSnapshot DiskSnapshot (WithOrigin r) replayTo
    -- ^ There was a LedgerDB snapshot on disk corresponding to the given tip.
    -- We're replaying more recent blocks against it.
    --
    -- The @replayTo@ parameter corresponds to the block at the tip of the
    -- ImmutableDB, i.e., the last block to replay.
  | ReplayedBlock r replayTo
    -- ^ We replayed the given block (reference) on the genesis snapshot
    -- during the initialisation of the LedgerDB.
    --
    -- The @blockInfo@ parameter corresponds replayed block and the @replayTo@
    -- parameter corresponds to the block at the tip of the ImmutableDB, i.e.,
    -- the last block to replay.
  deriving (Generic, Eq, Show, Functor, Foldable, Traversable)

TH.deriveBifunctor     ''TraceReplayEvent
TH.deriveBifoldable    ''TraceReplayEvent
TH.deriveBitraversable ''TraceReplayEvent

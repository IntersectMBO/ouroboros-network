{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Ouroboros.Consensus.Storage.LedgerDB.OnDisk (
    -- * Opening the database
    initLedgerDB
  , InitLog(..)
  , InitFailure(..)
    -- ** Instantiate in-memory to @blk@
  , LedgerDB'
  , ledgerDbTip'
  , ChainSummary'
  , csTip'
  , AnnLedgerError'
    -- ** Abstraction over the stream API
  , NextBlock(..)
  , StreamAPI(..)
    -- * Write to disk
  , takeSnapshot
  , trimSnapshots
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
import qualified Data.List as List
import           Data.Maybe (mapMaybe)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word
import           GHC.Generics (Generic)
import           GHC.Stack
import           Text.Read (readMaybe)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Inspect
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Util.CBOR (ReadIncrementalErr,
                     readIncremental)
import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Consensus.Storage.FS.API
import           Ouroboros.Consensus.Storage.FS.API.Types

import           Ouroboros.Consensus.Storage.LedgerDB.DiskPolicy
import           Ouroboros.Consensus.Storage.LedgerDB.InMemory

{-------------------------------------------------------------------------------
  Instantiate the in-memory DB to @blk@
-------------------------------------------------------------------------------}

type LedgerDB'       blk = LedgerDB       (ExtLedgerState blk) (RealPoint blk)
type ChainSummary'   blk = ChainSummary   (ExtLedgerState blk) (RealPoint blk)
type AnnLedgerError' blk = AnnLedgerError (ExtLedgerState blk) (RealPoint blk)

csTip' :: ChainSummary' blk -> Point blk
csTip' = withOriginRealPointToPoint . csTip

ledgerDbTip' :: LedgerDB' blk -> Point blk
ledgerDbTip' = withOriginRealPointToPoint . ledgerDbTip

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
      -- | Start streaming after the specified block
      streamAfter :: forall a. HasCallStack
        => Point blk
        -- Reference to the block corresponding to the snapshot we found
        -- (or 'TipGen' if we didn't find any)

        -> (Maybe (m (NextBlock blk)) -> m a)
        -- Get the next block (by value)
        --
        -- Should be 'Nothing' if the snapshot we found is more recent than
        -- the tip of the immutable DB; since we only store snapshots to disk
        -- for blocks in the immutable DB, this can only happen if the
        -- immutable DB got truncated due to disk corruption.
        -> m a
    }

-- | Stream all blocks
streamAll ::
     forall m blk e a. (Monad m, HasCallStack)
  => StreamAPI m blk
  -> Point blk         -- ^ Starting point for streaming
  -> (Point blk -> e)  -- ^ Error when tip not found
  -> a                 -- ^ Starting point when tip /is/ found
  -> (blk -> a -> m a) -- ^ Update function for each block
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
data InitLog blk =
    -- | Defaulted to initialization from genesis
    --
    -- NOTE: Unless the blockchain is near genesis, we should see this /only/
    -- if data corrupted occurred.
    InitFromGenesis

    -- | Used a snapshot corresponding to the specified tip
  | InitFromSnapshot DiskSnapshot (Point blk)

    -- | Initialization skipped a snapshot
    --
    -- We record the reason why it was skipped.
    --
    -- NOTE: We should /only/ see this if data corrupted occurred.
  | InitFailure DiskSnapshot (InitFailure blk) (InitLog blk)
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
initLedgerDB ::
     forall m blk. (
         IOLike m
       , LedgerSupportsProtocol blk
       , InspectLedger blk
       , HasCallStack
       )
  => Tracer m (TraceReplayEvent blk ())
  -> Tracer m (TraceEvent blk)
  -> SomeHasFS m
  -> (forall s. Decoder s (ExtLedgerState blk))
  -> (forall s. Decoder s (RealPoint blk))
  -> LedgerDbParams
  -> ExtLedgerCfg blk
  -> m (ExtLedgerState blk) -- ^ Genesis ledger state
  -> StreamAPI m blk
  -> m (InitLog blk, LedgerDB' blk, Word64)
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
    tryNewestFirst :: (InitLog blk -> InitLog blk)
                   -> [DiskSnapshot]
                   -> m (InitLog blk, LedgerDB' blk, Word64)
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
            deleteSnapshot hasFS s
            traceWith tracer $ InvalidSnapshot s err
            tryNewestFirst (acc . InitFailure s err) ss
          Right (r, l, replayed) ->
            return (acc (InitFromSnapshot s r), l, replayed)

{-------------------------------------------------------------------------------
  Internal: initialize using the given snapshot
-------------------------------------------------------------------------------}

data InitFailure blk =
    -- | We failed to deserialise the snapshot
    --
    -- This can happen due to data corruption in the ledger DB.
    InitFailureRead ReadIncrementalErr

    -- | This snapshot is too recent (ahead of the tip of the chain)
  | InitFailureTooRecent (Point blk)
  deriving (Show, Eq, Generic)

-- | Attempt to initialize the ledger DB from the given snapshot
--
-- If the chain DB or ledger layer reports an error, the whole thing is aborted
-- and an error is returned. This should not throw any errors itself (ignoring
-- unexpected exceptions such as asynchronous exceptions, of course).
initFromSnapshot ::
     forall m blk. (
         IOLike m
       , LedgerSupportsProtocol blk
       , InspectLedger blk
       , HasCallStack
       )
  => Tracer m (TraceReplayEvent blk ())
  -> SomeHasFS m
  -> (forall s. Decoder s (ExtLedgerState blk))
  -> (forall s. Decoder s (RealPoint blk))
  -> LedgerDbParams
  -> ExtLedgerCfg blk
  -> StreamAPI m blk
  -> DiskSnapshot
  -> ExceptT (InitFailure blk) m (Point blk, LedgerDB' blk, Word64)
initFromSnapshot tracer hasFS decLedger decRef params conf streamAPI ss = do
    initSS <- withExceptT InitFailureRead $
                readSnapshot hasFS decLedger decRef ss
    lift $ traceWith tracer $ ReplayFromSnapshot ss (csTip' initSS) ()
    (initDB, replayed) <- initStartingWith tracer conf streamAPI (ledgerDbWithAnchor params initSS)
    return (csTip' initSS, initDB, replayed)

-- | Attempt to initialize the ledger DB starting from the given ledger DB
initStartingWith ::
     forall m blk. (
         Monad m
       , LedgerSupportsProtocol blk
       , InspectLedger blk
       , HasCallStack
       )
  => Tracer m (TraceReplayEvent blk ())
  -> ExtLedgerCfg blk
  -> StreamAPI m blk
  -> LedgerDB' blk
  -> ExceptT (InitFailure blk) m (LedgerDB' blk, Word64)
initStartingWith tracer conf streamAPI initDb = do
    streamAll streamAPI (ledgerDbTip' initDb)
      InitFailureTooRecent
      (initDb, 0)
      push
  where
    push :: blk -> (LedgerDB' blk, Word64) -> m (LedgerDB' blk, Word64)
    push blk !(!db, !replayed) = do
        !db' <- ledgerDbPush conf (ReapplyVal (blockRealPoint blk) blk) db

        let replayed' :: Word64
            !replayed' = replayed + 1

            events :: [LedgerEvent blk]
            events = inspectLedger
                       (getExtLedgerCfg conf)
                       (ledgerState (ledgerDbCurrent db))
                       (ledgerState (ledgerDbCurrent db'))

        traceWith tracer (ReplayedBlock (blockRealPoint blk) events ())
        return (db', replayed')

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
-- NOTE: This is a lower-level API that unconditionally takes a snapshot
-- (i.e., independent from whether this snapshot corresponds to a state that
-- is more than @k@ back).
--
-- TODO: Should we delete the file if an error occurs during writing?
takeSnapshot ::
     forall m blk. MonadThrow m
  => Tracer m (TraceEvent blk)
  -> SomeHasFS m
  -> (ExtLedgerState blk -> Encoding)
  -> (RealPoint blk -> Encoding)
  -> LedgerDB' blk -> m (DiskSnapshot, Point blk)
takeSnapshot tracer hasFS encLedger encRef db = do
    ss <- nextAvailable <$> listSnapshots hasFS
    writeSnapshot hasFS encLedger encRef ss oldest
    traceWith tracer $ TookSnapshot ss (csTip' oldest)
    return (ss, csTip' oldest)
  where
    oldest :: ChainSummary' blk
    oldest = ledgerDbAnchor db

-- | Trim the number of on disk snapshots so that at most 'onDiskNumSnapshots'
-- snapshots are stored on disk. The oldest snapshots are deleted.
--
-- The deleted snapshots are returned.
trimSnapshots ::
     Monad m
  => Tracer m (TraceEvent r)
  -> SomeHasFS m
  -> DiskPolicy
  -> m [DiskSnapshot]
trimSnapshots tracer hasFS DiskPolicy{..} = do
    snapshots <- listSnapshots hasFS
    -- The snapshot are most recent first, so we can simply drop from the
    -- front to get the snapshots that are "too" old.
    forM (drop (fromIntegral onDiskNumSnapshots) snapshots) $ \snapshot -> do
      deleteSnapshot hasFS snapshot
      traceWith tracer $ DeletedSnapshot snapshot
      return snapshot

{-------------------------------------------------------------------------------
  Internal: reading from disk
-------------------------------------------------------------------------------}

-- | On disk snapshots are numbered monotonically
newtype DiskSnapshot = DiskSnapshot Int
  deriving (Show, Eq, Ord, Generic)

-- | Number of the next snapshot, given snapshots currently on disk
nextAvailable :: [DiskSnapshot] -> DiskSnapshot
nextAvailable [] = DiskSnapshot 1
nextAvailable ss = let DiskSnapshot n = maximum ss in DiskSnapshot (n + 1)

-- | Read snapshot from disk
readSnapshot ::
     forall m blk. IOLike m
  => SomeHasFS m
  -> (forall s. Decoder s (ExtLedgerState blk))
  -> (forall s. Decoder s (RealPoint blk))
  -> DiskSnapshot
  -> ExceptT ReadIncrementalErr m (ChainSummary' blk)
readSnapshot hasFS decLedger decRef =
      ExceptT
    . readIncremental hasFS decoder
    . snapshotToPath
  where
    decoder :: Decoder s (ChainSummary' blk)
    decoder = decodeChainSummary decLedger decRef

-- | Write snapshot to disk
writeSnapshot ::
     forall m l r. MonadThrow m
  => SomeHasFS m
  -> (l -> Encoding)
  -> (r -> Encoding)
  -> DiskSnapshot -> ChainSummary l r -> m ()
writeSnapshot (SomeHasFS hasFS) encLedger encRef ss cs = do
    withFile hasFS (snapshotToPath ss) (WriteMode MustBeNew) $ \h ->
      void $ hPut hasFS h $ CBOR.toBuilder (encode cs)
  where
    encode :: ChainSummary l r -> Encoding
    encode = encodeChainSummary encLedger encRef

-- | Delete snapshot from disk
deleteSnapshot :: HasCallStack => SomeHasFS m -> DiskSnapshot -> m ()
deleteSnapshot (SomeHasFS HasFS{..}) = removeFile . snapshotToPath

-- | List on-disk snapshots, most recent first
listSnapshots :: Monad m => SomeHasFS m -> m [DiskSnapshot]
listSnapshots (SomeHasFS HasFS{..}) =
    aux <$> listDirectory (mkFsPath [])
  where
    aux :: Set String -> [DiskSnapshot]
    aux = List.sortBy (flip compare) . mapMaybe snapshotFromPath . Set.toList

snapshotToPath :: DiskSnapshot -> FsPath
snapshotToPath (DiskSnapshot ss) = mkFsPath [show ss]

snapshotFromPath :: String -> Maybe DiskSnapshot
snapshotFromPath = fmap DiskSnapshot . readMaybe

{-------------------------------------------------------------------------------
  Trace events
-------------------------------------------------------------------------------}

data TraceEvent blk
  = InvalidSnapshot DiskSnapshot (InitFailure blk)
    -- ^ An on disk snapshot was skipped because it was invalid.
  | TookSnapshot DiskSnapshot (Point blk)
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
data TraceReplayEvent blk replayTo
  = ReplayFromGenesis replayTo
    -- ^ There were no LedgerDB snapshots on disk, so we're replaying all
    -- blocks starting from Genesis against the initial ledger.
    --
    -- The @replayTo@ parameter corresponds to the block at the tip of the
    -- ImmutableDB, i.e., the last block to replay.
  | ReplayFromSnapshot DiskSnapshot (Point blk) replayTo
    -- ^ There was a LedgerDB snapshot on disk corresponding to the given tip.
    -- We're replaying more recent blocks against it.
    --
    -- The @replayTo@ parameter corresponds to the block at the tip of the
    -- ImmutableDB, i.e., the last block to replay.
  | ReplayedBlock (RealPoint blk) [LedgerEvent blk] replayTo
    -- ^ We replayed the given block (reference) on the genesis snapshot
    -- during the initialisation of the LedgerDB.
    --
    -- The @blockInfo@ parameter corresponds replayed block and the @replayTo@
    -- parameter corresponds to the block at the tip of the ImmutableDB, i.e.,
    -- the last block to replay.
  deriving (Generic, Eq, Show, Functor, Foldable, Traversable)

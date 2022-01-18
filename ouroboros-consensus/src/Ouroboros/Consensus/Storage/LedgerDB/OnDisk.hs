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
    -- * Read from disk
  , readSnapshot
    -- * Write to disk
  , takeSnapshot
  , trimSnapshots
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
import           Codec.Serialise.Decoding (Decoder)
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
import           NoThunks.Class (OnlyCheckWhnfNamed (..))
import           Text.Read (readMaybe)

import           Ouroboros.Network.Block (Point (Point))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Abstract
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
import           Ouroboros.Network.AnchoredSeq (AnchoredSeq (Empty))
import           Ouroboros.Network.Point (WithOrigin (At))

import           Control.Exception
import           Ouroboros.Consensus.Config.SecurityParam
import qualified Ouroboros.Network.AnchoredSeq as AS

{-------------------------------------------------------------------------------
  Instantiate the in-memory DB to @blk@
-------------------------------------------------------------------------------}

type LedgerDB'       blk = LedgerDB       (ExtLedgerState blk)
type AnnLedgerError' blk = AnnLedgerError (ExtLedgerState blk) blk

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
        -- (or 'GenesisPoint' if we didn't find any)

        -> (Either (RealPoint blk) (m (NextBlock blk)) -> m a)
        -- Get the next block (by value)
        --
        -- Should be @Left pt@ if the snapshot we found is more recent than the
        -- tip of the immutable DB. Since we only store snapshots to disk for
        -- blocks in the immutable DB, this can only happen if the immutable DB
        -- got truncated due to disk corruption. The returned @pt@ is a
        -- 'RealPoint', not a 'Point', since it must always be possible to
        -- stream after genesis.
        -> m a

      , streamAfterBefore :: forall a. HasCallStack
        => Point blk
        -> Point blk
        -> (Either (RealPoint blk) (m (NextBlock blk)) -> m a)
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

-- | Stream all blocks
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
    streamAfterBefore tip goal $ \case
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
--
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
  => Tracer m (ReplayGoal blk -> TraceReplayEvent blk)
  -> Tracer m (TraceEvent blk)
  -> SomeHasFS m
  -> (forall s. Decoder s (ExtLedgerState blk ValuesMK))
  -> SomeHasFS m
  -> OnDiskLedgerStDb m (ExtLedgerState blk) blk
  -> (forall s. Decoder s (ExtLedgerState blk EmptyMK))
  -> (forall s. Decoder s (HeaderHash blk))
  -> LedgerDbCfg (ExtLedgerState blk)
  -> m (ExtLedgerState blk ValuesMK) -- ^ Genesis ledger state
  -> StreamAPI m blk
  -> Bool
  -> m ((InitLog blk, InitLog blk), LedgerDB' blk, Word64)
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
        LT -> bringUpOldLedgerDB runDual               cfg streamAPI                  ledgerDb ledgerDb' tipPoint tipPoint'
        GT -> bringUpNewLedgerDB runDual replayTracer' cfg streamAPI onDiskLedgerDbSt ledgerDb ledgerDb' tipPoint tipPoint'
        EQ -> combineLedgerDBs   runDual ledgerDb ledgerDb'

    ml <- runExceptT $ initStartingWith replayTracer' cfg onDiskLedgerDbSt streamAPI lgrDB
    case ml of
      Left err -> error $  "invariant violation: invalid current chain:" <> show err
      Right (ledgerDB, w64) -> return ((initLog, initLog'), ledgerDB, w + w64)

{-------------------------------------------------------------------------------
 Load snapshots from the disk
-------------------------------------------------------------------------------}

-- | Load a snapshot from the disk. Depending on the decoder, the snapshot is
-- expected to be of the @mk@ that the decoder knows how to decode.
loadSnapshot :: ( IOLike m
                , LedgerSupportsProtocol blk
                )
             => Maybe (Tracer m (ReplayGoal blk -> TraceReplayEvent blk)) -- ^ The replay tracer to be annotated with starting point.
             -> SomeHasFS m                                       -- ^ The filesystem with the snapshots
             -> (forall s. Decoder s (ExtLedgerState blk mk))     -- ^ A decoder for the snapshots
             -> (forall s. Decoder s (HeaderHash blk))            -- ^ A decoder for header hashes
             -> DiskSnapshot                                      -- ^ Which snapshot to load on the filesystem
             -> ExceptT (InitFailure blk) m ( ExtLedgerState blk mk                                                -- The resulting ledger state
                                            , RealPoint blk                                                        -- The real point corresponding to the ledger state
                                            , Maybe (Tracer m (ReplayStart blk -> ReplayGoal blk -> TraceReplayEvent blk)) -- The annotated tracer
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
oldInitLedgerDB :: ( IOLike m
                   , LedgerSupportsProtocol blk
                   , HasCallStack
                   )
                => m (ExtLedgerState blk ValuesMK)                     -- ^ Action that gives the Genesis ledger state
                -> SomeHasFS m                                         -- ^ Filesystem containing the old ledger snapshots
                -> (forall s. Decoder s (ExtLedgerState blk ValuesMK)) -- ^ A decoder for the old ledger snapshots
                -> (forall s. Decoder s (HeaderHash blk))              -- ^ A decoder for header hashes
                -> m ( InitLog blk                                                          -- The initialization log
                     , OldLedgerDB (ExtLedgerState blk)                                     -- The resulting old ledgerDB
                     , Point blk                                                            -- The point corresponding to the adopted ledger state
                     )
oldInitLedgerDB getGenesisLedger hasFS decLedger decHash = do
    listSnapshots hasFS >>= tryNewestFirst id
  where
    tryNewestFirst acc [] = do
      -- We're out of snapshots. Start at genesis

      initSS <- getGenesisLedger
      return ( acc InitFromGenesis
             , Empty (Checkpoint initSS)
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
                 , Empty (Checkpoint ls)
                 , Point (At $ undefined pt)
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
newInitLedgerDB :: forall m blk.
                   ( IOLike m
                   , LedgerSupportsProtocol blk
                   , HasCallStack
                   )
                => Tracer m (ReplayGoal blk -> TraceReplayEvent blk)  -- ^ A tracer for the replay events
                -> Tracer m (TraceEvent blk)                          -- ^ A tracer for general events
                -> m (ExtLedgerState blk ValuesMK)                    -- ^ An action to get the Genesis ledger state
                -> SomeHasFS m                                        -- ^ The filesystem with the new snapshots
                -> (forall s. Decoder s (ExtLedgerState blk EmptyMK)) -- ^ A decoder for new snapshots
                -> (forall s. Decoder s (HeaderHash blk))             -- ^ A decoder for header hashes
                -> OnDiskLedgerStDb m (ExtLedgerState blk) blk
                -> m ( InitLog blk                                                          -- The initialization log
                     , NewLedgerDB (ExtLedgerState blk)                                     -- The new ledger database
                     , Point blk                                                            -- The point corresponding to the adopted ledger state
                     , Tracer m (ReplayStart blk -> ReplayGoal blk -> TraceReplayEvent blk) -- An annotated tracer for replay events
                     )
newInitLedgerDB replayTracer tracer getGenesisLedger hasFS decLedger decHash onDiskLedgerDbSt = do
    snapshots <- listSnapshots hasFS
    case snapshots of
      []  -> initUsingGenesis
      s:_ -> do
        ml <- runExceptT $ loadSnapshot (Just replayTracer) hasFS decLedger decHash s
        case ml of
          Left err -> do
            deleteSnapshot hasFS s
            traceWith tracer $ InvalidSnapshot s err
            (l, db, pt, tr) <- initUsingGenesis
            return (InitFailure s err l, db, pt, tr)
          Right (initSS, pt', replayTracer') -> do
            pt'' <- odlsGetPt onDiskLedgerDbSt

            assert (realPointToPoint pt' == pt'') $ return ( InitFromSnapshot s pt'
                                          , initialDbChangelogWithEmptyState (getTipSlot initSS) initSS
                                          , Point (At $ undefined pt')
                                          , maybe (error "unreachable as we provided a Just to loadSnapshot") id replayTracer'
                                          )
  where initUsingGenesis = do
          initSS <- getGenesisLedger
          writeGenesisUTxO onDiskLedgerDbSt initSS
          let replayTracer' = decorateReplayTracerWithStart (Point Origin) replayTracer
          return ( InitFromGenesis
                 , initialDbChangelog (getTipSlot initSS) initSS
                 , Point Origin
                 , replayTracer'
                 )

initialDbChangelogWithEmptyState :: WithOrigin SlotNo -> (ExtLedgerState blk EmptyMK) -> NewLedgerDB (ExtLedgerState blk)
initialDbChangelogWithEmptyState = undefined

{-------------------------------------------------------------------------------
  Sync both ledger databases
-------------------------------------------------------------------------------}

-- | The old ledger DB is behind the new ledger DB.
--
-- Pushes blocks in the ImmutableDB up to the tip of the new ledger DB so that
-- they end up being in sync.
bringUpOldLedgerDB :: forall m blk. (Monad m, LedgerSupportsProtocol blk)
                   => Bool
                   -> LedgerDbCfg (ExtLedgerState blk)
                   -> StreamAPI m blk
                   -> OldLedgerDB (ExtLedgerState blk)
                   -> NewLedgerDB (ExtLedgerState blk)
                   -> Point blk
                   -> Point blk
                   -> m (LedgerDB' blk, Word64)
bringUpOldLedgerDB runDual cfg streamAPI old ledgerDbChangelog from to = do
    either (error . ("invariant violation: invalid current chain:" <>) . show) (\(ledgerDbCheckpoints, w) -> return (LedgerDB{..}, w)) =<<
      runExceptT (streamUpTo streamAPI from to InitFailureTooRecent (old, 0) push)
  where
    push :: blk -> (OldLedgerDB (ExtLedgerState blk), Word64) -> m (OldLedgerDB (ExtLedgerState blk), Word64)
    push blk !(!db, !replayed) = do
      let !db' = let ls =   forgetLedgerStateTracking
                          $ tickThenReapply (ledgerDbCfg cfg) blk
                          $ either unCheckpoint unCheckpoint . AS.head
                          $ db
                 in AS.anchorNewest (maxRollbacks $ ledgerDbCfgSecParam cfg) (db AS.:> Checkpoint ls)

          !replayed' = replayed + 1

      return (db', replayed')

-- | The new ledger DB is behind the old ledger DB.
--
-- Pushes blocks in the ImmutableDB up to the tip of the old ledger DB so that
-- they end up being in sync.
bringUpNewLedgerDB :: forall m blk. (Monad m, LedgerSupportsProtocol blk, InspectLedger blk)
                   => Bool
                   -> Tracer m (ReplayStart blk -> ReplayGoal blk -> TraceReplayEvent blk)
                   -> LedgerDbCfg (ExtLedgerState blk)
                   -> StreamAPI m blk
                   -> OnDiskLedgerStDb m (ExtLedgerState blk) blk
                   -> OldLedgerDB (ExtLedgerState blk)
                   -> NewLedgerDB (ExtLedgerState blk)
                   -> Point blk
                   -> Point blk
                   -> m (LedgerDB' blk, Word64)
bringUpNewLedgerDB runDual tracer cfg streamAPI onDiskLedgerDbSt ledgerDbCheckpoints new to from =
    either (error . ("invariant violation: invalid current chain:" <>) . show) (\(ledgerDbChangelog, w) -> return (LedgerDB{..}, w)) =<<
      runExceptT (streamUpTo streamAPI from to InitFailureTooRecent (new, 0) push)
  where
    push :: blk -> (NewLedgerDB (ExtLedgerState blk), Word64) -> m (NewLedgerDB (ExtLedgerState blk), Word64)
    push blk !(db, replayed) = do
      ls <- defaultReadKeySets (readKeySets onDiskLedgerDbSt) (withBlockReadSets blk db $ \lh -> return $ tickThenReapply (ledgerDbCfg cfg) blk lh)
      db'' <- flushDb onDiskLedgerDbSt $ extendDbChangelog (stateSeqNo ls) (trackingTablesToDiffs ls) db
      let replayed' :: Word64
          !replayed' = replayed + 1

          events :: [LedgerEvent blk]
          events = inspectLedger
                       (getExtLedgerCfg (ledgerDbCfg cfg))
                       (ledgerState (seqLast . dbChangelogStates $ db))
                       (ledgerState (seqLast . dbChangelogStates $ db''))

      traceWith tracer (ReplayedBlock (blockRealPoint blk) events)
      return (db'', replayed')

-- | Both databases are already in sync, just combine them in the @LedgerDB@ datatype.
combineLedgerDBs :: Monad m
                 => Bool
                 -> OldLedgerDB (ExtLedgerState blk)
                 -> NewLedgerDB (ExtLedgerState blk)
                 -> m (LedgerDB' blk, Word64)
combineLedgerDBs runDual ledgerDbCheckpoints ledgerDbChangelog = return (LedgerDB {..}, 0)

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
  deriving (Show, Eq, Generic)

mkOnDiskLedgerStDb :: SomeHasFS m -> m (OnDiskLedgerStDb m l blk)
mkOnDiskLedgerStDb = undefined
  -- \(SomeHasFS fs) -> do
  --   dbhandle <- hOpen fs "ledgerStateDb"
  --   ...

  --   return OnDiskLedgerStDb
  --   { ...
  --     , readKeySets = Snapshots.readDb dbhandle

  --     }

-- | On disk ledger state API.
--
--
data OnDiskLedgerStDb m l blk =
  OnDiskLedgerStDb
  { rewindTableKeySets  :: () -- TODO: move the corresponding function from
                               -- InMemory here.
  , forwardTableKeySets :: () -- TODO: ditto.

  , readKeySets         :: RewoundTableKeySets l -> m (UnforwardedReadSets l)
   -- ^ Captures the handle. Implemented by Snapshots.readDb
   --
   -- TODO: consider unifying this with defaultReadKeySets. Why? Because we are always using
   -- 'defaultReadKeySets' with readKeySets.
  , flushDb             :: DbChangelog l -> m (DbChangelog l )
    -- ^ Flush the ledger DB when appropriate. We assume the implementation of
    -- this function will determine when to flush.
    --
    -- NOTE: Captures the handle and the flushing policy. Implemented by
    -- Snapshots.writeDb.
  , createRestorePoint  :: DbChangelog l -> m ()
    -- ^ Captures the DbHandle. Implemented using createRestorePoint (proposed
    -- by Douglas). We need to take the current SeqNo for the on disk state from
    -- the DbChangelog.

    {- -* other restore point ops ... -}
  , closeDb             :: m ()
    -- ^ This closes the captured handle.
  , odlsGetPt           :: m (Point blk)
    -- ^ Get the point representing the latest ledger state flushed to the disk
  , writeGenesisUTxO    :: l ValuesMK -> m ()
    -- ^ Write the initial Genesis UTxO to the disk
  }
  deriving NoThunks via OnlyCheckWhnfNamed "OnDiskLedgerStDb" (OnDiskLedgerStDb m l blk)

-- | Attempt to initialize the ledger DB starting from the given ledger DB
initStartingWith ::
     forall m blk. (
         Monad m
       , LedgerSupportsProtocol blk
       , InspectLedger blk
       , HasCallStack
       )
  => Tracer m (ReplayStart blk -> ReplayGoal blk -> TraceReplayEvent blk)
  -> LedgerDbCfg (ExtLedgerState blk)
  -> OnDiskLedgerStDb m (ExtLedgerState blk) blk
  -> StreamAPI m blk
  -> LedgerDB' blk
  -> ExceptT (InitFailure blk) m (LedgerDB' blk, Word64)
initStartingWith tracer cfg onDiskLedgerDbSt streamAPI initDb = do
    streamAll streamAPI (castPoint (ledgerDbTip initDb))
      InitFailureTooRecent
      (initDb, 0)
      push
  where
    push :: blk -> (LedgerDB' blk, Word64) -> m (LedgerDB' blk, Word64)
    push blk !(!db, !replayed) = do
        !db' <- defaultReadKeySets (readKeySets onDiskLedgerDbSt) $
                  ledgerDbPush cfg (ReapplyVal blk) db
        db'' <- ledgerDbFlush (flushDb onDiskLedgerDbSt) db'

        let replayed' :: Word64
            !replayed' = replayed + 1

            events :: [LedgerEvent blk]
            events = inspectLedger
                       (getExtLedgerCfg (ledgerDbCfg cfg))
                       (ledgerState (ledgerDbCurrent db))
                       (ledgerState (ledgerDbCurrent db''))

        traceWith tracer (ReplayedBlock (blockRealPoint blk) events)
        return (db'', replayed')

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
  => Tracer m (TraceEvent blk)
  -> SomeHasFS m
  -> (ExtLedgerState blk EmptyMK -> Encoding)
  -> LedgerDB' blk -> m (Maybe (DiskSnapshot, RealPoint blk))
takeSnapshot tracer hasFS encLedger db =
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
  where
    oldest :: ExtLedgerState blk EmptyMK
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

-- | Write snapshot to disk
writeSnapshot ::
     forall m blk. MonadThrow m
  => SomeHasFS m
  -> (ExtLedgerState blk EmptyMK -> Encoding)
  -> DiskSnapshot
  -> ExtLedgerState blk EmptyMK -> m ()
writeSnapshot (SomeHasFS hasFS) encLedger ss cs = do
    withFile hasFS (snapshotToPath ss) (WriteMode MustBeNew) $ \h ->
      void $ hPut hasFS h $ CBOR.toBuilder (encode cs)
  where
    encode :: ExtLedgerState blk EmptyMK -> Encoding
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
  Trace events
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

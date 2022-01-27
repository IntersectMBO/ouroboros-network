{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE MultiWayIf           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Storage.LedgerDB.OnDisk (
    -- * Opening the database
    InitFailure (..)
  , InitLog (..)
  , NewLedgerInitParams (..)
  , OldLedgerInitParams (..)
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

import           Cardano.Slotting.Slot (WithOrigin (At))
import           Control.Monad.Trans.Except (throwE)
import           Ouroboros.Consensus.Storage.LedgerDB.DiskPolicy
import           Ouroboros.Consensus.Storage.LedgerDB.InMemory

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
data StreamAPI m e blk = StreamAPI {
  -- | Start streaming after the specified block up to the end of the
  -- immutable database.
  streamAfter :: forall a. HasCallStack
              => Point blk
              -- Reference to the block corresponding to the snapshot we found
              -- (or 'Point Origin' if we didn't find any)

              -> (Either (RealPoint blk) (m (NextBlock blk)) -> ExceptT e m a)
              -- Get the next block (by value)
              --
              -- Should be @Left pt@ if the snapshot we found is more recent than the
              -- tip of the immutable DB. Since we only store snapshots to disk for
              -- blocks in the immutable DB, this can only happen if the immutable DB
              -- got truncated due to disk corruption. The returned @pt@ is a
              -- 'RealPoint', not a 'Point', since it must always be possible to
              -- stream after genesis.
              -> ExceptT e m a
  }

-- | Stream all blocks
streamAll ::
     forall m blk e a. (Monad m, HasCallStack)
  => StreamAPI m e blk
  -> Point blk                           -- ^ Starting point for streaming
  -> (RealPoint blk -> e)                -- ^ Error when tip not found
  -> a                                   -- ^ Starting point when tip /is/ found
  -> (blk -> a -> ExceptT e m (Maybe a)) -- ^ Update function for each block with stop condition
  -> ExceptT e m a
streamAll StreamAPI{..} tip notFound e f =
    streamAfter tip $ \case
      Left tip' -> throwE (notFound tip')

      Right getNext -> do
        let go :: a -> ExceptT e m a
            go a = do mNext <- lift $ getNext
                      case mNext of
                        NoMoreBlocks -> return a
                        NextBlock b  -> maybe (return a) go =<< f b a
        go e

-- | The @StreamAPI@ type refined to expect an exception of type @InitFailure@
type StreamAPI' m blk = StreamAPI m (InitFailure blk) blk

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
  deriving (Eq, Show, Generic)

-- | Grouped parameters specific only for the old-style LedgerDB
data OldLedgerInitParams m blk = OldLedgerInitParams {
    oldFS      :: SomeHasFS m
  , oldDecoder :: forall s. Decoder s (ExtLedgerState blk ValuesMK)
  }

-- | Grouped parameters specific only for the new-style LedgerDB
data NewLedgerInitParams m blk = NewLedgerInitParams {
    newFS      :: SomeHasFS m
  , newDecoder :: forall s. Decoder s (ExtLedgerState blk EmptyMK)
  , backend    :: OnDiskLedgerStDb m (ExtLedgerState blk) blk
  }

-- | Initialize the ledger DB
--
-- This will:
-- 1. deserialize the most recent found snapshot for each of the internal databases
-- 2. bring up the database that is behind until they are at the same tip
-- 3. replay the remaining blocks on the immutable database on both databases
--
-- If no snapshot is found on either database, it will be initialized with the
-- Genesis ledger state.
--
-- During replay, no exceptions are caught, and instead it should be
-- responsibility of the 'ChainDB' to actually catch them and, if needed,
-- trigger further validation.
--
-- Snapshots will be deleted if:
--
-- * We cannot deserialize them, except if they are permanent snapshots.
-- * they are /ahead/ of the chain.
--
-- After initialization, if there was data corruptuion on the immutable
-- database, the LedgerDB might not be able to rollback @k@ blocks.
initLedgerDB ::
     forall m blk. (
         IOLike m
       , LedgerSupportsProtocol blk
       , InspectLedger blk
       , HasCallStack
       )
  => Tracer m (ReplayGoal blk -> TraceReplayEvent blk)
  -> Tracer m (TraceEvent blk)
  -> OldLedgerInitParams m blk
  -> NewLedgerInitParams m blk
  -> (forall s. Decoder s (HeaderHash blk))
  -> LedgerDbCfg (ExtLedgerState blk)
  -> m (ExtLedgerState blk ValuesMK)                     -- ^ Genesis ledger state
  -> StreamAPI' m blk
  -> Bool
  -> m ((Maybe (InitLog blk), InitLog blk), LedgerDB' blk, Word64)
initLedgerDB replayTracer tracer OldLedgerInitParams{..} NewLedgerInitParams{..} decHash cfg getGenesisLedger streamAPI runAlsoOld = do

    mOldLedgerDB <- if runAlsoOld then Just <$> oldInitLedgerDB
                                                       getGenesisLedger
                                                       oldFS
                                                       oldDecoder
                                                       decHash
                                  else return Nothing

    (initLog', newStyleDB, replayTracer') <- newInitLedgerDB
                                              replayTracer
                                              tracer
                                              getGenesisLedger
                                              newFS
                                              newDecoder
                                              decHash
                                              backend

    ml <- runExceptT $
      case mOldLedgerDB of
        Nothing ->
          -- We are only going to ever use the new-style LedgerDB
          let
            ledgerDb = combine Nothing newStyleDB
          in
            (Nothing,) <$> initStartingWith replayTracer' cfg backend streamAPI ledgerDb
        Just (initLog, oldStyleDB) -> do
          let oldTip = pointSlot . getTip $ oldLedgerDbCurrent oldStyleDB
              newTip = pointSlot . getTip $ newLedgerDbCurrent newStyleDB
          -- We are going to use both databases
          (inSyncDB, replayedOnNew) <-
            case compare oldTip newTip of
              -- The new-style database is behind the old-style one
              GT -> do
                (newStyleDB', replayed) <- initializeNewDB replayTracer' newStyleDB oldTip
                return (combine (Just oldStyleDB) newStyleDB', replayed)

              -- The old-style database is behind the new-style one
              LT -> do
                (oldStyleDB', _) <- initializeOldDB replayTracer' oldStyleDB newTip
                return (combine (Just oldStyleDB') newStyleDB, 0)

              -- The databases are in sync
              EQ -> return (combine (Just oldStyleDB) newStyleDB, 0)

          -- from this point on, apply blocks on both databases
          (initializedDb, replayedOnBoth) <- initStartingWith replayTracer' cfg backend streamAPI inSyncDB
          return (Just initLog, (initializedDb, replayedOnBoth + replayedOnNew))
    case ml of
      Left InitFailureNotInSyncWithUTxO -> error "invariant violation: UTxO-HD backend is not in sync with snapshots"
      Left err -> error $ "invariant violation: invalid current chain:" <> show err
      Right (initLog, (ledgerDB, replayedBlocks)) -> return ((initLog, initLog'), ledgerDB, replayedBlocks)
  where
    combine :: Maybe (OldLedgerDB (ExtLedgerState blk))
            -> NewLedgerDB (ExtLedgerState blk)
            -> LedgerDB (ExtLedgerState blk)
    combine = mkLedgerDB @(ExtLedgerState blk) @blk

    initializeDB
      :: Tracer m (ReplayStart blk -> ReplayGoal blk -> TraceReplayEvent blk)
      -> db
      -> (db -> Point blk)                                          -- ^ Get current tip point function
      -> (ExtLedgerCfg blk
          -> Ap m1 l blk (ReadsKeySets m1 l)
          -> db
          -> DbReader
               m
               (ExtLedgerState blk)
               (Either
                  (RealPoint blk, LedgerErr (ExtLedgerState blk))
                  resultingLedger))                                 -- ^ Apply block function
      -> (resultingLedger -> db -> db)                              -- ^ Push block function
      -> WithOrigin SlotNo                                          -- ^ Goal point
      -> ExceptT (InitFailure blk) m (db, Word64)
    initializeDB replayTracer' initDb getCur apBlock push goal =
      initDatabaseWithStreamAndUpd
        streamAPI
        initDb
        getCur
        (\blk (db, replayed) -> if At (blockSlot blk) > goal
                                then
                                  -- We have already replayed enough blocks, the databases are in sync now
                                  return Nothing
                                else do
                                  lift $ traceWith replayTracer' (ReplayedBlock (blockRealPoint blk) [])

                                  -- Apply the block to the tip of the db
                                  resultingLedgerState <-   lift (defaultReadKeySets (readKeySets backend)
                                                                   $ apBlock (ledgerDbCfg cfg) (ReapplyVal blk) db)

                                  -- Embed the computation in an @ExceptT (InitFailure blk) (NewLedgerDB l)@ as
                                  -- we need to do so for the @StreamAPI@ interface, but as we know that we are
                                  -- reapplying blocks, we don't need to rethrow an @AnnLedgerError@ as it will never happen.
                                  result <- either
                                    (\(_, err) -> error $ "invariant violation: invalid current chain:" <> show err)
                                    return
                                    resultingLedgerState
                                  -- TODO: add some trace
                                  return . Just . (, replayed + 1) $ push result db)

    initializeNewDB :: Tracer m (ReplayStart blk -> ReplayGoal blk -> TraceReplayEvent blk)
                    -> NewLedgerDB (ExtLedgerState blk)
                    -> WithOrigin SlotNo
                    -> ExceptT (InitFailure blk) m (NewLedgerDB (ExtLedgerState blk), Word64)
    initializeNewDB replayTracer' newStyleDB oldTip =
      initializeDB replayTracer' newStyleDB (castPoint . getTip . newLedgerDbCurrent) newApplyBlock pushLedgerStateNew oldTip


    initializeOldDB :: Tracer m (ReplayStart blk -> ReplayGoal blk -> TraceReplayEvent blk)
                    -> OldLedgerDB (ExtLedgerState blk)
                    -> WithOrigin SlotNo
                    -> ExceptT (InitFailure blk) m (OldLedgerDB (ExtLedgerState blk), Word64)
    initializeOldDB replayTracer' oldStyleDB newTip =
      initializeDB replayTracer' oldStyleDB (castPoint . getTip . oldLedgerDbCurrent) oldApplyBlock (pushLedgerStateOld (ledgerDbCfgSecParam cfg) . snd) newTip

{-------------------------------------------------------------------------------
 Load snapshots from the disk
-------------------------------------------------------------------------------}

-- | Load a snapshot from disk. Depending on the decoder, the snapshot is
-- expected to be of the @mk@ that the decoder knows how to decode.
--
-- This will throw an exception @InitFailureRead@ if it can't deserialize a
-- snapshot or an @InitFailureGenesis@ if we are trying to load a snapshot that
-- corresponds to Genesis, which should never happen as we don't ever serialize
-- a ledger state corresponding to Genesis.
loadSnapshot :: ( IOLike m
                , LedgerSupportsProtocol blk
                )
             => Maybe (Tracer m (ReplayGoal blk -> TraceReplayEvent blk))
             -> SomeHasFS m
             -> (forall s. Decoder s (ExtLedgerState blk mk))
             -> (forall s. Decoder s (HeaderHash blk))
             -> DiskSnapshot                      -- ^ Which snapshot to load from the filesystem
             -> ExceptT (InitFailure blk)
                        m
                        ( ExtLedgerState blk mk
                        , RealPoint blk   -- The real point corresponding to the deserialized ledger state
                        , Maybe (Tracer m (ReplayStart blk -> ReplayGoal blk -> TraceReplayEvent blk))
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
oldInitLedgerDB :: forall m blk. ( IOLike m
                   , LedgerSupportsProtocol blk
                   , HasCallStack
                   )
                => m (ExtLedgerState blk ValuesMK)                     -- ^ Action that gives the Genesis ledger state
                -> SomeHasFS m                                         -- ^ Filesystem containing the old ledger snapshots
                -> (forall s. Decoder s (ExtLedgerState blk ValuesMK)) -- ^ A decoder for the old ledger snapshots
                -> (forall s. Decoder s (HeaderHash blk))
                -> m ( InitLog blk
                     , OldLedgerDB (ExtLedgerState blk)
                     )
oldInitLedgerDB getGenesisLedger hasFS decLedger decHash = do
    listSnapshots hasFS >>= tryNewestFirst id
  where
    tryNewestFirst :: (InitLog blk -> InitLog blk)
                   -> [DiskSnapshot]
                   -> m (InitLog blk, OldLedgerDB (ExtLedgerState blk))
    tryNewestFirst acc [] = do
      -- We're out of snapshots. Start at genesis
      initSS <- getGenesisLedger
      return ( acc InitFromGenesis
             , oldLedgerDbWithAnchor initSS
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
                => Tracer m (ReplayGoal blk -> TraceReplayEvent blk)
                -> Tracer m (TraceEvent blk)
                -> m (ExtLedgerState blk ValuesMK)                    -- ^ An action to get the Genesis ledger state
                -> SomeHasFS m                                        -- ^ The filesystem with the new snapshots
                -> (forall s. Decoder s (ExtLedgerState blk EmptyMK)) -- ^ A decoder for new snapshots
                -> (forall s. Decoder s (HeaderHash blk))
                -> OnDiskLedgerStDb m (ExtLedgerState blk) blk
                -> m ( InitLog blk
                     , NewLedgerDB (ExtLedgerState blk)
                     , Tracer m (ReplayStart blk -> ReplayGoal blk -> TraceReplayEvent blk)
                     )
newInitLedgerDB replayTracer tracer getGenesisLedger hasFS decLedger decHash onDiskLedgerDbSt = do
    snapshots <- findNewSnapshot hasFS
    case snapshots of
      Nothing -> initUsingGenesis
      Just s  -> do
        ml <- runExceptT $ loadSnapshot (Just replayTracer) hasFS decLedger decHash s
        case ml of
          Left err -> do
            -- Here we don't guard for permanent snapshots because if we have a
            -- permanent snapshot at slot @s@ which fails to deserialize, we
            -- will start from genesis, and if we don't remove it and we don't
            -- go past it, the next time we will also fail to deserialize it and
            -- start again from Genesis.
            deleteSnapshot hasFS s
            traceWith tracer $ InvalidSnapshot s err
            (l, db, tr) <- initUsingGenesis
            return (InitFailure s err l, db, tr)
          Right (initSS, pt', replayTracer') -> do
            pt'' <- odlsGetPt onDiskLedgerDbSt

            -- if the @pt'@ from the deserialized ledger state is not in sync
            -- with the @pt''@ that the disk backend claims to be at, we cannot
            -- use this snapshot as it is not in sync with the disk, and as we
            -- cannot have previous snapshots we can just start from Genesis.

            if realPointToPoint pt' == pt''
              then return ( InitFromSnapshot s pt'
                          , newLedgerDbWithEmptyAnchor pt'' initSS
                          , maybe (error "unreachable as we provided a Just to loadSnapshot") id replayTracer'
                          )
              else do
                -- Same as above, we don't guard against permanent snapshots.
                deleteSnapshot hasFS s
                traceWith tracer $ InvalidSnapshot s InitFailureNotInSyncWithUTxO
                (l, db, tr) <- initUsingGenesis
                return (InitFailure s InitFailureNotInSyncWithUTxO l, db, tr)
  where initUsingGenesis = do
          initSS <- getGenesisLedger
          writeGenesisUTxO onDiskLedgerDbSt initSS
          let replayTracer' = decorateReplayTracerWithStart (Point Origin) replayTracer
          return ( InitFromGenesis
                 , newLedgerDbWithAnchor initSS
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

-- | A generalization of initializing a database by consuming a StreamAPI.
initDatabaseWithStreamAndUpd ::
     forall m blk ledgerDb. (Monad m, HasCallStack)
  => StreamAPI' m blk
  -> ledgerDb
  -> (ledgerDb -> Point blk)
  -> (blk -> (ledgerDb, Word64) -> ExceptT (InitFailure blk) m (Maybe (ledgerDb, Word64)))
  -> ExceptT (InitFailure blk) m (ledgerDb, Word64)
initDatabaseWithStreamAndUpd streamAPI initDb getPoint push = do
    streamAll streamAPI (getPoint initDb)
      InitFailureTooRecent
      (initDb, 0)
      push

-- | Attempt to initialize the ledger DB starting from the given ledger DB by
-- streaming blocks from the immutable database up to the immutable database
-- tip.
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
  -> StreamAPI' m blk
  -> LedgerDB' blk
  -> ExceptT (InitFailure blk) m (LedgerDB' blk, Word64)
initStartingWith tracer cfg onDiskLedgerDbSt streamAPI initDb = do
    initDatabaseWithStreamAndUpd streamAPI initDb (castPoint . ledgerDbTip) push
  where
    push :: blk -> (LedgerDB' blk, Word64) -> ExceptT (InitFailure blk) m (Maybe (LedgerDB' blk, Word64))
    push blk !(!db, !replayed) = lift $ do
        !db' <- defaultReadKeySets (readKeySets onDiskLedgerDbSt)
                $ ledgerDbPush cfg (ReapplyVal blk) db
        -- TODO: here it is important that we don't have a lock acquired.

        -- Alternatively, we could chose not to check for a lock when we're
        -- flushing here since we know the `LgrDB` does not exist at this point
        -- yet.
        db'' <- ledgerDbFlush (flushDb onDiskLedgerDbSt) db'
        -- TODO: it seems we'd want:
        --
        --     - flush
        --
        --     - make a restore-point
        --
        -- We can't make the flush in the levels above push since this function
        -- consumes the whole stream of immutable DB blocks.
        let replayed' :: Word64
            !replayed' = replayed + 1

            events :: [LedgerEvent blk]
            events = inspectLedger
                       (getExtLedgerCfg (ledgerDbCfg cfg))
                       (ledgerState (ledgerDbCurrent db))
                       (ledgerState (ledgerDbCurrent db''))

        traceWith tracer (ReplayedBlock (blockRealPoint blk) events)
        return $ Just (db'', replayed')

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

-- | Only one new-style snapshot should exist on the system. This functions
-- returns it or returns Nothing. If multiple snapshots are found, it will
-- return the one with the highest number (as it is expected to be the newest).
findNewSnapshot :: Monad m => SomeHasFS m -> m (Maybe DiskSnapshot)
findNewSnapshot (SomeHasFS HasFS{..}) = do
    ls <- listDirectory (mkFsPath [])
    case aux ls of
      []  -> return Nothing
      s:_ -> return (Just s)
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
  deriving (Show, Eq, Generic)

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

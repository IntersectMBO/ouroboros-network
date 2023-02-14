{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Thin wrapper around the LedgerDB
module Ouroboros.Consensus.Storage.ChainDB.Impl.LgrDB (
    LgrDB
  , lgrBackingStore
    -- opaque
  , LedgerDB'
  , LgrDbSerialiseConstraints
    -- * Initialization
  , LgrDbArgs (..)
  , defaultArgs
  , openDB
    -- * 'TraceReplayEvent' decorator
  , LedgerDB.decorateReplayTracerWithGoal
    -- * Wrappers
  , currentPoint
  , getCurrent
  , getDiskPolicy
  , setCurrent
  , takeSnapshot
  , trimSnapshots
    -- * Validation
  , ValidateResult (..)
  , validate
    -- * Previously applied blocks
  , garbageCollectPrevApplied
  , getPrevApplied
    -- * Reading ledger tables
  , getLedgerTablesAtFor
    -- * Flush lock
  , withReadLock
    -- * Re-exports
  , LedgerDB.AnnLedgerError (..)
  , LedgerDB.DiskPolicy (..)
  , LedgerDB.DiskSnapshot
  , LedgerDB.ExceededRollback (..)
  , LedgerDB.TraceLedgerDBEvent (..)
  , LedgerDB.TraceReplayEvent (..)
  , LedgerDB.TraceSnapshotEvent (..)
  , LedgerDB.current
    -- * Exported for testing purposes
  , mkLgrDB
  ) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import           Codec.Serialise (Serialise (decode))
import           Control.Monad.Trans.Class
import           Control.Tracer
import           Data.Foldable (foldl')
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Inspect
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util.Args
import           Ouroboros.Consensus.Util.IOLike
import qualified Ouroboros.Consensus.Util.MonadSTM.RAWLock as Lock
import           Ouroboros.Consensus.Util.ResourceRegistry

import           Ouroboros.Consensus.Storage.Common
import           Ouroboros.Consensus.Storage.FS.API (SomeHasFS (..),
                     createDirectoryIfMissing)
import           Ouroboros.Consensus.Storage.FS.API.Types (FsError, mkFsPath)

import           Ouroboros.Consensus.Storage.LedgerDB (LedgerDB')
import qualified Ouroboros.Consensus.Storage.LedgerDB as LedgerDB
import           Ouroboros.Consensus.Storage.LedgerDB.BackingStore
import           Ouroboros.Consensus.Storage.LedgerDB.DbChangelog hiding (flush)
import           Ouroboros.Consensus.Storage.LedgerDB.ReadsKeySets
import           Ouroboros.Consensus.Storage.LedgerDB.Stream

import           Data.Functor.Contravariant ((>$<))
import           Ouroboros.Consensus.Storage.ChainDB.API (ChainDbFailure (..))
import           Ouroboros.Consensus.Storage.ChainDB.Impl.BlockCache
                     (BlockCache)
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.BlockCache as BlockCache
import           Ouroboros.Consensus.Storage.ImmutableDB (ImmutableDB)
import qualified Ouroboros.Consensus.Storage.ImmutableDB as ImmutableDB
import           Ouroboros.Consensus.Storage.Serialisation


-- | Thin wrapper around the ledger database
data LgrDB m blk = LgrDB {
      varDB           :: !(StrictTVar m (LedgerDB' blk))
      -- ^ INVARIANT: the tip of the 'LedgerDB' is always in sync with the tip
      -- of the current chain of the ChainDB.
    , varPrevApplied  :: !(StrictTVar m (Set (RealPoint blk)))
      -- ^ INVARIANT: this set contains only points that are in the
      -- VolatileDB.
      --
      -- INVARIANT: all points on the current chain fragment are in this set.
      --
      -- The VolatileDB might contain invalid blocks, these will not be in
      -- this set.
      --
      -- When a garbage-collection is performed on the VolatileDB, the points
      -- of the blocks eligible for garbage-collection should be removed from
      -- this set.
    , lgrBackingStore :: !(LedgerBackingStore m (ExtLedgerState blk))
      -- ^ Handle to the ledger's backing store, containing the parts that grow
      -- too big for in-memory residency
    , lgrFlushLock    :: !(Lock.RAWLock m ())
      -- ^ The flush lock to the 'BackingStore'. This lock is crucial when it
      -- comes to keeping the data in memory consistent with the data on-disk.
      --
      -- This lock should be held whenever we want to keep a consistent view of
      -- the backing store for some time. In particular we use this:
      --
      -- - when performing a query on the ledger state, we need to hold a
      --   'DiskLedgerView' which, while live, must maintain a consistent view
      --   of the DB, and therefore we acquire a Read lock.
      --
      -- - when taking a snapshot of the ledger db, we need to prevent others
      --   from altering the backing store at the same time, thus we acquire a
      --   Write lock.
    , resolveBlock    :: !(RealPoint blk -> m blk)
      -- ^ Read a block from disk
    , cfg             :: !(TopLevelConfig blk)
    , diskPolicy      :: !LedgerDB.DiskPolicy
    , hasFS           :: !(SomeHasFS m)
    , tracer          :: !(Tracer m (LedgerDB.TraceLedgerDBEvent blk))
    } deriving (Generic)

deriving instance (IOLike m, LedgerSupportsProtocol blk)
               => NoThunks (LgrDB m blk)
  -- use generic instance

-- | 'EncodeDisk' and 'DecodeDisk' constraints needed for the LgrDB.
type LgrDbSerialiseConstraints blk =
  ( Serialise      (HeaderHash  blk)
  , EncodeDisk blk (LedgerState blk EmptyMK)
  , DecodeDisk blk (LedgerState blk EmptyMK)
  , EncodeDisk blk (AnnTip      blk)
  , DecodeDisk blk (AnnTip      blk)
  , EncodeDisk blk (ChainDepState (BlockProtocol blk))
  , DecodeDisk blk (ChainDepState (BlockProtocol blk))
  , CanSerializeLedgerTables (LedgerState blk)
  )

{-------------------------------------------------------------------------------
  Initialization
-------------------------------------------------------------------------------}

data LgrDbArgs f m blk = LgrDbArgs {
      lgrDiskPolicy           :: LedgerDB.DiskPolicy
    , lgrGenesis              :: HKD f (m (ExtLedgerState blk ValuesMK))
    , lgrHasFS                :: SomeHasFS m
    , lgrTopLevelConfig       :: HKD f (TopLevelConfig blk)
    , lgrTraceLedger          :: Tracer m (LedgerDB' blk)
    , lgrTracer               :: Tracer m (LedgerDB.TraceLedgerDBEvent blk)
    , lgrRegistry             :: HKD f (ResourceRegistry m)
    , lgrBackingStoreSelector :: !(LedgerDB.BackingStoreSelector m)
    }

-- | Default arguments
defaultArgs ::
     Applicative m
  => SomeHasFS m
  -> LedgerDB.DiskPolicy
  -> LedgerDB.BackingStoreSelector m
  -> LgrDbArgs Defaults m blk
defaultArgs lgrHasFS diskPolicy bss = LgrDbArgs {
      lgrDiskPolicy           = diskPolicy
    , lgrGenesis              = NoDefault
    , lgrHasFS
    , lgrTopLevelConfig       = NoDefault
    , lgrTraceLedger          = nullTracer
    , lgrTracer               = nullTracer
    , lgrRegistry             = NoDefault
    , lgrBackingStoreSelector = bss
    }

-- | Open the ledger DB
--
-- In addition to the ledger DB also returns the number of immutable blocks
-- that were replayed.
openDB :: forall m blk.
          ( IOLike m
          , LedgerSupportsProtocol blk
          , LgrDbSerialiseConstraints blk
          , InspectLedger blk
          , HasCallStack
          )
       => LgrDbArgs Identity m blk
       -- ^ Stateless initializaton arguments
       -> Tracer m (LedgerDB.ReplayGoal blk -> LedgerDB.TraceReplayEvent blk)
       -- ^ Used to trace the progress while replaying blocks against the
       -- ledger.
       -> ImmutableDB m blk
       -- ^ Reference to the immutable DB
       --
       -- After reading a snapshot from disk, the ledger DB will be brought
       -- up to date with tip of the immutable DB. The corresponding ledger
       -- state can then be used as the starting point for chain selection in
       -- the ChainDB driver.
       -> (RealPoint blk -> m blk)
       -- ^ Read a block from disk
       --
       -- The block may be in the immutable DB or in the volatile DB; the ledger
       -- DB does not know where the boundary is at any given point.
       -> m (LgrDB m blk, Word64)
openDB args@LgrDbArgs { lgrHasFS = lgrHasFS@(SomeHasFS hasFS), .. } replayTracer immutableDB getBlock = do
    createDirectoryIfMissing hasFS True (mkFsPath [])
    (db, replayed, lgrBackingStore) <- initFromDisk args replayTracer immutableDB
    -- When initializing the ledger DB from disk we:
    --
    -- - Look for the newest valid snapshot, say 'Lbs', which corresponds to the
    --   application of a block in the immutable DB, say 'b'.
    --
    -- - Push onto the ledger DB all the ledger states that result from applying
    --   blocks found in the on-disk immutable DB, starting from the successor
    --   of 'b'.
    --
    -- The anchor of 'LedgerDB' must be the oldest point we can rollback to. So
    -- if we follow the procedure described above (that 'initFromDisk'
    -- implements), the newest ledger state in 'db', say 'Lbn' corresponds to
    -- the most recent block in the immutable DB. If this block is in the
    -- immutable DB, it means that at some point it was part of a chain that was
    -- >k blocks long. Thus 'Lbn' is the oldest point we can roll back to.
    -- Therefore, we need to make the newest state (current) of the ledger DB
    -- the anchor.
    let dbPrunedToImmDBTip = LedgerDB.prune (SecurityParam 0) db
    (varDB, varPrevApplied) <-
      (,) <$> newTVarIO dbPrunedToImmDBTip <*> newTVarIO Set.empty
    flushLock <- Lock.new ()
    return (
        LgrDB {
            varDB           = varDB
          , varPrevApplied  = varPrevApplied
          , lgrBackingStore = lgrBackingStore
          , lgrFlushLock    = flushLock
          , resolveBlock    = getBlock
          , cfg             = lgrTopLevelConfig
          , diskPolicy      = lgrDiskPolicy
          , hasFS           = lgrHasFS
          , tracer          = lgrTracer
          }
      , replayed
      )

initFromDisk
  :: forall blk m.
     ( IOLike m
     , LedgerSupportsProtocol blk
     , LgrDbSerialiseConstraints blk
     , InspectLedger blk
     , HasCallStack
     )
  => LgrDbArgs Identity m blk
  -> Tracer m (LedgerDB.ReplayGoal blk -> LedgerDB.TraceReplayEvent blk)
  -> ImmutableDB m blk
  -> m (LedgerDB' blk, Word64, LedgerBackingStore m (ExtLedgerState blk))
initFromDisk LgrDbArgs { lgrHasFS = hasFS, .. }
             replayTracer
             immutableDB = wrapFailure (Proxy @blk) $ do
    (_initLog, db, replayed, backingStore) <-
      LedgerDB.initialize
        replayTracer
        lgrTracer
        hasFS
        lgrRegistry
        decodeExtLedgerState'
        decode
        (LedgerDB.configLedgerDb lgrTopLevelConfig)
        lgrGenesis
        (streamAPI immutableDB)
        lgrBackingStoreSelector
    return (db, replayed, backingStore)
  where
    ccfg = configCodec lgrTopLevelConfig

    decodeExtLedgerState' :: forall s. Decoder s (ExtLedgerState blk EmptyMK)
    decodeExtLedgerState' = decodeExtLedgerState
                              (decodeDisk ccfg)
                              (decodeDisk ccfg)
                              (decodeDisk ccfg)

-- | For testing purposes
mkLgrDB :: StrictTVar m (LedgerDB' blk)
        -> StrictTVar m (Set (RealPoint blk))
        -> LedgerBackingStore m (ExtLedgerState blk)
        -> Lock.RAWLock m ()
        -> (RealPoint blk -> m blk)
        -> LgrDbArgs Identity m blk
        -> LgrDB m blk
mkLgrDB varDB varPrevApplied lgrBackingStore lgrFlushLock resolveBlock args = LgrDB {..}
  where
    LgrDbArgs {
        lgrTopLevelConfig = cfg
      , lgrDiskPolicy     = diskPolicy
      , lgrHasFS          = hasFS
      , lgrTracer         = tracer
      } = args

{-------------------------------------------------------------------------------
  Wrappers
-------------------------------------------------------------------------------}

getCurrent :: IOLike m => LgrDB m blk -> STM m (LedgerDB' blk)
getCurrent LgrDB{..} = readTVar varDB

-- | PRECONDITION: The new 'LedgerDB' must be the result of calling either
-- 'LedgerDB.ledgerDbSwitch' or 'LedgerDB.ledgerDbPushMany' on the current
-- 'LedgerDB'.
setCurrent :: IOLike m => LgrDB m blk -> LedgerDB' blk -> STM m ()
setCurrent LgrDB{..} = writeTVar $! varDB

currentPoint :: forall blk. UpdateLedger blk => LedgerDB' blk -> Point blk
currentPoint = castPoint
             . ledgerTipPoint
             . ledgerState
             . LedgerDB.current

takeSnapshot ::
     forall m blk.
     ( IOLike m
     , LgrDbSerialiseConstraints blk
     , LedgerSupportsProtocol blk
     )
  => LgrDB m blk -> m (Maybe (LedgerDB.DiskSnapshot, RealPoint blk))
takeSnapshot lgrDB = wrapFailure (Proxy @blk) $ do
    withWriteLock lgrDB $ do
      flush lgrDB

      -- CRITICAL: Snapshots are taken from the last flushed state and not from
      -- the tip of the immutable db. See 'flush'.
      --
      -- In particular, the diffs for the immutable part have been flushed to
      -- disk at this point and therefore it is that same state the one that we
      -- should take a snapshot from. Usually it is the Immutable tip but
      -- nothing prevents another thread from flushing again and therefore
      -- moving the immutable tip. This *SHOULD* not happen because the write
      -- lock is held here, but to be on the safe side, we take a snapshot from
      -- the last flushed state which we know will be accurate.
      ledgerDB <- LedgerDB.lastFlushedState
                  <$> atomically (getCurrent lgrDB)
      LedgerDB.takeSnapshot
        (LedgerDB.LedgerDBSnapshotEvent >$< tracer)
        hasFS
        lgrBackingStore
        encodeExtLedgerState'
        ledgerDB
  where
    LgrDB{ cfg, tracer, hasFS, lgrBackingStore } = lgrDB

    ccfg = configCodec cfg

    encodeExtLedgerState' :: ExtLedgerState blk EmptyMK -> Encoding
    encodeExtLedgerState' = encodeExtLedgerState
                              (encodeDisk ccfg)
                              (encodeDisk ccfg)
                              (encodeDisk ccfg)

trimSnapshots ::
     forall m blk. (MonadCatch m, HasHeader blk)
  => LgrDB m blk
  -> m [LedgerDB.DiskSnapshot]
trimSnapshots LgrDB { diskPolicy, tracer, hasFS } = wrapFailure (Proxy @blk) $
    LedgerDB.trimSnapshots (LedgerDB.LedgerDBSnapshotEvent >$< tracer) hasFS diskPolicy

getDiskPolicy :: LgrDB m blk -> LedgerDB.DiskPolicy
getDiskPolicy = diskPolicy

-- | Flush the "immutable" diffs into the 'BackingStore' and replace the
-- 'LedgerDB' reference with the "volatile" part of the original db.
--
-- PRECONDITION: The 'flushLock' write lock must be held before calling this
-- function
flush :: (IOLike m, LedgerSupportsProtocol blk) => LgrDB m blk -> m ()
flush LgrDB { varDB, lgrBackingStore } = do
    toFlush <- atomically $ do
      db <- readTVar varDB
      let (toFlush, db') = LedgerDB.flush FlushAllImmutable db
      writeTVar varDB db'
      pure toFlush
    flushIntoBackingStore lgrBackingStore toFlush

{-------------------------------------------------------------------------------
  Validation
-------------------------------------------------------------------------------}

data ValidateResult blk =
    ValidateSuccessful       (LedgerDB'       blk)
  | ValidateLedgerError      (LedgerDB.AnnLedgerError' blk)
  | ValidateExceededRollBack LedgerDB.ExceededRollback

validate :: forall m blk. (IOLike m, LedgerSupportsProtocol blk, HasCallStack)
         => LgrDB m blk
         -> LedgerDB' blk
            -- ^ This is used as the starting point for validation, not the one
            -- in the 'LgrDB'.
         -> BlockCache blk
         -> Word64  -- ^ How many blocks to roll back
         -> (LedgerDB.UpdateLedgerDbTraceEvent blk -> m ())
         -> [Header blk]
         -> m (ValidateResult blk)
validate LgrDB{..} ledgerDB blockCache numRollbacks trace = \hdrs -> do
    aps <- mkAps hdrs <$> atomically (readTVar varPrevApplied)
    res <- fmap rewrap $ LedgerDB.defaultResolveWithErrors resolveBlock $
             LedgerDB.switch
               (LedgerDB.configLedgerDb cfg)
               numRollbacks
               (lift . lift . trace)
               aps
               (lift . lift . readKeySets lgrBackingStore)
               ledgerDB
    atomically $ modifyTVar varPrevApplied $
      addPoints (validBlockPoints res (map headerRealPoint hdrs))
    return res
  where
    rewrap :: Either (LedgerDB.AnnLedgerError' blk) (Either LedgerDB.ExceededRollback (LedgerDB' blk))
           -> ValidateResult blk
    rewrap (Left         e)  = ValidateLedgerError      e
    rewrap (Right (Left  e)) = ValidateExceededRollBack e
    rewrap (Right (Right l)) = ValidateSuccessful       l

    mkAps :: forall n l. l ~ ExtLedgerState blk
          => [Header blk]
          -> Set (RealPoint blk)
          -> [LedgerDB.Ap n l blk ( LedgerDB.ResolvesBlocks    n   blk
                                  , LedgerDB.ThrowsLedgerError n l blk
                                  )]
    mkAps hdrs prevApplied =
      [ case ( Set.member (headerRealPoint hdr) prevApplied
             , BlockCache.lookup (headerHash hdr) blockCache
             ) of
          (False, Nothing)  ->                   LedgerDB.ApplyRef   (headerRealPoint hdr)
          (True,  Nothing)  -> LedgerDB.Weaken $ LedgerDB.ReapplyRef (headerRealPoint hdr)
          (False, Just blk) -> LedgerDB.Weaken $ LedgerDB.ApplyVal   blk
          (True,  Just blk) -> LedgerDB.Weaken $ LedgerDB.ReapplyVal blk
      | hdr <- hdrs
      ]

    -- | Based on the 'ValidateResult', return the hashes corresponding to
    -- valid blocks.
    validBlockPoints :: ValidateResult blk -> [RealPoint blk] -> [RealPoint blk]
    validBlockPoints = \case
      ValidateExceededRollBack _ -> const []
      ValidateSuccessful       _ -> id
      ValidateLedgerError      e -> takeWhile (/= LedgerDB.annLedgerErrRef e)

    addPoints :: [RealPoint blk]
              -> Set (RealPoint blk) -> Set (RealPoint blk)
    addPoints hs set = foldl' (flip Set.insert) set hs

{-------------------------------------------------------------------------------
  Stream API to the immutable DB
-------------------------------------------------------------------------------}

streamAPI ::
     forall m blk.
     (IOLike m, HasHeader blk)
  => ImmutableDB m blk -> StreamAPI m blk blk
streamAPI = streamAPI' (return . NextItem) GetBlock

streamAPI' ::
     forall m blk a.
     (IOLike m, HasHeader blk)
  => (a -> m (NextItem a)) -- ^ Stop condition
  -> BlockComponent   blk a
  -> ImmutableDB    m blk
  -> StreamAPI      m blk a
streamAPI' shouldStop blockComponent immutableDB = StreamAPI streamAfter
  where
    streamAfter :: Point blk
                -> (Either (RealPoint blk) (m (NextItem a)) -> m b)
                -> m b
    streamAfter tip k = withRegistry $ \registry -> do
        eItr <-
          ImmutableDB.streamAfterPoint
            immutableDB
            registry
            blockComponent
            tip
        case eItr of
          -- Snapshot is too recent
          Left  err -> k $ Left  $ ImmutableDB.missingBlockPoint err
          Right itr -> k $ Right $ streamUsing itr

    streamUsing :: ImmutableDB.Iterator m blk a
                -> m (NextItem a)
    streamUsing itr = do
        itrResult <- ImmutableDB.iteratorNext itr
        case itrResult of
          ImmutableDB.IteratorExhausted -> return NoMoreItems
          ImmutableDB.IteratorResult b  -> shouldStop b

{-------------------------------------------------------------------------------
  Previously applied blocks
-------------------------------------------------------------------------------}

getPrevApplied :: IOLike m => LgrDB m blk -> STM m (Set (RealPoint blk))
getPrevApplied LgrDB{..} = readTVar varPrevApplied

-- | Remove all points with a slot older than the given slot from the set of
-- previously applied points.
garbageCollectPrevApplied :: IOLike m => LgrDB m blk -> SlotNo -> STM m ()
garbageCollectPrevApplied LgrDB{..} slotNo = modifyTVar varPrevApplied $
    Set.dropWhileAntitone ((< slotNo) . realPointSlot)

{-------------------------------------------------------------------------------
  Error handling
-------------------------------------------------------------------------------}

-- | Wrap exceptions that may indicate disk failure in a 'ChainDbFailure'
-- exception using the 'LgrDbFailure' constructor.
wrapFailure ::
     forall m x blk. (MonadCatch m, HasHeader blk)
  => Proxy blk
  -> m x
  -> m x
wrapFailure _ k = catch k rethrow
  where
    rethrow :: FsError -> m x
    rethrow err = throwIO $ LgrDbFailure @blk err

{-------------------------------------------------------------------------------
  Flush lock operations
-------------------------------------------------------------------------------}

-- | Acquire the ledger DB read lock and hold it while performing an action
withReadLock :: IOLike m => LgrDB m blk -> m a -> m a
withReadLock lgrDB m =
    Lock.withReadAccess (lgrFlushLock lgrDB) (\() -> m)

-- | Acquire the ledger DB write lock and hold it while performing an action
withWriteLock :: IOLike m => LgrDB m blk -> m a -> m a
withWriteLock lgrDB m =
    Lock.withWriteAccess (lgrFlushLock lgrDB) (\() -> (,) () <$> m)

{-------------------------------------------------------------------------------
 Getting tables
-------------------------------------------------------------------------------}

-- | Read and forward the values up to the given point on the chain.
getLedgerTablesAtFor ::
  ( IOLike m
  , LedgerSupportsProtocol blk
  , StandardHash (ExtLedgerState blk)
  )
  => Point blk
  -> LedgerTables (ExtLedgerState blk) KeysMK
  -> LgrDB m blk
  -> m (Either
        (PointNotFound blk)
        (LedgerTables (ExtLedgerState blk) ValuesMK))
getLedgerTablesAtFor pt keys lgr@LgrDB{ varDB, lgrBackingStore } = do
  lgrDb <- atomically $ readTVar varDB
  case LedgerDB.rollback pt lgrDb of
    Nothing -> pure $ Left $ PointNotFound pt
    Just l  -> do
      eValues <-
        getLedgerTablesFor l keys (readKeySets lgrBackingStore)
      case eValues of
        Right v -> pure $ Right v
        Left _  -> getLedgerTablesAtFor pt keys lgr

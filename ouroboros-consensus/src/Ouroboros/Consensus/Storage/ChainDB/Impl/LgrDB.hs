{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Thin wrapper around the LedgerDB
module Ouroboros.Consensus.Storage.ChainDB.Impl.LgrDB (
    LgrDB
    -- opaque
  , LedgerDB'
  , LgrDbSerialiseConstraints
    -- * Initialization
  , LgrDbArgs (..)
  , defaultArgs
  , openDB
    -- * Ledger HD operations
  , flush
    -- ** Read/Write lock operations
  , unsafeAcquireReadLock
  , unsafeAcquireWriteLock
  , unsafeReleaseReadLock
  , unsafeReleaseWriteLock
  , withReadLock
  , withWriteLock
    -- * 'TraceReplayEvent' decorator
  , LedgerDB.decorateReplayTracerWithGoal
    -- * Wrappers
  , currentPoint
  , getCurrent
  , getDiskPolicy
  , setCurrent
  , takeSnapshot
  , trimSnapshots
  , trimNewSnapshots
    -- * Validation
  , ValidateResult (..)
  , validate
    -- * Previously applied blocks
  , garbageCollectPrevApplied
  , getPrevApplied
    -- * Re-exports
  , DiskPolicy (..)
  , DiskSnapshot
  , ExceededRollback (..)
  , LedgerDB.AnnLedgerError (..)
  , LedgerDB.ledgerDbCurrent
  , TraceEvent (..)
  , TraceReplayEvent (..)
    -- * Exported for testing purposes
  , mkLgrDB
  ) where

import           Codec.CBOR.Encoding (Encoding)
import           Codec.CBOR.Decoding (Decoder)
import           Codec.Serialise (Serialise (decode))
import           Control.Monad.Trans.Class (lift)
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
import           Ouroboros.Consensus.Util.ResourceRegistry

import           Ouroboros.Consensus.Storage.Common
import           Ouroboros.Consensus.Storage.FS.API (SomeHasFS (..),
                     createDirectoryIfMissing)
import           Ouroboros.Consensus.Storage.FS.API.Types (FsError, mkFsPath)
import           Ouroboros.Consensus.Storage.LedgerDB.Types
                     (UpdateLedgerDbTraceEvent (..))

import           Ouroboros.Consensus.Storage.LedgerDB.DiskPolicy
                     (DiskPolicy (..))
import           Ouroboros.Consensus.Storage.LedgerDB.InMemory (Ap (..),
                     ExceededRollback (..), LedgerDbCfg (..))
import qualified Ouroboros.Consensus.Storage.LedgerDB.InMemory as LedgerDB
import           Ouroboros.Consensus.Storage.LedgerDB.OnDisk (AnnLedgerError',
                     DiskSnapshot, LedgerDB', NextBlock (..),
                     OnDiskLedgerStDb (..), ReplayGoal, StreamAPI (..),
                     TraceEvent (..), TraceReplayEvent (..))
import qualified Ouroboros.Consensus.Storage.LedgerDB.OnDisk as LedgerDB
import qualified Ouroboros.Consensus.Storage.LedgerDB.UTxOHD as LedgerDB

import           Ouroboros.Consensus.Storage.ChainDB.API (ChainDbFailure (..))
import           Ouroboros.Consensus.Storage.ChainDB.Impl.BlockCache
                     (BlockCache)
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.BlockCache as BlockCache
import           Ouroboros.Consensus.Storage.ImmutableDB (ImmutableDB)
import qualified Ouroboros.Consensus.Storage.ImmutableDB as ImmutableDB
import           Ouroboros.Consensus.Storage.Serialisation

-- | Thin wrapper around the ledger database
data LgrDB m blk = LgrDB {
      varDB               :: !(StrictTVar m (LedgerDB' blk))
      -- ^ INVARIANT: the tip of the 'LedgerDB' is always in sync with the tip
      -- of the current chain of the ChainDB.
    , varPrevApplied      :: !(StrictTVar m (Set (RealPoint blk)))
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
    , lgrOnDiskLedgerStDb :: !(LedgerDB.OnDiskLedgerStDb m (ExtLedgerState blk) blk)
      -- ^ The UTxO DB backend handle.
    , lgrDbFlushLock      :: !FlushLock
      -- ^ The flush lock that prevents concurrent read and write actions on the
      -- UTxO DB backend handle.
    , resolveBlock        :: !(LedgerDB.ResolveBlock m blk) -- TODO: ~ (RealPoint blk -> m blk)
      -- ^ Read a block from disk
    , cfg                 :: !(TopLevelConfig blk)
    , diskPolicy          :: !DiskPolicy
      -- ^ The disk policy dictating when to take snapshots of the in-memory data
    , oldHasFS            :: !(LedgerDB.OldLedgerFS m)
      -- ^ The filesystem containing old-style snapshots
    , newHasFS            :: !(LedgerDB.NewLedgerFS m)
      -- ^ The filesystem containing new-style snapshots
    , tracer              :: !(Tracer m (TraceEvent blk))
      -- ^ A general tracer for the LedgerDB events
    } deriving (Generic)

deriving instance (IOLike m, LedgerSupportsProtocol blk)
               => NoThunks (LgrDB m blk)
  -- use generic instance

-- | 'EncodeDisk' and 'DecodeDisk' constraints needed for the LgrDB.
type LgrDbSerialiseConstraints blk =
  ( Serialise      (HeaderHash  blk)
  , EncodeDisk blk (LedgerState blk EmptyMK)
  , DecodeDisk blk (LedgerState blk EmptyMK)
  , EncodeDisk blk (LedgerState blk ValuesMK)
  , DecodeDisk blk (LedgerState blk ValuesMK)
  , EncodeDisk blk (AnnTip      blk)
  , DecodeDisk blk (AnnTip      blk)
  , EncodeDisk blk (ChainDepState (BlockProtocol blk))
  , DecodeDisk blk (ChainDepState (BlockProtocol blk))
  )

{-------------------------------------------------------------------------------
  Initialization
-------------------------------------------------------------------------------}

data LgrDbArgs f m blk = LgrDbArgs {
      lgrDiskPolicy     :: DiskPolicy
    , lgrGenesis        :: HKD f (m (ExtLedgerState blk ValuesMK))
    , lgrOldHasFS       :: LedgerDB.OldLedgerFS m
    , lgrNewHasFS       :: LedgerDB.NewLedgerFS m
    , lgrTopLevelConfig :: HKD f (TopLevelConfig blk)
    , lgrTraceLedger    :: Tracer m (LedgerDB' blk)
    , lgrTracer         :: Tracer m (TraceEvent blk)
    }

-- | Default arguments
defaultArgs ::
     Applicative m
  => SomeHasFS m
  -> SomeHasFS m
  -- ^ TODO: we need some type wrappers to distinguish the two kind of SomeHasFS
  -- we use in this function.
  -> DiskPolicy
  -> LgrDbArgs Defaults m blk
defaultArgs oldHasFS newHasFS diskPolicy = LgrDbArgs {
      lgrDiskPolicy     = diskPolicy
    , lgrGenesis        = NoDefault
    , lgrOldHasFS       = LedgerDB.OldLedgerFS oldHasFS
    , lgrNewHasFS       = LedgerDB.NewLedgerFS newHasFS
    , lgrTopLevelConfig = NoDefault
    , lgrTraceLedger    = nullTracer
    , lgrTracer         = nullTracer
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
       -> Tracer m (ReplayGoal blk -> TraceReplayEvent blk)
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
       --
       -- TODO: should we replace this type with @ResolveBlock blk m@?
       -> Bool
       -> m (LgrDB m blk, (Word64, Word64))
openDB args@LgrDbArgs { lgrOldHasFS = (LedgerDB.OldLedgerFS (SomeHasFS oldHasFS))
                      , lgrNewHasFS = (LedgerDB.NewLedgerFS (SomeHasFS newHasFS))
                      , .. }
       replayTracer
       immutableDB
       getBlock
       _runDual = do
    createDirectoryIfMissing oldHasFS True (mkFsPath [])
    createDirectoryIfMissing newHasFS True (mkFsPath [])
    (db, replayed, onDiskLedgerStDb) <- initFromDisk args replayTracer immutableDB
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
    let dbPrunedToImmDBTip = LedgerDB.ledgerDbPrune (SecurityParam 0) db
    (varDB, varPrevApplied) <-
      (,) <$> newTVarIO dbPrunedToImmDBTip <*> newTVarIO Set.empty
    flock <- mkFlushLock -- TODO: do we want to register this lock in any
                         -- resource registry? Depending on how the lock will be
                         -- implemented an exception after lock creation might
                         -- not be a problem (eg using MVars or TVars).
    return (
        LgrDB {
            varDB               = varDB
          , varPrevApplied      = varPrevApplied
          , lgrOnDiskLedgerStDb = onDiskLedgerStDb
          , lgrDbFlushLock      = flock
          , resolveBlock        = getBlock
          , cfg                 = lgrTopLevelConfig
          , diskPolicy          = lgrDiskPolicy
          , oldHasFS            = lgrOldHasFS args
          , newHasFS            = lgrNewHasFS args
          , tracer              = lgrTracer
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
  -> Tracer m (ReplayGoal blk -> TraceReplayEvent blk)
  -> ImmutableDB m blk
  -> m (LedgerDB' blk, (Word64, Word64), OnDiskLedgerStDb m (ExtLedgerState blk) blk)
initFromDisk LgrDbArgs{ lgrNewHasFS = lgrNewHasFS@(LedgerDB.NewLedgerFS lgrNewFS)
                      , ..}
             replayTracer
             immutableDB =
    wrapFailure (Proxy @blk) $ do
      onDiskLedgerStDb <- LedgerDB.mkOnDiskLedgerStDb lgrNewFS
      -- TODO: is it correct that we pick a instance of the 'OnDiskLedgerStDb' here?
      (_initLog, db, replayed) <-
        LedgerDB.initLedgerDB
          replayTracer
          lgrTracer
          lgrOldHasFS
          decodeExtLedgerState'
          lgrNewHasFS
          onDiskLedgerStDb
          decodeExtLedgerState'
          decode
          (configLedgerDb lgrTopLevelConfig)
          lgrGenesis
          (streamAPI immutableDB)
          True
      return (db, replayed, onDiskLedgerStDb)
  where

    ccfg = configCodec lgrTopLevelConfig

    decodeExtLedgerState' :: forall s mk. DecodeDisk blk (LedgerState blk mk) => Decoder s (ExtLedgerState blk mk)
    decodeExtLedgerState' = decodeExtLedgerState
                              (decodeDisk ccfg)
                              (decodeDisk ccfg)
                              (decodeDisk ccfg)

-- | For testing purposes
mkLgrDB :: StrictTVar m (LedgerDB' blk)
        -> StrictTVar m (Set (RealPoint blk))
        -> LedgerDB.OnDiskLedgerStDb m (ExtLedgerState blk) blk
        -> FlushLock
        -> (RealPoint blk -> m blk)
        -> LgrDbArgs Identity m blk
        -> LgrDB m blk
mkLgrDB varDB varPrevApplied lgrOnDiskLedgerStDb lgrDbFlushLock resolveBlock args = LgrDB {..}
  where
    LgrDbArgs {
        lgrTopLevelConfig = cfg
      , lgrDiskPolicy     = diskPolicy
      , lgrOldHasFS       = oldHasFS
      , lgrNewHasFS       = newHasFS
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

currentPoint :: LedgerSupportsProtocol blk => LedgerDB' blk -> Point blk
currentPoint = castPoint
             . LedgerDB.ledgerDbTip

takeSnapshot ::
     forall m blk.
     ( IOLike m
     , LgrDbSerialiseConstraints blk
     , HasHeader blk
     , IsLedger (LedgerState blk)
     )
  => LgrDB m blk
  -> Bool
  -> m (Maybe (DiskSnapshot, RealPoint blk))
takeSnapshot lgrDB@LgrDB{ cfg
                        , tracer
                        , oldHasFS = LedgerDB.OldLedgerFS oldHasFS
                        , newHasFS = LedgerDB.NewLedgerFS newHasFS }
             isNew =
    wrapFailure (Proxy @blk) $ do
     cur <- atomically (getCurrent lgrDB)
     LedgerDB.takeSnapshot
        tracer
        (if isNew then newHasFS else oldHasFS)
        encodeExtLedgerState'
        encodeExtLedgerState'
        cur
        isNew
  where
    ccfg = configCodec cfg

    encodeExtLedgerState' :: forall mk. EncodeDisk blk (LedgerState blk mk) => ExtLedgerState blk mk -> Encoding
    encodeExtLedgerState' = encodeExtLedgerState
                              (encodeDisk ccfg)
                              (encodeDisk ccfg)
                              (encodeDisk ccfg)

trimSnapshots ::
     forall m blk. (MonadCatch m, HasHeader blk)
  => LgrDB m blk
  -> m [DiskSnapshot]
trimSnapshots LgrDB { diskPolicy, tracer, oldHasFS } = wrapFailure (Proxy @blk) $
    LedgerDB.trimOldSnapshots tracer oldHasFS diskPolicy

trimNewSnapshots
  :: forall m blk. (MonadCatch m, HasHeader blk)
  => LgrDB m blk
  -> DiskSnapshot
  -> m [DiskSnapshot]
trimNewSnapshots LgrDB {tracer , newHasFS } s = wrapFailure (Proxy @blk) $
    LedgerDB.trimNewSnapshots tracer newHasFS s

getDiskPolicy :: LgrDB m blk -> DiskPolicy
getDiskPolicy = diskPolicy

flush :: IOLike m => LgrDB m blk -> RealPoint blk -> m ()
flush lgrDB@LgrDB { varDB, lgrOnDiskLedgerStDb } pointToFlush =
  -- TODO: why putting the lock inside LgrDB and not in the CDB? I rather couple
  -- the ledger DB with the flush lock that guards it. I don't feel comfortable
  -- with the possiblility of having a lock as a parameter here that could come
  -- from anywhere. Also, when we use this function, we don't have to pass the
  -- extra argument.
  withWriteLock lgrDB $ do
    db  <- readTVarIO varDB
    db' <- LedgerDB.ledgerDbFlush lgrOnDiskLedgerStDb pointToFlush db
    atomically $ writeTVar varDB db'

{-------------------------------------------------------------------------------
  Validation
-------------------------------------------------------------------------------}

data ValidateResult blk =
    ValidateSuccessful       (LedgerDB'       blk)
  | ValidateLedgerError      (AnnLedgerError' blk)
  | ValidateExceededRollBack ExceededRollback

validate :: forall m blk. (IOLike m, LedgerSupportsProtocol blk, HasCallStack)
         => LgrDB m blk
         -> LedgerDB' blk
            -- ^ This is used as the starting point for validation, not the one
            -- in the 'LgrDB'.
         -> BlockCache blk
         -> Word64  -- ^ How many blocks to roll back
         -> (UpdateLedgerDbTraceEvent blk -> m ())
         -> [Header blk]
         -> m (ValidateResult blk)
validate LgrDB{..} ledgerDB blockCache numRollbacks trace = \hdrs -> do
    aps <- mkAps hdrs <$> atomically (readTVar varPrevApplied)
    res <- fmap rewrap $
             LedgerDB.defaultReadKeySets (readKeySets lgrOnDiskLedgerStDb) $
             LedgerDB.defaultResolveWithErrors (LedgerDB.DbReader . lift . resolveBlock) $
               LedgerDB.ledgerDbSwitch
                 (configLedgerDb cfg)
                 numRollbacks
                 (lift . lift . LedgerDB.DbReader . lift . trace)
                 aps
                 ledgerDB
    atomically $ modifyTVar varPrevApplied $
      addPoints (validBlockPoints res (map headerRealPoint hdrs))
    return res
  where
    rewrap :: Either (AnnLedgerError' blk) (Either ExceededRollback (LedgerDB' blk))
           -> ValidateResult blk
    rewrap (Left         e)  = ValidateLedgerError      e
    rewrap (Right (Left  e)) = ValidateExceededRollBack e
    rewrap (Right (Right l)) = ValidateSuccessful       l

    mkAps :: forall n l. l ~ ExtLedgerState blk
          => [Header blk]
          -> Set (RealPoint blk)
          -> [Ap n l blk ( LedgerDB.ResolvesBlocks    n   blk
                         , LedgerDB.ReadsKeySets         n l
                         , LedgerDB.ThrowsLedgerError n l blk
                         )]
    mkAps hdrs prevApplied =
      [ case ( Set.member (headerRealPoint hdr) prevApplied
             , BlockCache.lookup (headerHash hdr) blockCache
             ) of
          (False, Nothing)  -> Weaken $ ApplyRef   (headerRealPoint hdr)
          (True,  Nothing)  -> Weaken $ ReapplyRef (headerRealPoint hdr)
          (False, Just blk) -> Weaken $ ApplyVal   blk
          (True,  Just blk) -> Weaken $ ReapplyVal blk
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
  => ImmutableDB m blk -> StreamAPI m blk
streamAPI immutableDB = StreamAPI streamAfter streamAfterUpTo
  where
    streamAfter :: HasCallStack
                => Point blk
                -> (Either (RealPoint blk) (m (NextBlock blk)) -> m a)
                -> m a
    streamAfter tip k = withRegistry $ \registry -> do
        eItr <-
          ImmutableDB.streamAfterPoint
            immutableDB
            registry
            GetBlock
            tip
        case eItr of
          -- Snapshot is too recent
          Left  err -> k $ Left  $ ImmutableDB.missingBlockPoint err
          Right itr -> k $ Right $ streamUsing itr

    streamUsing :: ImmutableDB.Iterator m blk blk -> m (NextBlock blk)
    streamUsing itr = ImmutableDB.iteratorNext itr >>= \case
      ImmutableDB.IteratorExhausted  -> return $ NoMoreBlocks
      ImmutableDB.IteratorResult blk -> return $ NextBlock blk

    streamAfterUpTo :: HasCallStack
                => Point blk
                -> Point blk
                -> (Either (RealPoint blk) (m (NextBlock blk)) -> m a)
                -> m a
    streamAfterUpTo from to k = withRegistry $ \registry -> do
      eItr <- ImmutableDB.streamWithBounds
                immutableDB
                registry
                GetBlock
                from
                to
      case eItr of
        Left err  -> k $ Left $ ImmutableDB.missingBlockPoint err
        Right itr -> k $ Right $ streamUsing itr

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
  Auxiliary
-------------------------------------------------------------------------------}

configLedgerDb ::
     ConsensusProtocol (BlockProtocol blk)
  => TopLevelConfig blk
  -> LedgerDbCfg (ExtLedgerState blk)
configLedgerDb cfg = LedgerDbCfg {
      ledgerDbCfgSecParam = configSecurityParam cfg
    , ledgerDbCfg         = ExtLedgerCfg cfg
    }

{-------------------------------------------------------------------------------
  Flush lock operations
-------------------------------------------------------------------------------}

-- | Perform an action acquiring and holding the ledger DB read lock.
withReadLock :: IOLike m => LgrDB m blk -> m a -> m a
withReadLock lgrDB =
  bracket_ (unsafeAcquireReadLock lgrDB) (unsafeReleaseReadLock lgrDB)

-- | Perform an action acquiring and holding the ledger DB write lock.
withWriteLock :: IOLike m => LgrDB m blk -> m () -> m ()
withWriteLock lgrDB =
  bracket_ (unsafeAcquireWriteLock lgrDB) (unsafeReleaseWriteLock lgrDB)

-- | Acquire a read lock on the ledger DB.
--
-- This operation must be guarded against (asyncrhonous) exceptions, ensuring
-- that the corresponding release operation is called.
unsafeAcquireReadLock :: LgrDB m blk -> m ()
unsafeAcquireReadLock = undefined

unsafeReleaseReadLock :: LgrDB m blk -> m ()
unsafeReleaseReadLock = undefined

unsafeAcquireWriteLock :: LgrDB m blk -> m ()
unsafeAcquireWriteLock = undefined

unsafeReleaseWriteLock :: LgrDB m blk -> m ()
unsafeReleaseWriteLock = undefined

data FlushLock = FlushLock
  deriving (Show, Eq, Generic, NoThunks)

mkFlushLock :: IOLike m => m FlushLock
mkFlushLock = undefined

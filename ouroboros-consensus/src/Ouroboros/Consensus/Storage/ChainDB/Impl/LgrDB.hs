{-# LANGUAGE ConstraintKinds     #-}
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
    -- * 'TraceReplayEvent' decorator
  , TraceLedgerReplayEvent
  , decorateReplayTracer
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

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import           Codec.Serialise (Serialise (decode))
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

import           Ouroboros.Consensus.Storage.LedgerDB.DiskPolicy
                     (DiskPolicy (..))
import           Ouroboros.Consensus.Storage.LedgerDB.InMemory (Ap (..),
                     ExceededRollback (..), LedgerDbCfg (..))
import qualified Ouroboros.Consensus.Storage.LedgerDB.InMemory as LedgerDB
import           Ouroboros.Consensus.Storage.LedgerDB.OnDisk (AnnLedgerError',
                     DiskSnapshot, LedgerDB', NextBlock (..), StreamAPI (..),
                     TraceEvent (..), TraceReplayEvent (..))
import qualified Ouroboros.Consensus.Storage.LedgerDB.OnDisk as LedgerDB

import           Ouroboros.Consensus.Storage.ChainDB.API (ChainDbFailure (..))
import           Ouroboros.Consensus.Storage.ChainDB.Impl.BlockCache
                     (BlockCache)
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.BlockCache as BlockCache
import           Ouroboros.Consensus.Storage.ImmutableDB (ImmutableDB)
import qualified Ouroboros.Consensus.Storage.ImmutableDB as ImmutableDB
import           Ouroboros.Consensus.Storage.Serialisation

-- | Thin wrapper around the ledger database
data LgrDB m blk = LgrDB {
      varDB          :: !(StrictTVar m (LedgerDB' blk))
      -- ^ INVARIANT: the tip of the 'LedgerDB' is always in sync with the tip
      -- of the current chain of the ChainDB.
    , varPrevApplied :: !(StrictTVar m (Set (RealPoint blk)))
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
    , resolveBlock   :: !(RealPoint blk -> m blk)
      -- ^ Read a block from disk
    , cfg            :: !(TopLevelConfig blk)
    , diskPolicy     :: !DiskPolicy
    , hasFS          :: !(SomeHasFS m)
    , tracer         :: !(Tracer m (TraceEvent blk))
    } deriving (Generic)

deriving instance (IOLike m, LedgerSupportsProtocol blk)
               => NoThunks (LgrDB m blk)
  -- use generic instance

-- | 'EncodeDisk' and 'DecodeDisk' constraints needed for the LgrDB.
type LgrDbSerialiseConstraints blk =
  ( Serialise      (HeaderHash  blk)
  , EncodeDisk blk (LedgerState blk)
  , DecodeDisk blk (LedgerState blk)
  , EncodeDisk blk (AnnTip      blk)
  , DecodeDisk blk (AnnTip      blk)
  , EncodeDisk blk (ChainDepState (BlockProtocol blk))
  , DecodeDisk blk (ChainDepState (BlockProtocol blk))
  )

{-------------------------------------------------------------------------------
  Initialization
-------------------------------------------------------------------------------}

data LgrDbArgs f m blk = LgrDbArgs {
      lgrDiskPolicy     :: HKD f DiskPolicy
    , lgrGenesis        :: HKD f (m (ExtLedgerState blk))
    , lgrHasFS          :: SomeHasFS m
    , lgrTopLevelConfig :: HKD f (TopLevelConfig blk)
    , lgrTraceLedger    :: Tracer m (LedgerDB' blk)
    , lgrTracer         :: Tracer m (TraceEvent blk)
    }

-- | Default arguments
defaultArgs :: Applicative m => SomeHasFS m -> LgrDbArgs Defaults m blk
defaultArgs lgrHasFS = LgrDbArgs {
      lgrDiskPolicy     = NoDefault
    , lgrGenesis        = NoDefault
    , lgrHasFS
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
       -> Tracer m (TraceReplayEvent blk ())
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
    (db, replayed) <- initFromDisk args replayTracer immutableDB
    (varDB, varPrevApplied) <-
      (,) <$> newTVarIO db <*> newTVarIO Set.empty
    return (
        LgrDB {
            varDB          = varDB
          , varPrevApplied = varPrevApplied
          , resolveBlock   = getBlock
          , cfg            = lgrTopLevelConfig
          , diskPolicy     = lgrDiskPolicy
          , hasFS          = lgrHasFS
          , tracer         = lgrTracer
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
  -> Tracer m (TraceReplayEvent blk ())
  -> ImmutableDB m blk
  -> m (LedgerDB' blk, Word64)
initFromDisk LgrDbArgs { lgrHasFS = hasFS, .. }
             replayTracer
             immutableDB = wrapFailure (Proxy @blk) $ do
    (_initLog, db, replayed) <-
      LedgerDB.initLedgerDB
        replayTracer
        lgrTracer
        hasFS
        decodeExtLedgerState'
        decode
        (configLedgerDb lgrTopLevelConfig)
        lgrGenesis
        (streamAPI immutableDB)
    return (db, replayed)
  where
    ccfg = configCodec lgrTopLevelConfig

    decodeExtLedgerState' :: forall s. Decoder s (ExtLedgerState blk)
    decodeExtLedgerState' = decodeExtLedgerState
                              (decodeDisk ccfg)
                              (decodeDisk ccfg)
                              (decodeDisk ccfg)

-- | For testing purposes
mkLgrDB :: StrictTVar m (LedgerDB' blk)
        -> StrictTVar m (Set (RealPoint blk))
        -> (RealPoint blk -> m blk)
        -> LgrDbArgs Identity m blk
        -> LgrDB m blk
mkLgrDB varDB varPrevApplied resolveBlock args = LgrDB {..}
  where
    LgrDbArgs {
        lgrTopLevelConfig = cfg
      , lgrDiskPolicy     = diskPolicy
      , lgrHasFS          = hasFS
      , lgrTracer         = tracer
      } = args

{-------------------------------------------------------------------------------
  TraceReplayEvent decorator
-------------------------------------------------------------------------------}

-- | 'TraceReplayEvent' instantiated with additional information.
--
-- The @replayTo@ parameter is instantiated with the 'Point' of
-- the tip of the ImmutableDB.
type TraceLedgerReplayEvent blk = TraceReplayEvent blk (Point blk)

-- | Add the tip of the Immutable DB to the trace event
--
-- Between the tip of the immutable DB and the point of the starting block,
-- the node could (if it so desired) easily compute a "percentage complete".
decorateReplayTracer
  :: Point blk -- ^ Tip of the ImmutableDB
  -> Tracer m (TraceLedgerReplayEvent blk)
  -> Tracer m (TraceReplayEvent blk ())
decorateReplayTracer immTip = contramap $ fmap (const immTip)

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
             . ledgerTipPoint (Proxy @blk)
             . ledgerState
             . LedgerDB.ledgerDbCurrent

takeSnapshot ::
     forall m blk.
     ( IOLike m
     , LgrDbSerialiseConstraints blk
     , HasHeader blk
     , IsLedger (LedgerState blk)
     )
  => LgrDB m blk -> m (Maybe (DiskSnapshot, RealPoint blk))
takeSnapshot lgrDB@LgrDB{ cfg, tracer, hasFS } = wrapFailure (Proxy @blk) $ do
    ledgerDB <- atomically $ getCurrent lgrDB
    LedgerDB.takeSnapshot
      tracer
      hasFS
      encodeExtLedgerState'
      ledgerDB
  where
    ccfg = configCodec cfg

    encodeExtLedgerState' :: ExtLedgerState blk -> Encoding
    encodeExtLedgerState' = encodeExtLedgerState
                              (encodeDisk ccfg)
                              (encodeDisk ccfg)
                              (encodeDisk ccfg)

trimSnapshots ::
     forall m blk. (MonadCatch m, HasHeader blk)
  => LgrDB m blk
  -> m [DiskSnapshot]
trimSnapshots LgrDB { diskPolicy, tracer, hasFS } = wrapFailure (Proxy @blk) $
    LedgerDB.trimSnapshots tracer hasFS diskPolicy

getDiskPolicy :: LgrDB m blk -> DiskPolicy
getDiskPolicy = diskPolicy

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
         -> [Header blk]
         -> m (ValidateResult blk)
validate LgrDB{..} ledgerDB blockCache numRollbacks = \hdrs -> do
    aps <- mkAps hdrs <$> atomically (readTVar varPrevApplied)
    res <- fmap rewrap $ LedgerDB.defaultResolveWithErrors resolveBlock $
             LedgerDB.ledgerDbSwitch
               (configLedgerDb cfg)
               numRollbacks
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
                         , LedgerDB.ThrowsLedgerError n l blk
                         )]
    mkAps hdrs prevApplied =
      [ case ( Set.member (headerRealPoint hdr) prevApplied
             , BlockCache.lookup (headerHash hdr) blockCache
             ) of
          (False, Nothing)  ->          ApplyRef   (headerRealPoint hdr)
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
streamAPI immutableDB = StreamAPI streamAfter
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

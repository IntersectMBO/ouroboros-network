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
    LgrDB -- opaque
  , LedgerDB
  , LgrDbSerialiseConstraints
    -- * Initialization
  , LgrDbArgs(..)
  , defaultArgs
  , openDB
    -- * 'TraceReplayEvent' decorator
  , TraceLedgerReplayEvent
  , decorateReplayTracer
    -- * Wrappers
  , getCurrent
  , setCurrent
  , getCurrentState
  , getPastState
  , currentPoint
  , takeSnapshot
  , trimSnapshots
  , getDiskPolicy
    -- * Validation
  , validate
  , ValidateResult(..)
    -- * Previously applied blocks
  , getPrevApplied
  , garbageCollectPrevApplied
    -- * Re-exports
  , ExceededRollback(..)
  , LedgerDB.AnnLedgerError(..)
  , LedgerDbParams(..)
  , DiskPolicy (..)
  , DiskSnapshot
  , TraceEvent (..)
  , TraceReplayEvent (..)
  , LedgerDB.ledgerDbCurrent
    -- * Exported for testing purposes
  , mkLgrDB
  ) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import           Codec.Serialise (Serialise (decode, encode))
import           Control.Tracer
import           Data.Bifunctor (second)
import           Data.Foldable (foldl')
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)
import           System.FilePath ((</>))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry

import           Ouroboros.Consensus.Storage.Common
import           Ouroboros.Consensus.Storage.FS.API (SomeHasFS (..),
                     createDirectoryIfMissing)
import           Ouroboros.Consensus.Storage.FS.API.Types (FsError,
                     MountPoint (..), mkFsPath)
import           Ouroboros.Consensus.Storage.FS.IO (ioHasFS)

import           Ouroboros.Consensus.Storage.LedgerDB.DiskPolicy
                     (DiskPolicy (..))
import           Ouroboros.Consensus.Storage.LedgerDB.InMemory (Ap (..),
                     ExceededRollback (..), LedgerDbParams (..))
import qualified Ouroboros.Consensus.Storage.LedgerDB.InMemory as LedgerDB
import           Ouroboros.Consensus.Storage.LedgerDB.OnDisk (DiskSnapshot,
                     NextBlock (..), StreamAPI (..), TraceEvent (..),
                     TraceReplayEvent (..))
import qualified Ouroboros.Consensus.Storage.LedgerDB.OnDisk as LedgerDB

import           Ouroboros.Consensus.Storage.ChainDB.API (ChainDbFailure (..))
import           Ouroboros.Consensus.Storage.ChainDB.Impl.BlockCache
                     (BlockCache)
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.BlockCache as BlockCache
import           Ouroboros.Consensus.Storage.ChainDB.Impl.ImmDB (ImmDB,
                     ImmDbSerialiseConstraints)
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.ImmDB as ImmDB
import           Ouroboros.Consensus.Storage.ChainDB.Serialisation

-- | Thin wrapper around the ledger database
data LgrDB m blk = LgrDB {
      varDB          :: !(StrictTVar m (LedgerDB blk))
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
    , resolveBlock   :: RealPoint blk -> m blk
      -- ^ Read a block from disk
    , cfg            :: !(TopLevelConfig blk)
    , diskPolicy     :: !DiskPolicy
    , hasFS          :: !(SomeHasFS m)
    , tracer         :: !(Tracer m (TraceEvent (RealPoint blk)))
    } deriving (Generic)

deriving instance (IOLike m, LedgerSupportsProtocol blk)
               => NoUnexpectedThunks (LgrDB m blk)
  -- use generic instance

-- | 'EncodeDisk' and 'DecodeDisk' constraints needed for the LgrDB.
class ( -- TODO remove/replace this one once we remove it from the
        -- 'ConsensusProtocol'.
        Serialise      (HeaderHash  blk)
      , EncodeDisk blk (LedgerState blk)
      , DecodeDisk blk (LedgerState blk)
      , EncodeDisk blk (AnnTip      blk)
      , DecodeDisk blk (AnnTip      blk)
      , EncodeDisk blk (ChainDepState (BlockProtocol blk))
      , DecodeDisk blk (ChainDepState (BlockProtocol blk))
      ) => LgrDbSerialiseConstraints blk

-- | Shorter synonym for the instantiated 'LedgerDB.LedgerDB'.
type LedgerDB blk = LedgerDB.LedgerDB (ExtLedgerState blk) (RealPoint blk)

{-------------------------------------------------------------------------------
  Initialization
-------------------------------------------------------------------------------}

data LgrDbArgs m blk = LgrDbArgs {
      lgrTopLevelConfig :: TopLevelConfig blk
    , lgrHasFS          :: SomeHasFS m
    , lgrParams         :: LedgerDbParams
    , lgrDiskPolicy     :: DiskPolicy
    , lgrGenesis        :: m (ExtLedgerState blk)
    , lgrTracer         :: Tracer m (TraceEvent (RealPoint blk))
    , lgrTraceLedger    :: Tracer m (LedgerDB blk)
    }

-- | Default arguments
--
-- The following arguments must still be defined:
--
-- * 'lgrTopLevelConfig'
-- * 'lgrParams'
-- * 'lgrMemPolicy'
-- * 'lgrGenesis'
defaultArgs :: FilePath -> LgrDbArgs IO blk
defaultArgs fp = LgrDbArgs {
      lgrHasFS          = SomeHasFS $ ioHasFS $ MountPoint (fp </> "ledger")
      -- Fields without a default
    , lgrTopLevelConfig = error "no default for lgrTopLevelConfig"
    , lgrParams         = error "no default for lgrParams"
    , lgrDiskPolicy     = error "no default for lgrDiskPolicy"
    , lgrGenesis        = error "no default for lgrGenesis"
    , lgrTracer         = nullTracer
    , lgrTraceLedger    = nullTracer
    }

-- | Open the ledger DB
--
-- In addition to the ledger DB also returns the number of immutable blocks
-- that were replayed.
openDB :: forall m blk.
          ( IOLike m
          , LedgerSupportsProtocol blk
          , LgrDbSerialiseConstraints blk
          , ImmDbSerialiseConstraints blk
          , HasCallStack
          )
       => LgrDbArgs m blk
       -- ^ Stateless initializaton arguments
       -> Tracer m (TraceReplayEvent (RealPoint blk) ())
       -- ^ Used to trace the progress while replaying blocks against the
       -- ledger.
       -> ImmDB m blk
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
openDB args@LgrDbArgs { lgrHasFS = lgrHasFS@(SomeHasFS hasFS), .. } replayTracer immDB getBlock = do
    createDirectoryIfMissing hasFS True (mkFsPath [])
    (db, replayed) <- initFromDisk args replayTracer immDB
    (varDB, varPrevApplied) <-
      (,) <$> newTVarM db <*> newTVarM Set.empty
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
     , ImmDbSerialiseConstraints blk
     , HasCallStack
     )
  => LgrDbArgs m blk
  -> Tracer m (TraceReplayEvent (RealPoint blk) ())
  -> ImmDB     m blk
  -> m (LedgerDB blk, Word64)
initFromDisk LgrDbArgs { lgrHasFS = SomeHasFS hasFS, .. } replayTracer immDB = wrapFailure $ do
    (_initLog, db, replayed) <-
      LedgerDB.initLedgerDB
        replayTracer
        lgrTracer
        hasFS
        decodeExtLedgerState'
        (decodeRealPoint decode)
        lgrParams
        (extLedgerCfgFromTopLevel lgrTopLevelConfig)
        lgrGenesis
        (streamAPI immDB)
    return (db, replayed)
  where
    ccfg = configCodec lgrTopLevelConfig

    decodeExtLedgerState' :: forall s. Decoder s (ExtLedgerState blk)
    decodeExtLedgerState' = decodeExtLedgerState
                              (decodeDisk ccfg)
                              (decodeDisk ccfg)
                              (decodeDisk ccfg)

-- | For testing purposes
mkLgrDB :: StrictTVar m (LedgerDB blk)
        -> StrictTVar m (Set (RealPoint blk))
        -> (RealPoint blk -> m blk)
        -> LgrDbArgs m blk
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
type TraceLedgerReplayEvent blk = TraceReplayEvent (RealPoint blk) (Point blk)

-- | Add the tip of the Immutable DB to the trace event
--
-- Between the tip of the immutable DB and the point of the starting block,
-- the node could (if it so desired) easily compute a "percentage complete".
decorateReplayTracer
  :: Point blk -- ^ Tip of the ImmutableDB
  -> Tracer m (TraceLedgerReplayEvent blk)
  -> Tracer m (TraceReplayEvent (RealPoint blk) ())
decorateReplayTracer immTip = contramap $ fmap (const immTip)

{-------------------------------------------------------------------------------
  Wrappers
-------------------------------------------------------------------------------}

getCurrent :: IOLike m => LgrDB m blk -> STM m (LedgerDB blk)
getCurrent LgrDB{..} = readTVar varDB

getCurrentState :: IOLike m => LgrDB m blk -> STM m (ExtLedgerState blk)
getCurrentState LgrDB{..} = LedgerDB.ledgerDbCurrent <$> readTVar varDB

getPastState :: (IOLike m, HasHeader blk)
             => LgrDB m blk -> Point blk -> m (Maybe (ExtLedgerState blk))
getPastState LgrDB{..} p = do
    db <- atomically $ readTVar varDB
    return $ LedgerDB.ledgerDbPast (pointToWithOriginRealPoint p) db

-- | PRECONDITION: The new 'LedgerDB' must be the result of calling either
-- 'LedgerDB.ledgerDbSwitch' or 'LedgerDB.ledgerDbPushMany' on the current
-- 'LedgerDB'.
setCurrent :: IOLike m => LgrDB m blk -> LedgerDB blk -> STM m ()
setCurrent LgrDB{..} = writeTVar $! varDB

currentPoint :: forall blk. UpdateLedger blk => LedgerDB blk -> Point blk
currentPoint = castPoint
             . ledgerTipPoint (Proxy @blk)
             . ledgerState
             . LedgerDB.ledgerDbCurrent

takeSnapshot :: forall m blk.
                (IOLike m, LgrDbSerialiseConstraints blk)
             => LgrDB m blk -> m (DiskSnapshot, Point blk)
takeSnapshot lgrDB@LgrDB{ cfg, tracer, hasFS = SomeHasFS hasFS } = wrapFailure $ do
    ledgerDB <- atomically $ getCurrent lgrDB
    second withOriginRealPointToPoint <$> LedgerDB.takeSnapshot
      tracer
      hasFS
      encodeExtLedgerState'
      (encodeRealPoint encode)
      ledgerDB
  where
    ccfg = configCodec cfg

    encodeExtLedgerState' :: ExtLedgerState blk -> Encoding
    encodeExtLedgerState' = encodeExtLedgerState
                              (encodeDisk ccfg)
                              (encodeDisk ccfg)
                              (encodeDisk ccfg)

trimSnapshots :: MonadCatch m => LgrDB m blk -> m [DiskSnapshot]
trimSnapshots LgrDB { diskPolicy, tracer, hasFS = SomeHasFS hasFS } = wrapFailure $
    LedgerDB.trimSnapshots tracer hasFS diskPolicy

getDiskPolicy :: LgrDB m blk -> DiskPolicy
getDiskPolicy = diskPolicy

{-------------------------------------------------------------------------------
  Validation
-------------------------------------------------------------------------------}

type AnnLedgerError blk = LedgerDB.AnnLedgerError
                             (ExtLedgerState blk)
                             (RealPoint      blk)

data ValidateResult blk =
    ValidateSuccessful       (LedgerDB       blk)
  | ValidateLedgerError      (AnnLedgerError blk)
  | ValidateExceededRollBack ExceededRollback

validate :: forall m blk. (IOLike m, LedgerSupportsProtocol blk, HasCallStack)
         => LgrDB m blk
         -> LedgerDB blk
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
               (extLedgerCfgFromTopLevel cfg)
               numRollbacks
               aps
               ledgerDB
    atomically $ modifyTVar varPrevApplied $
      addPoints (validBlockPoints res (map headerRealPoint hdrs))
    return res
  where
    rewrap :: Either (AnnLedgerError blk) (Either ExceededRollback (LedgerDB blk))
           -> ValidateResult blk
    rewrap (Left         e)  = ValidateLedgerError      e
    rewrap (Right (Left  e)) = ValidateExceededRollBack e
    rewrap (Right (Right l)) = ValidateSuccessful       l

    mkAps :: forall n r l b. (
               r ~ RealPoint      blk
             , l ~ ExtLedgerState blk
             , b ~                blk
             )
          => [Header blk]
          -> Set r
          -> [Ap n l r b ( LedgerDB.ResolvesBlocks r b n
                         , LedgerDB.ThrowsLedgerError l r n
                         )]
    mkAps hdrs prevApplied =
      [ case ( Set.member (headerRealPoint hdr) prevApplied
             , BlockCache.lookup (headerHash hdr) blockCache
             ) of
          (False, Nothing)  ->          ApplyRef   (headerRealPoint hdr)
          (True,  Nothing)  -> Weaken $ ReapplyRef (headerRealPoint hdr)
          (False, Just blk) -> Weaken $ ApplyVal   (blockRealPoint blk) blk
          (True,  Just blk) -> Weaken $ ReapplyVal (blockRealPoint blk) blk
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

streamAPI
  :: forall m blk.
     (IOLike m, HasHeader blk, ImmDbSerialiseConstraints blk)
  => ImmDB m blk -> StreamAPI m (RealPoint blk) blk
streamAPI immDB = StreamAPI streamAfter
  where
    streamAfter :: HasCallStack
                => WithOrigin (RealPoint blk)
                -> (Maybe (m (NextBlock (RealPoint blk) blk)) -> m a)
                -> m a
    streamAfter tip k = do
      slotNoAtTip <- ImmDB.getSlotNoAtTip immDB
      if pointSlot tip' > slotNoAtTip
        then k Nothing
        else withRegistry $ \registry -> do
          mItr <- ImmDB.streamAfter immDB registry GetBlock tip'
          case mItr of
            Left _err ->
              k Nothing
            Right itr ->
              k . Just . getNext $ itr
      where
        tip' = withOriginRealPointToPoint tip

    getNext :: ImmDB.Iterator (HeaderHash blk) m (m blk)
            -> m (NextBlock (RealPoint blk) blk)
    getNext itr = ImmDB.iteratorNext immDB itr >>= \case
      ImmDB.IteratorExhausted   -> return NoMoreBlocks
      ImmDB.IteratorResult mblk -> (\blk -> NextBlock (blockRealPoint blk, blk)) <$> mblk

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
wrapFailure :: forall m x. MonadCatch m => m x -> m x
wrapFailure k = catch k rethrow
  where
    rethrow :: FsError -> m x
    rethrow err = throwM $ LgrDbFailure err

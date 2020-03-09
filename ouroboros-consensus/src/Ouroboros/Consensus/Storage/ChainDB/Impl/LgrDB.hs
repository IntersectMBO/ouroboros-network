{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DerivingVia               #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeFamilies              #-}

-- | Thin wrapper around the LedgerDB
module Ouroboros.Consensus.Storage.ChainDB.Impl.LgrDB (
    LgrDB -- opaque
  , LedgerDB
    -- * Initialization
  , LgrDbArgs(..)
  , defaultArgs
  , openDB
  , reopen
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
  , ValidateResult
    -- * Garbage collect points of previously applied blocks
  , garbageCollectPrevApplied
    -- * Re-exports
  , LedgerDbParams(..)
  , DiskPolicy (..)
  , DiskSnapshot
  , LedgerDB.PushManyResult (..)
  , LedgerDB.SwitchResult (..)
  , TraceEvent (..)
  , TraceReplayEvent (..)
    -- * Exported for testing purposes
  , LgrDBConf
  , mkLgrDB
  ) where

import           Codec.Serialise.Decoding (Decoder)
import           Codec.Serialise.Encoding (Encoding)
import           Control.Monad.Except (runExcept)
import           Control.Tracer
import           Data.Bifunctor (second)
import           Data.Foldable (foldl')
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)
import           System.FilePath ((</>))

import           Cardano.Prelude (OnlyCheckIsWHNF (..))

import           Ouroboros.Network.Block (HasHeader (..), HeaderHash, Point,
                     SlotNo)
import qualified Ouroboros.Network.Block as Block
import           Ouroboros.Network.Point (WithOrigin)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util ((.:))
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry

import           Ouroboros.Consensus.Storage.Common
import           Ouroboros.Consensus.Storage.FS.API (HasFS,
                     createDirectoryIfMissing)
import           Ouroboros.Consensus.Storage.FS.API.Types (FsError,
                     MountPoint (..), mkFsPath)
import           Ouroboros.Consensus.Storage.FS.IO (ioHasFS)

import           Ouroboros.Consensus.Storage.LedgerDB.Conf
import           Ouroboros.Consensus.Storage.LedgerDB.DiskPolicy
                     (DiskPolicy (..))
import           Ouroboros.Consensus.Storage.LedgerDB.InMemory (Apply (..),
                     LedgerDbParams (..), RefOrVal (..))
import qualified Ouroboros.Consensus.Storage.LedgerDB.InMemory as LedgerDB
import           Ouroboros.Consensus.Storage.LedgerDB.OnDisk (DiskSnapshot,
                     NextBlock (..), StreamAPI (..), TraceEvent (..),
                     TraceReplayEvent (..))
import qualified Ouroboros.Consensus.Storage.LedgerDB.OnDisk as LedgerDB

import           Ouroboros.Consensus.Storage.ChainDB.API (ChainDbFailure (..))
import           Ouroboros.Consensus.Storage.ChainDB.Impl.BlockCache
                     (BlockCache)
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.BlockCache as BlockCache
import           Ouroboros.Consensus.Storage.ChainDB.Impl.ImmDB (ImmDB)
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.ImmDB as ImmDB

-- | Thin wrapper around the ledger database
data LgrDB m blk = LgrDB {
      conf           :: !(LgrDBConf m blk)
    , varDB          :: !(StrictTVar m (LedgerDB blk))
      -- ^ INVARIANT: the tip of the 'LedgerDB' is always in sync with the tip
      -- of the current chain of the ChainDB.
    , varPrevApplied :: !(StrictTVar m (Set (RealPoint blk)))
      -- ^ INVARIANT: this set contains only points that are in the
      -- VolatileDB.
      --
      -- The VolatileDB might contain invalid blocks, these will not be in
      -- this set.
      --
      -- When a garbage-collection is performed on the VolatileDB, the points
      -- of the blocks eligible for garbage-collection should be removed from
      -- this set.
    , args           :: !(LgrDbArgs m blk)
      -- ^ The arguments used to open the 'LgrDB'. Needed for 'reopen'ing the
      -- 'LgrDB'.
    } deriving (Generic)

deriving instance (IOLike m, LedgerSupportsProtocol blk)
               => NoUnexpectedThunks (LgrDB m blk)
  -- use generic instance

-- | Shorter synonym for the instantiated 'LedgerDB.LedgerDB'.
type LedgerDB blk = LedgerDB.LedgerDB (ExtLedgerState blk) (RealPoint blk)

-- | Shorter synonym for the instantiated 'LedgerDbConf'.
type LgrDBConf m blk =
  LedgerDbConf m (ExtLedgerState blk) (RealPoint blk) blk (ExtValidationError blk)

{-------------------------------------------------------------------------------
  Initialization
-------------------------------------------------------------------------------}

data LgrDbArgs m blk = forall h. LgrDbArgs {
      lgrTopLevelConfig       :: TopLevelConfig blk
    , lgrHasFS                :: HasFS m h
    , lgrDecodeLedger         :: forall s. Decoder s (LedgerState                   blk)
    , lgrDecodeHash           :: forall s. Decoder s (HeaderHash                    blk)
    , lgrDecodeTipInfo        :: forall s. Decoder s (TipInfo                       blk)
    , lgrDecodeConsensusState :: forall s. Decoder s (ConsensusState (BlockProtocol blk))
    , lgrEncodeLedger         :: LedgerState                   blk  -> Encoding
    , lgrEncodeHash           :: HeaderHash                    blk  -> Encoding
    , lgrEncodeTipInfo        :: TipInfo                       blk  -> Encoding
    , lgrEncodeConsensusState :: ConsensusState (BlockProtocol blk) -> Encoding
    , lgrParams               :: LedgerDbParams
    , lgrDiskPolicy           :: DiskPolicy
    , lgrGenesis              :: m (ExtLedgerState blk)
    , lgrTracer               :: Tracer m (TraceEvent (RealPoint blk))
    , lgrTraceLedger          :: Tracer m (LedgerDB blk)
    }
  deriving NoUnexpectedThunks via OnlyCheckIsWHNF "LgrDbArgs" (LgrDbArgs m blk)

-- | Default arguments
--
-- The following arguments must still be defined:
--
-- * 'lgrTopLevelConfig'
-- * 'lgrDecodeLedger'
-- * 'lgrDecodeConsensusState'
-- * 'lgrDecodeHash'
-- * 'lgrDecodeTipInfo'
-- * 'lgrEncodeLedger'
-- * 'lgrEncodeConsensusState'
-- * 'lgrEncodeHash'
-- * 'lgrEncodeTipInfo'
-- * 'lgrMemPolicy'
-- * 'lgrGenesis'
defaultArgs :: FilePath -> LgrDbArgs IO blk
defaultArgs fp = LgrDbArgs {
      lgrHasFS            = ioHasFS $ MountPoint (fp </> "ledger")
      -- Fields without a default
    , lgrTopLevelConfig       = error "no default for lgrTopLevelConfig"
    , lgrDecodeLedger         = error "no default for lgrDecodeLedger"
    , lgrDecodeHash           = error "no default for lgrDecodeHash"
    , lgrDecodeTipInfo        = error "no default for lgrDecodeTipInfo"
    , lgrDecodeConsensusState = error "no default for lgrDecodeConsensusState"
    , lgrEncodeLedger         = error "no default for lgrEncodeLedger"
    , lgrEncodeHash           = error "no default for lgrEncodeHash"
    , lgrEncodeTipInfo        = error "no default for lgrEncodeTipInfo"
    , lgrEncodeConsensusState = error "no default for lgrEncodeConsensusState"
    , lgrParams               = error "no default for lgrParams"
    , lgrDiskPolicy           = error "no default for lgrDiskPolicy"
    , lgrGenesis              = error "no default for lgrGenesis"
    , lgrTracer               = nullTracer
    , lgrTraceLedger          = nullTracer
    }

-- | Open the ledger DB
--
-- In addition to the ledger DB also returns the number of immutable blocks
-- that were replayed.
openDB :: forall m blk. (IOLike m, LedgerSupportsProtocol blk)
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
openDB args@LgrDbArgs{..} replayTracer immDB getBlock = do
    createDirectoryIfMissing lgrHasFS True (mkFsPath [])
    (db, replayed) <- initFromDisk args replayTracer lgrDbConf immDB
    (varDB, varPrevApplied) <-
      (,) <$> newTVarM db <*> newTVarM Set.empty
    return (
        LgrDB {
            conf           = lgrDbConf
          , varDB          = varDB
          , varPrevApplied = varPrevApplied
          , args           = args
          }
      , replayed
      )
  where
    apply :: blk
          -> ExtLedgerState blk
          -> Either (ExtValidationError blk) (ExtLedgerState blk)
    apply = runExcept .: applyExtLedgerState
                           BlockNotPreviouslyApplied
                           lgrTopLevelConfig

    reapply :: blk
            -> ExtLedgerState blk
            -> ExtLedgerState blk
    reapply b l = case runExcept (applyExtLedgerState
                                    BlockPreviouslyApplied
                                    lgrTopLevelConfig
                                    b
                                    l) of
      Left  e  -> error $ "reapply failed: " <> show e
      Right l' -> l'

    lgrDbConf :: LgrDBConf m blk
    lgrDbConf = LedgerDbConf {
        ldbConfGenesis = lgrGenesis
      , ldbConfApply   = apply
      , ldbConfReapply = reapply
      , ldbConfResolve = getBlock
      }

-- | Reopen the ledger DB
--
-- Returns the number of immutable blocks replayed.
reopen :: (IOLike m, LedgerSupportsProtocol blk, HasCallStack)
       => LgrDB  m blk
       -> ImmDB  m blk
       -> Tracer m (TraceReplayEvent (RealPoint blk) ())
       -> m Word64
reopen LgrDB{..} immDB replayTracer = do
    (db, replayed) <- initFromDisk args replayTracer conf immDB
    atomically $ writeTVar varDB db
    return replayed

initFromDisk :: forall blk m. (IOLike m, HasHeader blk, HasCallStack)
             => LgrDbArgs m blk
             -> Tracer m (TraceReplayEvent (RealPoint blk) ())
             -> LgrDBConf m blk
             -> ImmDB     m blk
             -> m (LedgerDB blk, Word64)
initFromDisk LgrDbArgs{..} replayTracer lgrDbConf immDB = wrapFailure $ do
    (_initLog, db, replayed) <-
      LedgerDB.initLedgerDB
        replayTracer
        lgrTracer
        lgrHasFS
        decodeExtLedgerState'
        (decodeRealPoint lgrDecodeHash)
        lgrParams
        lgrDbConf
        (streamAPI immDB)
    return (db, replayed)
  where
    decodeExtLedgerState' :: forall s. Decoder s (ExtLedgerState blk)
    decodeExtLedgerState' = decodeExtLedgerState
                              lgrDecodeLedger
                              lgrDecodeConsensusState
                              lgrDecodeHash
                              lgrDecodeTipInfo

-- | For testing purposes
mkLgrDB :: LgrDBConf m blk
        -> StrictTVar m (LedgerDB blk)
        -> StrictTVar m (Set (RealPoint blk))
        -> LgrDbArgs m blk
        -> LgrDB m blk
mkLgrDB conf varDB varPrevApplied args = LgrDB {..}

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

getPastState :: (IOLike m, UpdateLedger blk)
             => LgrDB m blk -> Point blk -> m (Maybe (ExtLedgerState blk))
getPastState LgrDB{..} p =
        LedgerDB.ledgerDbPast conf (pointToWithOriginRealPoint p)
    =<< atomically (readTVar varDB)

-- | PRECONDITION: The new 'LedgerDB' must be the result of calling either
-- 'LedgerDB.ledgerDbSwitch' or 'LedgerDB.ledgerDbPushMany' on the current
-- 'LedgerDB'.
setCurrent :: IOLike m => LgrDB m blk -> LedgerDB blk -> STM m ()
setCurrent LgrDB{..} = writeTVar $! varDB

currentPoint :: UpdateLedger blk => LedgerDB blk -> Point blk
currentPoint = ledgerTipPoint
             . ledgerState
             . LedgerDB.ledgerDbCurrent

takeSnapshot :: IOLike m => LgrDB m blk -> m (DiskSnapshot, Point blk)
takeSnapshot lgrDB@LgrDB{ args = LgrDbArgs{..} } = wrapFailure $ do
    ledgerDB <- atomically $ getCurrent lgrDB
    second withOriginRealPointToPoint <$> LedgerDB.takeSnapshot
      lgrTracer
      lgrHasFS
      encodeExtLedgerState'
      (encodeRealPoint lgrEncodeHash)
      ledgerDB
  where
    encodeExtLedgerState' = encodeExtLedgerState
                              lgrEncodeLedger
                              lgrEncodeConsensusState
                              lgrEncodeHash
                              lgrEncodeTipInfo

trimSnapshots :: MonadCatch m => LgrDB m blk -> m [DiskSnapshot]
trimSnapshots LgrDB{ args = LgrDbArgs{..} } = wrapFailure $
    LedgerDB.trimSnapshots lgrTracer lgrHasFS lgrDiskPolicy

getDiskPolicy :: LgrDB m blk -> DiskPolicy
getDiskPolicy LgrDB{ args = LgrDbArgs{..} } = lgrDiskPolicy

{-------------------------------------------------------------------------------
  Validation
-------------------------------------------------------------------------------}

type ValidateResult blk =
  LedgerDB.SwitchResult (ExtValidationError blk) (ExtLedgerState blk) (RealPoint blk) 'False

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
    blocks <- toBlocks hdrs <$> atomically (readTVar varPrevApplied)
    res <- LedgerDB.ledgerDbSwitch conf numRollbacks blocks ledgerDB
    atomically $ modifyTVar varPrevApplied $
      addPoints (validBlockPoints res (map headerRealPoint hdrs))
    return res
  where
    toBlocks :: [Header blk]
             -> Set (RealPoint blk)
             -> [(Apply 'False, RefOrVal (RealPoint blk) blk)]
    toBlocks hdrs prevApplied =
      [ ( if Set.member (headerRealPoint hdr) prevApplied
          then Reapply else Apply
        , toRefOrVal $ BlockCache.toHeaderOrBlock hdr blockCache)
      | hdr <- hdrs ]

    -- | Based on the 'ValidateResult', return the hashes corresponding to
    -- valid blocks.
    validBlockPoints :: ValidateResult blk
                     -> ([RealPoint blk] -> [RealPoint blk])
    validBlockPoints = \case
      LedgerDB.MaximumRollbackExceeded _ _                              -> const []
      LedgerDB.RollbackSuccessful (LedgerDB.ValidBlocks _)              -> id
      LedgerDB.RollbackSuccessful (LedgerDB.InvalidBlock _ lastValid _) -> takeWhile (/= lastValid)

    addPoints :: [RealPoint blk]
              -> Set (RealPoint blk) -> Set (RealPoint blk)
    addPoints hs set = foldl' (flip Set.insert) set hs

{-------------------------------------------------------------------------------
  Stream API to the immutable DB
-------------------------------------------------------------------------------}

streamAPI :: forall m blk. (IOLike m, HasHeader blk)
          => ImmDB m blk -> StreamAPI m (RealPoint blk) blk
streamAPI immDB = StreamAPI streamAfter
  where
    streamAfter :: HasCallStack
                => WithOrigin (RealPoint blk)
                -> (Maybe (m (NextBlock (RealPoint blk) blk)) -> m a)
                -> m a
    streamAfter tip k = do
      slotNoAtTip <- ImmDB.getSlotNoAtTip immDB
      if Block.pointSlot tip' > slotNoAtTip
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
  Garbage collect points of previously applied blocks
-------------------------------------------------------------------------------}

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

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

toRefOrVal :: (HasHeader blk, HasHeader (Header blk))
           => Either (Header blk) blk -> RefOrVal (RealPoint blk) blk
toRefOrVal (Left  hdr) = Ref (headerRealPoint hdr)
toRefOrVal (Right blk) = Val (blockRealPoint  blk) blk

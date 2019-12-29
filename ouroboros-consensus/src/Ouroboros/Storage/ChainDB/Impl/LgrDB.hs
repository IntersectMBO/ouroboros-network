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
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}

{-# OPTIONS_GHC -Wredundant-constraints #-}
-- | Thin wrapper around the LedgerDB
module Ouroboros.Storage.ChainDB.Impl.LgrDB (
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
  ) where

import           Codec.Serialise.Decoding (Decoder)
import           Codec.Serialise.Encoding (Encoding)
import           Control.Monad.Except (runExcept)
import           Data.Bifunctor (second)
import           Data.Bitraversable (bitraverse)
import           Data.Foldable (foldl')
import           Data.Functor ((<&>))
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)
import           System.FilePath ((</>))

import           Cardano.Prelude (OnlyCheckIsWHNF (..))

import           Control.Monad.Class.MonadThrow
import           Control.Tracer

import           Ouroboros.Network.Block (pattern BlockPoint,
                     pattern GenesisPoint, HasHeader (..), HeaderHash, Point,
                     SlotNo, blockPoint, castPoint)
import qualified Ouroboros.Network.Block as Block
import           Ouroboros.Network.Point (WithOrigin (At))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util ((.:))
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry

import           Ouroboros.Storage.Common
import           Ouroboros.Storage.EpochInfo
import           Ouroboros.Storage.FS.API (HasFS (hasFsErr),
                     createDirectoryIfMissing)
import           Ouroboros.Storage.FS.API.Types (FsError, MountPoint (..),
                     mkFsPath)
import           Ouroboros.Storage.FS.IO (ioHasFS)
import qualified Ouroboros.Storage.Util.ErrorHandling as EH

import           Ouroboros.Storage.LedgerDB.Conf
import           Ouroboros.Storage.LedgerDB.DiskPolicy (DiskPolicy (..))
import           Ouroboros.Storage.LedgerDB.InMemory (Apply (..),
                     LedgerDbParams (..), RefOrVal (..))
import qualified Ouroboros.Storage.LedgerDB.InMemory as LedgerDB
import           Ouroboros.Storage.LedgerDB.OnDisk (DiskSnapshot,
                     NextBlock (..), StreamAPI (..), TraceEvent (..),
                     TraceReplayEvent (..))
import qualified Ouroboros.Storage.LedgerDB.OnDisk as LedgerDB

import           Ouroboros.Storage.ChainDB.API (BlockOrHeader (..),
                     ChainDbFailure (..))
import           Ouroboros.Storage.ChainDB.Impl.ImmDB (ImmDB)
import qualified Ouroboros.Storage.ChainDB.Impl.ImmDB as ImmDB

-- | Thin wrapper around the ledger database
data LgrDB m blk = LgrDB {
      conf           :: !(Conf m blk)
    , varDB          :: !(StrictTVar m (LedgerDB blk))
      -- ^ INVARIANT: the tip of the 'LedgerDB' is always in sync with the tip
      -- of the current chain of the ChainDB.
    , varPrevApplied :: !(StrictTVar m (Set (Point blk)))
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

deriving instance (IOLike m, ProtocolLedgerView blk)
               => NoUnexpectedThunks (LgrDB m blk)
  -- use generic instance

-- | Shorter synonym for the instantiated 'LedgerDB.LedgerDB'.
type LedgerDB blk = LedgerDB.LedgerDB (ExtLedgerState blk) (Point blk)

-- | Shorter synonym for the instantiated 'LedgerDbConf'.
type Conf m blk =
  LedgerDbConf m (ExtLedgerState blk) (Point blk) blk (ExtValidationError blk)

{-------------------------------------------------------------------------------
  Initialization
-------------------------------------------------------------------------------}

data LgrDbArgs m blk = forall h. LgrDbArgs {
      lgrNodeConfig       :: NodeConfig (BlockProtocol blk)
    , lgrHasFS            :: HasFS m h
    , lgrDecodeLedger     :: forall s. Decoder s (LedgerState blk)
    , lgrDecodeChainState :: forall s. Decoder s (ChainState (BlockProtocol blk))
    , lgrDecodeHash       :: forall s. Decoder s (HeaderHash blk)
    , lgrEncodeLedger     :: LedgerState blk                -> Encoding
    , lgrEncodeChainState :: ChainState (BlockProtocol blk) -> Encoding
    , lgrEncodeHash       :: HeaderHash blk                 -> Encoding
    , lgrParams           :: LedgerDbParams
    , lgrDiskPolicy       :: DiskPolicy m
    , lgrGenesis          :: m (ExtLedgerState blk)
    , lgrTracer           :: Tracer m (TraceEvent (Point blk))
    , lgrTraceLedger      :: Tracer m (LedgerDB blk)
    }
  deriving NoUnexpectedThunks via OnlyCheckIsWHNF "LgrDbArgs" (LgrDbArgs m blk)

-- | Default arguments
--
-- The following arguments must still be defined:
--
-- * 'lgrNodeConfig'
-- * 'lgrDecodeLedger'
-- * 'lgrDecodeChainState'
-- * 'lgrDecodeHash'
-- * 'lgrEncodeLedger'
-- * 'lgrEncodeChainState'
-- * 'lgrEncodeHash'
-- * 'lgrMemPolicy'
-- * 'lgrGenesis'
defaultArgs :: FilePath -> LgrDbArgs IO blk
defaultArgs fp = LgrDbArgs {
      lgrHasFS            = ioHasFS $ MountPoint (fp </> "ledger")
      -- Fields without a default
    , lgrNodeConfig       = error "no default for lgrNodeConfig"
    , lgrDecodeLedger     = error "no default for lgrDecodeLedger"
    , lgrDecodeChainState = error "no default for lgrDecodeChainState"
    , lgrDecodeHash       = error "no default for lgrDecodeHash"
    , lgrEncodeLedger     = error "no default for lgrEncodeLedger"
    , lgrEncodeChainState = error "no default for lgrEncodeChainState"
    , lgrEncodeHash       = error "no default for lgrEncodeHash"
    , lgrParams           = error "no default for lgrParams"
    , lgrDiskPolicy       = error "no default for lgrDiskPolicy"
    , lgrGenesis          = error "no default for lgrGenesis"
    , lgrTracer           = nullTracer
    , lgrTraceLedger      = nullTracer
    }

-- | Open the ledger DB
openDB :: forall m blk. (IOLike m, ProtocolLedgerView blk)
       => LgrDbArgs m blk
       -- ^ Stateless initializaton arguments
       -> Tracer m (TraceReplayEvent (Point blk) () (Point blk))
       -- ^ Used to trace the progress while replaying blocks against the
       -- ledger.
       -> ImmDB m blk
       -- ^ Reference to the immutable DB
       --
       -- After reading a snapshot from disk, the ledger DB will be brought
       -- up to date with tip of the immutable DB. The corresponding ledger
       -- state can then be used as the starting point for chain selection in
       -- the ChainDB driver.
       -> (Point blk -> m blk)
       -- ^ Read a block from disk
       --
       -- The block may be in the immutable DB or in the volatile DB; the ledger
       -- DB does not know where the boundary is at any given point.
       -> m (LgrDB m blk)
openDB args@LgrDbArgs{..} replayTracer immDB getBlock = do
    createDirectoryIfMissing lgrHasFS True (mkFsPath [])
    db <- initFromDisk args replayTracer lgrDbConf immDB
    (varDB, varPrevApplied) <-
      (,) <$> newTVarM db <*> newTVarM Set.empty
    return LgrDB {
        conf           = lgrDbConf
      , varDB          = varDB
      , varPrevApplied = varPrevApplied
      , args           = args
      }
  where
    apply :: blk
          -> ExtLedgerState blk
          -> Either (ExtValidationError blk) (ExtLedgerState blk)
    apply = runExcept .: applyExtLedgerState BlockNotPreviouslyApplied lgrNodeConfig

    reapply :: blk
            -> ExtLedgerState blk
            -> ExtLedgerState blk
    reapply b l = case runExcept (applyExtLedgerState BlockPreviouslyApplied lgrNodeConfig b l) of
      Left  e  -> error $ "reapply failed: " <> show e
      Right l' -> l'

    lgrDbConf = LedgerDbConf {
        ldbConfGenesis = lgrGenesis
      , ldbConfApply   = apply
      , ldbConfReapply = reapply
      , ldbConfResolve = getBlock
      }

reopen :: (IOLike m, ProtocolLedgerView blk)
       => LgrDB  m blk
       -> ImmDB  m blk
       -> Tracer m (TraceReplayEvent (Point blk) () (Point blk))
       -> m ()
reopen LgrDB{..} immDB replayTracer = do
    db <- initFromDisk args replayTracer conf immDB
    atomically $ writeTVar varDB db

initFromDisk :: (IOLike m, HasHeader blk)
             => LgrDbArgs m blk
             -> Tracer m (TraceReplayEvent (Point blk) () (Point blk))
             -> Conf      m blk
             -> ImmDB     m blk
             -> m (LedgerDB blk)
initFromDisk args@LgrDbArgs{..} replayTracer lgrDbConf immDB = wrapFailure args $ do
    (_initLog, db) <-
      LedgerDB.initLedgerDB
        replayTracer
        lgrTracer
        lgrHasFS
        (decodeExtLedgerState lgrDecodeLedger lgrDecodeChainState)
        (Block.decodePoint lgrDecodeHash)
        lgrParams
        lgrDbConf
        (streamAPI immDB)
    return db

{-------------------------------------------------------------------------------
  TraceReplayEvent decorator
-------------------------------------------------------------------------------}

-- | 'TraceReplayEvent' instantiated with additional information.
--
-- The @replayTo@ parameter is instantiated with the 'Point' and 'EpochNo' of
-- the tip of the ImmutableDB.
--
-- The @blockInfo@ parameter is instantiated with the 'EpochNo' of the block
-- and the 'SlotNo' of the first slot in the epoch.
type TraceLedgerReplayEvent blk =
  TraceReplayEvent (Point blk) (Point blk, EpochNo) (EpochNo, SlotNo)

decorateReplayTracer
  :: forall m blk. Monad m
  => EpochInfo m
  -> Point blk
     -- ^ Tip of the ImmutableDB
  -> Tracer m (TraceLedgerReplayEvent blk)
  -> m (Tracer m (TraceReplayEvent (Point blk) () (Point blk)))
decorateReplayTracer epochInfo immDbTip tracer = do
    (immDbTipEpoch, _) <- epochNoAndFirstSlot immDbTip
    return $ Tracer $ \ev -> do
      decoratedEv <- bitraverse
        -- Fill in @replayTo@
        (const $ return (immDbTip, immDbTipEpoch))
        -- Fill in @blockInfo@
        epochNoAndFirstSlot
        ev
      traceWith tracer decoratedEv
  where
    epochNoAndFirstSlot :: Point blk -> m (EpochNo, SlotNo)
    epochNoAndFirstSlot GenesisPoint          = return (0, 0)
    epochNoAndFirstSlot BlockPoint { atSlot } = do
      epoch      <- epochInfoEpoch epochInfo atSlot
      epochFirst <- epochInfoFirst epochInfo epoch
      return (epoch, epochFirst)

{-------------------------------------------------------------------------------
  Wrappers
-------------------------------------------------------------------------------}

getCurrent :: IOLike m => LgrDB m blk -> STM m (LedgerDB blk)
getCurrent LgrDB{..} = readTVar varDB

getCurrentState :: IOLike m => LgrDB m blk -> STM m (ExtLedgerState blk)
getCurrentState LgrDB{..} = LedgerDB.ledgerDbCurrent <$> readTVar varDB

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
takeSnapshot lgrDB@LgrDB{ args = args@LgrDbArgs{..} } = wrapFailure args $ do
    ledgerDB <- atomically $ getCurrent lgrDB
    second tipToPoint <$> LedgerDB.takeSnapshot
      lgrTracer
      lgrHasFS
      (encodeExtLedgerState lgrEncodeLedger lgrEncodeChainState)
      (Block.encodePoint lgrEncodeHash)
      ledgerDB

trimSnapshots :: MonadThrow m => LgrDB m blk -> m [DiskSnapshot]
trimSnapshots LgrDB{ args = args@LgrDbArgs{..} } = wrapFailure args $
    LedgerDB.trimSnapshots lgrTracer lgrHasFS lgrDiskPolicy

getDiskPolicy :: LgrDB m blk -> DiskPolicy m
getDiskPolicy LgrDB{ args = LgrDbArgs{..} } = lgrDiskPolicy

{-------------------------------------------------------------------------------
  Validation
-------------------------------------------------------------------------------}

type ValidateResult blk =
  LedgerDB.SwitchResult (ExtValidationError blk) (ExtLedgerState blk) (Point blk) 'False

validate :: forall m blk. (IOLike m, ProtocolLedgerView blk, HasCallStack)
         => LgrDB m blk
         -> LedgerDB blk
            -- ^ This is used as the starting point for validation, not the one
            -- in the 'LgrDB'.
         -> Word64  -- ^ How many blocks to roll back
         -> [Header blk]
         -> m (ValidateResult blk)
validate LgrDB{..} ledgerDB numRollbacks = \hdrs -> do
    blocks <- toBlocks hdrs <$> atomically (readTVar varPrevApplied)
    res <- LedgerDB.ledgerDbSwitch conf numRollbacks blocks ledgerDB
    atomically $ modifyTVar varPrevApplied $
      addPoints (validBlockPoints res (map headerPoint hdrs))
    return res
  where
    toBlocks :: [Header blk] -> Set (Point blk)
             -> [(Apply 'False, RefOrVal (Point blk) blk)]
    toBlocks hdrs prevApplied =
      [ ( if Set.member (headerPoint hdr) prevApplied
          then Reapply else Apply
        , toRefOrVal (Left hdr) )
      | hdr <- hdrs ]

    -- | Based on the 'ValidateResult', return the hashes corresponding to
    -- valid blocks.
    validBlockPoints :: ValidateResult blk
                     -> ([Point blk] -> [Point blk])
    validBlockPoints = \case
      LedgerDB.MaximumRollbackExceeded _ _                              -> const []
      LedgerDB.RollbackSuccessful (LedgerDB.ValidBlocks _)              -> id
      LedgerDB.RollbackSuccessful (LedgerDB.InvalidBlock _ lastValid _) -> takeWhile (/= lastValid)

    addPoints :: [Point blk] -> Set (Point blk)
              -> Set (Point blk)
    addPoints hs set = foldl' (flip Set.insert) set hs

{-------------------------------------------------------------------------------
  Stream API to the immutable DB
-------------------------------------------------------------------------------}

streamAPI :: forall m blk. (IOLike m, HasHeader blk)
          => ImmDB m blk -> StreamAPI m (Point blk) blk
streamAPI immDB = StreamAPI streamAfter
  where
    streamAfter :: Tip (Point blk)
                -> (Maybe (m (NextBlock (Point blk) blk)) -> m a)
                -> m a
    streamAfter tip k = do
      slotNoAtTip <- ImmDB.getSlotNoAtTip immDB
      if Block.pointSlot (tipToPoint tip) > slotNoAtTip
        then k Nothing
        else withRegistry $ \registry ->
          ImmDB.streamAfter immDB registry Block (tipToPoint tip) >>=
            k . Just . getNext . ImmDB.deserialiseIterator immDB

    getNext :: ImmDB.Iterator (HeaderHash blk) m blk
            -> m (NextBlock (Point blk) blk)
    getNext itr = ImmDB.iteratorNext immDB itr <&> \case
      ImmDB.IteratorExhausted    -> NoMoreBlocks
      ImmDB.IteratorResult _ _ blk -> NextBlock (Block.blockPoint blk, blk)
      ImmDB.IteratorEBB    _ _ blk -> NextBlock (Block.blockPoint blk, blk)

{-------------------------------------------------------------------------------
  Garbage collect points of previously applied blocks
-------------------------------------------------------------------------------}

-- | Remove all points with a slot older than the given slot from the set of
-- previously applied points.
garbageCollectPrevApplied :: IOLike m => LgrDB m blk -> SlotNo -> STM m ()
garbageCollectPrevApplied LgrDB{..} slotNo = modifyTVar varPrevApplied $
    Set.dropWhileAntitone ((< (At slotNo)) . Block.pointSlot)

{-------------------------------------------------------------------------------
  Error handling
-------------------------------------------------------------------------------}

-- | Wrap exceptions that may indicate disk failure in a 'ChainDbFailure'
-- exception using the 'LgrDbFailure' constructor.
wrapFailure :: forall m blk x. MonadThrow m => LgrDbArgs m blk -> m x -> m x
wrapFailure LgrDbArgs{ lgrHasFS = hasFS } k =
    EH.catchError (hasFsErr hasFS) k rethrow
  where
    rethrow :: FsError -> m x
    rethrow err = throwM $ LgrDbFailure err

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

toRefOrVal :: (HasHeader blk, HasHeader (Header blk))
           => Either (Header blk) blk -> RefOrVal (Point blk) blk
toRefOrVal (Left  hdr) = Ref (castPoint (blockPoint hdr))
toRefOrVal (Right blk) = Val (blockPoint blk) blk

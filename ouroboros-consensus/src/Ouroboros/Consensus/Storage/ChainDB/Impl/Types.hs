{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | Types used throughout the implementation: handle, state, environment,
-- types, trace types, etc.
module Ouroboros.Consensus.Storage.ChainDB.Impl.Types (
    ChainDbEnv (..)
  , ChainDbHandle (..)
  , ChainDbState (..)
  , SerialiseDiskConstraints
  , getEnv
  , getEnv1
  , getEnv2
  , getEnvSTM
  , getEnvSTM1
    -- * Exposed internals for testing purposes
  , Internal (..)
    -- * Iterator-related
  , IteratorKey (..)
    -- * Follower-related
  , FollowerHandle (..)
  , FollowerKey (..)
  , FollowerRollState (..)
  , FollowerState (..)
  , followerRollStatePoint
    -- * Invalid blocks
  , InvalidBlockInfo (..)
  , InvalidBlocks
    -- * Future blocks
  , FutureBlocks
    -- * Blocks to add
  , BlockToAdd (..)
  , BlocksToAdd
  , addBlockToAdd
  , getBlockToAdd
  , newBlocksToAdd
    -- * Trace types
  , NewTipInfo (..)
  , TraceAddBlockEvent (..)
  , TraceCopyToImmutableDBEvent (..)
  , TraceEvent (..)
  , TraceFollowerEvent (..)
  , TraceGCEvent (..)
  , TraceInitChainSelEvent (..)
  , TraceIteratorEvent (..)
  , TraceOpenEvent (..)
  , TraceValidationEvent (..)
  ) where

import           Control.Tracer
import           Data.Map.Strict (Map)
import           Data.Typeable
import           Data.Void (Void)
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           NoThunks.Class (OnlyCheckWhnfNamed (..))

import           Control.Monad.Class.MonadSTM.Strict (newEmptyTMVarIO)

import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Fragment.Diff (ChainDiff)
import           Ouroboros.Consensus.Fragment.InFuture (CheckInFuture)
import           Ouroboros.Consensus.Ledger.Extended (ExtValidationError)
import           Ouroboros.Consensus.Ledger.Inspect
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Storage.LedgerDB.Types
                     (UpdateLedgerDbTraceEvent)
import           Ouroboros.Consensus.Util.CallStack
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry
import           Ouroboros.Consensus.Util.STM (WithFingerprint)

import           Ouroboros.Consensus.Storage.ChainDB.API (AddBlockPromise (..),
                     ChainDbError (..), InvalidBlockReason, StreamFrom,
                     StreamTo, UnknownRange)
import           Ouroboros.Consensus.Storage.Serialisation

import           Ouroboros.Consensus.Storage.ChainDB.Impl.LgrDB (LedgerDB',
                     LgrDB, LgrDbSerialiseConstraints)
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.LgrDB as LgrDB
import           Ouroboros.Consensus.Storage.ImmutableDB (ImmutableDB,
                     ImmutableDbSerialiseConstraints)
import qualified Ouroboros.Consensus.Storage.ImmutableDB as ImmutableDB
import           Ouroboros.Consensus.Storage.VolatileDB (VolatileDB,
                     VolatileDbSerialiseConstraints)
import qualified Ouroboros.Consensus.Storage.VolatileDB as VolatileDB

-- | All the serialisation related constraints needed by the ChainDB.
class ( ImmutableDbSerialiseConstraints blk
      , LgrDbSerialiseConstraints blk
      , VolatileDbSerialiseConstraints blk
        -- Needed for Follower
      , EncodeDiskDep (NestedCtxt Header) blk
      ) => SerialiseDiskConstraints blk

-- | A handle to the internal ChainDB state
newtype ChainDbHandle m blk = CDBHandle (StrictTVar m (ChainDbState m blk))

-- | Check if the ChainDB is open, if so, executing the given function on the
-- 'ChainDbEnv', otherwise, throw a 'CloseDBError'.
getEnv :: forall m blk r. (IOLike m, HasCallStack, HasHeader blk)
       => ChainDbHandle m blk
       -> (ChainDbEnv m blk -> m r)
       -> m r
getEnv (CDBHandle varState) f = atomically (readTVar varState) >>= \case
    ChainDbOpen env -> f env
    ChainDbClosed   -> throwIO $ ClosedDBError @blk prettyCallStack

-- | Variant 'of 'getEnv' for functions taking one argument.
getEnv1 :: (IOLike m, HasCallStack, HasHeader blk)
        => ChainDbHandle m blk
        -> (ChainDbEnv m blk -> a -> m r)
        -> a -> m r
getEnv1 h f a = getEnv h (\env -> f env a)

-- | Variant 'of 'getEnv' for functions taking two arguments.
getEnv2 :: (IOLike m, HasCallStack, HasHeader blk)
        => ChainDbHandle m blk
        -> (ChainDbEnv m blk -> a -> b -> m r)
        -> a -> b -> m r
getEnv2 h f a b = getEnv h (\env -> f env a b)


-- | Variant of 'getEnv' that works in 'STM'.
getEnvSTM :: forall m blk r. (IOLike m, HasCallStack, HasHeader blk)
          => ChainDbHandle m blk
          -> (ChainDbEnv m blk -> STM m r)
          -> STM m r
getEnvSTM (CDBHandle varState) f = readTVar varState >>= \case
    ChainDbOpen env -> f env
    ChainDbClosed   -> throwSTM $ ClosedDBError @blk prettyCallStack

-- | Variant of 'getEnv1' that works in 'STM'.
getEnvSTM1 ::
     forall m blk a r. (IOLike m, HasCallStack, HasHeader blk)
  => ChainDbHandle m blk
  -> (ChainDbEnv m blk -> a -> STM m r)
  -> a -> STM m r
getEnvSTM1 (CDBHandle varState) f a = readTVar varState >>= \case
    ChainDbOpen env -> f env a
    ChainDbClosed   -> throwSTM $ ClosedDBError @blk prettyCallStack

data ChainDbState m blk
  = ChainDbOpen   !(ChainDbEnv m blk)
  | ChainDbClosed
  deriving (Generic, NoThunks)

data ChainDbEnv m blk = CDB
  { cdbImmutableDB     :: !(ImmutableDB m blk)
  , cdbVolatileDB      :: !(VolatileDB m blk)
  , cdbLgrDB           :: !(LgrDB m blk)
  , cdbChain           :: !(StrictTVar m (AnchoredFragment (Header blk)))
    -- ^ Contains the current chain fragment.
    --
    -- INVARIANT: the anchor point of this fragment is the tip of the
    -- ImmutableDB. This implies that this fragment never contains any blocks
    -- that are stored in the immutable DB.
    --
    -- Note that this fragment might be shorter than @k@ headers when the
    -- whole chain is shorter than @k@ or in case of corruption of the
    -- VolatileDB.
    --
    -- Note that this fragment might also be /longer/ than @k@ headers,
    -- because the oldest blocks from the fragment might not yet have been
    -- copied from the VolatileDB to the ImmutableDB.
    --
    -- The anchor point of this chain should be the most recent \"immutable\"
    -- block according to the protocol, i.e., a block that cannot be rolled
    -- back.
    --
    -- Note that the \"immutable\" block isn't necessarily at the tip of the
    -- ImmutableDB, but could temporarily still be on the in-memory chain
    -- fragment. When the background thread that copies blocks to the
    -- ImmutableDB has caught up, the \"immutable\" block will be at the tip
    -- of the ImmutableDB again.
    --
    -- Note that the \"immutable\" block might be less than @k@ blocks from
    -- our tip in case the whole chain is shorter than @k@ or in case of
    -- corruption of the VolatileDB.
    --
    -- Note that the \"immutable\" block will /never/ be /more/ than @k@
    -- blocks back, as opposed to the anchor point of 'cdbChain'.
  , cdbIterators       :: !(StrictTVar m (Map IteratorKey (m ())))
    -- ^ The iterators.
    --
    -- This maps the 'IteratorKey's of each open 'Iterator' to a function
    -- that, when called, closes the iterator. This is used when closing the
    -- ChainDB: the open file handles used by iterators can be closed, and the
    -- iterators themselves are closed so that it is impossible to use an
    -- iterator after closing the ChainDB itself.
  , cdbFollowers       :: !(StrictTVar m (Map FollowerKey (FollowerHandle m blk)))
    -- ^ The followers.
    --
    -- A follower is open iff its 'FollowerKey' is this 'Map'.
    --
    -- INVARIANT: the 'followerPoint' of each follower is 'withinFragmentBounds'
    -- of the current chain fragment (retrieved 'cdbGetCurrentChain', not by
    -- reading 'cdbChain' directly).
  , cdbTopLevelConfig  :: !(TopLevelConfig blk)
  , cdbInvalid         :: !(StrictTVar m (WithFingerprint (InvalidBlocks blk)))
    -- ^ See the docstring of 'InvalidBlocks'.
    --
    -- The 'Fingerprint' changes every time a hash is added to the map, but
    -- not when hashes are garbage-collected from the map.
  , cdbNextIteratorKey :: !(StrictTVar m IteratorKey)
  , cdbNextFollowerKey :: !(StrictTVar m FollowerKey)
  , cdbCopyLock        :: !(StrictMVar m ())
    -- ^ Lock used to ensure that 'copyToImmutableDB' is not executed more than
    -- once concurrently.
    --
    -- Note that 'copyToImmutableDB' can still be executed concurrently with all
    -- others functions, just not with itself.
  , cdbTracer          :: !(Tracer m (TraceEvent blk))
  , cdbTraceLedger     :: !(Tracer m (LedgerDB' blk))
  , cdbRegistry        :: !(ResourceRegistry m)
    -- ^ Resource registry that will be used to (re)start the background
    -- threads, see 'cdbBgThreads'.
  , cdbGcDelay         :: !DiffTime
    -- ^ How long to wait between copying a block from the VolatileDB to
    -- ImmutableDB and garbage collecting it from the VolatileDB
  , cdbGcInterval      :: !DiffTime
    -- ^ Minimum time between two garbage collections. Is used to batch
    -- garbage collections.
  , cdbKillBgThreads   :: !(StrictTVar m (m ()))
    -- ^ A handle to kill the background threads.
  , cdbChunkInfo       :: !ImmutableDB.ChunkInfo
  , cdbCheckIntegrity  :: !(blk -> Bool)
  , cdbCheckInFuture   :: !(CheckInFuture m blk)
  , cdbBlocksToAdd     :: !(BlocksToAdd m blk)
    -- ^ Queue of blocks that still have to be added.
  , cdbFutureBlocks    :: !(StrictTVar m (FutureBlocks blk))
    -- ^ Blocks from the future
    --
    -- Blocks that were added to the ChainDB but that were from the future
    -- according to 'CheckInFuture', without exceeding the clock skew
    -- ('inFutureExceedsClockSkew'). Blocks exceeding the clock skew are
    -- considered to be invalid ('InFutureExceedsClockSkew') and will be added
    -- 'cdbInvalid'.
    --
    -- Whenever a block is added to the ChainDB, we first trigger chain
    -- selection for all the blocks in this map so that blocks no longer from
    -- the future can get adopted. Note that when no blocks are added to the
    -- ChainDB, we will /not/ actively trigger chain selection for the blocks
    -- in this map.
    --
    -- The number of blocks from the future is bounded by the number of
    -- upstream peers multiplied by the max clock skew divided by the slot
    -- length.
  } deriving (Generic)

-- | We include @blk@ in 'showTypeOf' because it helps resolving type families
-- (but avoid including @m@ because we cannot impose @Typeable m@ as a
-- constraint and still have it work with the simulator)
instance (IOLike m, LedgerSupportsProtocol blk)
      => NoThunks (ChainDbEnv m blk) where
    showTypeOf _ = "ChainDbEnv m " ++ show (typeRep (Proxy @blk))

{-------------------------------------------------------------------------------
  Exposed internals for testing purposes
-------------------------------------------------------------------------------}

data Internal m blk = Internal
  { intCopyToImmutableDB     :: m (WithOrigin SlotNo)
    -- ^ Copy the blocks older than @k@ from to the VolatileDB to the
    -- ImmutableDB and update the in-memory chain fragment correspondingly.
    --
    -- The 'SlotNo' of the tip of the ImmutableDB after copying the blocks is
    -- returned. This can be used for a garbage collection on the VolatileDB.
  , intGarbageCollect        :: SlotNo -> m ()
    -- ^ Perform garbage collection for blocks <= the given 'SlotNo'.
  , intUpdateLedgerSnapshots :: m ()
    -- ^ Write a new LedgerDB snapshot to disk and remove the oldest one(s).
  , intAddBlockRunner        :: m Void
    -- ^ Start the loop that adds blocks to the ChainDB retrieved from the
    -- queue populated by 'ChainDB.addBlock'. Execute this loop in a separate
    -- thread.
  , intKillBgThreads         :: StrictTVar m (m ())
    -- ^ A handle to kill the background threads.
  }

{-------------------------------------------------------------------------------
  Iterator-related
-------------------------------------------------------------------------------}

-- | We use this internally to track iterators in a map ('cdbIterators') in
-- the ChainDB state so that we can remove them from the map when the iterator
-- is closed.
--
-- We store them in the map so that the ChainDB can close all open iterators
-- when it is closed itself.
newtype IteratorKey = IteratorKey Word
  deriving stock   (Show)
  deriving newtype (Eq, Ord, Enum, NoThunks)

{-------------------------------------------------------------------------------
  Follower-related
-------------------------------------------------------------------------------}

-- Note: these things are not in the Follower module, because 'TraceEvent'
-- depends on them, 'ChainDbEnv.cdbTracer' depends on 'TraceEvent', and most
-- modules depend on 'ChainDbEnv'. Also, 'ChainDbEnv.cdbFollowers' depends on
-- 'FollowerState'.

-- | We use this internally to track follower in a map ('cdbFollowers') in the
-- ChainDB state so that we can remove them from the map when the follower is
-- closed.
--
-- We store them in the map so that the ChainDB can close all open followers
-- when it is closed itself and to update the followers in case we switch to a
-- different chain.
newtype FollowerKey = FollowerKey Word
  deriving stock   (Show)
  deriving newtype (Eq, Ord, Enum, NoThunks)

-- | Internal handle to a 'Follower' without an explicit @b@ (@blk@, @'Header'
-- blk@, etc.) parameter so 'Follower's with different' @b@s can be stored
-- together in 'cdbFollowers'.
data FollowerHandle m blk = FollowerHandle
  { fhSwitchFork :: Point blk -> AnchoredFragment (Header blk) -> STM m ()
    -- ^ When we have switched to a fork, all open 'Follower's must be notified.
  , fhClose      :: m ()
    -- ^ When closing the ChainDB, we must also close all open 'Follower's, as
    -- they might be holding on to resources.
    --
    -- Call 'fhClose' will release the resources used by the 'Follower'.
    --
    -- NOTE the 'Follower' is not removed from 'cdbFollowers'. (That is done by
    -- 'closeAllFollowers').
  }
  deriving NoThunks via OnlyCheckWhnfNamed "FollowerHandle" (FollowerHandle m blk)

-- | @b@ corresponds to the 'BlockComponent' that is being read.
data FollowerState m blk b
  = FollowerInit
    -- ^ The 'Follower' is in its initial state. Its 'FollowerRollState' is
    -- @'RollBackTo' 'genesisPoint'@.
    --
    -- This is equivalent to having a 'FollowerInImmutableDB' with the same
    -- 'FollowerRollState' and an iterator streaming after genesis. Opening such
    -- an iterator has a cost (index files will have to be read). However, in
    -- most cases, right after opening a Follower, the user of the Follower will try
    -- to move it forward, moving it from genesis to a more recent point on the
    -- chain. So we incur the cost of opening the iterator while not even using
    -- it.
    --
    -- Therefore, we have this extra initial state, that avoids this cost.
    -- When the user doesn't move the Follower forward, an iterator is opened.
  | FollowerInImmutableDB
      !(FollowerRollState blk)
      !(ImmutableDB.Iterator m blk (Point blk, b))
    -- ^ The 'Follower' is reading from the ImmutableDB.
    --
    -- Note that the iterator includes 'Point blk' in addition to @b@, as it
    -- is needed to keep track of where the iterator is.
    --
    -- INVARIANT: for all @FollowerInImmutableDB rollState immIt@: the predecessor
    -- of the next block streamed by @immIt@ must be the block identified by
    -- @followerRollStatePoint rollState@. In other words: the iterator is
    -- positioned /on/ @followerRollStatePoint rollState@.
  | FollowerInMem !(FollowerRollState blk)
    -- ^ The 'Follower' is reading from the in-memory current chain fragment.
  deriving (Generic, NoThunks)

-- | Similar to 'Ouroboros.Network.MockChain.ProducerState.FollowerState'.
data FollowerRollState blk
  = RollBackTo      !(Point blk)
    -- ^ We don't know at which point the user is, but the next message we'll
    -- send is to roll back to this point.
  | RollForwardFrom !(Point blk)
    -- ^ We know that the follower is at this point and the next message we'll
    -- send is to roll forward to the point /after/ this point on our chain.
  deriving (Eq, Show, Generic, NoThunks)

-- | Get the point the 'FollowerRollState' should roll back to or roll forward
-- from.
followerRollStatePoint :: FollowerRollState blk -> Point blk
followerRollStatePoint (RollBackTo      pt) = pt
followerRollStatePoint (RollForwardFrom pt) = pt

{-------------------------------------------------------------------------------
  Invalid blocks
-------------------------------------------------------------------------------}

-- | Hashes corresponding to invalid blocks. This is used to ignore these
-- blocks during chain selection.
type InvalidBlocks blk = Map (HeaderHash blk) (InvalidBlockInfo blk)

-- | In addition to the reason why a block is invalid, the slot number of the
-- block is stored, so that whenever a garbage collection is performed on the
-- VolatileDB for some slot @s@, the hashes older or equal to @s@ can be
-- removed from this map.
data InvalidBlockInfo blk = InvalidBlockInfo
  { invalidBlockReason :: !(InvalidBlockReason blk)
  , invalidBlockSlotNo :: !SlotNo
  } deriving (Eq, Show, Generic, NoThunks)

{-------------------------------------------------------------------------------
  Future blocks
-------------------------------------------------------------------------------}

-- | Blocks from the future for which we still need to trigger chain
-- selection.
--
-- See 'cdbFutureBlocks' for more info.
type FutureBlocks blk = Map (HeaderHash blk) (Header blk)

{-------------------------------------------------------------------------------
  Blocks to add
-------------------------------------------------------------------------------}

-- | FIFO queue used to add blocks asynchronously to the ChainDB. Blocks are
-- read from this queue by a background thread, which processes the blocks
-- synchronously.
newtype BlocksToAdd m blk = BlocksToAdd (TBQueue m (BlockToAdd m blk))
  deriving NoThunks via OnlyCheckWhnfNamed "BlocksToAdd" (BlocksToAdd m blk)

-- | Entry in the 'BlocksToAdd' queue: a block together with the 'TMVar's used
-- to implement 'AddBlockPromise'.
data BlockToAdd m blk = BlockToAdd
  { blockToAdd            :: !blk
  , varBlockWrittenToDisk :: !(StrictTMVar m Bool)
    -- ^ Used for the 'blockWrittenToDisk' field of 'AddBlockPromise'.
  , varBlockProcessed     :: !(StrictTMVar m (Point blk))
    -- ^ Used for the 'blockProcessed' field of 'AddBlockPromise'.
  }

-- | Create a new 'BlocksToAdd' with the given size.
newBlocksToAdd :: IOLike m => Word -> m (BlocksToAdd m blk)
newBlocksToAdd queueSize = BlocksToAdd <$>
    atomically (newTBQueue (fromIntegral queueSize))

-- | Add a block to the 'BlocksToAdd' queue. Can block when the queue is full.
addBlockToAdd
  :: (IOLike m, HasHeader blk)
  => Tracer m (TraceAddBlockEvent blk)
  -> BlocksToAdd m blk
  -> blk
  -> m (AddBlockPromise m blk)
addBlockToAdd tracer (BlocksToAdd queue) blk = do
    varBlockWrittenToDisk <- newEmptyTMVarIO
    varBlockProcessed     <- newEmptyTMVarIO
    let !toAdd = BlockToAdd
          { blockToAdd = blk
          , varBlockWrittenToDisk
          , varBlockProcessed
          }
    queueSize <- atomically $ do
      writeTBQueue  queue toAdd
      lengthTBQueue queue
    traceWith tracer $
      AddedBlockToQueue (blockRealPoint blk) (fromIntegral queueSize)
    return AddBlockPromise
      { blockWrittenToDisk      = readTMVar varBlockWrittenToDisk
      , blockProcessed          = readTMVar varBlockProcessed
      }

-- | Get the oldest block from the 'BlocksToAdd' queue. Can block when the
-- queue is empty.
getBlockToAdd :: IOLike m => BlocksToAdd m blk -> m (BlockToAdd m blk)
getBlockToAdd (BlocksToAdd queue) = atomically $ readTBQueue queue

{-------------------------------------------------------------------------------
  Trace types
-------------------------------------------------------------------------------}

-- | Trace type for the various events of the ChainDB.
data TraceEvent blk
  = TraceAddBlockEvent          (TraceAddBlockEvent           blk)
  | TraceFollowerEvent          (TraceFollowerEvent           blk)
  | TraceCopyToImmutableDBEvent (TraceCopyToImmutableDBEvent  blk)
  | TraceGCEvent                (TraceGCEvent                 blk)
  | TraceInitChainSelEvent      (TraceInitChainSelEvent       blk)
  | TraceOpenEvent              (TraceOpenEvent               blk)
  | TraceIteratorEvent          (TraceIteratorEvent           blk)
  | TraceLedgerEvent            (LgrDB.TraceEvent             blk)
  | TraceLedgerReplayEvent      (LgrDB.TraceReplayEvent       blk)
  | TraceImmutableDBEvent       (ImmutableDB.TraceEvent       blk)
  | TraceVolatileDBEvent        (VolatileDB.TraceEvent        blk)
  deriving (Generic)

deriving instance
  ( HasHeader blk
  , Eq (Header blk)
  , LedgerSupportsProtocol blk
  , InspectLedger blk
  ) => Eq (TraceEvent blk)
deriving instance
  ( HasHeader blk
  , Show (Header blk)
  , LedgerSupportsProtocol blk
  , InspectLedger blk
  ) => Show (TraceEvent blk)

data TraceOpenEvent blk =
    -- | The ChainDB started the process of opening.
    StartedOpeningDB
    -- | The ChainDB was opened.
  | OpenedDB
      (Point blk)  -- ^ Immutable tip
      (Point blk)  -- ^ Tip of the current chain

    -- | The ChainDB was closed.
  | ClosedDB
      (Point blk)  -- ^ Immutable tip
      (Point blk)  -- ^ Tip of the current chain

    -- | The ImmutableDB started the process of opening.
  | StartedOpeningImmutableDB

    -- | The ImmutableDB was opened.
  | OpenedImmutableDB
      (Point blk)          -- ^ Immutable tip
      ImmutableDB.ChunkNo  -- ^ Chunk number of the immutable tip

    -- | The VolatileDB started opening.
  | StartedOpeningVolatileDB

    -- | The VolatileDB was opened.
  | OpenedVolatileDB

    -- | The LedgerDB started opening.
  | StartedOpeningLgrDB

    -- | The LedgerDB was opened.
  | OpenedLgrDB
  deriving (Generic, Eq, Show)

-- | Information about the new tip of the current chain.
--
-- NOTE: the fields of this record are intentionally lazy to prevent the
-- forcing of this information in case it doesn't have to be traced. However,
-- this means that the tracer processing this message /must not/ hold on to
-- it, otherwise it leaks memory.
data NewTipInfo blk = NewTipInfo {
      newTipPoint       :: RealPoint blk
      -- ^ The new tip of the current chain.
    , newTipEpoch       :: EpochNo
      -- ^ The epoch of the new tip.
    , newTipSlotInEpoch :: Word64
      -- ^ The slot in the epoch, i.e., the relative slot number, of the new
      -- tip.
    , newTipTrigger     :: RealPoint blk
      -- ^ The new tip of the current chain ('newTipPoint') is the result of
      -- performing chain selection for a /trigger/ block ('newTipTrigger').
      -- In most cases, we add a new block to the tip of the current chain, in
      -- which case the new tip /is/ the trigger block.
      --
      -- However, this is not always the case. For example, with our current
      -- chain being A and having a disconnected C lying around, adding B will
      -- result in A -> B -> C as the new chain. The trigger B /= the new tip
      -- C.
    }
  deriving (Eq, Show, Generic)

-- | Trace type for the various events that occur when adding a block.
data TraceAddBlockEvent blk =
    -- | A block with a 'BlockNo' more than @k@ back than the current tip was
    -- ignored.
    IgnoreBlockOlderThanK (RealPoint blk)

    -- | A block that is already in the Volatile DB was ignored.
  | IgnoreBlockAlreadyInVolatileDB (RealPoint blk)

    -- | A block that is know to be invalid was ignored.
  | IgnoreInvalidBlock (RealPoint blk) (InvalidBlockReason blk)

    -- | The block was added to the queue and will be added to the ChainDB by
    -- the background thread. The size of the queue is included.
  | AddedBlockToQueue (RealPoint blk) Word

    -- | The block is from the future, i.e., its slot number is greater than
    -- the current slot (the second argument).
  | BlockInTheFuture (RealPoint blk) SlotNo

    -- | A block was added to the Volatile DB
  | AddedBlockToVolatileDB (RealPoint blk) BlockNo IsEBB

    -- | The block fits onto the current chain, we'll try to use it to extend
    -- our chain.
  | TryAddToCurrentChain (RealPoint blk)

    -- | The block fits onto some fork, we'll try to switch to that fork (if
    -- it is preferable to our chain).
  | TrySwitchToAFork (RealPoint blk) (ChainDiff (HeaderFields blk))

    -- | The block doesn't fit onto any other block, so we store it and ignore
    -- it.
  | StoreButDontChange (RealPoint blk)

    -- | The new block fits onto the current chain (first
    -- fragment) and we have successfully used it to extend our (new) current
    -- chain (second fragment).
  | AddedToCurrentChain
      [LedgerEvent blk]
      (NewTipInfo blk)
      (AnchoredFragment (Header blk))
      (AnchoredFragment (Header blk))

    -- | The new block fits onto some fork and we have switched to that fork
    -- (second fragment), as it is preferable to our (previous) current chain
    -- (first fragment).
  | SwitchedToAFork
      [LedgerEvent blk]
      (NewTipInfo blk)
      (AnchoredFragment (Header blk))
      (AnchoredFragment (Header blk))

    -- | An event traced during validating performed while adding a block.
  | AddBlockValidation (TraceValidationEvent blk)

    -- | Run chain selection for a block that was previously from the future.
    -- This is done for all blocks from the future each time a new block is
    -- added.
  | ChainSelectionForFutureBlock (RealPoint blk)
  deriving (Generic)

deriving instance
  ( HasHeader blk
  , Eq (Header blk)
  , LedgerSupportsProtocol blk
  , InspectLedger blk
  ) => Eq (TraceAddBlockEvent blk)
deriving instance
  ( HasHeader blk
  , Show (Header blk)
  , LedgerSupportsProtocol blk
  , InspectLedger blk
  ) => Show (TraceAddBlockEvent blk)

data TraceValidationEvent blk =
    -- | A point was found to be invalid.
    InvalidBlock
      (ExtValidationError blk)
      (RealPoint blk)

    -- | A candidate chain was invalid.
  | InvalidCandidate
      (AnchoredFragment (Header blk))

    -- | A candidate chain was valid.
  | ValidCandidate (AnchoredFragment (Header blk))

    -- | Candidate contains headers from the future which do no exceed the
    -- clock skew.
  | CandidateContainsFutureBlocks
      (AnchoredFragment (Header blk))
      -- ^ Candidate chain containing headers from the future
      [Header blk]
      -- ^ Headers from the future, not exceeding clock skew

    -- | Candidate contains headers from the future which exceed the
    -- clock skew, making them invalid.
  | CandidateContainsFutureBlocksExceedingClockSkew
      (AnchoredFragment (Header blk))
      -- ^ Candidate chain containing headers from the future
      [Header blk]
      -- ^ Headers from the future, exceeding clock skew
  | UpdateLedgerDbTraceEvent (UpdateLedgerDbTraceEvent blk)
  deriving (Generic)

deriving instance
  ( HasHeader              blk
  , Eq (Header             blk)
  , LedgerSupportsProtocol blk
  ) => Eq (TraceValidationEvent blk)
deriving instance
  ( Show (Header           blk)
  , LedgerSupportsProtocol blk
  ) => Show (TraceValidationEvent blk)

data TraceInitChainSelEvent blk =
    StartedInitChainSelection
    -- ^ An event traced when inital chain selection has started during the
    -- initialization of ChainDB
  | InitalChainSelected
    -- ^ An event traced when inital chain has been selected
  | InitChainSelValidation (TraceValidationEvent blk)
    -- ^ An event traced during validation performed while performing initial
    -- chain selection.
  deriving (Generic)

deriving instance
  ( HasHeader              blk
  , Eq (Header             blk)
  , LedgerSupportsProtocol blk
  ) => Eq (TraceInitChainSelEvent blk)
deriving instance
  ( Show (Header           blk)
  , LedgerSupportsProtocol blk
  ) => Show (TraceInitChainSelEvent blk)


data TraceFollowerEvent blk =
    -- | A new follower was created.
    NewFollower

    -- | The follower was in the 'FollowerInMem' state but its point is no longer on
    -- the in-memory chain fragment, so it has to switch to the
    -- 'FollowerInImmutableDB' state.
  | FollowerNoLongerInMem (FollowerRollState blk)

    -- | The follower was in the 'FollowerInImmutableDB' state and is switched to
    -- the 'FollowerInMem' state.
  | FollowerSwitchToMem
      (Point blk)          -- ^ Point at which the follower is
      (WithOrigin SlotNo)  -- ^ Slot number at the tip of the ImmutableDB

    -- | The follower is in the 'FollowerInImmutableDB' state but the iterator is
    -- exhausted while the ImmutableDB has grown, so we open a new iterator to
    -- stream these blocks too.
  | FollowerNewImmIterator
      (Point blk)          -- ^ Point at which the follower is
      (WithOrigin SlotNo)  -- ^ Slot number at the tip of the ImmutableDB
  deriving (Generic, Eq, Show)


data TraceCopyToImmutableDBEvent blk
  = CopiedBlockToImmutableDB (Point blk)
    -- ^ A block was successfully copied to the ImmutableDB.
  | NoBlocksToCopyToImmutableDB
    -- ^ There are no block to copy to the ImmutableDB.
  deriving (Generic, Eq, Show)

data TraceGCEvent blk
  = ScheduledGC SlotNo Time
    -- ^ A garbage collection for the given 'SlotNo' was scheduled to happen
    -- at the given time.
  | PerformedGC SlotNo
    -- ^ A garbage collection for the given 'SlotNo' was performed.
  deriving (Generic, Eq, Show)

data TraceIteratorEvent blk
    -- | An unknown range was requested, see 'UnknownRange'.
  = UnknownRangeRequested (UnknownRange blk)

    -- | Stream only from the VolatileDB.
  | StreamFromVolatileDB
      (StreamFrom blk)
      (StreamTo   blk)
      [RealPoint  blk]

    -- | Stream only from the ImmutableDB.
  | StreamFromImmutableDB
      (StreamFrom blk)
      (StreamTo   blk)

    -- | Stream from both the VolatileDB and the ImmutableDB.
  | StreamFromBoth
      (StreamFrom blk)
      (StreamTo   blk)
      [RealPoint  blk]

    -- | A block is no longer in the VolatileDB because it has been garbage
    -- collected. It might now be in the ImmutableDB if it was part of the
    -- current chain.
  | BlockMissingFromVolatileDB (RealPoint blk)

    -- | A block that has been garbage collected from the VolatileDB is now
    -- found and streamed from the ImmutableDB.
  | BlockWasCopiedToImmutableDB (RealPoint blk)

    -- | A block is no longer in the VolatileDB and isn't in the ImmutableDB
    -- either; it wasn't part of the current chain.
  | BlockGCedFromVolatileDB    (RealPoint blk)

    -- | We have streamed one or more blocks from the ImmutableDB that were part
    -- of the VolatileDB when initialising the iterator. Now, we have to look
    -- back in the VolatileDB again because the ImmutableDB doesn't have the
    -- next block we're looking for.
  | SwitchBackToVolatileDB
  deriving (Generic, Eq, Show)

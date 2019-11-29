{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wredundant-constraints #-}
-- | Background tasks:
--
-- * Copying blocks from the VolatileDB to the ImmutableDB
-- * Performing and scheduling garbage collections on the VolatileDB
-- * Writing snapshots of the LedgerDB to disk and deleting old ones
-- * Executing scheduled chain selections
module Ouroboros.Storage.ChainDB.Impl.Background
  ( -- * Launch background tasks
    launchBgTasks
    -- * Copying blocks from the VolatileDB to the ImmutableDB
  , copyToImmDB
  , copyToImmDBRunner
     -- * Executing garbage collection
  , garbageCollect
    -- * Scheduling garbage collections
  , GcSchedule
  , newGcSchedule
  , scheduleGC
  , gcScheduleRunner
    -- * Taking and trimming ledger snapshots
  , updateLedgerSnapshots
  , updateLedgerSnapshotsRunner
    -- * Executing scheduled chain selections
  , scheduledChainSelection
  , scheduledChainSelectionRunner
  ) where

import           Control.Exception (assert)
import           Control.Monad (forM_, forever, void)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           GHC.Stack (HasCallStack)

import           Control.Monad.Class.MonadThrow
import           Control.Tracer

import           Ouroboros.Network.AnchoredFragment (AnchoredFragment (..))
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (ChainHash (..), HasHeader, Point,
                     SlotNo, pointHash, pointSlot)
import           Ouroboros.Network.Point (WithOrigin (..))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime (onSlotChange)
import           Ouroboros.Consensus.Ledger.Abstract (ProtocolLedgerView)
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util (whenJust)
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry

import           Ouroboros.Storage.ChainDB.Impl.ChainSel
                     (chainSelectionForBlock)
import qualified Ouroboros.Storage.ChainDB.Impl.ImmDB as ImmDB
import qualified Ouroboros.Storage.ChainDB.Impl.LgrDB as LgrDB
import           Ouroboros.Storage.ChainDB.Impl.Types
import qualified Ouroboros.Storage.ChainDB.Impl.VolDB as VolDB

{-------------------------------------------------------------------------------
  Launch background tasks
-------------------------------------------------------------------------------}

launchBgTasks
  :: forall m blk. (IOLike m, ProtocolLedgerView blk)
  => ChainDbEnv m blk
  -> m ()
launchBgTasks cdb@CDB{..} = do
    gcSchedule <- newGcSchedule
    !gcThread   <- launch $
      gcScheduleRunner gcSchedule $ garbageCollect cdb
    !copyThread <- launch $
      copyToImmDBRunner cdb gcSchedule
    !lgrSnapshotThread <- launch $
      updateLedgerSnapshotsRunner cdb
    !chainSyncThread <- launch $
      scheduledChainSelectionRunner cdb
    atomically $ writeTVar cdbBgThreads
      [gcThread, copyThread, lgrSnapshotThread, chainSyncThread]
  where
    launch :: m () -> m (Thread m ())
    launch = forkLinkedThread cdbRegistry

{-------------------------------------------------------------------------------
  Copying blocks from the VolatileDB to the ImmutableDB
-------------------------------------------------------------------------------}

-- | Copy the blocks older than @k@ from the VolatileDB to the ImmutableDB.
--
-- These headers of these blocks can be retrieved by dropping the @k@ most
-- recent blocks from the fragment stored in 'cdbChain'.
--
-- The copied blocks are removed from the fragment stored in 'cdbChain'.
--
-- This function does not remove blocks from the VolatileDB.
--
-- The 'SlotNo' of the tip of the ImmutableDB after copying the blocks is
-- returned. This can be used for a garbage collection on the VolatileDB.
--
-- NOTE: this function would not be safe when called multiple times
-- concurrently. To enforce thread-safety, a lock is obtained at the start of
-- this function and released at the end. So in practice, this function can be
-- called multiple times concurrently, but the calls will be serialised.
--
-- NOTE: this function /can/ run concurrently with all other functions, just
-- not with itself.
copyToImmDB
  :: forall m blk.
     ( IOLike m
     , OuroborosTag (BlockProtocol blk)
     , HasHeader blk
     , HasHeader (Header blk)
     , HasCallStack
     )
  => ChainDbEnv m blk
  -> m (WithOrigin SlotNo)
copyToImmDB CDB{..} = withCopyLock $ do
    toCopy <- atomically $ do
      curChain <- readTVar cdbChain
      let nbToCopy = max 0 (AF.length curChain - fromIntegral k)
          toCopy :: [Point blk]
          toCopy = map headerPoint
                 $ AF.toOldestFirst
                 $ AF.takeOldest nbToCopy curChain
      return toCopy

    if null toCopy
      -- This can't happen in practice, as we're only called when the fragment
      -- is longer than @k@. However, in the tests, we will be calling this
      -- function manually, which means it might be called when there are no
      -- blocks to copy.
      then trace NoBlocksToCopyToImmDB
      else forM_ toCopy $ \pt -> do
        let hash = case pointHash pt of
              BlockHash h -> h
              -- There is no actual genesis block that can occur on a chain
              GenesisHash -> error "genesis block on current chain"
        -- This call is cheap
        slotNoAtImmDBTip <- ImmDB.getSlotNoAtTip cdbImmDB
        assert (pointSlot pt >= slotNoAtImmDBTip) $ return ()
        blk <- VolDB.getKnownBlock cdbVolDB hash
        -- We're the only one modifying the ImmutableDB, so the tip cannot
        -- have changed since we last checked it.
        ImmDB.appendBlock cdbImmDB blk
        -- TODO the invariant of 'cdbChain' is shortly violated between
        -- these two lines: the tip was updated on the line above, but the
        -- anchor point is only updated on the line below.
        atomically $ removeFromChain pt
        trace $ CopiedBlockToImmDB pt

    -- Get the /possibly/ updated tip of the ImmDB
    ImmDB.getSlotNoAtTip cdbImmDB
  where
    SecurityParam k = protocolSecurityParam cdbNodeConfig
    trace = traceWith (contramap TraceCopyToImmDBEvent cdbTracer)

    -- | Remove the header corresponding to the given point from the beginning
    -- of the current chain fragment.
    --
    -- PRECONDITION: the header must be the first one (oldest) in the chain
    removeFromChain :: Point blk -> STM m ()
    removeFromChain pt = do
      -- The chain might have been extended in the meantime.
      curChain <- readTVar cdbChain
      case curChain of
        hdr :< curChain'
          | headerPoint hdr == pt
          -> writeTVar cdbChain curChain'
        -- We're the only one removing things from 'curChain', so this cannot
        -- happen if the precondition was satisfied.
        _ -> error "header to remove not on the current chain"

    withCopyLock :: forall a. HasCallStack => m a -> m a
    withCopyLock = bracket_
      (fmap mustBeUnlocked $ tryTakeMVar cdbCopyLock)
      (putMVar  cdbCopyLock ())

    mustBeUnlocked :: forall b. HasCallStack => Maybe b -> b
    mustBeUnlocked = fromMaybe
                   $ error "copyToImmDB running concurrently with itself"

-- | Watches the current chain for changes. Whenever the chain is longer than
-- @k@, then the headers older than @k@ are copied from the VolatileDB to the
-- ImmutableDB (with 'copyToImmDB'). Afterwards, a garbage collection of the
-- VolatileDB is scheduled using ('scheduleGC') for the 'SlotNo' of the most
-- recent block that was copied.
copyToImmDBRunner
  :: forall m blk.
     ( IOLike m
     , OuroborosTag (BlockProtocol blk)
     , HasHeader blk
     , HasHeader (Header blk)
     )
  => ChainDbEnv m blk
  -> GcSchedule m
  -> m ()
copyToImmDBRunner cdb@CDB{..} gcSchedule = forever $ do
    atomically $ do
      curChain <- readTVar cdbChain
      check $ fromIntegral (AF.length curChain) > k

    mSlotNo <- copyToImmDB cdb
    case mSlotNo of
      Origin    -> pure ()
      At slotNo -> scheduleGC (contramap TraceGCEvent cdbTracer) slotNo cdbGcDelay gcSchedule
  where
    SecurityParam k = protocolSecurityParam cdbNodeConfig

{-------------------------------------------------------------------------------
  Executing garbage collection
-------------------------------------------------------------------------------}

-- | Trigger a garbage collection for blocks older or equal to the given
-- 'SlotNo' on the VolatileDB.
--
-- Also removes the corresponding cached "previously applied points" from the
-- LedgerDB.
--
-- This is thread-safe as the VolatileDB locks itself while performing a GC.
--
-- TODO will a long GC be a bottleneck? It will block any other calls to
-- @putBlock@ and @getBlock@.
garbageCollect :: forall m blk. IOLike m => ChainDbEnv m blk -> SlotNo -> m ()
garbageCollect CDB{..} slotNo = do
    VolDB.garbageCollect cdbVolDB slotNo
    atomically $ do
      LgrDB.garbageCollectPrevApplied cdbLgrDB slotNo
      modifyTVar cdbInvalid $ fmap $ Map.filter ((> slotNo) . snd)
    traceWith cdbTracer $ TraceGCEvent $ PerformedGC slotNo

{-------------------------------------------------------------------------------
  Scheduling garbage collections
-------------------------------------------------------------------------------}

-- | A scheduled garbage collections.
--
-- Represented as a queue.
--
-- The background thread will continuously try to pop the first element of the
-- queue. Whenever it manages to pop a @('Time', 'SlotNo')@ off: it first
-- waits until the 'Time' is passed (using 'threadDelay' with the given 'Time'
-- minus the current 'Time') and then performs a garbage collection with the
-- given 'SlotNo'.
--
-- The 'Time's in the queue should be monotonically increasing. A fictional
-- example (with hh:mm:ss):
--
-- > [(16:01:12, SlotNo 1012), (16:04:38, SlotNo 1045), ..]
--
-- By using 'scheduleGC', monotonicity is practically guaranteed for standard
-- usage. Theoretically, multiple concurrent calls to 'scheduleGC' might
-- violate it, but this won't happen in practice, as it would mean that
-- multiple calls to 'copyToImmDB' (which cannot run concurrent with itself)
-- have finished very short after each other. Even then, the differences will
-- be ignorable (<1s), and actually won't matter at all, as garbage collection
-- timing doesn't have to be precise. In fact, it is exactly the goal to
-- introduce a delay between copying blocks from the ImmutableDB to the
-- VolatileDB and garbage-collecting them from the VolatileDB.
--
-- If a 'Time' @t@ in the queue /is/ earlier than the 'Time' @t'@ before it in
-- the queue, then the GC scheduled for @t@ will simply be executed at @t'@.
--
-- All this combined means that a garbage collection scheduled at 'Time' @t@
-- will be performed at @t@ /or/ later (but never earlier), with no hard
-- guarantees about how much later.
--
-- How long will this queue be in practice?
--
-- If we say a block is produced every 20 seconds, then a new GC is scheduled
-- every 20 seconds. Ignoring start-up, the queue will then be @delay / 20s@
-- entries long. For example, if the delay is 1h, then the queue will be 180
-- entries long, which is acceptable.
--
-- In Praos, the slot length decreases, but the number of blocks produced (per
-- time) will remain the same as the density decreases too (more slots, but
-- many of them empty), hence the queue queue length will also remain the
-- same.
newtype GcSchedule m = GcSchedule (TQueue m (Time, SlotNo))

newGcSchedule :: IOLike m => m (GcSchedule m)
newGcSchedule = GcSchedule <$> atomically newTQueue

scheduleGC
  :: forall m blk. IOLike m
  => Tracer m (TraceGCEvent blk)
  -> SlotNo    -- ^ The slot to use for garbage collection
  -> DiffTime  -- ^ How long to wait until performing the GC
  -> GcSchedule m
  -> m ()
scheduleGC tracer slotNo delay (GcSchedule queue) = do
    timeScheduledForGC <- addTime delay <$> getMonotonicTime
    atomically $ writeTQueue queue (timeScheduledForGC, slotNo)
    traceWith tracer $ ScheduledGC slotNo delay

gcScheduleRunner
  :: forall m. IOLike m
  => GcSchedule m
  -> (SlotNo -> m ())  -- ^ GC function
  -> m ()
gcScheduleRunner (GcSchedule queue) runGc = forever $ do
    (timeScheduledForGC, slotNo) <- atomically $ readTQueue queue
    currentTime <- getMonotonicTime
    let toWait = max 0 (timeScheduledForGC `diffTime` currentTime)
    threadDelay toWait
    -- Garbage collection is called synchronously
    runGc slotNo

{-------------------------------------------------------------------------------
  Taking and trimming ledger snapshots
-------------------------------------------------------------------------------}

-- | Write a snapshot of the LedgerDB to disk and remove old snapshots
-- (typically one) so that only 'onDiskNumSnapshots' snapshots are on disk.
updateLedgerSnapshots :: IOLike m => ChainDbEnv m blk -> m ()
updateLedgerSnapshots CDB{..} = do
    -- TODO avoid taking multiple snapshots corresponding to the same tip.
    void $ LgrDB.takeSnapshot  cdbLgrDB
    void $ LgrDB.trimSnapshots cdbLgrDB

-- | Execute 'updateLedgerSnapshots', wait 'onDiskWriteInterval', and repeat.
updateLedgerSnapshotsRunner :: IOLike m => ChainDbEnv m blk -> m ()
updateLedgerSnapshotsRunner cdb@CDB{..} = loop
  where
    LgrDB.DiskPolicy{..} = LgrDB.getDiskPolicy cdbLgrDB

    loop = updateLedgerSnapshots cdb >> waitInterval >> loop

    waitInterval = do
      interval <- atomically onDiskWriteInterval
      threadDelay interval

{-------------------------------------------------------------------------------
  Executing scheduled chain selections
-------------------------------------------------------------------------------}

-- | Retrieve the blocks from 'cdbFutureBlocks' for which chain selection was
-- scheduled at the current slot. Run chain selection for each of them.
scheduledChainSelection
  :: (IOLike m, ProtocolLedgerView blk, HasCallStack)
  => ChainDbEnv m blk
  -> SlotNo  -- ^ The current slot
  -> m ()
scheduledChainSelection cdb@CDB{..} curSlot = do
    (mbHdrs, remaining)
      <- atomically $ updateTVar cdbFutureBlocks $ \futureBlocks ->
        -- Extract and delete the value stored at @curSlot@
        let (mbHdrs, remaining) = Map.updateLookupWithKey
              (\_ _ -> Nothing) curSlot futureBlocks
        in (remaining, (mbHdrs, remaining))
    -- The list of headers is stored in reverse order so we can easily
    -- prepend. We reverse them here now so we add them in chronological
    -- order, even though this should not matter, as they are all blocks for
    -- the same slot: at most one block per slot can be adopted.
    --
    -- The only exception is an EBB, which shares the slot with a regular
    -- block. Either order of adding them would result in the same chain, but
    -- adding the EBB before the regular block is cheaper, as we can simply
    -- extend the current chain instead of adding a disconnected block first
    -- and then switching to a very short fork.
    whenJust (NE.reverse <$> mbHdrs) $ \hdrs -> do
      let nbScheduled = fromIntegral $ sum $ length <$> Map.elems remaining
      traceWith cdbTracer $
        TraceAddBlockEvent $
        RunningScheduledChainSelection (fmap headerPoint hdrs) curSlot nbScheduled
      -- If an exception occurs during a call to 'chainSelectionForBlock',
      -- then no chain selection will be performed for the blocks after it.
      -- Only real errors that would shut down the ChainDB could be thrown. In
      -- which case, the ChainDB has to be (re)started, triggering a full
      -- chain selection, which would include these blocks. So there is no
      -- risk of "forgetting" to add a block.
      mapM_ (chainSelectionForBlock cdb) hdrs

-- | Whenever the current slot changes, call 'scheduledChainSelection' for the
-- (new) current slot.
scheduledChainSelectionRunner
  :: (IOLike m, ProtocolLedgerView blk, HasCallStack)
  => ChainDbEnv m blk -> m ()
scheduledChainSelectionRunner cdb@CDB{..} =
    onSlotChange cdbBlockchainTime (scheduledChainSelection cdb)

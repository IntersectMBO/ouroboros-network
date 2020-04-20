{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE MultiWayIf           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Operations involving chain selection: the initial chain selection and
-- adding a block.
module Ouroboros.Consensus.Storage.ChainDB.Impl.ChainSel
  ( initialChainSelection
  , addBlockAsync
  , addBlockSync
  , chainSelectionForBlock
    -- * Exported for testing purposes
  , olderThanK
  ) where

import           Control.Exception (assert)
import           Control.Monad.Except
import           Control.Monad.Trans.State.Strict
import           Control.Tracer (Tracer, contramap, traceWith)
import           Data.Foldable (foldl')
import           Data.Function (on)
import           Data.List (sortBy)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe, isJust, mapMaybe)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word (Word64)
import           GHC.Stack (HasCallStack)

import           Cardano.Prelude (forceElemsToWHNF)

import           Ouroboros.Network.AnchoredFragment (Anchor,
                     AnchoredFragment (..))
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (BlockNo, HasHeader (..), HeaderHash,
                     Point, SlotNo, castHash, castPoint, pointHash)
import           Ouroboros.Network.Point (WithOrigin (..))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime (BlockchainTime (..))
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Fragment.Validated (ValidatedFragment)
import qualified Ouroboros.Consensus.Fragment.Validated as VF
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util.AnchoredFragment
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.STM (WithFingerprint (..))

import           Ouroboros.Consensus.Storage.ChainDB.API (AddBlockPromise (..),
                     InvalidBlockReason (..))
import           Ouroboros.Consensus.Storage.ChainDB.Impl.BlockCache
                     (BlockCache)
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.BlockCache as BlockCache
import           Ouroboros.Consensus.Storage.ChainDB.Impl.ImmDB (ImmDB)
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.ImmDB as ImmDB
import           Ouroboros.Consensus.Storage.ChainDB.Impl.LgrDB (LgrDB)
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.LgrDB as LgrDB
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.Query as Query
import           Ouroboros.Consensus.Storage.ChainDB.Impl.Types
import           Ouroboros.Consensus.Storage.ChainDB.Impl.VolDB (VolDB)
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.VolDB as VolDB

-- | Perform the initial chain selection based on the tip of the ImmutableDB
-- and the contents of the VolatileDB.
--
-- Returns the chosen validated chain and corresponding ledger.
--
-- See "## Initialization" in ChainDB.md.
initialChainSelection
  :: forall m blk. (IOLike m, LedgerSupportsProtocol blk)
  => ImmDB m blk
  -> VolDB m blk
  -> LgrDB m blk
  -> Tracer m (TraceEvent blk)
  -> TopLevelConfig blk
  -> StrictTVar m (WithFingerprint (InvalidBlocks blk))
  -> SlotNo -- ^ Current slot
  -> m (ChainAndLedger blk)
initialChainSelection immDB volDB lgrDB tracer cfg varInvalid curSlot = do
    -- We follow the steps from section "## Initialization" in ChainDB.md

    i :: Anchor blk <- ImmDB.getAnchorForTip immDB
    (succsOf, ledger) <- atomically $ do
      invalid <- forgetFingerprint <$> readTVar varInvalid
      (,) <$> (ignoreInvalidSuc volDB invalid <$> VolDB.filterByPredecessor volDB)
          <*> LgrDB.getCurrent lgrDB

    chains <- constructChains i succsOf

    -- We use the empty fragment anchored at @i@ as the current chain (and
    -- ledger) and the default in case there is no better candidate.
    let curChain          = Empty (AF.castAnchor i)
        curChainAndLedger = VF.new curChain ledger

    case NE.nonEmpty (filter (preferAnchoredCandidate cfg curChain) chains) of
      -- If there are no candidates, no chain selection is needed
      Nothing      -> return curChainAndLedger
      Just chains' -> fromMaybe curChainAndLedger <$>
        chainSelection' curChainAndLedger chains'
  where
    -- | Use the VolatileDB to construct all chains starting from the tip of
    -- the ImmutableDB.
    constructChains :: Anchor blk -- ^ Tip of the ImmutableDB, @i@
                    -> (WithOrigin (HeaderHash blk) -> Set (HeaderHash blk))
                    -> m [AnchoredFragment (Header blk)]
    constructChains i succsOf = flip evalStateT Map.empty $
        mapM constructChain suffixesAfterI
      where
        suffixesAfterI :: [NonEmpty (HeaderHash blk)]
        suffixesAfterI = VolDB.candidates succsOf (AF.anchorToPoint i)

        constructChain :: NonEmpty (HeaderHash blk)
                       -> StateT (Map (HeaderHash blk) (Header blk))
                                 m
                                 (AnchoredFragment (Header blk))
        constructChain hashes =
          AF.fromOldestFirst (AF.castAnchor i) .
          takeWhile ((<= curSlot) . blockSlot) <$>
          mapM (getKnownHeaderThroughCache volDB) (NE.toList hashes)

    -- | Perform chain selection (including validation) on the given
    -- candidates.
    --
    -- PRECONDITION: all candidates are anchored at @i@.
    --
    -- PRECONDITION: all candidates must be preferred over the current chain.
    chainSelection' :: HasCallStack
                    => ChainAndLedger blk
                       -- ^ The current chain and ledger, corresponding to
                       -- @i@.
                    -> NonEmpty (AnchoredFragment (Header blk))
                       -- ^ Candidates anchored at @i@
                    -> m (Maybe (ChainAndLedger blk))
    chainSelection' curChainAndLedger candidates =
      assert (all ((LgrDB.currentPoint ledger ==) .
                   castPoint . AF.anchorPoint)
                  candidates) $
      assert (all (preferAnchoredCandidate cfg curChain) candidates) $
      chainSelection
        lgrDB
        (contramap (TraceInitChainSelEvent . InitChainSelValidation) tracer)
        cfg
        varInvalid
        BlockCache.empty
        curChainAndLedger
        (fmap (mkCandidateSuffix 0) candidates)
      where
        curChain = VF.validatedFragment curChainAndLedger
        ledger   = VF.validatedLedger   curChainAndLedger

-- | Add a block to the ChainDB, /asynchronously/.
--
-- This adds a 'BlockToAdd' corresponding to the given block to the
-- 'cdbBlocksToAdd' queue. The entries in that queue are processed using
-- 'addBlockSync', see that function for more information.
--
-- When the queue is full, this function will still block.
--
-- An important advantage of this asynchronous approach over a synchronous
-- approach is that it doesn't have the following disadvantage: when a thread
-- adding a block to the ChainDB is killed, which can happen when
-- disconnecting from the corresponding node, we might have written the block
-- to disk, but not updated the corresponding in-memory state (e.g., that of
-- the VolatileDB), leaving both out of sync.
--
-- With this asynchronous approach, threads adding blocks asynchronously can
-- be killed without worries, the background thread processing the blocks
-- synchronously won't be killed. Only when the whole ChainDB shuts down will
-- that background thread get killed. But since there will be no more
-- in-memory state, it can't get out of sync with the file system state. On
-- the next startup, a correct in-memory state will be reconstructed from the
-- file system state.
addBlockAsync
  :: forall m blk. (IOLike m, HasHeader blk)
  => ChainDbEnv m blk
  -> blk
  -> m (AddBlockPromise m blk)
addBlockAsync CDB { cdbTracer, cdbBlocksToAdd } =
    addBlockToAdd (contramap TraceAddBlockEvent cdbTracer) cdbBlocksToAdd

-- | Add a block to the ChainDB, /synchronously/.
--
-- This is the only operation that actually changes the ChainDB. It will store
-- the block on disk and trigger chain selection, possibly switching to a
-- fork.
--
-- When the slot of the block is > the current slot, a chain selection will be
-- scheduled in the slot of the block.
addBlockSync
  :: forall m blk.
     ( IOLike m
     , HasHeader blk
     , LedgerSupportsProtocol blk
     , HasCallStack
     )
  => ChainDbEnv m blk
  -> BlockToAdd m blk
  -> m ()
addBlockSync cdb@CDB {..} BlockToAdd { blockToAdd = b, .. } = do
    -- No need to be in the same STM transaction
    curSlot <- atomically $ getCurrentSlot cdbBlockchainTime

    (isMember, invalid, curChain) <- atomically $ (,,)
      <$> VolDB.getIsMember               cdbVolDB
      <*> (forgetFingerprint <$> readTVar cdbInvalid)
      <*> Query.getCurrentChain           cdb

    let immBlockNo = AF.anchorBlockNo curChain
        curTip     = castPoint (AF.headPoint curChain)

    -- We follow the steps from section "## Adding a block" in ChainDB.md

    -- ### Ignore
    if
      | olderThanK hdr (cdbIsEBB hdr) immBlockNo -> do
        trace $ IgnoreBlockOlderThanK (blockRealPoint b)
        deliverPromises False curTip

      | isMember (blockHash b) -> do
        trace $ IgnoreBlockAlreadyInVolDB (blockRealPoint b)
        deliverPromises True curTip

      | Just (InvalidBlockInfo reason _) <- Map.lookup (blockHash b) invalid -> do
        trace $ IgnoreInvalidBlock (blockRealPoint b) reason
        deliverPromises False curTip

      -- ### Store but schedule chain selection
      | blockSlot b > curSlot -> do
        VolDB.putBlock cdbVolDB b
        trace $ AddedBlockToVolDB (blockRealPoint b) (blockNo b) (cdbIsEBB hdr)
        atomically $ putTMVar varBlockWrittenToDisk True
        atomically $ putTMVar varBlockProcessed curTip
        -- We'll fill in 'varChainSelectionPerformed' when the scheduled chain
        -- selection is performed.
        trace $ BlockInTheFuture (blockRealPoint b) curSlot
        scheduleChainSelection curSlot (blockSlot b)

      -- The remaining cases
      | otherwise -> do
        VolDB.putBlock cdbVolDB b
        trace $ AddedBlockToVolDB (blockRealPoint b) (blockNo b) (cdbIsEBB hdr)
        atomically $ putTMVar varBlockWrittenToDisk True
        newTip <- chainSelectionForBlock cdb (BlockCache.singleton b) hdr
        atomically $ putTMVar varBlockProcessed newTip
        atomically $ putTMVar varChainSelectionPerformed newTip
  where
    trace :: TraceAddBlockEvent blk -> m ()
    trace = traceWith (contramap TraceAddBlockEvent cdbTracer)

    hdr :: Header blk
    hdr = getHeader b

    -- | Use the given 'Bool' and 'Point' to fill in the 'TMVar's
    -- corresponding to the block's 'AddBlockPromise'.
    deliverPromises :: Bool -> Point blk -> m ()
    deliverPromises writtenToDisk tip = atomically $ do
      putTMVar varBlockWrittenToDisk      writtenToDisk
      putTMVar varBlockProcessed          tip
      putTMVar varChainSelectionPerformed tip

    scheduleChainSelection
      :: SlotNo  -- ^ Current slot number
      -> SlotNo  -- ^ Slot number of the block
      -> m ()
    scheduleChainSelection curSlot slot = do
      nbScheduled <- atomically $ updateTVar cdbFutureBlocks $ \futureBlocks ->
        let futureBlockToAdd = FutureBlockToAdd hdr varChainSelectionPerformed
            futureBlocks' = Map.insertWith strictAppend slot
              (forceElemsToWHNF (futureBlockToAdd NE.:| [])) futureBlocks
            nbScheduled   = fromIntegral $ sum $ length <$> Map.elems futureBlocks
        in (futureBlocks', nbScheduled)
      trace $ ScheduledChainSelection (headerPoint hdr) curSlot nbScheduled

    strictAppend :: (Semigroup (t a), Foldable t) => t a -> t a -> t a
    strictAppend x y = forceElemsToWHNF (x <> y)

-- | Return 'True' when the given header should be ignored when adding it
-- because it is too old, i.e., we wouldn't be able to switch to a chain
-- containing the corresponding block because its block number is more than
-- @k@ blocks or exactly @k@ blocks back.
--
-- Special case: the header corresponds to an EBB which has the same block
-- number as the block @k@ blocks back (the most recent \"immutable\" block).
-- As EBBs share their block number with the block before them, the EBB is not
-- too old in that case and can be adopted as part of our chain.
--
-- This special case can occur, for example, when the VolatileDB is empty
-- (because of corruption). The \"immutable\" block is then also the tip of
-- the chain. If we then try to add the EBB after it, it will have the same
-- block number, so we must allow it.
olderThanK
  :: HasHeader (Header blk)
  => Header blk
     -- ^ Header of the block to add
  -> IsEBB
     -- ^ Whether the block is an EBB or not
  -> WithOrigin BlockNo
     -- ^ The block number of the most recent \"immutable\" block, i.e., the
     -- block @k@ blocks back.
  -> Bool
olderThanK hdr isEBB immBlockNo
    | At bNo == immBlockNo
    , isEBB == IsEBB
    = False
    | otherwise
    = At bNo <= immBlockNo
  where
    bNo = blockNo hdr

-- | Trigger chain selection for the given block.
--
-- PRECONDITION: the block is in the VolatileDB.
--
-- PRECONDITION: the slot of the block <= the current (wall) slot
--
-- The new tip of the current chain is returned.
--
-- = Constructing candidate fragments
--
-- The VolatileDB keeps a \"successors\" map in memory, telling us the hashes
-- of the known successors of any block, but it does not keep /headers/ in
-- memory, which are needed to construct candidate fargments. We try to reuse
-- the headers from the current chain fragment where possible, but it will not
-- contain all needed headers. This means that we will need to read some
-- blocks from disk and extract their headers. Under normal circumstances this
-- does not matter too much; although this will be done every time we add a
-- block, the expected number of headers to read from disk is very small:
--
-- * None if we stay on the current chain and this is just the next block
-- * A handful if we stay on the current chain and the block we just received
--   was a missing block and we already received some of its successors
-- * A handful if we switch to a short fork
--
-- This is expensive only
--
-- * on startup: in this case we need to read at least @k@ blocks from the
--   VolatileDB, and possibly more if there are some other chains in the
--   VolatileDB starting from the tip of the ImmutableDB
-- * when we switch to a distant fork
--
-- This cost is currently deemed acceptable.
chainSelectionForBlock
  :: forall m blk.
     ( IOLike m
     , HasHeader blk
     , LedgerSupportsProtocol blk
     , HasCallStack
     )
  => ChainDbEnv m blk
  -> BlockCache blk
  -> Header blk
  -> m (Point blk)
chainSelectionForBlock cdb@CDB{..} blockCache hdr = do
    curSlot <- atomically $ getCurrentSlot cdbBlockchainTime

    (invalid, succsOf, predecessor, curChain, tipPoint, ledgerDB)
      <- atomically $ (,,,,,)
          <$> (forgetFingerprint <$> readTVar cdbInvalid)
          <*> VolDB.filterByPredecessor cdbVolDB
          <*> VolDB.getPredecessor      cdbVolDB
          <*> Query.getCurrentChain     cdb
          <*> Query.getTipPoint         cdb
          <*> LgrDB.getCurrent          cdbLgrDB
    let curChainAndLedger :: ChainAndLedger blk
        curChainAndLedger =
          -- The current chain we're working with here is not longer than @k@
          -- blocks (see 'getCurrentChain' and 'cdbChain'), which is easier to
          -- reason about when doing chain selection, etc.
          assert (fromIntegral (AF.length curChain) <= k) $
          VF.new curChain ledgerDB

        immBlockNo :: WithOrigin BlockNo
        immBlockNo = AF.anchorBlockNo curChain

        i :: Point blk
        i = castPoint $ AF.anchorPoint curChain

        -- Let these two functions ignore invalid blocks
        predecessor' = ignoreInvalid    cdb invalid predecessor
        succsOf'     = ignoreInvalidSuc cdb invalid succsOf

    -- The preconditions
    assert (blockSlot hdr <= curSlot)  $ return ()
    assert (isJust $ predecessor (headerHash hdr)) $ return ()

    if
      -- The chain might have grown since we added the block such that the
      -- block is older than @k@.
      | olderThanK hdr (cdbIsEBB hdr) immBlockNo -> do
        trace $ IgnoreBlockOlderThanK p
        return tipPoint

      -- We might have validated the block in the meantime
      | Just (InvalidBlockInfo reason _) <- Map.lookup (headerHash hdr) invalid -> do
        trace $ IgnoreInvalidBlock p reason
        return tipPoint

      -- The block @b@ fits onto the end of our current chain
      | pointHash tipPoint == castHash (blockPrevHash hdr) -> do
        -- ### Add to current chain
        trace (TryAddToCurrentChain p)
        addToCurrentChain succsOf' curChainAndLedger curSlot

      | Just hashes <- VolDB.isReachable predecessor' i p -> do
        -- ### Switch to a fork
        trace (TrySwitchToAFork p hashes)
        switchToAFork succsOf' curChainAndLedger hashes curSlot

      | otherwise -> do
        -- ### Store but don't change the current chain
        trace (StoreButDontChange p)
        return tipPoint

    -- Note that we may have extended the chain, but have not trimmed it to
    -- @k@ blocks/headers. That is the job of the background thread, which
    -- will first copy the blocks/headers to trim (from the end of the
    -- fragment) from the VolatileDB to the ImmutableDB.
  where
    SecurityParam k = configSecurityParam cdbTopLevelConfig

    p :: RealPoint blk
    p = headerRealPoint hdr

    trace :: TraceAddBlockEvent blk -> m ()
    trace = traceWith (contramap TraceAddBlockEvent cdbTracer)

    -- | PRECONDITION: the header @hdr@ (and block @b@) fit onto the end of
    -- the current chain.
    addToCurrentChain :: HasCallStack
                      => (WithOrigin (HeaderHash blk) -> Set (HeaderHash blk))
                      -> ChainAndLedger blk
                         -- ^ The current chain and ledger
                      -> SlotNo
                         -- ^ The current slot
                      -> m (Point blk)
    addToCurrentChain succsOf curChainAndLedger
                      curSlot = assert (AF.validExtension curChain hdr) $ do
        let suffixesAfterB = VolDB.candidates succsOf (realPointToPoint p)

        -- Fragments that are anchored at @curHead@, i.e. suffixes of the
        -- current chain.
        candidates <- case NE.nonEmpty suffixesAfterB of
          -- If there are no suffixes after @b@, just use the suffix just
          -- containing @b@ as the sole candidate.
          Nothing              ->
            return $ (AF.fromOldestFirst curHead [hdr]) NE.:| []
          Just suffixesAfterB' ->
            -- We can start with an empty cache, because we're only looking
            -- up the headers /after/ b, so they won't be on the current
            -- chain.
            flip evalStateT Map.empty $ forM suffixesAfterB' $ \hashes -> do
              -- Ignore headers/blocks from the future
              hdrs <- fmap (takeWhile ((<= curSlot) . blockSlot)) $
                mapM (getKnownHeaderThroughCache cdbVolDB) $
                NE.toList hashes
              return $ AF.fromOldestFirst curHead (hdr : hdrs)

        let candidateSuffixes = NE.nonEmpty
              $ NE.filter ( preferAnchoredCandidate cdbTopLevelConfig curChain
                          . csSuffix
                          )
              $ fmap (mkCandidateSuffix 0) candidates
        -- All candidates are longer than the current chain, so they will be
        -- preferred over it, /unless/ the block we just added is an EBB,
        -- which has the same 'BlockNo' as the block before it, so when
        -- using the 'BlockNo' as the proxy for the length (note that some
        -- protocols might do it differently), the candidate with the EBB
        -- appended will not be preferred over the current chain.
        --
        -- The consequence of this is that when adding an EBB, it will not
        -- be selected by chain selection and thus not appended to the chain
        -- until the block after it is added, which will again result in a
        -- candidate preferred over the current chain. In this case, the
        -- candidate will be a two-block (the EBB and the new block)
        -- extension of the current chain.
        case candidateSuffixes of
          Nothing                 -> return curTip
          Just candidateSuffixes' ->
            chainSelection' curChainAndLedger candidateSuffixes' >>= \case
              Nothing                -> return curTip
              Just newChainAndLedger ->
                trySwitchTo newChainAndLedger (AddedToCurrentChain p)
      where
        curChain = VF.validatedFragment curChainAndLedger
        curHead  = AF.headAnchor curChain
        curTip   = castPoint $ AF.headPoint curChain

    -- | We have found a path of hashes to the new block through the
    -- VolatileDB. We try to extend this path by looking for forks that start
    -- with the given block, then we do chain selection and /possibly/ try to
    -- switch to a new fork.
    switchToAFork :: HasCallStack
                  => (WithOrigin (HeaderHash blk) -> Set (HeaderHash blk))
                  -> ChainAndLedger blk
                     -- ^ The current chain (anchored at @i@) and ledger
                  -> NonEmpty (HeaderHash blk)
                     -- ^ An uninterrupted path of hashes @(i,b]@.
                  -> SlotNo
                     -- ^ The current slot
                  -> m (Point blk)
    switchToAFork succsOf curChainAndLedger hashes
                  curSlot = do
        let suffixesAfterB = VolDB.candidates succsOf (realPointToPoint p)
            initCache      = Map.insert (headerHash hdr) hdr (cacheHeaders curChain)
        -- Fragments that are anchored at @i@.
        candidates <- flip evalStateT initCache $
          case NE.nonEmpty suffixesAfterB of
            -- If there are no suffixes after @b@, just use the fragment that
            -- ends in @b@ as the sole candidate.
            Nothing              -> (NE.:| []) <$> constructFork curSlot i hashes []
            Just suffixesAfterB' -> mapM (constructFork curSlot i hashes . NE.toList)
                                         suffixesAfterB'
        let candidateSuffixes =
                -- The suffixes all fork off from the current chain within @k@
                -- blocks, so it satisfies the precondition of
                -- 'preferCandidate'.
                filter (preferAnchoredCandidate cdbTopLevelConfig curChain . csSuffix)
              . mapMaybe (intersectCandidateSuffix curChain)
              $ NE.toList candidates

        case NE.nonEmpty candidateSuffixes of
          -- No candidates preferred over the current chain
          Nothing                 -> return curTip
          Just candidateSuffixes' ->
            chainSelection' curChainAndLedger candidateSuffixes' >>= \case
              Nothing                -> return curTip
              Just newChainAndLedger ->
                trySwitchTo newChainAndLedger (SwitchedToAFork p)
      where
        curChain = VF.validatedFragment curChainAndLedger
        curTip   = castPoint $ AF.headPoint curChain
        i        = AF.castAnchor $ anchor curChain

    -- | 'chainSelection' partially applied to the parameters from the
    -- 'ChainDbEnv'.
    chainSelection'
      :: ChainAndLedger blk              -- ^ The current chain and ledger
      -> NonEmpty (CandidateSuffix blk)  -- ^ Candidates
      -> m (Maybe (ChainAndLedger blk))
    chainSelection' = chainSelection
      cdbLgrDB
      (contramap (TraceAddBlockEvent . AddBlockValidation) cdbTracer)
      cdbTopLevelConfig
      cdbInvalid
      blockCache

    -- | Try to swap the current (chain) fragment with the given candidate
    -- fragment. The 'LgrDB.LedgerDB' is updated in the same transaction.
    --
    -- Note that the current chain might have changed in the meantime. We only
    -- switch when the new chain is preferred over the current chain.
    --
    -- * Remember that we started with the @k@ most recent headers from the
    --   current chain.
    --
    --   - In the common case, the candidate will be one block longer than
    --     @k@, i.e. the new block that was appended to the current chain.
    --   - In some cases, it might be a few blocks longer, because the new
    --     block allowed us to append a few more blocks that we previously got
    --     to the chain.
    --   - In some cases, we have added one or more blocks to it, after
    --     rolling a few back (less or equal to the number we added to it),
    --     i.e. we switch to a short fork.
    --   - In exceptional cases, it will be exactly @k@ long, when we switch
    --     to a fork of the same length.
    --   - In exceptional cases, it will be much longer than @k@, i.e. when we
    --     have added a block that connects a much longer fork to the current
    --     chain.
    --
    -- * The current chain might be longer than @k@ because the background
    --   thread might not have written all blocks yet to the ImmutableDB.
    --
    -- If there's no concurrency, i.e., no concurrent calls to 'addBlock',
    -- then it must be that current chain and the candidate chain intersect
    -- within the last @k@ blocks (of the current chain).
    --
    -- If there is no such intersection, it must be that the current chain has
    -- /concurrently/ changed so much that the fragment is no longer within
    -- the last @k@ blocks of the current chain, and we are no longer
    -- interested in this candidate anymore.
    --
    -- If there is an intersection, we compare the candidate fragment to the
    -- current chain, and, if it is preferred, install it as the current
    -- chain.
    --
    -- In case the current chain was changed in the background, it must either
    -- have been done before we added our block to the VolatileDB or after. If
    -- it was before, we know of the other block. If it was done after, they
    -- must know of our block. In either case, somebody computed candidates
    -- with knowledge of both blocks, so we're safe. See See "### Concurrency"
    -- in ChainDB.md for more details.
    trySwitchTo
      :: HasCallStack
      => ChainAndLedger blk  -- ^ Chain and ledger to switch to
      -> (    AnchoredFragment (Header blk)
           -> AnchoredFragment (Header blk)
           -> TraceAddBlockEvent blk
         )
         -- ^ Given the previous chain and the new chain, return the event
         -- to trace when we switched to the new chain.
      -> m (Point blk)
    trySwitchTo newChainAndLedger mkTraceEvent = do
      (curChain, switched) <- atomically $ do
        curChain <- readTVar cdbChain
        case AF.intersect curChain newChain of
          Just (curChainPrefix, _newChainPrefix, curChainSuffix, newChainSuffix)
            | fromIntegral (AF.length curChainSuffix) <= k
            , preferAnchoredCandidate cdbTopLevelConfig curChain newChain
            -> do
              -- The current chain might still contain some old headers that
              -- have not yet been written to the ImmutableDB. We must make
              -- sure to prepend these headers to the new chain so that we
              -- still know what the full chain is.
              let newChain' = fromMaybe
                    (error "postcondition of intersect violated") $
                    AF.join curChainPrefix newChainSuffix
              writeTVar cdbChain newChain'
              LgrDB.setCurrent cdbLgrDB newLedger

              -- Update the readers
              -- 'Reader.switchFork' needs to know the intersection point
              -- (@ipoint@) between the old and the current chain.
              let ipoint = castPoint $ AF.anchorPoint newChainSuffix
              readerHandles <- Map.elems <$> readTVar cdbReaders
              forM_ readerHandles $ \readerHandle ->
                rhSwitchFork readerHandle ipoint newChain'

              return (curChain, True)
          -- The chain must be changed in the meantime such that our chain is
          -- no longer preferred.
          _ -> return (curChain, False)
      if switched then do
        trace $ mkTraceEvent curChain newChain
        traceWith cdbTraceLedger newLedger
        return $ castPoint $ AF.headPoint newChain
      else do
        trace $ ChainChangedInBg curChain newChain
        return $ castPoint $ AF.headPoint curChain
      where
        newChain  = VF.validatedFragment newChainAndLedger
        newLedger = VF.validatedLedger   newChainAndLedger


    -- | Build a cache from the headers in the fragment.
    cacheHeaders :: AnchoredFragment (Header blk)
                 -> Map (HeaderHash blk) (Header blk)
    cacheHeaders =
      foldl' (\m h -> Map.insert (blockHash h) h m) Map.empty .
      AF.toNewestFirst

    -- | We have a new block @b@ that doesn't fit onto the current chain, but
    -- there is an unbroken path from the tip of the ImmutableDB (@i@ = the
    -- anchor point of the current chain) to @b@. We also have a suffix @s@ of
    -- hashes that starts after @b@.
    --
    -- We will try to construct a fragment @f@ for the fork such that:
    -- * @f@ is anchored at @i@
    -- * @f@ starts with the headers corresponding to the hashes of @(i,b]@
    -- * The next header in @f@ is the header for @b@
    -- * Finally, @f@ ends with the headers corresponding to the hashes
    --   @(b,?]@ of the suffix @s@.
    -- * Any headers from the future are dropped.
    --
    -- Note that we need to read the headers corresponding to the hashes
    -- @(i,b]@ and @(b,?]@ from disk. It is likely that many of these headers
    -- are actually on the current chain, so when possible, we reuse these
    -- headers instead of reading them from disk.
    constructFork
      :: SlotNo                     -- ^ Current slot
      -> Anchor blk                 -- ^ Tip of ImmutableDB @i@
      -> NonEmpty (HeaderHash blk)  -- ^ Hashes of @(i,b]@
      -> [HeaderHash blk]           -- ^ Suffix @s@, hashes of @(b,?]@
      -> StateT (Map (HeaderHash blk) (Header blk))
                m
                (AnchoredFragment (Header blk))
         -- ^ Fork, anchored at @i@, contains (the header of) @b@ and ends
         -- with the suffix @s@.
    constructFork curSlot i hashes suffixHashes
      = fmap (AF.fromOldestFirst (AF.castAnchor i)
      .       takeWhile ((<= curSlot) . blockSlot))
      $ mapM (getKnownHeaderThroughCache cdbVolDB)
      $ NE.toList hashes <> suffixHashes


-- | Check whether the header for the hash is in the cache, if not, get
-- the corresponding header from the VolatileDB and store it in the cache.
--
-- PRECONDITION: the header (block) must exist in the VolatileDB.
getKnownHeaderThroughCache
  :: (MonadCatch m, HasHeader blk)
  => VolDB m blk
  -> HeaderHash blk
  -> StateT (Map (HeaderHash blk) (Header blk)) m (Header blk)
getKnownHeaderThroughCache volDB hash = gets (Map.lookup hash) >>= \case
  Just hdr -> return hdr
  Nothing  -> do
    hdr <- lift $ VolDB.getKnownHeader volDB hash
    modify (Map.insert hash hdr)
    return hdr

-- | Perform chain selection with the given candidates. If a validated
-- candidate was chosen to replace the current chain, return it along with the
-- corresponding ledger.
--
-- PRECONDITION: all candidates must be preferred over the current chain.
--
-- PRECONDITION: the candidate suffixes must fit on the (given) current chain.
chainSelection
  :: forall m blk.
     ( IOLike m
     , LedgerSupportsProtocol blk
     , HasCallStack
     )
  => LgrDB m blk
  -> Tracer m (TraceValidationEvent blk)
  -> TopLevelConfig blk
  -> StrictTVar m (WithFingerprint (InvalidBlocks blk))
  -> BlockCache blk
  -> ChainAndLedger blk              -- ^ The current chain and ledger
  -> NonEmpty (CandidateSuffix blk)  -- ^ Candidates
  -> m (Maybe (ChainAndLedger blk))
     -- ^ The (valid) chain and corresponding LedgerDB that was selected, or
     -- 'Nothing' if there is no valid chain preferred over the current
     -- chain.
chainSelection lgrDB tracer cfg varInvalid blockCache
               curChainAndLedger candidates =
  assert (all (preferAnchoredCandidate cfg curChain . csSuffix) candidates) $
  assert (all (isJust . fitCandidateSuffixOn curChain) candidates) $
    go (sortCandidates (NE.toList candidates))
  where
    curChain = VF.validatedFragment curChainAndLedger

    sortCandidates :: [CandidateSuffix blk] -> [CandidateSuffix blk]
    sortCandidates =
      sortBy (flip (compareAnchoredCandidates cfg) `on` csSuffix)

    validate :: ChainAndLedger  blk  -- ^ Current chain and ledger
             -> CandidateSuffix blk  -- ^ Candidate fragment
             -> m (Maybe (ChainAndLedger blk))
    validate = validateCandidate lgrDB tracer cfg varInvalid blockCache

    -- 1. Take the first candidate from the list of sorted candidates
    -- 2. Validate it
    --    - If it is invalid -> discard it and go to 1 with the rest of the
    --      list.
    --    - If it is valid and has the same tip -> return it
    --    - If it is valid, but is a prefix of the original ->
    --        add it to the list, sort it and go to 1. See the comment
    --        [Ouroboros] below.
    go :: [CandidateSuffix blk] -> m (Maybe (ChainAndLedger blk))
    go []                                           = return Nothing
    go (cand@(CandidateSuffix _ candSuffix):cands') =
      validate curChainAndLedger cand >>= \case
        Nothing -> do
          cands'' <- truncateInvalidCandidates cands'
          go (sortCandidates cands'')
        Just newChainAndLedger
            | validatedHead == AF.headPoint candSuffix
              -- Unchanged candidate
            -> return $ Just newChainAndLedger
            | otherwise
              -- Prefix of the candidate because it contained invalid blocks.
              -- Note that the spec says go back to candidate selection, see
              -- [^whyGoBack], because there might still be some candidates
              -- that contain the same invalid block. To simplify the control
              -- flow, We do it differently: instead of recomputing the
              -- candidates taking invalid blocks into account, we just
              -- truncate the remaining candidates that contain invalid
              -- blocks.
            -> do
              cands'' <- truncateInvalidCandidates cands'
              go (sortCandidates (candSuffix':cands''))
          where
            candChain     = VF.validatedFragment newChainAndLedger
            validatedHead = AF.headPoint candChain
            candSuffix'   = rollbackCandidateSuffix
              (castPoint validatedHead) cand

    -- | Truncate the given (remaining) candidates that contain invalid
    -- blocks. Discard them if they are truncated so much that they are no
    -- longer preferred over the current chain.
    truncateInvalidCandidates :: [CandidateSuffix blk]
                              -> m [CandidateSuffix blk]
    truncateInvalidCandidates cands = do
      isInvalid <- flip Map.member . forgetFingerprint <$>
        atomically (readTVar varInvalid)
      return $ filter (preferAnchoredCandidate cfg curChain . csSuffix)
             $ mapMaybe (truncateInvalidCandidate isInvalid) cands

    -- [Ouroboros]
    --
    -- Ouroboros says that when we are given an invalid chain by a peer, we
    -- should reject that peer's chain. However, since we're throwing all
    -- blocks together in the ChainDB, we can't tell which block or which
    -- chain came from which peer, so we can't simply reject a peer's chain.
    --
    -- It might be that a good peer gave us a valid chain, but another peer
    -- gave us an invalid block that fits onto the chain of the good peer. In
    -- that case, we do still want to adopt the chain of the good peer, which
    -- is a prefix of the chain that we constructed using all the blocks we
    -- found in the VolatileDB, including the invalid block.
    --
    -- This is the reason why we still take valid prefixes of a invalid chains
    -- into account during chain selection: they might correspond to the good
    -- peer's valid chain.

-- | Validate a candidate and return a 'ChainAndLedger' for it, i.e. a
-- validated fragment along with a ledger corresponding to its tip (most
-- recent block).
--
-- PRECONDITION: the candidate suffix must fit onto the given current chain.
--
-- If all blocks in the fragment are valid, then the fragment in the returned
-- 'ChainAndLedger' is the same as the given candidate suffix fitted on top of
-- the current chain ('fitCandidateSuffixOn').
--
-- If a block in the fragment is invalid, then the fragment in the returned
-- 'ChainAndLedger' is a prefix of the given candidate fragment (upto the last
-- valid block), if that fragment is still preferred ('preferCandidate') over
-- the current chain, if not, 'Nothing' is returned.
--
-- Returns 'Nothing' if this candidate requires a rollback we cannot support.
validateCandidate
  :: forall m blk.
     ( IOLike m
     , LedgerSupportsProtocol blk
     , HasCallStack
     )
  => LgrDB m blk
  -> Tracer m (TraceValidationEvent blk)
  -> TopLevelConfig blk
  -> StrictTVar m (WithFingerprint (InvalidBlocks blk))
  -> BlockCache blk
  -> ChainAndLedger  blk                   -- ^ Current chain and ledger
  -> CandidateSuffix blk                   -- ^ Candidate fragment
  -> m (Maybe (ChainAndLedger blk))
validateCandidate lgrDB tracer cfg varInvalid blockCache
                  curChainAndLedger candSuffix =
    LgrDB.validate lgrDB curLedger blockCache rollback newBlocks >>= \case
      LgrDB.ValidateExceededRollBack (LgrDB.ExceededRollback{rollbackMaximum}) -> do
        trace $ CandidateExceedsRollback
          rollbackMaximum
          (csRollback candSuffix)
          (csSuffix   candSuffix)
        return Nothing
      LgrDB.ValidateLedgerError (LgrDB.AnnLedgerError ledger' pt e) -> do
        let lastValid  = castPoint $ LgrDB.currentPoint ledger'
            candidate' = fromMaybe
              (error "cannot rollback to point on fragment") $
              AF.rollback lastValid candidate
        addInvalidBlocks (mkNewInvalidBlocks e pt)
        trace (InvalidBlock e pt)

        -- The candidate is now a prefix of the original candidate, and might be
        -- shorter than (or as long as) the current chain. We must check again
        -- whether it is preferred over the current chain.
        if preferAnchoredCandidate cfg curChain candidate'
          then do
            trace (ValidCandidate (csSuffix candSuffix))
            return $ Just $ VF.new candidate' ledger'
          else do
            trace (InvalidCandidate (csSuffix candSuffix))
            return Nothing
      LgrDB.ValidateSuccessful ledger' -> do
        trace (ValidCandidate (csSuffix candSuffix))
        return $ Just $ VF.new candidate ledger'
  where
    curChain  = VF.validatedFragment curChainAndLedger
    curLedger = VF.validatedLedger   curChainAndLedger
    trace     = traceWith tracer

    CandidateSuffix rollback suffix = candSuffix
    newBlocks = AF.toOldestFirst suffix
    candidate = fromMaybe
      (error "candidate suffix doesn't fit on the current chain") $
      fitCandidateSuffixOn curChain candSuffix

    -- | Add the given map of invalid points to 'cdbInvalid' and change its
    -- fingerprint.
    addInvalidBlocks :: InvalidBlocks blk -> m ()
    addInvalidBlocks invalidBlocksInCand = atomically $
      modifyTVar varInvalid $ \(WithFingerprint invalid fp) ->
        WithFingerprint (Map.union invalid invalidBlocksInCand) (succ fp)

    -- | Make an 'InvalidBlocks' for all the points after the given point
    -- (inclusive) in the candidate fragment that can be added to
    -- 'cdbInvalid'.
    --
    -- PRECONDITON: the given point is on the candidate fragment.
    mkNewInvalidBlocks :: HasCallStack
                       => ExtValidationError blk
                       -> RealPoint blk
                       -> InvalidBlocks blk
    mkNewInvalidBlocks e pt@(RealPoint slot hash) =
      case AF.splitAfterPoint suffix (realPointToPoint pt) of
        Nothing           -> error "point not on fragment"
        Just (_, afterPt) ->
          Map.insert hash (InvalidBlockInfo (ValidationError e) slot) $
          Map.fromList
            [ (blockHash hdr, InvalidBlockInfo reason (blockSlot hdr))
            | hdr <- AF.toOldestFirst afterPt
            , let reason = InChainAfterInvalidBlock pt e
            ]

{-------------------------------------------------------------------------------
  Auxiliary data types for 'cdbAddBlock'
-------------------------------------------------------------------------------}

-- | Instantiate 'ValidatedFragment' in the way that chain selection requires
type ChainAndLedger blk = ValidatedFragment (LgrDB.LedgerDB blk) blk

-- | Auxiliary data type for 'cdbAddBlock' for a candidate suffix.
--
-- INVARIANT: the length of the suffix must always be >= the rollback
data CandidateSuffix blk = CandidateSuffix
  { csRollback :: !Word64
    -- ^ The number of headers to roll back the current chain
  , csSuffix   :: !(AnchoredFragment (Header blk))
    -- ^ The new headers to add after rolling back the current chain.
  }

deriving instance (HasHeader blk, Eq (Header blk))
               => Eq   (CandidateSuffix blk)
deriving instance (HasHeader blk, Show (Header blk))
               => Show (CandidateSuffix blk)

mkCandidateSuffix :: HasHeader (Header blk)
                  => Word64
                  -> AnchoredFragment (Header blk)
                  -> CandidateSuffix blk
mkCandidateSuffix rollback suffix =
    assert (fromIntegral (AF.length suffix) >= rollback) $
    CandidateSuffix rollback suffix

-- | Fit the candidate suffix on a chain after first rolling back the given
-- chain.
--
-- If the given chain is the current chain on which the candidate is based, a
-- 'Just' will be returned. The returned candidate fragment will have the same
-- anchor point as the given chain.
fitCandidateSuffixOn :: HasHeader (Header blk)
                     => AnchoredFragment (Header blk)
                     -> CandidateSuffix  blk
                     -> Maybe (AnchoredFragment (Header blk))
fitCandidateSuffixOn curChain (CandidateSuffix rollback suffix) =
    AF.join (AF.dropNewest (fromIntegral rollback) curChain) suffix

-- | Roll back the candidate suffix to the given point.
--
-- PRECONDITION: the given point must correspond to one of the new headers of
-- the candidate suffix ('csSuffix').
--
-- PRECONDITION: the length of the suffix rolled back to the given point must
-- be >= the rollback ('csRollback').
rollbackCandidateSuffix :: (HasHeader (Header blk), HasCallStack)
                        => Point           blk
                        -> CandidateSuffix blk
                        -> CandidateSuffix blk
rollbackCandidateSuffix pt (CandidateSuffix rollback suffix)
    | Just suffix' <- AF.rollback (castPoint pt) suffix
    = mkCandidateSuffix rollback suffix'
    | otherwise
    = error "rollback point not on the candidate suffix"

-- | Calculate the candidate suffix of a fork of the current chain.
--
-- If the candidate fragment is shorter than the current chain, 'Nothing' is
-- returned (this would violate the invariant of 'CandidateSuffix').
--
-- PRECONDITION: the candidate fragment must intersect with the current chain
-- fragment.
intersectCandidateSuffix
  :: (HasHeader (Header blk), HasCallStack)
  => AnchoredFragment (Header blk)  -- ^ Current chain
  -> AnchoredFragment (Header blk)  -- ^ Candidate chain
  -> Maybe (CandidateSuffix   blk)  -- ^ Candidate suffix
intersectCandidateSuffix curChain candChain =
  case AF.intersect curChain candChain of
    Just (_curChainPrefix, _candPrefix, curChainSuffix, candSuffix)
      -> let rollback = AF.length curChainSuffix
         in if AF.length candSuffix >= rollback
            then Just $ mkCandidateSuffix (fromIntegral rollback) candSuffix
            else Nothing
    -- Precondition violated.
    _ -> error "candidate fragment doesn't intersect with current chain"

-- | If any invalid blocks occur in the candidate suffix, truncate it so that
-- it only contains valid blocks.
--
-- If the suffix becomes too short, return 'Nothing'.
truncateInvalidCandidate
  :: HasHeader (Header blk)
  => (HeaderHash blk -> Bool)  -- ^ Check whether a block is invalid
  -> CandidateSuffix blk
  -> Maybe (CandidateSuffix blk)
truncateInvalidCandidate isInvalid (CandidateSuffix rollback suffix)
    | truncatedLength >= rollback
    , truncatedLength > 0 -- No point in having an empty suffix
    = Just $ mkCandidateSuffix rollback truncated
    | otherwise
    = Nothing
  where
    truncatedLength = fromIntegral (AF.length truncated)
    truncated = AF.takeWhileOldest (not . isInvalid . blockHash) suffix

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}


-- | Wrap a @getter@ function so that it returns 'Nothing' for invalid blocks.
ignoreInvalid
  :: HasHeader blk
  => proxy blk
  -> InvalidBlocks blk
  -> (HeaderHash blk -> Maybe a)
  -> (HeaderHash blk -> Maybe a)
ignoreInvalid _ invalid getter hash
    | Map.member hash invalid = Nothing
    | otherwise               = getter hash

-- | Wrap a @successors@ function so that invalid blocks are not returned as
-- successors.
ignoreInvalidSuc
  :: HasHeader blk
  => proxy blk
  -> InvalidBlocks blk
  -> (WithOrigin (HeaderHash blk) -> Set (HeaderHash blk))
  -> (WithOrigin (HeaderHash blk) -> Set (HeaderHash blk))
ignoreInvalidSuc _ invalid succsOf =
    Set.filter (`Map.notMember` invalid) . succsOf

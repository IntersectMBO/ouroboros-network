{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE MultiWayIf           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Operations involving chain selection: the initial chain selection and
-- adding a block.
module Ouroboros.Consensus.Storage.ChainDB.Impl.ChainSel (
    addBlockAsync
  , addBlockSync
  , chainSelectionForBlock
  , initialChainSelection
    -- * Exported for testing purposes
  , olderThanK
  ) where

import           Control.Exception (assert)
import           Control.Monad.Except
import           Control.Monad.Trans.State.Strict
import           Control.Tracer (Tracer, nullTracer, traceWith)
import           Data.Function (on)
import           Data.List (partition, sortBy)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (isJust)
import           Data.Maybe.Strict (StrictMaybe (..), isSNothing,
                     strictMaybeToMaybe)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.SOP (K (..), unK)
import           GHC.Stack (HasCallStack)

import           Ouroboros.Network.AnchoredFragment (Anchor, AnchoredFragment,
                     AnchoredSeq (..))
import qualified Ouroboros.Network.AnchoredFragment as AF

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Fragment.InFuture (CheckInFuture (..))
import qualified Ouroboros.Consensus.Fragment.InFuture as InFuture
import           Ouroboros.Consensus.Fragment.Validated (ValidatedFragment)
import qualified Ouroboros.Consensus.Fragment.Validated as VF
import           Ouroboros.Consensus.HardFork.Abstract
import qualified Ouroboros.Consensus.HardFork.History as History
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Inspect
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Util (whenJust)
import           Ouroboros.Consensus.Util.AnchoredFragment
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.STM (WithFingerprint (..))
import           Ouroboros.Consensus.Util.TentativeState

import           Data.Functor.Contravariant ((>$<))
import           Ouroboros.Consensus.Fragment.Diff (ChainDiff (..))
import qualified Ouroboros.Consensus.Fragment.Diff as Diff
import           Ouroboros.Consensus.Fragment.ValidatedDiff
                     (ValidatedChainDiff (..))
import qualified Ouroboros.Consensus.Fragment.ValidatedDiff as ValidatedDiff
import           Ouroboros.Consensus.Storage.ChainDB.API (AddBlockPromise (..),
                     BlockComponent (..), ChainType (..),
                     InvalidBlockReason (..))
import           Ouroboros.Consensus.Storage.ChainDB.API.Types.InvalidBlockPunishment
                     (InvalidBlockPunishment)
import qualified Ouroboros.Consensus.Storage.ChainDB.API.Types.InvalidBlockPunishment as InvalidBlockPunishment
import           Ouroboros.Consensus.Storage.ChainDB.Impl.BlockCache
                     (BlockCache)
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.BlockCache as BlockCache
import           Ouroboros.Consensus.Storage.ChainDB.Impl.LgrDB (LedgerDB',
                     LgrDB)
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.LgrDB as LgrDB
import           Ouroboros.Consensus.Storage.ChainDB.Impl.Paths
                     (LookupBlockInfo)
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.Paths as Paths
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.Query as Query
import           Ouroboros.Consensus.Storage.ChainDB.Impl.Types
import           Ouroboros.Consensus.Storage.ImmutableDB (ImmutableDB)
import qualified Ouroboros.Consensus.Storage.ImmutableDB as ImmutableDB
import           Ouroboros.Consensus.Storage.VolatileDB (VolatileDB)
import qualified Ouroboros.Consensus.Storage.VolatileDB as VolatileDB
import           Ouroboros.Consensus.Util.Enclose (encloseWith)

-- | Perform the initial chain selection based on the tip of the ImmutableDB
-- and the contents of the VolatileDB.
--
-- Returns the chosen validated chain and corresponding ledger.
--
-- See "## Initialization" in ChainDB.md.
initialChainSelection
  :: forall m blk. (IOLike m, LedgerSupportsProtocol blk)
  => ImmutableDB m blk
  -> VolatileDB m blk
  -> LgrDB m blk
  -> Tracer m (TraceInitChainSelEvent blk)
  -> TopLevelConfig blk
  -> StrictTVar m (WithFingerprint (InvalidBlocks blk))
  -> StrictTVar m (FutureBlocks m blk)
  -> CheckInFuture m blk
  -> m (ChainAndLedger blk)
initialChainSelection immutableDB volatileDB lgrDB tracer cfg varInvalid
                      varFutureBlocks futureCheck = do
    -- We follow the steps from section "## Initialization" in ChainDB.md

    (i :: Anchor blk, succsOf, ledger) <- atomically $ do
      invalid <- forgetFingerprint <$> readTVar varInvalid
      (,,)
        <$> ImmutableDB.getTipAnchor immutableDB
        <*> (ignoreInvalidSuc volatileDB invalid <$>
              VolatileDB.filterByPredecessor volatileDB)
        <*> (K <$> LgrDB.getCurrent lgrDB)

    chains <- constructChains i succsOf

    -- We use the empty fragment anchored at @i@ as the current chain (and
    -- ledger) and the default in case there is no better candidate.
    let curChain          = Empty (AF.castAnchor i)
        curChainAndLedger = VF.ValidatedFragment curChain ledger

    case NE.nonEmpty (filter (preferAnchoredCandidate bcfg curChain) chains) of
      -- If there are no candidates, no chain selection is needed
      Nothing      -> return curChainAndLedger
      Just chains' -> maybe curChainAndLedger toChainAndLedger <$>
        chainSelection' curChainAndLedger chains'
  where
    bcfg :: BlockConfig blk
    bcfg = configBlock cfg

    -- | Turn the 'ValidatedChainDiff' into a 'ChainAndLedger'.
    --
    -- The rollback of the 'ChainDiff' must be empty, as the suffix starts
    -- from the tip of the ImmutableDB, and we can't roll back past that tip.
    -- This is guaranteed by the fact that all constructed candidates start
    -- from this tip.
    toChainAndLedger
      :: ValidatedChainDiff (Header blk) (K (LedgerDB' blk))
      -> ChainAndLedger blk
    toChainAndLedger (ValidatedChainDiff chainDiff ledger) =
      case chainDiff of
        ChainDiff rollback suffix
          | rollback == 0
          -> VF.ValidatedFragment suffix ledger
          | otherwise
          -> error "constructed an initial chain with rollback"

    -- | Use the VolatileDB to construct all chains starting from the tip of
    -- the ImmutableDB.
    constructChains ::
         Anchor blk -- ^ Tip of the ImmutableDB, @i@
      -> (ChainHash blk -> Set (HeaderHash blk))
      -> m [AnchoredFragment (Header blk)]
    constructChains i succsOf = flip evalStateT Map.empty $
        mapM constructChain suffixesAfterI
      where
        suffixesAfterI :: [NonEmpty (HeaderHash blk)]
        suffixesAfterI = Paths.maximalCandidates succsOf (AF.anchorToPoint i)

        constructChain ::
             NonEmpty (HeaderHash blk)
          -> StateT (Map (HeaderHash blk) (Header blk))
                    m
                    (AnchoredFragment (Header blk))
        constructChain hashes =
            AF.fromOldestFirst (AF.castAnchor i) <$>
            mapM (getKnownHeaderThroughCache volatileDB) (NE.toList hashes)

    -- | Perform chain selection (including validation) on the given
    -- candidates.
    --
    -- PRECONDITION: all candidates are anchored at @i@.
    --
    -- PRECONDITION: all candidates must be preferred over the current chain.
    chainSelection' ::
         HasCallStack
      => ChainAndLedger blk
         -- ^ The current chain and ledger, corresponding to
         -- @i@.
      -> NonEmpty (AnchoredFragment (Header blk))
         -- ^ Candidates anchored at @i@
      -> m (Maybe (ValidatedChainDiff (Header blk) (K (LedgerDB' blk))))
    chainSelection' curChainAndLedger candidates =
        assert (all ((LgrDB.currentPoint ledger ==) .
                     castPoint . AF.anchorPoint)
                    candidates) $
        assert (all (preferAnchoredCandidate bcfg curChain) candidates) $ do
          cse <- chainSelEnv
          chainSelection cse (Diff.extend <$> candidates)
      where
        curChain = VF.validatedFragment curChainAndLedger
        K ledger = VF.validatedLedger   curChainAndLedger
        chainSelEnv = do
          varTentativeState  <- newTVarIO NoLastInvalidTentative
          varTentativeHeader <- newTVarIO SNothing
          pure ChainSelEnv
            { lgrDB
            , bcfg
            , varInvalid
            , varFutureBlocks
            , futureCheck
            , blockCache = BlockCache.empty
            , curChainAndLedger
            , validationTracer = InitChainSelValidation >$< tracer
              -- initial chain selection is not concerned about pipelining
            , pipeliningTracer = nullTracer
            , varTentativeState
            , varTentativeHeader
            , punish = Nothing
            , getTentativeFollowers = pure []
            }

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
  -> InvalidBlockPunishment m
  -> blk
  -> m (AddBlockPromise m blk)
addBlockAsync CDB { cdbTracer, cdbBlocksToAdd } =
    addBlockToAdd (TraceAddBlockEvent >$< cdbTracer) cdbBlocksToAdd

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
     , LedgerSupportsProtocol blk
     , InspectLedger blk
     , HasHardForkHistory blk
     , HasCallStack
     )
  => ChainDbEnv m blk
  -> BlockToAdd m blk
  -> m ()
addBlockSync cdb@CDB {..} BlockToAdd { blockToAdd = b, .. } = do
    (isMember, invalid, curChain) <- atomically $ (,,)
      <$> VolatileDB.getIsMember          cdbVolatileDB
      <*> (forgetFingerprint <$> readTVar cdbInvalid)
      <*> Query.getCurrentChain           cdb

    let immBlockNo = AF.anchorBlockNo curChain

    -- We follow the steps from section "## Adding a block" in ChainDB.md

    -- Note: we call 'chainSelectionForFutureBlocks' in all branches instead
    -- of once, before branching, because we want to do it /after/ writing the
    -- block to the VolatileDB and delivering the 'varBlockWrittenToDisk'
    -- promise, as this is the promise the BlockFetch client waits for.
    -- Otherwise, the BlockFetch client would have to wait for
    -- 'chainSelectionForFutureBlocks'.

    -- ### Ignore
    newTip <- if
      | olderThanK hdr isEBB immBlockNo -> do
        traceWith addBlockTracer $ IgnoreBlockOlderThanK (blockRealPoint b)
        deliverWrittenToDisk False
        chainSelectionForFutureBlocks cdb BlockCache.empty

      | isMember (blockHash b) -> do
        traceWith addBlockTracer $ IgnoreBlockAlreadyInVolatileDB (blockRealPoint b)
        deliverWrittenToDisk True
        chainSelectionForFutureBlocks cdb BlockCache.empty

      | Just (InvalidBlockInfo reason _) <- Map.lookup (blockHash b) invalid -> do
        traceWith addBlockTracer $ IgnoreInvalidBlock (blockRealPoint b) reason
        deliverWrittenToDisk False

        -- We wouldn't know the block is invalid if its prefix was invalid,
        -- hence 'InvalidBlockPunishment.BlockItself'.
        InvalidBlockPunishment.enact
          blockPunish
          InvalidBlockPunishment.BlockItself

        chainSelectionForFutureBlocks cdb BlockCache.empty

      -- The remaining cases
      | otherwise -> do
        let traceEv = AddedBlockToVolatileDB (blockRealPoint b) (blockNo b) isEBB
        encloseWith (traceEv >$< addBlockTracer) $
          VolatileDB.putBlock cdbVolatileDB b
        deliverWrittenToDisk True

        let blockCache = BlockCache.singleton b
        -- Do chain selection for future blocks before chain selection for the
        -- new block. When some future blocks are now older than the current
        -- block, we will do chain selection in a more chronological order.
        void $ chainSelectionForFutureBlocks cdb blockCache
        chainSelectionForBlock cdb blockCache hdr blockPunish

    deliverProcessed newTip
  where
    addBlockTracer :: Tracer m (TraceAddBlockEvent blk)
    addBlockTracer = TraceAddBlockEvent >$< cdbTracer

    hdr :: Header blk
    hdr = getHeader b

    isEBB :: IsEBB
    isEBB = headerToIsEBB hdr

    -- | Fill in the 'TMVar' for the 'varBlockWrittenToDisk' of the block's
    -- 'AddBlockPromise' with the given 'Bool'.
    deliverWrittenToDisk :: Bool -> m ()
    deliverWrittenToDisk writtenToDisk = atomically $
        putTMVar varBlockWrittenToDisk writtenToDisk

    -- | Fill in the 'TMVar' for the 'varBlockProcessed' of the block's
    -- 'AddBlockPromise' with the given tip.
    deliverProcessed :: Point blk -> m ()
    deliverProcessed tip = atomically $
        putTMVar varBlockProcessed tip

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
    | NotOrigin bNo == immBlockNo
    , isEBB == IsEBB
    = False
    | otherwise
    = NotOrigin bNo <= immBlockNo
  where
    bNo = blockNo hdr

-- | When we switch to a new selected chain, we are either extending the current
-- chain by adding blocks on top or we are switching to a fork.
data ChainSwitchType = AddingBlocks | SwitchingToAFork
  deriving (Show, Eq)

-- | Return the new tip.
chainSelectionForFutureBlocks
  :: ( IOLike m
     , LedgerSupportsProtocol blk
     , InspectLedger blk
     , HasHardForkHistory blk
     , HasCallStack
     )
  => ChainDbEnv m blk -> BlockCache blk -> m (Point blk)
chainSelectionForFutureBlocks cdb@CDB{..} blockCache = do
    -- Get 'cdbFutureBlocks' and empty the map in the TVar. It will be
    -- repopulated with the blocks that are still from the future (but not the
    -- ones no longer from the future) during chain selection for those
    -- blocks.
    futureBlockHeaders <- atomically $ do
      futureBlocks <- readTVar cdbFutureBlocks
      writeTVar cdbFutureBlocks Map.empty
      return $ Map.elems futureBlocks
    forM_ futureBlockHeaders $ \(hdr, punish) -> do
      traceWith tracer $ ChainSelectionForFutureBlock (headerRealPoint hdr)
      chainSelectionForBlock cdb blockCache hdr punish
    atomically $ Query.getTipPoint cdb
  where
    tracer = TraceAddBlockEvent >$< cdbTracer

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
     , LedgerSupportsProtocol blk
     , InspectLedger blk
     , HasHardForkHistory blk
     , HasCallStack
     )
  => ChainDbEnv m blk
  -> BlockCache blk
  -> Header blk
  -> InvalidBlockPunishment m
  -> m (Point blk)
chainSelectionForBlock cdb@CDB{..} blockCache hdr punish = do
    (invalid, succsOf, lookupBlockInfo, curChain, tipPoint, ledgerDB)
      <- atomically $ (,,,,,)
          <$> (forgetFingerprint <$> readTVar cdbInvalid)
          <*> VolatileDB.filterByPredecessor  cdbVolatileDB
          <*> VolatileDB.getBlockInfo         cdbVolatileDB
          <*> Query.getCurrentChain           cdb
          <*> Query.getTipPoint               cdb
          <*> (K <$> LgrDB.getCurrent  cdbLgrDB)
    let curChainAndLedger :: ChainAndLedger blk
        curChainAndLedger =
          -- The current chain we're working with here is not longer than @k@
          -- blocks (see 'getCurrentChain' and 'cdbChain'), which is easier to
          -- reason about when doing chain selection, etc.
          assert (fromIntegral (AF.length curChain) <= k) $
          VF.ValidatedFragment curChain ledgerDB

        immBlockNo :: WithOrigin BlockNo
        immBlockNo = AF.anchorBlockNo curChain

        -- Let these two functions ignore invalid blocks
        lookupBlockInfo' = ignoreInvalid    cdb invalid lookupBlockInfo
        succsOf'         = ignoreInvalidSuc cdb invalid succsOf

    -- The preconditions
    assert (isJust $ lookupBlockInfo (headerHash hdr)) $ return ()

    if
      -- The chain might have grown since we added the block such that the
      -- block is older than @k@.
      | olderThanK hdr isEBB immBlockNo -> do
        traceWith addBlockTracer $ IgnoreBlockOlderThanK p
        return tipPoint

      -- We might have validated the block in the meantime
      | Just (InvalidBlockInfo reason _) <- Map.lookup (headerHash hdr) invalid -> do
        traceWith addBlockTracer $ IgnoreInvalidBlock p reason

        -- We wouldn't know the block is invalid if its prefix was invalid,
        -- hence 'InvalidBlockPunishment.BlockItself'.
        InvalidBlockPunishment.enact
          punish
          InvalidBlockPunishment.BlockItself

        return tipPoint

      -- The block @b@ fits onto the end of our current chain
      | pointHash tipPoint == headerPrevHash hdr -> do
        -- ### Add to current chain
        traceWith addBlockTracer (TryAddToCurrentChain p)
        addToCurrentChain succsOf' curChainAndLedger

      | Just diff <- Paths.isReachable lookupBlockInfo' curChain p -> do
        -- ### Switch to a fork
        traceWith addBlockTracer (TrySwitchToAFork p diff)
        switchToAFork succsOf' lookupBlockInfo' curChainAndLedger diff

      | otherwise -> do
        -- ### Store but don't change the current chain
        traceWith addBlockTracer (StoreButDontChange p)
        return tipPoint

    -- Note that we may have extended the chain, but have not trimmed it to
    -- @k@ blocks/headers. That is the job of the background thread, which
    -- will first copy the blocks/headers to trim (from the end of the
    -- fragment) from the VolatileDB to the ImmutableDB.
  where
    SecurityParam k = configSecurityParam cdbTopLevelConfig

    p :: RealPoint blk
    p = headerRealPoint hdr

    isEBB :: IsEBB
    isEBB = headerToIsEBB hdr

    addBlockTracer :: Tracer m (TraceAddBlockEvent blk)
    addBlockTracer = TraceAddBlockEvent >$< cdbTracer

    mkChainSelEnv :: ChainAndLedger blk -> ChainSelEnv m blk
    mkChainSelEnv curChainAndLedger = ChainSelEnv
      { lgrDB                 = cdbLgrDB
      , bcfg                  = configBlock cdbTopLevelConfig
      , varInvalid            = cdbInvalid
      , varFutureBlocks       = cdbFutureBlocks
      , varTentativeState     = cdbTentativeState
      , varTentativeHeader    = cdbTentativeHeader
      , getTentativeFollowers =
              filter ((TentativeChain ==) . fhChainType) . Map.elems
          <$> readTVar cdbFollowers
      , futureCheck           = cdbCheckInFuture
      , blockCache            = blockCache
      , curChainAndLedger     = curChainAndLedger
      , validationTracer      =
          TraceAddBlockEvent . AddBlockValidation >$< cdbTracer
      , pipeliningTracer       =
          TraceAddBlockEvent . PipeliningEvent >$< cdbTracer
      , punish                = Just (p, punish)
      }

    -- | PRECONDITION: the header @hdr@ (and block @b@) fit onto the end of
    -- the current chain.
    addToCurrentChain ::
         HasCallStack
      => (ChainHash blk -> Set (HeaderHash blk))
      -> ChainAndLedger blk
         -- ^ The current chain and ledger
      -> m (Point blk)
    addToCurrentChain succsOf curChainAndLedger = do
        let suffixesAfterB = Paths.maximalCandidates succsOf (realPointToPoint p)

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
              hdrs <- mapM (getKnownHeaderThroughCache cdbVolatileDB) $
                        NE.toList hashes
              return $ AF.fromOldestFirst curHead (hdr : hdrs)

        let chainDiffs = NE.nonEmpty
              $ NE.filter ( preferAnchoredCandidate (bcfg chainSelEnv) curChain
                          . Diff.getSuffix
                          )
              $ fmap Diff.extend candidates
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
        case chainDiffs of
          Nothing          -> return curTip
          Just chainDiffs' ->
            chainSelection chainSelEnv chainDiffs' >>= \case
              Nothing ->
                return curTip
              Just validatedChainDiff ->
                switchTo
                  validatedChainDiff
                  (varTentativeHeader chainSelEnv)
                  AddingBlocks
      where
        chainSelEnv = mkChainSelEnv curChainAndLedger
        curChain    = VF.validatedFragment curChainAndLedger
        curTip      = castPoint $ AF.headPoint curChain
        curHead     = AF.headAnchor curChain

    -- | We have found a 'ChainDiff' through the VolatileDB connecting the new
    -- block to the current chain. We'll call the intersection/anchor @x@.
    --
    -- We try to extend this path by looking for forks that start with the
    -- given block, then we do chain selection and /possibly/ try to switch to
    -- a new fork.
    switchToAFork ::
         HasCallStack
      => (ChainHash blk -> Set (HeaderHash blk))
      -> LookupBlockInfo blk
      -> ChainAndLedger blk
         -- ^ The current chain (anchored at @i@) and ledger
      -> ChainDiff (HeaderFields blk)
         -- ^ Header fields for @(x,b]@
      -> m (Point blk)
    switchToAFork succsOf lookupBlockInfo curChainAndLedger diff = do
        -- We use a cache to avoid reading the headers from disk multiple
        -- times in case they're part of multiple forks that go through @b@.
        let initCache = Map.singleton (headerHash hdr) hdr
        chainDiffs <-
          -- 4. Filter out candidates that are not preferred over the current
          -- chain.
          --
          -- The suffixes all fork off from the current chain within @k@
          -- blocks, so it satisfies the precondition of 'preferCandidate'.
            fmap
              ( filter
                  ( preferAnchoredCandidate (bcfg chainSelEnv) curChain
                  . Diff.getSuffix
                  )
              )
            -- 3. Translate the 'HeaderFields' to 'Header' by reading the
            -- headers from disk.
          . flip evalStateT initCache
          . mapM translateToHeaders
            -- 2. Filter out candidates that are shorter than the current
            -- chain. We don't want to needlessly read the headers from disk
            -- for those candidates.
          . NE.filter (not . Diff.rollbackExceedsSuffix)
            -- 1. Extend the diff with candidates fitting on @B@
          . Paths.extendWithSuccessors succsOf lookupBlockInfo
          $ diff

        case NE.nonEmpty chainDiffs of
          -- No candidates preferred over the current chain
          Nothing          -> return curTip
          Just chainDiffs' ->
            chainSelection chainSelEnv chainDiffs' >>= \case
              Nothing                 ->
                return curTip
              Just validatedChainDiff ->
                switchTo
                  validatedChainDiff
                  (varTentativeHeader chainSelEnv)
                  SwitchingToAFork
      where
        chainSelEnv = mkChainSelEnv curChainAndLedger
        curChain    = VF.validatedFragment curChainAndLedger
        curTip      = castPoint $ AF.headPoint curChain

    -- | Create a 'NewTipInfo' corresponding to the tip of the given ledger.
    mkNewTipInfo :: LedgerDB' blk -> NewTipInfo blk
    mkNewTipInfo newLedgerDB =
        NewTipInfo {
            newTipPoint       = tipPoint
          , newTipEpoch       = tipEpoch
          , newTipSlotInEpoch = tipSlotInEpoch
          , newTipTrigger     = p
          }
      where
        cfg :: TopLevelConfig blk
        cfg = cdbTopLevelConfig

        ledger :: LedgerState blk EmptyMK
        ledger = ledgerState (LgrDB.current newLedgerDB)

        summary :: History.Summary (HardForkIndices blk)
        summary = hardForkSummary
                    (configLedger cfg)
                    ledger

        (tipPoint, (tipEpoch, tipSlotInEpoch)) =
          case pointToWithOriginRealPoint
                 (ledgerTipPoint ledger) of
            Origin        -> error "cannot have switched to an empty chain"
            NotOrigin tip ->
              let query = History.slotToEpoch' (realPointSlot tip)
              in (tip, History.runQueryPure query summary)

    -- | Try to apply the given 'ChainDiff' on the current chain fragment. The
    -- 'LgrDB.LedgerDB' is updated in the same transaction.
    --
    -- Note that we /cannot/ have switched to a different current chain in the
    -- meantime, since this function will only be called by a single
    -- background thread.
    --
    -- It /is/ possible that the background thread copying headers older than
    -- @k@ from the VolatileDB to the ImmutableDB has removed some headers
    -- from the beginning of the current chain fragment, but does not affect
    -- us, as we cannot roll back more than @k@ headers anyway.
    switchTo
      :: HasCallStack
      => ValidatedChainDiff (Header blk) (K (LedgerDB' blk))
         -- ^ Chain and ledger to switch to
      -> StrictTVar m (StrictMaybe (Header blk))
         -- ^ Tentative header
      -> ChainSwitchType
      -> m (Point blk)
    switchTo vChainDiff varTentativeHeader chainSwitchType = do
        traceWith addBlockTracer $
            ChangingSelection
          $ castPoint
          $ AF.headPoint
          $ getSuffix
          $ getChainDiff vChainDiff
        (curChain, newChain, events, prevTentativeHeader) <- atomically $ do
          curChain  <- readTVar         cdbChain -- Not Query.getCurrentChain!
          curLedger <- LgrDB.getCurrent cdbLgrDB
          case Diff.apply curChain chainDiff of
            -- Impossible, as described in the docstring
            Nothing       ->
              error "chainDiff doesn't fit onto current chain"
            Just newChain -> do
              writeTVar cdbChain newChain
              LgrDB.setCurrent cdbLgrDB newLedger

              -- Inspect the new ledger for potential problems
              let events :: [LedgerEvent blk]
                  events = inspectLedger
                             cdbTopLevelConfig
                             (ledgerState $ LgrDB.current curLedger)
                             (ledgerState $ LgrDB.current newLedger)

              -- Clear the tentative header
              prevTentativeHeader <- swapTVar varTentativeHeader SNothing

              case chainSwitchType of
                -- When adding blocks, the intersection point of the old and new
                -- tentative/selected chain is not receding, in which case
                -- `fhSwitchFork` is unnecessary. In the case of pipelining a
                -- block, it would even result in rolling back by one block and
                -- rolling forward again.
                AddingBlocks      -> pure ()
                SwitchingToAFork -> do
                  -- Update the followers
                  --
                  -- 'Follower.switchFork' needs to know the intersection point
                  -- (@ipoint@) between the old and the current chain.
                  let ipoint = castPoint $ Diff.getAnchorPoint chainDiff
                  followerHandles <- Map.elems <$> readTVar cdbFollowers
                  forM_ followerHandles $ \followerHandle ->
                    fhSwitchFork followerHandle ipoint newChain

              return (curChain, newChain, events, prevTentativeHeader)

        let mkTraceEvent = case chainSwitchType of
              AddingBlocks     -> AddedToCurrentChain
              SwitchingToAFork -> SwitchedToAFork
        traceWith addBlockTracer $
          mkTraceEvent events (mkNewTipInfo newLedger) curChain newChain
        whenJust (strictMaybeToMaybe prevTentativeHeader) $ traceWith $
          PipeliningEvent . OutdatedTentativeHeader >$< addBlockTracer
        traceWith cdbTraceLedger newLedger

        return $ castPoint $ AF.headPoint newChain
      where
        ValidatedChainDiff chainDiff (K newLedger) = vChainDiff

    -- | We have a new block @b@ that doesn't fit onto the current chain, but
    -- we have found a 'ChainDiff' connecting it to the current chain via
    -- intersection point @x@. We may also have extended that 'ChainDiff' with
    -- more blocks fitting onto @b@, i.e., a suffix @s@.
    --
    -- We now translate that 'ChainDiff' from 'HeaderFields' to 'Header's by
    -- reading the headers from disk.
    --
    -- Note that we need to read the headers corresponding to the hashes
    -- @(x,b)@ and @(b,?]@ from disk. Not for @b@, as that's in our cache.
    translateToHeaders
      :: ChainDiff (HeaderFields blk)
      -> StateT (Map (HeaderHash blk) (Header blk))
                m
                (ChainDiff (Header blk))
         -- ^ Fork, anchored at @x@, contains (the header of) @b@ and ends
         -- with the suffix @s@.
    translateToHeaders =
        Diff.mapM (getKnownHeaderThroughCache cdbVolatileDB . headerFieldHash)

-- | Check whether the header for the hash is in the cache, if not, get
-- the corresponding header from the VolatileDB and store it in the cache.
--
-- PRECONDITION: the header (block) must exist in the VolatileDB.
getKnownHeaderThroughCache
  :: (MonadThrow m, HasHeader blk)
  => VolatileDB m blk
  -> HeaderHash blk
  -> StateT (Map (HeaderHash blk) (Header blk)) m (Header blk)
getKnownHeaderThroughCache volatileDB hash = gets (Map.lookup hash) >>= \case
    Just hdr -> return hdr
    Nothing  -> do
      hdr <- lift $ VolatileDB.getKnownBlockComponent volatileDB GetHeader hash
      modify (Map.insert hash hdr)
      return hdr

-- | Environment used by 'chainSelection' and related functions.
data ChainSelEnv m blk = ChainSelEnv
    { lgrDB                 :: LgrDB m blk
    , validationTracer      :: Tracer m (TraceValidationEvent blk)
    , pipeliningTracer      :: Tracer m (TracePipeliningEvent blk)
    , bcfg                  :: BlockConfig blk
    , varInvalid            :: StrictTVar m (WithFingerprint (InvalidBlocks blk))
    , varFutureBlocks       :: StrictTVar m (FutureBlocks m blk)
    , varTentativeState     :: StrictTVar m (TentativeState blk)
    , varTentativeHeader    :: StrictTVar m (StrictMaybe (Header blk))
    , getTentativeFollowers :: STM m [FollowerHandle m blk]
    , futureCheck           :: CheckInFuture m blk
    , blockCache            :: BlockCache blk
    , curChainAndLedger     :: ChainAndLedger blk
      -- | The block that this chain selection invocation is processing, and the
      -- punish action for the peer that sent that block; see
      -- 'InvalidBlockPunishment'.
      --
      -- Two notable subtleties:
      --
      -- o If a BlockFetch client adds an invalid block but that block isn't
      --   part of any desirable paths through the VolDB, then we won't attempt
      --   to validate it and so we won't discover it's invalid. The peer will
      --   not be punished. This seems acceptable, since it means we have turned
      --   our focus to a another peer offering better blocks and so this peer
      --   is no longer causing us BlockFetch work.
      --
      -- o If the block is frome the future but with clock skew, we'll add it to
      --   'varFutureBlocks'. We retain the punishment information, so that if
      --   the peer is still active once we do process that block, we're still
      --   able to punish them.
      --
      -- Thus invalid blocks can be skipped entirely or somewhat-arbitrarily
      -- delayed. This is part of the reason we bothered to restrict the
      -- expressiveness of the 'InvalidBlockPunishment' combiantors.
    , punish                :: Maybe (RealPoint blk, InvalidBlockPunishment m)
    }

-- | Perform chain selection with the given candidates. If a validated
-- candidate was chosen to replace the current chain, return it along with the
-- corresponding ledger.
--
-- PRECONDITION: all candidates must be preferred over the current chain.
--
-- PRECONDITION: the candidate chain diffs must fit on the (given) current
-- chain.
chainSelection
  :: forall m blk.
     ( IOLike m
     , LedgerSupportsProtocol blk
     , HasCallStack
     )
  => ChainSelEnv m blk
  -> NonEmpty (ChainDiff (Header blk))
  -> m (Maybe (ValidatedChainDiff (Header blk) (K (LedgerDB' blk))))
     -- ^ The (valid) chain diff and corresponding LedgerDB that was selected,
     -- or 'Nothing' if there is no valid chain diff preferred over the current
     -- chain.
chainSelection chainSelEnv chainDiffs =
    assert (all (preferAnchoredCandidate bcfg curChain . Diff.getSuffix)
                chainDiffs) $
    assert (all (isJust . Diff.apply curChain)
                chainDiffs) $
    go (sortCandidates (NE.toList chainDiffs))
  where
    ChainSelEnv {..} = chainSelEnv

    curChain = VF.validatedFragment curChainAndLedger

    sortCandidates :: [ChainDiff (Header blk)] -> [ChainDiff (Header blk)]
    sortCandidates =
      sortBy (flip (compareAnchoredFragments bcfg) `on` Diff.getSuffix)

    -- 1. Take the first candidate from the list of sorted candidates
    -- 2. Validate it
    --    - If it is invalid -> discard it and go to 1 with the rest of the
    --      list.
    --    - If it is valid and has the same tip -> return it
    --    - If it is valid, but is a prefix of the original ->
    --        add it to the list, sort it and go to 1. See the comment
    --        [Ouroboros] below.
    go ::
         [ChainDiff (Header blk)]
      -> m (Maybe (ValidatedChainDiff (Header blk) (K (LedgerDB' blk))))
    go []            = return Nothing
    go (candidate:candidates0) = do
        mTentativeHeader <- setTentativeHeader
        validateCandidate chainSelEnv candidate >>= \case
          InsufficientSuffix ->
            -- When the body of the tentative block turns out to be invalid, we
            -- have a valid *empty* prefix, as the tentative header fits on top
            -- of the current chain.
            assert (isSNothing mTentativeHeader) $ do
              candidates1 <- truncateRejectedBlocks candidates0
              go (sortCandidates candidates1)
          FullyValid validatedCandidate@(ValidatedChainDiff candidate' _) ->
            -- The entire candidate is valid
            assert (Diff.getTip candidate == Diff.getTip candidate') $
            return $ Just validatedCandidate
          ValidPrefix candidate' -> do
            whenJust (strictMaybeToMaybe mTentativeHeader) clearTentativeHeader
            -- Prefix of the candidate because it contained rejected blocks
            -- (invalid blocks and/or blocks from the future). Note that the
            -- spec says go back to candidate selection, see [^whyGoBack],
            -- because there might still be some candidates that contain the
            -- same rejected block. To simplify the control flow, we do it
            -- differently: instead of recomputing the candidates taking
            -- rejected blocks into account, we just truncate the remaining
            -- candidates that contain rejected blocks.
            candidates1 <- truncateRejectedBlocks candidates0
            -- Only include the prefix if it is still preferred over the current
            -- chain. When the candidate is now empty because of the truncation,
            -- it will be dropped here, as it will not be preferred over the
            -- current chain.
            let candidates2
                  | preferAnchoredCandidate bcfg curChain (Diff.getSuffix candidate')
                  = candidate':candidates1
                  | otherwise
                  = candidates1
            go (sortCandidates candidates2)
      where
        -- | Set and return the tentative header, if applicable. Also, notify
        -- the tentative followers.
        setTentativeHeader :: m (StrictMaybe (Header blk))
        setTentativeHeader = do
            mTentativeHeader <-
                  (\ts -> isPipelineable bcfg ts candidate)
              <$> readTVarIO varTentativeState
            whenJust (strictMaybeToMaybe mTentativeHeader) $ \tentativeHeader -> do
              let setTentative = SetTentativeHeader tentativeHeader
              encloseWith (setTentative >$< pipeliningTracer) $
                atomically $ writeTVar varTentativeHeader $ SJust tentativeHeader
                -- As we are only extending the existing chain, the intersection
                -- point is not receding, in which case fhSwitchFork is not
                -- necessary.

              -- Just in case, explicitly yield to ensure that a capability (by
              -- default, the node uses just two) has the opportunity to switch
              -- to a ChainSync server thread.
              yield
            pure mTentativeHeader

        -- | Clear a tentative header that turned out to be invalid. Also, roll
        -- back the tentative followers.
        clearTentativeHeader :: Header blk -> m ()
        clearTentativeHeader tentativeHeader = do
            atomically $ do
              writeTVar varTentativeHeader SNothing
              writeTVar varTentativeState $
                LastInvalidTentative (selectView bcfg tentativeHeader)
              forTentativeFollowers $ \followerHandle -> do
                let curTipPoint = castPoint $ AF.headPoint curChain
                fhSwitchFork followerHandle curTipPoint curChain
            traceWith pipeliningTracer $ TrapTentativeHeader tentativeHeader
          where
            forTentativeFollowers f = getTentativeFollowers >>= mapM_ f

    -- | Truncate the given (remaining) candidates that contain rejected
    -- blocks. Discard them if they are truncated so much that they are no
    -- longer preferred over the current chain.
    --
    -- A block is rejected if:
    --
    -- * It is invalid (present in 'varInvalid', i.e., 'cdbInvalid').
    -- * It is from the future (present in 'varFutureBlocks', i.e.,
    --   'cdbFutureBlocks').
    truncateRejectedBlocks ::
         [ChainDiff (Header blk)]
      -> m [ChainDiff (Header blk)]
    truncateRejectedBlocks cands = do
      (invalid, futureBlocks) <-
        atomically $ (,) <$> readTVar varInvalid <*> readTVar varFutureBlocks
      let isRejected hdr =
               Map.member (headerHash hdr) (forgetFingerprint invalid)
            || Map.member (headerHash hdr) futureBlocks
      return $ filter (preferAnchoredCandidate bcfg curChain . Diff.getSuffix)
             $ map (Diff.takeWhileOldest (not . isRejected)) cands

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

-- | Result of 'validateCandidate'.
data ValidationResult blk =
      -- | The entire candidate fragment was valid. No blocks were from the
      -- future.
      FullyValid (ValidatedChainDiff (Header blk) (K (LedgerDB' blk)))

      -- | The candidate fragment contained invalid blocks and/or blocks from
      -- the future that had to be truncated from the fragment.
    | ValidPrefix (ChainDiff (Header blk))

      -- | After truncating the invalid blocks or blocks from the future from
      -- the 'ChainDiff', it no longer contains enough blocks in its suffix to
      -- compensate for the number of blocks it wants to roll back.
    | InsufficientSuffix

-- | Validate a candidate by applying its blocks to the ledger, and return a
-- 'ValidatedChainDiff' for it, i.e., a chain diff along with a ledger
-- corresponding to its tip (the most recent block).
--
-- PRECONDITION: the candidate (chain diff) must fit onto the given current
-- chain.
--
-- If all blocks in the fragment are valid, then the chain diff in the
-- returned 'ValidatedChainDiff' is the same as the given candidate chain
-- diff.
--
-- If a block in the fragment is invalid, then the fragment in the returned
-- 'ValidatedChainDiff' is a prefix of the given candidate chain diff (upto
-- the last valid block).
ledgerValidateCandidate
  :: forall m blk.
     ( IOLike m
     , LedgerSupportsProtocol blk
     , HasCallStack
     )
  => ChainSelEnv m blk
  -> ChainDiff (Header blk)
  -> m (ValidatedChainDiff (Header blk) (K (LedgerDB' blk)))
ledgerValidateCandidate chainSelEnv chainDiff@(ChainDiff rollback suffix) =
    LgrDB.validate lgrDB curLedger blockCache rollback traceUpdate newBlocks >>= \case
      LgrDB.ValidateExceededRollBack {} ->
        -- Impossible: we asked the LgrDB to roll back past the immutable tip,
        -- which is impossible, since the candidates we construct must connect
        -- to the immutable tip.
        error "found candidate requiring rolling back past the immutable tip"

      LgrDB.ValidateLedgerError (LgrDB.AnnLedgerError ledger' pt e) -> do
        let lastValid  = LgrDB.currentPoint ledger'
            chainDiff' = Diff.truncate (castPoint lastValid) chainDiff
        traceWith validationTracer (InvalidBlock e pt)
        addInvalidBlock e pt
        traceWith validationTracer (ValidCandidate (Diff.getSuffix chainDiff'))

        -- punish the peer who sent a block if it is invalid or a block from its
        -- prefix is invalid
        --
        -- Note that it is a chain selection invariant that all candidates
        -- involve the block being processed: see Lemma 11.1 (Properties of the
        -- set of candidates) in the Chain Selection chapter of the The Cardano
        -- Consensus and Storage Layer technical report.
        whenJust punish $ \(addedPt, punishment) -> do
          let m = InvalidBlockPunishment.enact punishment
                $ if addedPt == pt
                  then InvalidBlockPunishment.BlockItself
                  else InvalidBlockPunishment.BlockPrefix
          case realPointSlot pt `compare` realPointSlot addedPt of
            LT -> m
            GT -> pure ()
            EQ -> when (lastValid /= realPointToPoint addedPt) m
              -- If pt and addedPt have the same slot, and addedPt is the tip of
              -- the ledger that pt was validated against, then addedPt is an
              -- EBB and is valid.
              --
              -- Otherwise, either pt == addedPt or addedPt comes after pt, so
              -- we should punish. (Tacit assumption made here: it's impossible
              -- three blocks in a row have the same slot.)

        return $ ValidatedDiff.new chainDiff' (K ledger')

      LgrDB.ValidateSuccessful ledger' -> do
        traceWith validationTracer (ValidCandidate suffix)
        return $ ValidatedDiff.new chainDiff (K ledger')
  where
    ChainSelEnv {
        lgrDB
      , validationTracer
      , curChainAndLedger
      , blockCache
      , varInvalid
      , punish
      } = chainSelEnv

    traceUpdate = traceWith $ UpdateLedgerDbTraceEvent >$< validationTracer

    curLedger :: LedgerDB' blk
    K curLedger = VF.validatedLedger curChainAndLedger

    newBlocks :: [Header blk]
    newBlocks = AF.toOldestFirst suffix

    -- | Record the invalid block in 'cdbInvalid' and change its fingerprint.
    addInvalidBlock :: ExtValidationError blk -> RealPoint blk -> m ()
    addInvalidBlock e (RealPoint slot hash) = atomically $
      modifyTVar varInvalid $ \(WithFingerprint invalid fp) ->
        WithFingerprint
          (Map.insert hash (InvalidBlockInfo (ValidationError e) slot) invalid)
          (succ fp)

-- | Truncate any future headers from the candidate 'ValidatedChainDiff'.
--
-- Future headers that don't exceed the clock skew
-- ('inFutureExceedsClockSkew') are added to 'cdbFutureBlocks'.
--
-- Future headers that exceed the clock skew are added to 'cdbInvalid' with
-- 'InFutureExceedsClockSkew' as the reason.
--
-- When truncation happened, 'Left' is returned, otherwise 'Right'.
futureCheckCandidate
  :: forall m blk. (IOLike m, LedgerSupportsProtocol blk)
  => ChainSelEnv m blk
  -> ValidatedChainDiff (Header blk) (K (LedgerDB' blk))
  -> m (Either (ChainDiff (Header blk))
               (ValidatedChainDiff (Header blk) (K (LedgerDB' blk))))
futureCheckCandidate chainSelEnv validatedChainDiff =
    checkInFuture futureCheck validatedSuffix >>= \case

      (suffix', []) ->
        -- If no headers are in the future, then the fragment must be untouched
        assert (AF.headPoint suffix == AF.headPoint suffix') $
        return $ Right validatedChainDiff

      (suffix', inFuture) -> do
        let (exceedClockSkew, inNearFuture) =
              partition InFuture.inFutureExceedsClockSkew inFuture
        -- Record future blocks
        unless (null inNearFuture) $ do
          let futureBlocks = Map.fromList
                [ (headerHash hdr, (hdr, InFuture.inFuturePunish x))
                | x <- inNearFuture
                , let hdr = InFuture.inFutureHeader x
                ]
          atomically $ modifyTVar varFutureBlocks $ flip Map.union futureBlocks
          -- Trace the original @suffix@, as it contains the headers from the
          -- future
          traceWith validationTracer $
            CandidateContainsFutureBlocks
              suffix
              (InFuture.inFutureHeader <$> inNearFuture)

        -- Record any blocks exceeding the clock skew as invalid
        unless (null exceedClockSkew) $ do
          let invalidHeaders = InFuture.inFutureHeader <$> exceedClockSkew
              invalidBlocks  = Map.fromList
                [ (headerHash hdr, info)
                | hdr <- invalidHeaders
                , let reason = InFutureExceedsClockSkew (headerRealPoint hdr)
                      info   = InvalidBlockInfo reason (blockSlot hdr)
                ]
          atomically $ modifyTVar varInvalid $ \(WithFingerprint invalid fp) ->
            WithFingerprint (Map.union invalid invalidBlocks) (succ fp)
          traceWith validationTracer $
            CandidateContainsFutureBlocksExceedingClockSkew
              -- Trace the original @suffix@, as it contains the headers
              -- from the future
              suffix
              invalidHeaders

          -- It's possible the block's prefix is invalid, but we can't know for
          -- sure. So we use 'InvalidBlockPunishment.BlockItself' to be more
          -- conservative.
          forM_ exceedClockSkew $ \x -> do
            InvalidBlockPunishment.enact
              (InFuture.inFuturePunish x)
              InvalidBlockPunishment.BlockItself

        -- Truncate the original 'ChainDiff' to match the truncated
        -- 'AnchoredFragment'.
        return $ Left $ Diff.truncate (castPoint (AF.headPoint suffix')) chainDiff
  where
    ChainSelEnv { validationTracer, varInvalid, varFutureBlocks, futureCheck } =
      chainSelEnv

    ValidatedChainDiff chainDiff@(ChainDiff _ suffix) _ = validatedChainDiff

    validatedSuffix :: ValidatedFragment (Header blk) (LedgerState blk)
    validatedSuffix =
      let
        validatedChainDiff' = ValidatedDiff.toValidatedFragment validatedChainDiff
      in
        validatedChainDiff' {
          VF.validatedLedger = ledgerState
                             . LgrDB.current
                             . unK
                             . VF.validatedLedger
                             $ validatedChainDiff'
                            }


-- | Validate a candidate chain using 'ledgerValidateCandidate' and
-- 'futureCheck'.
validateCandidate
  :: ( IOLike m
     , LedgerSupportsProtocol blk
     , HasCallStack
     )
  => ChainSelEnv m blk
  -> ChainDiff (Header blk)
  -> m (ValidationResult blk)
validateCandidate chainSelEnv chainDiff =
    ledgerValidateCandidate chainSelEnv chainDiff >>= \case
      validatedChainDiff
        | ValidatedDiff.rollbackExceedsSuffix validatedChainDiff
        -> return InsufficientSuffix
        | otherwise
        -> futureCheckCandidate chainSelEnv validatedChainDiff >>= \case
          Left chainDiff'
              | Diff.rollbackExceedsSuffix chainDiff'
              -> return InsufficientSuffix
              | otherwise
              -> return $ ValidPrefix chainDiff'
          Right validatedChainDiff'
              | ValidatedDiff.rollbackExceedsSuffix validatedChainDiff'
              -> return InsufficientSuffix
              | AF.length (Diff.getSuffix chainDiff) ==
                AF.length (Diff.getSuffix chainDiff')
                -- No truncation
              -> return $ FullyValid validatedChainDiff'
              | otherwise
                -- In case of invalid blocks but no blocks from the future, we
                -- throw away the ledger corresponding to the truncated
                -- fragment and will have to validate it again, even when it's
                -- the sole candidate.
              -> return $ ValidPrefix chainDiff'
            where
              chainDiff' = ValidatedDiff.getChainDiff validatedChainDiff'

{-------------------------------------------------------------------------------
  'ChainAndLedger'
-------------------------------------------------------------------------------}

-- | Instantiate 'ValidatedFragment' in the way that chain selection requires.
type ChainAndLedger blk = ValidatedFragment (Header blk) (K (LedgerDB' blk))

{-------------------------------------------------------------------------------
  Diffusion pipelining
-------------------------------------------------------------------------------}

-- | Check whether a 'ChainDiff' can be pipelined. If it can, the tentative
-- header is returned.
--
-- PRECONDITION: The 'ChainDiff' fits on top of the current chain and is better.
isPipelineable ::
     LedgerSupportsProtocol blk
  => BlockConfig blk
  -> TentativeState blk
  -> ChainDiff (Header blk)
  -> StrictMaybe (Header blk)
isPipelineable bcfg tentativeState ChainDiff {..}
  | -- we apply exactly one header
    AF.Empty _ :> hdr <- getSuffix
  , preferToLastInvalidTentative bcfg tentativeState hdr
    -- ensure that the diff is applied to the chain tip
  , getRollback == 0
  = SJust hdr
  | otherwise = SNothing

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
  -> (ChainHash blk -> Set (HeaderHash blk))
  -> (ChainHash blk -> Set (HeaderHash blk))
ignoreInvalidSuc _ invalid succsOf =
    Set.filter (`Map.notMember` invalid) . succsOf

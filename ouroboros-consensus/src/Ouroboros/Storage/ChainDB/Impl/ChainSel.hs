{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE MultiWayIf           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wredundant-constraints #-}
-- | Operations involving chain selection: the initial chain selection and
-- adding a block.
module Ouroboros.Storage.ChainDB.Impl.ChainSel
  ( initialChainSelection
  , addBlock
    -- * Type for in-sync chain and ledger
  , ChainAndLedger -- Opaque
  , clChain
  , clLedger
    -- * Helpers
  , getImmBlockNo
  ) where

import           Control.Exception (assert)
import           Control.Monad (unless)
import           Control.Monad.Except
import           Control.Monad.Trans.State.Strict
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

import           Control.Monad.Class.MonadThrow
import           Control.Tracer (Tracer, contramap, traceWith)

import           Ouroboros.Network.AnchoredFragment (AnchoredFragment (..))
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (BlockNo, HasHeader (..), HeaderHash,
                     Point, SlotNo, blockPoint, castPoint, pointHash)
import           Ouroboros.Network.Point (WithOrigin)

import           Ouroboros.Consensus.Block (BlockProtocol, GetHeader (..))
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.STM (WithFingerprint (..))

import           Ouroboros.Storage.ChainDB.Impl.ImmDB (ImmDB)
import qualified Ouroboros.Storage.ChainDB.Impl.ImmDB as ImmDB
import           Ouroboros.Storage.ChainDB.Impl.LgrDB (LgrDB)
import qualified Ouroboros.Storage.ChainDB.Impl.LgrDB as LgrDB
import qualified Ouroboros.Storage.ChainDB.Impl.Query as Query
import qualified Ouroboros.Storage.ChainDB.Impl.Reader as Reader
import           Ouroboros.Storage.ChainDB.Impl.Types
import           Ouroboros.Storage.ChainDB.Impl.VolDB (VolDB)
import qualified Ouroboros.Storage.ChainDB.Impl.VolDB as VolDB

-- | Perform the initial chain selection based on the tip of the ImmutableDB
-- and the contents of the VolatileDB.
--
-- Returns the chosen validated chain and corresponding ledger.
--
-- See "## Initialization" in ChainDB.md.
initialChainSelection
  :: forall m blk. (IOLike m, ProtocolLedgerView blk)
  => ImmDB m blk
  -> VolDB m blk
  -> LgrDB m blk
  -> Tracer m (TraceEvent blk)
  -> NodeConfig (BlockProtocol blk)
  -> StrictTVar m (WithFingerprint (Map (HeaderHash blk) SlotNo)) -- ^ 'cdbInvalid'
  -> m (ChainAndLedger blk)
initialChainSelection immDB volDB lgrDB tracer cfg varInvalid = do
    -- We follow the steps from section "## Initialization" in ChainDB.md

    i :: Point blk <- ImmDB.getPointAtTip immDB
    (succsOf, ledger) <- atomically $ do
      invalid <- forgetFingerprint <$> readTVar varInvalid
      (,) <$> (ignoreInvalidSuc volDB invalid <$> VolDB.getSuccessors volDB)
          <*> LgrDB.getCurrent lgrDB

    chains <- constructChains i succsOf

    -- We use the empty fragment anchored at @i@ as the current chain (and
    -- ledger) and the default in case there is no better candidate.
    let curChainAndLedger = mkChainAndLedger (Empty (castPoint i)) ledger

    case NE.nonEmpty chains of
      -- If there are no candidates, no chain selection is needed
      Nothing      -> return curChainAndLedger
      Just chains' -> fromMaybe curChainAndLedger <$>
        chainSelection' curChainAndLedger chains'
  where
    -- | Use the VolatileDB to construct all chains starting from the tip of
    -- the ImmutableDB.
    constructChains :: Point blk -- ^ Tip of the ImmutableDB, @i@
                    -> (WithOrigin (HeaderHash blk) -> Set (HeaderHash blk))
                    -> m [AnchoredFragment (Header blk)]
    constructChains i succsOf = flip evalStateT Map.empty $
        mapM constructChain suffixesAfterI
      where
        suffixesAfterI :: [NonEmpty (HeaderHash blk)]
        suffixesAfterI = VolDB.candidates succsOf i

        constructChain :: NonEmpty (HeaderHash blk)
                       -> StateT (Map (HeaderHash blk) (Header blk))
                                 m
                                 (AnchoredFragment (Header blk))
        constructChain hashes =
          AF.fromOldestFirst (castPoint i) <$>
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
    chainSelection' curChainAndLedger@(ChainAndLedger curChain ledger) candidates =
      assert (all ((LgrDB.currentPoint ledger ==) .
                   castPoint . AF.anchorPoint)
                  candidates) $
      -- Since all these candidates are longer than the current chain, they
      -- will be preferred over it. However, in the future, it might no longer
      -- be that a longer chain always implies that 'preferCandidate' is true.
      assert (all (preferCandidate cfg curChain) candidates) $
      chainSelection
        lgrDB
        (contramap (TraceInitChainSelEvent . InitChainSelValidation) tracer)
        cfg
        varInvalid
        curChainAndLedger
        (fmap (mkCandidateSuffix 0) candidates)

-- | Add a block to the ChainDB.
--
-- This is the only operation that actually changes the ChainDB. It will add
-- store the block on disk and trigger chain selection, possibly switching to
-- a fork.
--
-- PRECONDITION: The slot number of the new block may not be too far in the
-- future: @blockSlot b <= currentSlot + maxSkew@ where @b@ is the new block,
-- @currentSlot@ is the current slot according to @BlockchainTime@ and
-- @maxSkew@ is the maximum allowed @ClockSkew@. The chain sync client
-- guarantees this.
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
addBlock
  :: forall m blk.
     ( IOLike m
     , HasHeader blk
     , ProtocolLedgerView blk
     , HasCallStack
     )
  => ChainDbEnv m blk
  -> blk
  -> m ()
addBlock cdb@CDB{..} b = do
    (isMember, curChain, tipPoint, ledgerDB, invalid, immBlockNo) <-
      atomically $ (,,,,,)
        <$> VolDB.getIsMember           cdbVolDB
        <*> Query.getCurrentChain       cdb
        <*> Query.getTipPoint           cdb
        <*> LgrDB.getCurrent            cdbLgrDB
        <*> (forgetFingerprint <$> readTVar cdbInvalid)
        <*> readTVar                    cdbImmBlockNo
    let curChainAndLedger =
          -- The current chain we're working with here is not longer than @k@
          -- blocks (see 'getCurrentChain' and 'cdbChain'), which is easier to
          -- reason about when doing chain selection, etc.
          assert (fromIntegral (AF.length curChain) <= k) $
          mkChainAndLedger curChain ledgerDB
        -- See 'getCurrentChain'
        i :: Point blk
        i = castPoint $ AF.anchorPoint curChain

    -- We follow the steps from section "## Adding a block" in ChainDB.md

    -- ### Ignore
    unless ((blockNo b <= immBlockNo && blockNo b /= 0) ||
            isMember (blockHash b)  ||
            Map.member (blockHash b) invalid) $ do

      -- Write the block to the VolatileDB in all other cases
      VolDB.putBlock cdbVolDB b
      trace $ AddedBlockToVolDB (blockPoint b) (blockNo b) (cdbIsEBB b)

      -- We need to get these after adding the block to the VolatileDB
      (isMember', succsOf, predecessor) <- atomically $
         (,,) <$> (ignoreInvalid    cdb invalid <$> VolDB.getIsMember    cdbVolDB)
              <*> (ignoreInvalidSuc cdb invalid <$> VolDB.getSuccessors  cdbVolDB)
              <*> VolDB.getPredecessor cdbVolDB

      -- The block @b@ fits onto the end of our current chain
      if | pointHash tipPoint == blockPrevHash b -> do
           -- ### Add to current chain
           trace (TryAddToCurrentChain (blockPoint b))
           addToCurrentChain succsOf curChainAndLedger

         | Just hashes <- VolDB.isReachable predecessor isMember'
             i (blockPoint b) -> do
           -- ### Switch to a fork
           trace (TrySwitchToAFork (blockPoint b) hashes)
           switchToAFork succsOf curChainAndLedger hashes

         | otherwise ->
           -- ### Store but don't change the current chain

           -- We have already stored the block in the VolatileDB
           trace (StoreButDontChange (blockPoint b))

      -- Note that we may have extended the chain, but have not trimmed it to
      -- @k@ blocks/headers. That is the job of the background thread, which
      -- will first copy the blocks/headers to trim (from the end of the
      -- fragment) from the VolatileDB to the ImmutableDB.

  where
    secParam@(SecurityParam k) = protocolSecurityParam cdbNodeConfig

    trace :: TraceAddBlockEvent blk -> m ()
    trace = traceWith (contramap TraceAddBlockEvent cdbTracer)

    -- | PRECONDITION: the header @hdr@ (and block @b@) fit onto the end of
    -- the current chain.
    addToCurrentChain :: HasCallStack
                      => (WithOrigin (HeaderHash blk) -> Set (HeaderHash blk))
                      -> ChainAndLedger blk
                         -- ^ The current chain and ledger
                      -> m ()
    addToCurrentChain succsOf curChainAndLedger@(ChainAndLedger curChain _) =
        assert (AF.validExtension curChain hdr) $ do
          let suffixesAfterB = VolDB.candidates succsOf (blockPoint b)

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
                hdrs <- mapM (getKnownHeaderThroughCache cdbVolDB) $
                  NE.toList hashes
                return $ AF.fromOldestFirst curHead (hdr : hdrs)

          let candidateSuffixes = NE.nonEmpty
                $ NE.filter (preferCandidate cdbNodeConfig curChain . _suffix)
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
            Nothing                 -> return ()
            Just candidateSuffixes' ->
              chainSelection' curChainAndLedger candidateSuffixes' >>= \case
                Nothing                -> return ()
                Just newChainAndLedger -> trySwitchTo newChainAndLedger
      where
        curHead = AF.headPoint curChain
        hdr     = getHeader b

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
                  -> m ()
    switchToAFork succsOf curChainAndLedger@(ChainAndLedger curChain _) hashes = do
        let suffixesAfterB = VolDB.candidates succsOf (blockPoint b)
            initCache      = Map.insert (blockHash b) hdr (cacheHeaders curChain)
        -- Fragments that are anchored at @i@.
        candidates <- flip evalStateT initCache $
          case NE.nonEmpty suffixesAfterB of
            -- If there are no suffixes after @b@, just use the fragment that
            -- ends in @b@ as the sole candidate.
            Nothing              -> (NE.:| []) <$> constructFork i hashes []
            Just suffixesAfterB' -> mapM (constructFork i hashes . NE.toList)
                                         suffixesAfterB'
        let candidateSuffixes =
                -- The suffixes all fork off from the current chain within @k@
                -- blocks, so it satisfies the precondition of
                -- 'preferCandidate'.
                filter (preferCandidate cdbNodeConfig curChain . _suffix)
              . mapMaybe (intersectCandidateSuffix curChain)
              $ NE.toList candidates

        case NE.nonEmpty candidateSuffixes of
          -- No candidates preferred over the current chain
          Nothing                 -> return ()
          Just candidateSuffixes' ->
            chainSelection' curChainAndLedger candidateSuffixes' >>= \case
              Nothing                -> return ()
              Just newChainAndLedger -> trySwitchTo newChainAndLedger
      where
        i   = castPoint $ anchorPoint curChain
        hdr = getHeader b


    -- | 'chainSelection' partially applied to the parameters from the
    -- 'ChainDbEnv'.
    chainSelection'
      :: ChainAndLedger blk              -- ^ The current chain and ledger
      -> NonEmpty (CandidateSuffix blk)  -- ^ Candidates
      -> m (Maybe (ChainAndLedger blk))
    chainSelection' = chainSelection
      cdbLgrDB
      (contramap (TraceAddBlockEvent . AddBlockValidation) cdbTracer)
      cdbNodeConfig
      cdbInvalid

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
    trySwitchTo :: HasCallStack
                => ChainAndLedger blk  -- ^ Chain and ledger to switch to
                -> m ()
    trySwitchTo (ChainAndLedger newChain newLedger) = do
      (curChain, switched) <- atomically $ do
        curChain <- readTVar cdbChain
        case AF.intersect curChain newChain of
          Just (curChainPrefix, _newChainPrefix, curChainSuffix, newChainSuffix)
            | fromIntegral (AF.length curChainSuffix) <= k
            , preferCandidate cdbNodeConfig curChain newChain
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
              modifyTVar cdbImmBlockNo $ getImmBlockNo secParam newChain'

              -- Update the readers
              -- 'Reader.switchFork' needs to know the intersection point
              -- (@ipoint@) between the old and the current chain.
              let ipoint = castPoint $ AF.anchorPoint newChainSuffix
              varReaders <- Map.elems <$> readTVar cdbReaders
              forM_ varReaders $ \varReader -> modifyTVar varReader $
                Reader.switchFork ipoint newChain'

              return (curChain, True)
          -- The chain must be changed in the meantime such that our chain is
          -- no longer preferred.
          _ -> return (curChain, False)
      trace $ if switched
              then SwitchedToChain  curChain newChain
              else ChainChangedInBg curChain newChain

    -- | Build a cache from the headers in the fragment.
    cacheHeaders :: AnchoredFragment (Header blk)
                 -> Map (HeaderHash blk) (Header blk)
    cacheHeaders =
      foldl' (\m hdr -> Map.insert (blockHash hdr) hdr m) Map.empty .
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
    --
    -- Note that we need to read the headers corresponding to the hashes
    -- @(i,b]@ and @(b,?]@ from disk. It is likely that many of these headers
    -- are actually on the current chain, so when possible, we reuse these
    -- headers instead of reading them from disk.
    constructFork
      :: Point blk                  -- ^ Tip of ImmutableDB @i@
      -> NonEmpty (HeaderHash blk)  -- ^ Hashes of @(i,b]@
      -> [HeaderHash blk]           -- ^ Suffix @s@, hashes of @(b,?]@
      -> StateT (Map (HeaderHash blk) (Header blk))
                m
                (AnchoredFragment (Header blk))
         -- ^ Fork, anchored at @i@, contains (the header of) @b@ and ends
         -- with the suffix @s@.
    constructFork i hashes suffixHashes
      = fmap (AF.fromOldestFirst (castPoint i))
      $ mapM (getKnownHeaderThroughCache cdbVolDB)
      $ NE.toList hashes <> suffixHashes


-- | Check whether the header for the hash is in the cache, if not, get
-- the corresponding header from the VolatileDB and store it in the cache.
--
-- PRECONDITION: the header (block) must exist in the VolatileDB.
getKnownHeaderThroughCache
  :: (MonadCatch m, HasHeader blk, GetHeader blk)
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
     , ProtocolLedgerView blk
     , HasCallStack
     )
  => LgrDB m blk
  -> Tracer m (TraceValidationEvent blk)
  -> NodeConfig (BlockProtocol blk)
  -> StrictTVar m (WithFingerprint (Map (HeaderHash blk) SlotNo))
     -- ^ The invalid blocks
  -> ChainAndLedger blk              -- ^ The current chain and ledger
  -> NonEmpty (CandidateSuffix blk)  -- ^ Candidates
  -> m (Maybe (ChainAndLedger blk))
     -- ^ The (valid) chain and corresponding LedgerDB that was selected, or
     -- 'Nothing' if there is no valid chain preferred over the current
     -- chain.
chainSelection lgrDB tracer cfg varInvalid
               curChainAndLedger@(ChainAndLedger curChain _) candidates =
  assert (all (isJust . fitCandidateSuffixOn curChain) candidates) $
    go (sortCandidates (NE.toList candidates))
  where
    sortCandidates :: [CandidateSuffix blk] -> [CandidateSuffix blk]
    sortCandidates =
      sortBy (flip (compareCandidates cfg) `on` _suffix)

    validate :: ChainAndLedger  blk  -- ^ Current chain and ledger
             -> CandidateSuffix blk  -- ^ Candidate fragment
             -> m (Maybe (ChainAndLedger blk))
    validate = validateCandidate lgrDB tracer cfg varInvalid

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
        Just newChainAndLedger@(ChainAndLedger candChain _)
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
      return $ filter (preferCandidate cfg curChain . _suffix)
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
     , ProtocolLedgerView blk
     , HasCallStack
     )
  => LgrDB m blk
  -> Tracer m (TraceValidationEvent blk)
  -> NodeConfig (BlockProtocol blk)
  -> StrictTVar m (WithFingerprint (Map (HeaderHash blk) SlotNo))
     -- ^ The invalid blocks
  -> ChainAndLedger  blk                   -- ^ Current chain and ledger
  -> CandidateSuffix blk                   -- ^ Candidate fragment
  -> m (Maybe (ChainAndLedger blk))
validateCandidate lgrDB tracer cfg varInvalid
                  (ChainAndLedger curChain curLedger) candSuffix =
    LgrDB.validate lgrDB curLedger rollback newBlocks >>= \case
      LgrDB.MaximumRollbackExceeded supported _ -> do
        trace $ CandidateExceedsRollback {
            _supportedRollback = supported
          , _candidateRollback = _rollback candSuffix
          , _candidate         = _suffix   candSuffix
          }
        return Nothing
      LgrDB.RollbackSuccessful (LgrDB.InvalidBlock e pt ledger') -> do
        let lastValid  = castPoint $ LgrDB.currentPoint ledger'
            candidate' = fromMaybe
              (error "cannot rollback to point on fragment") $
              AF.rollback lastValid candidate
        addInvalidBlocks (hashesStartingFrom pt)
        trace (InvalidBlock e pt)

        -- The candidate is now a prefix of the original candidate, and might be
        -- shorter than (or as long as) the current chain. We must check again
        -- whether it is preferred over the current chain.
        if preferCandidate cfg curChain candidate'
          then do
            trace (ValidCandidate (_suffix candSuffix))
            return $ Just $ mkChainAndLedger candidate' ledger'
          else do
            trace (InvalidCandidate (_suffix candSuffix))
            return Nothing
      LgrDB.RollbackSuccessful (LgrDB.ValidBlocks ledger') -> do
        trace (ValidCandidate (_suffix candSuffix))
        return $ Just $ mkChainAndLedger candidate ledger'
  where
    trace = traceWith tracer
    CandidateSuffix rollback suffix = candSuffix
    newBlocks = AF.toOldestFirst suffix
    candidate = fromMaybe
      (error "candidate suffix doesn't fit on the current chain") $
      fitCandidateSuffixOn curChain candSuffix

    -- | Add the given map of invalid points to 'cdbInvalid' and change its
    -- fingerprint.
    addInvalidBlocks :: Map (HeaderHash blk) SlotNo -> m ()
    addInvalidBlocks invalidBlocksInCand = atomically $
      modifyTVar varInvalid $ \(WithFingerprint invalid fp) ->
        WithFingerprint (Map.union invalid invalidBlocksInCand) (succ fp)

    -- | Make a list of all the points after from the given point (inclusive)
    -- in the candidate fragment and turn it into a map from hash to slot that
    -- can be added to 'cdbInvalid'.
    --
    -- PRECONDITON: the given point is on the candidate fragment.
    hashesStartingFrom :: HasCallStack
                       => Point blk -> Map (HeaderHash blk) SlotNo
    hashesStartingFrom pt = case AF.splitBeforePoint suffix pt of
        Nothing                  -> error "point not on fragment"
        Just (_, startingFromPt) -> Map.fromList $
          map (\hdr -> (blockHash hdr, blockSlot hdr)) $
          AF.toOldestFirst startingFromPt

{-------------------------------------------------------------------------------
  Auxiliary data types for 'cdbAddBlock'
-------------------------------------------------------------------------------}

-- | Auxiliary data type for 'cdbAddBlock' for a chain with a ledger that
-- corresponds to it.
--
-- INVARIANT:
-- > for (x :: ChainAndLedger blk),
-- >   AF.headPoint (_chain x) == LgrDB.currentPoint (_ledger x)
data ChainAndLedger blk = ChainAndLedger
  { clChain  :: !(AnchoredFragment (Header blk))
    -- ^ Chain fragment
  , clLedger :: !(LgrDB.LedgerDB blk)
    -- ^ Ledger corresponding to '_chain'
  }

mkChainAndLedger :: (HasHeader blk , UpdateLedger blk)
                 => AnchoredFragment (Header blk) -> LgrDB.LedgerDB blk
                 -> ChainAndLedger blk
mkChainAndLedger c l =
    assert (castPoint (AF.headPoint c) == LgrDB.currentPoint l) $
    ChainAndLedger c l

-- | Auxiliary data type for 'cdbAddBlock' for a candidate suffix.
--
-- INVARIANT: the length of the suffix must always be >= the rollback
data CandidateSuffix blk = CandidateSuffix
  { _rollback :: !Word64
    -- ^ The number of headers to roll back the current chain
  , _suffix   :: !(AnchoredFragment (Header blk))
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
-- the candidate suffix ('_suffix').
--
-- PRECONDITION: the length of the suffix rolled back to the given point must
-- be >= the rollback ('_rollback').
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


-- | Wrap an @isMember@ function so that it returns 'False' for invalid
-- blocks.
ignoreInvalid
  :: HasHeader blk
  => proxy blk
  -> Map (HeaderHash blk) SlotNo  -- ^ 'cdbInvalid'
  -> (HeaderHash blk -> Bool)
  -> (HeaderHash blk -> Bool)
ignoreInvalid _ invalid isMember hash
    | Map.member hash invalid = False
    | otherwise               = isMember hash

-- | Wrap a @successors@ function so that invalid blocks are not returned as
-- successors.
ignoreInvalidSuc
  :: HasHeader blk
  => proxy blk
  -> Map (HeaderHash blk) SlotNo  -- ^ 'cdbInvalid'
  -> (WithOrigin (HeaderHash blk) -> Set (HeaderHash blk))
  -> (WithOrigin (HeaderHash blk) -> Set (HeaderHash blk))
ignoreInvalidSuc _ invalid succsOf =
    Set.filter (`Map.notMember` invalid) . succsOf

-- | Get the 'BlockNo' corresponding to the block @k@ blocks back in the given
-- new chain, i.e. the most recent \"immutable\" block.
--
-- As the given chain might be shorter than @k@ (see 'cdbImmBlockNo' for why),
-- the 'BlockNo' of the previous \"immutable\" block must be passed, since in
-- that case, it will not change.
--
-- Also, when switching to a chain that is exactly as long as the current
-- chain, there will be no header on the fragment @k@ blocks back, only the
-- anchor point. In that case, the \"immutable\" block will also not change.
getImmBlockNo
  :: HasHeader (Header blk)
  => SecurityParam
  -> AnchoredFragment (Header blk)  -- ^ New chain
  -> BlockNo  -- ^ 'BlockNo' of the previous \"immutable\" block
  -> BlockNo
getImmBlockNo (SecurityParam k) chain prevBlockNo =
    fromMaybe prevBlockNo $
    AF.headBlockNo $ AF.dropNewest (fromIntegral k) chain

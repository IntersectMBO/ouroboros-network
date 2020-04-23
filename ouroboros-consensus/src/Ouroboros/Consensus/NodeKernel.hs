{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -Werror=missing-fields #-}

module Ouroboros.Consensus.NodeKernel (
    -- * Node kernel
    NodeKernel (..)
  , BlockProduction (..)
  , MaxBlockSizeOverride (..)
  , MempoolCapacityBytesOverride (..)
  , NodeArgs (..)
  , TraceForgeEvent (..)
  , initNodeKernel
  , getMempoolReader
  , getMempoolWriter
  ) where

import           Control.Monad
import           Control.Monad.Except
import           Data.Map.Strict (Map)
import           Data.Maybe (isJust)
import           Data.Proxy
import           Data.Word (Word32)

import           Control.Tracer

import           Ouroboros.Network.AnchoredFragment (AnchoredFragment (..))
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block
import           Ouroboros.Network.BlockFetch
import           Ouroboros.Network.BlockFetch.State (FetchMode (..))
import           Ouroboros.Network.NodeToNode (MiniProtocolParameters (..))
import           Ouroboros.Network.Point (WithOrigin (..))
import           Ouroboros.Network.TxSubmission.Inbound
                     (TxSubmissionMempoolWriter)
import qualified Ouroboros.Network.TxSubmission.Inbound as Inbound
import           Ouroboros.Network.TxSubmission.Mempool.Reader
                     (TxSubmissionMempoolReader)
import qualified Ouroboros.Network.TxSubmission.Mempool.Reader as MempoolReader

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Forecast
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Mempool
import           Ouroboros.Consensus.Mempool.TxSeq (TicketNo)
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Node.State
import           Ouroboros.Consensus.Node.Tracers
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util (whenJust)
import           Ouroboros.Consensus.Util.AnchoredFragment
import           Ouroboros.Consensus.Util.EarlyExit
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Consensus.Util.Random
import           Ouroboros.Consensus.Util.ResourceRegistry
import           Ouroboros.Consensus.Util.STM

import           Ouroboros.Consensus.Storage.ChainDB.API (ChainDB)
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB

{-------------------------------------------------------------------------------
  Relay node
-------------------------------------------------------------------------------}

-- | Interface against running relay node
data NodeKernel m remotePeer localPeer blk = NodeKernel {
      -- | The 'ChainDB' of the node
      getChainDB             :: ChainDB m blk

      -- | The node's mempool
    , getMempool             :: Mempool m blk TicketNo

      -- | The node's top-level static configuration
    , getTopLevelConfig      :: TopLevelConfig blk

      -- | The fetch client registry, used for the block fetch clients.
    , getFetchClientRegistry :: FetchClientRegistry remotePeer (Header blk) blk m

      -- | Read the current candidates
    , getNodeCandidates      :: StrictTVar m (Map remotePeer (StrictTVar m (AnchoredFragment (Header blk))))

      -- | The node's tracers
    , getTracers             :: Tracers m remotePeer localPeer blk
    }

-- | Callbacks required to produce blocks
data BlockProduction m blk = BlockProduction {
      -- | Produce a block
      --
      -- The function is passed the contents of the mempool; this is a set of
      -- transactions that is guaranteed to be consistent with the ledger state
      -- (also provided as an argument) and with each other (when applied in
      -- order). In principle /all/ of them could be included in the block (up
      -- to maximum block size).
      --
      -- Note that this function is not run in @m@, but in some monad @n@
      -- which only has the ability to produce random number and access to the
      -- 'NodeState'.
      produceBlock :: forall n. MonadRandom n
                   => (forall a. m a -> n a)
                   -- Lift actions into @n@
                   --
                   -- This allows block production to execute arbitrary side
                   -- effects; this is primarily useful for tests.

                   -> Update n (NodeState blk)
                   -> BlockNo               -- Current block number
                   -> TickedLedgerState blk -- Current ledger state
                   -> [GenTx blk]           -- Contents of the mempool
                   -> IsLeader (BlockProtocol blk) -- Proof we are leader
                   -> n blk

      -- | How to run a computation requiring 'MonadRandom'.
      --
      -- When @m = IO@, this can be 'runMonadRandomIO', because the
      -- 'MonadRandom' instance for 'IO' can be used.
      --
      -- In the tests, we can simulate a 'MonadRandom' by keeping track of a
      -- DRG in a 'TVar'.
    , runMonadRandomDict :: RunMonadRandom m
    }

-- | An override for the maximum block size from the protocol parameters in
-- the ledger.
data MaxBlockSizeOverride
  = NoOverride
    -- ^ Use the maximum block size in bytes from the protocol parameters in
    -- the ledger.
  | MaxBlockSize !Word32
    -- ^ Use the following maximum size in bytes for the whole block.
  | MaxBlockBodySize !Word32
    -- ^ Use the following maximum size in bytes for the block body.

-- | An override for the default 'MempoolCapacityBytes' which is 2x the
-- maximum block size from the protocol parameters in the ledger.
data MempoolCapacityBytesOverride
  = NoMempoolCapacityBytesOverride
    -- ^ Use 2x the maximum block size from the protocol parameters in the
    -- ledger.
  | MempoolCapacityBytesOverride !MempoolCapacityBytes
    -- ^ Use the following 'MempoolCapacityBytes'.

-- | Arguments required when initializing a node
data NodeArgs m remotePeer localPeer blk = NodeArgs {
      tracers                :: Tracers m remotePeer localPeer blk
    , registry               :: ResourceRegistry m
    , maxClockSkew           :: ClockSkew
    , cfg                    :: TopLevelConfig blk
    , initState              :: NodeState blk
    , btime                  :: BlockchainTime m
    , chainDB                :: ChainDB m blk
    , initChainDB            :: TopLevelConfig blk -> ChainDB m blk -> m ()
    , blockFetchSize         :: Header blk -> SizeInBytes
    , blockProduction        :: Maybe (BlockProduction m blk)
    , blockMatchesHeader     :: Header blk -> blk -> Bool
    , maxBlockSize           :: MaxBlockSizeOverride
    , mempoolCap             :: MempoolCapacityBytesOverride
    , miniProtocolParameters :: MiniProtocolParameters
    }

initNodeKernel
    :: forall m remotePeer localPeer blk.
       ( IOLike m
       , RunNode blk
       , NoUnexpectedThunks remotePeer
       , Ord remotePeer
       )
    => NodeArgs m remotePeer localPeer blk
    -> m (NodeKernel m remotePeer localPeer blk)
initNodeKernel args@NodeArgs { registry, cfg, tracers, maxBlockSize
                             , blockProduction, chainDB, initChainDB
                             , miniProtocolParameters } = do

    initChainDB cfg chainDB

    st <- initInternalState args

    whenJust blockProduction $ forkBlockProduction maxBlockSize st

    let IS { blockFetchInterface, fetchClientRegistry, varCandidates,
             mempool } = st

    -- Run the block fetch logic in the background. This will call
    -- 'addFetchedBlock' whenever a new block is downloaded.
    void $ forkLinkedThread registry "NodeKernel.blockFetchLogic" $
      blockFetchLogic
        (blockFetchDecisionTracer tracers)
        (blockFetchClientTracer   tracers)
        blockFetchInterface
        fetchClientRegistry
        blockFetchConfiguration

    return NodeKernel
      { getChainDB             = chainDB
      , getMempool             = mempool
      , getTopLevelConfig      = cfg
      , getFetchClientRegistry = fetchClientRegistry
      , getNodeCandidates      = varCandidates
      , getTracers             = tracers
      }

  where
    blockFetchConfiguration :: BlockFetchConfiguration
    blockFetchConfiguration = BlockFetchConfiguration
      { bfcMaxConcurrencyBulkSync = 1 -- Set to 1 for now, see #1526
      , bfcMaxConcurrencyDeadline = 1
      , bfcMaxRequestsInflight    = blockFetchPipeliningMax miniProtocolParameters
      }

{-------------------------------------------------------------------------------
  Internal node components
-------------------------------------------------------------------------------}

data InternalState m remotePeer localPeer blk = IS {
      tracers             :: Tracers m remotePeer localPeer blk
    , cfg                 :: TopLevelConfig blk
    , registry            :: ResourceRegistry m
    , btime               :: BlockchainTime m
    , chainDB             :: ChainDB m blk
    , blockFetchInterface :: BlockFetchConsensusInterface remotePeer (Header blk) blk m
    , fetchClientRegistry :: FetchClientRegistry remotePeer (Header blk) blk m
    , varCandidates       :: StrictTVar m (Map remotePeer (StrictTVar m (AnchoredFragment (Header blk))))
    , varState            :: StrictTVar m (NodeState blk)
    , mempool             :: Mempool m blk TicketNo
    }

initInternalState
    :: forall m remotePeer localPeer blk.
       ( IOLike m
       , LedgerSupportsProtocol blk
       , Ord remotePeer
       , NoUnexpectedThunks remotePeer
       , RunNode blk
       )
    => NodeArgs m remotePeer localPeer blk
    -> m (InternalState m remotePeer localPeer blk)
initInternalState NodeArgs { tracers, chainDB, registry, cfg,
                             blockFetchSize, blockMatchesHeader, btime,
                             initState, mempoolCap } = do
    varCandidates  <- newTVarM mempty
    varState       <- newTVarM initState
    mpCap          <- atomically $ do
      -- If no override is provided, calculate the default mempool capacity as
      -- 2x the current ledger's maximum block size.
      --
      -- It's worth noting that even though the ledger's maximum block size is
      -- a dynamic value (it can be changed via an update proposal), we only
      -- calculate the default mempool capacity once here. i.e. if the
      -- ledger's maximum block size changes during runtime, the mempool
      -- capacity /will not/ automatically adapt. The mempool capacity would
      -- only be calculated again upon restarting the node.
      ledger <- ledgerState <$> ChainDB.getCurrentLedger chainDB
      pure (mempoolCapacity ledger)
    mempool        <- openMempool registry
                                  (chainDBLedgerInterface chainDB)
                                  (configLedger cfg)
                                  mpCap
                                  (mempoolTracer tracers)

    fetchClientRegistry <- newFetchClientRegistry

    let getCandidates :: STM m (Map remotePeer (AnchoredFragment (Header blk)))
        getCandidates = readTVar varCandidates >>= traverse readTVar

        blockFetchInterface :: BlockFetchConsensusInterface remotePeer (Header blk) blk m
        blockFetchInterface = initBlockFetchConsensusInterface
          cfg chainDB getCandidates blockFetchSize blockMatchesHeader btime

    return IS {..}
  where
    mempoolCapacity :: LedgerState blk -> MempoolCapacityBytes
    mempoolCapacity ledger = case mempoolCap of
        NoMempoolCapacityBytesOverride   -> noOverride
        MempoolCapacityBytesOverride mcb -> mcb
      where
        noOverride = MempoolCapacityBytes (nodeMaxBlockSize ledger * 2)

initBlockFetchConsensusInterface
    :: forall m peer blk. (IOLike m, BlockSupportsProtocol blk)
    => TopLevelConfig blk
    -> ChainDB m blk
    -> STM m (Map peer (AnchoredFragment (Header blk)))
    -> (Header blk -> SizeInBytes)
    -> (Header blk -> blk -> Bool)
    -> BlockchainTime m
    -> BlockFetchConsensusInterface peer (Header blk) blk m
initBlockFetchConsensusInterface cfg chainDB getCandidates blockFetchSize
    blockMatchesHeader btime = BlockFetchConsensusInterface {..}
  where
    readCandidateChains :: STM m (Map peer (AnchoredFragment (Header blk)))
    readCandidateChains = getCandidates

    readCurrentChain :: STM m (AnchoredFragment (Header blk))
    readCurrentChain = ChainDB.getCurrentChain chainDB

    readFetchMode :: STM m FetchMode
    readFetchMode = do
      curSlot      <- getCurrentSlot btime
      curChainSlot <- AF.headSlot <$> ChainDB.getCurrentChain chainDB
      let slotsBehind = case curChainSlot of
            -- There's nothing in the chain. If the current slot is 0, then
            -- we're 1 slot behind.
            Origin  -> unSlotNo curSlot + 1
            At slot -> unSlotNo curSlot - unSlotNo slot
          maxBlocksBehind = 5
          -- Convert from blocks to slots. This is more or less the @f@
          -- parameter, the frequency of blocks. TODO should be 10 for Praos,
          -- so make this part of 'OuroborosTag'.
          blocksToSlots = 1
      return $ if slotsBehind < maxBlocksBehind * blocksToSlots
       -- When the current chain is near to "now", use deadline mode, when it
       -- is far away, use bulk sync mode.
        then FetchModeDeadline
        else FetchModeBulkSync

    readFetchedBlocks :: STM m (Point blk -> Bool)
    readFetchedBlocks = ChainDB.getIsFetched chainDB

    -- Waits until the block has been written to disk, but not until chain
    -- selection has processed the block.
    addFetchedBlock :: Point blk -> blk -> m ()
    addFetchedBlock _pt = void . ChainDB.addBlockWaitWrittenToDisk chainDB

    readFetchedMaxSlotNo :: STM m MaxSlotNo
    readFetchedMaxSlotNo = ChainDB.getMaxSlotNo chainDB

    plausibleCandidateChain :: AnchoredFragment (Header blk)
                            -> AnchoredFragment (Header blk)
                            -> Bool
    plausibleCandidateChain = preferAnchoredCandidate cfg

    compareCandidateChains :: AnchoredFragment (Header blk)
                           -> AnchoredFragment (Header blk)
                           -> Ordering
    compareCandidateChains = compareAnchoredCandidates cfg

forkBlockProduction
    :: forall m remotePeer localPeer blk.
       (IOLike m, RunNode blk)
    => MaxBlockSizeOverride
    -> InternalState m remotePeer localPeer blk
    -> BlockProduction m blk
    -> m ()
forkBlockProduction maxBlockSizeOverride IS{..} BlockProduction{..} =
    void $ onKnownSlotChange registry btime "NodeKernel.blockProduction" $
      withEarlyExit_ . go
  where
    RunMonadRandom{..} = runMonadRandomDict

    go :: SlotNo -> WithEarlyExit m ()
    go currentSlot = do
        trace $ TraceStartLeadershipCheck currentSlot

        -- Figure out which block to connect to
        --
        -- Normally this will be the current block at the tip, but it may
        -- be the /previous/ block, if there were multiple slot leaders
        BlockContext{bcBlockNo, bcPrevPoint} <- do
          eBlkCtx <- lift $ atomically $
            mkCurrentBlockContext currentSlot
                <$> ChainDB.getCurrentChain chainDB
          case eBlkCtx of
            Right blkCtx -> return blkCtx
            Left failure -> do
              trace failure
              exitEarly

        -- Get ledger state corresponding to bcPrevPoint
        --
        -- This might fail if, in between choosing 'bcPrevPoint' and this call to
        -- 'getPastLedger', we switched to a fork where 'bcPrevPoint' is no longer
        -- on our chain. When that happens, we simply give up on the chance to
        -- produce a block.
        extLedger <- do
          mExtLedger <- lift $ ChainDB.getPastLedger chainDB bcPrevPoint
          case mExtLedger of
            Just l  -> return l
            Nothing -> do
              trace $ TraceNoLedgerState currentSlot bcPrevPoint
              exitEarly
        let unticked = ledgerState extLedger

        -- Check if we are not too far ahead of the chain
        --
        -- TODO: This check is not strictly necessary, but omitting it breaks
        -- the consensus tests at the moment.
        -- <https://github.com/input-output-hk/ouroboros-network/issues/1941>
        case runExcept $ forecastFor
                           (ledgerViewForecastAtTip (configLedger cfg) unticked)
                           currentSlot of
          Left err -> do
            -- There are so many empty slots between the tip of our chain and
            -- the current slot that we cannot get an ledger view anymore
            -- In principle, this is no problem; we can still produce a block
            -- (we use the ticked ledger state). However, we probably don't
            -- /want/ to produce a block in this case; we are most likely
            -- missing a blocks on our chain.
            trace $ TraceNoLedgerView currentSlot err
            exitEarly
          Right _ ->
            return ()

        -- Tick the ledger state for the 'SlotNo' we're producing a block for
        let ticked = applyChainTick (configLedger cfg) currentSlot unticked

        -- Check if we are the leader
        proof <- do
          let ledgerView = protocolLedgerView
                             (configLedger cfg)
                             (tickedLedgerState ticked)
          mIsLeader :: Maybe (IsLeader (BlockProtocol blk)) <- lift $
            runMonadRandom $ \_lift' ->
              checkIsLeader
                (configConsensus cfg)
                currentSlot
                ledgerView
                (headerStateConsensus (headerState extLedger))
          case mIsLeader of
            Just p  -> return p
            Nothing -> do
              trace $ TraceNodeNotLeader currentSlot
              exitEarly

        -- At this point we have established that we are indeed slot leader
        trace $ TraceNodeIsLeader currentSlot

        -- Get a snapshot of the mempool that is consistent with the ledger
        --
        -- NOTE: It is possible that due to adoption of new blocks the
        -- /current/ ledger will have changed. This doesn't matter: we will
        -- produce a block that fits onto the ledger we got above; if the
        -- ledger in the meantime changes, the block we produce here may or
        -- may not be adopted, but it won't be invalid.
        mempoolSnapshot <- lift $ atomically $
                             getSnapshotFor
                               mempool
                               (ForgeInKnownSlot ticked)
        let txs = map fst $ snapshotTxsForSize
                              mempoolSnapshot
                              (maxBlockBodySize $ tickedLedgerState ticked)

        -- Actually produce the block
        newBlock <- lift $ runMonadRandom $ \lift' ->
          produceBlock
            lift'
            (updateFromTVar (castStrictTVar varState))
            bcBlockNo
            ticked
            txs
            proof
        trace $ TraceForgedBlock
                  currentSlot
                  (ledgerTipPoint (ledgerState extLedger))
                  newBlock
                  (snapshotMempoolSize mempoolSnapshot)

        -- Add the block to the chain DB
        result <- lift $ ChainDB.addBlockAsync chainDB newBlock
        -- Block until we have performed chain selection for the block
        curTip <- lift $ atomically $ ChainDB.chainSelectionPerformed result

        -- Check whether we adopted our block
        when (curTip /= blockPoint newBlock) $ do
          isInvalid <- lift $ atomically $
            ($ blockHash newBlock) . forgetFingerprint <$>
            ChainDB.getIsInvalidBlock chainDB
          case isInvalid of
            Nothing ->
              trace $ TraceDidntAdoptBlock currentSlot newBlock
            Just reason -> do
              trace $ TraceForgedInvalidBlock currentSlot newBlock reason
              -- We just produced a block that is invalid according to the
              -- ledger in the ChainDB, while the mempool said it is valid.
              -- There is an inconsistency between the two!
              --
              -- Remove all the transactions in that block, otherwise we'll
              -- run the risk of forging the same invalid block again. This
              -- means that we'll throw away some good transactions in the
              -- process.
              lift $ removeTxs mempool (map txId txs)
          exitEarly

        -- We successfully produced /and/ adopted a block
        trace $ TraceAdoptedBlock currentSlot newBlock txs

    trace :: TraceForgeEvent blk (GenTx blk) -> WithEarlyExit m ()
    trace = lift . traceWith (forgeTracer tracers)

    -- Compute maximum block size
    --
    -- We allow the overrides to /reduce/ the maximum size, but not increase it.
    -- This is important because the maximum block size could be reduced due to
    -- a protocol update, in which case any local configuration should not be
    -- allowed to increase it.
    maxBlockBodySize :: LedgerState blk -> Word32
    maxBlockBodySize ledger =
        min noOverride $ case maxBlockSizeOverride of
          NoOverride            -> noOverride
          MaxBlockSize     mbs  -> mbs - blockEncOverhead
          MaxBlockBodySize mbbs -> mbbs
      where
        blockEncOverhead = nodeBlockEncodingOverhead ledger
        noOverride       = nodeMaxBlockSize ledger - blockEncOverhead

-- | Context required to forge a block
data BlockContext blk = BlockContext
  { bcBlockNo   :: !BlockNo
    -- ^ the block number of the block to be forged
  , bcPrevPoint :: !(Point blk)
    -- ^ the point of /the predecessor of/ the block
    --
    -- Note that a block/header stores the hash of its predecessor but not the
    -- slot.
  }

-- | Create the 'BlockContext' from the header of the previous block
blockContextFromPrevHeader ::
     HasHeader (Header blk)
  => Header blk -> BlockContext blk
blockContextFromPrevHeader hdr =
    -- Recall that an EBB has the same block number as its predecessor, so this
    -- @succ@ is even correct when @hdr@ is an EBB.
    BlockContext (succ (blockNo hdr)) (headerPoint hdr)

-- | Determine the 'BlockContext' for a block about to be forged from the
-- current slot, ChainDB chain fragment, and ChainDB tip block number
--
-- The 'bcPrevPoint' will either refer to the header at the tip of the current
-- chain or, in case there is already a block in this slot (e.g. another node
-- was also elected leader and managed to produce a block before us), the tip's
-- predecessor. If the chain is empty, then it will refer to the chain's anchor
-- point, which may be genesis.
mkCurrentBlockContext
  :: forall blk. RunNode blk
  => SlotNo
     -- ^ the current slot, i.e. the slot of the block about to be forged
  -> AnchoredFragment (Header blk)
     -- ^ the current chain fragment
     --
     -- Recall that the anchor point is the tip of the ImmDB.
  -> Either (TraceForgeEvent blk (GenTx blk)) (BlockContext blk)
     -- ^ the event records the cause of the failure
mkCurrentBlockContext currentSlot c = case c of
    Empty AF.AnchorGenesis ->
      -- The chain is entirely empty.
      Right $ BlockContext (firstBlockNo (Proxy @blk)) genesisPoint

    Empty (AF.Anchor anchorSlot anchorHash anchorBlockNo) ->
      let p :: Point blk = BlockPoint anchorSlot anchorHash
      in if anchorSlot < currentSlot
           then Right $ BlockContext (succ anchorBlockNo) p
           else Left  $ TraceSlotIsImmutable currentSlot p anchorBlockNo

    c' :> hdr -> case blockSlot hdr `compare` currentSlot of

      -- The block at the tip of our chain has a slot number /before/ the
      -- current slot number. This is the common case, and we just want to
      -- connect our new block to the block at the tip.
      LT -> Right $ blockContextFromPrevHeader hdr

      -- The block at the tip of our chain has a slot that lies in the
      -- future. Although the chain DB does not adopt future blocks, if the
      -- system is under heavy load, it is possible (though unlikely) that
      -- one or more slots have passed after @currentSlot@ that we got from
      -- @onSlotChange@ and and before we queried the chain DB for the block
      -- at its tip. At the moment, we simply don't produce a block if this
      -- happens.

      -- TODO: We may wish to produce a block here anyway, treating this
      -- as similar to the @EQ@ case below, but we should be careful:
      --
      -- 1. We should think about what slot number to use.
      -- 2. We should be careful to distinguish between the case where we
      --    need to drop a block from the chain and where we don't.
      -- 3. We should be careful about slot numbers and EBBs.
      -- 4. We should probably not produce a block if the system is under
      --    very heavy load (e.g., if a lot of blocks have been produced
      --    after @currentTime@).
      --
      -- See <https://github.com/input-output-hk/ouroboros-network/issues/1462>
      GT -> Left $ TraceBlockFromFuture currentSlot (blockSlot hdr)

      -- The block at the tip has the same slot as the block we're going to
      -- produce (@currentSlot@).
      EQ -> Right $ if isJust (nodeIsEBB hdr)
        -- We allow forging a block that is the successor of an EBB in the
        -- same slot.
        then blockContextFromPrevHeader hdr
        -- If @hdr@ is not an EBB, then forge an alternative to @hdr@: same
        -- block no and same predecessor.
        else BlockContext (blockNo hdr) $ castPoint $ AF.headPoint c'

{-------------------------------------------------------------------------------
  TxSubmission integration
-------------------------------------------------------------------------------}

getMempoolReader
  :: forall m blk. (IOLike m, ApplyTx blk, HasTxId (GenTx blk))
  => Mempool m blk TicketNo
  -> TxSubmissionMempoolReader (GenTxId blk) (GenTx blk) TicketNo m
getMempoolReader mempool = MempoolReader.TxSubmissionMempoolReader
    { mempoolZeroIdx     = zeroIdx mempool
    , mempoolGetSnapshot = convertSnapshot <$> getSnapshot mempool
    }
  where
    convertSnapshot
      :: MempoolSnapshot               blk                       TicketNo
      -> MempoolReader.MempoolSnapshot (GenTxId blk) (GenTx blk) TicketNo
    convertSnapshot MempoolSnapshot { snapshotTxsAfter, snapshotLookupTx,
                                      snapshotHasTx } =
      MempoolReader.MempoolSnapshot
        { mempoolTxIdsAfter = \idx ->
            [ (txId tx, idx', txSize tx)
            | (tx, idx') <- snapshotTxsAfter idx
            ]
        , mempoolLookupTx   = snapshotLookupTx
        , mempoolHasTx      = snapshotHasTx
        }

getMempoolWriter
  :: (IOLike m, ApplyTx blk, HasTxId (GenTx blk))
  => Mempool m blk TicketNo
  -> TxSubmissionMempoolWriter (GenTxId blk) (GenTx blk) TicketNo m
getMempoolWriter mempool = Inbound.TxSubmissionMempoolWriter
    { Inbound.txId          = txId
    , mempoolAddTxs = \txs ->
        map (txId . fst) . filter (isMempoolTxAdded . snd) <$>
        addTxs mempool txs
    }

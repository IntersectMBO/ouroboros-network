{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -Wredundant-constraints -Werror=missing-fields #-}

module Ouroboros.Consensus.NodeKernel (
    -- * Node kernel
    NodeKernel (..)
  , BlockProduction (..)
  , MaxBlockSizeOverride (..)
  , NodeArgs (..)
  , TraceForgeEvent (..)
  , initNodeKernel
  , getMempoolReader
  , getMempoolWriter
  , ProtocolM
  ) where

import           Control.Monad
import           Crypto.Random (ChaChaDRG)
import           Data.Map.Strict (Map)
import           Data.Maybe (isNothing)
import           Data.Word (Word16, Word32)

import           Cardano.Prelude (UseIsNormalForm (..))
import           Control.Tracer

import           Ouroboros.Network.AnchoredFragment (AnchoredFragment (..),
                     headSlot)
import           Ouroboros.Network.Block
import           Ouroboros.Network.BlockFetch
import           Ouroboros.Network.BlockFetch.State (FetchMode (..))
import           Ouroboros.Network.Point (WithOrigin (..))
import           Ouroboros.Network.Protocol.ChainSync.PipelineDecision
                     (MkPipelineDecision)
import           Ouroboros.Network.TxSubmission.Inbound
                     (TxSubmissionMempoolWriter)
import qualified Ouroboros.Network.TxSubmission.Inbound as Inbound
import           Ouroboros.Network.TxSubmission.Outbound
                     (TxSubmissionMempoolReader)
import qualified Ouroboros.Network.TxSubmission.Outbound as Outbound

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.ChainSyncClient
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Mempool
import           Ouroboros.Consensus.Mempool.TxSeq (TicketNo)
import           Ouroboros.Consensus.Node.Run (RunNode (..))
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

import           Ouroboros.Storage.ChainDB.API (ChainDB)
import qualified Ouroboros.Storage.ChainDB.API as ChainDB


{-------------------------------------------------------------------------------
  Relay node
-------------------------------------------------------------------------------}

-- | Interface against running relay node
data NodeKernel m peer blk = NodeKernel {
      -- | The 'ChainDB' of the node
      getChainDB             :: ChainDB m blk

      -- | The node's mempool
    , getMempool             :: Mempool m blk TicketNo

      -- | The node's static configuration
    , getNodeConfig          :: NodeConfig (BlockProtocol blk)

      -- | The fetch client registry, used for the block fetch clients.
    , getFetchClientRegistry :: FetchClientRegistry peer (Header blk) blk m

      -- | Read the current candidates
    , getNodeCandidates      :: StrictTVar m (Map peer (StrictTVar m (AnchoredFragment (Header blk))))

      -- | The node's tracers
    , getTracers             :: Tracers m peer blk
    }

-- | Monad that we run protocol specific functions in
type ProtocolM blk m = NodeStateT (BlockProtocol blk) (ChaChaT (STM m))

-- | Callbacks required to produce blocks
data BlockProduction m blk = BlockProduction {
      -- | Produce a block
      --
      -- The function is passed the contents of the mempool; this is a set of
      -- transactions that is guaranteed to be consistent with the ledger state
      -- (also provided as an argument) and with each other (when applied in
      -- order). In principle /all/ of them could be included in the block (up
      -- to maximum block size).
      produceBlock :: SlotNo             -- Current slot
                   -> BlockNo            -- Current block number
                   -> ExtLedgerState blk -- Current ledger state
                   -> [GenTx blk]        -- Contents of the mempool
                   -> IsLeader (BlockProtocol blk) -- Proof we are leader
                   -> ProtocolM blk m blk

      -- | Produce a random seed
      --
      -- We want to be able to use real (crypto strength) random numbers, but
      -- obviously have no access to a sytem random number source inside an
      -- STM transaction. So we use the system RNG to generate a local DRG,
      -- which we then use for this transaction, and /only/ this transaction.
      -- The loss of entropy therefore is minimal.
      --
      -- In IO, can use 'Crypto.Random.drgNew'.
    , produceDRG :: m ChaChaDRG
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

-- | Arguments required when initializing a node
data NodeArgs m peer blk = NodeArgs {
      tracers             :: Tracers m peer blk
    , registry            :: ResourceRegistry m
    , maxClockSkew        :: ClockSkew
    , cfg                 :: NodeConfig (BlockProtocol blk)
    , initState           :: NodeState (BlockProtocol blk)
    , btime               :: BlockchainTime m
    , chainDB             :: ChainDB m blk
    , blockFetchSize      :: Header blk -> SizeInBytes
    , blockProduction     :: Maybe (BlockProduction m blk)
    , blockMatchesHeader  :: Header blk -> blk -> Bool
    , maxUnackTxs         :: Word16
    , maxBlockSize        :: MaxBlockSizeOverride
    , mempoolCap          :: MempoolCapacityBytes
    , chainSyncPipelining :: MkPipelineDecision
    }

initNodeKernel
    :: forall m peer blk.
       ( IOLike m
       , RunNode blk
       , NoUnexpectedThunks peer
       , Ord peer
       )
    => NodeArgs m peer blk
    -> m (NodeKernel m peer blk)
initNodeKernel args@NodeArgs { registry, cfg, tracers, maxBlockSize
                             , blockProduction, chainDB } = do

    nodeInitChainDB cfg chainDB

    st <- initInternalState args

    whenJust blockProduction $ forkBlockProduction maxBlockSize st

    let IS { blockFetchInterface, fetchClientRegistry, varCandidates,
             mempool } = st

    -- Run the block fetch logic in the background. This will call
    -- 'addFetchedBlock' whenever a new block is downloaded.
    void $ forkLinkedThread registry $ blockFetchLogic
        (blockFetchDecisionTracer tracers)
        (blockFetchClientTracer   tracers)
        blockFetchInterface
        fetchClientRegistry

    return NodeKernel
      { getChainDB             = chainDB
      , getMempool             = mempool
      , getNodeConfig          = cfg
      , getFetchClientRegistry = fetchClientRegistry
      , getNodeCandidates      = varCandidates
      , getTracers             = tracers
      }

{-------------------------------------------------------------------------------
  Internal node components
-------------------------------------------------------------------------------}

data InternalState m peer blk = IS {
      tracers             :: Tracers m peer blk
    , cfg                 :: NodeConfig (BlockProtocol blk)
    , registry            :: ResourceRegistry m
    , btime               :: BlockchainTime m
    , chainDB             :: ChainDB m blk
    , blockFetchInterface :: BlockFetchConsensusInterface peer (Header blk) blk m
    , fetchClientRegistry :: FetchClientRegistry peer (Header blk) blk m
    , varCandidates       :: StrictTVar m (Map peer (StrictTVar m (AnchoredFragment (Header blk))))
    , varState            :: StrictTVar m (NodeState (BlockProtocol blk))
    , mempool             :: Mempool m blk TicketNo
    }

initInternalState
    :: forall m peer blk.
       ( IOLike m
       , ProtocolLedgerView blk
       , Ord peer
       , NoUnexpectedThunks peer
       , ApplyTx blk
       )
    => NodeArgs m peer blk
    -> m (InternalState m peer blk)
initInternalState NodeArgs { tracers, chainDB, registry, cfg,
                             blockFetchSize, blockMatchesHeader, btime,
                             initState, mempoolCap } = do
    varCandidates  <- newTVarM mempty
    varState       <- newTVarM initState
    mempool        <- openMempool registry
                                  (chainDBLedgerInterface chainDB)
                                  (ledgerConfigView cfg)
                                  mempoolCap
                                  (mempoolTracer tracers)

    fetchClientRegistry <- newFetchClientRegistry

    let getCandidates :: STM m (Map peer (AnchoredFragment (Header blk)))
        getCandidates = readTVar varCandidates >>= traverse readTVar

        blockFetchInterface :: BlockFetchConsensusInterface peer (Header blk) blk m
        blockFetchInterface = initBlockFetchConsensusInterface
          cfg chainDB getCandidates blockFetchSize blockMatchesHeader btime

    return IS {..}

initBlockFetchConsensusInterface
    :: forall m peer blk. (IOLike m, SupportedBlock blk)
    => NodeConfig (BlockProtocol blk)
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
      curChainSlot <- headSlot <$> ChainDB.getCurrentChain chainDB
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

    addFetchedBlock :: Point blk -> blk -> m ()
    addFetchedBlock _pt = ChainDB.addBlock chainDB

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
    :: forall m peer blk.
       (IOLike m, RunNode blk)
    => MaxBlockSizeOverride
    -> InternalState m peer blk
    -> BlockProduction m blk
    -> m ()
forkBlockProduction maxBlockSizeOverride IS{..} BlockProduction{..} =
    void $ onSlotChange btime $ \currentSlot -> do
      varDRG <- newTVarM =<< (PRNG <$> produceDRG)
      withEarlyExit_ $ go currentSlot varDRG
  where
    go :: SlotNo -> StrictTVar m PRNG -> WithEarlyExit m ()
    go currentSlot varDRG = do
        trace $ TraceStartLeadershipCheck currentSlot

        -- Figure out which block to connect to
        --
        -- Normally this will be the current block at the tip, but it may
        -- be the /previous/ block, if there were multiple slot leaders
        (prevPoint, prevNo) <- do
          mPrev <- lift $ atomically $ prevPointAndBlockNo currentSlot <$>
                     ChainDB.getCurrentChain chainDB
          case mPrev of
            Right prev       -> return prev
            Left  futureSlot -> do
              trace $ TraceBlockFromFuture currentSlot futureSlot
              exitEarly

        -- Get ledger state corresponding to prevPoint
        --
        -- This might fail if, in between choosing 'prevPoint' and this call to
        -- 'getPastLedger', we switched to a fork where 'prevPoint' is no longer
        -- on our chain. When that happens, we simply give up on the chance to
        -- produce a block.
        extLedger <- do
          mExtLedger <- lift $ ChainDB.getPastLedger chainDB prevPoint
          case mExtLedger of
            Just l  -> return l
            Nothing -> do
              trace $ TraceNoLedgerState currentSlot prevPoint
              exitEarly
        let ledger = ledgerState extLedger

        -- Check if we are the leader
        proof <-
          case anachronisticProtocolLedgerView cfg ledger (At currentSlot) of
            Right ledgerView -> do
              mIsLeader <- lift $ atomically $ runProtocol varDRG $
                checkIsLeader
                  cfg
                  currentSlot
                  ledgerView
                  (ouroborosChainState extLedger)
              case mIsLeader of
                Just p  -> return p
                Nothing -> do
                  trace $ TraceNodeNotLeader currentSlot
                  exitEarly
            Left err -> do
              -- There are so many empty slots between the tip of our chain and
              -- the current slot that we cannot even get an accurate ledger
              -- view anymore. This is indicative of a serious problem: we are
              -- not receiving blocks. It is /possible/ it's just due to our
              -- network connectivity, and we might still get these blocks at
              -- some point; but we certainly can't produce a block of our own.
              trace $ TraceNoLedgerView currentSlot err
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
        mempoolSnapshot <- lift $ atomically $ getSnapshotFor
                                                 mempool
                                                 (TxsForBlockInSlot currentSlot)
                                                 (ledgerState extLedger)
        let txs = map fst $ snapshotTxsForSize
                              mempoolSnapshot
                              (maxBlockBodySize ledger)

        -- Actually produce the block
        newBlock <- lift $ atomically $ runProtocol varDRG $
          produceBlock
            currentSlot
            (succ prevNo)
            extLedger
            txs
            proof
        trace $ TraceForgedBlock
                  currentSlot
                  newBlock
                  (snapshotMempoolSize mempoolSnapshot)

        -- Add the block to the chain DB
        lift $ ChainDB.addBlock chainDB newBlock

        -- Check whether we adopted our block
        --
        -- addBlock is synchronous, so when it returns the block we produced
        -- will have been considered and possibly (probably) adopted
        --
        -- TODO: This is wrong. Additional blocks may have been added to the
        -- chain since.
        -- <https://github.com/input-output-hk/ouroboros-network/issues/1463>
        curTip <- lift $ atomically $ ChainDB.getTipPoint chainDB
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

    -- Return the point and block number of the most recent block in the
    -- current chain with a slot < the given slot. These will either
    -- correspond to the header at the tip of the current chain or, in case
    -- another node was also elected leader and managed to produce a block
    -- before us, the header right before the one at the tip of the chain.
    prevPointAndBlockNo :: SlotNo
                        -> AnchoredFragment (Header blk)
                        -> Either SlotNo (Point blk, BlockNo)
    prevPointAndBlockNo slot c = case c of
        Empty _   -> Right (genesisPoint, genesisBlockNo)
        c' :> hdr -> case blockSlot hdr `compare` slot of

          -- The block at the tip of our chain has a slot number /before/ the
          -- current slot number. This is the common case, and we just want to
          -- connect our new block to the block at the tip.
          LT -> Right (headerPoint hdr, blockNo hdr)

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
          GT -> Left $ blockSlot hdr

          -- The block at the tip has the same slot as the block we're going
          -- to produce (@slot@), so look at the block before it.
          EQ
             | Just{} <- nodeIsEBB hdr
               -- We allow forging a block that is the successor of an EBB in
               -- the same slot.
             -> Right (headerPoint hdr, blockNo hdr)
             | _ :> hdr' <- c'
             -> Right (headerPoint hdr', blockNo hdr')
             | otherwise
               -- If there is no block before it, so use genesis.
             -> Right (genesisPoint, genesisBlockNo)

    runProtocol :: StrictTVar m PRNG -> ProtocolM blk m a -> STM m a
    runProtocol varDRG = simOuroborosStateT varState
                       $ simChaChaT varDRG
                       $ id

-- | State of the pseudo-random number generator
newtype PRNG = PRNG ChaChaDRG
  deriving NoUnexpectedThunks via UseIsNormalForm PRNG

{-------------------------------------------------------------------------------
  TxSubmission integration
-------------------------------------------------------------------------------}

getMempoolReader
  :: forall m blk. (IOLike m, ApplyTx blk)
  => Mempool m blk TicketNo
  -> TxSubmissionMempoolReader (GenTxId blk) (GenTx blk) TicketNo m
getMempoolReader mempool = Outbound.TxSubmissionMempoolReader
    { mempoolZeroIdx     = zeroIdx mempool
    , mempoolGetSnapshot = convertSnapshot <$> getSnapshot mempool
    }
  where
    convertSnapshot
      :: MempoolSnapshot          blk                       TicketNo
      -> Outbound.MempoolSnapshot (GenTxId blk) (GenTx blk) TicketNo
    convertSnapshot MempoolSnapshot{snapshotTxsAfter, snapshotLookupTx} =
      Outbound.MempoolSnapshot
        { mempoolTxIdsAfter = \idx ->
            [ (txId tx, idx', txSize tx)
            | (tx, idx') <- snapshotTxsAfter idx
            ]
        , mempoolLookupTx   = snapshotLookupTx
        }

getMempoolWriter
  :: (Monad m, ApplyTx blk)
  => Mempool m blk TicketNo
  -> TxSubmissionMempoolWriter (GenTxId blk) (GenTx blk) TicketNo m
getMempoolWriter mempool = Inbound.TxSubmissionMempoolWriter
    { Inbound.txId          = txId
    , mempoolAddTxs = \txs ->
        map (txId . fst) . filter (isNothing . snd) <$>
        addTxs mempool txs
    }

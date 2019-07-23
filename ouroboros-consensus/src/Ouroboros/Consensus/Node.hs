{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -Wredundant-constraints -Werror=missing-fields #-}

module Ouroboros.Consensus.Node (
    -- * Node
    NodeKernel (..)
  , NodeCallbacks (..)
  , NodeParams (..)
  , TraceConstraints
  , nodeKernel
  , getMempoolReader
  , getMempoolWriter
    -- * Auxiliary functions
  , tracePrefix
  ) where

import           Control.Monad (void)
import           Crypto.Random (ChaChaDRG)
import           Data.Functor.Contravariant (contramap)
import           Data.Map.Strict (Map)
import           Data.Maybe (isNothing)
import           Data.Word (Word16)

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork (MonadFork)
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Control.Tracer

import           Ouroboros.Network.AnchoredFragment (AnchoredFragment (..),
                     headSlot)
import           Ouroboros.Network.Block
import           Ouroboros.Network.BlockFetch
import           Ouroboros.Network.BlockFetch.State (FetchMode (..))
import           Ouroboros.Network.Point (WithOrigin(..))
import           Ouroboros.Network.TxSubmission.Inbound
                     (TraceTxSubmissionInbound, TxSubmissionMempoolWriter)
import qualified Ouroboros.Network.TxSubmission.Inbound as Inbound
import           Ouroboros.Network.TxSubmission.Outbound
                     (TraceTxSubmissionOutbound, TxSubmissionMempoolReader)
import qualified Ouroboros.Network.TxSubmission.Outbound as Outbound

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.ChainSyncClient
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Mempool
import           Ouroboros.Consensus.Mempool.TxSeq (TicketNo)
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Consensus.Util.Random
import           Ouroboros.Consensus.Util.STM
import           Ouroboros.Consensus.Util.ThreadRegistry

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
    , getNodeCandidates      :: TVar m (Map peer (TVar m (CandidateState blk)))
    }

-- | Monad that we run protocol specific functions in
type ProtocolM blk m = NodeStateT (BlockProtocol blk) (ChaChaT (STM m))

-- | Callbacks required when running the node
data NodeCallbacks m blk = NodeCallbacks {
      -- | Produce a block
      --
      -- The function is passed the contents of the mempool; this is a set of
      -- transactions that is guaranteed to be consistent with the ledger state
      -- (also provided as an argument) and with each other (when applied in
      -- order). In principle /all/ of them could be included in the block (up
      -- to maximum block size).
      produceBlock :: IsLeader (BlockProtocol blk) -- Proof we are leader
                   -> ExtLedgerState blk -- Current ledger state
                   -> SlotNo             -- Current slot
                   -> Point blk          -- Previous point
                   -> BlockNo            -- Previous block number
                   -> [GenTx blk]        -- Contents of the mempool
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

-- | Parameters required when initializing a node
data NodeParams m peer blk = NodeParams {
      tracer             :: Tracer m String
    , mempoolTracer      :: Tracer m (TraceEventMempool blk)
    , decisionTracer     :: Tracer m [TraceLabelPeer peer (FetchDecision [Point (Header blk)])]
    , fetchClientTracer  :: Tracer m (TraceLabelPeer peer (TraceFetchClientState (Header blk)))
    , txInboundTracer    :: Tracer m (TraceTxSubmissionInbound  (GenTxId blk) (GenTx blk))
    , txOutboundTracer   :: Tracer m (TraceTxSubmissionOutbound (GenTxId blk) (GenTx blk))
    , threadRegistry     :: ThreadRegistry m
    , maxClockSkew       :: ClockSkew
    , cfg                :: NodeConfig (BlockProtocol blk)
    , initState          :: NodeState (BlockProtocol blk)
    , btime              :: BlockchainTime m
    , chainDB            :: ChainDB m blk
    , callbacks          :: NodeCallbacks m blk
    , blockFetchSize     :: Header blk -> SizeInBytes
    , blockMatchesHeader :: Header blk -> blk -> Bool
    , maxUnackTxs        :: Word16
    }

nodeKernel
    :: forall m peer blk.
       ( MonadAsync m
       , MonadFork  m
       , MonadMask  m
       , ProtocolLedgerView blk
       , Ord peer
       , TraceConstraints peer blk
       , ApplyTx blk
       )
    => NodeParams m peer blk
    -> m (NodeKernel m peer blk)
nodeKernel params@NodeParams { threadRegistry, cfg, decisionTracer, fetchClientTracer } = do
    st <- initInternalState params

    forkBlockProduction st

    let IS { blockFetchInterface, fetchClientRegistry, varCandidates,
             chainDB, mempool } = st

    -- Run the block fetch logic in the background. This will call
    -- 'addFetchedBlock' whenever a new block is downloaded.
    void $ forkLinked threadRegistry $ blockFetchLogic
        decisionTracer
        fetchClientTracer
        blockFetchInterface
        fetchClientRegistry

    return NodeKernel {
        getChainDB    = chainDB
      , getMempool    = mempool
      , getNodeConfig = cfg
      , getFetchClientRegistry = fetchClientRegistry
      , getNodeCandidates      = varCandidates
      }

{-------------------------------------------------------------------------------
  Internal node components
-------------------------------------------------------------------------------}

-- | Constraints required to trace nodes, block, headers, etc.
type TraceConstraints peer blk =
  ( Condense peer
  , Condense blk
  , Condense (ChainHash blk)
  , Condense (Header blk)
  )

data InternalState m peer blk = IS {
      cfg                 :: NodeConfig (BlockProtocol blk)
    , threadRegistry      :: ThreadRegistry m
    , btime               :: BlockchainTime m
    , callbacks           :: NodeCallbacks m blk
    , chainDB             :: ChainDB m blk
    , blockFetchInterface :: BlockFetchConsensusInterface peer (Header blk) blk m
    , fetchClientRegistry :: FetchClientRegistry peer (Header blk) blk m
    , varCandidates       :: TVar m (Map peer (TVar m (CandidateState blk)))
    , varState            :: TVar m (NodeState (BlockProtocol blk))
    , tracer              :: Tracer m String
    , mempool             :: Mempool m blk TicketNo
    }

initInternalState
    :: forall m peer blk.
       ( MonadAsync m
       , MonadFork m
       , MonadMask m
       , ProtocolLedgerView blk
       , Ord peer
       , TraceConstraints peer blk
       , ApplyTx blk
       )
    => NodeParams m peer blk
    -> m (InternalState m peer blk)
initInternalState NodeParams {..} = do
    varCandidates  <- atomically $ newTVar mempty
    varState       <- atomically $ newTVar initState
    mempool        <- openMempool threadRegistry
                                  chainDB
                                  (ledgerConfigView cfg)
                                  mempoolTracer

    fetchClientRegistry <- newFetchClientRegistry

    let getCandidates :: STM m (Map peer (AnchoredFragment (Header blk)))
        getCandidates = readTVar varCandidates >>=
                        traverse (fmap candidateChain . readTVar)

        blockFetchInterface :: BlockFetchConsensusInterface peer (Header blk) blk m
        blockFetchInterface = initBlockFetchConsensusInterface
          (tracePrefix "ChainDB" (Nothing :: Maybe peer) tracer)
          cfg chainDB getCandidates blockFetchSize blockMatchesHeader btime

    return IS {..}

tracePrefix :: Condense peer
            => String
            -> Maybe peer
            -> Tracer m String
            -> Tracer m String
tracePrefix p mbUp tr =
  let prefix = p <> maybe "" ((" " <>) . condense) mbUp <> " | "
  in contramap (prefix <>) tr

initBlockFetchConsensusInterface
    :: forall m peer blk.
       ( MonadSTM m
       , TraceConstraints peer blk
       , SupportedBlock blk
       )
    => Tracer m String
    -> NodeConfig (BlockProtocol blk)
    -> ChainDB m blk
    -> STM m (Map peer (AnchoredFragment (Header blk)))
    -> (Header blk -> SizeInBytes)
    -> (Header blk -> blk -> Bool)
    -> BlockchainTime m
    -> BlockFetchConsensusInterface peer (Header blk) blk m
initBlockFetchConsensusInterface tracer cfg chainDB getCandidates blockFetchSize
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
    addFetchedBlock _pt blk = do
      ChainDB.addBlock chainDB blk
      traceWith tracer $ "Downloaded block: " <> condense blk

    plausibleCandidateChain :: AnchoredFragment (Header blk)
                            -> AnchoredFragment (Header blk)
                            -> Bool
    plausibleCandidateChain = preferCandidate cfg

    compareCandidateChains :: AnchoredFragment (Header blk)
                           -> AnchoredFragment (Header blk)
                           -> Ordering
    compareCandidateChains = compareCandidates cfg

forkBlockProduction
    :: forall m peer blk.
       ( MonadAsync m
       , ProtocolLedgerView blk
       , TraceConstraints peer blk
       )
    => InternalState m peer blk -> m ()
forkBlockProduction IS{..} =
    onSlotChange btime $ \currentSlot -> do
      drg  <- produceDRG
      -- See the docstring of 'withSyncState' for why we're using it instead
      -- of 'atomically'.
      mNewBlock <- withSyncState mempool $ \MempoolSnapshot{snapshotTxs} -> do
        varDRG <- newTVar drg
        l@ExtLedgerState{..} <- ChainDB.getCurrentLedger chainDB
        mIsLeader            <- runProtocol varDRG $
                                   checkIsLeader
                                     cfg
                                     currentSlot
                                     (protocolLedgerView cfg ledgerState)
                                     ouroborosChainState

        case mIsLeader of
          Nothing    -> return Nothing
          Just proof -> do
            (prevPoint, prevNo) <- prevPointAndBlockNo currentSlot <$>
                                     ChainDB.getCurrentChain chainDB
            newBlock            <- runProtocol varDRG $
                                     produceBlock
                                       proof
                                       l
                                       currentSlot
                                       prevPoint
                                       prevNo
                                       (map fst snapshotTxs)
            return $ Just newBlock

      -- Note that there is a possible race condition here: we have produced a
      -- block containing valid transactions w.r.t. the current ledger state
      -- (this was race-free), but the current chain might change before we
      -- complete adding the block to the ChainDB. If the current chain has
      -- changed to a longer chain (than the one at the time of producing the
      -- block), chain selection will not select the block we just produced
      -- ourselves, as it would mean switching to a shorter chain.
      whenJust mNewBlock $ \newBlock -> do
        traceWith tracer $
          "As leader of slot " <> condense currentSlot <> " I produce: " <>
          condense newBlock
        ChainDB.addBlock chainDB newBlock
  where
    NodeCallbacks{..} = callbacks

    -- Return the point and block number of the most recent block in the
    -- current chain with a slot < the given slot. These will either
    -- correspond to the header at the tip of the current chain or, in case
    -- another node was also elected leader and managed to produce a block
    -- before us, the header right before the one at the tip of the chain.
    prevPointAndBlockNo :: SlotNo
                        -> AnchoredFragment (Header blk)
                        -> (Point blk, BlockNo)
    prevPointAndBlockNo slot c = case c of
        Empty _   -> (genesisPoint, genesisBlockNo)
        c' :> hdr -> case blockSlot hdr `compare` slot of
          LT -> (headerPoint hdr, blockNo hdr)
          -- The block at the tip of our chain has a slot that lies in the
          -- future.
          GT -> error "prevPointAndBlockNo: block in future"
          -- The block at the tip has the same slot as the block we're going
          -- to produce (@slot@), so look at the block before it.
          EQ | _ :> hdr' <- c'
             -> (headerPoint hdr', blockNo hdr')
             | otherwise
               -- If there is no block before it, so use genesis.
             -> (genesisPoint, genesisBlockNo)

    runProtocol :: TVar m ChaChaDRG -> ProtocolM blk m a -> STM m a
    runProtocol varDRG = simOuroborosStateT varState
                       $ simChaChaT varDRG
                       $ id


{-------------------------------------------------------------------------------
  TxSubmission integration
-------------------------------------------------------------------------------}

getMempoolReader
  :: forall m blk.
     (MonadSTM m, ApplyTx blk)
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

{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -Wredundant-constraints -Werror=missing-fields #-}

module Ouroboros.Consensus.Node (
    -- * Node
    NodeKernel (..)
  , NodeCallbacks (..)
  , NodeParams (..)
  , TraceConstraints
  , nodeKernel
    -- * Auxiliary functions
  , tracePrefix
  ) where

import           Control.Monad (void)
import           Crypto.Random (ChaChaDRG)
import qualified Data.Foldable as Foldable
import           Data.Functor.Contravariant (contramap)
import           Data.Map.Strict (Map)

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
import qualified Ouroboros.Network.Chain as Chain

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
      getChainDB :: ChainDB m blk (Header blk)

      -- | The node's mempool
    , getMempool :: Mempool m blk TicketNo

      -- | The node's static configuration
    , getNodeConfig :: NodeConfig (BlockProtocol blk)

      -- | The fetch client registry, used for the block fetch clients.
    , getFetchClientRegistry :: FetchClientRegistry peer (Header blk) blk m

      -- | Read the current candidates
    , getNodeCandidates :: TVar m (Map peer (TVar m (CandidateState blk)))
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
    , threadRegistry     :: ThreadRegistry m
    , maxClockSkew       :: ClockSkew
    , cfg                :: NodeConfig (BlockProtocol blk)
    , initState          :: NodeState (BlockProtocol blk)
    , btime              :: BlockchainTime m
    , chainDB            :: ChainDB m blk (Header blk)
    , callbacks          :: NodeCallbacks m blk
    , blockFetchSize     :: Header blk -> SizeInBytes
    , blockMatchesHeader :: Header blk -> blk -> Bool
    }

nodeKernel
    :: forall m peer blk.
       ( MonadAsync m
       , MonadFork  m
       , MonadMask  m
       , ProtocolLedgerView blk
       , Ord (GenTxId blk)
       , Ord peer
       , TraceConstraints peer blk
       , ApplyTx blk
       , Eq (Header blk)
       )
    => NodeParams m peer blk
    -> m (NodeKernel m peer blk)
nodeKernel params@NodeParams { threadRegistry, cfg } = do
    st <- initInternalState params

    forkBlockProduction st

    let IS { blockFetchInterface, fetchClientRegistry, varCandidates,
             chainDB, mempool } = st

    -- Run the block fetch logic in the background. This will call
    -- 'addFetchedBlock' whenever a new block is downloaded.
    void $ forkLinked threadRegistry $ blockFetchLogic
        nullTracer            -- fetch decision tracer
        nullTracer            -- fetch client state tracer
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
  , Condense (HeaderHash blk)
  , Condense (Header blk)
  )

data InternalState m peer blk = IS {
      cfg                 :: NodeConfig (BlockProtocol blk)
    , threadRegistry      :: ThreadRegistry m
    , btime               :: BlockchainTime m
    , callbacks           :: NodeCallbacks m blk
    , chainDB             :: ChainDB m blk (Header blk)
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
       , Ord (GenTxId blk)
       , Ord peer
       , TraceConstraints peer blk
       , ApplyTx blk
       )
    => NodeParams m peer blk
    -> m (InternalState m peer blk)
initInternalState NodeParams {..} = do
    varCandidates  <- atomically $ newTVar mempty
    varState       <- atomically $ newTVar initState
    mempool        <- openMempool threadRegistry chainDB (ledgerConfigView cfg)

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
    -> ChainDB m blk (Header blk)
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
      -- FIXME an empty chain doesn't really have a current slot.
      -- We use SlotNo 0 in this case. Hopefully that's ok.
      curChainSlot <- headSlot <$> ChainDB.getCurrentChain chainDB
      let curChainSlot' = fromTPoint 0 unSlotNo curChainSlot
          slotsBehind = unSlotNo curSlot - curChainSlot'
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

    readFetchedBlocks :: STM m (BlockPoint blk -> Bool)
    readFetchedBlocks = ChainDB.getIsFetched chainDB

    addFetchedBlock :: BlockPoint blk -> blk -> m ()
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
      mNewBlock <- atomically $ do
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

            -- In this circumstance, it is required that we call 'syncState'
            -- before 'getTxs' within this 'STM' transaction since we need to
            -- guarantee that the transactions returned from 'getTxs' are
            -- valid with respect to the current ledger state of the
            -- 'ChainDB'. Refer to the 'getTxs' documentation for more
            -- information.
            _invalidTxs         <- syncState mempool

            txs                 <- Foldable.toList . (fmap sndOfTriple)
                                     <$> getTxs mempool
            newBlock            <- runProtocol varDRG $
                                     produceBlock
                                       proof
                                       l
                                       currentSlot
                                       prevPoint
                                       prevNo
                                       txs
            return $ Just newBlock

      whenJust mNewBlock $ \newBlock -> do
        traceWith tracer $
          "As leader of slot " <> condense currentSlot <> " I produce: " <>
          condense newBlock
        ChainDB.addBlock chainDB newBlock
  where
    NodeCallbacks{..} = callbacks

    -- Return the second item in a triple.
    sndOfTriple (_, b, _) = b

    -- Return the point and block number of the most recent block in the
    -- current chain with a slot < the given slot. These will either
    -- correspond to the header at the tip of the current chain or, in case
    -- another node was also elected leader and managed to produce a block
    -- before us, the header right before the one at the tip of the chain.
    prevPointAndBlockNo :: SlotNo
                        -> AnchoredFragment (Header blk)
                        -> (Point blk, BlockNo)
    prevPointAndBlockNo slot c = case c of
        Empty _   -> (Chain.genesisPoint, Chain.genesisBlockNo)
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
             -> (Chain.genesisPoint, Chain.genesisBlockNo)

    runProtocol :: TVar m ChaChaDRG -> ProtocolM blk m a -> STM m a
    runProtocol varDRG = simOuroborosStateT varState
                       $ simChaChaT varDRG
                       $ id


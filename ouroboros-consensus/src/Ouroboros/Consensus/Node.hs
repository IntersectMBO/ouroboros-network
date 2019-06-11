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
  , NodeComms (..)
  , NodeParams (..)
  , nodeKernel
    -- * Channels (re-exports from the network layer)
  , Channel
  , Network.createConnectedChannels
  , Network.loggingChannel
  ) where

import           Control.Monad (void)
import           Crypto.Random (ChaChaDRG)
import qualified Data.Foldable as Foldable
import           Data.Functor.Contravariant (contramap)
import           Data.Map.Strict (Map)
import           Data.Void (Void)

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork (MonadFork)
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Tracer

import           Network.TypedProtocol.Driver
import           Ouroboros.Network.Channel as Network
import           Ouroboros.Network.Codec

import           Ouroboros.Network.AnchoredFragment (AnchoredFragment (..),
                     headSlot)
import           Ouroboros.Network.Block
import           Ouroboros.Network.BlockFetch
import           Ouroboros.Network.BlockFetch.Client (BlockFetchClient,
                     blockFetchClient)
import           Ouroboros.Network.BlockFetch.State (FetchMode (..))
import qualified Ouroboros.Network.Chain as Chain
import           Ouroboros.Network.Protocol.BlockFetch.Server
                     (BlockFetchServer (..), blockFetchServerPeer)
import           Ouroboros.Network.Protocol.BlockFetch.Type (BlockFetch)
import           Ouroboros.Network.Protocol.ChainSync.Client
import           Ouroboros.Network.Protocol.ChainSync.Server
import           Ouroboros.Network.Protocol.ChainSync.Type

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.BlockFetchServer
import           Ouroboros.Consensus.ChainSyncClient
import           Ouroboros.Consensus.ChainSyncServer
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Mempool
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
data NodeKernel m up blk = NodeKernel {
      -- | The 'ChainDB' of the node
      getChainDB :: ChainDB m blk (Header blk)

      -- | The node's mempool
    , getMempool :: Mempool m blk

      -- | The node's static configuration
    , getNodeConfig :: NodeConfig (BlockProtocol blk)

      -- | Notify network layer of new upstream node
      --
      -- NOTE: Eventually it will be the responsibility of the network layer
      -- itself to register and deregister peers.
    , addUpstream   :: forall eCS eBF bytesCS bytesBF.
                       (Exception eCS, Exception eBF)
                    => up
                    -> NodeComms m (ChainSync (Header blk) (Point blk)) eCS bytesCS
                    -> NodeComms m (BlockFetch blk)                     eBF bytesBF
                    -> m ()

      -- | Notify network layer of a new downstream node
      --
      -- NOTE: Eventually it will be the responsibility of the network layer
      -- itself to register and deregister peers.
    , addDownstream :: forall eCS eBF bytesCS bytesBF.
                       (Exception eCS, Exception eBF)
                    => NodeComms m (ChainSync (Header blk) (Point blk)) eCS bytesCS
                    -> NodeComms m (BlockFetch blk)                     eBF bytesBF
                    -> m ()
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
data NodeParams m up blk = NodeParams {
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
    :: forall m up blk.
       ( MonadAsync m
       , MonadFork  m
       , MonadMask  m
       , MonadTime  m
       , MonadThrow (STM m)
       , ProtocolLedgerView blk
       , Ord up
       , TraceConstraints up blk
       , ApplyTx blk
       )
    => NodeParams m up blk
    -> m (NodeKernel m up blk)
nodeKernel params@NodeParams { threadRegistry, cfg } = do
    st <- initInternalState params

    forkBlockProduction st

    let IS { blockFetchInterface, fetchClientRegistry, chainDB, mempool } = st

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
      , addUpstream   = npAddUpstream   (networkLayer st)
      , addDownstream = npAddDownstream (networkLayer st)
      }

{-------------------------------------------------------------------------------
  Internal node components
-------------------------------------------------------------------------------}

-- | Constraints required to trace nodes, block, headers, etc.
type TraceConstraints up blk =
  ( Condense up
  , Condense blk
  , Condense (ChainHash blk)
  , Condense (Header blk)
  )

data InternalState m up blk = IS {
      cfg                 :: NodeConfig (BlockProtocol blk)
    , threadRegistry      :: ThreadRegistry m
    , btime               :: BlockchainTime m
    , callbacks           :: NodeCallbacks m blk
    , networkLayer        :: NetworkProvides m up blk
    , chainDB             :: ChainDB m blk (Header blk)
    , blockFetchInterface :: BlockFetchConsensusInterface up (Header blk) blk m
    , fetchClientRegistry :: FetchClientRegistry up (Header blk) blk m
    , varCandidates       :: TVar m (Map up (TVar m (CandidateState blk)))
    , varState            :: TVar m (NodeState (BlockProtocol blk))
    , tracer              :: Tracer m String
    , mempool             :: Mempool m blk
    }

initInternalState
    :: forall m up blk.
       ( MonadAsync m
       , MonadFork  m
       , MonadMask  m
       , MonadTime  m
       , MonadThrow (STM m)
       , ProtocolLedgerView blk
       , Ord up
       , TraceConstraints up blk
       , ApplyTx blk
       )
    => NodeParams m up blk
    -> m (InternalState m up blk)
initInternalState NodeParams {..} = do
    varCandidates  <- atomically $ newTVar mempty
    varState       <- atomically $ newTVar initState
    mempool        <- openMempool chainDB (ledgerConfigView cfg)

    fetchClientRegistry <- newFetchClientRegistry

    let getCandidates :: STM m (Map up (AnchoredFragment (Header blk)))
        getCandidates = readTVar varCandidates >>=
                        traverse (fmap candidateChain . readTVar)

        blockFetchInterface :: BlockFetchConsensusInterface up (Header blk) blk m
        blockFetchInterface = initBlockFetchConsensusInterface
          (tracePrefix "ChainDB" Nothing)
          cfg chainDB getCandidates blockFetchSize blockMatchesHeader btime

        nrChainSyncClient :: up -> Consensus ChainSyncClient blk m
        nrChainSyncClient up = chainSyncClient
          (tracePrefix "CSClient" (Just up))
          cfg
          btime
          maxClockSkew
          (ChainDB.getCurrentChain chainDB)
          (ChainDB.getCurrentLedger chainDB)
          varCandidates
          up

        nrChainSyncServer :: ChainSyncServer (Header blk) (Point blk) m ()
        nrChainSyncServer =
          chainSyncServer (tracePrefix "CSServer" Nothing) chainDB

        nrBlockFetchClient :: BlockFetchClient (Header blk) blk m ()
        nrBlockFetchClient = blockFetchClient
          -- Note the tracer for the changes in the fetch client state
          -- is passed to the blockFetchLogic.
          -- The message level tracer is passed to runPipelinedPeer.

        nrBlockFetchServer :: BlockFetchServer blk m ()
        nrBlockFetchServer =
          blockFetchServer (tracePrefix "BFServer" Nothing) chainDB

        nrFetchClientRegistry = fetchClientRegistry

        networkRequires :: NetworkRequires m up blk
        networkRequires = NetworkRequires {..}

        networkLayer :: NetworkProvides m up blk
        networkLayer = initNetworkLayer
          (contramap ("BlockFetch | " <>) tracer)
          threadRegistry
          networkRequires

    return IS {..}
  where
    tracePrefix :: String -> Maybe up -> Tracer m String
    tracePrefix p mbUp =
      let prefix = p <> maybe "" ((" " <>) . condense) mbUp <> " | "
      in contramap (prefix <>) tracer

initBlockFetchConsensusInterface
    :: forall m up blk.
       ( MonadSTM m
       , TraceConstraints up blk
       , SupportedBlock blk
       )
    => Tracer m String
    -> NodeConfig (BlockProtocol blk)
    -> ChainDB m blk (Header blk)
    -> STM m (Map up (AnchoredFragment (Header blk)))
    -> (Header blk -> SizeInBytes)
    -> (Header blk -> blk -> Bool)
    -> BlockchainTime m
    -> BlockFetchConsensusInterface up (Header blk) blk m
initBlockFetchConsensusInterface tracer cfg chainDB getCandidates blockFetchSize
    blockMatchesHeader btime = BlockFetchConsensusInterface {..}
  where
    readCandidateChains :: STM m (Map up (AnchoredFragment (Header blk)))
    readCandidateChains = getCandidates

    readCurrentChain :: STM m (AnchoredFragment (Header blk))
    readCurrentChain = ChainDB.getCurrentChain chainDB

    readFetchMode :: STM m FetchMode
    readFetchMode = do
      curSlot      <- getCurrentSlot btime
      curChainSlot <- headSlot <$> ChainDB.getCurrentChain chainDB
      let slotsBehind = unSlotNo curSlot - unSlotNo curChainSlot
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
    :: forall m up blk.
       ( MonadAsync m
       , ProtocolLedgerView blk
       , TraceConstraints up blk
       )
    => InternalState m up blk -> m ()
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
            txs                 <- getTxs mempool
            newBlock            <- runProtocol varDRG $
                                     produceBlock
                                       proof
                                       l
                                       currentSlot
                                       (castPoint prevPoint)
                                       prevNo
                                       (Foldable.toList txs)
            return $ Just newBlock

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

{-------------------------------------------------------------------------------
  New network layer
-------------------------------------------------------------------------------}

data NetworkRequires m up blk = NetworkRequires {
      -- | Start a chain sync client that communicates with the given upstream
      -- node.
      nrChainSyncClient     :: up -> ChainSyncClient (Header blk) (Point blk) m Void

      -- | Start a chain sync server.
    , nrChainSyncServer     :: ChainSyncServer (Header blk) (Point blk) m ()

      -- | Start a block fetch client that communicates with the given
      -- upstream node.
    , nrBlockFetchClient    :: BlockFetchClient (Header blk) blk m ()

      -- | Start a block fetch server server.
    , nrBlockFetchServer    :: BlockFetchServer blk m ()

      -- | The fetch client registry, used by the block fetch client.
    , nrFetchClientRegistry :: FetchClientRegistry up (Header blk) blk m
    }

-- | Required by the network layer to initiate comms to a new node
data NodeComms m ps e bytes = NodeComms {
      -- | Codec used for the protocol
      ncCodec    :: Codec ps e m bytes

      -- | Construct a channel to the node
      --
      -- This is in CPS style to allow for resource allocation.
    , ncWithChan :: forall a. (Channel m bytes -> m a) -> m a
    }

-- TODO something nicer than eCS, eBF, bytesCS, and bytesBF. Mux them over one
-- channel.
data NetworkProvides m up blk = NetworkProvides {
      -- | Notify network layer of new upstream node
      --
      -- NOTE: Eventually it will be the responsibility of the network layer
      -- itself to register and deregister peers.
      npAddUpstream   :: forall eCS eBF bytesCS bytesBF.
                         (Exception eCS, Exception eBF)
                      => up
                      -> NodeComms m (ChainSync (Header blk) (Point blk)) eCS bytesCS
                         -- Communication for the Chain Sync protocol
                      -> NodeComms m (BlockFetch blk)                     eBF bytesBF
                         -- Communication for the Block Fetch protocol
                      -> m ()

      -- | Notify network layer of a new downstream node
      --
      -- NOTE: Eventually it will be the responsibility of the network layer
      -- itself to register and deregister peers.p
    , npAddDownstream :: forall eCS eBF bytesCS bytesBF.
                         (Exception eCS, Exception eBF)
                      => NodeComms m (ChainSync (Header blk) (Point blk)) eCS bytesCS
                         -- Communication for the Chain Sync protocol
                      -> NodeComms m (BlockFetch blk)                     eBF bytesBF
                         -- Communication for the Block Fetch protocol
                      -> m ()
    }

initNetworkLayer
    :: forall m up blk.
       ( MonadSTM   m
       , MonadAsync m
       , MonadFork  m
       , MonadCatch m
       , MonadMask  m
       , Ord up
       )
    => Tracer m String
    -> ThreadRegistry m
    -> NetworkRequires m up blk
    -> NetworkProvides m up blk
initNetworkLayer _tracer registry NetworkRequires{..} = NetworkProvides {..}
  where
    npAddDownstream :: (Exception eCS, Exception eBF)
                    => NodeComms m (ChainSync (Header blk) (Point blk)) eCS bytesCS
                    -> NodeComms m (BlockFetch blk)                     eBF bytesBF
                    -> m ()
    npAddDownstream ncCS ncBF = do
      -- TODO use subregistry here?
      let NodeComms csCodec csWithChan = ncCS
          NodeComms bfCodec bfWithChan = ncBF
      void $ forkLinked registry $ bfWithChan $ \chan ->
        runPeer nullTracer bfCodec chan $
          blockFetchServerPeer nrBlockFetchServer
      void $ forkLinked registry $ csWithChan $ \chan ->
        runPeer nullTracer csCodec chan $
          chainSyncServerPeer nrChainSyncServer

    npAddUpstream :: (Exception eCS, Exception eBF)
                  => up
                  -> NodeComms m (ChainSync (Header blk) (Point blk)) eCS bytesCS
                  -> NodeComms m (BlockFetch blk)                     eBF bytesBF
                  -> m ()
    npAddUpstream up ncCS ncBF = do
      -- TODO use subregistry here?
      let NodeComms csCodec csWithChan = ncCS
          NodeComms bfCodec bfWithChan = ncBF

      clientRegistered <- newEmptyTMVarM

      void $ forkLinked registry $ bfWithChan $ \chan ->
        bracketFetchClient nrFetchClientRegistry up $ \clientCtx -> do
          atomically $ putTMVar clientRegistered ()
          -- TODO make 10 a parameter. Or encapsulate the pipelining
          -- stuff
          runPipelinedPeer 10 nullTracer bfCodec chan $
            nrBlockFetchClient clientCtx

      -- The block fetch logic thread in the background wants there to be a
      -- block fetch client thread for each chain sync candidate it sees. So
      -- start the chain sync client after the block fetch thread was
      -- registered to make sure it never sees a chain sync candidate without
      -- a corresponding block fetch client thread.
      atomically $ takeTMVar clientRegistered

      void $ forkNonTerminating registry $ csWithChan $ \chan ->
        runPeer nullTracer csCodec chan $
          chainSyncClientPeer (nrChainSyncClient up)

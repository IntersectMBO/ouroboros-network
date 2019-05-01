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
    -- * Node IDs
    NodeId (..)
  , CoreNodeId (..)
  , fromCoreNodeId
    -- * Node
  , NodeKernel (..)
  , NodeCallbacks (..)
  , NodeComms (..)
  , NodeParams (..)
  , nodeKernel
    -- * Channels (re-exports from the network layer)
  , Channel
  , Network.createConnectedChannels
  , Network.loggingChannel
  ) where

import           Codec.Serialise (Serialise)
import           Control.Monad (void)
import           Crypto.Random (ChaChaDRG)
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
                     (BlockFetchConsensusInterface (..), FetchClientRegistry,
                     blockFetchLogic, newFetchClientRegistry)
import qualified Ouroboros.Network.BlockFetch.Client as BlockFetchClient
import           Ouroboros.Network.BlockFetch.State (FetchMode (..))
import           Ouroboros.Network.BlockFetch.Types (SizeInBytes)
import qualified Ouroboros.Network.Chain as Chain
import           Ouroboros.Network.Protocol.BlockFetch.Server
                     (BlockFetchServer (..), blockFetchServerPeer)
import           Ouroboros.Network.Protocol.BlockFetch.Type (BlockFetch)
import           Ouroboros.Network.Protocol.ChainSync.Client
import           Ouroboros.Network.Protocol.ChainSync.Server
import           Ouroboros.Network.Protocol.ChainSync.Type

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.BlockFetchClient
import           Ouroboros.Consensus.BlockFetchServer
import           Ouroboros.Consensus.ChainSyncClient
import           Ouroboros.Consensus.ChainSyncServer
import           Ouroboros.Consensus.Ledger.Abstract
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
  Node IDs
-------------------------------------------------------------------------------}

-- TODO: This was moved here from the network layer Node module. We should
-- review this and make sure it makes sense here.
data NodeId = CoreId Int
            | RelayId Int
  deriving (Eq, Ord, Show)

instance Condense NodeId where
  condense (CoreId  i) = "c" ++ show i
  condense (RelayId i) = "r" ++ show i

-- | Core node ID
newtype CoreNodeId = CoreNodeId Int
  deriving (Show, Eq, Ord, Condense, Serialise)

fromCoreNodeId :: CoreNodeId -> NodeId
fromCoreNodeId (CoreNodeId n) = CoreId n

{-------------------------------------------------------------------------------
  Relay node
-------------------------------------------------------------------------------}

-- | Interface against running relay node
data NodeKernel m up blk hdr = NodeKernel {
      -- | The 'ChainDB' of the node
      getChainDB   :: ChainDB m blk hdr

      -- | Notify network layer of new upstream node
      --
      -- NOTE: Eventually it will be the responsibility of the network layer
      -- itself to register and deregister peers.
    , addUpstream   :: forall eCS eBF bytesCS bytesBF.
                       (Exception eCS, Exception eBF)
                    => up
                    -> NodeComms m (ChainSync hdr (Point hdr)) eCS bytesCS
                    -> NodeComms m (BlockFetch hdr blk)        eBF bytesBF
                    -> m ()

      -- | Notify network layer of a new downstream node
      --
      -- NOTE: Eventually it will be the responsibility of the network layer
      -- itself to register and deregister peers.
    , addDownstream :: forall eCS eBF bytesCS bytesBF.
                       (Exception eCS, Exception eBF)
                    => NodeComms m (ChainSync hdr (Point hdr)) eCS bytesCS
                    -> NodeComms m (BlockFetch hdr blk)        eBF bytesBF
                    -> m ()
    }

-- | Monad that we run protocol specific functions in
type ProtocolM blk m = NodeStateT (BlockProtocol blk) (ChaChaT (STM m))

-- | Callbacks required when initializing the node
data NodeCallbacks m blk = NodeCallbacks {
      -- | Produce a block
      produceBlock :: IsLeader (BlockProtocol blk) -- Proof we are leader
                   -> ExtLedgerState blk -- Current ledger state
                   -> SlotNo             -- Current slot
                   -> Point blk          -- Previous point
                   -> BlockNo            -- Previous block number
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
data NodeParams m up blk hdr = NodeParams {
      tracer             :: Tracer m String
    , threadRegistry     :: ThreadRegistry m
    , maxClockSkew       :: ClockSkew
    , cfg                :: NodeConfig (BlockProtocol blk)
    , initState          :: NodeState (BlockProtocol blk)
    , btime              :: BlockchainTime m
    , chainDB            :: ChainDB m blk hdr
    , callbacks          :: NodeCallbacks m blk
    , blockFetchSize     :: hdr -> SizeInBytes
    , blockMatchesHeader :: hdr -> blk -> Bool
    }

nodeKernel
    :: forall m up blk hdr.
       ( MonadAsync m
       , MonadFork  m
       , MonadMask  m
       , MonadTime  m
       , MonadThrow (STM m)
       , ProtocolLedgerView blk
       , HasHeader hdr
       , HeaderHash hdr ~ HeaderHash blk
       , BlockProtocol hdr ~ BlockProtocol blk
       , Ord up
       , TraceConstraints up blk hdr
       )
    => NodeParams m up blk hdr
    -> m (NodeKernel m up blk hdr)
nodeKernel params@NodeParams { tracer, threadRegistry } = do
    st <- initInternalState params

    forkBlockProduction st

    let IS { blockFetchInterface, fetchClientRegistry, chainDB } = st

    -- Run the block fetch logic in the background. This will call
    -- 'addFetchedBlock' whenever a new block is downloaded.
    void $ forkLinked threadRegistry $ blockFetchLogic
        (showTracing tracer)
        blockFetchInterface
        fetchClientRegistry

    return NodeKernel {
        getChainDB    = chainDB
      , addUpstream   = npAddUpstream   (networkLayer st)
      , addDownstream = npAddDownstream (networkLayer st)
      }

{-------------------------------------------------------------------------------
  Internal node components
-------------------------------------------------------------------------------}

-- | Constraints required to trace nodes, block, headers, etc.
type TraceConstraints up blk hdr =
  (Condense up, Condense blk, Condense hdr)

data InternalState m up blk hdr = IS {
      cfg                 :: NodeConfig (BlockProtocol blk)
    , threadRegistry      :: ThreadRegistry m
    , btime               :: BlockchainTime m
    , callbacks           :: NodeCallbacks m blk
    , networkLayer        :: NetworkProvides m up blk hdr
    , chainDB             :: ChainDB m blk hdr
    , blockFetchInterface :: BlockFetchConsensusInterface up hdr blk m
    , fetchClientRegistry :: FetchClientRegistry up hdr m
    , varCandidates       :: TVar m (Map up (TVar m (CandidateState blk hdr)))
    , varState            :: TVar m (NodeState (BlockProtocol blk))
    , tracer              :: Tracer m String
    }

initInternalState
    :: forall m up blk hdr.
       ( MonadAsync m
       , MonadFork  m
       , MonadMask  m
       , MonadTime  m
       , MonadThrow (STM m)
       , HasHeader hdr
       , HeaderHash hdr ~ HeaderHash blk
       , ProtocolLedgerView blk
       , BlockProtocol hdr ~ BlockProtocol blk
       , Ord up
       , TraceConstraints up blk hdr
       )
    => NodeParams m up blk hdr
    -> m (InternalState m up blk hdr)
initInternalState NodeParams {..} = do
    varCandidates   <- atomically $ newTVar mempty
    varState        <- atomically $ newTVar initState

    fetchClientRegistry <- newFetchClientRegistry

    let getCandidates :: STM m (Map up (AnchoredFragment hdr))
        getCandidates = readTVar varCandidates >>=
                        traverse (fmap candidateChain . readTVar)

        blockFetchInterface :: BlockFetchConsensusInterface up hdr blk m
        blockFetchInterface = initBlockFetchConsensusInterface
          cfg chainDB getCandidates blockFetchSize blockMatchesHeader btime

        nrChainSyncClient :: up -> Consensus ChainSyncClient hdr m
        nrChainSyncClient up = chainSyncClient
          (tracePrefix "CSClient" (Just up))
          cfg
          btime
          maxClockSkew
          (ChainDB.getCurrentChain chainDB)
          (ChainDB.getCurrentLedger chainDB)
          varCandidates
          up

        nrChainSyncServer :: ChainSyncServer hdr (Point hdr) m ()
        nrChainSyncServer =
          chainSyncServer (tracePrefix "CSServer" Nothing) chainDB

        nrBlockFetchClient :: up -> BlockFetchClient hdr blk m ()
        nrBlockFetchClient up =
          blockFetchClient (tracePrefix "BFClient" (Just up)) blockFetchInterface up

        nrBlockFetchServer :: BlockFetchServer hdr blk m ()
        nrBlockFetchServer =
          blockFetchServer (tracePrefix "BFServer" Nothing) chainDB

        nrFetchClientRegistry = fetchClientRegistry

        networkRequires :: NetworkRequires m up blk hdr
        networkRequires = NetworkRequires {..}

        networkLayer :: NetworkProvides m up blk hdr
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
    :: forall m up blk hdr.
       ( MonadSTM m
       , OuroborosTag (BlockProtocol blk)
       , HasHeader hdr
       )
    => NodeConfig (BlockProtocol blk)
    -> ChainDB m blk hdr
    -> STM m (Map up (AnchoredFragment hdr))
    -> (hdr -> SizeInBytes)
    -> (hdr -> blk -> Bool)
    -> BlockchainTime m
    -> BlockFetchConsensusInterface up hdr blk m
initBlockFetchConsensusInterface cfg chainDB getCandidates blockFetchSize
    blockMatchesHeader btime = BlockFetchConsensusInterface {..}
  where
    readCandidateChains :: STM m (Map up (AnchoredFragment hdr))
    readCandidateChains = getCandidates

    readCurrentChain :: STM m (AnchoredFragment hdr)
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
    addFetchedBlock _pt = ChainDB.addBlock chainDB

    plausibleCandidateChain :: AnchoredFragment hdr
                            -> AnchoredFragment hdr
                            -> Bool
    plausibleCandidateChain = preferCandidate cfg

    compareCandidateChains :: AnchoredFragment hdr
                           -> AnchoredFragment hdr
                           -> Ordering
    compareCandidateChains = compareCandidates cfg

forkBlockProduction
    :: forall m up blk hdr.
       ( MonadAsync m
       , ProtocolLedgerView blk
       , HasHeader hdr
       , HeaderHash hdr ~ HeaderHash blk
       , TraceConstraints up blk hdr
       )
    => InternalState m up blk hdr -> m ()
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
            newBlock <- runProtocol varDRG $
              produceBlock proof l currentSlot (castPoint prevPoint) prevNo
            return $ Just newBlock

      whenJust mNewBlock $ \newBlock -> do
        traceWith tracer $
          "I'm the leader of slot " <> show currentSlot <>
          " and I produced: " <> condense newBlock
        ChainDB.addBlock chainDB newBlock
  where
    NodeCallbacks{..} = callbacks

    -- Return the point and block number of the most recent block in the
    -- current chain with a slot < the given slot. These will either
    -- correspond to the header at the tip of the current chain or, in case
    -- another node was also elected leader and managed to produce a block
    -- before us, the header right before the one at the tip of the chain.
    prevPointAndBlockNo :: SlotNo
                        -> AnchoredFragment hdr
                        -> (Point hdr, BlockNo)
    prevPointAndBlockNo slot c = case c of
        Empty _   -> (Chain.genesisPoint, Chain.genesisBlockNo)
        c' :> hdr -> case blockSlot hdr `compare` slot of
          LT -> (blockPoint hdr, blockNo hdr)
          -- The block at the tip of our chain has a slot that lies in the
          -- future.
          GT -> error "prevPointAndBlockNo: block in future"
          -- The block at the tip has the same slot as the block we're going
          -- to produce (@slot@), so look at the block before it.
          EQ | _ :> hdr' <- c'
             -> (blockPoint hdr', blockNo hdr')
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

data NetworkRequires m up blk hdr = NetworkRequires {
      -- | Start a chain sync client that communicates with the given upstream
      -- node.
      nrChainSyncClient     :: up -> ChainSyncClient hdr (Point hdr) m Void

      -- | Start a chain sync server.
    , nrChainSyncServer     :: ChainSyncServer hdr (Point hdr) m ()

      -- | Start a block fetch client that communicates with the given
      -- upstream node.
    , nrBlockFetchClient    :: up -> BlockFetchClient hdr blk m ()

      -- | Start a block fetch server server.
    , nrBlockFetchServer    :: BlockFetchServer hdr blk m ()

      -- | The fetch client registry, used by the block fetch client.
    , nrFetchClientRegistry :: FetchClientRegistry up hdr m
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
data NetworkProvides m up blk hdr = NetworkProvides {
      -- | Notify network layer of new upstream node
      --
      -- NOTE: Eventually it will be the responsibility of the network layer
      -- itself to register and deregister peers.
      npAddUpstream   :: forall eCS eBF bytesCS bytesBF.
                         (Exception eCS, Exception eBF)
                      => up
                      -> NodeComms m (ChainSync hdr (Point hdr)) eCS bytesCS
                         -- Communication for the Chain Sync protocol
                      -> NodeComms m (BlockFetch hdr blk)        eBF bytesBF
                         -- Communication for the Block Fetch protocol
                      -> m ()

      -- | Notify network layer of a new downstream node
      --
      -- NOTE: Eventually it will be the responsibility of the network layer
      -- itself to register and deregister peers.p
    , npAddDownstream :: forall eCS eBF bytesCS bytesBF.
                         (Exception eCS, Exception eBF)
                      => NodeComms m (ChainSync hdr (Point hdr)) eCS bytesCS
                         -- Communication for the Chain Sync protocol
                      -> NodeComms m (BlockFetch hdr blk)        eBF bytesBF
                         -- Communication for the Block Fetch protocol
                      -> m ()
    }

initNetworkLayer
    :: forall m up hdr blk.
       ( MonadSTM   m
       , MonadAsync m
       , MonadFork  m
       , MonadCatch m
       , MonadMask  m
       , Ord up
       )
    => Tracer m String
    -> ThreadRegistry m
    -> NetworkRequires m up blk hdr
    -> NetworkProvides m up blk hdr
initNetworkLayer _tracer registry NetworkRequires{..} = NetworkProvides {..}
  where
    npAddDownstream :: (Exception eCS, Exception eBF)
                    => NodeComms m (ChainSync hdr (Point hdr)) eCS bytesCS
                    -> NodeComms m (BlockFetch hdr blk)        eBF bytesBF
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
                  -> NodeComms m (ChainSync hdr (Point hdr)) eCS bytesCS
                  -> NodeComms m (BlockFetch hdr blk)        eBF bytesBF
                  -> m ()
    npAddUpstream up ncCS ncBF = do
      -- TODO use subregistry here?
      let NodeComms csCodec csWithChan = ncCS
          NodeComms bfCodec bfWithChan = ncBF

      clientRegistered <- newEmptyTMVarM

      void $ forkLinked registry $ bfWithChan $ \chan ->
        BlockFetchClient.bracketFetchClient nrFetchClientRegistry up $ \stateVars -> do
          atomically $ putTMVar clientRegistered ()
          -- TODO make 10 a parameter. Or encapsulate the pipelining
          -- stuff
          runPipelinedPeer 10 nullTracer bfCodec chan $
            nrBlockFetchClient up stateVars

      -- The block fetch logic thread in the background wants there to be a
      -- block fetch client thread for each chain sync candidate it sees. So
      -- start the chain sync client after the block fetch thread was
      -- registered to make sure it never sees a chain sync candidate without
      -- a corresponding block fetch client thread.
      atomically $ takeTMVar clientRegistered

      void $ forkNonTerminating registry $ csWithChan $ \chan ->
        runPeer nullTracer csCodec chan $
          chainSyncClientPeer (nrChainSyncClient up)

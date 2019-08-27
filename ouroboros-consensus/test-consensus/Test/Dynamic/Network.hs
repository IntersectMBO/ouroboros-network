{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Setup network
module Test.Dynamic.Network (
    broadcastNetwork
  , TracingConstraints
    -- * Tracers
  , MiniProtocolExpectedException (..)
  , MiniProtocolState (..)
  , TraceMiniProtocolRestart (..)
    -- * Test Output
  , TestOutput (..)
  , NodeOutput (..)
  , NodeInfo (..)
  ) where

import           Control.Monad
import           Control.Tracer
import           Crypto.Random (ChaChaDRG, drgNew)
import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Proxy (Proxy (..))
import           GHC.Stack

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork (MonadFork)
import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer

import           Network.TypedProtocol.Channel
import           Network.TypedProtocol.Codec (AnyMessage (..))

import           Ouroboros.Network.Block
import           Ouroboros.Network.MockChain.Chain

import qualified Ouroboros.Network.BlockFetch.Client as BFClient
import           Ouroboros.Network.Protocol.BlockFetch.Type
import           Ouroboros.Network.Protocol.ChainSync.Type
import           Ouroboros.Network.Protocol.TxSubmission.Type
import qualified Ouroboros.Network.TxSubmission.Inbound as TxInbound
import qualified Ouroboros.Network.TxSubmission.Outbound as TxOutbound

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime
import qualified Ouroboros.Consensus.BlockFetchServer as BFServer
import           Ouroboros.Consensus.ChainSyncClient (ClockSkew (..))
import qualified Ouroboros.Consensus.ChainSyncClient as CSClient
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Mock
import           Ouroboros.Consensus.Mempool
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Node.Tracers
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.NodeKernel
import           Ouroboros.Consensus.NodeNetwork
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Consensus.Util.RedundantConstraints
import           Ouroboros.Consensus.Util.ResourceRegistry
import           Ouroboros.Consensus.Util.STM

import qualified Ouroboros.Storage.ChainDB as ChainDB
import           Ouroboros.Storage.ChainDB.Impl (ChainDbArgs (..))
import           Ouroboros.Storage.EpochInfo (EpochInfo, newEpochInfo)
import           Ouroboros.Storage.FS.Sim.MockFS (MockFS)
import qualified Ouroboros.Storage.FS.Sim.MockFS as Mock
import           Ouroboros.Storage.FS.Sim.STM (simHasFS)
import qualified Ouroboros.Storage.ImmutableDB as ImmDB
import qualified Ouroboros.Storage.LedgerDB.DiskPolicy as LgrDB
import qualified Ouroboros.Storage.LedgerDB.MemPolicy as LgrDB
import qualified Ouroboros.Storage.Util.ErrorHandling as EH

import           Test.Dynamic.TxGen
import           Test.Dynamic.Util.NodeJoinPlan

-- | Setup fully-connected topology, where every node is both a producer
-- and a consumer, and joins according to the node join plan
--
-- We run for the specified number of blocks, then return the final state of
-- each node.
broadcastNetwork :: forall m blk.
                    ( MonadAsync m
                    , MonadFork  m
                    , MonadMask  m
                    , MonadST    m
                    , MonadTime  m
                    , MonadTimer m
                    , MonadThrow (STM m)
                    , RunNode blk
                    , TxGen blk
                    , TracingConstraints blk
                    , HasCallStack
                    )
                 => ResourceRegistry m
                 -> TestBlockchainTime m
                 -> NumCoreNodes
                 -> NodeJoinPlan
                 -> (CoreNodeId -> ProtocolInfo blk)
                 -> ChaChaDRG
                 -> DiffTime
                 -> m (TestOutput blk)
broadcastNetwork registry testBtime numCoreNodes nodeJoinPlan pInfo initRNG slotLen = do
    -- This function is organized around the notion of a network of nodes as a
    -- simple graph with no loops. The graph topology is fully-connected/mesh.
    --
    -- Each graph node is a Ouroboros core node, with its own private threads
    -- managing the node's internal state. Some nodes join the network later
    -- than others, according to @nodeJoinPlan@.
    --
    -- Each undirected edge denotes two opposing directed edges. Each directed
    -- edge denotes a bundle of mini protocols with client threads on the tail
    -- node and server threads on the head node. These mini protocols begin as
    -- soon as both nodes have joined the network, according to @nodeJoinPlan@.

    varRNG <- atomically $ newTVar initRNG

    -- allocate a TMVar for each node's network app
    nodeVars <- fmap Map.fromList $ do
      forM coreNodeIds $ \nid -> (,) nid <$> atomically newEmptyTMVar

    -- spawn threads for each undirected edge
    let meshEdges =
          [ (n1, n2) | n1 <- coreNodeIds, n2 <- coreNodeIds, n1 < n2 ]
    forM_ meshEdges $ \edge -> do
      void $ forkLinkedThread registry $ do
        undirectedEdge nullTracer nodeVars edge

    -- create nodes
    let meshNodes =
          List.sortOn fst $   -- sort non-descending by join slot
          map (\nv@(n, _) -> (joinSlotOf n, nv)) $
          Map.toList nodeVars
    nodes <- forM meshNodes $ \(joinSlot, (coreNodeId, nodeVar)) -> do
      -- do not start the node before its joinSlot
      tooLate <- blockUntilSlot btime joinSlot
      when tooLate $ do
        error $ "unsatisfiable nodeJoinPlan: " ++ show coreNodeId

      -- allocate the node's internal state and spawn its internal threads
      (node, nodeInfo, app) <- createNode varRNG coreNodeId

      -- unblock the threads of edges that involve this node
      atomically $ putTMVar nodeVar app

      return (coreNodeId, pInfoConfig (pInfo coreNodeId), node, nodeInfo)

    -- Wait some extra time after the end of the test block fetch and chain
    -- sync to finish
    testBlockchainTimeDone testBtime
    threadDelay 2000   -- arbitrary "small" duration

    -- Close the 'ResourceRegistry': this shuts down the background threads of
    -- a node. This is important because we close the ChainDBs in
    -- 'getTestOutput' and if background threads that use the ChainDB are
    -- still running at that point, they will throw a 'CloseDBError'.
    closeRegistry registry

    getTestOutput nodes
  where
    _ = keepRedundantConstraint (Proxy @(TracingConstraints blk))

    btime = testBlockchainTime testBtime

    coreNodeIds :: [CoreNodeId]
    coreNodeIds = enumCoreNodes numCoreNodes

    joinSlotOf :: CoreNodeId -> SlotNo
    joinSlotOf = coreNodeIdJoinSlot nodeJoinPlan

    undirectedEdge ::
         HasCallStack
      => Tracer m (SlotNo, MiniProtocolState, MiniProtocolExpectedException blk)
      -> Map CoreNodeId (StrictTMVar m (LimitedApp m NodeId blk))
      -> (CoreNodeId, CoreNodeId)
      -> m ()
    undirectedEdge tr nodeVars (node1, node2) = do
      -- block until both endpoints have joined the network
      (endpoint1, endpoint2) <- do
        let lu node = case Map.lookup node nodeVars of
              Nothing  -> error $ "node not found: " ++ show node
              Just var -> (,) node <$> atomically (readTMVar var)
        (,) <$> lu node1 <*> lu node2

      -- spawn threads for both directed edges
      void $ withAsyncsWaitAny $
          directedEdge tr btime endpoint1 endpoint2
        NE.:|
        [ directedEdge tr btime endpoint2 endpoint1
        ]

    -- | Produce transactions every time the slot changes and submit them to
    -- the mempool.
    txProducer :: HasCallStack
               => NodeConfig (BlockProtocol blk)
               -> m ChaChaDRG
                  -- ^ How to get a DRG
               -> STM m (ExtLedgerState blk)
                  -- ^ How to get the current ledger state
               -> Mempool m blk TicketNo
               -> m ()
    txProducer cfg produceDRG getExtLedger mempool =
      onSlotChange btime $ \_curSlotNo -> do
        drg <- produceDRG
        txs <- atomically $ do
          ledger <- ledgerState <$> getExtLedger
          varDRG <- newTVar drg
          simChaChaT varDRG id $ testGenTxs numCoreNodes cfg ledger
        void $ addTxs mempool txs

    mkArgs :: NodeConfig (BlockProtocol blk)
           -> ExtLedgerState blk
           -> EpochInfo m
           -> (StrictTVar m MockFS, StrictTVar m MockFS, StrictTVar m MockFS)
              -- ^ ImmutableDB, VolatileDB, LedgerDB
           -> ChainDbArgs m blk
    mkArgs cfg initLedger epochInfo (immDbFsVar, volDbFsVar, lgrDbFsVar) = ChainDbArgs
        { -- Decoders
          cdbDecodeHash       = nodeDecodeHeaderHash (Proxy @blk)
        , cdbDecodeBlock      = nodeDecodeBlock cfg
        , cdbDecodeLedger     = nodeDecodeLedgerState cfg
        , cdbDecodeChainState = nodeDecodeChainState (Proxy @blk)
          -- Encoders
        , cdbEncodeBlock      = nodeEncodeBlock cfg
        , cdbEncodeHash       = nodeEncodeHeaderHash (Proxy @blk)
        , cdbEncodeLedger     = nodeEncodeLedgerState cfg
        , cdbEncodeChainState = nodeEncodeChainState (Proxy @blk)
          -- Error handling
        , cdbErrImmDb         = EH.monadCatch
        , cdbErrVolDb         = EH.monadCatch
        , cdbErrVolDbSTM      = EH.throwSTM
          -- HasFS instances
        , cdbHasFSImmDb       = simHasFS EH.monadCatch immDbFsVar
        , cdbHasFSVolDb       = simHasFS EH.monadCatch volDbFsVar
        , cdbHasFSLgrDB       = simHasFS EH.monadCatch lgrDbFsVar
          -- Policy
        , cdbValidation       = ImmDB.ValidateAllEpochs
        , cdbBlocksPerFile    = 4
        , cdbMemPolicy        = LgrDB.defaultMemPolicy  (protocolSecurityParam cfg)
        , cdbDiskPolicy       = LgrDB.defaultDiskPolicy (protocolSecurityParam cfg) slotLen
          -- Integration
        , cdbNodeConfig       = cfg
        , cdbEpochInfo        = epochInfo
        , cdbIsEBB            = \blk -> if nodeIsEBB blk
                                        then Just (blockHash blk)
                                        else Nothing
        , cdbGenesis          = return initLedger
        -- Misc
        , cdbTracer           = nullTracer
        , cdbRegistry         = registry
        , cdbGcDelay          = 0
        }

    createNode
      :: HasCallStack
      => StrictTVar m ChaChaDRG
      -> CoreNodeId
      -> m ( NodeKernel m NodeId blk
           , NodeInfo blk (StrictTVar m MockFS)
           , LimitedApp m NodeId blk
           )
    createNode varRNG coreNodeId = do
      let ProtocolInfo{..} = pInfo coreNodeId

      let callbacks :: NodeCallbacks m blk
          callbacks = NodeCallbacks {
              produceBlock = \proof _l slot prevPoint prevNo txs -> do
                let curNo :: BlockNo
                    curNo = succ prevNo

                let prevHash :: ChainHash blk
                    prevHash = castHash (pointHash prevPoint)

                nodeForgeBlock pInfoConfig
                               slot
                               curNo
                               prevHash
                               txs
                               proof

            , produceDRG      = atomically $ simChaChaT varRNG id $ drgNew
            }

      epochInfo <- newEpochInfo $ nodeEpochSize (Proxy @blk) pInfoConfig
      fsVars@(immDbFsVar, volDbFsVar, lgrDbFsVar)  <- atomically $ (,,)
        <$> newTVar Mock.empty <*> newTVar Mock.empty <*> newTVar Mock.empty
      let args = mkArgs pInfoConfig pInfoInitLedger epochInfo fsVars
      chainDB <- ChainDB.openDB args

      let nodeArgs = NodeArgs
            { tracers            = nullTracers
            , registry           = registry
            , maxClockSkew       = ClockSkew 1
            , cfg                = pInfoConfig
            , initState          = pInfoInitState
            , btime
            , chainDB
            , callbacks
            , blockFetchSize     = nodeBlockFetchSize
            , blockMatchesHeader = nodeBlockMatchesHeader
            , maxUnackTxs        = 1000 -- TODO
            }

      nodeKernel <- initNodeKernel nodeArgs
      let app = consensusNetworkApps
                  nodeKernel
                  nullProtocolTracers
                  protocolCodecsId
                  (protocolHandlers nodeArgs nodeKernel)

      void $ forkLinkedThread registry $ txProducer
        pInfoConfig
        (produceDRG callbacks)
        (ChainDB.getCurrentLedger chainDB)
        (getMempool nodeKernel)

      let nodeInfo = NodeInfo
            { nodeInfoImmDbFs = immDbFsVar
            , nodeInfoVolDbFs = volDbFsVar
            , nodeInfoLgrDbFs = lgrDbFsVar
            }
      return (nodeKernel, nodeInfo, LimitedApp app)

{-------------------------------------------------------------------------------
  Running the Mini Protocols on an Ordered Pair of Nodes
-------------------------------------------------------------------------------}

-- | Spawn all mini protocols' threads for a given directed edge in the node
-- network topology (ie an ordered pair of core nodes, client first, server
-- second)
--
-- Key property: if any client thread or server thread in any of the mini
-- protocols throws an exception, restart all of the threads.
--
-- The actual node implementation kills the other threads on the same peer as
-- the thread that threw the exception, and then relies on TCP socket semantics
-- to eventually kill the corresponding threads on the remote peer. The client
-- node recreates its client threads after a delay, and they reconnect to the
-- remote peer, thereby recreating the server threads.
--
-- This mock network instead ensures the property directly via the async
-- interface rather than relying on some sort of mock socket semantics to
-- convey the cancellation.
--
-- It only catches-and-restarts on /expected/ exceptions; anything else will
-- tear down the whole hierarchy of test threads. See
-- 'MiniProtocolExpectedException'.
directedEdge ::
  forall m blk.
     ( MonadAsync m
     , MonadCatch m
     , SupportedBlock blk
     )
  => Tracer m (SlotNo, MiniProtocolState, MiniProtocolExpectedException blk)
  -> BlockchainTime m
  -> (CoreNodeId, LimitedApp m NodeId blk)
  -> (CoreNodeId, LimitedApp m NodeId blk)
  -> m ()
directedEdge tr btime nodeapp1 nodeapp2 =
    loopOnMPEE
  where
    loopOnMPEE = directedEdgeInner nodeapp1 nodeapp2 `catch` h
      where
        h :: MiniProtocolExpectedException blk -> m ()
        h e = do
          s@(SlotNo i) <- atomically $ getCurrentSlot btime
          traceWith tr (s, MiniProtocolDelayed, e)
          void $ blockUntilSlot btime $ SlotNo (succ i)
          traceWith tr (s, MiniProtocolRestarting, e)
          loopOnMPEE

-- | Spawn threads for all of the mini protocols
--
-- See 'directedEdge'.
directedEdgeInner ::
  forall m blk.
     ( MonadAsync m
     , MonadCatch m
     , SupportedBlock blk
     )
  => (CoreNodeId, LimitedApp m NodeId blk)
     -- ^ client threads on this node
  -> (CoreNodeId, LimitedApp m NodeId blk)
     -- ^ server threads on this node
  -> m ()
directedEdgeInner (node1, LimitedApp app1) (node2, LimitedApp app2) = do
    void $ (>>= withAsyncsWaitAny) $
      fmap flattenPairs $
      sequence $
      ( miniProtocol
          (wrapMPEE MPEEChainSyncClient naChainSyncClient)
          naChainSyncServer
      ) NE.:|
      [ miniProtocol
          (wrapMPEE MPEEBlockFetchClient naBlockFetchClient)
          (wrapMPEE MPEEBlockFetchServer naBlockFetchServer)
      , miniProtocol
          (wrapMPEE MPEETxSubmissionClient naTxSubmissionClient)
          (wrapMPEE MPEETxSubmissionServer naTxSubmissionServer)
      ]
  where
    flattenPairs :: forall a. NE.NonEmpty (a, a) -> NE.NonEmpty a
    flattenPairs = uncurry (<>) . NE.unzip

    miniProtocol ::
         (forall unused1 unused2.
            LimitedApp' m NodeId blk unused1 unused2
         -> NodeId
         -> Channel m msg
         -> m ())
        -- ^ client action to run on node1
      -> (forall unused1 unused2.
            LimitedApp' m NodeId blk unused1 unused2
         -> NodeId
         -> Channel m msg
         -> m ())
         -- ^ server action to run on node2
      -> m (m (), m ())
    miniProtocol client server = do
       (chan, dualChan) <- createConnectedChannels
       pure
         ( client app1 (fromCoreNodeId node2) chan
         , server app2 (fromCoreNodeId node1) dualChan
         )

    wrapMPEE ::
         Exception e
      => (e -> MiniProtocolExpectedException blk)
      -> (app -> peer -> chan -> m a)
      -> (app -> peer -> chan -> m a)
    wrapMPEE f m = \app them chan ->
        catch (m app them chan) $ throwM . f

{-------------------------------------------------------------------------------
  Node Info
-------------------------------------------------------------------------------}

data NodeInfo blk fs = NodeInfo
  { nodeInfoImmDbFs :: fs
  , nodeInfoVolDbFs :: fs
  , nodeInfoLgrDbFs :: fs
  }

readFsTVars :: MonadSTM m
            => NodeInfo blk (StrictTVar m MockFS)
            -> m (NodeInfo blk MockFS)
readFsTVars tvars = atomically $ NodeInfo
    <$> readTVar (nodeInfoImmDbFs tvars)
    <*> readTVar (nodeInfoVolDbFs tvars)
    <*> readTVar (nodeInfoLgrDbFs tvars)

{-------------------------------------------------------------------------------
  Test Output - records of how each node's chain changed
-------------------------------------------------------------------------------}

data NodeOutput blk = NodeOutput
  { nodeOutputCfg        :: NodeConfig (BlockProtocol blk)
  , nodeOutputFinalChain :: Chain blk
  , nodeOutputNodeInfo   :: NodeInfo blk MockFS
  }

newtype TestOutput blk = TestOutput
    { testOutputNodes :: Map NodeId (NodeOutput blk)
    }

-- | Gather the test output from the nodes
getTestOutput ::
    forall m blk.
       ( MonadSTM m
       , MonadMask m
       , MonadFork m
       , HasHeader blk
       )
    => [( CoreNodeId
        , NodeConfig (BlockProtocol blk)
        , NodeKernel m NodeId blk
        , NodeInfo blk (StrictTVar m MockFS)
        )]
    -> m (TestOutput blk)
getTestOutput nodes = do
    nodes' <- fmap Map.fromList $ forM nodes $ \(cid, cfg, node, nodeInfo) -> do
      let chainDB = getChainDB node
      ch <- ChainDB.toChain chainDB
      ChainDB.closeDB chainDB
      nodeInfo' <- readFsTVars nodeInfo
      let nodeOutput = NodeOutput
            { nodeOutputCfg        = cfg
            , nodeOutputFinalChain = ch
            , nodeOutputNodeInfo   = nodeInfo'
            }
      return (fromCoreNodeId cid, nodeOutput)

    pure $ TestOutput
        { testOutputNodes = nodes'
        }

{-------------------------------------------------------------------------------
  Constraints needed for verbose tracing
-------------------------------------------------------------------------------}

-- These constraints are when using @showTracer(s) debugTracer@ instead of
-- @nullTracer(s)@.
type TracingConstraints blk =
  ( Show blk
  , Show (Header blk)
  , Show (GenTx blk)
  )

{-------------------------------------------------------------------------------
  Ancillaries
-------------------------------------------------------------------------------}

-- | Spawn multiple async actions and wait for the first one to complete.
--
-- Each child thread is spawned with 'withAsync' and so won't outlive this one.
-- In the use case where each child thread only terminates on an exception, the
-- 'waitAny' ensures that this parent thread will run until a child terminates
-- with an exception, and it will also reraise that exception.
--
-- Why 'NE.NonEmpty'? An empty argument list would have blocked indefinitely,
-- which is likely not intended.
withAsyncsWaitAny :: forall m a. MonadAsync m => NE.NonEmpty (m a) -> m a
withAsyncsWaitAny = go [] . NE.toList
  where
    go acc = \case
      []   -> snd <$> waitAny acc
      m:ms -> withAsync m $ \h -> go (h:acc) ms

-- | The partially instantiation of the 'NetworkApplication' type according to
-- its use in this module
--
-- Used internal to this module, essentially as an abbreviatiation.
data LimitedApp m peer blk =
   forall unused1 unused2.
   LimitedApp (LimitedApp' m peer blk unused1 unused2)

-- | Argument of 'LimitedApp' data constructor
--
-- Used internal to this module, essentially as an abbreviatiation.
type LimitedApp' m peer blk unused1 unused2 =
    NetworkApplication m peer
        (AnyMessage (ChainSync (Header blk) (Point (Header blk))))
        (AnyMessage (BlockFetch blk))
        (AnyMessage (TxSubmission (GenTxId blk) (GenTx blk)))
        unused1 -- the local node-to-client channel types
        unused2
        ()

{-------------------------------------------------------------------------------
  Tracing
-------------------------------------------------------------------------------}

data MiniProtocolExpectedException blk
  = MPEEChainSyncClient (CSClient.ChainSyncClientException blk (Point (Header blk)))
    -- ^ see "Ouroboros.Consensus.ChainSyncClient"
    --
    -- NOTE: the second type in 'ChainSyncClientException' denotes the 'tip'.
    -- If it does not agree with the consensus client & server, 'Dynamic chain
    -- generation' tests will fail, since they will not catch the right
    -- exception.
  | MPEEBlockFetchClient BFClient.BlockFetchProtocolFailure
    -- ^ see "Ouroboros.Network.BlockFetch.Client"
  | MPEEBlockFetchServer (BFServer.BlockFetchServerException blk)
    -- ^ see "Ouroboros.Consensus.BlockFetchServer"
  | MPEETxSubmissionClient TxOutbound.TxSubmissionProtocolError
    -- ^ see "Ouroboros.Network.TxSubmission.Outbound"
  | MPEETxSubmissionServer TxInbound.TxSubmissionProtocolError
    -- ^ see "Ouroboros.Network.TxSubmission.Inbound"
  deriving (Show)

instance (SupportedBlock blk) => Exception (MiniProtocolExpectedException blk)

data MiniProtocolState = MiniProtocolDelayed | MiniProtocolRestarting
  deriving (Show)

data TraceMiniProtocolRestart peer blk
  = TraceMiniProtocolRestart
      peer peer
      SlotNo
      MiniProtocolState
      (MiniProtocolExpectedException blk)
    -- ^ us them when-start-blocking state reason
  deriving (Show)

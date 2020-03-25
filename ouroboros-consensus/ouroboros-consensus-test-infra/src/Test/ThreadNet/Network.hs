{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Setup network
module Test.ThreadNet.Network (
    runThreadNetwork
  , ForgeEbbEnv (..)
  , RekeyM
  , ThreadNetworkArgs (..)
  , TestNodeInitialization (..)
  , plainTestNodeInitialization
  , TracingConstraints
    -- * Tracers
  , MiniProtocolExpectedException (..)
  , MiniProtocolFatalException (..)
  , MiniProtocolState (..)
  , TraceMiniProtocolRestart (..)
    -- * Test Output
  , TestOutput (..)
  , NodeOutput (..)
  , NodeDBs (..)
  ) where

import           Ouroboros.Network.BlockFetch.ClientRegistry (
                     readFetchClientsStateVars)
import           Ouroboros.Network.BlockFetch.ClientState (
                     FetchClientStateVars(..), FetchRequest,
                     IsIdle (..), PeerFetchStatus (..), tryReadTMergeVar)
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF

import           Codec.CBOR.Read (DeserialiseFailure)
import qualified Control.Exception as Exn
import           Control.Monad
import qualified Control.Monad.Except as Exc
import           Control.Tracer
import           Crypto.Random (ChaChaDRG)
import qualified Data.ByteString.Lazy as Lazy
import           Data.Function (on)
import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (isJust)
import           Data.Proxy (Proxy (..))
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Typeable as Typeable
import           GHC.Stack

import           Cardano.Slotting.EpochInfo
import           Cardano.Slotting.Slot

import           Ouroboros.Network.Block
import           Ouroboros.Network.Channel
import           Ouroboros.Network.Codec (AnyMessage (..), CodecFailure,
                     mapFailureCodec)
import           Ouroboros.Network.MockChain.Chain (Chain (Genesis))
import           Ouroboros.Network.Point (WithOrigin (..))

import qualified Ouroboros.Network.BlockFetch.Client as BFClient
import           Ouroboros.Network.NodeToNode (MiniProtocolParameters (..))
import           Ouroboros.Network.Protocol.ChainSync.Type
import           Ouroboros.Network.Protocol.LocalStateQuery.Type
import           Ouroboros.Network.Protocol.LocalTxSubmission.Type
import           Ouroboros.Network.Protocol.TxSubmission.Type
import qualified Ouroboros.Network.TxSubmission.Inbound as TxInbound
import qualified Ouroboros.Network.TxSubmission.Outbound as TxOutbound

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.BlockchainTime.Mock
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Mempool
import qualified Ouroboros.Consensus.MiniProtocol.BlockFetch.Server as BFServer
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client
                     (ClockSkew (..))
import qualified Ouroboros.Consensus.MiniProtocol.ChainSync.Client as CSClient
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Server (Tip)
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Node.Tracers
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.NodeKernel as NodeKernel
import           Ouroboros.Consensus.NodeNetwork
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Consensus.Util.Random
import           Ouroboros.Consensus.Util.RedundantConstraints
import           Ouroboros.Consensus.Util.ResourceRegistry
import           Ouroboros.Consensus.Util.STM

import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import           Ouroboros.Consensus.Storage.ChainDB.Impl (ChainDbArgs (..))
import qualified Ouroboros.Consensus.Storage.ImmutableDB as ImmDB
import qualified Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index as Index
import qualified Ouroboros.Consensus.Storage.LedgerDB.DiskPolicy as LgrDB
import qualified Ouroboros.Consensus.Storage.LedgerDB.InMemory as LgrDB
import qualified Ouroboros.Consensus.Storage.VolatileDB as VolDB

import           Test.ThreadNet.TxGen
import           Test.ThreadNet.Util.NodeJoinPlan
import           Test.ThreadNet.Util.NodeRestarts
import           Test.ThreadNet.Util.NodeTopology

import           Test.Util.FS.Sim.MockFS (MockFS)
import qualified Test.Util.FS.Sim.MockFS as Mock
import           Test.Util.FS.Sim.STM (simHasFS)
import           Test.Util.Random
import           Test.Util.Tracer

-- | How to forge an EBB
--
data ForgeEbbEnv blk = ForgeEbbEnv
  { forgeEBB ::
         TopLevelConfig blk
      -> SlotNo
         -- EBB slot
      -> BlockNo
         -- EBB block number (i.e. that of its predecessor)
      -> ChainHash blk
         -- EBB predecessor's hash
      -> blk
  }

-- | How to rekey a node with a fresh operational key
--
type RekeyM m blk =
     CoreNodeId
  -> ProtocolInfo blk
  -> SlotNo
     -- ^ The slot in which the node is rekeying
  -> (SlotNo -> m EpochNo)
  -> m (TestNodeInitialization blk)
     -- ^ 'tniProtocolInfo' should include new delegation cert/operational key,
     -- and 'tniCrucialTxs' should include the new delegation certificate
     -- transaction

-- | Data used when starting/restarting a node
data TestNodeInitialization blk = TestNodeInitialization
  { tniCrucialTxs   :: [GenTx blk]
    -- ^ these transactions are added immediately and repeatedly (whenever the
    -- 'ledgerTipSlot' changes)
  , tniProtocolInfo :: ProtocolInfo blk
  }

plainTestNodeInitialization
  :: ProtocolInfo blk -> TestNodeInitialization blk
plainTestNodeInitialization pInfo = TestNodeInitialization
    { tniCrucialTxs   = []
    , tniProtocolInfo = pInfo
    }

-- | Parameters for the test node net
--
data ThreadNetworkArgs m blk = ThreadNetworkArgs
  { tnaForgeEbbEnv  :: Maybe (ForgeEbbEnv blk)
  , tnaJoinPlan     :: NodeJoinPlan
  , tnaNodeInfo     :: CoreNodeId -> TestNodeInitialization blk
  , tnaNumCoreNodes :: NumCoreNodes
  , tnaNumSlots     :: NumSlots
  , tnaRNG          :: ChaChaDRG
  , tnaRekeyM       :: Maybe (RekeyM m blk)
  , tnaRestarts     :: NodeRestarts
  , tnaSlotLength   :: SlotLength
  , tnaTopology     :: NodeTopology
  , tnaEpochSize    :: EpochSize
    -- ^ Epoch size
    --
    -- The ThreadNet tests need to know epoch boundaries in order to know when
    -- to insert EBBs, when delegation certificates become active, etc. (This
    -- is therefore not related to the /chunking/ of the immutable DB.)
    --
    -- This is temporary: once we have proper support for the hard fork
    -- combinator, 'EpochInfo' must be /derived/ from the current ledger state.
  }

{-------------------------------------------------------------------------------
  Vertex and Edge Statuses
-------------------------------------------------------------------------------}

-- | A /vertex/ denotes the \"operator of a node\"; in production, that's
-- typically a person.
--
-- There is always exactly one vertex for each genesis key. When its current
-- node instance crashes/terminates, the vertex replaces it with a new one.
-- Every node instance created by a vertex uses the same file system.
--
-- The term \"vertex\" is only explicitly used in this module. However, the
-- concept exists throughout the project; it's usually denoted by the term
-- \"node\", which can mean either \"vertex\" or \"node instance\". We take
-- more care than usual in this module to be explicit, but still often rely on
-- context.
--
data VertexStatus m blk
  = VDown !(Chain blk) !(LedgerState blk)
    -- ^ The vertex does not currently have a node instance; its previous
    -- instance stopped with this chain and ledger state (empty/initial before
    -- first instance)
  | VFalling
    -- ^ The vertex has a node instance, but it is about to transition to
    -- 'VDown' as soon as its edges transition to 'EDown'.
  | VUp !Int
      !(BlockchainTime m)
      !(NodeKernel m NodeId blk)
      !(LimitedApp m NodeId blk)
    -- ^ The vertex currently has a node instance (the @n@th), with these
    -- handles.

-- | A directed /edge/ denotes the \"operator of a node-to-node connection\";
-- in production, that's generally the TCP connection and the networking layers
-- built atop it.
--
-- There are always exactly two edges between two vertices that are connected
-- by the 'NodeTopology': one for the client-server relationship in each
-- direction. When the mini protocol instances crash, the edge replaces them
-- with new instances, possibly after a delay (see 'RestartCause').
--
-- (We do not need 'EFalling' because node instances can exist without mini
-- protocols; we only need 'VFalling' because mini protocol instances cannot
-- exist without node instances.)
--
data EdgeStatus m
  = EDown !Int !RestartCause
    -- ^ The edge does not currently have mini protocol instances (was formerly
    -- the @n@th such set).
  | EUp !Int !(STM m Bool) !(STM m Bool) !(STM m Bool)
    -- ^ The edge currently has mini protocol instances (the @n@th such set).

type VertexStatusVar m blk = StrictTVar m (VertexStatus m blk)
type EdgeStatusVar m = StrictTVar m (EdgeStatus m)

{-------------------------------------------------------------------------------
  Running the node net
-------------------------------------------------------------------------------}

-- | Setup a network of core nodes, where each joins according to the node join
-- plan and is interconnected according to the node topology
--
-- We run for the specified number of blocks, then return the final state of
-- each node.
runThreadNetwork :: forall m blk.
                    ( IOLike m
                    , RunNode blk
                    , TxGen blk
                    , TracingConstraints blk
                    , HasCallStack
                    , Eq (Header blk)
                    )
                 => ThreadNetworkArgs m blk -> m (TestOutput blk)
runThreadNetwork ThreadNetworkArgs
  { tnaForgeEbbEnv    = mbForgeEbbEnv
  , tnaJoinPlan       = nodeJoinPlan
  , tnaNodeInfo       = mkProtocolInfo
  , tnaNumCoreNodes   = numCoreNodes
  , tnaNumSlots       = numSlots
  , tnaRNG            = initRNG
  , tnaRekeyM         = mbRekeyM
  , tnaRestarts       = nodeRestarts
  , tnaSlotLength     = slotLength
  , tnaTopology       = nodeTopology
  , tnaEpochSize      = epochSize
  } = withRegistry $ \sharedRegistry -> do
    traceWith debugTracer $ show (numCoreNodes, numSlots)

    -- This shared registry is used for 'newTestBlockchainTime' and the
    -- network communication threads. Each node will create its own registry
    -- for its ChainDB.
    -- TODO each node should run in its own thread and have its own (single
    -- top-level, bracketed) registry used to spawn all of the node's threads,
    -- including its own BlockchainTime. This will allow us to use
    -- ChainDB.withDB and avoid issues with termination and using registries
    -- from the wrong thread. To stop the network, wait for all the nodes'
    -- blockchain times to be done and then kill the main thread of each node,
    -- which should terminate all other threads it spawned.
    sharedTestBtime <- newTestBlockchainTime
                         sharedRegistry
                         numSlots
                         (singletonSlotLengths slotLength)
    let sharedBtime = testBlockchainTime sharedTestBtime

    -- This function is organized around the notion of a network of nodes as a
    -- simple graph with no loops. The graph topology is determined by
    -- @nodeTopology@.
    --
    -- Each graph vertex is a node operator, and maintains its own Ouroboros
    -- core node, which in turn has its own private threads managing its
    -- internal state. Some nodes join the network later than others, according
    -- to @nodeJoinPlan@.
    --
    -- Each undirected edge denotes two opposing directed edges. Each directed
    -- edge denotes a bundle of mini protocols with client threads on the tail
    -- node and server threads on the head node. These mini protocols begin as
    -- soon as both nodes have joined the network, according to @nodeJoinPlan@.

    varRNG <- uncheckedNewTVarM initRNG

    -- allocate the status variable for each vertex
    vertexStatusVars <- fmap Map.fromList $ do
      forM coreNodeIds $ \nid -> do
        -- assume they all start with the empty chain and the same initial
        -- ledger
        let nodeInitData = mkProtocolInfo (CoreNodeId 0)
            TestNodeInitialization{tniProtocolInfo} = nodeInitData
            ProtocolInfo{pInfoInitLedger} = tniProtocolInfo
            ExtLedgerState{ledgerState} = pInfoInitLedger
        v <- uncheckedNewTVarM (VDown Genesis ledgerState)
        pure (nid, v)

    -- fork the directed edges, which also allocates their status variables
    let uedges = edgesNodeTopology nodeTopology
    edgeStatusVars <- fmap (Map.fromList . concat) $ do
      forM uedges $ \uedge -> do
        forkBothEdges
          sharedRegistry
          sharedBtime
          -- traces when/why the mini protocol instances start and stop
          nullDebugTracer
          vertexStatusVars
          uedge

    -- monitor the status variables etc
    void $ forkLinkedThread sharedRegistry $ let
      vvs  = Map.mapKeys CoreId vertexStatusVars
      devs = Map.mapKeys (\(c, s) -> (CoreId c, CoreId s)) edgeStatusVars

      loop fp1 = do
          -- wait for it to change
          fp2 <- atomically $ do
            fp2 <- getNextStableNetFingerprintSTM nodeTopology vvs devs
            check $ fp1 /= fp2
            pure fp2
          -- wait for it to stabilize again
          fp3 <- getNextStableNetFingerprint nodeTopology vvs devs fp2
          case checkStableNetFingerprint nodeTopology fp3 of
            [] -> pure ()
            ss -> error $ unlines ss
          loop fp3
      in loop (StableNetFingerprint Map.empty)

    -- fork the vertices
    let nodesByJoinSlot =
          List.sortOn fst $   -- sort non-descending by join slot
          map (\nv@(n, _) -> (joinSlotOf n, nv)) $
          Map.toList vertexStatusVars
    vertexInfos0 <- forM nodesByJoinSlot $ \vertexData -> do
      let (joinSlot, (coreNodeId, vertexStatusVar)) = vertexData

      -- the vertex cannot create its first node instance until the
      -- 'NodeJoinPlan' allows
      tooLate <- blockUntilSlot sharedBtime joinSlot
      when tooLate $ do
        error $ "unsatisfiable nodeJoinPlan: " ++ show coreNodeId

      -- fork the per-vertex state variables, including the mock filesystem
      (nodeInfo, readNodeInfo) <- newNodeInfo

      let myEdgeStatusVars =
            [ v
            | ((n1, n2), v) <- Map.toList edgeStatusVars
            , coreNodeId `elem` [n1, n2]
            ]
      forkVertex
        varRNG
        sharedTestBtime
        sharedRegistry
        coreNodeId
        vertexStatusVar
        myEdgeStatusVars
        nodeInfo

      -- Instrumentation: record the tip's block number at the onset of the
      -- slot.
      --
      -- With such a short transaction (read a few TVars) we assume this runs
      -- 1) before anything else in the slot and 2) once per slot.
      void $ forkLinkedThread sharedRegistry $ do
        let NodeInfo{nodeInfoEvents} = nodeInfo
            loop next = do
              (s, bno) <- atomically $ do
                s <- getCurrentSlot sharedBtime
                check $ s >= next
                readTVar vertexStatusVar >>= \case
                  VUp _n _btime kernel _app -> do
                    bno <- ChainDB.getTipBlockNo (getChainDB kernel)
                    pure (s, bno)
                  _ -> retry
              traceWith (nodeEventsTipBlockNos nodeInfoEvents) (s, bno)
              loop (succ s)
        loop 0

      return (coreNodeId, vertexStatusVar, readNodeInfo)

    -- Wait for the last slot to end
    testBlockchainTimeDone sharedTestBtime

    -- Collect all nodes' final chains
    vertexInfos <-
      atomically $
      forM vertexInfos0 $ \(coreNodeId, vertexStatusVar, readNodeInfo) -> do
        readTVar vertexStatusVar >>= \case
          VDown ch ldgr -> pure (coreNodeId, readNodeInfo, ch, ldgr)
          _        -> retry

    mkTestOutput vertexInfos
  where
    _ = keepRedundantConstraint (Proxy @(Show (LedgerView (BlockProtocol blk))))

    epochInfo :: EpochInfo m
    epochInfo = fixedSizeEpochInfo epochSize

    coreNodeIds :: [CoreNodeId]
    coreNodeIds = enumCoreNodes numCoreNodes

    joinSlotOf :: CoreNodeId -> SlotNo
    joinSlotOf = coreNodeIdJoinSlot nodeJoinPlan

    forkVertex
      :: StrictTVar m ChaChaDRG
      -> TestBlockchainTime m
      -> ResourceRegistry m
      -> CoreNodeId
      -> VertexStatusVar m blk
      -> [EdgeStatusVar m]
      -> NodeInfo blk (StrictTVar m MockFS) (Tracer m)
      -> m ()
    forkVertex
      varRNG
      sharedTestBtime
      sharedRegistry
      coreNodeId
      vertexStatusVar
      edgeStatusVars
      nodeInfo =
        void $ forkLinkedThread sharedRegistry $ do
          loop 0 0 tniProtocolInfo NodeRestart restarts0
      where
        TestNodeInitialization
           { tniCrucialTxs
           , tniProtocolInfo
           } = mkProtocolInfo coreNodeId

        restarts0 :: Map SlotNo NodeRestart
        restarts0 = Map.mapMaybe (Map.lookup coreNodeId) m
          where
            NodeRestarts m = nodeRestarts

        loop :: Int -> SlotNo -> ProtocolInfo blk -> NodeRestart -> Map SlotNo NodeRestart -> m ()
        loop n s pInfo nr rs = do
          -- a registry solely for the resources of this specific node instance
          (again, finalChain, finalLdgr) <- withRegistry $ \nodeRegistry -> do
            let nodeTestBtime = testBlockchainTimeClone sharedTestBtime nodeRegistry
                nodeBtime     = testBlockchainTime nodeTestBtime

            -- change the node's key and prepare a delegation transaction if
            -- the node is restarting because it just rekeyed
            tni' <- case (nr, mbRekeyM) of
              (NodeRekey, Just rekeyM) -> do
                rekeyM coreNodeId pInfo s (epochInfoEpoch epochInfo)
              _                        ->
                  pure $ plainTestNodeInitialization pInfo
            let TestNodeInitialization
                  { tniCrucialTxs   = crucialTxs'
                  , tniProtocolInfo = pInfo'
                  } = tni'

            -- allocate the node's internal state and fork its internal threads
            -- (specifically not the communication threads running the Mini
            -- Protocols, like the ChainSync Client)
            (kernel, app) <- forkNode coreNodeId
              varRNG
              nodeBtime
              nodeRegistry
              pInfo'
              nodeInfo
              (crucialTxs' ++ tniCrucialTxs)
            atomically $ do
              writeTVar vertexStatusVar $ VUp n nodeBtime kernel app

            -- wait until this node instance should stop
            again <- case Map.minViewWithKey rs of
              -- end of test
              Nothing               -> do
                testBlockchainTimeDone nodeTestBtime
                pure Nothing
              -- onset of schedule restart slot
              Just ((s', nr'), rs') -> do
                -- wait until the node should stop
                tooLate <- blockUntilSlot nodeBtime s'
                when tooLate $ do
                  error $ "unsatisfiable nodeRestarts: "
                    ++ show (coreNodeId, s')
                pure $ Just (s', pInfo', nr', rs')

            -- stop threads that depend on/stimulate the kernel
            atomically $ writeTVar vertexStatusVar VFalling
            forM_ edgeStatusVars $ \edgeStatusVar -> atomically $ do
              let isDown = \case
                      EDown{} -> True
                      EUp{}   -> False
              readTVar edgeStatusVar >>= check . isDown

            -- assuming nothing else is changing it, read the final chain
            let chainDB = getChainDB kernel
            ExtLedgerState{ledgerState} <- atomically $
              ChainDB.getCurrentLedger chainDB
            finalChain <- ChainDB.toChain chainDB

            pure (again, finalChain, ledgerState)
            -- end of the node's withRegistry

          atomically $ writeTVar vertexStatusVar $
            VDown finalChain finalLdgr

          case again of
            Nothing                     -> pure ()
            Just (s', pInfo', nr', rs') -> loop (n+1) s' pInfo' nr' rs'

    -- | Persistently attempt to add the given transactions to the mempool
    -- every time the ledger slot changes, even if successful!
    --
    -- If we add the transaction and then the mempools discards it for some
    -- reason, this thread will add it again.
    --
    forkCrucialTxs
      :: forall fingerprint.
         (Eq fingerprint, HasCallStack)
      => ResourceRegistry m
      -> (fingerprint, STM m fingerprint)
      -- ^ How to get the fingerprint of the current ledger state
      -> Mempool m blk TicketNo
      -> [GenTx blk]
         -- ^ valid transactions the node should immediately propagate
      -> m ()
    forkCrucialTxs registry (initialLdgr, getLdgr) mempool txs0 =
      void $ forkLinkedThread registry $ do
        let getFingerprint :: STM m ([TicketNo], fingerprint)
            getFingerprint = do
              -- NB the following two hypotheticals may happen independently
              --
              -- In particular, a different ledger state does not necessarily
              -- imply a different mempool snapshot.

              -- a new tx (e.g. added by TxSubmission) might render a crucial
              -- transaction valid
              mempoolFp <- (map snd . snapshotTxs) <$> getSnapshot mempool

              -- a new ledger state might render a crucial transaction valid
              ldgrFp <- getLdgr

              pure (mempoolFp, ldgrFp)

            loop fp = do
              _ <- addTxs mempool txs0
              (fp', _) <- atomically $ blockUntilChanged id fp getFingerprint
              -- avoid the race in which we wake up before the mempool's
              -- background thread wakes up by mimicking it before we do
              -- anything else
              void $ syncWithLedger mempool
              loop fp'
        loop ([], initialLdgr)

    -- | Produce transactions every time the slot changes and submit them to
    -- the mempool.
    forkTxProducer :: HasCallStack
                   => BlockchainTime m
                   -> TopLevelConfig blk
                   -> RunMonadRandom m
                   -> STM m (ExtLedgerState blk)
                      -- ^ How to get the current ledger state
                   -> Mempool m blk TicketNo
                   -> m ()
    forkTxProducer btime cfg runMonadRandomDict getExtLedger mempool =
      void $ onSlotChange btime $ \curSlotNo -> do
        ledger <- atomically $ ledgerState <$> getExtLedger
        txs    <- runMonadRandom runMonadRandomDict $ \_lift' ->
          testGenTxs numCoreNodes curSlotNo cfg ledger
        void $ addTxs mempool txs

    mkArgs :: CoreNodeId
           -> BlockchainTime m
           -> ResourceRegistry m
           -> TopLevelConfig blk
           -> ExtLedgerState blk
           -> Tracer m (RealPoint blk, ExtValidationError blk)
              -- ^ invalid block tracer
           -> Tracer m (RealPoint blk, BlockNo)
              -- ^ added block tracer
           -> NodeDBs (StrictTVar m MockFS)
           -> ChainDbArgs m blk
    mkArgs _coreNodeId
      btime registry
      cfg initLedger
      invalidTracer addTracer
      nodeDBs = ChainDbArgs
        { -- Decoders
          cdbDecodeHash           = nodeDecodeHeaderHash     (Proxy @blk)
        , cdbDecodeBlock          = nodeDecodeBlock          cfg
        , cdbDecodeHeader         = nodeDecodeHeader         cfg SerialisedToDisk
        , cdbDecodeLedger         = nodeDecodeLedgerState    cfg
        , cdbDecodeConsensusState = nodeDecodeConsensusState (Proxy @blk) cfg
        , cdbDecodeTipInfo        = nodeDecodeTipInfo        (Proxy @blk)
          -- Encoders
        , cdbEncodeHash           = nodeEncodeHeaderHash     (Proxy @blk)
        , cdbEncodeBlock          = nodeEncodeBlockWithInfo  cfg
        , cdbEncodeHeader         = nodeEncodeHeader         cfg SerialisedToDisk
        , cdbEncodeLedger         = nodeEncodeLedgerState    cfg
        , cdbEncodeConsensusState = nodeEncodeConsensusState (Proxy @blk) cfg
        , cdbEncodeTipInfo        = nodeEncodeTipInfo        (Proxy @blk)
          -- HasFS instances
        , cdbHasFSImmDb           = simHasFS (nodeDBsImm nodeDBs)
        , cdbHasFSVolDb           = simHasFS (nodeDBsVol nodeDBs)
        , cdbHasFSLgrDB           = simHasFS (nodeDBsLgr nodeDBs)
          -- Policy
        , cdbImmValidation        = ImmDB.ValidateAllChunks
        , cdbVolValidation        = VolDB.ValidateAll
        , cdbBlocksPerFile        = VolDB.mkBlocksPerFile 4
        , cdbParamsLgrDB          = LgrDB.ledgerDbDefaultParams (configSecurityParam cfg)
        , cdbDiskPolicy           = LgrDB.defaultDiskPolicy (configSecurityParam cfg)
          -- Integration
        , cdbTopLevelConfig       = cfg
        , cdbChunkInfo            = ImmDB.simpleChunkInfo epochSize
        , cdbHashInfo             = nodeHashInfo (Proxy @blk)
        , cdbIsEBB                = nodeIsEBB
        , cdbCheckIntegrity       = nodeCheckIntegrity cfg
        , cdbGenesis              = return initLedger
        , cdbBlockchainTime       = btime
        , cdbAddHdrEnv            = nodeAddHeaderEnvelope (Proxy @blk)
        , cdbImmDbCacheConfig     = Index.CacheConfig 2 60
        -- Misc
        , cdbTracer               = instrumentationTracer <> nullDebugTracer
        , cdbTraceLedger          = nullDebugTracer
        , cdbRegistry             = registry
        , cdbGcDelay              = 0
        , cdbBlocksToAddSize      = 2
        }
      where
        -- prop_general relies on this tracer
        instrumentationTracer = Tracer $ \case
          ChainDB.TraceAddBlockEvent
              (ChainDB.AddBlockValidation (ChainDB.InvalidBlock e p))
              -> traceWith invalidTracer (p, e)
          ChainDB.TraceAddBlockEvent
              (ChainDB.AddedBlockToVolDB p bno IsNotEBB)
              -> traceWith addTracer (p, bno)
          _   -> pure ()

    forkNode
      :: HasCallStack
      => CoreNodeId
      -> StrictTVar m ChaChaDRG
      -> BlockchainTime m
      -> ResourceRegistry m
      -> ProtocolInfo blk
      -> NodeInfo blk (StrictTVar m MockFS) (Tracer m)
      -> [GenTx blk]
         -- ^ valid transactions the node should immediately propagate
      -> m ( NodeKernel m NodeId blk
           , LimitedApp m NodeId blk
           )
    forkNode coreNodeId varRNG btime registry pInfo nodeInfo txs0 = do
      let ProtocolInfo{..} = pInfo

      let NodeInfo
            { nodeInfoEvents
            , nodeInfoDBs
            } = nodeInfo

      -- prop_general relies on these tracers
      let invalidTracer = (nodeEventsInvalids nodeInfoEvents)
          addTracer = Tracer $ \(p, bno) -> do
            s <- atomically $ getCurrentSlot btime
            traceWith (nodeEventsAdds nodeInfoEvents) (s, p, bno)
      let chainDbArgs = mkArgs coreNodeId
            btime registry
            pInfoConfig pInfoInitLedger
            invalidTracer
            addTracer
            nodeInfoDBs
      chainDB <- snd <$>
        allocate registry (const (ChainDB.openDB chainDbArgs)) ChainDB.closeDB

      let blockProduction :: BlockProduction m blk
          blockProduction = BlockProduction {
              produceBlock       = \lift' upd currentSlot currentBno extLdgSt txs prf -> do
                -- the typical behavior, which doesn't add a Just-In-Time EBB
                let forgeWithoutEBB =
                      nodeForgeBlock pInfoConfig upd
                        currentSlot currentBno extLdgSt txs prf

                case mbForgeEbbEnv of
                  Nothing          -> forgeWithoutEBB
                  Just forgeEbbEnv -> do
                    let ebbSlot :: SlotNo
                        ebbSlot =
                            SlotNo $ denom * div numer denom
                          where
                            SlotNo numer    = currentSlot
                            EpochSize denom = epochSize

                    let p = ledgerTipPoint $ ledgerState extLdgSt
                    let mSlot = pointSlot p
                    if (At ebbSlot <= mSlot) then forgeWithoutEBB else do
                      -- the EBB is needed
                      --
                      -- The EBB shares its BlockNo with its predecessor (if
                      -- there is one)
                      let ebbBno = case currentBno of
                            -- We assume this invariant:
                            --
                            -- If forging of EBBs is enabled then the node
                            -- initialization is responsible for producing any
                            -- proper non-EBB blocks with block number 0.
                            --
                            -- So this case is unreachable.
                            0 -> error "Error, only node initialization can forge non-EBB with block number 0."
                            n -> pred n
                      let ebb = forgeEBB forgeEbbEnv pInfoConfig
                                  ebbSlot ebbBno (pointHash p)

                      -- fail if the EBB is invalid
                      let apply = applyExtLedgerState BlockNotPreviouslyApplied
                      extLdgSt' <- case Exc.runExcept $ apply pInfoConfig ebb extLdgSt of
                        Left e   -> Exn.throw $ JitEbbError e
                        Right st -> pure st

                      -- forge the block usings the ledger state that includes
                      -- the EBB
                      blk <- nodeForgeBlock pInfoConfig upd
                        currentSlot currentBno extLdgSt' txs prf

                      -- /if the new block is valid/, add the EBB to the
                      -- ChainDB
                      --
                      -- If the new block is invalid, then adding the EBB would
                      -- be premature in some scenarios.
                      case Exc.runExcept $ apply pInfoConfig blk extLdgSt' of
                        -- ASSUMPTION: If it's invalid with the EBB,
                        -- it will be invalid without the EBB.
                        Left{}  -> forgeWithoutEBB
                        Right{} -> do
                          -- TODO: We assume this succeeds; failure modes?
                          void $ lift' $ ChainDB.addBlock chainDB ebb
                          pure blk

            , runMonadRandomDict = runMonadRandomWithTVar varRNG
            }

      -- prop_general relies on these tracers
      let instrumentationTracers = nullTracers
            { forgeTracer = nodeEventsForges nodeInfoEvents
            }
      let nodeArgs = NodeArgs
            { tracers             =
                -- traces the node's local events other than those from the
                -- ChainDB
                instrumentationTracers <> nullDebugTracers
            , registry
            , maxClockSkew           = ClockSkew 1
            , cfg                    = pInfoConfig
            , initState              = pInfoInitState
            , btime
            , chainDB
            , initChainDB            = nodeInitChainDB
            , blockProduction        = Just blockProduction
            , blockFetchSize         = nodeBlockFetchSize
            , blockMatchesHeader     = nodeBlockMatchesHeader
            , maxBlockSize           = NoOverride
            , mempoolCap             = NoMempoolCapacityBytesOverride
            , miniProtocolParameters = MiniProtocolParameters {
                  chainSyncPipelineingHighMark = 4,
                  chainSyncPipelineingLowMark  = 2,
                  blockFetchPipelineingMax     = 10,
                  txSubmissionMaxUnacked       = 1000 -- TODO ?
                }
            }

      nodeKernel <- initNodeKernel nodeArgs
      let mempool = getMempool nodeKernel
      let app = consensusNetworkApps
                  nodeKernel
                  -- these tracers report every message sent/received by this
                  -- node
                  nullDebugProtocolTracers
                  (customProtocolCodecs pInfoConfig)
                  (protocolHandlers nodeArgs nodeKernel)

      -- In practice, a robust wallet/user can persistently add a transaction
      -- until it appears on the chain. This thread adds robustness for the
      -- @txs0@ argument, which in practice contains delegation certificates
      -- that the node operator would very insistently add.
      --
      -- It's necessary here because under some circumstances a transaction in
      -- the mempool can be \"lost\" due to no fault of its own. If a dlg cert
      -- is lost, a node that rekeyed can never lead again. Moreover,
      -- promptness of certain transactions simplifies the definition of
      -- corresponding test properties: it's easier to predict whether a
      -- proposal will expire if we're ensured all votes are as prompt as
      -- possible. Lastly, the \"wallet\" might simply need to wait until
      -- enough of the chain is synced that the transaction is valid.
      --
      -- TODO Is there a risk that this will block because the 'forkTxProducer'
      -- fills up the mempool too quickly?
      forkCrucialTxs
        registry
        -- a fingerprint for the ledger
        ( (Origin, GenesisPoint)
        , do
            -- time matters, because some transaction expire
            now <- getCurrentSlot btime
            p <- (ledgerTipPoint . ledgerState) <$> ChainDB.getCurrentLedger chainDB
            pure (At now, p)
        )
        mempool
        txs0

      forkTxProducer
        btime
        pInfoConfig
        -- Uses the same varRNG as the block producer, but we split the RNG
        -- each time, so this is fine.
        (runMonadRandomWithTVar varRNG)
        (ChainDB.getCurrentLedger chainDB)
        mempool

      return (nodeKernel, LimitedApp app)

    customProtocolCodecs
      :: TopLevelConfig blk
      -> ProtocolCodecs blk CodecError m
           Lazy.ByteString
           Lazy.ByteString
           Lazy.ByteString
           Lazy.ByteString
           (AnyMessage (TxSubmission (GenTxId blk) (GenTx blk)))
           (AnyMessage (ChainSync (Serialised blk) (Tip blk)))
           (AnyMessage (LocalTxSubmission (GenTx blk) (ApplyTxErr blk)))
           (AnyMessage (LocalStateQuery blk (Query blk)))
    customProtocolCodecs cfg = ProtocolCodecs
        { pcChainSyncCodec =
            mapFailureCodec CodecBytesFailure $
            pcChainSyncCodec binaryProtocolCodecs
        , pcChainSyncCodecSerialised =
            mapFailureCodec CodecBytesFailure $
            pcChainSyncCodecSerialised binaryProtocolCodecs
        , pcBlockFetchCodec =
            mapFailureCodec CodecBytesFailure $
            pcBlockFetchCodec binaryProtocolCodecs
        , pcBlockFetchCodecSerialised =
            mapFailureCodec CodecBytesFailure $
            pcBlockFetchCodecSerialised binaryProtocolCodecs
        , pcTxSubmissionCodec =
            mapFailureCodec CodecIdFailure $
            pcTxSubmissionCodec protocolCodecsId
        , pcLocalChainSyncCodec =
            mapFailureCodec CodecIdFailure $
            pcLocalChainSyncCodec protocolCodecsId
        , pcLocalTxSubmissionCodec =
            mapFailureCodec CodecIdFailure $
            pcLocalTxSubmissionCodec protocolCodecsId
        , pcLocalStateQueryCodec =
            mapFailureCodec CodecIdFailure $
            pcLocalStateQueryCodec protocolCodecsId
        }
      where
        binaryProtocolCodecs = protocolCodecs cfg
                                 (mostRecentNetworkProtocolVersion (Proxy @blk))

-- | Sum of 'CodecFailure' (from 'protocolCodecsId') and 'DeserialiseFailure'
-- (from 'protocolCodecs').
data CodecError
  = CodecIdFailure    CodecFailure
  | CodecBytesFailure DeserialiseFailure
  deriving (Show, Exception)

{-------------------------------------------------------------------------------
  Running an edge
-------------------------------------------------------------------------------}

-- | Cause for an edge to restart
--
data RestartCause
  = RestartExn !MiniProtocolExpectedException
    -- ^ restart due to an exception in one of the mini protocol instances
    --
    -- Edges only catch-and-restart on /expected/ exceptions; anything else
    -- will tear down the whole hierarchy of test threads. See
    -- 'MiniProtocolExpectedException'.
  | RestartNode
    -- ^ restart because at least one of the two nodes is 'VFalling'
  deriving (Eq, Show)

-- | Fork two directed edges, one in each direction between the two vertices
--
forkBothEdges
  :: (IOLike m, HasCallStack)
  => ResourceRegistry m
  -> BlockchainTime m
  -> Tracer m (SlotNo, MiniProtocolState, MiniProtocolExpectedException)
  -> Map CoreNodeId (VertexStatusVar m blk)
  -> (CoreNodeId, CoreNodeId)
  -> m [((CoreNodeId, CoreNodeId), EdgeStatusVar m)]
forkBothEdges sharedRegistry btime tr vertexStatusVars (node1, node2) = do
  let endpoint1 = mkEndpoint node1
      endpoint2 = mkEndpoint node2
      mkEndpoint node = case Map.lookup node vertexStatusVars of
          Nothing  -> error $ "node not found: " ++ show node
          Just var -> (node, var)

  let mkDirEdge e1 e2 = do
        v <- uncheckedNewTVarM (EDown 0 RestartNode)
        void $ forkLinkedThread sharedRegistry $ do
          directedEdge tr btime v e1 e2
        pure ((fst e1, fst e2), v)

  ev12 <- mkDirEdge endpoint1 endpoint2
  ev21 <- mkDirEdge endpoint2 endpoint1

  pure [ev12, ev21]

-- | Spawn all mini protocols' threads for a given directed edge in the node
-- network topology (ie an ordered pair of core nodes, with client first and
-- server second)
--
-- The edge cannot start until both nodes are simultaneously 'VUp'.
--
-- The edge may restart itself for the reasons modeled by 'RestartCause'
--
-- The actual control flow here does not faithfully model the real
-- implementation. On an exception, for example, the actual node implementation
-- kills the other threads on the same peer as the thread that threw the
-- exception, and then relies on TCP socket semantics to eventually kill the
-- corresponding threads on the remote peer. The client node recreates its
-- client threads after a delay, and they reconnect to the remote peer, thereby
-- recreating the server threads.
--
-- This model instead propagates the exception to the rest of the /un/directed
-- edge via the @async@ interface rather than relying on some sort of mock
-- socket semantics to convey the cancellation.
directedEdge ::
  forall m blk. IOLike m
  => Tracer m (SlotNo, MiniProtocolState, MiniProtocolExpectedException)
  -> BlockchainTime m
  -> EdgeStatusVar m
  -> (CoreNodeId, VertexStatusVar m blk)
  -> (CoreNodeId, VertexStatusVar m blk)
  -> m ()
directedEdge tr btime edgeStatusVar client server =
    loop 0
  where
    loop n = do
        restart <- directedEdgeInner n edgeStatusVar client server
          `catch` (pure . RestartExn)
          `catch` hUnexpected
        atomically $ writeTVar edgeStatusVar (EDown n restart)
        case restart of
          RestartNode  -> pure ()
          RestartExn e -> do
            -- "error policy": restart at beginning of next slot
            s <- atomically $ getCurrentSlot btime
            let s' = succ s
            traceWith tr (s, MiniProtocolDelayed, e)
            void $ blockUntilSlot btime s'
            traceWith tr (s', MiniProtocolRestarting, e)
        loop (n+1)
      where
        -- Wrap synchronous exceptions in 'MiniProtocolFatalException'
        --
        hUnexpected :: forall a. SomeException -> m a
        hUnexpected e@(Exn.SomeException e') = case fromException e of
          Just (_ :: Exn.AsyncException) -> throwM e
          Nothing                        -> case fromException e of
            Just (_ :: Exn.SomeAsyncException) -> throwM e
            Nothing                            -> throwM MiniProtocolFatalException
              { mpfeType   = Typeable.typeOf e'
              , mpfeExn    = e
              , mpfeClient = fst client
              , mpfeServer = fst server
              }

-- | Spawn threads for all of the mini protocols
--
-- See 'directedEdge'.
directedEdgeInner ::
  forall m blk. IOLike m
  => Int
  -> EdgeStatusVar m
  -> (CoreNodeId, VertexStatusVar m blk)
     -- ^ client threads on this node
  -> (CoreNodeId, VertexStatusVar m blk)
     -- ^ server threads on this node
  -> m RestartCause
directedEdgeInner n edgeStatusVar
  (node1, vertexStatusVar1) (node2, vertexStatusVar2) = do
    -- block until both nodes are 'VUp'
    (LimitedApp app1, LimitedApp app2) <- atomically $ do
      (,) <$> getApp vertexStatusVar1 <*> getApp vertexStatusVar2

    let miniProtocol ::
             (forall unused1 unused2 unused3.
                LimitedApp' m NodeId blk unused1 unused2 unused3
             -> NodeId
             -> Channel m msg
             -> m ())
            -- ^ client action to run on node1
          -> (forall unused1 unused2 unused3.
                LimitedApp' m NodeId blk unused1 unused2 unused3
             -> NodeId
             -> Channel m msg
             -> m ())
             -- ^ server action to run on node2
          -> m (STM m Bool, (m (), m ()))
        miniProtocol client server = do
           pair <- createConnectedTestChannelPair
           pure
             ( testChannelEmpty pair
             , ( client app1 (fromCoreNodeId node2) (testChannel1 pair)
               , server app2 (fromCoreNodeId node1) (testChannel2 pair)
               )
             )

    chainSync <- miniProtocol
        (wrapMPEE MPEEChainSyncClient naChainSyncClient)
        naChainSyncServer
    blockFetch <- miniProtocol
        (wrapMPEE MPEEBlockFetchClient naBlockFetchClient)
        (wrapMPEE MPEEBlockFetchServer naBlockFetchServer)
    txSub <- miniProtocol
        (wrapMPEE MPEETxSubmissionClient naTxSubmissionClient)
        (wrapMPEE MPEETxSubmissionServer naTxSubmissionServer)

    atomically $ writeTVar edgeStatusVar $
      EUp n (fst chainSync) (fst blockFetch) (fst txSub)

    -- NB only 'watcher' ever returns in these tests
    fmap (\() -> RestartNode) $
      withAsyncsWaitAny $
      flattenPairs $
        (watcher vertexStatusVar1, watcher vertexStatusVar2) NE.:|
        [ snd chainSync
        , snd blockFetch
        , snd txSub
        ]
  where
    getApp v = readTVar v >>= \case
      VUp _n _btime _kernel app -> pure app
      _                         -> retry

    flattenPairs :: forall a. NE.NonEmpty (a, a) -> NE.NonEmpty a
    flattenPairs = uncurry (<>) . NE.unzip

    -- TODO only wrap actually expected exceptions
    wrapMPEE ::
         Exception e
      => (e -> MiniProtocolExpectedException)
      -> (app -> peer -> chan -> m a)
      -> (app -> peer -> chan -> m a)
    wrapMPEE f m = \app them chan ->
        catch (m app them chan) $ throwM . f

    -- terminates when the vertex starts 'VFalling'
    --
    -- because of 'withAsyncsWaitAny' used above, this brings down the whole
    -- edge
    watcher :: VertexStatusVar m blk -> m ()
    watcher v = do
        atomically $ readTVar v >>= \case
          VFalling -> pure ()
          _        -> retry

{-------------------------------------------------------------------------------
  Node information not bound to lifetime of a specific node instance
-------------------------------------------------------------------------------}

data NodeInfo blk db ev = NodeInfo
  { nodeInfoEvents :: NodeEvents blk ev
  , nodeInfoDBs    :: NodeDBs db
  }

-- | A vector with an @ev@-shaped element for a particular set of
-- instrumentation events
--
-- The @ev@ type parameter is instantiated by this module at types for
-- 'Tracer's and lists: actions for accumulating and lists as accumulations.
data NodeEvents blk ev = NodeEvents
  { nodeEventsAdds        :: ev (SlotNo, RealPoint blk, BlockNo)
    -- ^ every 'AddedBlockToVolDB' excluding EBBs
  , nodeEventsForges      :: ev (TraceForgeEvent blk (GenTx blk))
    -- ^ every 'TraceForgeEvent'
  , nodeEventsInvalids    :: ev (RealPoint blk, ExtValidationError blk)
    -- ^ the point of every 'ChainDB.InvalidBlock' event
  , nodeEventsTipBlockNos :: ev (SlotNo, WithOrigin BlockNo)
    -- ^ 'ChainDB.getTipBlockNo' for each node at the onset of each slot
  }

-- | A vector with an element for each database of a node
--
-- The @db@ type parameter is instantiated by this module at types for mock
-- filesystems; either the 'MockFS' type or reference cells thereof.
data NodeDBs db = NodeDBs
  { nodeDBsImm :: db
  , nodeDBsVol :: db
  , nodeDBsLgr :: db
  }

newNodeInfo ::
  forall blk m.
     IOLike m
  => m ( NodeInfo blk (StrictTVar m MockFS) (Tracer m)
       , m (NodeInfo blk MockFS [])
       )
newNodeInfo = do
  (nodeInfoEvents, readEvents) <- do
      (t1, m1) <- recordingTracerTVar
      (t2, m2) <- recordingTracerTVar
      (t3, m3) <- recordingTracerTVar
      (t4, m4) <- recordingTracerTVar
      pure
          ( NodeEvents     t1     t2     t3     t4
          , NodeEvents <$> m1 <*> m2 <*> m3 <*> m4
          )

  (nodeInfoDBs, readDBs) <- do
      let mk :: m (StrictTVar m MockFS, STM m MockFS)
          mk = do
              v <- uncheckedNewTVarM Mock.empty
              pure (v, readTVar v)
      (v1, m1) <- mk
      (v2, m2) <- mk
      (v3, m3) <- mk
      pure
          ( NodeDBs     v1     v2     v3
          , NodeDBs <$> m1 <*> m2 <*> m3
          )

  pure
      ( NodeInfo{nodeInfoEvents, nodeInfoDBs}
      , NodeInfo <$> readEvents <*> atomically readDBs
      )

{-------------------------------------------------------------------------------
  Test Output - output data about each node
-------------------------------------------------------------------------------}

data NodeOutput blk = NodeOutput
  { nodeOutputAdds        :: Map SlotNo (Set (RealPoint blk, BlockNo))
  , nodeOutputFinalChain  :: Chain blk
  , nodeOutputFinalLedger :: LedgerState blk
  , nodeOutputForges      :: Map SlotNo blk
  , nodeOutputInvalids    :: Map (RealPoint blk) [ExtValidationError blk]
  , nodeOutputNodeDBs     :: NodeDBs MockFS
  }

data TestOutput blk = TestOutput
    { testOutputNodes       :: Map NodeId (NodeOutput blk)
    , testOutputTipBlockNos :: Map SlotNo (Map NodeId (WithOrigin BlockNo))
    }

-- | Gather the test output from the nodes
mkTestOutput ::
    forall m blk. (IOLike m, HasHeader blk)
    => [( CoreNodeId
        , m (NodeInfo blk MockFS [])
        , Chain blk
        , LedgerState blk
        )]
    -> m (TestOutput blk)
mkTestOutput vertexInfos = do
    (nodeOutputs', tipBlockNos') <- fmap unzip $ forM vertexInfos $
      \(cid, readNodeInfo, ch, ldgr) -> do
        let nid = fromCoreNodeId cid
        nodeInfo <- readNodeInfo
        let NodeInfo
              { nodeInfoEvents
              , nodeInfoDBs
              } = nodeInfo
        let NodeEvents
              { nodeEventsAdds
              , nodeEventsForges
              , nodeEventsInvalids
              , nodeEventsTipBlockNos
              } = nodeInfoEvents
        let nodeOutput = NodeOutput
              { nodeOutputAdds        =
                  Map.fromListWith Set.union $
                  [ (s, Set.singleton (p, bno)) | (s, p, bno) <- nodeEventsAdds ]
              , nodeOutputFinalChain  = ch
              , nodeOutputFinalLedger = ldgr
              , nodeOutputForges      =
                  Map.fromList $
                  [ (s, b) | TraceForgedBlock s _ b _ <- nodeEventsForges ]
              , nodeOutputInvalids    = (:[]) <$> Map.fromList nodeEventsInvalids
              , nodeOutputNodeDBs     = nodeInfoDBs
              }

        pure
          ( Map.singleton nid nodeOutput
          , Map.singleton nid <$> Map.fromList nodeEventsTipBlockNos
          )

    pure $ TestOutput
        { testOutputNodes       = Map.unions nodeOutputs'
        , testOutputTipBlockNos = Map.unionsWith Map.union tipBlockNos'
        }

{-------------------------------------------------------------------------------
  Constraints needed for verbose tracing
-------------------------------------------------------------------------------}

-- | Occurs throughout in positions that might be useful for debugging.
nullDebugTracer :: (Applicative m, Show a) => Tracer m a
nullDebugTracer = nullTracer `asTypeOf` showTracing debugTracer

-- | Occurs throughout in positions that might be useful for debugging.
nullDebugTracers ::
     ( Monad m
     , Show peer
     , LedgerSupportsProtocol blk
     , TracingConstraints blk
     )
  => Tracers m peer blk
nullDebugTracers = nullTracers `asTypeOf` showTracers debugTracer

-- | Occurs throughout in positions that might be useful for debugging.
nullDebugProtocolTracers ::
     ( Monad m
     , HasHeader blk
     , TracingConstraints blk
     , Show peer
     , Show localPeer
     )
  => ProtocolTracers m peer localPeer blk failure
nullDebugProtocolTracers =
  nullProtocolTracers `asTypeOf` showProtocolTracers debugTracer

-- These constraints are when using @showTracer(s) debugTracer@ instead of
-- @nullTracer(s)@.
type TracingConstraints blk =
  ( Show blk
  , Show (ApplyTxErr blk)
  , Show (Header blk)
  , Show (GenTx blk)
  , Show (GenTxId blk)
  , ShowQuery (Query blk)
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
withAsyncsWaitAny :: forall m a. IOLike m => NE.NonEmpty (m a) -> m a
withAsyncsWaitAny = go [] . NE.toList
  where
    go acc = \case
      []   -> snd <$> waitAny acc
      m:ms -> withAsync m $ \h -> go (h:acc) ms

-- | The partially instantiation of the 'NetworkApplication' type according to
-- its use in this module
--
-- Used internal to this module, essentially as an abbreviation.
data LimitedApp m peer blk =
   forall unused1 unused2 unused3.
   LimitedApp (LimitedApp' m peer blk unused1 unused2 unused3)

-- | Argument of 'LimitedApp' data constructor
--
-- Used internal to this module, essentially as an abbreviation.
type LimitedApp' m peer blk unused1 unused2 unused3 =
    NetworkApplication m peer peer
        -- The 'ChainSync' and 'BlockFetch' protocols use @'Serialised' x@ for
        -- the servers and @x@ for the clients. Since both have to match to be
        -- sent across a channel, we can't use @'AnyMessage' ..@, instead, we
        -- (de)serialise the messages so that they can be sent across the
        -- channel with the same type on both ends, i.e., 'Lazy.ByteString'.
        Lazy.ByteString
        Lazy.ByteString
        (AnyMessage (TxSubmission (GenTxId blk) (GenTx blk)))
        unused1 -- the local node-to-client channel types
        unused2
        unused3
        ()

{-------------------------------------------------------------------------------
  Tracing
-------------------------------------------------------------------------------}

-- | Non-fatal exceptions expected from the threads of a 'directedEdge'
--
data MiniProtocolExpectedException
  = MPEEChainSyncClient CSClient.ChainSyncClientException
    -- ^ see "Ouroboros.Consensus.MiniProtocol.ChainSync.Client"
    --
    -- NOTE: the second type in 'ChainSyncClientException' denotes the 'tip'.
    -- If it does not agree with the consensus client & server, 'Dynamic chain
    -- generation' tests will fail, since they will not catch the right
    -- exception.
  | MPEEBlockFetchClient BFClient.BlockFetchProtocolFailure
    -- ^ see "Ouroboros.Network.BlockFetch.Client"
  | MPEEBlockFetchServer BFServer.BlockFetchServerException
    -- ^ see "Ouroboros.Consensus.MiniProtocol.BlockFetch.Server"
  | MPEETxSubmissionClient TxOutbound.TxSubmissionProtocolError
    -- ^ see "Ouroboros.Network.TxSubmission.Outbound"
  | MPEETxSubmissionServer TxInbound.TxSubmissionProtocolError
    -- ^ see "Ouroboros.Network.TxSubmission.Inbound"
  deriving (Eq, Show)

instance Exception MiniProtocolExpectedException

data MiniProtocolState = MiniProtocolDelayed | MiniProtocolRestarting
  deriving (Show)

data TraceMiniProtocolRestart peer
  = TraceMiniProtocolRestart
      peer peer
      SlotNo
      MiniProtocolState
      MiniProtocolExpectedException
    -- ^ us them when-start-blocking state reason
  deriving (Show)

-- | Any synchronous exception from a 'directedEdge' that was not handled as a
-- 'MiniProtocolExpectedException'
--
data MiniProtocolFatalException = MiniProtocolFatalException
  { mpfeType   :: !Typeable.TypeRep
    -- ^ Including the type explicitly makes it easier for a human to debug
  , mpfeExn    :: !SomeException
  , mpfeClient :: !CoreNodeId
  , mpfeServer :: !CoreNodeId
  }
  deriving (Show)

instance Exception MiniProtocolFatalException

-- | Our scheme for Just-In-Time EBBs makes some assumptions
--
data JitEbbError blk
  = JitEbbError (ExtValidationError blk)
    -- ^ we were unable to extend the ledger state with the JIT EBB
  deriving (Show)

instance LedgerSupportsProtocol blk => Exception (JitEbbError blk)

{-------------------------------------------------------------------------------
  Detecting stable states of the net
-------------------------------------------------------------------------------}

-- | Fingerprint of those state variables in a client node relevant to chain
-- selection with a peer server node
--
-- This is the latest output from the ChainSync client thread, the BlockFetch
-- client thread, and the client node's fetch logic thread.
--
data EUpFingerprint hdr = MkEUpFingerprint
  { ufpBlockFetch :: PeerFetchStatus hdr
  , ufpChainSync  :: AnchoredFragment hdr
  , ufpCounter    :: !Int
  , ufpFetchLogic :: Maybe (FetchRequest hdr)
  }
  deriving (Show)

instance (Eq hdr, StandardHash hdr) => Eq (EUpFingerprint hdr) where
  MkEUpFingerprint l1 l2 l3 l4 == MkEUpFingerprint r1 r2 r3 r4 =
    l1 == r1 &&
    l2 == r2 &&
    l3 == r3 &&
    -- since ClientState.hs doesn't instantiate Eq (FetchRequest hdr)
    on (==) (fmap BFClient.fetchRequestFragments) l4 r4

data EDownFingerprint = MkEDownFingerprint
  { dfpCause   :: RestartCause
  , dfpCounter :: !Int
  }
  deriving (Eq, Show)

data DirectedEdgeFingerprint hdr =
    EDownFingerprint !EDownFingerprint
  | EUpFingerprint !(EUpFingerprint hdr)
  deriving (Eq, Show)

-- | Fingerprint of those state variables in a node relevant to chain selection
-- throughout the net
--
data VertexFingerprint peer hdr = VertexFingerprint
  { vfpChainDB       :: Tip hdr
  , vfpClock         :: SlotNo
  , vfpCounter       :: !Int
  , vfpDirectedEdges :: Map peer (DirectedEdgeFingerprint hdr)
  }
  deriving (Eq, Show)

-- | Fingerprint of a /stable/ state of a net relevant to chain selection
--
-- This involves no channels because all channels in a stable state are assumed
-- to be empty.
--
newtype StableNetFingerprint peer hdr =
    StableNetFingerprint (Map peer (VertexFingerprint peer hdr))
 deriving (Eq, Show)

-- | Each element is an error message
--
checkStableNetFingerprint
  :: (HasHeader hdr, Show hdr)
  => NodeTopology
  -> StableNetFingerprint NodeId hdr
  -> [String]
checkStableNetFingerprint topo (StableNetFingerprint m) =
    concatMap checkVertex (Map.toList m) <>
    concatMap checkUedgeExists (edgesNodeTopology topo)
    -- TODO confirm any missing nodes haven't joined yet via NodeJoinPlan
  where
    checkVertex (client, vClient) =
        map (\s -> show client <> " " <> s) $
          concatMap (checkDedge vClient) (Map.toList vfpDirectedEdges) <>
          case vfpChainDB of
            TipGenesis -> []
            Tip s _ _  ->
                [ "CDB tip is from the future " <> show (s, vfpClock)
                | s > vfpClock
                ]
      where
        VertexFingerprint
          { vfpChainDB
          , vfpClock
          , vfpDirectedEdges
          } = vClient
      
    checkDedge vClient (server, EUpFingerprint ufp) =
        map (\s -> show server <> " " <> s) $

        [ "BF status is not ready " <> show ufpBlockFetch
        | ufpBlockFetch /= PeerFetchStatusReady Set.empty IsIdle
        ] <>

        [ "BF request is not empty " <> show ufpFetchLogic
        | isJust ufpFetchLogic
        ] <>

        -- TODO use 'Ouroboros.Consensus.Protocol.Abstract.preferCandidate'
        [ "candidate is preferable to CDB tip " <>
            show (AF.head ufpChainSync, cdbTip)
        | let cdbTip = vfpChainDB
        , case cdbTip of
            TipGenesis  -> Origin /= candidateBNo
            Tip _ _ bno -> candidateBNo > At bno
        ] <>

        checkServer ufpChainSync (Map.lookup server m)
      where
        VertexFingerprint{vfpChainDB} = vClient
        MkEUpFingerprint
          { ufpBlockFetch
          , ufpChainSync
          , ufpFetchLogic
          } = ufp
        candidateBNo = AF.headBlockNo ufpChainSync
    checkDedge _      (server, EDownFingerprint dfp) =
        map (\s -> show server <> " " <> s) $
        case dfpCause of
          RestartNode  ->
              [ "a node restart was apparently not instantaneous" ]
          -- TODO confirm via...
          --
          -- * NodeJoinPlan should suffice for non-Praos
          --
          -- * not sure yet about Praos
          RestartExn
            (MPEEChainSyncClient
              CSClient.ForkTooDeep{})
                       -> []
          -- TODO confirm similarly to ForkTooDeep?
          RestartExn
            (MPEEChainSyncClient
              CSClient.NoMoreIntersection{})
                       -> []
          RestartExn e ->
              [ "unexpected mini protocol fatal exception " <> show e ]
      where
        MkEDownFingerprint{dfpCause} = dfp

    checkServer candidate = \case
        Nothing     ->
            -- if the dedge is present, both vertices must be too
            [ "client's server is not in stable state" ]
        Just vertex ->
            [ "candidate is not server's tip"
            | AF.anchorToTip (AF.headAnchor candidate) /= vfpChainDB
            ]
          where
            VertexFingerprint{vfpChainDB} = vertex

    checkUedgeExists (cClient, cServer) =
        case (Map.lookup client m, Map.lookup server m) of
          (Just vClient, Just vServer) ->
              checkDedgeExists (client, vClient) (server, vServer) <>
              checkDedgeExists (server, vServer) (client, vClient)
          _                  -> []   -- TODO confirm with NodeJoinPlan
      where
        client = CoreId cClient
        server = CoreId cServer

    checkDedgeExists (client, vClient) (server, _vServer) =

        [ "Directed edge missing! " <> show (client, server)
        | Map.notMember server vfpDirectedEdges
        ]

      where
        VertexFingerprint{vfpDirectedEdges} = vClient

getNextStableNetFingerprint
  :: (IOLike m, HasHeader (Header blk), Eq (Header blk))
  => NodeTopology
  -> Map NodeId (VertexStatusVar m blk)
  -> Map (NodeId, NodeId) (EdgeStatusVar m)
  -> StableNetFingerprint NodeId (Header blk)
  -> m (StableNetFingerprint NodeId (Header blk))
getNextStableNetFingerprint topo vvs devs = go 0
  where
    yield = atomically (return ())

    -- Our core assumption here is ASSUMPTION: If this thread sees the same
    -- fingerprint before and after yielding @iteration@-many times, then all
    -- other threads (including currently-irrelevant threads like TxSub, though
    -- I don't anticipate that mattering) in the @io-sim@ulation are blocked,
    -- waiting for a node 'BlockchainTime' to tick. So, the number of
    -- iterations is asserted here as an assumed upper bound on the
    -- \"complexity\" of each thread in a running node.
    --
    -- This approach relies on all relevant computations being instantaneous in
    -- @io-sim@. E.G. If we were to add a random 'threadDelay' prior to forging
    -- a block to more emulate the fact that that computation is not
    -- instantaneous on real machine, then this implementation would conclude
    -- the net is stable during that 'threadDelay', even though it should not.
    --
    -- Here are some possible alternatives to this implementation, all of which
    -- would still rely on all relevant computations being instantaneous in
    -- @io-sim@.
    --
    --  * Use threadDelay @1@ instead of yielding a fixed number of times. If
    --    all other @threadDelays@ are significantly greater than 1, then this
    --    should also work.
    --
    --  * Add a hook to the @io-sim@ interface that lets us simply sleep until
    --    we're the only otherwise-unblocked thread.
    --
    -- The approaches discussed above are all indirect in the sense that they
    -- don't require us to explicitly track all the relevant threads we're
    -- monitoring. Our ultimate goal is to wait until all /relevant/ threads
    -- are blocked in an 'STM' transaction, in particular until they are all
    -- transitively waiting for the 'BlockchainTime's' underlying 'STM'
    -- variables to tick. (Note that /relevant/ excludes those background
    -- threads that rely on 'threadDelay' directly instead of waiting for the
    -- slot to change.)
    --
    iterations = 10000  -- TODO tune the iteration count

    go n fp0
      | n >= (10000 :: Int) = error "safety pressure release valve opened!"
      | otherwise = do
        -- give all other threads a generous opportunity to finish their
        -- computations
        replicateM_ iterations yield
        fp1 <- atomically $ getNextStableNetFingerprintSTM topo vvs devs
        -- ASSUMPTION: We're assuming that the threads' computation are
        -- effectively idempotent if the 'STM' variables they're blocked on are
        -- written to but without actually changing the contained value.
        if fp0 /= fp1 then go (n + 1) fp1 else pure fp0

getNextStableNetFingerprintSTM
  :: forall m blk.
     (IOLike m, HasHeader (Header blk))
  => NodeTopology
  -> Map NodeId (VertexStatusVar m blk)
  -> Map (NodeId, NodeId) (EdgeStatusVar m)
  -> STM m (StableNetFingerprint NodeId (Header blk))
getNextStableNetFingerprintSTM topo vvs devs =
    StableNetFingerprint <$> Map.traverseWithKey vertex vvs
  where
    vertex
      :: NodeId
      -> VertexStatusVar m blk
      -> STM m (VertexFingerprint NodeId (Header blk))
    vertex client = \vsv -> readTVar vsv >>= \case
        VUp n btime kernel _app -> do
            vfpChainDB     <-
              fmap castTip $ ChainDB.getCurrentTip $ getChainDB kernel
            vfpClock       <- getCurrentSlot btime
            let vfpCounter  = n

            -- pair up the BF and CS servers
            serversCS <- readTVar $ getNodeCandidates kernel
            serversBF <-
              readFetchClientsStateVars $ getFetchClientRegistry kernel

            -- not stable if BF and CS servers are not paired
            check $ Map.keysSet serversCS == Map.keysSet serversBF

            let servers = Map.fromAscList $
                    [ (k, (v1, v2))
                    | ((k, v1), v2) <-
                      Map.toList serversCS `zip` Map.elems serversBF
                    ]
            vfpDirectedEdges <- dedges client servers

            pure VertexFingerprint{..}

        -- not stable if not up, since node restarts are currently
        -- \"instantaneous\" and node status vars do not exist before the node
        -- first joins the network
        VDown{}         -> retry
        VFalling{}      -> retry

    dedges
      :: NodeId
      -> Map NodeId
           ( StrictTVar m (AnchoredFragment (Header blk))
           , FetchClientStateVars m (Header blk)
           )
      -> STM m (Map NodeId (DirectedEdgeFingerprint (Header blk)))
    dedges client servers = do
      let neighbors = case client of
              CoreId c  -> CoreId <$> coreNodeIdNeighbors topo c
              RelayId{} -> error "impossible! RelayId"
          collide = error "impossible! EUp and EDown"

      up   <- dedgesUp   client servers
      down <- dedgesDown client $ filter (`Map.notMember` up) neighbors

      pure $ Map.unionWith collide
        (fmap EUpFingerprint   up)
        (fmap EDownFingerprint down)

    dedgesUp
      :: NodeId
      -> Map NodeId
           ( StrictTVar m (AnchoredFragment (Header blk))
           , FetchClientStateVars m (Header blk)
           )
      -> STM m (Map NodeId (EUpFingerprint (Header blk)))
    dedgesUp client servers =
        flip Map.traverseWithKey servers $ \server (sync, fetch) -> do
          ufpChainSync <- readTVar sync
          ufpBlockFetch <- readTVar $ fetchClientStatusVar fetch
          ufpFetchLogic <- do
            let v = fetchClientRequestVar fetch
                wibble (fr, _gsv, _lims) = fr
            fmap wibble <$> tryReadTMergeVar v
          case Map.lookup (client, server) devs of
            Nothing ->
                error "impossible! no edge var"
            Just dev -> readTVar dev >>= \case
                -- we never expect a down edge to still be in the
                -- FetchClientRegistry
                EDown _nE _restart ->
                    error $ "an EDown edge is in the fetch client registry"
                -- if edge is up, the relevant channels must be empty for the
                -- net to be stable
                EUp nE isEmptyCS isEmptyBF _isEmptyTS -> do
                    ((&&) <$> isEmptyCS <*> isEmptyBF) >>= check
                    let ufpCounter = nE
                    pure MkEUpFingerprint{..}

    dedgesDown
      :: NodeId
      -> [NodeId]
      -> STM m (Map NodeId EDownFingerprint)
    dedgesDown client servers =
        fmap Map.fromList $ forM servers $ \server -> (,) server <$>
          case Map.lookup (client, server) devs of
            Nothing ->
                error "impossible! no edge var"
            Just dev -> readTVar dev >>= \case
                -- edges can be down in a stable net (they restart at the next
                -- slot onset)
                EDown nE restart -> do
                    let dfpCause = restart
                        dfpCounter = nE
                    pure MkEDownFingerprint{..}
                -- once an edge is up, it should be in the FetchClientRegistry
                -- \"instantaneously\", so this case indicates the net state is
                -- not yet stable
                EUp _nE _isEmptyCS _isEmptyBF _isEmptyTS ->
                    retry

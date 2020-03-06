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

import           Codec.CBOR.Read (DeserialiseFailure)
import qualified Control.Exception as Exn
import           Control.Monad
import qualified Control.Monad.Except as Exc
import           Control.Tracer
import           Crypto.Random (ChaChaDRG)
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (maybeToList)
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
import           Ouroboros.Network.Protocol.ChainSync.PipelineDecision
                     (pipelineDecisionLowHighMark)
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
  -> m (ProtocolInfo blk, Maybe (GenTx blk))
     -- ^ resulting config and any corresponding delegation certificate transaction

-- | Parameters for the test node net
--
data ThreadNetworkArgs m blk = ThreadNetworkArgs
  { tnaForgeEbbEnv  :: Maybe (ForgeEbbEnv blk)
  , tnaJoinPlan     :: NodeJoinPlan
  , tnaNodeInfo     :: CoreNodeId -> ProtocolInfo blk
  , tnaNumCoreNodes :: NumCoreNodes
  , tnaNumSlots     :: NumSlots
  , tnaRNG          :: ChaChaDRG
  , tnaRekeyM       :: Maybe (RekeyM m blk)
  , tnaRestarts     :: NodeRestarts
  , tnaSlotLengths  :: SlotLengths
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
  = VDown (Chain blk)
    -- ^ The vertex does not currently have a node instance; its previous
    -- instance stopped with this chain (empty before first instance)
  | VFalling
    -- ^ The vertex has a node instance, but it is about to transition to
    -- 'VDown' as soon as its edges transition to 'EDown'.
  | VUp !(NodeKernel m NodeId blk) !(LimitedApp m NodeId blk)
    -- ^ The vertex currently has a node instance, with these handles.

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
data EdgeStatus
  = EDown
    -- ^ The edge does not currently have mini protocol instances.
  | EUp
    -- ^ The edge currently has mini protocol instances.
  deriving (Eq)

type VertexStatusVar m blk = StrictTVar m (VertexStatus m blk)
type EdgeStatusVar m = StrictTVar m EdgeStatus

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
                    , Show (LedgerView (BlockProtocol blk))
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
  , tnaSlotLengths    = slotLengths
  , tnaTopology       = nodeTopology
  , tnaEpochSize      = epochSize
  } = withRegistry $ \sharedRegistry -> do
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
    sharedTestBtime <- newTestBlockchainTime sharedRegistry numSlots slotLengths
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
        v <- uncheckedNewTVarM (VDown Genesis)
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
                  VUp kernel _ -> do
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
          VDown ch -> pure (coreNodeId, readNodeInfo, ch)
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
          loop 0 (mkProtocolInfo coreNodeId) NodeRestart restarts0
      where
        restarts0 :: Map SlotNo NodeRestart
        restarts0 = Map.mapMaybe (Map.lookup coreNodeId) m
          where
            NodeRestarts m = nodeRestarts

        loop :: SlotNo -> ProtocolInfo blk -> NodeRestart -> Map SlotNo NodeRestart -> m ()
        loop s pInfo nr rs = do
          -- a registry solely for the resources of this specific node instance
          (again, finalChain) <- withRegistry $ \nodeRegistry -> do
            let nodeTestBtime = testBlockchainTimeClone sharedTestBtime nodeRegistry
                nodeBtime     = testBlockchainTime nodeTestBtime

            -- change the node's key and prepare a delegation transaction if
            -- the node is restarting because it just rekeyed
            (pInfo', txs0) <- case (nr, mbRekeyM) of
              (NodeRekey, Just rekeyM) -> do
                fmap maybeToList <$> rekeyM coreNodeId pInfo s (epochInfoEpoch epochInfo)
              _                        -> pure (pInfo, [])

            -- allocate the node's internal state and fork its internal threads
            -- (specifically not the communication threads running the Mini
            -- Protocols, like the ChainSync Client)
            (kernel, app) <- forkNode
              varRNG
              nodeBtime
              nodeRegistry
              pInfo'
              nodeInfo
              txs0
            atomically $ writeTVar vertexStatusVar $ VUp kernel app

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
              readTVar edgeStatusVar >>= check . (== EDown)

            -- assuming nothing else is changing it, read the final chain
            let chainDB = getChainDB kernel
            finalChain <- ChainDB.toChain chainDB

            pure (again, finalChain)   -- end of the node's withRegistry

          atomically $ writeTVar vertexStatusVar $ VDown finalChain

          case again of
            Nothing                     -> pure ()
            Just (s', pInfo', nr', rs') -> loop s' pInfo' nr' rs'

    -- | Persistently attempt to add the given transactions to the mempool
    -- every time the ledger slot changes, even if successful!
    --
    -- If we add the transaction and then the mempools discards it for some
    -- reason, this thread will add it again.
    --
    forkTxs0
      :: HasCallStack
      => ResourceRegistry m
      -> STM m (WithOrigin SlotNo)
      -- ^ How to get the slot of the current ledger state
      -> Mempool m blk TicketNo
      -> [GenTx blk]
         -- ^ valid transactions the node should immediately propagate
      -> m ()
    forkTxs0 registry get mempool txs0 =
      void $ forkLinkedThread registry $ do
        let loop mbSlot = do
              _ <- addTxs mempool txs0
              (mbSlot', _) <- atomically $ blockUntilChanged id mbSlot get
              loop mbSlot'
        loop Origin

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

    mkArgs :: BlockchainTime m
           -> ResourceRegistry m
           -> TopLevelConfig blk
           -> ExtLedgerState blk
           -> Tracer m (RealPoint blk, ExtValidationError blk)
              -- ^ invalid block tracer
           -> Tracer m (RealPoint blk, BlockNo)
              -- ^ added block tracer
           -> NodeDBs (StrictTVar m MockFS)
           -> ChainDbArgs m blk
    mkArgs
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
              (ChainDB.AddBlockValidation ChainDB.InvalidBlock
                  { _invalidPoint  = p
                  , _validationErr = e
                  })
              -> traceWith invalidTracer (p, e)
          ChainDB.TraceAddBlockEvent
              (ChainDB.AddedBlockToVolDB p bno IsNotEBB)
              -> traceWith addTracer (p, bno)
          _   -> pure ()

    forkNode
      :: HasCallStack
      => StrictTVar m ChaChaDRG
      -> BlockchainTime m
      -> ResourceRegistry m
      -> ProtocolInfo blk
      -> NodeInfo blk (StrictTVar m MockFS) (Tracer m)
      -> [GenTx blk]
         -- ^ valid transactions the node should immediately propagate
      -> m ( NodeKernel m NodeId blk
           , LimitedApp m NodeId blk
           )
    forkNode varRNG btime registry pInfo nodeInfo txs0 = do
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
      let chainDbArgs = mkArgs
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
            , maxClockSkew        = ClockSkew 1
            , cfg                 = pInfoConfig
            , initState           = pInfoInitState
            , btime
            , chainDB
            , initChainDB         = nodeInitChainDB
            , blockProduction     = Just blockProduction
            , blockFetchSize      = nodeBlockFetchSize
            , blockMatchesHeader  = nodeBlockMatchesHeader
            , maxUnackTxs         = 1000 -- TODO
            , maxBlockSize        = NoOverride
            , mempoolCap          = NoMempoolCapacityBytesOverride
            , chainSyncPipelining = pipelineDecisionLowHighMark 2 4
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
      -- is lost, a node that rekeyed can never lead again.
      --
      -- The thread might also have to block until enough of the chain is
      -- synced that the transaction is valid.
      --
      -- TODO Is there a risk that this will block because the 'forkTxProducer'
      -- fills up the mempool too quickly?
      forkTxs0
        registry
        ((ledgerTipSlot . ledgerState) <$> ChainDB.getCurrentLedger chainDB)
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
        v <- uncheckedNewTVarM EDown
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
    loop
  where
    loop = do
        restart <- directedEdgeInner edgeStatusVar client server
          `catch` (pure . RestartExn)
          `catch` hUnexpected
        atomically $ writeTVar edgeStatusVar EDown
        case restart of
          RestartNode  -> pure ()
          RestartExn e -> do
            -- "error policy": restart at beginning of next slot
            s <- atomically $ getCurrentSlot btime
            let s' = succ s
            traceWith tr (s, MiniProtocolDelayed, e)
            void $ blockUntilSlot btime s'
            traceWith tr (s', MiniProtocolRestarting, e)
        loop
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
  => EdgeStatusVar m
  -> (CoreNodeId, VertexStatusVar m blk)
     -- ^ client threads on this node
  -> (CoreNodeId, VertexStatusVar m blk)
     -- ^ server threads on this node
  -> m RestartCause
directedEdgeInner edgeStatusVar
  (node1, vertexStatusVar1) (node2, vertexStatusVar2) = do
    -- block until both nodes are 'VUp'
    (LimitedApp app1, LimitedApp app2) <- atomically $ do
      (,) <$> getApp vertexStatusVar1 <*> getApp vertexStatusVar2

    atomically $ writeTVar edgeStatusVar EUp

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
          -> m (m (), m ())
        miniProtocol client server = do
           (chan, dualChan) <- createConnectedChannels
           pure
             ( client app1 (fromCoreNodeId node2) chan
             , server app2 (fromCoreNodeId node1) dualChan
             )

    -- NB only 'watcher' ever returns in these tests
    fmap (\() -> RestartNode) $
      (>>= withAsyncsWaitAny) $
      fmap flattenPairs $
      sequence $
        pure (watcher vertexStatusVar1, watcher vertexStatusVar2)
        NE.:|
      [ miniProtocol
          (wrapMPEE MPEEChainSyncClient naChainSyncClient)
          naChainSyncServer
      , miniProtocol
          (wrapMPEE MPEEBlockFetchClient naBlockFetchClient)
          (wrapMPEE MPEEBlockFetchServer naBlockFetchServer)
      , miniProtocol
          (wrapMPEE MPEETxSubmissionClient naTxSubmissionClient)
          (wrapMPEE MPEETxSubmissionServer naTxSubmissionServer)
      ]
  where
    getApp v = readTVar v >>= \case
      VUp _ app -> pure app
      _         -> retry

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
  { nodeOutputAdds       :: Map SlotNo (Set (RealPoint blk, BlockNo))
  , nodeOutputFinalChain :: Chain blk
  , nodeOutputNodeDBs    :: NodeDBs MockFS
  , nodeOutputForges     :: Map SlotNo blk
  , nodeOutputInvalids   :: Map (RealPoint blk) [ExtValidationError blk]
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
        )]
    -> m (TestOutput blk)
mkTestOutput vertexInfos = do
    (nodeOutputs', tipBlockNos') <- fmap unzip $ forM vertexInfos $
      \(cid, readNodeInfo, ch) -> do
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
              { nodeOutputAdds       =
                  Map.fromListWith Set.union $
                  [ (s, Set.singleton (p, bno)) | (s, p, bno) <- nodeEventsAdds ]
              , nodeOutputFinalChain = ch
              , nodeOutputNodeDBs    = nodeInfoDBs
              , nodeOutputForges     =
                  Map.fromList $
                  [ (s, b) | TraceForgedBlock s _ b _ <- nodeEventsForges ]
              , nodeOutputInvalids   = (:[]) <$> Map.fromList nodeEventsInvalids
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
  deriving (Show)

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

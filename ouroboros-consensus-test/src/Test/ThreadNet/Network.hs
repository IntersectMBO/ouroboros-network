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
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Setup network
module Test.ThreadNet.Network (
    CalcMessageDelay (..)
  , ForgeEbbEnv (..)
  , RekeyM
  , TestNodeInitialization (..)
  , ThreadNetworkArgs (..)
  , TracingConstraints
  , noCalcMessageDelay
  , plainTestNodeInitialization
  , runThreadNetwork
    -- * Tracers
  , MiniProtocolFatalException (..)
  , MiniProtocolState (..)
    -- * Test Output
  , NodeDBs (..)
  , NodeOutput (..)
  , TestOutput (..)
  ) where

import           Codec.CBOR.Read (DeserialiseFailure)
import qualified Control.Exception as Exn
import           Control.Monad
import qualified Control.Monad.Class.MonadSTM as MonadSTM
import           Control.Monad.Class.MonadTime (MonadTime)
import           Control.Monad.Class.MonadTimer (MonadTimer)
import qualified Control.Monad.Except as Exc
import           Control.Tracer
import qualified Data.ByteString.Lazy as Lazy
import           Data.Either (isRight)
import           Data.Functor.Identity (Identity)
import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Typeable as Typeable
import           Data.Void (Void)
import           GHC.Stack
import           System.Random (mkStdGen)

import           Network.TypedProtocol.Codec (AnyMessage (..), CodecFailure,
                     mapFailureCodec)
import qualified Network.TypedProtocol.Codec as Codec

import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.BlockFetch (BlockFetchConfiguration (..),
                     TraceLabelPeer (..))
import           Ouroboros.Network.Channel
import           Ouroboros.Network.MockChain.Chain (Chain (Genesis))
import           Ouroboros.Network.Point (WithOrigin (..))
import qualified Ouroboros.Network.Protocol.ChainSync.Type as CS

import           Ouroboros.Network.Mux (ControlMessage (..), ControlMessageSTM)
import           Ouroboros.Network.NodeToNode (MiniProtocolParameters (..))
import           Ouroboros.Network.Protocol.KeepAlive.Type
import           Ouroboros.Network.Protocol.Limits (waitForever)
import           Ouroboros.Network.Protocol.TxSubmission.Type
import           Ouroboros.Network.Protocol.TxSubmission2.Type

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Config
import qualified Ouroboros.Consensus.Fragment.InFuture as InFuture
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Inspect
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Mempool
import qualified Ouroboros.Consensus.MiniProtocol.ChainSync.Client as CSClient
import qualified Ouroboros.Consensus.Network.NodeToNode as NTN
import           Ouroboros.Consensus.Node.InitStorage
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Node.Tracers
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.NodeKernel as NodeKernel
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util.Assert
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Consensus.Util.RedundantConstraints
import           Ouroboros.Consensus.Util.ResourceRegistry
import           Ouroboros.Consensus.Util.STM
import           Ouroboros.Consensus.Util.Time

import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import           Ouroboros.Consensus.Storage.ChainDB.Impl (ChainDbArgs (..))
import           Ouroboros.Consensus.Storage.FS.API (SomeHasFS (..))
import qualified Ouroboros.Consensus.Storage.ImmutableDB as ImmutableDB
import qualified Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index as Index
import qualified Ouroboros.Consensus.Storage.LedgerDB.DiskPolicy as LgrDB
import qualified Ouroboros.Consensus.Storage.VolatileDB as VolatileDB

import           Test.ThreadNet.TxGen
import           Test.ThreadNet.Util.NodeJoinPlan
import           Test.ThreadNet.Util.NodeRestarts
import           Test.ThreadNet.Util.NodeTopology
import           Test.ThreadNet.Util.Seed

import           Test.Util.FS.Sim.MockFS (MockFS)
import qualified Test.Util.FS.Sim.MockFS as Mock
import           Test.Util.FS.Sim.STM (simHasFS)
import           Test.Util.HardFork.Future (Future)
import qualified Test.Util.HardFork.Future as HFF
import           Test.Util.HardFork.OracularClock (OracularClock (..))
import qualified Test.Util.HardFork.OracularClock as OracularClock
import           Test.Util.Slots (NumSlots (..))
import           Test.Util.Time
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
instance Show (ForgeEbbEnv blk) where
  showsPrec p _ = showParen (p > 10) $ showString "ForgeEbbEnv _"

-- | How to rekey a node with a fresh operational key
--
-- When there is a 'NodeRekey' scheduled in the 'NodeRestarts', the test node
-- will restart and use 'tnaRekeyM' to compute its new 'ProtocolInfo'.
type RekeyM m blk =
     CoreNodeId
  -> ProtocolInfo m blk
  -> SlotNo
     -- ^ The slot in which the node is rekeying
  -> (SlotNo -> m EpochNo)
     -- ^ Which epoch the slot is in
  -> m (TestNodeInitialization m blk)
     -- ^ 'tniProtocolInfo' should include new delegation cert/operational key,
     -- and 'tniCrucialTxs' should include the new delegation certificate
     -- transaction

-- | Data used when starting/restarting a node
data TestNodeInitialization m blk = TestNodeInitialization
  { tniCrucialTxs   :: [GenTx blk]
    -- ^ these transactions are added immediately and repeatedly (whenever the
    -- 'ledgerTipSlot' changes)
    --
    -- In particular, a leading node's crucial transactions must (if valid)
    -- enter its mempool each slot /before/ the node takes the mempool snapshot
    -- that determines which transactions will be included in the block it's
    -- about to forge.
  , tniProtocolInfo :: ProtocolInfo m blk
  }

plainTestNodeInitialization
  :: ProtocolInfo m blk -> TestNodeInitialization m blk
plainTestNodeInitialization pInfo = TestNodeInitialization
    { tniCrucialTxs   = []
    , tniProtocolInfo = pInfo
    }

-- | Compute the chain diffusion delay
--
-- This is the number of slots a 'CS.MsgRollForward' should arrive after the
-- forge slot of the header it is carrying.
--
-- It may depend on the @(sender, recipient)@, the current slot, and header.
newtype CalcMessageDelay blk = CalcMessageDelay
    ((CoreNodeId, CoreNodeId) -> SlotNo -> Header blk -> NumSlots)

noCalcMessageDelay :: CalcMessageDelay blk
noCalcMessageDelay = CalcMessageDelay $ \_ _ _ -> NumSlots 0

-- | This type occurs in records where most of the fields are data
instance Show (CalcMessageDelay blk) where
  show _ = "_CalcMessageDelay"

-- | Parameters for the test node net
--
data ThreadNetworkArgs m blk = ThreadNetworkArgs
  { tnaForgeEbbEnv  :: Maybe (ForgeEbbEnv blk)
  , tnaFuture       :: Future
  , tnaJoinPlan     :: NodeJoinPlan
  , tnaNodeInfo     :: CoreNodeId -> TestNodeInitialization m blk
  , tnaNumCoreNodes :: NumCoreNodes
  , tnaNumSlots     :: NumSlots
  , tnaMessageDelay :: CalcMessageDelay blk
  , tnaSeed         :: Seed
  , tnaMkRekeyM     :: Maybe (m (RekeyM m blk))
  , tnaRestarts     :: NodeRestarts
  , tnaTopology     :: NodeTopology
  , tnaTxGenExtra   :: TxGenExtra blk
  , tnaVersion      :: NodeToNodeVersion
  , tnaBlockVersion :: BlockNodeToNodeVersion blk
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
  = VDown (Chain blk) (LedgerState blk)
    -- ^ The vertex does not currently have a node instance; its previous
    -- instance stopped with this chain and ledger state (empty/initial before
    -- first instance)
  | VFalling
    -- ^ The vertex has a node instance, but it is about to transition to
    -- 'VDown' as soon as its edges transition to 'EDown'.
  | VUp !(NodeKernel m NodeId Void blk) !(LimitedApp m NodeId blk)
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
                    , MonadTime m
                    , MonadTimer m
                    , RunNode blk
                    , TxGen blk
                    , TracingConstraints blk
                    , HasCallStack
                    )
                 => SystemTime m -> ThreadNetworkArgs m blk -> m (TestOutput blk)
runThreadNetwork systemTime ThreadNetworkArgs
  { tnaForgeEbbEnv    = mbForgeEbbEnv
  , tnaFuture         = future
  , tnaJoinPlan       = nodeJoinPlan
  , tnaNodeInfo       = mkProtocolInfo
  , tnaNumCoreNodes   = numCoreNodes
  , tnaNumSlots       = numSlots
  , tnaMessageDelay   = calcMessageDelay
  , tnaSeed           = seed
  , tnaMkRekeyM       = mbMkRekeyM
  , tnaRestarts       = nodeRestarts
  , tnaTopology       = nodeTopology
  , tnaTxGenExtra     = txGenExtra
  , tnaVersion        = version
  , tnaBlockVersion   = blockVersion
  } = withRegistry $ \sharedRegistry -> do
    mbRekeyM <- sequence mbMkRekeyM

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
    let clock = OracularClock.mkOracularClock systemTime numSlots future
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
      -- assume they all use the same CodecConfig
      let nodeInitData = mkProtocolInfo (CoreNodeId 0)
          TestNodeInitialization{tniProtocolInfo} = nodeInitData
          ProtocolInfo{pInfoConfig} = tniProtocolInfo
          codecConfig = configCodec pInfoConfig
      forM uedges $ \uedge -> do
        forkBothEdges
          sharedRegistry
          clock
          -- traces when/why the mini protocol instances start and stop
          nullDebugTracer
          (version, blockVersion)
          (codecConfig, calcMessageDelay)
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
      tooLate <- OracularClock.blockUntilSlot clock joinSlot
      when tooLate $ do
        error $ "unsatisfiable nodeJoinPlan: " ++ show coreNodeId

      -- fork the per-vertex state variables, including the mock filesystem
      (nodeInfo, readNodeInfo) <- newNodeInfo

      -- a tvar containing the next slot to be recorded in
      -- nodeEventsTipBlockNos
      nextInstrSlotVar <- uncheckedNewTVarM joinSlot

      let myEdgeStatusVars =
            [ v
            | ((n1, n2), v) <- Map.toList edgeStatusVars
            , coreNodeId `elem` [n1, n2]
            ]
      forkVertex
        mbRekeyM
        clock
        joinSlot
        sharedRegistry
        coreNodeId
        vertexStatusVar
        myEdgeStatusVars
        nodeInfo
        nextInstrSlotVar

      forkInstrumentation
        clock
        sharedRegistry
        vertexStatusVar
        nodeInfo
        nextInstrSlotVar

      return (coreNodeId, vertexStatusVar, readNodeInfo)

    -- Wait for the last slot to end
    OracularClock.waitUntilDone clock

    -- Collect all nodes' final chains
    vertexInfos <-
      atomically $
      forM vertexInfos0 $ \(coreNodeId, vertexStatusVar, readNodeInfo) -> do
        readTVar vertexStatusVar >>= \case
          VDown ch ldgr -> pure (coreNodeId, readNodeInfo, ch, ldgr)
          _             -> retry

    mkTestOutput vertexInfos
  where
    _ = keepRedundantConstraint (Proxy @(Show (LedgerView (BlockProtocol blk))))

    -- epoch size of the first era (ie the one that might have EBBs)
    epochSize0 :: EpochSize
    epochSize0 = HFF.futureFirstEpochSize future

    coreNodeIds :: [CoreNodeId]
    coreNodeIds = enumCoreNodes numCoreNodes

    joinSlotOf :: CoreNodeId -> SlotNo
    joinSlotOf = coreNodeIdJoinSlot nodeJoinPlan

    forkVertex
      :: Maybe (RekeyM m blk)
      -> OracularClock m
      -> SlotNo
      -> ResourceRegistry m
      -> CoreNodeId
      -> VertexStatusVar m blk
      -> [EdgeStatusVar m]
      -> NodeInfo blk (StrictTVar m MockFS) (Tracer m)
      -> StrictTVar m SlotNo
      -> m ()
    forkVertex
      mbRekeyM
      clock
      joinSlot
      sharedRegistry
      coreNodeId
      vertexStatusVar
      edgeStatusVars
      nodeInfo
      nextInstrSlotVar =
        void $ forkLinkedThread sharedRegistry label $ do
          loop 0 tniProtocolInfo NodeRestart restarts0
      where
        label = "vertex-" <> condense coreNodeId

        TestNodeInitialization
           { tniCrucialTxs
           , tniProtocolInfo
           } = mkProtocolInfo coreNodeId

        restarts0 :: Map SlotNo NodeRestart
        restarts0 = Map.mapMaybe (Map.lookup coreNodeId) m
          where
            NodeRestarts m = nodeRestarts

        loop :: SlotNo
             -> ProtocolInfo m blk
             -> NodeRestart
             -> Map SlotNo NodeRestart -> m ()
        loop s pInfo nr rs = do
          -- a registry solely for the resources of this specific node instance
          (again, finalChain, finalLdgr) <- withRegistry $ \nodeRegistry -> do
            -- change the node's key and prepare a delegation transaction if
            -- the node is restarting because it just rekeyed
            tni' <- case (nr, mbRekeyM) of
              (NodeRekey, Just rekeyM) -> do
                rekeyM coreNodeId pInfo s (pure . HFF.futureSlotToEpoch future)
              _                        ->
                  pure $ plainTestNodeInitialization pInfo
            let TestNodeInitialization
                  { tniCrucialTxs   = crucialTxs'
                  , tniProtocolInfo = pInfo'
                  } = tni'

            -- allocate the node's internal state and fork its internal threads
            -- (specifically not the communication threads running the Mini
            -- Protocols, like the ChainSync Client)
            (kernel, app) <- forkNode
              coreNodeId
              clock
              joinSlot
              nodeRegistry
              pInfo'
              nodeInfo
              (crucialTxs' ++ tniCrucialTxs)
            atomically $ writeTVar vertexStatusVar $ VUp kernel app

            -- wait until this node instance should stop
            again <- case Map.minViewWithKey rs of
              -- end of test
              Nothing               -> do
                OracularClock.waitUntilDone clock
                pure Nothing
              -- onset of schedule restart slot
              Just ((s', nr'), rs') -> do
                -- wait until the node should stop
                tooLate <- OracularClock.blockUntilSlot clock s'
                when tooLate $ do
                  error $ "unsatisfiable nodeRestarts: "
                    ++ show (coreNodeId, s')

                -- this synchronization prevents a race with the
                -- instrumentation thread: we it want to record the current
                -- block number at the start of the slot, before this vertex
                -- restarts the node
                atomically $ do
                  nextSlot <- readTVar nextInstrSlotVar
                  check $ nextSlot > s'

                pure $ Just (s', pInfo', nr', rs')

            -- stop threads that depend on/stimulate the kernel
            atomically $ writeTVar vertexStatusVar VFalling
            forM_ edgeStatusVars $ \edgeStatusVar -> atomically $ do
              readTVar edgeStatusVar >>= check . (== EDown)

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
            Just (s', pInfo', nr', rs') -> loop s' pInfo' nr' rs'

    -- | Instrumentation: record the tip's block number at the onset of the
    -- slot.
    --
    -- With such a short transaction (read a few TVars) we assume this runs (1)
    -- before anything else in the slot and (2) once per slot.
    forkInstrumentation
      :: OracularClock m
      -> ResourceRegistry m
      -> VertexStatusVar m blk
      -> NodeInfo blk (StrictTVar m MockFS) (Tracer m)
      -> StrictTVar m SlotNo
      -> m ()
    forkInstrumentation
      clock
      registry
      vertexStatusVar
      nodeInfo
      nextInstrSlotVar =
        void $ OracularClock.forkEachSlot registry clock lbl $ \s -> do
          bno <- atomically $ readTVar vertexStatusVar >>= \case
            VUp kernel _ -> ChainDB.getTipBlockNo (getChainDB kernel)
            _            -> retry
          traceWith nodeEventsTipBlockNos (s, bno)
          atomically $ modifyTVar nextInstrSlotVar $ max (succ s)
      where
        NodeInfo{nodeInfoEvents}          = nodeInfo
        NodeEvents{nodeEventsTipBlockNos} = nodeInfoEvents
        lbl                               = "instrumentation"

    -- | Persistently attempt to add the given transactions to the mempool
    -- every time the ledger slot changes, even if successful!
    --
    -- If we add the transaction and then the mempools discards it for some
    -- reason, this thread will add it again.
    --
    forkCrucialTxs
      :: HasCallStack
      => OracularClock m
      -> SlotNo
      -> ResourceRegistry m
      -> (SlotNo -> STM m ())
      -> LedgerConfig blk
      -> STM m (LedgerState blk)
      -> Mempool m blk TicketNo
      -> [GenTx blk]
         -- ^ valid transactions the node should immediately propagate
      -> m ()
    forkCrucialTxs clock s0 registry unblockForge lcfg getLdgr mempool txs0 =
      void $ forkLinkedThread registry "crucialTxs" $ do
        let wouldBeValid slot st tx =
                isRight $ Exc.runExcept $ applyTx lcfg DoNotIntervene slot tx st

            checkSt slot snap =
                any (wouldBeValid slot (snapshotLedgerState snap)) txs0

        let loop (slot, ledger, mempFp) = do
              (snap1, snap2) <- atomically $ do
                snap1 <- getSnapshotFor mempool $
                  -- This node would include these crucial txs if it leads in
                  -- this slot.
                  ForgeInKnownSlot slot $ applyChainTick lcfg slot ledger
                snap2 <- getSnapshotFor mempool $
                  -- Other nodes might include these crucial txs when leading
                  -- in the next slot.
                  ForgeInKnownSlot (succ slot) $ applyChainTick lcfg (succ slot) ledger
                -- This loop will repeat for the next slot, so we only need to
                -- check for this one and the next.
                pure (snap1, snap2)

              -- Don't attempt to add them if we're sure they'll be invalid.
              -- That just risks blocking on a full mempool unnecessarily.
              when (checkSt slot snap1 || checkSt (succ slot) snap2) $ do
                _ <- addTxs mempool txs0
                pure ()

              -- See 'unblockForge' in 'forkNode'
              atomically $ unblockForge slot

              let
                -- a clock tick might render a crucial transaction valid
                slotChanged = do
                  let slot' = succ slot
                  _ <- OracularClock.blockUntilSlot clock slot'
                  pure (slot', ledger, mempFp)

                -- a new tx (e.g. added by TxSubmission) might render a crucial
                -- transaction valid
                mempChanged = do
                  let getMemp = (map snd . snapshotTxs) <$> getSnapshot mempool
                  (mempFp', _) <- atomically $ blockUntilChanged id mempFp getMemp
                  pure (slot, ledger, mempFp')

                -- a new ledger state might render a crucial transaction valid
                ldgrChanged = do
                  let prj = ledgerTipPoint (Proxy @blk)
                  (ledger', _) <- atomically $ blockUntilChanged prj (prj ledger) getLdgr
                  pure (slot, ledger', mempFp)

              -- wake up when any of those change
              --
              -- key observation: it's OK to add the crucial txs too often
              fps' <- fmap (either (either id id) id) $
                slotChanged `race` mempChanged `race` ldgrChanged

              -- avoid the race in which we wake up before the mempool's
              -- background thread wakes up by mimicking it before we do
              -- anything else
              void $ syncWithLedger mempool

              loop fps'
        ledger0 <- atomically $ getLdgr
        loop (s0, ledger0, [])

    -- | Produce transactions every time the slot changes and submit them to
    -- the mempool.
    forkTxProducer :: HasCallStack
                   => CoreNodeId
                   -> ResourceRegistry m
                   -> OracularClock m
                   -> TopLevelConfig blk
                   -> Seed
                   -> STM m (ExtLedgerState blk)
                      -- ^ How to get the current ledger state
                   -> Mempool m blk TicketNo
                   -> m ()
    forkTxProducer coreNodeId registry clock cfg nodeSeed getExtLedger mempool =
      void $ OracularClock.forkEachSlot registry clock "txProducer" $ \curSlotNo -> do
        ledger <- atomically $ ledgerState <$> getExtLedger
        -- Combine the node's seed with the current slot number, to make sure
        -- we generate different transactions in each slot.
        let txs = runGen
                (nodeSeed `combineWith` unSlotNo curSlotNo)
                (testGenTxs coreNodeId numCoreNodes curSlotNo cfg txGenExtra ledger)

        void $ addTxs mempool txs

    mkArgs :: OracularClock m
           -> ResourceRegistry m
           -> TopLevelConfig blk
           -> ExtLedgerState blk
           -> Tracer m (RealPoint blk, ExtValidationError blk)
              -- ^ invalid block tracer
           -> Tracer m (RealPoint blk, BlockNo)
              -- ^ added block tracer
           -> Tracer m (RealPoint blk, BlockNo)
              -- ^ block selection tracer
           -> Tracer m (LedgerUpdate blk)
              -- ^ ledger updates tracer
           -> NodeDBs (StrictTVar m MockFS)
           -> CoreNodeId
           -> ChainDbArgs Identity m blk
    mkArgs
      clock registry
      cfg initLedger
      invalidTracer addTracer selTracer updatesTracer
      nodeDBs _coreNodeId = ChainDbArgs {
          -- HasFS instances
          cdbHasFSImmutableDB       = SomeHasFS $ simHasFS (nodeDBsImm nodeDBs)
        , cdbHasFSVolatileDB        = SomeHasFS $ simHasFS (nodeDBsVol nodeDBs)
        , cdbHasFSLgrDB             = SomeHasFS $ simHasFS (nodeDBsLgr nodeDBs)
          -- Policy
        , cdbImmutableDbValidation  = ImmutableDB.ValidateAllChunks
        , cdbVolatileDbValidation   = VolatileDB.ValidateAll
        , cdbMaxBlocksPerFile       = VolatileDB.mkBlocksPerFile 4
        , cdbDiskPolicy             = LgrDB.defaultDiskPolicy (configSecurityParam cfg) LgrDB.DefaultSnapshotInterval
          -- Integration
        , cdbTopLevelConfig         = cfg
        , cdbChunkInfo              = ImmutableDB.simpleChunkInfo epochSize0
        , cdbCheckIntegrity         = nodeCheckIntegrity (configStorage cfg)
        , cdbGenesis                = return initLedger
        , cdbCheckInFuture          = InFuture.reference (configLedger cfg) InFuture.defaultClockSkew
                                      (OracularClock.finiteSystemTime clock)
        , cdbImmutableDbCacheConfig = Index.CacheConfig 2 60
        -- Misc
        , cdbTracer                 = instrumentationTracer <> nullDebugTracer
        , cdbTraceLedger            = nullDebugTracer
        , cdbRegistry               = registry
          -- TODO vary these
        , cdbGcDelay                = 0
        , cdbGcInterval             = 1
        , cdbBlocksToAddSize        = 2
        }
      where
        prj af = case AF.headBlockNo af of
            At bno -> bno
            Origin -> error "selTracer"

        -- prop_general relies on this tracer
        instrumentationTracer = Tracer $ \case
          ChainDB.TraceAddBlockEvent
              (ChainDB.AddBlockValidation (ChainDB.InvalidBlock e p))
              -> traceWith invalidTracer (p, e)

          ChainDB.TraceAddBlockEvent
              (ChainDB.AddedBlockToVolatileDB p bno IsNotEBB)
              -> traceWith addTracer (p, bno)

          ChainDB.TraceAddBlockEvent
              (ChainDB.AddedToCurrentChain events p _old new)
              -> let (warnings, updates) = partitionLedgerEvents events in
                 assertWithMsg (noWarnings warnings) $ do
                   mapM_ (traceWith updatesTracer) updates
                   traceWith selTracer (ChainDB.newTipPoint p, prj new)
          ChainDB.TraceAddBlockEvent
              (ChainDB.SwitchedToAFork events p _old new)
              -> let (warnings, updates) = partitionLedgerEvents events in
                 assertWithMsg (noWarnings warnings) $ do
                   mapM_ (traceWith updatesTracer) updates
                   traceWith selTracer (ChainDB.newTipPoint p, prj new)

          _   -> pure ()

    -- We don't expect any ledger warnings
    -- (that would indicate node misconfiguration in the tests)
    noWarnings :: Show a => [a] -> Either String ()
    noWarnings [] = Right ()
    noWarnings ws = Left $ "Unexpected warnings: " ++ show ws

    -- Augment a tracer message with the node which produces it.
    _decorateId :: CoreNodeId -> Tracer m String -> Tracer m String
    _decorateId (CoreNodeId cid) = contramap $ \s ->
        show cid <> " | " <> s

    forkNode
      :: HasCallStack
      => CoreNodeId
      -> OracularClock m
      -> SlotNo
      -> ResourceRegistry m
      -> ProtocolInfo m blk
      -> NodeInfo blk (StrictTVar m MockFS) (Tracer m)
      -> [GenTx blk]
         -- ^ valid transactions the node should immediately propagate
      -> m ( NodeKernel m NodeId Void blk
           , LimitedApp m NodeId      blk
           )
    forkNode coreNodeId clock joinSlot registry pInfo nodeInfo txs0 = do
      let ProtocolInfo{..} = pInfo

      let NodeInfo
            { nodeInfoEvents
            , nodeInfoDBs
            } = nodeInfo

      -- prop_general relies on these tracers
      let invalidTracer = nodeEventsInvalids nodeInfoEvents
          updatesTracer = nodeEventsUpdates  nodeInfoEvents
          wrapTracer tr   = Tracer $ \(p, bno) -> do
            s <- OracularClock.getCurrentSlot clock
            traceWith tr (s, p, bno)
          addTracer       = wrapTracer $ nodeEventsAdds nodeInfoEvents
          selTracer       = wrapTracer $ nodeEventsSelects nodeInfoEvents
          headerAddTracer = wrapTracer $ nodeEventsHeaderAdds nodeInfoEvents
      let chainDbArgs = mkArgs
            clock registry
            pInfoConfig pInfoInitLedger
            invalidTracer
            addTracer
            selTracer
            updatesTracer
            nodeInfoDBs
            coreNodeId
      chainDB <- snd <$>
        allocate registry (const (ChainDB.openDB chainDbArgs)) ChainDB.closeDB

      let customForgeBlock ::
               BlockForging m blk
            -> TopLevelConfig blk
            -> BlockNo
            -> SlotNo
            -> TickedLedgerState blk
            -> [Validated (GenTx blk)]
            -> IsLeader (BlockProtocol blk)
            -> m blk
          customForgeBlock origBlockForging cfg' currentBno currentSlot tickedLdgSt txs prf = do
            let currentEpoch = HFF.futureSlotToEpoch future currentSlot

            -- EBBs are only ever possible in the first era
            let inFirstEra = HFF.futureEpochInFirstEra future currentEpoch

            let ebbSlot :: SlotNo
                ebbSlot = SlotNo $ x * y
                  where
                    EpochNo   x = currentEpoch
                    EpochSize y = epochSize0

            let p :: Point blk
                p = castPoint $ getTip tickedLdgSt

            let needEBB = inFirstEra && NotOrigin ebbSlot > pointSlot p
            case mbForgeEbbEnv <* guard needEBB of
              Nothing ->
                 -- no EBB needed, forge without making one
                 forgeBlock
                   origBlockForging
                   cfg'
                   currentBno
                   currentSlot
                   tickedLdgSt
                   txs
                   prf
              Just forgeEbbEnv -> do
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
                  -- if it is valid, we retick to the /same/ slot
                  let apply = applyLedgerBlock (configLedger pInfoConfig)
                  tickedLdgSt' <- case Exc.runExcept $ apply ebb tickedLdgSt of
                    Left e   -> Exn.throw $ JitEbbError @blk e
                    Right st -> pure $ applyChainTick
                                        (configLedger pInfoConfig)
                                        currentSlot
                                        st

                  -- forge the block usings the ledger state that includes
                  -- the EBB
                  blk <- forgeBlock
                           origBlockForging
                           cfg'
                           currentBno
                           currentSlot
                           tickedLdgSt'
                           txs
                           prf

                  -- If the EBB or the subsequent block is invalid, then the
                  -- ChainDB will reject it as invalid, and
                  -- 'Test.ThreadNet.General.prop_general' will eventually fail
                  -- because of a block rejection.
                  void $ ChainDB.addBlock chainDB ebb
                  pure blk

      origBlockForging <- pInfoBlockForging
      let blockForging =
            [ bf { forgeBlock = customForgeBlock bf }
            | bf <- origBlockForging
            ]

      -- This variable holds the number of the earliest slot in which the
      -- crucial txs have not yet been added. In other words, it holds the
      -- successor of the number of the latest slot in which the crucial txs
      -- have been added.
      --
      -- Key facts: The thread that adds the crucial transactions updates this
      -- variable, and the forge tracer for 'TraceNodeIsLeader' blocks on it.
      (unblockForge, blockOnCrucial) <- do
        var <- uncheckedNewTVarM 0
        pure
          ( \s -> do
              modifyTVar var (succ s `max`)
          , \s -> do
              sentinel <- readTVar var
              check $ s < sentinel
          )

      let -- prop_general relies on these tracers
          instrumentationTracers = nullTracers
                { chainSyncClientTracer = Tracer $ \case
                    TraceLabelPeer _ (CSClient.TraceDownloadedHeader hdr)
                      -> case blockPoint hdr of
                            GenesisPoint   -> pure ()
                            BlockPoint s h ->
                                -- TODO include tip in TraceDownloadedHeader
                                -- and only trace if hdr == tip?
                                traceWith headerAddTracer
                                  (RealPoint s h, blockNo hdr)
                    _ -> pure ()
                , forgeTracer           = Tracer $ \(TraceLabelCreds _ ev) -> do
                    traceWith (nodeEventsForges nodeInfoEvents) ev
                    case ev of
                      TraceNodeIsLeader s -> atomically $ blockOnCrucial s
                      _                   -> pure ()
                }

          -- traces the node's local events other than those from the -- ChainDB
          tracers = instrumentationTracers <> nullDebugTracers

      let -- use a backoff delay of exactly one slot length (which the
          -- 'OracularClock' always knows) for the following reasons
          --
          -- o It gives the node a chance to sync some blocks so that it will
          --   eventually not need to backoff
          --
          -- o It maintains the invariant that the node's activities all happen "
          --   during " a slot onset
          --
          -- o It avoids causing the node to miss a slot it could have
          --   nominally lead. EG If we used a backoff of two slot durations,
          --   then it might have synced during the first slot and then been
          --   able to productively lead the second slot had it not still been
          --   asleep.
          --
          -- o We assume a node will only backoff when it joins late and only
          --   until it syncs enough of the net's existing common prefix.
          hfbtBackoffDelay =
              BackoffDelay <$> OracularClock.delayUntilNextSlot clock
      btime <- hardForkBlockchainTime HardForkBlockchainTimeArgs
        { hfbtBackoffDelay
        , hfbtGetLedgerState =
            ledgerState <$> ChainDB.getCurrentLedger chainDB
        , hfbtLedgerConfig   = configLedger pInfoConfig
        , hfbtRegistry       = registry
        , hfbtSystemTime     = OracularClock.finiteSystemTime clock
        , hfbtTracer         =
            contramap
              -- We don't really have a SystemStart in the tests
              (fmap (fromRelativeTime (SystemStart dawnOfTime)))
              (blockchainTimeTracer tracers)
        , hfbtMaxClockRewind = secondsToNominalDiffTime 0
        }

      let kaRng = case seed of
                    Seed s -> mkStdGen s
      let nodeKernelArgs = NodeKernelArgs
            { tracers
            , registry
            , cfg                     = pInfoConfig
            , btime
            , chainDB
            , initChainDB             = nodeInitChainDB
            , blockForging            = blockForging
            , blockFetchSize          = estimateBlockSize
            , mempoolCapacityOverride = NoMempoolCapacityBytesOverride
            , keepAliveRng            = kaRng
            , miniProtocolParameters  = MiniProtocolParameters {
                  chainSyncPipeliningHighMark = 4,
                  chainSyncPipeliningLowMark  = 2,
                  blockFetchPipeliningMax     = 10,
                  txSubmissionMaxUnacked      = 1000 -- TODO ?
                }
            , blockFetchConfiguration = BlockFetchConfiguration {
                  bfcMaxConcurrencyBulkSync = 1
                , bfcMaxConcurrencyDeadline = 2
                , bfcMaxRequestsInflight    = 10
                , bfcDecisionLoopInterval   = 0.0 -- Mock testsuite can use sub-second slot
                                                  -- interval which doesn't play nice with
                                                  -- blockfetch descision interval.
                , bfcSalt                   = 0
                }
            }

      nodeKernel <- initNodeKernel nodeKernelArgs
      let mempool = getMempool nodeKernel
      let app = NTN.mkApps
                  nodeKernel
                  -- these tracers report every message sent/received by this
                  -- node
                  nullDebugProtocolTracers
                  (customNodeToNodeCodecs pInfoConfig)
                  -- see #1882, tests that can't cope with timeouts.
                  (pure $ NTN.ChainSyncTimeout
                     { canAwaitTimeout  = waitForever
                     , intersectTimeout = waitForever
                     , mustReplyTimeout = waitForever
                     })
                  (NTN.mkHandlers nodeKernelArgs nodeKernel)

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
        clock
        joinSlot
        registry
        unblockForge
        (configLedger pInfoConfig)
        (ledgerState <$> ChainDB.getCurrentLedger chainDB)
        mempool
        txs0

      forkTxProducer
        coreNodeId
        registry
        clock
        pInfoConfig
        -- Combine with the CoreNodeId, otherwise each node would generate the
        -- same transactions.
        (seed `combineWith` unCoreNodeId coreNodeId)
        -- Uses the same varRNG as the block producer, but we split the RNG
        -- each time, so this is fine.
        (ChainDB.getCurrentLedger chainDB)
        mempool

      return (nodeKernel, LimitedApp app)

    customNodeToNodeCodecs
      :: TopLevelConfig blk
      -> NodeToNodeVersion
      -> NTN.Codecs blk CodecError m
           Lazy.ByteString
           Lazy.ByteString
           Lazy.ByteString
           Lazy.ByteString
           (AnyMessage (TxSubmission (GenTxId blk) (GenTx blk)))
           (AnyMessage (TxSubmission2 (GenTxId blk) (GenTx blk)))
           (AnyMessage KeepAlive)
    customNodeToNodeCodecs cfg ntnVersion = NTN.Codecs
        { cChainSyncCodec =
            mapFailureCodec (CodecBytesFailure "ChainSync") $
              NTN.cChainSyncCodec binaryProtocolCodecs
        , cChainSyncCodecSerialised =
            mapFailureCodec (CodecBytesFailure "ChainSyncSerialised") $
              NTN.cChainSyncCodecSerialised binaryProtocolCodecs
        , cBlockFetchCodec =
            mapFailureCodec (CodecBytesFailure "BlockFetch") $
              NTN.cBlockFetchCodec binaryProtocolCodecs
        , cBlockFetchCodecSerialised =
            mapFailureCodec (CodecBytesFailure "BlockFetchSerialised") $
              NTN.cBlockFetchCodecSerialised binaryProtocolCodecs
        , cTxSubmissionCodec =
            mapFailureCodec CodecIdFailure $
              NTN.cTxSubmissionCodec NTN.identityCodecs
        , cTxSubmission2Codec =
            mapFailureCodec CodecIdFailure $
              NTN.cTxSubmission2Codec NTN.identityCodecs
        , cKeepAliveCodec =
            mapFailureCodec CodecIdFailure $
              NTN.cKeepAliveCodec NTN.identityCodecs
        }
      where
        binaryProtocolCodecs = NTN.defaultCodecs (configCodec cfg) blockVersion ntnVersion

-- | Sum of 'CodecFailure' (from @identityCodecs@) and 'DeserialiseFailure'
-- (from @defaultCodecs@).
data CodecError
  = CodecIdFailure CodecFailure
  | CodecBytesFailure
      String  -- ^ Extra error message, e.g., the name of the codec
      DeserialiseFailure
  deriving (Show, Exception)

{-------------------------------------------------------------------------------
  Running an edge
-------------------------------------------------------------------------------}

-- | Cause for an edge to restart
--
data RestartCause
  = RestartScheduled
    -- ^ restart because at least one of the two nodes set its status to
    -- 'VFalling' because of a scheduled restart in 'tnaRestarts'
  | RestartChainSyncTerminated
    -- ^ restart because the ChainSync client terminated the mini protocol

-- | Fork two directed edges, one in each direction between the two vertices
--
forkBothEdges
  :: (IOLike m, RunNode blk, HasCallStack)
  => ResourceRegistry m
  -> OracularClock m
  -> Tracer m (SlotNo, MiniProtocolState)
  -> (NodeToNodeVersion, BlockNodeToNodeVersion blk)
  -> (CodecConfig blk, CalcMessageDelay blk)
  -> Map CoreNodeId (VertexStatusVar m blk)
  -> (CoreNodeId, CoreNodeId)
  -> m [((CoreNodeId, CoreNodeId), EdgeStatusVar m)]
forkBothEdges sharedRegistry clock tr version cfg vertexStatusVars (node1, node2) = do
  let endpoint1 = mkEndpoint node1
      endpoint2 = mkEndpoint node2
      mkEndpoint node = case Map.lookup node vertexStatusVars of
          Nothing  -> error $ "node not found: " ++ show node
          Just var -> (node, var)

  let mkDirEdge e1 e2 = do
        v <- uncheckedNewTVarM EDown
        let label = concat
              ["directed-edge-", condense (fst e1), "-", condense (fst e2)]
        void $ forkLinkedThread sharedRegistry label $ do
          directedEdge sharedRegistry tr version cfg clock v e1 e2
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
  forall m blk. (IOLike m, RunNode blk)
  => ResourceRegistry m
  -> Tracer m (SlotNo, MiniProtocolState)
  -> (NodeToNodeVersion, BlockNodeToNodeVersion blk)
  -> (CodecConfig blk, CalcMessageDelay blk)
  -> OracularClock m
  -> EdgeStatusVar m
  -> (CoreNodeId, VertexStatusVar m blk)
  -> (CoreNodeId, VertexStatusVar m blk)
  -> m ()
directedEdge registry tr version cfg clock edgeStatusVar client server =
    loop
  where
    loop = do
        restart <- directedEdgeInner registry clock version cfg edgeStatusVar client server
          `catch` hUnexpected
        atomically $ writeTVar edgeStatusVar EDown
        case restart of
          RestartScheduled            -> pure ()
          RestartChainSyncTerminated  -> do
            -- "error" policy: restart at beginning of next slot
            s <- OracularClock.getCurrentSlot clock
            let s' = succ s
            traceWith tr (s, MiniProtocolDelayed)
            void $ OracularClock.blockUntilSlot clock s'
            traceWith tr (s', MiniProtocolRestarting)
        loop
      where
        -- Wrap synchronous exceptions in 'MiniProtocolFatalException'
        --
        hUnexpected :: forall a. SomeException -> m a
        hUnexpected e@(Exn.SomeException e') = case fromException e of
          Just (_ :: Exn.AsyncException) -> throwIO e
          Nothing                        -> case fromException e of
            Just (_ :: Exn.SomeAsyncException) -> throwIO e
            Nothing                            -> throwIO MiniProtocolFatalException
              { mpfeType   = Typeable.typeOf e'
              , mpfeExn    = e
              , mpfeClient = fst client
              , mpfeServer = fst server
              }

-- | Spawn threads for all of the mini protocols
--
-- See 'directedEdge'.
directedEdgeInner ::
  forall m blk. (IOLike m, RunNode blk)
  => ResourceRegistry m
  -> OracularClock m
  -> (NodeToNodeVersion, BlockNodeToNodeVersion blk)
  -> (CodecConfig blk, CalcMessageDelay blk)
  -> EdgeStatusVar m
  -> (CoreNodeId, VertexStatusVar m blk)
     -- ^ client threads on this node
  -> (CoreNodeId, VertexStatusVar m blk)
     -- ^ server threads on this node
  -> m RestartCause
directedEdgeInner registry clock (version, blockVersion) (cfg, calcMessageDelay) edgeStatusVar
  (node1, vertexStatusVar1) (node2, vertexStatusVar2) = do
    -- block until both nodes are 'VUp'
    (LimitedApp app1, LimitedApp app2) <- atomically $ do
      (,) <$> getApp vertexStatusVar1 <*> getApp vertexStatusVar2

    atomically $ writeTVar edgeStatusVar EUp

    let miniProtocol ::
             String
             -- ^ protocol name
          -> (String -> a -> RestartCause)
          -> (  LimitedApp' m NodeId blk
             -> NodeToNodeVersion
             -> ControlMessageSTM m
             -> NodeId
             -> Channel m msg
             -> m (a, trailingBytes)
             )
            -- ^ client action to run on node1
          -> (  LimitedApp' m NodeId blk
             -> NodeToNodeVersion
             -> NodeId
             -> Channel m msg
             -> m (a, trailingBytes)
             )
             -- ^ server action to run on node2
          -> (msg -> m ())
          -> m (m RestartCause, m RestartCause)
        miniProtocol proto ret client server middle = do
           (chan, dualChan) <-
             createConnectedChannelsWithDelay registry (node1, node2, proto) middle
           pure
             ( (ret (proto <> ".client") . fst) <$> client app1 version (return Continue) (fromCoreNodeId node2) chan
             , (ret (proto <> ".server") . fst) <$> server app2 version (fromCoreNodeId node1) dualChan
             )

    (>>= withAsyncsWaitAny) $
      fmap flattenPairs $
      sequence $
        pure (watcher vertexStatusVar1, watcher vertexStatusVar2)
        NE.:|
      [ miniProtocol "ChainSync"
          (\_s () -> RestartChainSyncTerminated)
          NTN.aChainSyncClient
          NTN.aChainSyncServer
          chainSyncMiddle
      , miniProtocol "BlockFetch"
          neverReturns
          NTN.aBlockFetchClient
          NTN.aBlockFetchServer
          (\_ -> pure ())
      , miniProtocol "TxSubmission"
          neverReturns
          NTN.aTxSubmissionClient
          NTN.aTxSubmissionServer
          (\_ -> pure ())
      , miniProtocol "KeepAlive"
          neverReturns
          NTN.aKeepAliveClient
          NTN.aKeepAliveServer
          (\_ -> pure ())
      ]
  where
    getApp v = readTVar v >>= \case
      VUp _ app -> pure app
      _         -> retry

    flattenPairs :: forall a. NE.NonEmpty (a, a) -> NE.NonEmpty a
    flattenPairs = uncurry (<>) . NE.unzip

    neverReturns :: String -> () -> void
    neverReturns s () = error $ s <> " never returns!"

    -- terminates (by returning, not via exception) when the vertex starts
    -- 'VFalling'
    --
    -- because of 'withAsyncsWaitAny' used above, this brings down the whole
    -- edge
    watcher :: VertexStatusVar m blk -> m RestartCause
    watcher v = do
        atomically $ readTVar v >>= \case
          VFalling -> pure RestartScheduled
          _        -> retry

    -- introduce a delay for 'CS.MsgRollForward'
    --
    -- It is reasonable to delay only this message because this message is the
    -- first step in process of one node diffusing a block to another node.
    chainSyncMiddle :: Lazy.ByteString -> m ()
    chainSyncMiddle bs = do
        let tok = Codec.ServerAgency $ CS.TokNext CS.TokMustReply
        decodeStep <- Codec.decode codec tok
        Codec.runDecoder [bs] decodeStep >>= \case
          Right (Codec.SomeMessage (CS.MsgRollForward hdr _tip)) -> do
              s <- OracularClock.getCurrentSlot clock
              let NumSlots d = f (node1, node2) s hdr
                    where
                      CalcMessageDelay f = calcMessageDelay
              void $ OracularClock.blockUntilSlot clock $ blockSlot hdr + SlotNo d
          _ -> pure ()
      where
        codec =
            NTN.cChainSyncCodec $ NTN.defaultCodecs cfg blockVersion version

-- | Variant of 'createConnectChannels' with intermediate queues for
-- delayed-but-in-order messages
--
-- Sending adds the message to a queue. The delaying argument should use
-- 'threadDelay' in order to delay the transfer of the given message from the
-- queue to the recipient.
createConnectedChannelsWithDelay
  :: IOLike m
  => ResourceRegistry m
  -> (CoreNodeId, CoreNodeId, String)
     -- ^ (client, server, protocol)
  -> (a -> m ())
     -- ^ per-message delay
  -> m (Channel m a, Channel m a)
createConnectedChannelsWithDelay registry (client, server, proto) middle = do
    -- queue for async send and an mvar for delayed-but-in-order reads from the
    -- queue
    qA <- atomically $ MonadSTM.newTQueue
    bA <- atomically $ MonadSTM.newEmptyTMVar
    spawn (client, server) qA bA

    qB <- atomically $ MonadSTM.newTQueue
    bB <- atomically $ MonadSTM.newEmptyTMVar
    spawn (server, client) qB bB

    return (chan qA bB, chan qB bA)   -- note the crossover
  where
    spawn (cid1, cid2) q b = do
        let label =
                "delaying thread for " <> proto <> " " <>
                show cid1 <> " to " <> show cid2
        void $ forkLinkedThread registry label $ forever $ do
          x <- atomically $ MonadSTM.readTQueue q
          middle x
          atomically $ MonadSTM.putTMVar b x

    chan q b = Channel
        { recv = fmap Just $ atomically $ MonadSTM.takeTMVar b
        , send = atomically . MonadSTM.writeTQueue q
        }

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
    -- ^ every 'AddedBlockToVolatileDB' excluding EBBs
  , nodeEventsForges      :: ev (TraceForgeEvent blk)
    -- ^ every 'TraceForgeEvent'
  , nodeEventsHeaderAdds  :: ev (SlotNo, RealPoint blk, BlockNo)
    -- ^ every 'TraceDownloadedHeader', excluding EBBs
  , nodeEventsInvalids    :: ev (RealPoint blk, ExtValidationError blk)
    -- ^ the point of every 'ChainDB.InvalidBlock' event
  , nodeEventsSelects     :: ev (SlotNo, RealPoint blk, BlockNo)
    -- ^ every 'ChainDB.AddedToCurrentChain' and 'ChainDB.SwitchedToAFork'
  , nodeEventsTipBlockNos :: ev (SlotNo, WithOrigin BlockNo)
    -- ^ 'ChainDB.getTipBlockNo' for each node at the onset of each slot
  , nodeEventsUpdates     :: ev (LedgerUpdate blk)
    -- ^ Ledger updates every time we adopt a block/switch to a fork
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
      (t5, m5) <- recordingTracerTVar
      (t6, m6) <- recordingTracerTVar
      (t7, m7) <- recordingTracerTVar
      pure
          ( NodeEvents     t1     t2     t3     t4     t5     t6     t7
          , NodeEvents <$> m1 <*> m2 <*> m3 <*> m4 <*> m5 <*> m6 <*> m7
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
  { nodeOutputAdds         :: Map SlotNo (Set (RealPoint blk, BlockNo))
  , nodeOutputCannotForges :: Map SlotNo [CannotForge blk]
  , nodeOutputFinalChain   :: Chain blk
  , nodeOutputFinalLedger  :: LedgerState blk
  , nodeOutputForges       :: Map SlotNo blk
  , nodeOutputHeaderAdds   :: Map SlotNo [(RealPoint blk, BlockNo)]
  , nodeOutputInvalids     :: Map (RealPoint blk) [ExtValidationError blk]
  , nodeOutputNodeDBs      :: NodeDBs MockFS
  , nodeOutputSelects      :: Map SlotNo [(RealPoint blk, BlockNo)]
  , nodeOutputUpdates      :: [LedgerUpdate blk]
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
              , nodeEventsHeaderAdds
              , nodeEventsInvalids
              , nodeEventsSelects
              , nodeEventsTipBlockNos
              , nodeEventsUpdates
              } = nodeInfoEvents
        let nodeOutput = NodeOutput
              { nodeOutputAdds        =
                  Map.fromListWith Set.union $
                  [ (s, Set.singleton (p, bno)) | (s, p, bno) <- nodeEventsAdds ]
              , nodeOutputCannotForges =
                  Map.fromListWith (flip (++)) $
                  [ (s, [err]) | TraceNodeCannotForge s err <- nodeEventsForges ]
              , nodeOutputFinalChain  = ch
              , nodeOutputFinalLedger = ldgr
              , nodeOutputForges      =
                  Map.fromList $
                  [ (s, b) | TraceForgedBlock s _ b _ <- nodeEventsForges ]
              , nodeOutputHeaderAdds  =
                  Map.fromListWith (flip (++)) $
                  [ (s, [(p, bno)])
                  | (s, p, bno) <- nodeEventsHeaderAdds
                  ]
              , nodeOutputSelects     =
                  Map.fromListWith (flip (++)) $
                  [ (s, [(p, bno)])
                  | (s, p, bno) <- nodeEventsSelects
                  ]
              , nodeOutputInvalids    = (:[]) <$> Map.fromList nodeEventsInvalids
              , nodeOutputNodeDBs     = nodeInfoDBs
              , nodeOutputUpdates     = nodeEventsUpdates
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
  => Tracers m peer Void blk
nullDebugTracers = nullTracers `asTypeOf` showTracers debugTracer

-- | Occurs throughout in positions that might be useful for debugging.
nullDebugProtocolTracers ::
     ( Monad m
     , HasHeader blk
     , TracingConstraints blk
     , Show peer
     )
  => NTN.Tracers m peer blk failure
nullDebugProtocolTracers =
  NTN.nullTracers `asTypeOf` NTN.showTracers debugTracer

-- These constraints are when using @showTracer(s) debugTracer@ instead of
-- @nullTracer(s)@.
type TracingConstraints blk =
  ( Show blk
  , Show (ApplyTxErr blk)
  , Show (Header blk)
  , Show (GenTx blk)
  , Show (Validated (GenTx blk))
  , Show (GenTxId blk)
  , Show (ForgeStateInfo blk)
  , Show (ForgeStateUpdateError blk)
  , Show (CannotForge blk)
  , HasNestedContent Header blk
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
   LimitedApp (LimitedApp' m peer blk)

-- | Argument of 'LimitedApp' data constructor
--
-- Used internal to this module, essentially as an abbreviation.
type LimitedApp' m peer blk =
    NTN.Apps m peer
        -- The 'ChainSync' and 'BlockFetch' protocols use @'Serialised' x@ for
        -- the servers and @x@ for the clients. Since both have to match to be
        -- sent across a channel, we can't use @'AnyMessage' ..@, instead, we
        -- (de)serialise the messages so that they can be sent across the
        -- channel with the same type on both ends, i.e., 'Lazy.ByteString'.
        Lazy.ByteString
        Lazy.ByteString
        (AnyMessage (TxSubmission  (GenTxId blk) (GenTx blk)))
        (AnyMessage (TxSubmission2 (GenTxId blk) (GenTx blk)))
        (AnyMessage KeepAlive)
        ()

{-------------------------------------------------------------------------------
  Tracing
-------------------------------------------------------------------------------}

data MiniProtocolState = MiniProtocolDelayed | MiniProtocolRestarting
  deriving (Show)

-- | Any synchronous exception from a 'directedEdge'
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
  = JitEbbError (LedgerError blk)
    -- ^ we were unable to extend the ledger state with the JIT EBB

deriving instance LedgerSupportsProtocol blk => Show (JitEbbError blk)
instance LedgerSupportsProtocol blk => Exception (JitEbbError blk)

-- | The 'TxGen' generator consecutively failed too many times
data TxGenFailure = TxGenFailure Int   -- ^ how many times it failed
  deriving (Show)

instance Exception TxGenFailure

{-# LANGUAGE DeriveFoldable       #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

module Scratch.ThreadNet.MockNode (
  -- * A vertex in the node topology
  VertexArguments (..),
  runVertex,
  -- * Tracers
  ThreadNetEvent (..),
  VertexTracers (..),
  nullVertexTracers,
  -- * Abbreviations
  CommandVar,
  InputVars,
  -- * Incidentals
  NodeDBs (..),
  VertexCommand (..),
  ) where

import           Control.Monad (forM_, forever)
import           Control.Tracer (Tracer (..), nullTracer)
import qualified Data.Map as Map
import           Data.Proxy (Proxy (..))

import           Control.Monad.Class.MonadSTM (atomically)
import           Control.Monad.Class.MonadSTM.Strict (StrictTMVar, takeTMVar)
import           Control.Monad.Class.MonadTime (MonadTime)
import           Control.Monad.Class.MonadTimer (MonadTimer)

import           Cardano.Slotting.Slot (EpochSize)

import qualified Ouroboros.Network.BlockFetch as BF
import qualified Ouroboros.Network.Codec as Codec
import qualified Ouroboros.Network.Diffusion as Diffusion
import qualified Ouroboros.Network.NodeToNode as NTN
import qualified Ouroboros.Network.Mux as Mux
import           Ouroboros.Network.Protocol.Handshake.Unversioned
import           Ouroboros.Network.Protocol.Limits (waitForever)

import           Ouroboros.Consensus.BlockchainTime (RelativeTime)
import qualified Ouroboros.Consensus.BlockchainTime as BlockchainTime
import qualified Ouroboros.Consensus.Fragment.InFuture as InFuture
import qualified Ouroboros.Consensus.Network.NodeToClient as CNTC
import qualified Ouroboros.Consensus.Network.NodeToNode as CNTN
import qualified Ouroboros.Consensus.Node as Real
import qualified Ouroboros.Consensus.Node.NetworkProtocolVersion as Real
import qualified Ouroboros.Consensus.Node.Tracers as Real
import           Ouroboros.Consensus.NodeId (CoreNodeId (..))
import           Ouroboros.Consensus.Storage.ChainDB (ChainDbArgs (..))
import           Ouroboros.Consensus.Storage.FS.API
import qualified Ouroboros.Consensus.Storage.ImmutableDB as ImmutableDB
import qualified Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index as Index
import qualified Ouroboros.Consensus.Storage.LedgerDB.OnDisk as LedgerDB.OnDisk
import qualified Ouroboros.Consensus.Storage.VolatileDB as VolatileDB
import           Ouroboros.Consensus.Util.Args (Defaults (..))
import           Ouroboros.Consensus.Util.IOLike (IOLike)
import qualified Ouroboros.Consensus.Util.IOLike as IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry (ResourceRegistry)

import qualified Scratch.ThreadNet.MockNetwork as MockNetwork
import           Scratch.ThreadNet.Types

data VertexArguments m blk = VertexArguments {
    coreNodeId     :: CoreNodeId
  , epochSizeImmDb :: EpochSize
  , nodeDBs        :: NodeDBs (SomeHasFS m)
  , nodeUpData     :: NodeUpData
  , protocolInfo   :: Real.ProtocolInfo m blk
  }

-- | Fork a vertex in the node topology.
--
-- Over the course of the simulation, distinct node instances may inhabit this
-- vertex, one at a time. Each will re-use the same persisent store (ie file
-- system). Simple example: the vertex is a specific computer on which a node
-- operator runs their software; sometimes the operator reboots the computer,
-- adjusts the node configuration, changes a private key, etc.
--
-- TODO Right now this code only ever spawns one node instance, so the vertex
-- and node are currently conflated. In particular, when a new node instance
-- replaces the previous one, it should immediately re-connect to other nodes
-- in the network.
runVertex :: forall m blk.
     ( IOLike m
     , MonadTime m
     , MonadTimer m
     , Real.RunNode blk
     )
  => VertexTracers m blk
  -> VertexArguments m blk
  -> InputVars m
  -> m ()
runVertex vertexTracers vertexArguments inputVars = do
    Real.runWith Real.RunNodeArgs {

      -- Tracers

        Real.rnTraceConsensus = tracersConsensus
      , Real.rnTraceNTN       = tracersNTN
      , Real.rnTraceNTC       = CNTC.nullTracers  -- TODO

      -- Node kernel

      , Real.rnProtocolInfo   = protocolInfo
      , Real.rnNodeKernelHook =
          \_reg _nodeKernel -> pure ()

      }
      Real.LowLevelRunNodeArgs {

      -- ChainDB

        Real.llrnWithCheckedDB =
          \k -> k (Real.LastShutDownWasClean True)   -- empty FSs are clean

      , Real.llrnChainDbArgsDefaults = ChainDbArgs
          -- the layout of this record expression matches the layout of the
          -- ChainDbArgs definition
          { cdbHasFSImmutableDB       = nodeDBsImmutable
          , cdbHasFSVolatileDB        = nodeDBsVolatile
          , cdbHasFSLgrDB             = nodeDBsLedger

          , cdbImmutableDbValidation  = ImmutableDB.ValidateAllChunks
              -- vs defaultArgs's ValidateMostRecentChunk
          , cdbVolatileDbValidation   = VolatileDB.ValidateAll
              -- vs defaultArgs's NoValidation
          , cdbMaxBlocksPerFile       = VolatileDB.mkBlocksPerFile 4
              -- vs defaultArgs's 1000
          , cdbDiskPolicy             = NoDefault

          , cdbTopLevelConfig         = NoDefault
          , cdbChunkInfo              = NoDefault
          , cdbCheckIntegrity         = NoDefault
          , cdbGenesis                = NoDefault
          , cdbCheckInFuture          = NoDefault
          , cdbImmutableDbCacheConfig = Index.CacheConfig 2 60
              -- vs defaultArgs's 250 and 300

          , cdbTracer                 = tracerChainDB
          , cdbTraceLedger            = tracerLedgerDB
          , cdbRegistry               = NoDefault
          , cdbGcDelay                = 0
              -- vs defaultArgs's 60 seconds
          , cdbGcInterval             = 1
              -- vs defaultArgs's 10 seconds
          , cdbBlocksToAddSize        = 2
              -- vs defaultArgs's 10
          }

      , Real.llrnMaxClockSkew = InFuture.defaultClockSkew

      , Real.llrnCustomiseChainDbArgs = \x -> x
          { cdbChunkInfo = ImmutableDB.simpleChunkInfo epochSizeImmDb
              -- vs nodeImmutableDbChunkInfo cfg in Ouroboros.Consensus.Node.run
              --
              -- TODO still necessary?
          }
      , Real.llrnCustomiseNodeKernelArgs = \x -> x
          { Real.blockFetchConfiguration = (Real.blockFetchConfiguration x)
              { BF.bfcMaxConcurrencyBulkSync = 1
              , BF.bfcMaxConcurrencyDeadline = 2
              , BF.bfcMaxRequestsInflight    = 10
              , BF.bfcDecisionLoopInterval   = 0.0
                  -- Mock testsuite can use sub-second slot interval which
                  -- doesn't play nice with blockfetch descision interval.
              }
              -- vs sync     = 1
              --    deadline = 1
              --    inflight = blockFetchPipeliningMax defaultMiniProtocolParameters
              --    interval = 0.01
          , Real.miniProtocolParameters  = NTN.MiniProtocolParameters {
                NTN.chainSyncPipeliningHighMark = 4,
                NTN.chainSyncPipeliningLowMark  = 2,
                NTN.blockFetchPipeliningMax     = 10,
                NTN.txSubmissionMaxUnacked      = 1000 -- TODO ?
              }
            -- vs low = 200, high = 300, max = 100, maxUnacked = 10
          }

      -- BlockchainTime

         -- TODO remove finiteSystemTime from OracularClock; the rewrite no
         -- longer needs it

         -- TODO remove OracularClock from the node; the node no longer needs
         -- it

      , Real.llrnCustomiseHardForkBlockchainTimeArgs = \x -> x
          { Real.hfbtBackoffDelay =
              pure $ error "synched nodes never backoff"
          , Real.hfbtTracer       = tracerBtime
          }

      -- Node kernel

      , Real.llrnBfcSalt      = 0   -- TODO OK?
      , Real.llrnKeepAliveRng = keepAliveRng

      , Real.llrnNodeToNodeVersions   =
          Real.supportedNodeToNodeVersions (Proxy :: Proxy blk)
      , Real.llrnNodeToClientVersions = Map.empty

      -- Diffusion

      , Real.llrnChainSyncTimeout =
          -- see #1882, tests that can't cope with timeouts.
          --
          -- Specifically, we want the ThreadNet tests to explicitly control
          -- when nodes connect and disconnect.
          --
          -- TODO Low priority, perhaps not worthwhile: use proper timeouts and
          -- intentionally inject delays that exceed them.
          pure CNTN.ChainSyncTimeout
            { CNTN.canAwaitTimeout  = waitForever
            , CNTN.intersectTimeout = waitForever
            , CNTN.mustReplyTimeout = waitForever
            }

      , Real.llrnVersionDataNTC = UnversionedProtocolData
      , Real.llrnVersionDataNTN = UnversionedProtocolData

      , Real.llrnRunDataDiffusion = \registry apps -> do
          mockDataDiffusion
            tracerHarness
            (NodeId coreNodeId 0)   -- TODO
            inputVars
            registry
            apps

      }
  where
    VertexTracers {
        tracerBtime
      , tracerChainDB
      , tracerHarness
      , tracerLedgerDB
      , tracersConsensus
      , tracersNTN
      } = vertexTracers

    VertexArguments {
        coreNodeId
      , epochSizeImmDb
      , nodeDBs
      , nodeUpData
      , protocolInfo
      } = vertexArguments

    NodeDBs {
        nodeDBsImmutable
      , nodeDBsVolatile
      , nodeDBsLedger
      } = nodeDBs

    NodeUpData {
        keepAliveRng
      } = nodeUpData

-- | Run the mock data diffusion layer
--
-- NOTE This never returns.
mockDataDiffusion :: forall m blk.
     IOLike m
  => Tracer m (ThreadNetEvent blk)
  -> NodeId
  -> InputVars m
  -> ResourceRegistry m
  -> Diffusion.DiffusionApplications
       PeerId PeerId
       UnversionedProtocolData UnversionedProtocolData
       m
  -> m ()
mockDataDiffusion tracerHarness nodeId inputVars registry apps = do
    -- state tracking for each client, so that we can disconnect from a
    -- particular peer when commanded to do so
    controlTerminatorsCS <- IOLike.uncheckedNewTVarM Map.empty

    -- receive each command from the test plan/infrastructure
    receptionist vertexCommandVar $ \case
      DisconnectFrom them
        -> atomically $ do
          vars <- IOLike.readTVar controlTerminatorsCS
          forM_ (Map.lookup them vars) $ \terminator -> do
            terminator
            IOLike.writeTVar controlTerminatorsCS $ Map.delete them vars
      InitiateTo handshakeCmd
        -> do
        (controlMessageSTM, terminator) <- do
          controlVarCS <- IOLike.uncheckedNewTVarM Mux.Continue
          pure ( IOLike.readTVar  controlVarCS
               , IOLike.writeTVar controlVarCS Mux.Terminate
               )
        let _ = controlMessageSTM :: IOLike.STM m Mux.ControlMessage
            _ = terminator        :: IOLike.STM m ()

        atomically $ IOLike.modifyTVar controlTerminatorsCS $ do
          let (them, _nodeToNodeVersion, _clientChans) = handshakeCmd
          Map.insert them terminator

        -- instigate the client threads
        wrapFun (MockNetwork.initiate controlMessageSTM) $ handshakeCmd
      RespondTo handshakeCmd
        -> do
        -- instigate the server threads
        wrapFun MockNetwork.respond $ handshakeCmd
  where
    vertexCommandVar = inputVars

    receptionist :: forall msg.
         StrictTMVar m msg
      -> (msg -> m ())
      -> m ()
    receptionist var k =
        forever $ do
            msg <- atomically (takeTMVar var)
            k msg

    -- omitting noisy type
    wrapFun f msg =
        f
          tracerHarness
          (\(NodeId coreNodeId _k) -> coreNodeId)
          nodeId
          registry
          apps
          msg

{-------------------------------------------------------------------------------
  Tracers
-------------------------------------------------------------------------------}

data VertexTracers m blk = VertexTracers {
    tracerBtime    :: Tracer m (BlockchainTime.TraceBlockchainTimeEvent RelativeTime)
  , tracerChainDB  :: Tracer m (Real.TraceEvent blk)
  , tracerHarness  :: Tracer m (ThreadNetEvent blk)
    -- ^ For humans to observe the test harness's events.
  , tracerLedgerDB :: Tracer m (LedgerDB.OnDisk.LedgerDB' blk)
  , tracersConsensus    :: Real.Tracers m (Real.ConnectionId PeerId) (Real.ConnectionId PeerId) blk
  , tracersNTN     :: CNTN.Tracers m (Real.ConnectionId PeerId) blk Codec.DeserialiseFailure
  }

instance (Monad m, Monad (IOLike.STM m)) => Monoid (VertexTracers m blk) where
  mempty = nullVertexTracers

instance (Applicative m, Applicative (IOLike.STM m)) => Semigroup (VertexTracers m blk) where
  VertexTracers l0 l1 l2 l3 l4 l5 <> VertexTracers r0 r1 r2 r3 r4 r5 =
      VertexTracers
        (l0 <> r0) (l1 <> r1) (l2 <> r2) (l3 <> r3) (l4 <> r4) (l5 <> r5)

nullVertexTracers :: (Monad m, Monad (IOLike.STM m)) => VertexTracers m blk
nullVertexTracers = VertexTracers {
    tracerBtime    = nullTracer
  , tracerChainDB  = nullTracer
  , tracerHarness  = nullTracer
  , tracerLedgerDB = nullTracer
  , tracersConsensus    = Real.nullTracers
  , tracersNTN     = CNTN.nullTracers
  }

{-------------------------------------------------------------------------------
  Abbreviations
-------------------------------------------------------------------------------}

-- | Writing to a vertex's 'CommandVar' causes the node instance occupying that
-- vertex to spawn client mini protocol threads for the given peer using fresh
-- channels, and then send those channels to the peer's 'HandshakeVar'.
type CommandVar m = StrictTMVar m (VertexCommand m)

data VertexCommand m =
    DisconnectFrom PeerId
    -- ^ send Mux.Terminate to the ChainSync client for that peer
  | InitiateTo !(MockNetwork.HandshakeCmd m)
    -- ^ initiate directed edge to that peer
  | RespondTo !(MockNetwork.HandshakeCmd m)
    -- ^ receive directed edge from that peer

  -- TODO
  --
  --   | ReceiveSIGINT
  --     -- ^ cause the node to shutdown a la SIGINT
  --   | Start
  --     -- ^ (re)start the node

-- | The variables on which a node instance --- the node instance itself, not
-- each of its mini protocols threads --- expects to receive messages.
type InputVars m = CommandVar m

{-------------------------------------------------------------------------------
  Incidentals
-------------------------------------------------------------------------------}

data NodeDBs db = NodeDBs {
    nodeDBsImmutable :: db
  , nodeDBsVolatile  :: db
  , nodeDBsLedger    :: db
  }
  deriving (Foldable, Functor, Traversable)

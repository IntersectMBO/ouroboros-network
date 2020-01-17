{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
-- | Run the whole Node
--
-- Intended for qualified import.
--
module Ouroboros.Consensus.Node
  ( DiffusionTracers (..)
  , DiffusionArguments (..)
  , run
  , IsProducer (..)
    -- * Exposed by 'run'
  , RunNode (..)
  , Tracers
  , Tracers' (..)
  , ChainDB.TraceEvent (..)
  , ProtocolInfo (..)
  , ChainDbArgs (..)
  , NodeArgs (..)
  , NodeKernel (..)
  , MaxBlockSizeOverride (..)
  , IPSubscriptionTarget (..)
  , DnsSubscriptionTarget (..)
  , ConnectionId (..)
    -- * Internal helpers
  , withChainDB
  , mkChainDbArgs
  , mkNodeArgs
  ) where

import           Control.Tracer (Tracer)
import           Crypto.Random
import           Data.ByteString.Lazy (ByteString)
import           Data.Proxy (Proxy (..))
import           Data.Time.Clock (secondsToDiffTime)

import           Control.Monad.Class.MonadThrow

import           Ouroboros.Network.Block (WithBlockSize (..))
import           Ouroboros.Network.Diffusion
import           Ouroboros.Network.Magic
import           Ouroboros.Network.NodeToClient (DictVersion (..),
                     NodeToClientVersion (..), NodeToClientVersionData (..),
                     nodeToClientCodecCBORTerm)
import           Ouroboros.Network.NodeToNode (NodeToNodeVersion (..),
                     NodeToNodeVersionData (..), nodeToNodeCodecCBORTerm)
import           Ouroboros.Network.Protocol.ChainSync.PipelineDecision
                     (pipelineDecisionLowHighMark)
import           Ouroboros.Network.Socket (ConnectionId)

import           Ouroboros.Consensus.Block (BlockProtocol, getHeader)
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.ChainSyncClient (ClockSkew (..))
import           Ouroboros.Consensus.Ledger.Extended (ExtLedgerState (..))
import           Ouroboros.Consensus.Mempool (MempoolCapacityBytes (..))
import           Ouroboros.Consensus.Node.DbMarker
import           Ouroboros.Consensus.Node.ErrorPolicy
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Node.Tracers
import           Ouroboros.Consensus.NodeKernel
import           Ouroboros.Consensus.NodeNetwork
import           Ouroboros.Consensus.Protocol hiding (Protocol)
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Consensus.Util.ResourceRegistry

import           Ouroboros.Storage.ChainDB (ChainDB, ChainDbArgs)
import qualified Ouroboros.Storage.ChainDB as ChainDB
import           Ouroboros.Storage.EpochInfo (EpochInfo, newEpochInfo)
import           Ouroboros.Storage.FS.API.Types
import           Ouroboros.Storage.FS.IO (ioHasFS)
import           Ouroboros.Storage.ImmutableDB (ValidationPolicy (..))
import           Ouroboros.Storage.LedgerDB.DiskPolicy (defaultDiskPolicy)
import           Ouroboros.Storage.LedgerDB.InMemory (ledgerDbDefaultParams)

-- | Whether the node produces blocks or not.
data IsProducer
  = IsProducer
  | IsNotProducer
  deriving (Eq, Show)

-- | Start a node.
--
-- This opens the 'ChainDB', sets up the 'NodeKernel' and initialises the
-- network layer.
--
-- This function runs forever unless an exception is thrown.
run
  :: forall blk.
     RunNode blk
  => Tracers IO ConnectionId blk          -- ^ Consensus tracers
  -> Tracer  IO (ChainDB.TraceEvent blk)  -- ^ ChainDB tracer
  -> DiffusionTracers                     -- ^ Diffusion tracers
  -> DiffusionArguments                   -- ^ Diffusion arguments
  -> NetworkMagic
  -> FilePath                             -- ^ Database path
  -> ProtocolInfo blk
  -> IsProducer
  -> (ChainDbArgs IO blk -> ChainDbArgs IO blk)
      -- ^ Customise the 'ChainDbArgs'
  -> (NodeArgs IO ConnectionId blk -> NodeArgs IO ConnectionId blk)
      -- ^ Customise the 'NodeArgs'
  -> (ResourceRegistry IO -> NodeKernel IO ConnectionId blk -> IO ())
     -- ^ Called on the 'NodeKernel' after creating it, but before the network
     -- layer is initialised.
  -> IO ()
run tracers chainDbTracer diffusionTracers diffusionArguments networkMagic
    dbPath pInfo isProducer customiseChainDbArgs customiseNodeArgs
    onNodeKernel = do
    let mountPoint = MountPoint dbPath
    either throwM return =<< checkDbMarker
      (ioHasFS mountPoint)
      mountPoint
      (nodeProtocolMagicId (Proxy @blk) cfg)
    withRegistry $ \registry -> do

      lockDbMarkerFile registry dbPath
      btime <- realBlockchainTime
        registry
        (blockchainTimeTracer tracers)
        (nodeStartTime (Proxy @blk) cfg)
        (focusSlotLengths slotLengths)

      withChainDB
        chainDbTracer
        registry
        btime
        dbPath
        cfg
        initLedger
        customiseChainDbArgs
        $ \chainDB -> do

          let nodeArgs = customiseNodeArgs $ mkNodeArgs
                registry
                cfg
                initState
                tracers
                btime
                chainDB
                isProducer

          nodeKernel <- initNodeKernel nodeArgs
          onNodeKernel registry nodeKernel
          let networkApps :: NetworkApplication
                               IO ConnectionId
                               ByteString ByteString ByteString ByteString ByteString
                               ()
              networkApps = consensusNetworkApps
                nodeKernel
                nullProtocolTracers
                (protocolCodecs (getNodeConfig nodeKernel))
                (protocolHandlers nodeArgs nodeKernel)

              diffusionApplications = DiffusionApplications
               { daResponderApplication =
                   simpleSingletonVersions
                     NodeToNodeV_1
                     nodeToNodeVersionData
                     (DictVersion nodeToNodeCodecCBORTerm)
                     (responderNetworkApplication networkApps)
               , daInitiatorApplication =
                   simpleSingletonVersions
                     NodeToNodeV_1
                     nodeToNodeVersionData
                     (DictVersion nodeToNodeCodecCBORTerm)
                     (initiatorNetworkApplication networkApps)
               , daLocalResponderApplication =
                   simpleSingletonVersions
                     NodeToClientV_1
                     nodeToClientVersionData
                     (DictVersion nodeToClientCodecCBORTerm)
                     (localResponderNetworkApplication networkApps)
               , daErrorPolicies = consensusErrorPolicy
               }

          runDataDiffusion diffusionTracers
                           diffusionArguments
                           diffusionApplications
  where
    ProtocolInfo
      { pInfoConfig     = cfg
      , pInfoInitLedger = initLedger
      , pInfoInitState  = initState
      } = pInfo

    slotLengths = protocolSlotLengths cfg

    nodeToNodeVersionData   = NodeToNodeVersionData { networkMagic   = networkMagic }
    nodeToClientVersionData = NodeToClientVersionData { networkMagic = networkMagic }

withChainDB
  :: forall blk a. RunNode blk
  => Tracer IO (ChainDB.TraceEvent blk)
  -> ResourceRegistry IO
  -> BlockchainTime IO
  -> FilePath
     -- ^ Database path
  -> NodeConfig (BlockProtocol blk)
  -> ExtLedgerState blk
     -- ^ Initial ledger
  -> (ChainDbArgs IO blk -> ChainDbArgs IO blk)
      -- ^ Customise the 'ChainDbArgs'
  -> (ChainDB IO blk -> IO a)
  -> IO a
withChainDB tracer registry btime dbPath cfg initLedger customiseArgs k = do
    epochInfo <- newEpochInfo $ nodeEpochSize (Proxy @blk) cfg
    let args = customiseArgs $
          mkChainDbArgs tracer registry btime dbPath cfg initLedger
          epochInfo
    ChainDB.withDB args k

mkChainDbArgs
  :: forall blk. RunNode blk
  => Tracer IO (ChainDB.TraceEvent blk)
  -> ResourceRegistry IO
  -> BlockchainTime IO
  -> FilePath
     -- ^ Database path
  -> NodeConfig (BlockProtocol blk)
  -> ExtLedgerState blk
     -- ^ Initial ledger
  -> EpochInfo IO
  -> ChainDbArgs IO blk
mkChainDbArgs tracer registry btime dbPath cfg initLedger
              epochInfo = (ChainDB.defaultArgs dbPath)
    { ChainDB.cdbBlocksPerFile    = 1000
    , ChainDB.cdbDecodeBlock      = nodeDecodeBlock         cfg
    , ChainDB.cdbDecodeHeader     = nodeDecodeHeader        cfg
    , ChainDB.cdbDecodeChainState = nodeDecodeChainState    (Proxy @blk) cfg
    , ChainDB.cdbDecodeHash       = nodeDecodeHeaderHash    (Proxy @blk)
    , ChainDB.cdbDecodeLedger     = nodeDecodeLedgerState   cfg
    , ChainDB.cdbEncodeBlock      = nodeEncodeBlockWithInfo cfg
    , ChainDB.cdbEncodeHeader     = nodeEncodeHeader        cfg
    , ChainDB.cdbEncodeChainState = nodeEncodeChainState    (Proxy @blk) cfg
    , ChainDB.cdbEncodeHash       = nodeEncodeHeaderHash    (Proxy @blk)
    , ChainDB.cdbEncodeLedger     = nodeEncodeLedgerState   cfg
    , ChainDB.cdbEpochInfo        = epochInfo
    , ChainDB.cdbHashInfo         = nodeHashInfo            (Proxy @blk)
    , ChainDB.cdbGenesis          = return initLedger
    , ChainDB.cdbAddHdrEnv        = nodeAddHeaderEnvelope   (Proxy @blk)
    , ChainDB.cdbDiskPolicy       = defaultDiskPolicy secParam
    , ChainDB.cdbIsEBB            = nodeIsEBB . getHeader
    , ChainDB.cdbBlockSize        = nodeBlockSize
    , ChainDB.cdbCheckIntegrity   = nodeCheckIntegrity      cfg
    , ChainDB.cdbParamsLgrDB      = ledgerDbDefaultParams secParam
    , ChainDB.cdbNodeConfig       = cfg
    , ChainDB.cdbRegistry         = registry
    , ChainDB.cdbTracer           = tracer
    , ChainDB.cdbValidation       = ValidateMostRecentEpoch
    , ChainDB.cdbGcDelay          = secondsToDiffTime 10
    , ChainDB.cdbBlockchainTime   = btime
    }
  where
    secParam = protocolSecurityParam cfg

mkNodeArgs
  :: forall blk. RunNode blk
  => ResourceRegistry IO
  -> NodeConfig (BlockProtocol blk)
  -> NodeState  (BlockProtocol blk)
  -> Tracers IO ConnectionId blk
  -> BlockchainTime IO
  -> ChainDB IO blk
  -> IsProducer
  -> NodeArgs IO ConnectionId blk
mkNodeArgs registry cfg initState tracers btime chainDB isProducer = NodeArgs
    { tracers
    , registry
    , maxClockSkew       = ClockSkew 1
    , cfg
    , initState
    , btime
    , chainDB
    , blockProduction
    , blockMatchesHeader  = nodeBlockMatchesHeader . withoutBlockSize
    , maxUnackTxs         = 100 -- TODO
    , maxBlockSize        = NoOverride
    , mempoolCap          = MempoolCapacityBytes 128_000 -- TODO
    , chainSyncPipelining = pipelineDecisionLowHighMark 200 300 -- TODO
    }
  where
    blockProduction = case isProducer of
      IsNotProducer -> Nothing
      IsProducer    -> Just BlockProduction
                         { produceDRG   = drgNew
                         , produceBlock = nodeForgeBlock cfg
                         }

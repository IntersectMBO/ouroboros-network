{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NumericUnderscores  #-}
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
  , MempoolCapacityBytesOverride (..)
  , IPSubscriptionTarget (..)
  , DnsSubscriptionTarget (..)
  , ConnectionId (..)
  , RemoteConnectionId
    -- * Internal helpers
  , openChainDB
  , mkChainDbArgs
  , mkNodeArgs
  ) where

import           Codec.Serialise (DeserialiseFailure)
import           Control.Monad (when)
import           Control.Tracer (Tracer)
import           Crypto.Random
import           Data.ByteString.Lazy (ByteString)
import           Data.Proxy (Proxy (..))
import           Data.Time.Clock (secondsToDiffTime)

import           Ouroboros.Network.Diffusion
import           Ouroboros.Network.Magic
import           Ouroboros.Network.NodeToClient (DictVersion (..),
                     LocalConnectionId, NodeToClientVersionData (..),
                     nodeToClientCodecCBORTerm)
import           Ouroboros.Network.NodeToNode (NodeToNodeVersionData (..),
                     RemoteConnectionId, nodeToNodeCodecCBORTerm)
import           Ouroboros.Network.Protocol.ChainSync.PipelineDecision
                     (pipelineDecisionLowHighMark)

import           Ouroboros.Consensus.Block (BlockProtocol)
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.ChainSyncClient (ClockSkew (..))
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Extended (ExtLedgerState (..))
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Node.DbMarker
import           Ouroboros.Consensus.Node.ErrorPolicy
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Node.Recovery
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Node.Tracers
import           Ouroboros.Consensus.NodeKernel
import           Ouroboros.Consensus.NodeNetwork
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Consensus.Util.ResourceRegistry

import           Ouroboros.Consensus.Storage.ChainDB (ChainDB, ChainDbArgs)
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import           Ouroboros.Consensus.Storage.EpochInfo (EpochInfo, newEpochInfo)
import           Ouroboros.Consensus.Storage.FS.API.Types
import           Ouroboros.Consensus.Storage.FS.IO (ioHasFS)
import           Ouroboros.Consensus.Storage.ImmutableDB (ValidationPolicy (..))
import           Ouroboros.Consensus.Storage.LedgerDB.DiskPolicy
                     (defaultDiskPolicy)
import           Ouroboros.Consensus.Storage.LedgerDB.InMemory
                     (ledgerDbDefaultParams)
import           Ouroboros.Consensus.Storage.VolatileDB
                     (BlockValidationPolicy (..), mkBlocksPerFile)

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
  => Tracers IO RemoteConnectionId  blk   -- ^ Consensus tracers
  -> ProtocolTracers IO RemoteConnectionId LocalConnectionId blk DeserialiseFailure
     -- ^ Protocol tracers
  -> Tracer IO (ChainDB.TraceEvent blk)   -- ^ ChainDB tracer
  -> DiffusionTracers                     -- ^ Diffusion tracers
  -> DiffusionArguments                   -- ^ Diffusion arguments
  -> NetworkMagic
  -> FilePath                             -- ^ Database path
  -> ProtocolInfo blk
  -> IsProducer
  -> (ChainDbArgs IO blk -> ChainDbArgs IO blk)
      -- ^ Customise the 'ChainDbArgs'
  -> (NodeArgs IO RemoteConnectionId blk -> NodeArgs IO RemoteConnectionId blk)
      -- ^ Customise the 'NodeArgs'
  -> (ResourceRegistry IO -> NodeKernel IO RemoteConnectionId blk -> IO ())
     -- ^ Called on the 'NodeKernel' after creating it, but before the network
     -- layer is initialised.
  -> IO ()
run tracers protocolTracers chainDbTracer diffusionTracers diffusionArguments
    networkMagic dbPath pInfo isProducer customiseChainDbArgs
    customiseNodeArgs onNodeKernel = do
    either throwM return =<< checkDbMarker
      hasFS
      mountPoint
      (nodeProtocolMagicId (Proxy @blk) cfg)
    withRegistry $ \registry -> do

      lockDbMarkerFile registry dbPath
      btime <- realBlockchainTime
        registry
        (blockchainTimeTracer tracers)
        (nodeStartTime (Proxy @blk) cfg)
        (focusSlotLengths slotLengths)

      -- When we shut down cleanly, we create a marker file so that the next
      -- time we start, we know we don't have to validate the contents of the
      -- whole ChainDB. When we shut down with an exception indicating
      -- corruption or something going wrong with the file system, we don't
      -- create this marker file so that the next time we start, we do a full
      -- validation.
      lastShutDownWasClean <- hasCleanShutdownMarker hasFS
      when lastShutDownWasClean $ removeCleanShutdownMarker hasFS
      let customiseChainDbArgs' args
            | lastShutDownWasClean
            = customiseChainDbArgs args
            | otherwise
              -- When the last shutdown was not clean, validate the complete
              -- ChainDB to detect and recover from any corruptions. This will
              -- override the default value /and/ the user-customised value of
              -- the 'ChainDB.cdbImmValidation' and the
              -- 'ChainDB.cdbVolValidation' fields.
            = (customiseChainDbArgs args)
              { ChainDB.cdbImmValidation = ValidateAllEpochs
              , ChainDB.cdbVolValidation = ValidateAll
              }

      -- On a clean shutdown, create a marker in the database folder so that
      -- next time we start up, we know we don't have to validate the whole
      -- database.
      createMarkerOnCleanShutdown (Proxy @blk) hasFS $ do

        (_, chainDB) <- allocate registry
          (\_ -> openChainDB
            chainDbTracer registry btime dbPath cfg initLedger
            customiseChainDbArgs')
          ChainDB.closeDB

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

        let networkApps = mkNetworkApps nodeArgs nodeKernel
            diffusionApplications = mkDiffusionApplications networkApps

        runDataDiffusion diffusionTracers
                         diffusionArguments
                         diffusionApplications
  where
    mountPoint = MountPoint dbPath
    hasFS      = ioHasFS mountPoint

    ProtocolInfo
      { pInfoConfig     = cfg
      , pInfoInitLedger = initLedger
      , pInfoInitState  = initState
      } = pInfo

    slotLengths = protocolSlotLengths cfg

    nodeToNodeVersionData   = NodeToNodeVersionData { networkMagic   = networkMagic }
    nodeToClientVersionData = NodeToClientVersionData { networkMagic = networkMagic }

    mkNetworkApps
      :: NodeArgs   IO RemoteConnectionId blk
      -> NodeKernel IO RemoteConnectionId blk
      -> NetworkProtocolVersion blk
      -> NetworkApplication
           IO RemoteConnectionId LocalConnectionId
           ByteString ByteString ByteString ByteString ByteString ByteString
           ()
    mkNetworkApps nodeArgs nodeKernel version = consensusNetworkApps
      nodeKernel
      protocolTracers
      (protocolCodecs (getNodeConfig nodeKernel) version)
      (protocolHandlers nodeArgs nodeKernel)

    mkDiffusionApplications
      :: (   NetworkProtocolVersion blk
          -> NetworkApplication
              IO RemoteConnectionId LocalConnectionId
              ByteString ByteString ByteString ByteString ByteString ByteString
              ()
         )
      -> DiffusionApplications
    mkDiffusionApplications networkApps = DiffusionApplications
      { daResponderApplication = combineVersions [
      simpleSingletonVersions
    (nodeToNodeProtocolVersion (Proxy @blk) version)
    nodeToNodeVersionData
    (DictVersion nodeToNodeCodecCBORTerm)
    (responderNetworkApplication $ networkApps version)
      | version <- supportedNetworkProtocolVersions (Proxy @blk)
      ]
     , daInitiatorApplication = combineVersions [
           simpleSingletonVersions
             (nodeToNodeProtocolVersion (Proxy @blk) version)
             nodeToNodeVersionData
             (DictVersion nodeToNodeCodecCBORTerm)
             (initiatorNetworkApplication $ networkApps version)
         | version <- supportedNetworkProtocolVersions (Proxy @blk)
         ]
     , daLocalResponderApplication = combineVersions [
           simpleSingletonVersions
             (nodeToClientProtocolVersion (Proxy @blk) version)
             nodeToClientVersionData
             (DictVersion nodeToClientCodecCBORTerm)
             (localResponderNetworkApplication $ networkApps version)
         | version <- supportedNetworkProtocolVersions (Proxy @blk)
         ]
     , daErrorPolicies = consensusErrorPolicy
     }

    combineVersions :: Semigroup a => [a] -> a
    combineVersions = foldr1 (<>)

openChainDB
  :: forall blk. RunNode blk
  => Tracer IO (ChainDB.TraceEvent blk)
  -> ResourceRegistry IO
  -> BlockchainTime IO
  -> FilePath
     -- ^ Database path
  -> TopLevelConfig blk
  -> ExtLedgerState blk
     -- ^ Initial ledger
  -> (ChainDbArgs IO blk -> ChainDbArgs IO blk)
      -- ^ Customise the 'ChainDbArgs'
  -> IO (ChainDB IO blk)
openChainDB tracer registry btime dbPath cfg initLedger customiseArgs = do
    epochInfo <- newEpochInfo $ nodeEpochSize (Proxy @blk) cfg
    let args = customiseArgs $
          mkChainDbArgs tracer registry btime dbPath cfg initLedger
          epochInfo
    ChainDB.openDB args

mkChainDbArgs
  :: forall blk. RunNode blk
  => Tracer IO (ChainDB.TraceEvent blk)
  -> ResourceRegistry IO
  -> BlockchainTime IO
  -> FilePath
     -- ^ Database path
  -> TopLevelConfig blk
  -> ExtLedgerState blk
     -- ^ Initial ledger
  -> EpochInfo IO
  -> ChainDbArgs IO blk
mkChainDbArgs tracer registry btime dbPath cfg initLedger
              epochInfo = (ChainDB.defaultArgs dbPath)
    { ChainDB.cdbBlocksPerFile    = mkBlocksPerFile 1000
    , ChainDB.cdbDecodeBlock      = nodeDecodeBlock         cfg
    , ChainDB.cdbDecodeHeader     = nodeDecodeHeader        cfg SerialisedToDisk
    , ChainDB.cdbDecodeChainState = nodeDecodeChainState    (Proxy @blk) cfg
    , ChainDB.cdbDecodeHash       = nodeDecodeHeaderHash    (Proxy @blk)
    , ChainDB.cdbDecodeLedger     = nodeDecodeLedgerState   cfg
    , ChainDB.cdbDecodeTipInfo    = nodeDecodeTipInfo       (Proxy @blk)
    , ChainDB.cdbEncodeBlock      = nodeEncodeBlockWithInfo cfg
    , ChainDB.cdbEncodeHeader     = nodeEncodeHeader        cfg SerialisedToDisk
    , ChainDB.cdbEncodeChainState = nodeEncodeChainState    (Proxy @blk) cfg
    , ChainDB.cdbEncodeHash       = nodeEncodeHeaderHash    (Proxy @blk)
    , ChainDB.cdbEncodeLedger     = nodeEncodeLedgerState   cfg
    , ChainDB.cdbEncodeTipInfo    = nodeEncodeTipInfo       (Proxy @blk)
    , ChainDB.cdbEpochInfo        = epochInfo
    , ChainDB.cdbHashInfo         = nodeHashInfo            (Proxy @blk)
    , ChainDB.cdbGenesis          = return initLedger
    , ChainDB.cdbAddHdrEnv        = nodeAddHeaderEnvelope   (Proxy @blk)
    , ChainDB.cdbDiskPolicy       = defaultDiskPolicy secParam
    , ChainDB.cdbIsEBB            = nodeIsEBB
    , ChainDB.cdbCheckIntegrity   = nodeCheckIntegrity      cfg
    , ChainDB.cdbParamsLgrDB      = ledgerDbDefaultParams secParam
    , ChainDB.cdbNodeConfig       = cfg
    , ChainDB.cdbRegistry         = registry
    , ChainDB.cdbTracer           = tracer
    , ChainDB.cdbImmValidation    = ValidateMostRecentEpoch
    , ChainDB.cdbVolValidation    = NoValidation
    , ChainDB.cdbGcDelay          = secondsToDiffTime 10
    , ChainDB.cdbBlockchainTime   = btime
    }
  where
    secParam = configSecurityParam cfg

mkNodeArgs
  :: forall blk. RunNode blk
  => ResourceRegistry IO
  -> TopLevelConfig blk
  -> NodeState (BlockProtocol blk)
  -> Tracers IO RemoteConnectionId blk
  -> BlockchainTime IO
  -> ChainDB IO blk
  -> IsProducer
  -> NodeArgs IO RemoteConnectionId blk
mkNodeArgs registry cfg initState tracers btime chainDB isProducer = NodeArgs
    { tracers
    , registry
    , maxClockSkew       = ClockSkew 1
    , cfg
    , initState
    , btime
    , chainDB
    , initChainDB         = nodeInitChainDB
    , blockProduction
    , blockFetchSize      = nodeBlockFetchSize
    , blockMatchesHeader  = nodeBlockMatchesHeader
    , maxUnackTxs         = 100 -- TODO
    , maxBlockSize        = NoOverride
    , mempoolCap          = NoMempoolCapacityBytesOverride
    , chainSyncPipelining = pipelineDecisionLowHighMark 200 300 -- TODO
    }
  where
    blockProduction = case isProducer of
      IsNotProducer -> Nothing
      IsProducer    -> Just BlockProduction
                         { produceDRG   = drgNew
                         , produceBlock = nodeForgeBlock cfg
                         }

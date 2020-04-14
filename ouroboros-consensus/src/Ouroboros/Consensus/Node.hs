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
import           Data.ByteString.Lazy (ByteString)
import           Data.Proxy (Proxy (..))

import           Ouroboros.Network.Diffusion
import           Ouroboros.Network.Magic
import           Ouroboros.Network.NodeToClient (DictVersion (..),
                     LocalConnectionId, NodeToClientVersionData (..),
                     nodeToClientCodecCBORTerm)
import           Ouroboros.Network.NodeToNode (MiniProtocolParameters (..),
                     NodeToNodeVersionData (..), RemoteConnectionId,
                     defaultMiniProtocolParameters, nodeToNodeCodecCBORTerm)

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Extended (ExtLedgerState (..))
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client
                     (ClockSkew (..))
import           Ouroboros.Consensus.Node.DbMarker
import           Ouroboros.Consensus.Node.ErrorPolicy
import           Ouroboros.Consensus.Node.LedgerDerivedInfo
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Node.Recovery
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Node.State
import           Ouroboros.Consensus.Node.Tracers
import           Ouroboros.Consensus.NodeKernel
import           Ouroboros.Consensus.NodeNetwork
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Consensus.Util.Random
import           Ouroboros.Consensus.Util.ResourceRegistry

import           Ouroboros.Consensus.Storage.ChainDB (ChainDB, ChainDbArgs)
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import           Ouroboros.Consensus.Storage.FS.API.Types
import           Ouroboros.Consensus.Storage.FS.IO (ioHasFS)
import           Ouroboros.Consensus.Storage.ImmutableDB (ChunkInfo,
                     ValidationPolicy (..))
import           Ouroboros.Consensus.Storage.LedgerDB.DiskPolicy
                     (defaultDiskPolicy)
import           Ouroboros.Consensus.Storage.LedgerDB.InMemory
                     (ledgerDbDefaultParams)
import           Ouroboros.Consensus.Storage.VolatileDB
                     (BlockValidationPolicy (..), mkBlocksPerFile)

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
  -> (ChainDbArgs IO blk -> ChainDbArgs IO blk)
      -- ^ Customise the 'ChainDbArgs'
  -> (NodeArgs IO RemoteConnectionId blk -> NodeArgs IO RemoteConnectionId blk)
      -- ^ Customise the 'NodeArgs'
  -> (ResourceRegistry IO -> NodeKernel IO RemoteConnectionId blk -> IO ())
     -- ^ Called on the 'NodeKernel' after creating it, but before the network
     -- layer is initialised.
  -> IO ()
run tracers protocolTracers chainDbTracer diffusionTracers diffusionArguments
    networkMagic dbPath pInfo customiseChainDbArgs
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
              { ChainDB.cdbImmValidation = ValidateAllChunks
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
        nodeKernel <- initNodeKernel nodeArgs
        onNodeKernel registry nodeKernel

        let networkApps = mkNetworkApps nodeArgs nodeKernel
            diffusionApplications = mkDiffusionApplications (miniProtocolParameters nodeArgs) networkApps

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

    slotLengths = knownSlotLengths (configBlock cfg)

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
      (protocolCodecs (getTopLevelConfig nodeKernel) version)
      (Just 70) -- timeout after waiting this long for the next header
                -- 70s allows for 3 slots (3 * 20s)
      (protocolHandlers nodeArgs nodeKernel)

    mkDiffusionApplications
      :: MiniProtocolParameters
      -> (   NetworkProtocolVersion blk
          -> NetworkApplication
              IO RemoteConnectionId LocalConnectionId
              ByteString ByteString ByteString ByteString ByteString ByteString
              ()
         )
      -> DiffusionApplications
    mkDiffusionApplications miniProtocolParams networkApps = DiffusionApplications
      { daResponderApplication = combineVersions [
      simpleSingletonVersions
    (nodeToNodeProtocolVersion (Proxy @blk) version)
    nodeToNodeVersionData
    (DictVersion nodeToNodeCodecCBORTerm)
    (responderNetworkApplication miniProtocolParams $ networkApps version)
      | version <- supportedNetworkProtocolVersions (Proxy @blk)
      ]
     , daInitiatorApplication = combineVersions [
           simpleSingletonVersions
             (nodeToNodeProtocolVersion (Proxy @blk) version)
             nodeToNodeVersionData
             (DictVersion nodeToNodeCodecCBORTerm)
             (initiatorNetworkApplication miniProtocolParams $ networkApps version)
         | version <- supportedNetworkProtocolVersions (Proxy @blk)
         ]
     , daLocalResponderApplication = combineVersions [
           simpleSingletonVersions
             nodeToClientVersion
             nodeToClientVersionData
             (DictVersion nodeToClientCodecCBORTerm)
             (localResponderNetworkApplication
               (networkApps version)
               nodeToClientVersion)
         | version <- supportedNetworkProtocolVersions (Proxy @blk)
         , let nodeToClientVersion = nodeToClientProtocolVersion (Proxy @blk) version
         ]
     , daErrorPolicies = consensusErrorPolicy (Proxy @blk)
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
openChainDB tracer registry btime dbPath cfg initLedger customiseArgs =
    ChainDB.openDB args
  where
    args :: ChainDbArgs IO blk
    args = customiseArgs $
             mkChainDbArgs tracer registry btime dbPath cfg initLedger
             (nodeImmDbChunkInfo (Proxy @blk) cfg)

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
  -> ChunkInfo
  -> ChainDbArgs IO blk
mkChainDbArgs tracer registry btime dbPath cfg initLedger
              chunkInfo = (ChainDB.defaultArgs dbPath)
    { ChainDB.cdbBlocksPerFile        = mkBlocksPerFile 1000
    , ChainDB.cdbDecodeBlock          = nodeDecodeBlock          cfg
    , ChainDB.cdbDecodeHeader         = nodeDecodeHeader         cfg SerialisedToDisk
    , ChainDB.cdbDecodeConsensusState = nodeDecodeConsensusState (Proxy @blk) cfg
    , ChainDB.cdbDecodeHash           = nodeDecodeHeaderHash     (Proxy @blk)
    , ChainDB.cdbDecodeLedger         = nodeDecodeLedgerState    cfg
    , ChainDB.cdbDecodeTipInfo        = nodeDecodeTipInfo        (Proxy @blk)
    , ChainDB.cdbEncodeBlock          = nodeEncodeBlockWithInfo  cfg
    , ChainDB.cdbEncodeHeader         = nodeEncodeHeader         cfg SerialisedToDisk
    , ChainDB.cdbEncodeConsensusState = nodeEncodeConsensusState (Proxy @blk) cfg
    , ChainDB.cdbEncodeHash           = nodeEncodeHeaderHash     (Proxy @blk)
    , ChainDB.cdbEncodeLedger         = nodeEncodeLedgerState    cfg
    , ChainDB.cdbEncodeTipInfo        = nodeEncodeTipInfo        (Proxy @blk)
    , ChainDB.cdbChunkInfo            = chunkInfo
    , ChainDB.cdbHashInfo             = nodeHashInfo             (Proxy @blk)
    , ChainDB.cdbGenesis              = return initLedger
    , ChainDB.cdbAddHdrEnv            = nodeAddHeaderEnvelope    (Proxy @blk)
    , ChainDB.cdbDiskPolicy           = defaultDiskPolicy secParam
    , ChainDB.cdbIsEBB                = nodeIsEBB
    , ChainDB.cdbCheckIntegrity       = nodeCheckIntegrity       cfg
    , ChainDB.cdbParamsLgrDB          = ledgerDbDefaultParams secParam
    , ChainDB.cdbTopLevelConfig       = cfg
    , ChainDB.cdbRegistry             = registry
    , ChainDB.cdbTracer               = tracer
    , ChainDB.cdbImmValidation        = ValidateMostRecentChunk
    , ChainDB.cdbVolValidation        = NoValidation
    , ChainDB.cdbBlockchainTime       = btime
    }
  where
    secParam = configSecurityParam cfg

mkNodeArgs
  :: forall blk. RunNode blk
  => ResourceRegistry IO
  -> TopLevelConfig blk
  -> NodeState blk
  -> Tracers IO RemoteConnectionId blk
  -> BlockchainTime IO
  -> ChainDB IO blk
  -> NodeArgs IO RemoteConnectionId blk
mkNodeArgs registry cfg initState tracers btime chainDB = NodeArgs
    { tracers
    , registry
    , maxClockSkew           = ClockSkew 1
    , cfg
    , initState
    , btime
    , chainDB
    , initChainDB            = nodeInitChainDB
    , blockProduction
    , blockFetchSize         = nodeBlockFetchSize
    , blockMatchesHeader     = nodeBlockMatchesHeader
    , maxBlockSize           = NoOverride
    , mempoolCap             = NoMempoolCapacityBytesOverride
    , miniProtocolParameters = defaultMiniProtocolParameters
    }
  where
    blockProduction
      | checkIfCanBeLeader (configConsensus cfg)
      = Just BlockProduction
               { produceBlock       = \_lift' -> nodeForgeBlock cfg
               , runMonadRandomDict = runMonadRandomIO
               }
      | otherwise
      = Nothing


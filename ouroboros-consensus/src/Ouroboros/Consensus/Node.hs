{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE MonadComprehensions #-}
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
  , RunNodeArgs (..)
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
import           Data.List.NonEmpty (NonEmpty)
import           Data.Proxy (Proxy (..))

import           Ouroboros.Network.Diffusion
import           Ouroboros.Network.Magic
import           Ouroboros.Network.NodeToClient (DictVersion (..),
                     LocalConnectionId, NodeToClientVersionData (..),
                     nodeToClientCodecCBORTerm)
import           Ouroboros.Network.NodeToNode (MiniProtocolParameters (..),
                     NodeToNodeVersionData (..), RemoteConnectionId,
                     defaultMiniProtocolParameters, nodeToNodeCodecCBORTerm,
                     combineVersions)

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Extended (ExtLedgerState (..))
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client
                     (ClockSkew (..))
import qualified Ouroboros.Consensus.Network.NodeToClient as NTC
import qualified Ouroboros.Consensus.Network.NodeToNode as NTN
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

-- | Arguments required by 'runNode'
data RunNodeArgs blk = RunNodeArgs {
      -- | Consensus tracers
      rnTraceConsensus :: Tracers IO RemoteConnectionId LocalConnectionId blk

      -- | Protocol tracers for node-to-node communication
    , rnTraceNTN :: NTN.Tracers IO RemoteConnectionId blk DeserialiseFailure

      -- | Protocol tracers for node-to-client communication
    , rnTraceNTC :: NTC.Tracers IO LocalConnectionId blk DeserialiseFailure

      -- | ChainDB tracer
    , rnTraceDB :: Tracer IO (ChainDB.TraceEvent blk)

      -- | Diffusion tracers
    , rnTraceDiffusion :: DiffusionTracers

      -- | Diffusion arguments
    , rnDiffusionArguments :: DiffusionArguments

      -- | Network magic
    , rnNetworkMagic :: NetworkMagic

      -- | Database path
    , rnDatabasePath :: FilePath

      -- | Protocol info
    , rnProtocolInfo :: ProtocolInfo blk

      -- | Customise the 'ChainDbArgs'
    , rnCustomiseChainDbArgs :: ChainDbArgs IO blk -> ChainDbArgs IO blk

      -- | Customise the 'NodeArgs'
    , rnCustomiseNodeArgs :: NodeArgs IO RemoteConnectionId LocalConnectionId blk
                          -> NodeArgs IO RemoteConnectionId LocalConnectionId blk

      -- | node-to-node protocol versions to run.
      --
    , rnNodeToNodeVersions   :: NonEmpty (NodeToNodeVersion blk)

      -- | node-to-client protocol versions to run.
      --
    , rnNodeToClientVersions :: NonEmpty (NodeToClientVersion blk)

      -- | Hook called after the initialisation of the 'NodeKernel'
      --
      -- Called on the 'NodeKernel' after creating it, but before the network
      -- layer is initialised.
    , rnNodeKernelHook :: ResourceRegistry IO
                       -> NodeKernel IO RemoteConnectionId LocalConnectionId blk
                       -> IO ()
    }

-- | Start a node.
--
-- This opens the 'ChainDB', sets up the 'NodeKernel' and initialises the
-- network layer.
--
-- This function runs forever unless an exception is thrown.
run :: forall blk. RunNode blk => RunNodeArgs blk -> IO ()
run RunNodeArgs{..} = do
    either throwM return =<< checkDbMarker
      hasFS
      mountPoint
      (nodeProtocolMagicId (Proxy @blk) cfg)
    withRegistry $ \registry -> do

      lockDbMarkerFile registry rnDatabasePath
      btime <- realBlockchainTime
        registry
        (blockchainTimeTracer rnTraceConsensus)
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
            = rnCustomiseChainDbArgs args
            | otherwise
              -- When the last shutdown was not clean, validate the complete
              -- ChainDB to detect and recover from any corruptions. This will
              -- override the default value /and/ the user-customised value of
              -- the 'ChainDB.cdbImmValidation' and the
              -- 'ChainDB.cdbVolValidation' fields.
            = (rnCustomiseChainDbArgs args)
              { ChainDB.cdbImmValidation = ValidateAllChunks
              , ChainDB.cdbVolValidation = ValidateAll
              }

      -- On a clean shutdown, create a marker in the database folder so that
      -- next time we start up, we know we don't have to validate the whole
      -- database.
      createMarkerOnCleanShutdown (Proxy @blk) hasFS $ do

        (_, chainDB) <- allocate registry
          (\_ -> openChainDB
            rnTraceDB registry btime rnDatabasePath cfg initLedger
            customiseChainDbArgs')
          ChainDB.closeDB

        let nodeArgs = rnCustomiseNodeArgs $
              mkNodeArgs
                registry
                cfg
                initState
                rnTraceConsensus
                btime
                chainDB
        nodeKernel <- initNodeKernel nodeArgs
        rnNodeKernelHook registry nodeKernel

        let ntnApps = mkNodeToNodeApps   nodeArgs nodeKernel
            ntcApps = mkNodeToClientApps nodeArgs nodeKernel
            diffusionApplications = mkDiffusionApplications
                                      (miniProtocolParameters nodeArgs)
                                      ntnApps
                                      ntcApps

        runDataDiffusion rnTraceDiffusion
                         rnDiffusionArguments
                         diffusionApplications
  where
    mountPoint              = MountPoint rnDatabasePath
    hasFS                   = ioHasFS mountPoint
    slotLengths             = knownSlotLengths (configBlock cfg)
    nodeToNodeVersionData   = NodeToNodeVersionData   { networkMagic = rnNetworkMagic }
    nodeToClientVersionData = NodeToClientVersionData { networkMagic = rnNetworkMagic }

    ProtocolInfo
      { pInfoConfig     = cfg
      , pInfoInitLedger = initLedger
      , pInfoInitState  = initState
      } = rnProtocolInfo

    mkNodeToNodeApps
      :: NodeArgs   IO RemoteConnectionId LocalConnectionId blk
      -> NodeKernel IO RemoteConnectionId LocalConnectionId blk
      -> NodeToNodeVersion blk
      -> NTN.Apps IO RemoteConnectionId ByteString ByteString ByteString ()
    mkNodeToNodeApps nodeArgs nodeKernel version =
        NTN.mkApps
          nodeKernel
          rnTraceNTN
          (NTN.defaultCodecs (configBlock (getTopLevelConfig nodeKernel)) version)
          (Just 70) -- timeout after waiting this long for the next header
                    -- 70s allows for 3 slots (3 * 20s)
          (NTN.mkHandlers nodeArgs nodeKernel)

    mkNodeToClientApps
      :: NodeArgs   IO RemoteConnectionId LocalConnectionId blk
      -> NodeKernel IO RemoteConnectionId LocalConnectionId blk
      -> NodeToClientVersion blk
      -> NTC.Apps IO LocalConnectionId ByteString ByteString ByteString ()
    mkNodeToClientApps nodeArgs nodeKernel version =
        NTC.mkApps
          rnTraceNTC
          (NTC.defaultCodecs (configBlock (getTopLevelConfig nodeKernel)) version)
          (NTC.mkHandlers nodeArgs nodeKernel)

    mkDiffusionApplications
      :: MiniProtocolParameters
      -> (   NodeToNodeVersion   blk
          -> NTN.Apps IO RemoteConnectionId ByteString ByteString ByteString ()
         )
      -> (   NodeToClientVersion blk
          -> NTC.Apps IO LocalConnectionId  ByteString ByteString ByteString ()
         )
      -> DiffusionApplications
    mkDiffusionApplications miniProtocolParams ntnApps ntcApps =
      DiffusionApplications {
          daResponderApplication = combineVersions [
              simpleSingletonVersions
                version'
                nodeToNodeVersionData
                (DictVersion nodeToNodeCodecCBORTerm)
                (NTN.responder miniProtocolParams version' $ ntnApps version)
            | version <- rnNodeToNodeVersions
            , let version' = nodeToNodeProtocolVersion (Proxy @blk) version
            ]
        , daInitiatorApplication = combineVersions [
              simpleSingletonVersions
                version'
                nodeToNodeVersionData
                (DictVersion nodeToNodeCodecCBORTerm)
                (NTN.initiator miniProtocolParams version' $ ntnApps version)
            | version <- rnNodeToNodeVersions
            , let version' = nodeToNodeProtocolVersion (Proxy @blk) version
            ]
        , daLocalResponderApplication = combineVersions [
              simpleSingletonVersions
                version'
                nodeToClientVersionData
                (DictVersion nodeToClientCodecCBORTerm)
                (NTC.responder version' $ ntcApps version)
            | version <- rnNodeToClientVersions
            , let version' = nodeToClientProtocolVersion (Proxy @blk) version
            ]
        , daErrorPolicies = consensusErrorPolicy (Proxy @blk)
        }

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
    , ChainDB.cdbDecodeBlock          = nodeDecodeBlock          bcfg
    , ChainDB.cdbDecodeHeader         = nodeDecodeHeader         bcfg SerialisedToDisk
    , ChainDB.cdbDecodeConsensusState = nodeDecodeConsensusState pb cfg
    , ChainDB.cdbDecodeHash           = nodeDecodeHeaderHash     pb
    , ChainDB.cdbDecodeLedger         = nodeDecodeLedgerState
    , ChainDB.cdbDecodeTipInfo        = nodeDecodeTipInfo        pb
    , ChainDB.cdbEncodeBlock          = nodeEncodeBlockWithInfo  bcfg
    , ChainDB.cdbEncodeHeader         = nodeEncodeHeader         bcfg SerialisedToDisk
    , ChainDB.cdbEncodeConsensusState = nodeEncodeConsensusState pb cfg
    , ChainDB.cdbEncodeHash           = nodeEncodeHeaderHash     pb
    , ChainDB.cdbEncodeLedger         = nodeEncodeLedgerState
    , ChainDB.cdbEncodeTipInfo        = nodeEncodeTipInfo        pb
    , ChainDB.cdbChunkInfo            = chunkInfo
    , ChainDB.cdbHashInfo             = nodeHashInfo             pb
    , ChainDB.cdbGenesis              = return initLedger
    , ChainDB.cdbAddHdrEnv            = nodeAddHeaderEnvelope    pb
    , ChainDB.cdbDiskPolicy           = defaultDiskPolicy k
    , ChainDB.cdbIsEBB                = nodeIsEBB
    , ChainDB.cdbCheckIntegrity       = nodeCheckIntegrity       cfg
    , ChainDB.cdbParamsLgrDB          = ledgerDbDefaultParams k
    , ChainDB.cdbTopLevelConfig       = cfg
    , ChainDB.cdbRegistry             = registry
    , ChainDB.cdbTracer               = tracer
    , ChainDB.cdbImmValidation        = ValidateMostRecentChunk
    , ChainDB.cdbVolValidation        = NoValidation
    , ChainDB.cdbBlockchainTime       = btime
    }
  where
    k    = configSecurityParam cfg
    bcfg = configBlock         cfg
    pb   = Proxy @blk

mkNodeArgs
  :: forall blk. RunNode blk
  => ResourceRegistry IO
  -> TopLevelConfig blk
  -> NodeState blk
  -> Tracers IO RemoteConnectionId LocalConnectionId blk
  -> BlockchainTime IO
  -> ChainDB IO blk
  -> NodeArgs IO RemoteConnectionId LocalConnectionId blk
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

{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE NamedFieldPuns      #-}
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
  , MaxTxCapacityOverride (..)
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
import           Control.Tracer (Tracer, contramap)
import           Data.ByteString.Lazy (ByteString)
import           Data.List.NonEmpty (NonEmpty)
import           Data.Proxy (Proxy (..))
import           System.Random (randomRIO)

import           Ouroboros.Network.Diffusion
import           Ouroboros.Network.Magic
import           Ouroboros.Network.NodeToClient (DictVersion (..),
                     LocalConnectionId, NodeToClientVersionData (..),
                     nodeToClientCodecCBORTerm)
import           Ouroboros.Network.NodeToNode (MiniProtocolParameters (..),
                     NodeToNodeVersionData (..), RemoteConnectionId,
                     combineVersions, defaultMiniProtocolParameters,
                     nodeToNodeCodecCBORTerm)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime hiding (getSystemStart)
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Config.SupportsNode
import           Ouroboros.Consensus.Fragment.InFuture (CheckInFuture,
                     ClockSkew)
import qualified Ouroboros.Consensus.Fragment.InFuture as InFuture
import           Ouroboros.Consensus.Ledger.Extended (ExtLedgerState (..))
import qualified Ouroboros.Consensus.Network.NodeToClient as NTC
import qualified Ouroboros.Consensus.Network.NodeToNode as NTN
import           Ouroboros.Consensus.Node.BlockProduction
import           Ouroboros.Consensus.Node.DbLock
import           Ouroboros.Consensus.Node.DbMarker
import           Ouroboros.Consensus.Node.ErrorPolicy
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Node.Recovery
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Node.Tracers
import           Ouroboros.Consensus.NodeKernel
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.Orphans ()
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
    , rnProtocolInfo :: ProtocolInfo IO blk

      -- | Customise the 'ChainDbArgs'
    , rnCustomiseChainDbArgs :: ChainDbArgs IO blk -> ChainDbArgs IO blk

      -- | Customise the 'NodeArgs'
    , rnCustomiseNodeArgs :: NodeArgs IO RemoteConnectionId LocalConnectionId blk
                          -> NodeArgs IO RemoteConnectionId LocalConnectionId blk

      -- | node-to-node protocol versions to run.
      --
    , rnNodeToNodeVersions   :: NonEmpty (BlockNodeToNodeVersion blk)

      -- | node-to-client protocol versions to run.
      --
    , rnNodeToClientVersions :: NonEmpty (BlockNodeToClientVersion blk)

      -- | Hook called after the initialisation of the 'NodeKernel'
      --
      -- Called on the 'NodeKernel' after creating it, but before the network
      -- layer is initialised.
    , rnNodeKernelHook :: ResourceRegistry IO
                       -> NodeKernel IO RemoteConnectionId LocalConnectionId blk
                       -> IO ()

      -- | Maximum clock skew.
      --
      -- Use 'defaultClockSkew' when unsure.
    , rnMaxClockSkew :: ClockSkew
    }

-- | Start a node.
--
-- This opens the 'ChainDB', sets up the 'NodeKernel' and initialises the
-- network layer.
--
-- This function runs forever unless an exception is thrown.
run :: forall blk. RunNode blk => RunNodeArgs blk -> IO ()
run runargs@RunNodeArgs{..} =

    withDBChecks runargs $ \lastShutDownWasClean ->
    withRegistry $ \registry -> do

      let systemStart :: SystemStart
          systemStart = getSystemStart (configBlock cfg)

          systemTime :: SystemTime IO
          systemTime = defaultSystemTime
                         systemStart
                         (blockchainTimeTracer rnTraceConsensus)

          inFuture :: CheckInFuture IO blk
          inFuture = InFuture.reference
                       (configLedger cfg)
                       rnMaxClockSkew
                       systemTime

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

      (_, chainDB) <- allocate registry
        (\_ -> openChainDB
          rnTraceDB registry inFuture rnDatabasePath cfg initLedger
          customiseChainDbArgs')
        ChainDB.closeDB

      btime      <- hardForkBlockchainTime
                      registry
                      (contramap
                         (\(t, ex) ->
                              TraceCurrentSlotUnknown
                                (fromRelativeTime systemStart t)
                                ex)
                         (blockchainTimeTracer rnTraceConsensus))
                      systemTime
                      (configLedger cfg)
                      (pure $ BackoffDelay 60) -- see 'BackoffDelay'
                      (ledgerState <$>
                         ChainDB.getCurrentLedger chainDB)
      nodeArgs   <- rnCustomiseNodeArgs <$>
                      mkNodeArgs
                        registry
                        cfg
                        leaderCreds
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
    randomElem :: [a] -> IO a
    randomElem xs = do
      ix <- randomRIO (0, length xs - 1)
      return $ xs !! ix

    nodeToNodeVersionData   = NodeToNodeVersionData   { networkMagic = rnNetworkMagic }
    nodeToClientVersionData = NodeToClientVersionData { networkMagic = rnNetworkMagic }

    ProtocolInfo
      { pInfoConfig      = cfg
      , pInfoInitLedger  = initLedger
      , pInfoLeaderCreds = leaderCreds
      } = rnProtocolInfo

    codecConfig :: CodecConfig blk
    codecConfig = getCodecConfig $ configBlock cfg

    mkNodeToNodeApps
      :: NodeArgs   IO RemoteConnectionId LocalConnectionId blk
      -> NodeKernel IO RemoteConnectionId LocalConnectionId blk
      -> BlockNodeToNodeVersion blk
      -> NTN.Apps IO RemoteConnectionId blk ByteString ByteString ByteString ()
    mkNodeToNodeApps nodeArgs nodeKernel version =
        NTN.mkApps
          nodeKernel
          rnTraceNTN
          (NTN.defaultCodecs codecConfig version)
          -- These values approximately correspond to false positive thresholds
          -- for streaks of empty slots with 99% probability, 99.9% probability up to
          -- 99.999% probability.
          -- t = T_s [log (1-Y) / log (1-f)]
          -- Y = [0.99, 0.999...]
          -- T_s = slot length of 1s.
          -- f = 0.05
          -- The timeout is randomly picked per bearer to avoid all bearers going down
          -- at the same time in case of a long streak of empty slots.
          -- TODO: workaround until peer selection governor.
          (Just <$> randomElem [90, 135, 180, 224, 269])
          (NTN.mkHandlers nodeArgs nodeKernel)

    mkNodeToClientApps
      :: NodeArgs   IO RemoteConnectionId LocalConnectionId blk
      -> NodeKernel IO RemoteConnectionId LocalConnectionId blk
      -> BlockNodeToClientVersion blk
      -> NTC.Apps IO LocalConnectionId ByteString ByteString ByteString ()
    mkNodeToClientApps nodeArgs nodeKernel version =
        NTC.mkApps
          rnTraceNTC
          (NTC.defaultCodecs codecConfig version)
          (NTC.mkHandlers nodeArgs nodeKernel)

    mkDiffusionApplications
      :: MiniProtocolParameters
      -> (   BlockNodeToNodeVersion blk
          -> NTN.Apps IO RemoteConnectionId blk ByteString ByteString ByteString ()
         )
      -> (   BlockNodeToClientVersion blk
          -> NTC.Apps IO LocalConnectionId      ByteString ByteString ByteString ()
         )
      -> DiffusionApplications
    mkDiffusionApplications miniProtocolParams ntnApps ntcApps =
      DiffusionApplications {
          daResponderApplication = combineVersions [
              simpleSingletonVersions
                version'
                nodeToNodeVersionData
                (DictVersion nodeToNodeCodecCBORTerm)
                (NTN.responder miniProtocolParams version $ ntnApps version)
            | version <- rnNodeToNodeVersions
            , let version' = nodeToNodeProtocolVersion (Proxy @blk) version
            ]
        , daInitiatorApplication = combineVersions [
              simpleSingletonVersions
                version'
                nodeToNodeVersionData
                (DictVersion nodeToNodeCodecCBORTerm)
                (NTN.initiator miniProtocolParams version $ ntnApps version)
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
        , daErrorPolicies = consensusErrorPolicy
        }

-- | Check the DB marker, lock the DB and look for the clean shutdown marker.
--
-- Run the body action with the DB locked, and if the last shutdown was clean.
--
withDBChecks :: forall blk a.
                RunNode blk
             => RunNodeArgs blk
             -> (Bool -> IO a)  -- ^ Body action with last shutdown was clean.
             -> IO a
withDBChecks RunNodeArgs{..} body = do

    -- Check the DB marker first, before doing the lock file, since if the
    -- marker is not present, it expects an empty DB dir.
    either throwM return =<< checkDbMarker
      hasFS
      mountPoint
      (getProtocolMagicId (configBlock pInfoConfig))

    -- Then create the lock file.
    withLockDB mountPoint $ do

      -- When we shut down cleanly, we create a marker file so that the next
      -- time we start, we know we don't have to validate the contents of the
      -- whole ChainDB. When we shut down with an exception indicating
      -- corruption or something going wrong with the file system, we don't
      -- create this marker file so that the next time we start, we do a full
      -- validation.
      lastShutDownWasClean <- hasCleanShutdownMarker hasFS
      when lastShutDownWasClean $ removeCleanShutdownMarker hasFS

      -- On a clean shutdown, create a marker in the database folder so that
      -- next time we start up, we know we don't have to validate the whole
      -- database.
      createMarkerOnCleanShutdown hasFS $
        body lastShutDownWasClean
  where
    mountPoint                   = MountPoint rnDatabasePath
    hasFS                        = ioHasFS mountPoint
    ProtocolInfo { pInfoConfig } = rnProtocolInfo

openChainDB
  :: forall blk. RunNode blk
  => Tracer IO (ChainDB.TraceEvent blk)
  -> ResourceRegistry IO
  -> CheckInFuture IO blk
  -> FilePath
     -- ^ Database path
  -> TopLevelConfig blk
  -> ExtLedgerState blk
     -- ^ Initial ledger
  -> (ChainDbArgs IO blk -> ChainDbArgs IO blk)
      -- ^ Customise the 'ChainDbArgs'
  -> IO (ChainDB IO blk)
openChainDB tracer registry inFuture dbPath cfg initLedger customiseArgs =
    ChainDB.openDB args
  where
    args :: ChainDbArgs IO blk
    args = customiseArgs $
             mkChainDbArgs tracer registry inFuture dbPath cfg initLedger
             (nodeImmDbChunkInfo cfg)

mkChainDbArgs
  :: forall blk. RunNode blk
  => Tracer IO (ChainDB.TraceEvent blk)
  -> ResourceRegistry IO
  -> CheckInFuture IO blk
  -> FilePath
     -- ^ Database path
  -> TopLevelConfig blk
  -> ExtLedgerState blk
     -- ^ Initial ledger
  -> ChunkInfo
  -> ChainDbArgs IO blk
mkChainDbArgs tracer registry inFuture dbPath cfg initLedger
              chunkInfo = (ChainDB.defaultArgs dbPath)
    { ChainDB.cdbBlocksPerFile      = mkBlocksPerFile 1000
    , ChainDB.cdbChunkInfo          = chunkInfo
    , ChainDB.cdbGenesis            = return initLedger
    , ChainDB.cdbGetBinaryBlockInfo = nodeGetBinaryBlockInfo
    , ChainDB.cdbDiskPolicy         = defaultDiskPolicy k
    , ChainDB.cdbCheckIntegrity     = nodeCheckIntegrity cfg
    , ChainDB.cdbParamsLgrDB        = ledgerDbDefaultParams k
    , ChainDB.cdbTopLevelConfig     = cfg
    , ChainDB.cdbRegistry           = registry
    , ChainDB.cdbTracer             = tracer
    , ChainDB.cdbImmValidation      = ValidateMostRecentChunk
    , ChainDB.cdbVolValidation      = NoValidation
    , ChainDB.cdbCheckInFuture      = inFuture
    }
  where
    k = configSecurityParam cfg

mkNodeArgs
  :: forall blk. RunNode blk
  => ResourceRegistry IO
  -> TopLevelConfig blk
  -> Maybe (CanBeLeader (BlockProtocol blk), MaintainForgeState IO blk)
  -> Tracers IO RemoteConnectionId LocalConnectionId blk
  -> BlockchainTime IO
  -> ChainDB IO blk
  -> IO (NodeArgs IO RemoteConnectionId LocalConnectionId blk)
mkNodeArgs registry cfg mIsLeader tracers btime chainDB = do
    blockProduction <-
      case mIsLeader of
        Just (proof, mfs) -> Just <$> blockProductionIO cfg proof mfs
        Nothing           -> return Nothing
    return NodeArgs
      { tracers
      , registry
      , cfg
      , btime
      , chainDB
      , blockProduction
      , initChainDB             = nodeInitChainDB
      , blockFetchSize          = nodeBlockFetchSize
      , maxTxCapacityOverride   = NoMaxTxCapacityOverride
      , mempoolCapacityOverride = NoMempoolCapacityBytesOverride
      , miniProtocolParameters  = defaultMiniProtocolParameters
      }

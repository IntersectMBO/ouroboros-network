{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
-- | Run the whole Node
--
-- Intended for qualified import.
--
module Ouroboros.Consensus.Node (
    run
  , runWith
    -- * Standard arguments
  , StdRunNodeArgs (..)
  , stdBfcSaltIO
  , stdChainSyncTimeout
  , stdKeepAliveRngIO
  , stdLowLevelRunNodeArgsIO
  , stdMkChainDbHasFS
  , stdRunDataDiffusion
  , stdVersionDataNTC
  , stdVersionDataNTN
  , stdWithCheckedDB
    -- * Exposed by 'run' et al
  , ChainDB.RelativeMountPoint (..)
  , ChainDB.TraceEvent (..)
  , ChainDbArgs (..)
  , ConnectionId (..)
  , DiffusionArguments (..)
  , DiffusionTracers (..)
  , DnsSubscriptionTarget (..)
  , HardForkBlockchainTimeArgs (..)
  , IPSubscriptionTarget (..)
  , LastShutDownWasClean (..)
  , LowLevelRunNodeArgs (..)
  , MaxTxCapacityOverride (..)
  , MempoolCapacityBytesOverride (..)
  , NodeKernel (..)
  , NodeKernelArgs (..)
  , ProtocolInfo (..)
  , RunNode
  , RunNodeArgs (..)
  , Tracers
  , Tracers' (..)
    -- * Internal helpers
  , mkChainDbArgs
  , mkNodeKernelArgs
  , nodeKernelArgsEnforceInvariants
  , openChainDB
  ) where

import           Codec.Serialise (DeserialiseFailure)
import           Control.Monad (when)
import           Control.Tracer (Tracer, contramap)
import           Data.ByteString.Lazy (ByteString)
import           Data.Hashable (Hashable)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Typeable (Typeable)
import           System.FilePath ((</>))
import           System.Random (StdGen, newStdGen, randomIO, randomRIO)

import           Control.Monad.Class.MonadTime (MonadTime)
import           Control.Monad.Class.MonadTimer (MonadTimer)

import           Ouroboros.Network.BlockFetch (BlockFetchConfiguration (..))
import           Ouroboros.Network.Diffusion
import           Ouroboros.Network.Magic
import           Ouroboros.Network.NodeToClient (LocalAddress,
                     NodeToClientVersionData (..))
import           Ouroboros.Network.NodeToNode (DiffusionMode,
                     MiniProtocolParameters (..), NodeToNodeVersionData (..),
                     RemoteAddress, combineVersions,
                     defaultMiniProtocolParameters)
import           Ouroboros.Network.Protocol.Limits (shortWait)

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
import           Ouroboros.Consensus.Node.DbLock
import           Ouroboros.Consensus.Node.DbMarker
import           Ouroboros.Consensus.Node.ErrorPolicy
import           Ouroboros.Consensus.Node.InitStorage
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Node.Recovery
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Node.Tracers
import           Ouroboros.Consensus.NodeKernel
import           Ouroboros.Consensus.Util.Args
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Consensus.Util.ResourceRegistry
import           Ouroboros.Consensus.Util.Time (secondsToNominalDiffTime)

import           Ouroboros.Consensus.Storage.ChainDB (ChainDB, ChainDbArgs)
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import           Ouroboros.Consensus.Storage.FS.API (SomeHasFS (..))
import           Ouroboros.Consensus.Storage.FS.API.Types
import           Ouroboros.Consensus.Storage.FS.IO (ioHasFS)
import           Ouroboros.Consensus.Storage.ImmutableDB (ChunkInfo,
                     ValidationPolicy (..))
import           Ouroboros.Consensus.Storage.LedgerDB.DiskPolicy
                     (SnapshotInterval (..), defaultDiskPolicy)
import           Ouroboros.Consensus.Storage.VolatileDB
                     (BlockValidationPolicy (..))

-- | Arguments expected from any invocation of 'runWith', whether by deployed
-- code, tests, etc.
data RunNodeArgs m addrNTN addrNTC blk = RunNodeArgs {
      -- | Consensus tracers
      rnTraceConsensus :: Tracers m (ConnectionId addrNTN) (ConnectionId addrNTC) blk

      -- | Protocol tracers for node-to-node communication
    , rnTraceNTN :: NTN.Tracers m (ConnectionId addrNTN) blk DeserialiseFailure

      -- | Protocol tracers for node-to-client communication
    , rnTraceNTC :: NTC.Tracers m (ConnectionId addrNTC) blk DeserialiseFailure

      -- | Protocol info
    , rnProtocolInfo :: ProtocolInfo m blk

      -- | Hook called after the initialisation of the 'NodeKernel'
      --
      -- Called on the 'NodeKernel' after creating it, but before the network
      -- layer is initialised.
    , rnNodeKernelHook :: ResourceRegistry m
                       -> NodeKernel m (ConnectionId addrNTN) (ConnectionId addrNTC) blk
                       -> m ()

    }

-- | Arguments that usually only tests /directly/ specify.
--
-- A non-testing invocation probably wouldn't explicitly provide these values to
-- 'runWith'. The @cardano-node@, for example, instead calls the 'run'
-- abbreviation, which uses 'stdLowLevelRunNodeArgsIO' to indirectly specify
-- these low-level values from the higher-level 'StdRunNodeArgs'.
data LowLevelRunNodeArgs m addrNTN addrNTC versionDataNTN versionDataNTC blk = LowLevelRunNodeArgs {

      -- | How to manage the clean-shutdown marker on disk
      llrnWithCheckedDB :: forall a. (LastShutDownWasClean -> m a) -> m a

      -- | The " static " ChainDB arguments
    , llrnChainDbArgsDefaults :: ChainDbArgs Defaults m blk

      -- | Customise the 'ChainDbArgs'
    , llrnCustomiseChainDbArgs ::
           ChainDbArgs Identity m blk
        -> ChainDbArgs Identity m blk

      -- | Customise the 'NodeArgs'
    , llrnCustomiseNodeKernelArgs ::
           NodeKernelArgs m (ConnectionId addrNTN) (ConnectionId addrNTC) blk
        -> NodeKernelArgs m (ConnectionId addrNTN) (ConnectionId addrNTC) blk

      -- | Ie 'bfcSalt'
    , llrnBfcSalt :: Int

      -- | Ie 'keepAliveRng'
    , llrnKeepAliveRng :: StdGen

      -- | Customise the 'HardForkBlockchainTimeArgs'
    , llrnCustomiseHardForkBlockchainTimeArgs ::
           HardForkBlockchainTimeArgs m blk
        -> HardForkBlockchainTimeArgs m blk

      -- | See 'NTN.ChainSyncTimeout'
    , llrnChainSyncTimeout :: m NTN.ChainSyncTimeout

      -- | How to run the data diffusion applications
      --
      -- 'run' will not return before this does.
    , llrnRunDataDiffusion ::
           ResourceRegistry m
        -> DiffusionApplications
             addrNTN        addrNTC
             versionDataNTN versionDataNTC
             m
        -> m ()

    , llrnVersionDataNTC :: versionDataNTC

    , llrnVersionDataNTN :: versionDataNTN

      -- | node-to-node protocol versions to run.
    , llrnNodeToNodeVersions :: Map NodeToNodeVersion (BlockNodeToNodeVersion blk)

      -- | node-to-client protocol versions to run.
    , llrnNodeToClientVersions :: Map NodeToClientVersion (BlockNodeToClientVersion blk)

      -- | Maximum clock skew
    , llrnMaxClockSkew :: ClockSkew
    }

-- | Combination of 'runWith' and 'stdLowLevelRunArgsIO'
run :: forall blk.
     RunNode blk
  => RunNodeArgs IO RemoteAddress LocalAddress blk
  -> StdRunNodeArgs IO blk
  -> IO ()
run args stdArgs = stdLowLevelRunNodeArgsIO args stdArgs >>= runWith args

-- | Start a node.
--
-- This opens the 'ChainDB', sets up the 'NodeKernel' and initialises the
-- network layer.
--
-- This function runs forever unless an exception is thrown.
runWith :: forall m addrNTN addrNTC versionDataNTN versionDataNTC blk.
     ( RunNode blk
     , IOLike m, MonadTime m, MonadTimer m
     , Hashable addrNTN, Ord addrNTN, Typeable addrNTN
     )
  => RunNodeArgs m addrNTN addrNTC blk
  -> LowLevelRunNodeArgs m addrNTN addrNTC versionDataNTN versionDataNTC blk
  -> m ()
runWith RunNodeArgs{..} LowLevelRunNodeArgs{..} =

    llrnWithCheckedDB $ \(LastShutDownWasClean lastShutDownWasClean) ->
    withRegistry $ \registry -> do

      let systemStart :: SystemStart
          systemStart = getSystemStart (configBlock cfg)

          systemTime :: SystemTime m
          systemTime = defaultSystemTime
                         systemStart
                         (blockchainTimeTracer rnTraceConsensus)

          inFuture :: CheckInFuture m blk
          inFuture = InFuture.reference
                       (configLedger cfg)
                       llrnMaxClockSkew
                       systemTime

      let customiseChainDbArgs' args
            | lastShutDownWasClean
            = llrnCustomiseChainDbArgs args
            | otherwise
              -- When the last shutdown was not clean, validate the complete
              -- ChainDB to detect and recover from any corruptions. This will
              -- override the default value /and/ the user-customised value of
              -- the 'ChainDB.cdbImmValidation' and the
              -- 'ChainDB.cdbVolValidation' fields.
            = (llrnCustomiseChainDbArgs args) {
                  ChainDB.cdbImmutableDbValidation = ValidateAllChunks
                , ChainDB.cdbVolatileDbValidation  = ValidateAll
                }

      (_, chainDB) <- allocate registry
        (\_ -> openChainDB
          registry inFuture cfg initLedger
          llrnChainDbArgsDefaults customiseChainDbArgs')
        ChainDB.closeDB

      btime <-
        hardForkBlockchainTime $
        llrnCustomiseHardForkBlockchainTimeArgs $
        HardForkBlockchainTimeArgs
          { hfbtBackoffDelay   = pure $ BackoffDelay 60
          , hfbtGetLedgerState =
              ledgerState <$> ChainDB.getCurrentLedger chainDB
          , hfbtLedgerConfig   = configLedger cfg
          , hfbtRegistry       = registry
          , hfbtSystemTime     = systemTime
          , hfbtTracer         =
              contramap (fmap (fromRelativeTime systemStart))
                (blockchainTimeTracer rnTraceConsensus)
          , hfbtMaxClockRewind = secondsToNominalDiffTime 20
          }

      nodeKernelArgs <-
          fmap (nodeKernelArgsEnforceInvariants . llrnCustomiseNodeKernelArgs) $
          mkNodeKernelArgs
            registry
            llrnBfcSalt
            llrnKeepAliveRng
            cfg
            blockForging
            rnTraceConsensus
            btime
            chainDB
      nodeKernel <- initNodeKernel nodeKernelArgs
      rnNodeKernelHook registry nodeKernel

      let ntnApps = mkNodeToNodeApps   nodeKernelArgs nodeKernel
          ntcApps = mkNodeToClientApps nodeKernelArgs nodeKernel
          diffusionApplications = mkDiffusionApplications
                                    (miniProtocolParameters nodeKernelArgs)
                                    ntnApps
                                    ntcApps
                                    nodeKernel

      llrnRunDataDiffusion registry diffusionApplications
  where
    ProtocolInfo
      { pInfoConfig       = cfg
      , pInfoInitLedger   = initLedger
      , pInfoBlockForging = blockForging
      } = rnProtocolInfo

    codecConfig :: CodecConfig blk
    codecConfig = configCodec cfg

    mkNodeToNodeApps
      :: NodeKernelArgs m (ConnectionId addrNTN) (ConnectionId addrNTC) blk
      -> NodeKernel     m (ConnectionId addrNTN) (ConnectionId addrNTC) blk
      -> BlockNodeToNodeVersion blk
      -> NTN.Apps m (ConnectionId addrNTN) ByteString ByteString ByteString ByteString ByteString ()
    mkNodeToNodeApps nodeKernelArgs nodeKernel version =
        NTN.mkApps
          nodeKernel
          rnTraceNTN
          (NTN.defaultCodecs codecConfig version)
          llrnChainSyncTimeout
          (NTN.mkHandlers nodeKernelArgs nodeKernel)

    mkNodeToClientApps
      :: NodeKernelArgs m (ConnectionId addrNTN) (ConnectionId addrNTC) blk
      -> NodeKernel     m (ConnectionId addrNTN) (ConnectionId addrNTC) blk
      -> BlockNodeToClientVersion blk
      -> NodeToClientVersion
      -> NTC.Apps m (ConnectionId addrNTC) ByteString ByteString ByteString ()
    mkNodeToClientApps nodeKernelArgs nodeKernel blockVersion networkVersion =
        NTC.mkApps
          nodeKernel
          rnTraceNTC
          (NTC.defaultCodecs codecConfig blockVersion networkVersion)
          (NTC.mkHandlers nodeKernelArgs nodeKernel)

    mkDiffusionApplications
      :: MiniProtocolParameters
      -> (   BlockNodeToNodeVersion blk
          -> NTN.Apps m (ConnectionId addrNTN) ByteString ByteString ByteString ByteString ByteString ()
         )
      -> (   BlockNodeToClientVersion blk
          -> NodeToClientVersion
          -> NTC.Apps m (ConnectionId addrNTC) ByteString ByteString ByteString ()
         )
      -> NodeKernel m (ConnectionId addrNTN) (ConnectionId addrNTC) blk
      -> DiffusionApplications
           addrNTN addrNTC
           versionDataNTN versionDataNTC
           m
    mkDiffusionApplications miniProtocolParams ntnApps ntcApps kernel =
      DiffusionApplications {
          daResponderApplication = combineVersions [
              simpleSingletonVersions
                version
                llrnVersionDataNTN
                (NTN.responder miniProtocolParams version $ ntnApps blockVersion)
            | (version, blockVersion) <- Map.toList llrnNodeToNodeVersions
            ]
        , daInitiatorApplication = combineVersions [
              simpleSingletonVersions
                version
                llrnVersionDataNTN
                (NTN.initiator miniProtocolParams version $ ntnApps blockVersion)
            | (version, blockVersion) <- Map.toList llrnNodeToNodeVersions
            ]
        , daLocalResponderApplication = combineVersions [
              simpleSingletonVersions
                version
                llrnVersionDataNTC
                (NTC.responder version $ ntcApps blockVersion version)
            | (version, blockVersion) <- Map.toList llrnNodeToClientVersions
            ]
        , daErrorPolicies = consensusErrorPolicy (Proxy @blk)
        , daLedgerPeersCtx = LedgerPeersConsensusInterface (getPeersFromCurrentLedgerAfterSlot kernel)
        }

-- | Did the ChainDB already have existing clean-shutdown marker on disk?
newtype LastShutDownWasClean = LastShutDownWasClean Bool
  deriving (Eq, Show)

-- | Check the DB marker, lock the DB and look for the clean shutdown marker.
--
-- Run the body action with the DB locked, and if the last shutdown was clean.
--
stdWithCheckedDB ::
     forall blk a. (StandardHash blk, Typeable blk)
  => Proxy blk
  -> FilePath
  -> NetworkMagic
  -> (LastShutDownWasClean -> IO a)  -- ^ Body action with last shutdown was clean.
  -> IO a
stdWithCheckedDB pb databasePath networkMagic body = do

    -- Check the DB marker first, before doing the lock file, since if the
    -- marker is not present, it expects an empty DB dir.
    either throwIO return =<< checkDbMarker
      hasFS
      mountPoint
      networkMagic

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
      createMarkerOnCleanShutdown pb hasFS $
        body (LastShutDownWasClean lastShutDownWasClean)
  where
    mountPoint = MountPoint databasePath
    hasFS      = ioHasFS mountPoint

openChainDB
  :: forall m blk. (RunNode blk, IOLike m)
  => ResourceRegistry m
  -> CheckInFuture m blk
  -> TopLevelConfig blk
  -> ExtLedgerState blk
     -- ^ Initial ledger
  -> ChainDbArgs Defaults m blk
  -> (ChainDbArgs Identity m blk -> ChainDbArgs Identity m blk)
      -- ^ Customise the 'ChainDbArgs'
  -> m (ChainDB m blk)
openChainDB registry inFuture cfg initLedger defArgs customiseArgs =
    ChainDB.openDB args
  where
    args :: ChainDbArgs Identity m blk
    args = customiseArgs $
             mkChainDbArgs registry inFuture cfg initLedger
             (nodeImmutableDbChunkInfo (configStorage cfg))
             defArgs

mkChainDbArgs
  :: forall m blk. (RunNode blk, IOLike m)
  => ResourceRegistry m
  -> CheckInFuture m blk
  -> TopLevelConfig blk
  -> ExtLedgerState blk
     -- ^ Initial ledger
  -> ChunkInfo
  -> ChainDbArgs Defaults m blk
  -> ChainDbArgs Identity m blk
mkChainDbArgs
  registry
  inFuture
  cfg
  initLedger
  chunkInfo
  defArgs
  = defArgs {
      ChainDB.cdbTopLevelConfig = cfg
    , ChainDB.cdbChunkInfo      = chunkInfo
    , ChainDB.cdbCheckIntegrity = nodeCheckIntegrity (configStorage cfg)
    , ChainDB.cdbGenesis        = return initLedger
    , ChainDB.cdbCheckInFuture  = inFuture

    , ChainDB.cdbRegistry       = registry
    }

mkNodeKernelArgs
  :: forall m addrNTN addrNTC blk. (RunNode blk, IOLike m)
  => ResourceRegistry m
  -> Int
  -> StdGen
  -> TopLevelConfig blk
  -> m [BlockForging m blk]
  -> Tracers m (ConnectionId addrNTN) (ConnectionId addrNTC) blk
  -> BlockchainTime m
  -> ChainDB m blk
  -> m (NodeKernelArgs m (ConnectionId addrNTN) (ConnectionId addrNTC) blk)
mkNodeKernelArgs
  registry
  bfcSalt
  keepAliveRng
  cfg
  initBlockForging
  tracers
  btime
  chainDB
  = do
    blockForging <- initBlockForging
    return NodeKernelArgs
      { tracers
      , registry
      , cfg
      , btime
      , chainDB
      , blockForging
      , initChainDB             = nodeInitChainDB
      , blockFetchSize          = estimateBlockSize
      , maxTxCapacityOverride   = NoMaxTxCapacityOverride
      , mempoolCapacityOverride = NoMempoolCapacityBytesOverride
      , miniProtocolParameters  = defaultMiniProtocolParameters
      , blockFetchConfiguration = defaultBlockFetchConfiguration
      , keepAliveRng
      }
  where
    defaultBlockFetchConfiguration :: BlockFetchConfiguration
    defaultBlockFetchConfiguration = BlockFetchConfiguration
      { bfcMaxConcurrencyBulkSync = 1
      , bfcMaxConcurrencyDeadline = 1
      , bfcMaxRequestsInflight    = blockFetchPipeliningMax defaultMiniProtocolParameters
      , bfcDecisionLoopInterval   = 0.01 -- 10ms
      , bfcSalt
      }

-- | We allow the user running the node to customise the 'NodeKernelArgs'
-- through 'llrnCustomiseNodeKernelArgs', but there are some limits to some
-- values. This function makes sure we don't exceed those limits and that the
-- values are consistent.
nodeKernelArgsEnforceInvariants
  :: NodeKernelArgs m (ConnectionId addrNTN) (ConnectionId addrNTC) blk
  -> NodeKernelArgs m (ConnectionId addrNTN) (ConnectionId addrNTC) blk
nodeKernelArgsEnforceInvariants nodeKernelArgs = nodeKernelArgs
    { miniProtocolParameters = miniProtocolParameters
        -- If 'blockFetchPipeliningMax' exceeds the configured default, it
        -- would be a protocol violation.
        { blockFetchPipeliningMax =
            min (blockFetchPipeliningMax miniProtocolParameters)
                (blockFetchPipeliningMax defaultMiniProtocolParameters)
        }
    , blockFetchConfiguration = blockFetchConfiguration
        -- 'bfcMaxRequestsInflight' must be <= 'blockFetchPipeliningMax'
        { bfcMaxRequestsInflight =
            min (bfcMaxRequestsInflight blockFetchConfiguration)
                (blockFetchPipeliningMax miniProtocolParameters)
        }
    }
  where
    NodeKernelArgs{..} = nodeKernelArgs

{-------------------------------------------------------------------------------
  Arguments for use in the real node
-------------------------------------------------------------------------------}

-- | How to locate the ChainDB on disk
stdMkChainDbHasFS ::
     FilePath
  -> ChainDB.RelativeMountPoint
  -> SomeHasFS IO
stdMkChainDbHasFS rootPath (ChainDB.RelativeMountPoint relPath) =
    SomeHasFS $ ioHasFS $ MountPoint $ rootPath </> relPath

stdBfcSaltIO :: IO Int
stdBfcSaltIO = randomIO

stdKeepAliveRngIO :: IO StdGen
stdKeepAliveRngIO = newStdGen

stdChainSyncTimeout :: IO NTN.ChainSyncTimeout
stdChainSyncTimeout = do
    -- These values approximately correspond to false positive
    -- thresholds for streaks of empty slots with 99% probability,
    -- 99.9% probability up to 99.999% probability.
    -- t = T_s [log (1-Y) / log (1-f)]
    -- Y = [0.99, 0.999...]
    -- T_s = slot length of 1s.
    -- f = 0.05
    -- The timeout is randomly picked per bearer to avoid all bearers
    -- going down at the same time in case of a long streak of empty
    -- slots. TODO: workaround until peer selection governor.
    mustReplyTimeout <- Just <$> randomElem [90, 135, 180, 224, 269]
    return NTN.ChainSyncTimeout
      { canAwaitTimeout  = shortWait
      , intersectTimeout = shortWait
      , mustReplyTimeout
      }
  where
    randomElem xs = do
      ix <- randomRIO (0, length xs - 1)
      return $ xs !! ix

stdVersionDataNTN :: NetworkMagic -> DiffusionMode -> NodeToNodeVersionData
stdVersionDataNTN networkMagic diffusionMode = NodeToNodeVersionData
    { networkMagic
    , diffusionMode
    }

stdVersionDataNTC :: NetworkMagic -> NodeToClientVersionData
stdVersionDataNTC networkMagic = NodeToClientVersionData
    { networkMagic
    }

stdRunDataDiffusion ::
     DiffusionTracers
  -> DiffusionArguments
  -> DiffusionApplications
       RemoteAddress LocalAddress
       NodeToNodeVersionData NodeToClientVersionData
       IO
  -> IO ()
stdRunDataDiffusion = runDataDiffusion

-- | Higher-level arguments that can determine the 'LowLevelRunNodeArgs' under
-- some usual assumptions for realistic use cases such as in @cardano-node@.
--
-- See 'stdLowLevelRunNodeArgsIO'.
data StdRunNodeArgs m blk = StdRunNodeArgs
  { srnBfcMaxConcurrencyBulkSync   :: Maybe Word
  , srnBfcMaxConcurrencyDeadline   :: Maybe Word
  , srnChainDbValidateOverride     :: Bool
    -- ^ If @True@, validate the ChainDB on init no matter what
  , srnSnapshotInterval            :: SnapshotInterval
  , srnDatabasePath                :: FilePath
    -- ^ Location of the DBs
  , srnDiffusionArguments          :: DiffusionArguments
  , srnDiffusionTracers            :: DiffusionTracers
  , srnEnableInDevelopmentVersions :: Bool
    -- ^ If @False@, then the node will limit the negotiated NTN and NTC
    -- versions to the latest " official " release (as chosen by Network and
    -- Consensus Team, with input from Node Team)
  , srnTraceChainDB                :: Tracer m (ChainDB.TraceEvent blk)
    -- ^ ChainDB Tracer
  , srnMaxTxCapacityOverride       :: MaxTxCapacityOverride blk
  }

-- | Conveniently packaged 'LowLevelRunNodeArgs' arguments from a standard
-- non-testing invocation.
stdLowLevelRunNodeArgsIO ::
     forall blk. RunNode blk
  => RunNodeArgs IO RemoteAddress LocalAddress blk
  -> StdRunNodeArgs IO blk
  -> IO (LowLevelRunNodeArgs
          IO
          RemoteAddress
          LocalAddress
          NodeToNodeVersionData
          NodeToClientVersionData
          blk)
stdLowLevelRunNodeArgsIO RunNodeArgs{ rnProtocolInfo } StdRunNodeArgs{..} = do
    llrnBfcSalt      <- stdBfcSaltIO
    llrnKeepAliveRng <- stdKeepAliveRngIO
    pure LowLevelRunNodeArgs
      { llrnBfcSalt
      , llrnChainSyncTimeout = stdChainSyncTimeout
      , llrnCustomiseHardForkBlockchainTimeArgs = id
      , llrnKeepAliveRng
      , llrnChainDbArgsDefaults =
          updateChainDbDefaults $ ChainDB.defaultArgs mkHasFS diskPolicy
      , llrnCustomiseChainDbArgs = id
      , llrnCustomiseNodeKernelArgs
      , llrnRunDataDiffusion =
          \_reg apps ->
            stdRunDataDiffusion srnDiffusionTracers srnDiffusionArguments apps
      , llrnVersionDataNTC =
          stdVersionDataNTC networkMagic
      , llrnVersionDataNTN =
          stdVersionDataNTN
            networkMagic
            (daDiffusionMode srnDiffusionArguments)
      , llrnNodeToNodeVersions =
          limitToLatestReleasedVersion
            fst
            (supportedNodeToNodeVersions (Proxy @blk))
      , llrnNodeToClientVersions =
          limitToLatestReleasedVersion
            snd
            (supportedNodeToClientVersions (Proxy @blk))
      , llrnWithCheckedDB =
          stdWithCheckedDB (Proxy @blk) srnDatabasePath networkMagic
      , llrnMaxClockSkew =
          InFuture.defaultClockSkew
      }
  where
    diskPolicy =
      let
        cfg = pInfoConfig rnProtocolInfo
        k   = configSecurityParam cfg
      in defaultDiskPolicy k srnSnapshotInterval

    mkHasFS :: ChainDB.RelativeMountPoint -> SomeHasFS IO
    mkHasFS = stdMkChainDbHasFS srnDatabasePath

    networkMagic :: NetworkMagic
    networkMagic = getNetworkMagic $ configBlock $ pInfoConfig rnProtocolInfo

    updateChainDbDefaults ::
         ChainDbArgs Defaults IO blk
      -> ChainDbArgs Defaults IO blk
    updateChainDbDefaults =
        (\x -> x { ChainDB.cdbTracer = srnTraceChainDB }) .
        (if not srnChainDbValidateOverride then id else \x -> x
          { ChainDB.cdbImmutableDbValidation = ValidateAllChunks
          , ChainDB.cdbVolatileDbValidation  = ValidateAll
          })

    llrnCustomiseNodeKernelArgs ::
         NodeKernelArgs m (ConnectionId addrNTN) (ConnectionId addrNTC) blk
      -> NodeKernelArgs m (ConnectionId addrNTN) (ConnectionId addrNTC) blk
    llrnCustomiseNodeKernelArgs =
        overrideMaxTxCapacityOverride
      . (overBlockFetchConfiguration modifyBlockFetchConfiguration)
      where
        overrideMaxTxCapacityOverride args = args {
              maxTxCapacityOverride = srnMaxTxCapacityOverride
            }
        modifyBlockFetchConfiguration =
            maybe id
              (\mc bfc -> bfc { bfcMaxConcurrencyDeadline = mc })
              srnBfcMaxConcurrencyDeadline
          . maybe id
              (\mc bfc -> bfc { bfcMaxConcurrencyBulkSync = mc })
              srnBfcMaxConcurrencyBulkSync

    -- Limit the node version unless srnEnableInDevelopmentVersions is set
    limitToLatestReleasedVersion :: forall k v.
         Ord k
      => ((Maybe NodeToNodeVersion, Maybe NodeToClientVersion) -> Maybe k)
      -> Map k v
      -> Map k v
    limitToLatestReleasedVersion prj =
        if srnEnableInDevelopmentVersions then id
        else
        case prj $ latestReleasedNodeVersion (Proxy @blk) of
          Nothing      -> id
          Just version -> Map.takeWhileAntitone (<= version)

{-------------------------------------------------------------------------------
  Miscellany
-------------------------------------------------------------------------------}

overBlockFetchConfiguration ::
     (BlockFetchConfiguration -> BlockFetchConfiguration)
  -> NodeKernelArgs m (ConnectionId addrNTN) (ConnectionId addrNTC) blk
  -> NodeKernelArgs m (ConnectionId addrNTN) (ConnectionId addrNTC) blk
overBlockFetchConfiguration f args = args {
      blockFetchConfiguration = f blockFetchConfiguration
    }
  where
    NodeKernelArgs { blockFetchConfiguration } = args

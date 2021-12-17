{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
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
    -- ** P2P Switch
  , NetworkP2PMode (..)
    -- * Exposed by 'run' et al
  , ChainDB.RelativeMountPoint (..)
  , ChainDB.TraceEvent (..)
  , ChainDbArgs (..)
  , HardForkBlockchainTimeArgs (..)
  , LastShutDownWasClean (..)
  , LowLevelRunNodeArgs (..)
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
import qualified Ouroboros.Network.Diffusion as Diffusion
import qualified Ouroboros.Network.Diffusion.NonP2P as NonP2P
import qualified Ouroboros.Network.Diffusion.P2P as P2P
import           Ouroboros.Network.Magic
import           Ouroboros.Network.NodeToClient (ConnectionId, LocalAddress,
                     LocalSocket, NodeToClientVersionData (..), combineVersions,
                     simpleSingletonVersions)
import           Ouroboros.Network.NodeToNode (DiffusionMode,
                     MiniProtocolParameters, NodeToNodeVersionData (..),
                     RemoteAddress, Socket, blockFetchPipeliningMax,
                     defaultMiniProtocolParameters)
import           Ouroboros.Network.PeerSelection.LedgerPeers
                     (LedgerPeersConsensusInterface (..))
import           Ouroboros.Network.PeerSelection.PeerMetric (PeerMetrics (..),
                     newPeerMetric, reportMetric)
import           Ouroboros.Network.Protocol.Limits (shortWait)
import           Ouroboros.Network.RethrowPolicy

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
import           Ouroboros.Consensus.Node.RethrowPolicy
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

{-------------------------------------------------------------------------------
  The arguments to the Consensus Layer node functionality
-------------------------------------------------------------------------------}

-- How to add a new argument
--
-- 1) As a Consensus Layer maintainer, use your judgement to determine whether
-- the new argument belongs in 'RunNodeArgs' or 'LowLevelArgs'. Give it the type
-- that seems most " natural ", future-proof, and useful for the whole range of
-- invocations: our tests, our own benchmarks, deployment on @mainnet@, etc. The
-- major litmus test is: it only belongs in 'RunNodeArgs' if /every/ invocation
-- of our node code must specify it.
--
-- 2) If you add it to 'LowLevelArgs', you'll have type errors in
-- 'stdLowLevelRunNodeArgsIO'. To fix them, you'll need to either hard-code a
-- default value or else extend 'StdRunNodeArgs' with a new sufficient field.
--
-- 3) When extending either 'RunNodeArgs' or 'StdRunNodeArgs', the
-- @cardano-node@ will have to be updated, so consider the Node Team's
-- preferences when choosing the new field's type. As an oversimplification,
-- Consensus /owns/ 'RunNodeArgs' while Node /owns/ 'StdRunNodeArgs', but it's
-- always worth spending some effort to try to find a type that satisfies both
-- teams.

-- | Arguments expected from any invocation of 'runWith', whether by deployed
-- code, tests, etc.
data RunNodeArgs m addrNTN addrNTC blk (p2p :: Diffusion.P2P) = RunNodeArgs {
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

      -- | Network P2P Mode switch
    , rnEnableP2P :: NetworkP2PMode p2p
    }

-- | Arguments that usually only tests /directly/ specify.
--
-- A non-testing invocation probably wouldn't explicitly provide these values to
-- 'runWith'. The @cardano-node@, for example, instead calls the 'run'
-- abbreviation, which uses 'stdLowLevelRunNodeArgsIO' to indirectly specify
-- these low-level values from the higher-level 'StdRunNodeArgs'.
data LowLevelRunNodeArgs m addrNTN addrNTC versionDataNTN versionDataNTC blk
                         (p2p :: Diffusion.P2P) =
   LowLevelRunNodeArgs {

      -- | An action that will receive a marker indicating whether the previous
      -- shutdown was considered clean and a wrapper for installing a handler to
      -- create a clean file on exit if needed. See
      -- 'Ouroboros.Consensus.Node.Recovery.runWithCheckedDB'.
      llrnWithCheckedDB :: forall a. (  LastShutDownWasClean
                                     -> (ChainDB m blk -> m a -> m a)
                                     -> m a)
                        -> m a

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
        -> Diffusion.Applications
             addrNTN NodeToNodeVersion   versionDataNTN
             addrNTC NodeToClientVersion versionDataNTC
             m
        -> Diffusion.ExtraApplications p2p addrNTN m
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

{-------------------------------------------------------------------------------
  Entrypoints to the Consensus Layer node functionality
-------------------------------------------------------------------------------}

-- | P2P Switch
--
data NetworkP2PMode (p2p :: Diffusion.P2P) where
    EnabledP2PMode  :: NetworkP2PMode 'Diffusion.P2P
    DisabledP2PMode :: NetworkP2PMode 'Diffusion.NonP2P

deriving instance Eq   (NetworkP2PMode p2p)
deriving instance Show (NetworkP2PMode p2p)


-- | Combination of 'runWith' and 'stdLowLevelRunArgsIO'
run :: forall blk p2p.
     RunNode blk
  => RunNodeArgs IO RemoteAddress LocalAddress blk p2p
  -> StdRunNodeArgs IO blk p2p
  -> IO ()
run args stdArgs = stdLowLevelRunNodeArgsIO args stdArgs >>= runWith args

-- | Start a node.
--
-- This opens the 'ChainDB', sets up the 'NodeKernel' and initialises the
-- network layer.
--
-- This function runs forever unless an exception is thrown.
runWith :: forall m addrNTN addrNTC versionDataNTN versionDataNTC blk p2p.
     ( RunNode blk
     , IOLike m, MonadTime m, MonadTimer m
     , Hashable addrNTN, Ord addrNTN, Typeable addrNTN
     )
  => RunNodeArgs m addrNTN addrNTC blk p2p
  -> LowLevelRunNodeArgs m addrNTN addrNTC versionDataNTN versionDataNTC blk p2p
  -> m ()
runWith RunNodeArgs{..} LowLevelRunNodeArgs{..} =

    llrnWithCheckedDB $ \(LastShutDownWasClean lastShutDownWasClean) continueWithCleanChainDB ->
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

      chainDB <- openChainDB registry inFuture cfg initLedger
                llrnChainDbArgsDefaults customiseChainDbArgs'

      continueWithCleanChainDB chainDB $ do
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

        peerMetrics <- newPeerMetric
        let ntnApps = mkNodeToNodeApps   nodeKernelArgs nodeKernel peerMetrics
            ntcApps = mkNodeToClientApps nodeKernelArgs nodeKernel
            (apps, appsExtra) = mkDiffusionApplications
                                      rnEnableP2P
                                      (miniProtocolParameters nodeKernelArgs)
                                      ntnApps
                                      ntcApps
                                      nodeKernel
                                      peerMetrics

        llrnRunDataDiffusion registry apps appsExtra
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
      -> PeerMetrics m addrNTN
      -> BlockNodeToNodeVersion blk
      -> NTN.Apps m
          (ConnectionId addrNTN)
          ByteString
          ByteString
          ByteString
          ByteString
          ByteString
          ()
    mkNodeToNodeApps nodeKernelArgs nodeKernel peerMetrics version =
        NTN.mkApps
          nodeKernel
          rnTraceNTN
          (NTN.defaultCodecs codecConfig version)
          NTN.byteLimits
          llrnChainSyncTimeout
          (reportMetric peerMetrics)
          (NTN.mkHandlers nodeKernelArgs nodeKernel)

    mkNodeToClientApps
      :: NodeKernelArgs m (ConnectionId addrNTN) (ConnectionId addrNTC) blk
      -> NodeKernel     m (ConnectionId addrNTN) (ConnectionId addrNTC) blk
      -> BlockNodeToClientVersion blk
      -> NodeToClientVersion
      -> NTC.Apps m (ConnectionId addrNTC) ByteString ByteString ByteString ByteString ()
    mkNodeToClientApps nodeKernelArgs nodeKernel blockVersion networkVersion =
        NTC.mkApps
          nodeKernel
          rnTraceNTC
          (NTC.defaultCodecs codecConfig blockVersion networkVersion)
          (NTC.mkHandlers nodeKernelArgs nodeKernel)

    mkDiffusionApplications
      :: NetworkP2PMode p2p
      -> MiniProtocolParameters
      -> (   BlockNodeToNodeVersion blk
          -> NTN.Apps
               m
               (ConnectionId addrNTN)
               ByteString
               ByteString
               ByteString
               ByteString
               ByteString
               ()
        )
      -> (   BlockNodeToClientVersion blk
          -> NodeToClientVersion
          -> NTC.Apps
               m (ConnectionId addrNTC) ByteString ByteString ByteString ByteString ()
        )
      -> NodeKernel m remotePeer localPeer blk
      -> PeerMetrics m addrNTN
      -> ( Diffusion.Applications
             addrNTN NodeToNodeVersion   versionDataNTN
             addrNTC NodeToClientVersion versionDataNTC
             m
         , Diffusion.ExtraApplications p2p addrNTN m
         )
    mkDiffusionApplications
      enP2P
      miniProtocolParams
      ntnApps
      ntcApps
      kernel
      peerMetrics =
      case enP2P of
        EnabledP2PMode ->
          ( apps
          , Diffusion.P2PApplications
              P2P.ApplicationsExtra {
                P2P.daMiniProtocolParameters = miniProtocolParams,
                P2P.daRethrowPolicy          = consensusRethrowPolicy (Proxy @blk),
                P2P.daLocalRethrowPolicy     = localRethrowPolicy,
                P2P.daPeerMetrics            = peerMetrics,
                P2P.daBlockFetchMode         = getFetchMode kernel
              }
          )
        DisabledP2PMode ->
          ( apps
          , Diffusion.NonP2PApplications
              NonP2P.ApplicationsExtra {
                NonP2P.daErrorPolicies = consensusErrorPolicy (Proxy @blk)
              }
          )
      where
        apps = Diffusion.Applications {
            Diffusion.daApplicationInitiatorMode =
              combineVersions
                [ simpleSingletonVersions
                    version
                    llrnVersionDataNTN
                    (NTN.initiator miniProtocolParams version
                      $ ntnApps blockVersion)
                | (version, blockVersion) <- Map.toList llrnNodeToNodeVersions
                ],
            Diffusion.daApplicationInitiatorResponderMode =
              combineVersions
                [ simpleSingletonVersions
                    version
                    llrnVersionDataNTN
                    (NTN.initiatorAndResponder miniProtocolParams version
                      $ ntnApps blockVersion)
                | (version, blockVersion) <- Map.toList llrnNodeToNodeVersions
                ],
            Diffusion.daLocalResponderApplication =
              combineVersions
                [ simpleSingletonVersions
                    version
                    llrnVersionDataNTC
                    (NTC.responder version $ ntcApps blockVersion version)
                | (version, blockVersion) <- Map.toList llrnNodeToClientVersions
                ],
            Diffusion.daLedgerPeersCtx =
              LedgerPeersConsensusInterface
                (getPeersFromCurrentLedgerAfterSlot kernel)
          }

        localRethrowPolicy :: RethrowPolicy
        localRethrowPolicy = mempty

-- | Check the DB marker, lock the DB and look for the clean shutdown marker.
--
-- Run the body action with the DB locked.
--
stdWithCheckedDB ::
     forall blk a. (StandardHash blk, Typeable blk)
  => Proxy blk
  -> FilePath
  -> NetworkMagic
  -> (LastShutDownWasClean -> (ChainDB IO blk -> IO a -> IO a) -> IO a)  -- ^ Body action with last shutdown was clean.
  -> IO a
stdWithCheckedDB pb databasePath networkMagic body = do

    -- Check the DB marker first, before doing the lock file, since if the
    -- marker is not present, it expects an empty DB dir.
    either throwIO return =<< checkDbMarker
      hasFS
      mountPoint
      networkMagic

    -- Then create the lock file.
    withLockDB mountPoint $ runWithCheckedDB pb hasFS body
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
     Diffusion.Tracers
       RemoteAddress  NodeToNodeVersion
       LocalAddress   NodeToClientVersion
       IO
  -> Diffusion.ExtraTracers p2p
  -> Diffusion.Arguments
       Socket      RemoteAddress
       LocalSocket LocalAddress
  -> Diffusion.ExtraArguments p2p IO
  -> Diffusion.Applications
       RemoteAddress  NodeToNodeVersion   NodeToNodeVersionData
       LocalAddress   NodeToClientVersion NodeToClientVersionData
       IO
  -> Diffusion.ExtraApplications p2p RemoteAddress IO
  -> IO ()
stdRunDataDiffusion = Diffusion.run

-- | Higher-level arguments that can determine the 'LowLevelRunNodeArgs' under
-- some usual assumptions for realistic use cases such as in @cardano-node@.
--
-- See 'stdLowLevelRunNodeArgsIO'.
data StdRunNodeArgs m blk (p2p :: Diffusion.P2P) = StdRunNodeArgs
  { srnBfcMaxConcurrencyBulkSync    :: Maybe Word
  , srnBfcMaxConcurrencyDeadline    :: Maybe Word
  , srnChainDbValidateOverride      :: Bool
    -- ^ If @True@, validate the ChainDB on init no matter what
  , srnSnapshotInterval             :: SnapshotInterval
  , srnDatabasePath                 :: FilePath
    -- ^ Location of the DBs
  , srnDiffusionArguments           :: Diffusion.Arguments
                                         Socket      RemoteAddress
                                         LocalSocket LocalAddress
  , srnDiffusionArgumentsExtra      :: Diffusion.ExtraArguments p2p m
  , srnDiffusionTracers             :: Diffusion.Tracers
                                         RemoteAddress  NodeToNodeVersion
                                         LocalAddress   NodeToClientVersion
                                         IO
  , srnDiffusionTracersExtra        :: Diffusion.ExtraTracers p2p
  , srnEnableInDevelopmentVersions  :: Bool
    -- ^ If @False@, then the node will limit the negotiated NTN and NTC
    -- versions to the latest " official " release (as chosen by Network and
    -- Consensus Team, with input from Node Team)
  , srnTraceChainDB                 :: Tracer m (ChainDB.TraceEvent blk)
  , srnMaybeMempoolCapacityOverride :: Maybe MempoolCapacityBytesOverride
    -- ^ Determine whether to use the system default mempool capacity or explicitly set
    -- capacity of the mempool.
  }

-- | Conveniently packaged 'LowLevelRunNodeArgs' arguments from a standard
-- non-testing invocation.
stdLowLevelRunNodeArgsIO ::
     forall blk p2p. RunNode blk
  => RunNodeArgs IO RemoteAddress LocalAddress blk p2p
  -> StdRunNodeArgs IO blk p2p
  -> IO (LowLevelRunNodeArgs
          IO
          RemoteAddress
          LocalAddress
          NodeToNodeVersionData
          NodeToClientVersionData
          blk
          p2p)
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
          \_reg apps extraApps ->
            stdRunDataDiffusion srnDiffusionTracers
                                srnDiffusionTracersExtra
                                srnDiffusionArguments
                                srnDiffusionArgumentsExtra
                                apps extraApps
      , llrnVersionDataNTC =
          stdVersionDataNTC networkMagic
      , llrnVersionDataNTN =
          stdVersionDataNTN
            networkMagic
            (Diffusion.daMode srnDiffusionArguments)
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
        overBlockFetchConfiguration modifyBlockFetchConfiguration
      . modifyMempoolCapacityOverride
      where
        modifyBlockFetchConfiguration =
            maybe id
              (\mc bfc -> bfc { bfcMaxConcurrencyDeadline = mc })
              srnBfcMaxConcurrencyDeadline
          . maybe id
              (\mc bfc -> bfc { bfcMaxConcurrencyBulkSync = mc })
              srnBfcMaxConcurrencyBulkSync
        modifyMempoolCapacityOverride =
            maybe id
              (\mc nka -> nka { mempoolCapacityOverride = mc })
              srnMaybeMempoolCapacityOverride

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

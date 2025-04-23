{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE NamedFieldPuns    #-}

module DMQ.Configuration where

import Control.Concurrent.Class.MonadSTM (MonadSTM (..))
import Control.Exception (Exception (..), IOException, try)
import Control.Monad (forM)
import Control.Monad.Class.MonadTime.SI (DiffTime)
import Data.Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Data.Text qualified as Text
import DMQ.Configuration.CLIOptions (CLIOptions (..))
import DMQ.Configuration.Topology (readPeerSnapshotFileOrError)
import GHC.Generics (Generic)
import Network.Socket (AddrInfo (..), AddrInfoFlag (..), SockAddr,
           SocketType (..), defaultHints, getAddrInfo)

import Ouroboros.Network.Diffusion.Configuration
           (defaultAcceptedConnectionsLimit, defaultDeadlineChurnInterval,
           defaultDeadlineTargets, defaultProtocolIdleTimeout,
           defaultTimeWaitTimeout)
import Ouroboros.Network.Diffusion.Topology (NetworkTopology (..),
           producerAddresses)
import Ouroboros.Network.Diffusion.Types qualified as Diffusion
import Ouroboros.Network.Magic (NetworkMagic (..))
import Ouroboros.Network.NodeToNode (DiffusionMode (..))
import Ouroboros.Network.OrphanInstances ()
import Ouroboros.Network.PeerSelection.Governor.Types
           (PeerSelectionTargets (..), makePublicPeerSelectionStateVar)
import Ouroboros.Network.PeerSelection.LedgerPeers.Type
           (LedgerPeerSnapshot (..))
import Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing (..))
import Ouroboros.Network.Server.RateLimiting (AcceptedConnectionsLimit (..))

data Configuration ntnFd ntnAddr ntcFd ntcAddr =
  Configuration {
    dmqcAcceptedConnectionsLimit          :: AcceptedConnectionsLimit
  , dmqcDiffusionMode                     :: DiffusionMode
  , dmqcTargetOfRootPeers                 :: Int
  , dmqcTargetOfKnownPeers                :: Int
  , dmqcTargetOfEstablishedPeers          :: Int
  , dmqcTargetOfActivePeers               :: Int
  , dmqcTargetOfKnownBigLedgerPeers       :: Int
  , dmqcTargetOfEstablishedBigLedgerPeers :: Int
  , dmqcTargetOfActiveBigLedgerPeers      :: Int
  , dmqcProtocolIdleTimeout               :: DiffTime
  , dmqcChurnInterval                     :: DiffTime
  , dmqcPeerSharing                       :: PeerSharing
  , dmqcNetworkMagic                      :: NetworkMagic
  }
  deriving Generic

instance FromJSON (Configuration ntnFd ntnAddr ntcFd ntcAddr) where
  parseJSON = withObject "DMQConfiguration" $ \v -> do
    dmqcAcceptedConnectionsLimit <- v .:? "AcceptedConnectionsLimit"
                                      .!= defaultAcceptedConnectionsLimit

    dmqcDiffusionMode <- v .:? "DiffusionMode"
                           .!= InitiatorAndResponderDiffusionMode

    dmqcTargetOfRootPeers                 <- v .:? "TargetNumberOfRootPeers"
                                               .!= (targetNumberOfRootPeers defaultDeadlineTargets)
    dmqcTargetOfKnownPeers                <- v .:? "TargetNumberOfKnownPeers"
                                               .!= (targetNumberOfKnownPeers defaultDeadlineTargets)
    dmqcTargetOfEstablishedPeers          <- v .:? "TargetNumberOfEstablishedPeers"
                                               .!= (targetNumberOfEstablishedPeers defaultDeadlineTargets)
    dmqcTargetOfActivePeers               <- v .:? "TargetNumberOfActivePeers"
                                               .!= (targetNumberOfActivePeers defaultDeadlineTargets)
    dmqcTargetOfKnownBigLedgerPeers       <- v .:? "TargetNumberOfKnownBigLedgerPeers"
                                               .!= (targetNumberOfKnownBigLedgerPeers defaultDeadlineTargets)
    dmqcTargetOfEstablishedBigLedgerPeers <- v .:? "TargetNumberOfEstablishedBigLedgerPeers"
                                               .!= (targetNumberOfEstablishedBigLedgerPeers defaultDeadlineTargets)
    dmqcTargetOfActiveBigLedgerPeers      <- v .:? "TargetNumberOfActiveBigLedgerPeers"
                                               .!= (targetNumberOfActiveBigLedgerPeers defaultDeadlineTargets)

    dmqcProtocolIdleTimeout <- v .:? "ProtocolIdleTimeout"
                                 .!= defaultProtocolIdleTimeout

    dmqcChurnInterval <- v .:? "ChurnInterval"
                           .!= defaultDeadlineChurnInterval

    dmqcPeerSharing <- v .:? "PeerSharing"
                         .!= PeerSharingEnabled
    networkMagic <- v .: "NetworkMagic"

    pure $
      Configuration
        { dmqcAcceptedConnectionsLimit
        , dmqcDiffusionMode
        , dmqcTargetOfRootPeers
        , dmqcTargetOfKnownPeers
        , dmqcTargetOfEstablishedPeers
        , dmqcTargetOfActivePeers
        , dmqcTargetOfKnownBigLedgerPeers
        , dmqcTargetOfEstablishedBigLedgerPeers
        , dmqcTargetOfActiveBigLedgerPeers
        , dmqcProtocolIdleTimeout
        , dmqcChurnInterval
        , dmqcPeerSharing
        , dmqcNetworkMagic = NetworkMagic networkMagic
        }

-- | Read the `DMQConfiguration` from the specified file.
--
readConfigurationFile
  :: FilePath
  -> IO (Either Text (Configuration ntnFd ntnAddr ntcFd ntcAddr))
readConfigurationFile nc = do
  eBs <- try $ BS.readFile nc

  case eBs of
    Left e -> return . Left $ handler e
    Right bs ->
      let bs' = LBS.fromStrict bs in
        case eitherDecode bs' of
          Left err -> return $ Left (handlerJSON err)
          Right t  -> return $ Right t
  where
    handler :: IOException -> Text
    handler e = Text.pack $ "DMQ.Configurations.readConfigurationFile: "
                          ++ displayException e
    handlerJSON :: String -> Text
    handlerJSON err = mconcat
      [ "Is your configuration file formatted correctly? "
      , Text.pack err
      ]

readConfigurationFileOrError
  :: FilePath
  -> IO (Configuration ntnFd ntnAddr ntcFd ntcAddr)
readConfigurationFileOrError nc =
      readConfigurationFile nc
  >>= either (\err -> error $ "DMQ.Topology.readTopologyFile: "
                           <> Text.unpack err)
             pure

mkDiffusionConfiguration
  :: CLIOptions
  -> NetworkTopology extraConfig extraFlags
  -> Configuration ntnFd SockAddr ntcFd ntcAddr
  -> IO (Diffusion.Configuration extraFlags IO ntnFd SockAddr ntcFd ntcAddr)
mkDiffusionConfiguration
  CLIOptions {
    ipv4
  , port
  }
  nt@NetworkTopology {
    useLedgerPeers
  , peerSnapshotPath
  }
  Configuration {
    dmqcAcceptedConnectionsLimit
  , dmqcDiffusionMode
  , dmqcTargetOfRootPeers
  , dmqcTargetOfKnownPeers
  , dmqcTargetOfEstablishedPeers
  , dmqcTargetOfActivePeers
  , dmqcTargetOfKnownBigLedgerPeers
  , dmqcTargetOfEstablishedBigLedgerPeers
  , dmqcTargetOfActiveBigLedgerPeers
  , dmqcProtocolIdleTimeout
  , dmqcChurnInterval
  , dmqcPeerSharing
  } = do
    s <- (addrAddress . NonEmpty.head)
      <$> getAddrInfo (Just hints)
                      (Just (show ipv4))
                      (Just (show port))

    publicPeerSelectionVar <- makePublicPeerSelectionStateVar

    let (localRoots, publicRoots) = producerAddresses nt
    localRootsVar   <- newTVarIO localRoots
    publicRootsVar  <- newTVarIO publicRoots
    useLedgerVar    <- newTVarIO useLedgerPeers
    ledgerPeerSnapshotPathVar <- newTVarIO peerSnapshotPath
    ledgerPeerSnapshotVar <- newTVarIO =<< updateLedgerPeerSnapshot
                                            (readTVar ledgerPeerSnapshotPathVar)
                                            (const . pure $ ())

    return $
      Diffusion.Configuration {
        Diffusion.dcIPv4Address              = Just (Right s)
      , Diffusion.dcIPv6Address              = Nothing
      , Diffusion.dcLocalAddress             = Nothing
      , Diffusion.dcAcceptedConnectionsLimit = dmqcAcceptedConnectionsLimit
      , Diffusion.dcMode                     = dmqcDiffusionMode
      , Diffusion.dcPublicPeerSelectionVar   = publicPeerSelectionVar
      , Diffusion.dcPeerSelectionTargets     =
          PeerSelectionTargets {
            targetNumberOfRootPeers                 = dmqcTargetOfRootPeers
          , targetNumberOfKnownPeers                = dmqcTargetOfKnownPeers
          , targetNumberOfEstablishedPeers          = dmqcTargetOfEstablishedPeers
          , targetNumberOfActivePeers               = dmqcTargetOfActivePeers
          , targetNumberOfKnownBigLedgerPeers       = dmqcTargetOfKnownBigLedgerPeers
          , targetNumberOfEstablishedBigLedgerPeers = dmqcTargetOfEstablishedBigLedgerPeers
          , targetNumberOfActiveBigLedgerPeers      = dmqcTargetOfActiveBigLedgerPeers
          }
      , Diffusion.dcReadLocalRootPeers       = readTVar localRootsVar
      , Diffusion.dcReadPublicRootPeers      = readTVar publicRootsVar
      , Diffusion.dcReadLedgerPeerSnapshot   = readTVar ledgerPeerSnapshotVar
      , Diffusion.dcOwnPeerSharing           = dmqcPeerSharing
      , Diffusion.dcReadUseLedgerPeers       = readTVar useLedgerVar
      , Diffusion.dcProtocolIdleTimeout      = dmqcProtocolIdleTimeout
      , Diffusion.dcTimeWaitTimeout          = defaultTimeWaitTimeout
      , Diffusion.dcDeadlineChurnInterval    = dmqcChurnInterval
      , Diffusion.dcBulkChurnInterval        = dmqcChurnInterval
      , Diffusion.dcMuxForkPolicy            = Diffusion.noBindForkPolicy -- TODO: Make option flag for responderForkPolicy
      , Diffusion.dcLocalMuxForkPolicy       = Diffusion.noBindForkPolicy -- TODO: Make option flag for responderForkPolicy
      , Diffusion.dcEgressPollInterval       = 0                          -- TODO: Make option flag for egress poll interval
      }
  where
    hints = defaultHints {
              addrFlags = [AI_PASSIVE, AI_ADDRCONFIG]
            , addrSocketType = Stream
            }

    updateLedgerPeerSnapshot :: STM IO (Maybe FilePath)
                             -> (Maybe LedgerPeerSnapshot -> STM IO ())
                             -> IO (Maybe LedgerPeerSnapshot)
    updateLedgerPeerSnapshot readLedgerPeerPath writeVar = do
      mPeerSnapshotFile <- atomically readLedgerPeerPath
      mLedgerPeerSnapshot <- forM mPeerSnapshotFile $ readPeerSnapshotFileOrError
      atomically . writeVar $ mLedgerPeerSnapshot
      pure mLedgerPeerSnapshot


{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module DMQ.Configuration
  ( Configuration' (..)
  , PartialConfig
  , Configuration
  , I (..)
  , readConfigurationFileOrError
  , mkDiffusionConfiguration
  , defaultSigDecisionPolicy
  , defaultConfiguration
  , NoExtraConfig (..)
  , NoExtraFlags (..)
  , LocalAddress (..)
  ) where

import Control.Concurrent.Class.MonadSTM (MonadSTM (..))
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI (DiffTime)
import Data.Act
import Data.Act.Generic (gpact)
import Data.Aeson
import Data.Aeson.Types (parseFail)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Functor.Identity
import Data.IP
import Data.List.NonEmpty qualified as NonEmpty
import Data.Monoid (Last (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Generic.Data (gmappend, gmempty)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Network.Socket (AddrInfo (..), AddrInfoFlag (..), PortNumber,
           SocketType (..), defaultHints, getAddrInfo)
import System.Directory qualified as Directory
import System.FilePath qualified as FilePath
import System.IO.Error (isDoesNotExistError)
import Text.Read (readMaybe)

import Ouroboros.Network.Diffusion.Configuration (BlockProducerOrRelay (..),
           defaultAcceptedConnectionsLimit, defaultDeadlineChurnInterval,
           defaultDeadlineTargets, defaultProtocolIdleTimeout,
           defaultTimeWaitTimeout)
import Ouroboros.Network.Diffusion.Topology (NetworkTopology (..),
           producerAddresses)
import Ouroboros.Network.Diffusion.Types qualified as Diffusion
import Ouroboros.Network.DiffusionMode
import Ouroboros.Network.Magic (NetworkMagic (..))
import Ouroboros.Network.OrphanInstances ()
import Ouroboros.Network.PeerSelection.Governor.Types
           (PeerSelectionTargets (..), makePublicPeerSelectionStateVar)
import Ouroboros.Network.PeerSelection.LedgerPeers.Type
           (LedgerPeerSnapshot (..))
import Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing (..))
import Ouroboros.Network.Server.RateLimiting (AcceptedConnectionsLimit (..))
import Ouroboros.Network.Snocket (LocalAddress (..), RemoteAddress)
import Ouroboros.Network.TxSubmission.Inbound.V2 (TxDecisionPolicy (..))

import DMQ.Configuration.Topology (NoExtraConfig (..), NoExtraFlags (..),
           readPeerSnapshotFileOrError)

-- | Configuration comes in two flavours paramemtrised by `f` functor:
-- `PartialConfig` is using `Last` and `Configuration` is using an identity
-- functor `I`.
--
data Configuration' f =
  Configuration {
    dmqcIPv4                                       :: f (Maybe IPv4),
    dmqcIPv6                                       :: f (Maybe IPv6),
    dmqcLocalAddress                               :: f LocalAddress,
    dmqcPortNumber                                 :: f PortNumber,
    dmqcConfigFile                                 :: f FilePath,
    dmqcTopologyFile                               :: f FilePath,
    dmqcShelleyGenesisFile                         :: f FilePath,
    -- ^ shelley genesis file, e.g.
    -- `/configuration/cardano/mainnet-shelley-genesis.json` in `cardano-node`
    -- repo.
    dmqcAcceptedConnectionsLimit                   :: f AcceptedConnectionsLimit,
    dmqcDiffusionMode                              :: f DiffusionMode,
    dmqcTargetOfRootPeers                          :: f Int,
    dmqcTargetOfKnownPeers                         :: f Int,
    dmqcTargetOfEstablishedPeers                   :: f Int,
    dmqcTargetOfActivePeers                        :: f Int,
    dmqcTargetOfKnownBigLedgerPeers                :: f Int,
    dmqcTargetOfEstablishedBigLedgerPeers          :: f Int,
    dmqcTargetOfActiveBigLedgerPeers               :: f Int,
    dmqcProtocolIdleTimeout                        :: f DiffTime,
    dmqcChurnInterval                              :: f DiffTime,
    dmqcPeerSharing                                :: f PeerSharing,
    dmqcNetworkMagic                               :: f NetworkMagic,
    dmqcPrettyLog                                  :: f Bool,

    dmqcMuxTracer                                  :: f Bool,
    dmqcChannelTracer                              :: f Bool,
    dmqcBearerTracer                               :: f Bool,
    dmqcHandshakeTracer                            :: f Bool,
    dmqcLocalMuxTracer                             :: f Bool,
    dmqcLocalChannelTracer                         :: f Bool,
    dmqcLocalBearerTracer                          :: f Bool,
    dmqcLocalHandshakeTracer                       :: f Bool,
    dmqcDiffusionTracer                            :: f Bool,
    dmqcTraceLocalRootPeersTracer                  :: f Bool,
    dmqcTracePublicRootPeersTracer                 :: f Bool,
    dmqcTraceLedgerPeersTracer                     :: f Bool,
    dmqcTracePeerSelectionTracer                   :: f Bool,
    dmqcTraceChurnCounters                         :: f Bool,
    dmqcDebugPeerSelectionInitiatorTracer          :: f Bool,
    dmqcDebugPeerSelectionInitiatorResponderTracer :: f Bool,
    dmqcTracePeerSelectionCounters                 :: f Bool,
    dmqcPeerSelectionActionsTracer                 :: f Bool,
    dmqcConnectionManagerTracer                    :: f Bool,
    dmqcConnectionManagerTransitionTracer          :: f Bool,
    dmqcServerTracer                               :: f Bool,
    dmqcInboundGovernorTracer                      :: f Bool,
    dmqcInboundGovernorTransitionTracer            :: f Bool,
    dmqcLocalConnectionManagerTracer               :: f Bool,
    dmqcLocalServerTracer                          :: f Bool,
    dmqcLocalInboundGovernorTracer                 :: f Bool,
    dmqcDnsTracer                                  :: f Bool,

    -- low level verbose traces which trace protocol messages
    -- TODO: pref
    dmqcSigSubmissionClientProtocolTracer          :: f Bool,
    dmqcSigSubmissionServerProtocolTracer          :: f Bool,
    dmqcKeepAliveClientProtocolTracer              :: f Bool,
    dmqcKeepAliveServerProtocolTracer              :: f Bool,
    dmqcPeerSharingClientProtocolTracer            :: f Bool,
    dmqcPeerSharingServerProtocolTracer            :: f Bool,
    dmqcLocalMsgSubmissionServerProtocolTracer     :: f Bool,
    dmqcLocalMsgNotificationServerProtocolTracer   :: f Bool,

    dmqcSigSubmissionLogicTracer                   :: f Bool,
    dmqcSigSubmissionOutboundTracer                :: f Bool,
    dmqcSigSubmissionInboundTracer                 :: f Bool,
    dmqcLocalMsgSubmissionServerTracer             :: f Bool,

    dmqcVersion                                    :: f Bool
  }
  deriving Generic

instance (forall a. Semigroup (f a))
      => Semigroup (Configuration' f) where
  (<>) = gmappend
instance (forall a. Monoid (f a))
      => Monoid (Configuration' f) where
  mempty = gmempty

-- Using an action, eliminates the need to use `undefined`, e.g. instead of
-- transforming
-- ```
--   (defaultConfig <> configFileOptions <> cliOptions) :: PartialConfig
-- ```
-- to `Configuration` we just have
-- ```
--   (configFileOptions <> cliOptions • defaultConfig) :: Configuration
-- ```
-- without any partial functions.
--
--
instance (forall a. Act (f a) (g a))
      => Act (Configuration' f) (Configuration' g) where
  act = gpact

deriving instance Show Configuration
deriving instance Show PartialConfig

-- | An Identity functor, but shorter to type.
--
newtype I a = I { unI :: a }
  deriving stock Generic
  deriving newtype Show
  deriving (Functor, Applicative, Monad) via Identity

-- NOTE: it would be more convenient to have a right action of `Last` on `I`,
-- but `acts` library only provides left actions.
instance Act (Last a) (I a) where
  act (Last Nothing)  i = i
  act (Last (Just a)) _ = I a

type Configuration = Configuration' I
type PartialConfig = Configuration' Last


-- | By using `Configuration` type we enforce that every value has a default,
-- except of IP addresses, which are using `Maybe` values.  This is needed to
-- make sure one can configure only the IP addresses which are available on the
-- system.
--
defaultConfiguration :: Configuration
defaultConfiguration = Configuration {
      dmqcIPv4                                       = I Nothing,
      dmqcIPv6                                       = I Nothing,
      dmqcLocalAddress                               = I (LocalAddress "dmq-node.socket"),
      dmqcNetworkMagic                               = I NetworkMagic { unNetworkMagic = 3_141_592 },
      dmqcPortNumber                                 = I 3_141,
      dmqcConfigFile                                 = I "dmq.configuration.yaml",
      dmqcTopologyFile                               = I "dmq.topology.json",
      dmqcShelleyGenesisFile                         = I "mainnet-shelley-genesis.json",
      dmqcAcceptedConnectionsLimit                   = I defaultAcceptedConnectionsLimit,
      dmqcDiffusionMode                              = I InitiatorAndResponderDiffusionMode,
      dmqcTargetOfRootPeers                          = I targetNumberOfRootPeers,
      dmqcTargetOfKnownPeers                         = I targetNumberOfKnownPeers,
      dmqcTargetOfEstablishedPeers                   = I targetNumberOfEstablishedPeers,
      dmqcTargetOfActivePeers                        = I targetNumberOfActivePeers,
      dmqcTargetOfKnownBigLedgerPeers                = I targetNumberOfKnownBigLedgerPeers,
      dmqcTargetOfEstablishedBigLedgerPeers          = I targetNumberOfEstablishedBigLedgerPeers,
      dmqcTargetOfActiveBigLedgerPeers               = I targetNumberOfActiveBigLedgerPeers,
      dmqcProtocolIdleTimeout                        = I defaultProtocolIdleTimeout,
      dmqcChurnInterval                              = I defaultDeadlineChurnInterval,
      dmqcPeerSharing                                = I PeerSharingEnabled,
      dmqcPrettyLog                                  = I False,
      dmqcMuxTracer                                  = I False,
      dmqcChannelTracer                              = I False,
      dmqcBearerTracer                               = I False,
      dmqcHandshakeTracer                            = I True,
      dmqcLocalMuxTracer                             = I True,
      dmqcLocalChannelTracer                         = I False,
      dmqcLocalBearerTracer                          = I False,
      dmqcLocalHandshakeTracer                       = I True,
      dmqcDiffusionTracer                            = I True,
      dmqcTraceLocalRootPeersTracer                  = I False,
      dmqcTracePublicRootPeersTracer                 = I False,
      dmqcTraceLedgerPeersTracer                     = I False,
      dmqcTracePeerSelectionTracer                   = I True,
      dmqcTraceChurnCounters                         = I False,
      dmqcDebugPeerSelectionInitiatorTracer          = I False,
      dmqcDebugPeerSelectionInitiatorResponderTracer = I False,
      dmqcTracePeerSelectionCounters                 = I True,
      dmqcPeerSelectionActionsTracer                 = I False,
      dmqcConnectionManagerTracer                    = I True,
      dmqcConnectionManagerTransitionTracer          = I False,
      dmqcServerTracer                               = I True,
      dmqcInboundGovernorTracer                      = I True,
      dmqcInboundGovernorTransitionTracer            = I False,
      dmqcLocalConnectionManagerTracer               = I False,
      dmqcLocalServerTracer                          = I False,
      dmqcLocalInboundGovernorTracer                 = I False,
      dmqcDnsTracer                                  = I False,

      dmqcSigSubmissionClientProtocolTracer          = I False,
      dmqcSigSubmissionServerProtocolTracer          = I False,
      dmqcKeepAliveClientProtocolTracer              = I False,
      dmqcKeepAliveServerProtocolTracer              = I False,
      dmqcPeerSharingClientProtocolTracer            = I False,
      dmqcPeerSharingServerProtocolTracer            = I False,
      dmqcLocalMsgSubmissionServerProtocolTracer     = I True,
      dmqcLocalMsgNotificationServerProtocolTracer   = I True,

      dmqcSigSubmissionOutboundTracer                = I False,
      dmqcSigSubmissionInboundTracer                 = I True,
      dmqcSigSubmissionLogicTracer                   = I False,
      dmqcLocalMsgSubmissionServerTracer             = I True,

      -- CLI only options
      dmqcVersion                                    = I False
    }
  where
    PeerSelectionTargets {
      targetNumberOfRootPeers,
      targetNumberOfKnownPeers,
      targetNumberOfEstablishedPeers,
      targetNumberOfActivePeers,
      targetNumberOfKnownBigLedgerPeers,
      targetNumberOfEstablishedBigLedgerPeers,
      targetNumberOfActiveBigLedgerPeers
    } = defaultDeadlineTargets Relay
    -- TODO: use DMQ's own default values


-- | Parsing configuration used when reading it from disk
--
instance FromJSON PartialConfig where
  parseJSON = withObject "DMQConfiguration" $ \v -> do

      dmqcIPv4 <- fmap readMaybe <$> v .:? "IPv4"
      case dmqcIPv4 of
        Just Nothing -> parseFail "couldn't parse IPv4 address"
        _            -> pure ()
      dmqcIPv6 <- fmap readMaybe <$> v .:? "IPv6"
      case dmqcIPv6 of
        Just Nothing -> parseFail "couldn't parse IPv6 address"
        _            -> pure ()
      dmqcLocalAddress <- Last . fmap LocalAddress <$> v .:? "LocalAddress"
      dmqcPortNumber <- Last . fmap (fromIntegral @Int) <$> v.:? "PortNumber"
      dmqcNetworkMagic <- Last . fmap NetworkMagic <$> v .:? "NetworkMagic"
      dmqcDiffusionMode <- Last <$> v .:? "DiffusionMode"
      dmqcPeerSharing <- Last <$> v .:? "PeerSharing"

      dmqcShelleyGenesisFile <- Last <$> v .:? "ShelleyGenesisFile"

      dmqcTargetOfRootPeers                 <- Last <$> v .:? "TargetNumberOfRootPeers"
      dmqcTargetOfKnownPeers                <- Last <$> v .:? "TargetNumberOfKnownPeers"
      dmqcTargetOfEstablishedPeers          <- Last <$> v .:? "TargetNumberOfEstablishedPeers"
      dmqcTargetOfActivePeers               <- Last <$> v .:? "TargetNumberOfActivePeers"
      dmqcTargetOfKnownBigLedgerPeers       <- Last <$> v .:? "TargetNumberOfKnownBigLedgerPeers"
      dmqcTargetOfEstablishedBigLedgerPeers <- Last <$> v .:? "TargetNumberOfEstablishedBigLedgerPeers"
      dmqcTargetOfActiveBigLedgerPeers      <- Last <$> v .:? "TargetNumberOfActiveBigLedgerPeers"

      dmqcAcceptedConnectionsLimit <- Last <$> v .:? "AcceptedConnectionsLimit"
      dmqcProtocolIdleTimeout <- Last <$> v .:? "ProtocolIdleTimeout"
      dmqcChurnInterval <- Last <$> v .:? "ChurnInterval"

      dmqcPrettyLog <- Last <$> v .:? "PrettyLog"

      dmqcMuxTracer                                  <- Last <$> v .:? "MuxTracer"
      dmqcChannelTracer                              <- Last <$> v .:? "ChannelTracer"
      dmqcBearerTracer                               <- Last <$> v .:? "BearerTracer"
      dmqcHandshakeTracer                            <- Last <$> v .:? "HandshakeTracer"
      dmqcLocalMuxTracer                             <- Last <$> v .:? "LocalMuxTracer"
      dmqcLocalChannelTracer                         <- Last <$> v .:? "LocalChannelTracer"
      dmqcLocalBearerTracer                          <- Last <$> v .:? "LocalBearerTracer"
      dmqcLocalHandshakeTracer                       <- Last <$> v .:? "LocalHandshakeTracer"
      dmqcDiffusionTracer                            <- Last <$> v .:? "DiffusionTracer"
      dmqcTraceLocalRootPeersTracer                  <- Last <$> v .:? "LocalRootPeersTracer"
      dmqcTracePublicRootPeersTracer                 <- Last <$> v .:? "PublicRootPeersTracer"
      dmqcTraceLedgerPeersTracer                     <- Last <$> v .:? "LedgerPeersTracer"
      dmqcTracePeerSelectionTracer                   <- Last <$> v .:? "PeerSelectionTracer"
      dmqcTraceChurnCounters                         <- Last <$> v .:? "ChurnCounters"
      dmqcDebugPeerSelectionInitiatorTracer          <- Last <$> v .:? "DebugPeerSelectionInitiatorTracer"
      dmqcDebugPeerSelectionInitiatorResponderTracer <- Last <$> v .:? "DebugPeerSelectionInitiatorResponderTracer"
      dmqcTracePeerSelectionCounters                 <- Last <$> v .:? "PeerSelectionCounters"
      dmqcPeerSelectionActionsTracer                 <- Last <$> v .:? "PeerSelectionActionsTracer"
      dmqcConnectionManagerTracer                    <- Last <$> v .:? "ConnectionManagerTracer"
      dmqcConnectionManagerTransitionTracer          <- Last <$> v .:? "ConnectionManagerTransitionTracer"
      dmqcServerTracer                               <- Last <$> v .:? "ServerTracer"
      dmqcInboundGovernorTracer                      <- Last <$> v .:? "InboundGovernorTracer"
      dmqcInboundGovernorTransitionTracer            <- Last <$> v .:? "InboundGovernorTransitionTracer"
      dmqcLocalConnectionManagerTracer               <- Last <$> v .:? "LocalConnectionManagerTracer"
      dmqcLocalServerTracer                          <- Last <$> v .:? "LocalServerTracer"
      dmqcLocalInboundGovernorTracer                 <- Last <$> v .:? "LocalInboundGovernorTracer"
      dmqcDnsTracer                                  <- Last <$> v .:? "DnsTracer"

      dmqcSigSubmissionClientProtocolTracer          <- Last <$> v .:? "SigSubmissionClientProtocolTracer"
      dmqcSigSubmissionServerProtocolTracer          <- Last <$> v .:? "SigSubmissionServerProtocolTracer"
      dmqcKeepAliveClientProtocolTracer              <- Last <$> v .:? "KeepAliveServerProtocolTracer"
      dmqcKeepAliveServerProtocolTracer              <- Last <$> v .:? "KeepAliveClientProtocolTracer"
      dmqcPeerSharingClientProtocolTracer            <- Last <$> v .:? "PeerSharingServerProtocolTracer"
      dmqcPeerSharingServerProtocolTracer            <- Last <$> v .:? "PeerSharingClientProtocolTracer"
      dmqcLocalMsgSubmissionServerProtocolTracer     <- Last <$> v .:? "LocalMsgSubmissionServerProtocolracer"
      dmqcLocalMsgNotificationServerProtocolTracer   <- Last <$> v .:? "LocalMsgNotificationServerProtocolracer"

      dmqcSigSubmissionOutboundTracer                <- Last <$> v .:? "SigSubmissionOutboundTracer"
      dmqcSigSubmissionInboundTracer                 <- Last <$> v .:? "SigSubmissionInboundTracer"
      dmqcSigSubmissionLogicTracer                   <- Last <$> v .:? "SigSubmissionLogicTracer"
      dmqcLocalMsgSubmissionServerTracer             <- Last <$> v .:? "LocalMsgSubmissionServerTracer"

      pure $
        Configuration
          { dmqcIPv4         = Last dmqcIPv4
          , dmqcIPv6         = Last dmqcIPv6
          , dmqcConfigFile   = mempty
          , dmqcTopologyFile = mempty
          , dmqcVersion      = mempty
          , ..
          }

-- | ToJSON instance used by logging system.
--
instance ToJSON Configuration where
  toJSON Configuration {..} =
    object [ "IPv4"                                       .= (show <$> unI dmqcIPv4)
           , "IPv6"                                       .= (show <$> unI dmqcIPv6)
           , "PortNumber"                                 .= unI dmqcPortNumber
           , "LocalAddress"                               .= unI dmqcLocalAddress
           , "ConfigFile"                                 .= unI dmqcConfigFile
           , "TopologyFile"                               .= unI dmqcTopologyFile
           , "ShelleyGenesisFile"                         .= unI dmqcShelleyGenesisFile
           , "AcceptedConnectionsLimit"                   .= unI dmqcAcceptedConnectionsLimit
           , "DiffusionMode"                              .= unI dmqcDiffusionMode
           , "TargetOfRootPeers"                          .= unI dmqcTargetOfRootPeers
           , "TargetOfKnownPeers"                         .= unI dmqcTargetOfKnownPeers
           , "TargetOfEstablishedPeers"                   .= unI dmqcTargetOfEstablishedPeers
           , "TargetOfActivePeers"                        .= unI dmqcTargetOfActivePeers
           , "TargetOfKnownBigLedgerPeers"                .= unI dmqcTargetOfKnownBigLedgerPeers
           , "TargetOfEstablishedBigLedgerPeers"          .= unI dmqcTargetOfEstablishedBigLedgerPeers
           , "TargetOfActiveBigLedgerPeers"               .= unI dmqcTargetOfActiveBigLedgerPeers
           , "ProtocolIdleTimeout"                        .= unI dmqcProtocolIdleTimeout
           , "ChurnInterval"                              .= unI dmqcChurnInterval
           , "PeerSharing"                                .= unI dmqcPeerSharing
           , "NetworkMagic"                               .= unNetworkMagic (unI dmqcNetworkMagic)
           , "PrettyLog"                                  .= unI dmqcPrettyLog
           , "MuxTracer"                                  .= unI dmqcMuxTracer
           , "ChannelTracer"                              .= unI dmqcChannelTracer
           , "BearerTracer"                               .= unI dmqcBearerTracer
           , "HandshakeTracer"                            .= unI dmqcHandshakeTracer
           , "LocalMuxTracer"                             .= unI dmqcLocalMuxTracer
           , "LocalChannelTracer"                         .= unI dmqcLocalChannelTracer
           , "LocalBearerTracer"                          .= unI dmqcLocalBearerTracer
           , "LocalHandshakeTracer"                       .= unI dmqcLocalHandshakeTracer
           , "DiffusionTracer"                            .= unI dmqcDiffusionTracer
           , "LocalRootPeersTracer"                       .= unI dmqcTraceLocalRootPeersTracer
           , "PublicRootPeersTracer"                      .= unI dmqcTracePublicRootPeersTracer
           , "LedgerPeersTracer"                          .= unI dmqcTraceLedgerPeersTracer
           , "PeerSelectionTracer"                        .= unI dmqcTracePeerSelectionTracer
           , "ChurnCounters"                              .= unI dmqcTraceChurnCounters
           , "DebugPeerSelectionInitiatorTracer"          .= unI dmqcDebugPeerSelectionInitiatorTracer
           , "DebugPeerSelectionInitiatorResponderTracer" .= unI dmqcDebugPeerSelectionInitiatorResponderTracer
           , "PeerSelectionCounters"                      .= unI dmqcTracePeerSelectionCounters
           , "PeerSelectionActionsTracer"                 .= unI dmqcPeerSelectionActionsTracer
           , "ConnectionManagerTracer"                    .= unI dmqcConnectionManagerTracer
           , "ConnectionManagerTransitionTracer"          .= unI dmqcConnectionManagerTransitionTracer
           , "ServerTracer"                               .= unI dmqcServerTracer
           , "InboundGovernorTracer"                      .= unI dmqcInboundGovernorTracer
           , "InboundGovernorTransitionTracer"            .= unI dmqcInboundGovernorTransitionTracer
           , "LocalConnectionManagerTracer"               .= unI dmqcLocalConnectionManagerTracer
           , "LocalServerTracer"                          .= unI dmqcLocalServerTracer
           , "LocalInboundGovernorTracer"                 .= unI dmqcLocalInboundGovernorTracer
           , "DnsTracer"                                  .= unI dmqcDnsTracer
           , "SigSubmissionClientProtocolTracer"          .= unI dmqcSigSubmissionClientProtocolTracer
           , "SigSubmissionServerProtocolTracer"          .= unI dmqcSigSubmissionServerProtocolTracer
           , "KeepAliveClientProtocolTracer"              .= unI dmqcKeepAliveClientProtocolTracer
           , "KeepAliveServerProtocolTracer"              .= unI dmqcKeepAliveServerProtocolTracer
           , "PeerSharingClientProtocolTracer"            .= unI dmqcPeerSharingClientProtocolTracer
           , "PeerSharingServerProtocolTracer"            .= unI dmqcPeerSharingServerProtocolTracer
           , "LocalMsgSubmissionServerProtocolTracer"     .= unI dmqcLocalMsgSubmissionServerProtocolTracer
           , "LocalMsgNotificationServerProtocolTracer"   .= unI dmqcLocalMsgNotificationServerProtocolTracer
           , "SigSubmissionOutboundTracer"                .= unI dmqcSigSubmissionOutboundTracer
           , "SigSubmissionInboundTracer"                 .= unI dmqcSigSubmissionInboundTracer
           , "SigSubmissionLogicTracer"                   .= unI dmqcSigSubmissionLogicTracer
           ]

-- | Read the `DMQConfiguration` from the specified file.
--
readConfigurationFile
  :: FilePath
  -> IO (Either Text PartialConfig)
readConfigurationFile nc = do
  ebs <- try $ BS.readFile nc
  case ebs of
    -- use the default configuration if it's not on disk
    Left e | isDoesNotExistError e
           -> return $ Right mempty
    Left e -> return . Left $ handler e
    Right bs -> do
      let bs' = LBS.fromStrict bs
      case eitherDecode bs' of
        Left err -> return $ Left (handlerJSON err)
        Right t  -> return (Right t)
  where
    handler :: IOError -> Text
    handler e = Text.pack $ "DMQ.Configurations.readConfigurationFile: "
                          ++ displayException e
    handlerJSON :: String -> Text
    handlerJSON err = mconcat
      [ "Is your configuration file formatted correctly? "
      , Text.pack err
      ]

readConfigurationFileOrError
  :: FilePath
  -> IO PartialConfig
readConfigurationFileOrError nc =
      readConfigurationFile nc
  >>= either (\err -> error $ "DMQ.Topology.eeadConfigurationFile: "
                           <> Text.unpack err)
             pure

mkDiffusionConfiguration
  :: HasCallStack
  => Configuration
  -> NetworkTopology NoExtraConfig NoExtraFlags
  -> IO (Diffusion.Configuration NoExtraFlags IO ntnFd RemoteAddress ntcFd LocalAddress)
mkDiffusionConfiguration
  Configuration {
    dmqcIPv4                              = I ipv4
  , dmqcIPv6                              = I ipv6
  , dmqcLocalAddress                      = I localAddress
  , dmqcTopologyFile                      = I topologyFile
  , dmqcPortNumber                        = I portNumber
  , dmqcDiffusionMode                     = I diffusionMode
  , dmqcAcceptedConnectionsLimit          = I acceptedConnectionsLimit
  , dmqcTargetOfRootPeers                 = I targetOfRootPeers
  , dmqcTargetOfKnownPeers                = I targetOfKnownPeers
  , dmqcTargetOfEstablishedPeers          = I targetOfEstablishedPeers
  , dmqcTargetOfActivePeers               = I targetOfActivePeers
  , dmqcTargetOfKnownBigLedgerPeers       = I targetOfKnownBigLedgerPeers
  , dmqcTargetOfEstablishedBigLedgerPeers = I targetOfEstablishedBigLedgerPeers
  , dmqcTargetOfActiveBigLedgerPeers      = I targetOfActiveBigLedgerPeers
  , dmqcProtocolIdleTimeout               = I protocolIdleTimeout
  , dmqcChurnInterval                     = I churnInterval
  , dmqcPeerSharing                       = I peerSharing
  }
  nt@NetworkTopology {
    useLedgerPeers
  , peerSnapshotPath
  } = do
    case (ipv4, ipv6) of
      (Nothing, Nothing) ->
           throwIO NoAddressInformation
      _ -> return ()
    addrIPv4 <-
      case ipv4 of
        Just ipv4' ->
          Just . addrAddress . NonEmpty.head
            <$> getAddrInfo (Just hints)
                            (Just (show ipv4'))
                            (Just (show portNumber))
        Nothing -> return Nothing
    addrIPv6 <-
      case ipv6 of
        Just ipv6' ->
          Just . addrAddress . NonEmpty.head
            <$> getAddrInfo (Just hints)
                            (Just (show ipv6'))
                            (Just (show portNumber))
        Nothing -> return Nothing

    publicPeerSelectionVar <- makePublicPeerSelectionStateVar

    let (localRoots, publicRoots) = producerAddresses nt
    localRootsVar   <- newTVarIO localRoots
    publicRootsVar  <- newTVarIO publicRoots
    useLedgerVar    <- newTVarIO useLedgerPeers
    ledgerPeerSnapshotPathVar <- newTVarIO peerSnapshotPath
    topologyDir <- FilePath.takeDirectory <$> Directory.makeAbsolute topologyFile
    ledgerPeerSnapshotVar <- newTVarIO =<< updateLedgerPeerSnapshot
                                            topologyDir
                                            (readTVar ledgerPeerSnapshotPathVar)
                                            (const . pure $ ())

    return $
      Diffusion.Configuration {
        Diffusion.dcIPv4Address              = Right <$> addrIPv4
      , Diffusion.dcIPv6Address              = Right <$> addrIPv6
      , Diffusion.dcLocalAddress             = Just (Right localAddress)
      , Diffusion.dcAcceptedConnectionsLimit = acceptedConnectionsLimit
      , Diffusion.dcMode                     = diffusionMode
      , Diffusion.dcPublicPeerSelectionVar   = publicPeerSelectionVar
      , Diffusion.dcPeerSelectionTargets     =
          PeerSelectionTargets {
            targetNumberOfRootPeers                 = targetOfRootPeers
          , targetNumberOfKnownPeers                = targetOfKnownPeers
          , targetNumberOfEstablishedPeers          = targetOfEstablishedPeers
          , targetNumberOfActivePeers               = targetOfActivePeers
          , targetNumberOfKnownBigLedgerPeers       = targetOfKnownBigLedgerPeers
          , targetNumberOfEstablishedBigLedgerPeers = targetOfEstablishedBigLedgerPeers
          , targetNumberOfActiveBigLedgerPeers      = targetOfActiveBigLedgerPeers
          }
      , Diffusion.dcReadLocalRootPeers       = readTVar localRootsVar
      , Diffusion.dcReadPublicRootPeers      = readTVar publicRootsVar
      , Diffusion.dcReadLedgerPeerSnapshot   = readTVar ledgerPeerSnapshotVar
      , Diffusion.dcPeerSharing              = peerSharing
      , Diffusion.dcReadUseLedgerPeers       = readTVar useLedgerVar
      , Diffusion.dcProtocolIdleTimeout      = protocolIdleTimeout
      , Diffusion.dcTimeWaitTimeout          = defaultTimeWaitTimeout
      , Diffusion.dcDeadlineChurnInterval    = churnInterval
      , Diffusion.dcBulkChurnInterval        = churnInterval
      , Diffusion.dcMuxForkPolicy            = Diffusion.noBindForkPolicy -- TODO: Make option flag for responderForkPolicy
      , Diffusion.dcLocalMuxForkPolicy       = Diffusion.noBindForkPolicy -- TODO: Make option flag for responderForkPolicy
      , Diffusion.dcEgressPollInterval       = 0                          -- TODO: Make option flag for egress poll interval
      }
  where
    hints = defaultHints {
              addrFlags = [AI_PASSIVE, AI_ADDRCONFIG]
            , addrSocketType = Stream
            }

    updateLedgerPeerSnapshot :: HasCallStack
                             => FilePath
                             -> STM IO (Maybe FilePath)
                             -> (Maybe LedgerPeerSnapshot -> STM IO ())
                             -> IO (Maybe LedgerPeerSnapshot)
    updateLedgerPeerSnapshot topologyDir readLedgerPeerPath writeVar = do
      mPeerSnapshotFile <- atomically readLedgerPeerPath
      mLedgerPeerSnapshot <- case mPeerSnapshotFile of
        Nothing -> pure Nothing
        Just peerSnapshotFile | FilePath.isRelative peerSnapshotFile -> do
          peerSnapshotFile' <- Directory.makeAbsolute $ topologyDir FilePath.</> peerSnapshotFile
          Just <$> readPeerSnapshotFileOrError peerSnapshotFile'
        Just peerSnapshotFile ->
          Just <$> readPeerSnapshotFileOrError peerSnapshotFile
      atomically . writeVar $ mLedgerPeerSnapshot
      pure mLedgerPeerSnapshot


-- TODO: review this once we know what is the size of a `Sig`.
-- TODO: parts of should be configurable
defaultSigDecisionPolicy :: TxDecisionPolicy
defaultSigDecisionPolicy = TxDecisionPolicy {
    maxNumTxIdsToRequest   = 10,
    maxUnacknowledgedTxIds = 40,
    txsSizeInflightPerPeer = 100_000,
    maxTxsSizeInflight     = 250_000,
    txInflightMultiplicity = 1,
    bufferedTxsMinLifetime = 0,
    scoreRate              = 0.1,
    scoreMax               = 15 * 60
  }

data ConfigurationError =
    NoAddressInformation -- ^ dmq was not configured with IPv4 or IPv6 address
  deriving Show

instance Exception ConfigurationError where
  displayException NoAddressInformation = "no ipv4 or ipv6 address specified, use --host-addr or --host-ipv6-addr"



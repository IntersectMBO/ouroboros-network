{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PackageImports      #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module DMQ.Tracer
  ( mkCardanoTracer
  , EventType (..)
  , WithEventType (..)
  , NoExtraPeers (..)
  , NoExtraState (..)
  , NoExtraDebugState (..)
  , NoExtraCounters (..)
  , NoExtraFlags (..)
  , NoExtraConfig (..)
  , NoExtraAPI (..)
  , NoExtraChurnArgs (..)
  , NoExtraTracer (..)
  ) where

import Codec.CBOR.Term (Term)
import "contra-tracer" Control.Tracer

import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.KeyMap (fromList)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)

-- TODO
--import Cardano.Network.Logging ()
import Network.Mux.Logging ()
import Ouroboros.Network.Logging ()
import Ouroboros.Network.Logging.Framework ()

import Ouroboros.Network.Diffusion qualified as Diffusion
import Ouroboros.Network.OrphanInstances ()
import Ouroboros.Network.PeerSelection (DebugPeerSelection (..))
import Ouroboros.Network.PeerSelection.Governor.Types qualified as Governor
import Ouroboros.Network.PeerSelection.PublicRootPeers (PublicRootPeers)
import Ouroboros.Network.PeerSelection.PublicRootPeers qualified as PublicRootPeers
import Ouroboros.Network.Snocket (RemoteAddress)

import qualified Cardano.Logging as Logging

import DMQ.Configuration
import DMQ.NodeToClient.Version
import DMQ.NodeToNode.Version

data EventType = DMQ String
  deriving (Eq, Show)

data WithEventType = forall a. ToJSON a => WithEventType EventType a

instance Logging.LogFormatting WithEventType where
  -- Machine readable representation with varying details based on the detail level.
  -- forMachine :: DetailLevel -> a -> Aeson.Object
  forMachine _ (WithEventType _ event) = fromList [ ("data", toJSON event) ]
  -- Human readable representation.
  -- forHuman :: a -> Text
  forHuman (WithEventType _ event) = toStrict $ decodeUtf8 $ encodePretty event
  -- Metrics representation.
  -- asMetrics :: a -> [Metric]
  asMetrics _ = []

instance Logging.MetaTrace WithEventType where
  -- allNamespaces :: [Namespace a]
  allNamespaces = [
    -- DMQ-Node only traces.
    ------------------------
    -- Main.
      Logging.Namespace [] ["Configuration"]
    , Logging.Namespace [] ["ShelleyGenesisFile"]
    , Logging.Namespace [] ["NetworkTopology"]
    -- Diffusion.NodeKernel and NodeToClient
    , Logging.Namespace [] ["SigSubmission.Logic"]
    -- NodeToClient.
    , Logging.Namespace [] ["LocalMsgNotification.Protocol.Server"]
    , Logging.Namespace [] ["LocalMsgSubmission.Protocol.Server"]
    , Logging.Namespace [] ["LocalMsgSubmission.Server"]
    -- NodeToNode.
    , Logging.Namespace [] ["KeepAlive.Protocol.Client"]
    , Logging.Namespace [] ["KeepAlive.Protocol.Server"]
    , Logging.Namespace [] ["PeerSharing.Protocol.Client"]
    , Logging.Namespace [] ["PeerSharing.Protocol.Server"]
    , Logging.Namespace [] ["SigSubmission.Inbound"]
    , Logging.Namespace [] ["SigSubmission.Outbound"]
    , Logging.Namespace [] ["SigSubmission.Protocol.Client"]
    , Logging.Namespace [] ["SigSubmission.Protocol.Server"]
    ]
  namespaceFor (WithEventType et _) = Logging.Namespace [] [(Text.pack $ show et)]
  severityFor _ _ = Just Logging.Info
  privacyFor _ _ =  Just Logging.Public
  detailsFor _ _ =  Just Logging.DNormal
  documentFor _ = Nothing
  metricsDocFor _ = []

mkCardanoTracer :: FilePath
                -> IO (
                        Tracer IO WithEventType
                      , Diffusion.Tracers
                          RemoteAddress
                          NodeToNodeVersion
                          NodeToNodeVersionData
                          LocalAddress
                          NodeToClientVersion
                          NodeToClientVersionData
                          NoExtraState
                          NoExtraDebugState
                          NoExtraFlags
                          NoExtraPeers
                          NoExtraCounters
                          NoExtraTracer
                          IO
                      )
mkCardanoTracer dmqConfigFilePath = do
  traceConfig <- Logging.readConfiguration dmqConfigFilePath
  emptyConfigReflection <- Logging.emptyConfigReflection
  stdoutTrace <- Logging.standardTracer
  let trForward = mempty
  let mbTrEkg = Nothing
  {-- From: Cardano.Logging.Tracer.Composed
      -- | Construct a tracer according to the requirements for cardano node.
      -- The tracer gets a 'name', which is appended to its namespace.
      -- The tracer has to be an instance of LogFormatting for the display of
      -- messages and an instance of MetaTrace for meta information such as
      -- severity, privacy, details and backends'.
      -- The tracer gets the backends': 'trStdout', 'trForward' and 'mbTrEkg'
      -- as arguments.
      -- The returned tracer needs to be configured with a configuration
      -- before it is used.
      mkCardanoTracer :: forall evt. ( LogFormatting evt , MetaTrace evt)
                      => Trace IO FormattedMessage
                      -> Trace IO FormattedMessage
                      -> Maybe (Trace IO FormattedMessage)
                      -> [Text]
                      -> IO (Trace IO evt)
  --}
  -- This is a `Logging.Trace IO WithEventType`.
  cardanoTracer <- Logging.mkCardanoTracer
                  stdoutTrace
                  mempty
                  Nothing
                  [] -- ["DMQ"]
  {-- From: Cardano.Logging.Configuration
      -- | Call this function at initialisation, and later for reconfiguration.
      -- Config reflection is used to optimise the tracers and has to collect
      -- information about the tracers. Although it is possible to give more
      -- then one tracer of the same time, it is not a common case to do this.
      configureTracers :: forall a m. (MetaTrace a ,  MonadIO m)
                       => ConfigReflection
                       -> TraceConfig
                       -> [Trace m a]
                       -> m ()
  --}
  Logging.configureTracers
    emptyConfigReflection
    traceConfig
    [cardanoTracer]

  -- Make it a "contra-tracer" tracer for backward compatibility.
  -- This is a `Tracer IO WithEventType`.
  let dmqTracer = contramapM
                    (\wet@(WithEventType _ _) -> do
                      Logging.traceWith cardanoTracer wet
                    )
                  $ Tracer (\_ -> return ())

  !dtMuxTracer <- Logging.mkCardanoTracer
    stdoutTrace trForward mbTrEkg
    ["Net", "Mux", "Remote"]
  Logging.configureTracers emptyConfigReflection traceConfig [dtMuxTracer]
  !dtChannelTracer <- Logging.mkCardanoTracer
    stdoutTrace trForward mbTrEkg
    ["Net", "Mux", "Remote", "Channel"]
  Logging.configureTracers emptyConfigReflection traceConfig [dtChannelTracer]
  !dtBearerTracer <- Logging.mkCardanoTracer
    stdoutTrace trForward mbTrEkg
    ["Net", "Mux", "Remote", "Bearer"]
  Logging.configureTracers emptyConfigReflection traceConfig [dtBearerTracer]
  !dtHandshakeTracer <- Logging.mkCardanoTracer
    stdoutTrace trForward mbTrEkg
    ["Net", "Handshake", "Remote"]
  Logging.configureTracers emptyConfigReflection traceConfig [dtHandshakeTracer]
  !dtLocalMuxTracer <- Logging.mkCardanoTracer
    stdoutTrace trForward mbTrEkg
    ["Net", "Mux", "Local"]
  Logging.configureTracers emptyConfigReflection traceConfig [dtLocalMuxTracer]
  !dtLocalChannelTracer <- Logging.mkCardanoTracer
    stdoutTrace trForward mbTrEkg
    ["Net", "Mux", "Local", "Channel"]
  Logging.configureTracers emptyConfigReflection traceConfig [dtLocalChannelTracer]
  !dtLocalBearerTracer <- Logging.mkCardanoTracer
    stdoutTrace trForward mbTrEkg
    ["Net", "Mux", "Local", "Bearer"]
  Logging.configureTracers emptyConfigReflection traceConfig [dtLocalBearerTracer]
  !dtLocalHandshakeTracer <- Logging.mkCardanoTracer
    stdoutTrace trForward mbTrEkg
    ["Net", "Handshake", "Local"]
  Logging.configureTracers emptyConfigReflection traceConfig [dtLocalHandshakeTracer]
  !dtDiffusionTracer <- Logging.mkCardanoTracer
    stdoutTrace trForward mbTrEkg
    ["Startup", "DiffusionInit"]
  Logging.configureTracers emptyConfigReflection traceConfig [dtDiffusionTracer]
  !dtTraceLocalRootPeersTracer  <- Logging.mkCardanoTracer
    stdoutTrace trForward mbTrEkg
    ["Net", "Peers", "LocalRoot"]
  Logging.configureTracers emptyConfigReflection traceConfig [dtTraceLocalRootPeersTracer]
  !dtTracePublicRootPeersTracer  <- Logging.mkCardanoTracer
    stdoutTrace trForward mbTrEkg
    ["Net", "Peers", "PublicRoot"]
  Logging.configureTracers emptyConfigReflection traceConfig [dtTracePublicRootPeersTracer]
  !dtTraceLedgerPeersTracer <- Logging.mkCardanoTracer
    stdoutTrace trForward mbTrEkg
    ["Net", "Peers", "Ledger"]
  Logging.configureTracers emptyConfigReflection traceConfig [dtTraceLedgerPeersTracer]
  !dtTracePeerSelectionTracer <- Logging.mkCardanoTracer
    stdoutTrace trForward mbTrEkg
    ["Net", "PeerSelection", "Selection"]
  Logging.configureTracers emptyConfigReflection traceConfig [dtTracePeerSelectionTracer]
  !dtDebugPeerSelectionInitiatorTracer <- Logging.mkCardanoTracer
    stdoutTrace trForward mbTrEkg
    ["Net", "PeerSelection", "Initiator"]
  Logging.configureTracers emptyConfigReflection traceConfig [dtDebugPeerSelectionInitiatorTracer]
  !dtDebugPeerSelectionInitiatorResponderTracer <- Logging.mkCardanoTracer
    stdoutTrace trForward mbTrEkg
    ["Net", "PeerSelection", "Responder"]
  Logging.configureTracers emptyConfigReflection traceConfig [dtDebugPeerSelectionInitiatorResponderTracer]
  !dtTraceChurnCounters <- Logging.mkCardanoTracer
    stdoutTrace trForward mbTrEkg
    ["Net", "Churn"]
  Logging.configureTracers emptyConfigReflection traceConfig [dtTraceChurnCounters]
  !dtTracePeerSelectionCounters <- Logging.mkCardanoTracer
    stdoutTrace trForward mbTrEkg
    ["Net", "PeerSelection"]
  Logging.configureTracers emptyConfigReflection traceConfig [dtTracePeerSelectionCounters]
  !dtPeerSelectionActionsTracer <- Logging.mkCardanoTracer
    stdoutTrace trForward mbTrEkg
    ["Net", "PeerSelection", "Actions"]
  Logging.configureTracers emptyConfigReflection traceConfig [dtPeerSelectionActionsTracer]
  !dtConnectionManagerTracer <- Logging.mkCardanoTracer
    stdoutTrace trForward mbTrEkg
    ["Net", "ConnectionManager", "Remote"]
  Logging.configureTracers emptyConfigReflection traceConfig [dtConnectionManagerTracer]
  !dtConnectionManagerTransitionTracer <- Logging.mkCardanoTracer
    stdoutTrace trForward mbTrEkg
    ["Net", "ConnectionManager", "Transition"]
  Logging.configureTracers emptyConfigReflection traceConfig [dtConnectionManagerTransitionTracer]
  !dtServerTracer <- Logging.mkCardanoTracer
    stdoutTrace trForward mbTrEkg
    ["Net", "Server", "Local"]
  Logging.configureTracers emptyConfigReflection traceConfig [dtServerTracer]
  !dtInboundGovernorTracer <- Logging.mkCardanoTracer
    stdoutTrace trForward mbTrEkg
    ["Net", "InboundGovernor", "Remote"]
  Logging.configureTracers emptyConfigReflection traceConfig [dtInboundGovernorTracer]
  !dtInboundGovernorTransitionTracer <- Logging.mkCardanoTracer
    stdoutTrace trForward mbTrEkg
    ["Net", "InboundGovernor", "Transition"]
  Logging.configureTracers emptyConfigReflection traceConfig [dtInboundGovernorTransitionTracer]
  !dtLocalConnectionManagerTracer <- Logging.mkCardanoTracer
    stdoutTrace trForward Nothing -- never conflate metrics of the same name with those originating from `connectionManagerTr`
    ["Net", "ConnectionManager", "Local"]
  Logging.configureTracers emptyConfigReflection traceConfig [dtLocalConnectionManagerTracer]
  !dtLocalServerTracer <- Logging.mkCardanoTracer
    stdoutTrace trForward mbTrEkg
    ["Net", "Server", "Local"]
  Logging.configureTracers emptyConfigReflection traceConfig [dtLocalServerTracer]
  !dtLocalInboundGovernorTracer <- Logging.mkCardanoTracer
    stdoutTrace trForward mbTrEkg
    ["Net", "InboundGovernor", "Local"]
  Logging.configureTracers emptyConfigReflection traceConfig [dtLocalInboundGovernorTracer]
  !dtDnsTracer <- Logging.mkCardanoTracer
    stdoutTrace trForward mbTrEkg
    ["Net", "DNS"]
  Logging.configureTracers emptyConfigReflection traceConfig [dtDnsTracer]

  let dmqDifussionTracers =
        -- From `Cardano.Node.Tracing.Tracers`
        -- Branch "ana/10.6-final-integration-mix"
{--
Diffusion.Tracers {
  Diffusion.dtMuxTracer = Tracer $ traceWith dtMuxTracer
, Diffusion.dtChannelTracer = Tracer $ traceWith dtChannelTracer
, Diffusion.dtBearerTracer = Tracer $ traceWith dtBearerTracer
, Diffusion.dtHandshakeTracer = Tracer $ traceWith dtHandshakeTracer
, Diffusion.dtLocalMuxTracer = Tracer $ traceWith dtLocalMuxTracer
, Diffusion.dtLocalChannelTracer = Tracer $ traceWith dtLocalChannelTracer
, Diffusion.dtLocalBearerTracer = Tracer $ traceWith dtLocalBearerTracer
, Diffusion.dtLocalHandshakeTracer = Tracer $ traceWith dtLocalHandshakeTracer
, Diffusion.dtDiffusionTracer = Tracer $ traceWith dtDiffusionTracer
, Diffusion.dtTraceLocalRootPeersTracer = Tracer $ traceWith localRootPeersTr
, Diffusion.dtTracePublicRootPeersTracer = Tracer $ traceWith publicRootPeersTr
, Diffusion.dtTraceLedgerPeersTracer = Tracer $ traceWith dtTraceLedgerPeersTracer
, Diffusion.dtTracePeerSelectionTracer = Tracer $ traceWith dtTracePeerSelectionTracer
, Diffusion.dtDebugPeerSelectionInitiatorTracer = Tracer $ traceWith dtDebugPeerSelectionInitiatorTracer
, Diffusion.dtDebugPeerSelectionInitiatorResponderTracer = Tracer $ traceWith dtDebugPeerSelectionInitiatorResponderTracer

, Diffusion.dtTracePeerSelectionCounters = Tracer $ traceWith peerSelectionCountersTr
, Diffusion.dtTraceChurnCounters = Tracer $ traceWith churnCountersTr
, Diffusion.dtPeerSelectionActionsTracer = Tracer $ traceWith peerSelectionActionsTr
, Diffusion.dtConnectionManagerTracer = Tracer $ traceWith dtConnectionManagerTracer
, Diffusion.dtConnectionManagerTransitionTracer = Tracer $ traceWith dtConnectionManagerTransitionTracer
, Diffusion.dtServerTracer = Tracer $ traceWith serverTr
, Diffusion.dtInboundGovernorTracer = Tracer $ traceWith inboundGovernorTr
, Diffusion.dtInboundGovernorTransitionTracer = Tracer $ traceWith inboundGovernorTransitionsTr
, Diffusion.dtDnsTracer = Tracer $ traceWith dtDnsTr
, Diffusion.dtLocalConnectionManagerTracer =  Tracer $ traceWith localConnectionManagerTr
, Diffusion.dtLocalServerTracer = Tracer $ traceWith localServerTr
, Diffusion.dtLocalInboundGovernorTracer = Tracer $ traceWith localInboundGovernorTr
}
--}
        Diffusion.Tracers {
            Diffusion.dtMuxTracer                                  = Tracer $ Logging.traceWith dtMuxTracer,
            Diffusion.dtChannelTracer                              = Tracer $ Logging.traceWith dtChannelTracer,
            Diffusion.dtBearerTracer                               = Tracer $ Logging.traceWith dtBearerTracer,
            Diffusion.dtHandshakeTracer                            = Tracer $ Logging.traceWith dtHandshakeTracer,
            Diffusion.dtLocalMuxTracer                             = Tracer $ Logging.traceWith dtLocalMuxTracer,
            Diffusion.dtLocalChannelTracer                         = Tracer $ Logging.traceWith dtLocalChannelTracer,
            Diffusion.dtLocalBearerTracer                          = Tracer $ Logging.traceWith dtLocalBearerTracer,
            Diffusion.dtLocalHandshakeTracer                       = Tracer $ Logging.traceWith dtLocalHandshakeTracer,
            Diffusion.dtDiffusionTracer                            = Tracer $ Logging.traceWith dtDiffusionTracer,
            Diffusion.dtTraceLocalRootPeersTracer                  = Tracer $ Logging.traceWith dtTraceLocalRootPeersTracer,
            Diffusion.dtTracePublicRootPeersTracer                 = Tracer $ Logging.traceWith dtTracePublicRootPeersTracer,
            Diffusion.dtTraceLedgerPeersTracer                     = Tracer $ Logging.traceWith dtTraceLedgerPeersTracer,
            Diffusion.dtTracePeerSelectionTracer                   = Tracer $ Logging.traceWith dtTracePeerSelectionTracer,
            Diffusion.dtDebugPeerSelectionInitiatorTracer          = Tracer $ Logging.traceWith dtDebugPeerSelectionInitiatorTracer,
            Diffusion.dtDebugPeerSelectionInitiatorResponderTracer = Tracer $ Logging.traceWith dtDebugPeerSelectionInitiatorResponderTracer,
            Diffusion.dtTraceChurnCounters                         = Tracer $ Logging.traceWith dtTraceChurnCounters,
            Diffusion.dtTracePeerSelectionCounters                 = Tracer $ Logging.traceWith dtTracePeerSelectionCounters,
            Diffusion.dtPeerSelectionActionsTracer                 = Tracer $ Logging.traceWith dtPeerSelectionActionsTracer,
            Diffusion.dtConnectionManagerTracer                    = Tracer $ Logging.traceWith dtConnectionManagerTracer,
            Diffusion.dtConnectionManagerTransitionTracer          = Tracer $ Logging.traceWith dtConnectionManagerTransitionTracer,
            Diffusion.dtServerTracer                               = Tracer $ Logging.traceWith dtServerTracer,
            Diffusion.dtInboundGovernorTracer                      = Tracer $ Logging.traceWith dtInboundGovernorTracer,
            Diffusion.dtInboundGovernorTransitionTracer            = Tracer $ Logging.traceWith dtInboundGovernorTransitionTracer,
            Diffusion.dtLocalConnectionManagerTracer               = Tracer $ Logging.traceWith dtLocalConnectionManagerTracer,
            Diffusion.dtLocalServerTracer                          = Tracer $ Logging.traceWith dtLocalServerTracer,
            Diffusion.dtLocalInboundGovernorTracer                 = Tracer $ Logging.traceWith dtLocalInboundGovernorTracer,
            Diffusion.dtDnsTracer                                  = Tracer $ Logging.traceWith dtDnsTracer
          }

  return (dmqTracer, dmqDifussionTracers)

-- An orphan instance needed for `Handshake versionNumber Term`
instance ToJSON Term where
  toJSON term = String (Text.pack . show $ term)

data NoExtraPeers = NoExtraPeers
instance Semigroup NoExtraPeers where
  _ <> _ = NoExtraPeers
instance Monoid NoExtraPeers where
  mempty = NoExtraPeers
instance Show NoExtraPeers where
  show _ = ""
instance ToJSON NoExtraPeers where
  toJSON _ = Null
  omitField _ = True

instance ToJSON (PublicRootPeers NoExtraPeers RemoteAddress) where
  toJSON prp =
    object [ "kind"              .= String "PublicRootPeers"
           , "ledgerPeers"       .= PublicRootPeers.getLedgerPeers prp
           , "bigLedgerPeers"    .= PublicRootPeers.getBigLedgerPeers prp
           ]

data NoExtraState      = NoExtraState
data NoExtraCounters   = NoExtraCounters   deriving Eq
data NoExtraDebugState = NoExtraDebugState
instance ToJSON NoExtraDebugState where
  toJSON _ = Null
  omitField _ = True
data NoExtraChurnArgs  = NoExtraChurnArgs
data NoExtraAPI        = NoExtraAPI
data NoExtraTracer     = NoExtraTracer
instance Show NoExtraState where
  show _ = ""
instance Show NoExtraDebugState where
  show _ = ""
instance Show NoExtraTracer where
  show _ = ""
instance ToJSON NoExtraTracer where
  toJSON _ = Null
  omitField _ = True

instance ToJSON (Governor.PeerSelectionCounters NoExtraCounters) where
  toJSON Governor.PeerSelectionCounters {..} =
    object [ "kind" .= String "PeerSelectionCounters"

           , "knownPeers" .= numberOfKnownPeers
           , "rootPeers" .= numberOfRootPeers
           , "coldPeersPromotions" .= numberOfColdPeersPromotions
           , "establishedPeers" .= numberOfEstablishedPeers
           , "warmPeersDemotions" .= numberOfWarmPeersDemotions
           , "warmPeersPromotions" .= numberOfWarmPeersPromotions
           , "activePeers" .= numberOfActivePeers
           , "activePeersDemotions" .= numberOfActivePeersDemotions

           , "knownBigLedgerPeers" .= numberOfKnownBigLedgerPeers
           , "coldBigLedgerPeersPromotions" .= numberOfColdBigLedgerPeersPromotions
           , "establishedBigLedgerPeers" .= numberOfEstablishedBigLedgerPeers
           , "warmBigLedgerPeersDemotions" .= numberOfWarmBigLedgerPeersDemotions
           , "warmBigLedgerPeersPromotions" .= numberOfWarmBigLedgerPeersPromotions
           , "activeBigLedgerPeers" .= numberOfActiveBigLedgerPeers
           , "activeBigLedgerPeersDemotions" .= numberOfActiveBigLedgerPeersDemotions

           , "knownLocalRootPeers" .= numberOfKnownLocalRootPeers
           , "establishedLocalRootPeers" .= numberOfEstablishedLocalRootPeers
           , "warmLocalRootPeersPromotions" .= numberOfWarmLocalRootPeersPromotions
           , "activeLocalRootPeers" .= numberOfActiveLocalRootPeers
           , "activeLocalRootPeersDemotions" .= numberOfActiveLocalRootPeersDemotions

           , "knownNonRootPeers" .= numberOfKnownNonRootPeers
           , "coldNonRootPeersPromotions" .= numberOfColdNonRootPeersPromotions
           , "establishedNonRootPeers" .= numberOfEstablishedNonRootPeers
           , "warmNonRootPeersDemotions" .= numberOfWarmNonRootPeersDemotions
           , "warmNonRootPeersPromotions" .= numberOfWarmNonRootPeersPromotions
           , "activeNonRootPeers" .= numberOfActiveNonRootPeers
           , "activeNonRootPeersDemotions" .= numberOfActiveNonRootPeersDemotions
           ]

instance ToJSON (DebugPeerSelection NoExtraState NoExtraFlags NoExtraPeers RemoteAddress) where
  toJSON (TraceGovernorState blockedAt wakeupAfter st@Governor.PeerSelectionState { Governor.targets }) =
    object [ "kind"        .= String "DebugPeerSelection"
           , "blockedAt"   .= String (Text.pack . show $ blockedAt)
           , "wakeupAfter" .= String (Text.pack . show $ wakeupAfter)
           , "targets"     .= targets
           , "counters"    .= Governor.peerSelectionStateToCounters
                                (const Set.empty)
                                (const NoExtraCounters)
                                st
           ]

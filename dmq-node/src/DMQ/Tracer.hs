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
import Data.Functor.Contravariant ((>$<))
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)

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

data EventType =
    Mux
  | Channel
  | Bearer
  | Handshake
  | LocalMux
  | LocalChannel
  | LocalBearer
  | LocalHandshake
  | Diffusion
  | LocalRootPeers
  | PublicRootPeers
  | LedgerPeers
  | PeerSelection
  | ChurnCounters
  | DebugPeerSelectionInitiator
  | DebugPeerSelectionInitiatorResponder
  | PeerSelectionCounters
  | PeerSelectionActions
  | ConnectionManager
  | ConnectionManagerTransition
  | Server
  | InboundGovernor
  | InboundGovernorTransition
  | LocalConnectionManager
  | LocalServer
  | LocalInboundGovernor
  | Dns
  | DMQ String
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
    -- Diffusion traces.
    --------------------
      Logging.Namespace [] ["Mux"]
    , Logging.Namespace [] ["Channel"]
    , Logging.Namespace [] ["Bearer"]
    , Logging.Namespace [] ["Handshake"]
    , Logging.Namespace [] ["LocalMux"]
    , Logging.Namespace [] ["LocalChannel"]
    , Logging.Namespace [] ["LocalBearer"]
    , Logging.Namespace [] ["LocalHandshake"]
    , Logging.Namespace [] ["Diffusion"]
    , Logging.Namespace [] ["LocalRootPeers"]
    , Logging.Namespace [] ["PublicRootPeers"]
    , Logging.Namespace [] ["LedgerPeers"]
    , Logging.Namespace [] ["PeerSelection"]
    , Logging.Namespace [] ["ChurnCounters"]
    , Logging.Namespace [] ["DebugPeerSelectionInitiator"]
    , Logging.Namespace [] ["DebugPeerSelectionInitiatorResponder"]
    , Logging.Namespace [] ["PeerSelectionCounters"]
    , Logging.Namespace [] ["PeerSelectionActions"]
    , Logging.Namespace [] ["ConnectionManager"]
    , Logging.Namespace [] ["ConnectionManagerTransition"]
    , Logging.Namespace [] ["Server"]
    , Logging.Namespace [] ["InboundGovernor"]
    , Logging.Namespace [] ["InboundGovernorTransition"]
    , Logging.Namespace [] ["LocalConnectionManager"]
    , Logging.Namespace [] ["LocalServer"]
    , Logging.Namespace [] ["LocalInboundGovernor"]
    , Logging.Namespace [] ["DnsTracer"]
    -- DMQ-Node only traces.
    ------------------------
    -- Main.
    , Logging.Namespace [] ["Configuration"]
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
  severityFor (Logging.Namespace [] ["Mux"]) _ = Just Logging.Info
  severityFor (Logging.Namespace [] ["Channel"]) _ = Just Logging.Info
  severityFor (Logging.Namespace [] ["Bearer"]) _ = Just Logging.Info
  severityFor (Logging.Namespace [] ["Handshake"]) _ = Just Logging.Info
  severityFor (Logging.Namespace [] ["LocalMux"]) _ = Just Logging.Info
  severityFor (Logging.Namespace [] ["LocalChannel"]) _ = Just Logging.Info
  severityFor (Logging.Namespace [] ["LocalBearer"]) _ = Just Logging.Info
  severityFor (Logging.Namespace [] ["LocalHandshake"]) _ = Just Logging.Info
  severityFor (Logging.Namespace [] ["Diffusion"]) _ = Just Logging.Info
  severityFor (Logging.Namespace [] ["LocalRootPeers"]) _ = Just Logging.Info
  severityFor (Logging.Namespace [] ["PublicRootPeers"]) _ = Just Logging.Info
  severityFor (Logging.Namespace [] ["LedgerPeers"]) _ = Just Logging.Info
  severityFor (Logging.Namespace [] ["PeerSelection"]) _ = Just Logging.Info
  severityFor (Logging.Namespace [] ["ChurnCounters"]) _ = Just Logging.Info
  severityFor (Logging.Namespace [] ["DebugPeerSelectionInitiator"]) _ = Just Logging.Info
  severityFor (Logging.Namespace [] ["DebugPeerSelectionInitiatorResponder"]) _ = Just Logging.Info
  severityFor (Logging.Namespace [] ["PeerSelectionCounters"]) _ = Just Logging.Info
  severityFor (Logging.Namespace [] ["PeerSelectionActions"]) _ = Just Logging.Info
  severityFor (Logging.Namespace [] ["ConnectionManager"]) _ = Just Logging.Info
  severityFor (Logging.Namespace [] ["ConnectionManagerTransition"]) _ = Just Logging.Info
  severityFor (Logging.Namespace [] ["Server"]) _ = Just Logging.Info
  severityFor (Logging.Namespace [] ["InboundGovernor"]) _ = Just Logging.Info
  severityFor (Logging.Namespace [] ["InboundGovernorTransition"]) _ = Just Logging.Info
  severityFor (Logging.Namespace [] ["LocalConnectionManager"]) _ = Just Logging.Info
  severityFor (Logging.Namespace [] ["LocalServerTracer"]) _ = Just Logging.Info
  severityFor (Logging.Namespace [] ["LocalInboundGovernor"]) _ = Just Logging.Info
  severityFor (Logging.Namespace [] ["Dns"]) _ = Just Logging.Info
  severityFor _ _ = Just Logging.Info
  privacyFor _ _ =  Just Logging.Public
  detailsFor (Logging.Namespace [] ["Mux"]) _ = Just Logging.DNormal
  detailsFor (Logging.Namespace [] ["Channel"]) _ = Just Logging.DNormal
  detailsFor (Logging.Namespace [] ["Bearer"]) _ = Just Logging.DNormal
  detailsFor (Logging.Namespace [] ["Handshake"]) _ = Just Logging.DNormal
  detailsFor (Logging.Namespace [] ["LocalMux"]) _ = Just Logging.DNormal
  detailsFor (Logging.Namespace [] ["LocalChannel"]) _ = Just Logging.DNormal
  detailsFor (Logging.Namespace [] ["LocalBearer"]) _ = Just Logging.DNormal
  detailsFor (Logging.Namespace [] ["LocalHandshake"]) _ = Just Logging.DNormal
  detailsFor (Logging.Namespace [] ["Diffusion"]) _ = Just Logging.DNormal
  detailsFor (Logging.Namespace [] ["LocalRootPeers"]) _ = Just Logging.DNormal
  detailsFor (Logging.Namespace [] ["PublicRootPeers"]) _ = Just Logging.DNormal
  detailsFor (Logging.Namespace [] ["LedgerPeers"]) _ = Just Logging.DNormal
  detailsFor (Logging.Namespace [] ["PeerSelection"]) _ = Just Logging.DNormal
  detailsFor (Logging.Namespace [] ["ChurnCounters"]) _ = Just Logging.DNormal
  detailsFor (Logging.Namespace [] ["DebugPeerSelectionInitiator"]) _ = Just Logging.DNormal
  detailsFor (Logging.Namespace [] ["DebugPeerSelectionInitiatorResponder"]) _ = Just Logging.DNormal
  detailsFor (Logging.Namespace [] ["PeerSelectionCounters"]) _ = Just Logging.DNormal
  detailsFor (Logging.Namespace [] ["PeerSelectionActions"]) _ = Just Logging.DNormal
  detailsFor (Logging.Namespace [] ["ConnectionManager"]) _ = Just Logging.DNormal
  detailsFor (Logging.Namespace [] ["ConnectionManagerTransition"]) _ = Just Logging.DNormal
  detailsFor (Logging.Namespace [] ["Server"]) _ = Just Logging.DNormal
  detailsFor (Logging.Namespace [] ["InboundGovernor"]) _ = Just Logging.DNormal
  detailsFor (Logging.Namespace [] ["InboundGovernorTransition"]) _ = Just Logging.DNormal
  detailsFor (Logging.Namespace [] ["LocalConnectionManager"]) _ = Just Logging.DNormal
  detailsFor (Logging.Namespace [] ["LocalServer"]) _ = Just Logging.DNormal
  detailsFor (Logging.Namespace [] ["LocalInboundGovernor"]) _ = Just Logging.DNormal
  detailsFor (Logging.Namespace [] ["Dns"]) _ = Just Logging.DNormal
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
  return (dmqTracer, dmqDiffusionTracers dmqTracer)

-- An orphan instance needed for `Handshake versionNumber Term`
instance ToJSON Term where
  toJSON term = String (Text.pack . show $ term)

data NoExtraPeers = NoExtraPeers
instance Semigroup NoExtraPeers where
  _ <> _ = NoExtraPeers
instance Monoid NoExtraPeers where
  mempty = NoExtraPeers

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

dmqDiffusionTracers
  :: (Tracer IO WithEventType)
  -> Diffusion.Tracers RemoteAddress NodeToNodeVersion   NodeToNodeVersionData
                       LocalAddress  NodeToClientVersion NodeToClientVersionData
                       NoExtraState
                       NoExtraDebugState
                       NoExtraFlags
                       NoExtraPeers
                       NoExtraCounters
                       NoExtraTracer
                       IO
dmqDiffusionTracers tracer =
  Diffusion.Tracers {
    Diffusion.dtMuxTracer                                  = WithEventType Mux >$< tracer,
    Diffusion.dtChannelTracer                              = WithEventType Channel >$< tracer,
    Diffusion.dtBearerTracer                               = WithEventType Bearer >$< tracer,
    Diffusion.dtHandshakeTracer                            = WithEventType Handshake >$< tracer,
    Diffusion.dtLocalMuxTracer                             = WithEventType LocalMux >$< tracer,
    Diffusion.dtLocalChannelTracer                         = WithEventType LocalChannel >$< tracer,
    Diffusion.dtLocalBearerTracer                          = WithEventType LocalBearer >$< tracer,
    Diffusion.dtLocalHandshakeTracer                       = WithEventType LocalHandshake >$< tracer,
    Diffusion.dtDiffusionTracer                            = WithEventType Diffusion >$< tracer,
    Diffusion.dtTraceLocalRootPeersTracer                  = WithEventType LocalRootPeers >$< tracer,
    Diffusion.dtTracePublicRootPeersTracer                 = WithEventType PublicRootPeers >$< tracer,
    Diffusion.dtTraceLedgerPeersTracer                     = WithEventType LedgerPeers >$< tracer,
    Diffusion.dtTracePeerSelectionTracer                   = WithEventType PeerSelection >$< tracer,
    Diffusion.dtTraceChurnCounters                         = WithEventType ChurnCounters >$< tracer,
    Diffusion.dtDebugPeerSelectionInitiatorTracer          = WithEventType DebugPeerSelectionInitiator >$< tracer,
    Diffusion.dtDebugPeerSelectionInitiatorResponderTracer = WithEventType DebugPeerSelectionInitiatorResponder >$< tracer,
    Diffusion.dtTracePeerSelectionCounters                 = WithEventType PeerSelectionCounters >$< tracer,
    Diffusion.dtPeerSelectionActionsTracer                 = WithEventType PeerSelectionActions >$< tracer,
    Diffusion.dtConnectionManagerTracer                    = WithEventType ConnectionManager >$< tracer,
    Diffusion.dtConnectionManagerTransitionTracer          = WithEventType ConnectionManagerTransition >$< tracer,
    Diffusion.dtServerTracer                               = WithEventType Server >$< tracer,
    Diffusion.dtInboundGovernorTracer                      = WithEventType InboundGovernor >$< tracer,
    Diffusion.dtInboundGovernorTransitionTracer            = WithEventType InboundGovernorTransition >$< tracer,
    Diffusion.dtLocalConnectionManagerTracer               = WithEventType LocalConnectionManager >$< tracer,
    Diffusion.dtLocalServerTracer                          = WithEventType LocalServer >$< tracer,
    Diffusion.dtLocalInboundGovernorTracer                 = WithEventType LocalInboundGovernor >$< tracer,
    Diffusion.dtDnsTracer                                  = WithEventType Dns >$< tracer
  }


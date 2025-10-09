{-# LANGUAGE OverloadedStrings   #-}

module DMQ.Tracer.Translation (WithEventType (..)) where

import qualified Cardano.Logging as Logging

{-- Needed / copied from "module Ouroboros.Network.Diffusion.Types" --}
import qualified Codec.CBOR.Term as CBOR
import qualified Network.Mux as Mx
import           Ouroboros.Network.ConnectionHandler (ConnectionHandlerTrace)
import           Ouroboros.Network.ConnectionId (ConnectionId)
import qualified Ouroboros.Network.ConnectionManager.Core as CM
import qualified Ouroboros.Network.ConnectionManager.State as CM
import           Ouroboros.Network.ConnectionManager.Types (AbstractTransitionTrace)
import qualified Ouroboros.Network.Diffusion.Types as Diffusion
import           Ouroboros.Network.Driver.Simple (TraceSendRecv)
import qualified Ouroboros.Network.InboundGovernor as IG
import qualified Ouroboros.Network.PeerSelection as PeerSelection
import           Ouroboros.Network.PeerSelection.LedgerPeers (TraceLedgerPeers)
--import           Ouroboros.Network.PeerSelection.Governor.EstablishedPeers (PeerSelectionCounters)
import           Ouroboros.Network.PeerSelection.Governor.Types (DebugPeerSelection, PeerSelectionCounters, 
  TracePeerSelection)
import           Ouroboros.Network.PeerSelection.RootPeersDNS.LocalRootPeers (TraceLocalRootPeers)
import           Ouroboros.Network.PeerSelection.RootPeersDNS.PublicRootPeers (TracePublicRootPeers)
import           Ouroboros.Network.PeerSelection.PeerStateActions (PeerSelectionActionsTrace)
import           Ouroboros.Network.PeerSelection.RootPeersDNS.DNSActions (DNSTrace)
import           Ouroboros.Network.Protocol.Handshake (Handshake)
import qualified Ouroboros.Network.Server as Server
-- Others.
import qualified Data.Text as Text
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
-- Me:
import DMQ.Tracer.Types ()

data WithEventType
       ntnAddr ntnVersion ntnVersionData
       ntcAddr ntcVersion ntcVersionData
       extraState extraDebugState
       extraFlags extraPeers extraCounters =
    MuxTracer (Mx.WithBearer (ConnectionId ntnAddr) Mx.Trace)
  | ChannelTracer (Mx.WithBearer (ConnectionId ntnAddr) Mx.ChannelTrace)
  | BearerTracer (Mx.WithBearer (ConnectionId ntnAddr) Mx.BearerTrace)
  | HandshakeTracer (Mx.WithBearer (ConnectionId ntnAddr) (TraceSendRecv (Handshake ntnVersion CBOR.Term)))
  | LocalMuxTracer (Mx.WithBearer (ConnectionId ntcAddr) Mx.Trace)
  | LocalChannelTracer (Mx.WithBearer (ConnectionId ntcAddr) Mx.ChannelTrace)
  | LocalBearerTracer (Mx.WithBearer (ConnectionId ntcAddr) Mx.BearerTrace)
  | LocalHandshakeTracer (Mx.WithBearer (ConnectionId ntcAddr) (TraceSendRecv (Handshake ntcVersion CBOR.Term)))
  | DiffusionTracer (Diffusion.DiffusionTracer ntnAddr ntcAddr)
  | TraceLocalRootPeersTracer (TraceLocalRootPeers extraFlags ntnAddr)
  | TracePublicRootPeersTracer TracePublicRootPeers
  | TraceLedgerPeersTracer TraceLedgerPeers
  | TracePeerSelectionTracer (TracePeerSelection extraDebugState extraFlags extraPeers ntnAddr)
  | DebugPeerSelectionInitiatorTracer (DebugPeerSelection extraState extraFlags extraPeers ntnAddr)
  | DebugPeerSelectionInitiatorResponderTracer (DebugPeerSelection extraState extraFlags extraPeers ntnAddr)
  | TracePeerSelectionCounters (PeerSelectionCounters extraCounters)
  | TraceChurnCounters PeerSelection.ChurnCounters
  | PeerSelectionActionsTracer (PeerSelectionActionsTrace ntnAddr ntnVersion)
  | ConnectionManagerTracer (CM.Trace ntnAddr (ConnectionHandlerTrace ntnVersion ntnVersionData))
  | ConnectionManagerTransitionTracer (AbstractTransitionTrace CM.ConnStateId)
  | ServerTracer (Server.Trace ntnAddr)
  | InboundGovernorTracer (IG.Trace ntnAddr)
  | InboundGovernorTransitionTracer (IG.RemoteTransitionTrace ntnAddr)
  | DnsTracer DNSTrace
  | LocalConnectionManagerTracer (CM.Trace ntcAddr (ConnectionHandlerTrace ntcVersion ntcVersionData))
  | LocalServerTracer (Server.Trace ntcAddr)
  | LocalInboundGovernorTracer (IG.Trace ntcAddr)
  | forall a. Aeson.ToJSON a => WithEventType String a

instance Logging.LogFormatting (WithEventType ntnAddr ntnVersion ntnVersionData ntcAddr ntcVersion ntcVersionData extraState extraDebugState extraFlags extraPeers extraCounters) where
  -- Machine readable representation with varying details based on the detail level.
  -- forMachine :: DetailLevel -> a -> Aeson.Object
  forMachine dl (MuxTracer a) = Logging.forMachine dl a
  forMachine dl (ChannelTracer a) = Logging.forMachine dl a
  forMachine dl (BearerTracer a) = Logging.forMachine dl a
  forMachine dl (HandshakeTracer a) = Logging.forMachine dl a
  forMachine dl (LocalMuxTracer a) = Logging.forMachine dl a
  forMachine dl (LocalChannelTracer a) = Logging.forMachine dl a
  forMachine dl (LocalBearerTracer a) = Logging.forMachine dl a
  forMachine dl (LocalHandshakeTracer a) = Logging.forMachine dl a
  forMachine dl (DiffusionTracer a) = Logging.forMachine dl a
  forMachine dl (TraceLocalRootPeersTracer a) = Logging.forMachine dl a
  forMachine dl (TracePublicRootPeersTracer a) = Logging.forMachine dl a
  forMachine dl (TraceLedgerPeersTracer a) = Logging.forMachine dl a
  forMachine dl (TracePeerSelectionTracer a) = Logging.forMachine dl a
  forMachine dl (DebugPeerSelectionInitiatorTracer a) = Logging.forMachine dl a
  forMachine dl (DebugPeerSelectionInitiatorResponderTracer a) = Logging.forMachine dl a
  forMachine dl (TracePeerSelectionCounters a) = Logging.forMachine dl a
  forMachine dl (TraceChurnCounters a) = Logging.forMachine dl a
  forMachine dl (PeerSelectionActionsTracer a) = Logging.forMachine dl a
  forMachine dl (ConnectionManagerTracer a) = Logging.forMachine dl a
  forMachine dl (ConnectionManagerTransitionTracer a) = Logging.forMachine dl a
  forMachine dl (ServerTracer a) = Logging.forMachine dl a
  forMachine dl (InboundGovernorTracer a) = Logging.forMachine dl a
  forMachine dl (InboundGovernorTransitionTracer a) = Logging.forMachine dl a
  forMachine dl (DnsTracer a) = Logging.forMachine dl a
  forMachine dl (LocalConnectionManagerTracer a) = Logging.forMachine dl a
  forMachine dl (LocalServerTracer a) = Logging.forMachine dl a
  forMachine dl (LocalInboundGovernorTracer a) = Logging.forMachine dl a
  forMachine _ (WithEventType _ event) = KeyMap.fromList [ ("data", Aeson.toJSON event) ]
  -- Human readable representation.
  -- forHuman :: a -> Text
  forHuman (MuxTracer a) = Logging.forHuman a
  forHuman (ChannelTracer a) = Logging.forHuman a
  forHuman (BearerTracer a) = Logging.forHuman a
  forHuman (HandshakeTracer a) = Logging.forHuman a
  forHuman (LocalMuxTracer a) = Logging.forHuman a
  forHuman (LocalChannelTracer a) = Logging.forHuman a
  forHuman (LocalBearerTracer a) = Logging.forHuman a
  forHuman (LocalHandshakeTracer a) = Logging.forHuman a
  forHuman (DiffusionTracer a) = Logging.forHuman a
  forHuman (TraceLocalRootPeersTracer a) = Logging.forHuman a
  forHuman (TracePublicRootPeersTracer a) = Logging.forHuman a
  forHuman (TraceLedgerPeersTracer a) = Logging.forHuman a
  forHuman (TracePeerSelectionTracer a) = Logging.forHuman a
  forHuman (DebugPeerSelectionInitiatorTracer a) = Logging.forHuman a
  forHuman (DebugPeerSelectionInitiatorResponderTracer a) = Logging.forHuman a
  forHuman (TracePeerSelectionCounters a) = Logging.forHuman a
  forHuman (TraceChurnCounters a) = Logging.forHuman a
  forHuman (PeerSelectionActionsTracer a) = Logging.forHuman a
  forHuman (ConnectionManagerTracer a) = Logging.forHuman a
  forHuman (ConnectionManagerTransitionTracer a) = Logging.forHuman a
  forHuman (ServerTracer a) = Logging.forHuman a
  forHuman (InboundGovernorTracer a) = Logging.forHuman a
  forHuman (InboundGovernorTransitionTracer a) = Logging.forHuman a
  forHuman (DnsTracer a) = Logging.forHuman a
  forHuman (LocalConnectionManagerTracer a) = Logging.forHuman a
  forHuman (LocalServerTracer a) = Logging.forHuman a
  forHuman (LocalInboundGovernorTracer a) = Logging.forHuman a
  forHuman _ = Text.pack ""
  -- Metrics representation.
  -- asMetrics :: a -> [Metric]
  asMetrics _ = []

instance Logging.MetaTrace (WithEventType ntnAddr ntnVersion ntnVersionData ntcAddr ntcVersion ntcVersionData extraState extraDebugState extraFlags extraPeers extraCounters) where
  -- allNamespaces :: [Namespace a]
  allNamespaces = [
      Logging.Namespace [] [Text.pack "Mux"]
    , Logging.Namespace [] [Text.pack "Channel"]
    , Logging.Namespace [] [Text.pack "Bearer"]
    , Logging.Namespace [] [Text.pack "Handshake"]
    , Logging.Namespace [] [Text.pack "LocalMux"]
    , Logging.Namespace [] [Text.pack "LocalChannel"]
    , Logging.Namespace [] [Text.pack "LocalBearer"]
    , Logging.Namespace [] [Text.pack "LocalHandshake"]
    , Logging.Namespace [] [Text.pack "Diffusion"]
    , Logging.Namespace [] [Text.pack "LocalRootPeers"]
    , Logging.Namespace [] [Text.pack "PublicRootPeers"]
    , Logging.Namespace [] [Text.pack "LedgerPeers"]
    , Logging.Namespace [] [Text.pack "PeerSelection"]
    , Logging.Namespace [] [Text.pack "DebugPeerSelectionInitiator"]
    , Logging.Namespace [] [Text.pack "DebugPeerSelectionInitiatorResponder"]
    , Logging.Namespace [] [Text.pack "PeerSelectionCounters"]
    , Logging.Namespace [] [Text.pack "ChurnCounters"]
    , Logging.Namespace [] [Text.pack "PeerSelectionActions"]
    , Logging.Namespace [] [Text.pack "ConnectionManager"]
    , Logging.Namespace [] [Text.pack "ConnectionManagerTransition"]
    , Logging.Namespace [] [Text.pack "Server"]
    , Logging.Namespace [] [Text.pack "InboundGovernor"]
    , Logging.Namespace [] [Text.pack "InboundGovernorTransition"]
    , Logging.Namespace [] [Text.pack "dtDnsTracer"]
    , Logging.Namespace [] [Text.pack "dtLocalConnectionManagerTracer"]
    , Logging.Namespace [] [Text.pack "dtLocalServerTracer"]
    , Logging.Namespace [] [Text.pack "dtLocalInboundGovernorTracer"]
    ]
  namespaceFor (MuxTracer a) = Logging.nsCast $ Logging.namespaceFor a
  namespaceFor (ChannelTracer a) = Logging.nsCast $ Logging.namespaceFor a
  namespaceFor (BearerTracer a) = Logging.nsCast $ Logging.namespaceFor a
  namespaceFor (HandshakeTracer a) = Logging.nsCast $ Logging.namespaceFor a
  namespaceFor (LocalMuxTracer a) = Logging.nsCast $ Logging.namespaceFor a
  namespaceFor (LocalChannelTracer a) = Logging.nsCast $ Logging.namespaceFor a
  namespaceFor (LocalBearerTracer a) = Logging.nsCast $ Logging.namespaceFor a
  namespaceFor (LocalHandshakeTracer a) = Logging.nsCast $ Logging.namespaceFor a
  namespaceFor (DiffusionTracer a) = Logging.nsCast $ Logging.namespaceFor a
  namespaceFor (TraceLocalRootPeersTracer a) = Logging.nsCast $ Logging.namespaceFor a
  namespaceFor (TracePublicRootPeersTracer a) = Logging.nsCast $ Logging.namespaceFor a
  namespaceFor (TraceLedgerPeersTracer a) = Logging.nsCast $ Logging.namespaceFor a
  namespaceFor (TracePeerSelectionTracer a) = Logging.nsCast $ Logging.namespaceFor a
  namespaceFor (DebugPeerSelectionInitiatorTracer a) = Logging.nsCast $ Logging.namespaceFor a
  namespaceFor (DebugPeerSelectionInitiatorResponderTracer a) = Logging.nsCast $ Logging.namespaceFor a
  namespaceFor (TracePeerSelectionCounters a) = Logging.nsCast $ Logging.namespaceFor a
  namespaceFor (TraceChurnCounters a) = Logging.nsCast $ Logging.namespaceFor a
  namespaceFor (PeerSelectionActionsTracer a) = Logging.nsCast $ Logging.namespaceFor a
  namespaceFor (ConnectionManagerTracer a) = Logging.nsCast $ Logging.namespaceFor a
  namespaceFor (ConnectionManagerTransitionTracer a) = Logging.nsCast $ Logging.namespaceFor a
  namespaceFor (ServerTracer a) = Logging.nsCast $ Logging.namespaceFor a
  namespaceFor (InboundGovernorTracer a) = Logging.nsCast $ Logging.namespaceFor a
  namespaceFor (InboundGovernorTransitionTracer a) = Logging.nsCast $ Logging.namespaceFor a
  namespaceFor (DnsTracer a) = Logging.nsCast $ Logging.namespaceFor a
  namespaceFor (LocalConnectionManagerTracer a) = Logging.nsCast $ Logging.namespaceFor a
  namespaceFor (LocalServerTracer a) = Logging.nsCast $ Logging.namespaceFor a
  namespaceFor (LocalInboundGovernorTracer a) = Logging.nsCast $ Logging.namespaceFor a
  namespaceFor (WithEventType str _) = Logging.Namespace [] [(Text.pack str)]
  severityFor ns Nothing = Logging.severityFor (Logging.nsCast ns :: Logging.Namespace tr) Nothing
  severityFor ns (Just (MuxTracer a)) = Logging.severityFor (Logging.nsCast ns) (Just a)
  severityFor ns (Just (MuxTracer a)) = Logging.severityFor (Logging.nsCast ns) (Just a)
  severityFor ns (Just (ChannelTracer a)) = Logging.severityFor (Logging.nsCast ns) (Just a)
  severityFor ns (Just (BearerTracer a)) = Logging.severityFor (Logging.nsCast ns) (Just a)
  severityFor ns (Just (HandshakeTracer a)) = Logging.severityFor (Logging.nsCast ns) (Just a)
  severityFor ns (Just (LocalMuxTracer a)) = Logging.severityFor (Logging.nsCast ns) (Just a)
  severityFor ns (Just (LocalChannelTracer a)) = Logging.severityFor (Logging.nsCast ns) (Just a)
  severityFor ns (Just (LocalBearerTracer a)) = Logging.severityFor (Logging.nsCast ns) (Just a)
  severityFor ns (Just (LocalHandshakeTracer a)) = Logging.severityFor (Logging.nsCast ns) (Just a)
  severityFor ns (Just (DiffusionTracer a)) = Logging.severityFor (Logging.nsCast ns) (Just a)
  severityFor ns (Just (TraceLocalRootPeersTracer a)) = Logging.severityFor (Logging.nsCast ns) (Just a)
  severityFor ns (Just (TracePublicRootPeersTracer a)) = Logging.severityFor (Logging.nsCast ns) (Just a)
  severityFor ns (Just (TraceLedgerPeersTracer a)) = Logging.severityFor (Logging.nsCast ns) (Just a)
  severityFor ns (Just (TracePeerSelectionTracer a)) = Logging.severityFor (Logging.nsCast ns) (Just a)
  severityFor ns (Just (DebugPeerSelectionInitiatorTracer a)) = Logging.severityFor (Logging.nsCast ns) (Just a)
  severityFor ns (Just (DebugPeerSelectionInitiatorResponderTracer a)) = Logging.severityFor (Logging.nsCast ns) (Just a)
  severityFor ns (Just (TracePeerSelectionCounters a)) = Logging.severityFor (Logging.nsCast ns) (Just a)
  severityFor ns (Just (TraceChurnCounters a)) = Logging.severityFor (Logging.nsCast ns) (Just a)
  severityFor ns (Just (PeerSelectionActionsTracer a)) = Logging.severityFor (Logging.nsCast ns) (Just a)
  severityFor ns (Just (ConnectionManagerTracer a)) = Logging.severityFor (Logging.nsCast ns) (Just a)
  severityFor ns (Just (ConnectionManagerTransitionTracer a)) = Logging.severityFor (Logging.nsCast ns) (Just a)
  severityFor ns (Just (ServerTracer a)) = Logging.severityFor (Logging.nsCast ns) (Just a)
  severityFor ns (Just (InboundGovernorTracer a)) = Logging.severityFor (Logging.nsCast ns) (Just a)
  severityFor ns (Just (InboundGovernorTransitionTracer a)) = Logging.severityFor (Logging.nsCast ns) (Just a)
  severityFor ns (Just (DnsTracer a)) = Logging.severityFor (Logging.nsCast ns) (Just a)
  severityFor ns (Just (LocalConnectionManagerTracer a)) = Logging.severityFor (Logging.nsCast ns) (Just a)
  severityFor ns (Just (LocalServerTracer a)) = Logging.severityFor (Logging.nsCast ns) (Just a)
  severityFor ns (Just (LocalInboundGovernorTracer a)) = Logging.severityFor (Logging.nsCast ns) (Just a)
  severityFor _ _ = Just Logging.Info
  privacyFor ns Nothing = Logging.privacyFor (Logging.nsCast ns :: Logging.Namespace tr) Nothing
  privacyFor ns (Just (MuxTracer a)) = Logging.privacyFor (Logging.nsCast ns) (Just a)
  privacyFor ns (Just (MuxTracer a)) = Logging.privacyFor (Logging.nsCast ns) (Just a)
  privacyFor ns (Just (ChannelTracer a)) = Logging.privacyFor (Logging.nsCast ns) (Just a)
  privacyFor ns (Just (BearerTracer a)) = Logging.privacyFor (Logging.nsCast ns) (Just a)
  privacyFor ns (Just (HandshakeTracer a)) = Logging.privacyFor (Logging.nsCast ns) (Just a)
  privacyFor ns (Just (LocalMuxTracer a)) = Logging.privacyFor (Logging.nsCast ns) (Just a)
  privacyFor ns (Just (LocalChannelTracer a)) = Logging.privacyFor (Logging.nsCast ns) (Just a)
  privacyFor ns (Just (LocalBearerTracer a)) = Logging.privacyFor (Logging.nsCast ns) (Just a)
  privacyFor ns (Just (LocalHandshakeTracer a)) = Logging.privacyFor (Logging.nsCast ns) (Just a)
  privacyFor ns (Just (DiffusionTracer a)) = Logging.privacyFor (Logging.nsCast ns) (Just a)
  privacyFor ns (Just (TraceLocalRootPeersTracer a)) = Logging.privacyFor (Logging.nsCast ns) (Just a)
  privacyFor ns (Just (TracePublicRootPeersTracer a)) = Logging.privacyFor (Logging.nsCast ns) (Just a)
  privacyFor ns (Just (TraceLedgerPeersTracer a)) = Logging.privacyFor (Logging.nsCast ns) (Just a)
  privacyFor ns (Just (TracePeerSelectionTracer a)) = Logging.privacyFor (Logging.nsCast ns) (Just a)
  privacyFor ns (Just (DebugPeerSelectionInitiatorTracer a)) = Logging.privacyFor (Logging.nsCast ns) (Just a)
  privacyFor ns (Just (DebugPeerSelectionInitiatorResponderTracer a)) = Logging.privacyFor (Logging.nsCast ns) (Just a)
  privacyFor ns (Just (TracePeerSelectionCounters a)) = Logging.privacyFor (Logging.nsCast ns) (Just a)
  privacyFor ns (Just (TraceChurnCounters a)) = Logging.privacyFor (Logging.nsCast ns) (Just a)
  privacyFor ns (Just (PeerSelectionActionsTracer a)) = Logging.privacyFor (Logging.nsCast ns) (Just a)
  privacyFor ns (Just (ConnectionManagerTracer a)) = Logging.privacyFor (Logging.nsCast ns) (Just a)
  privacyFor ns (Just (ConnectionManagerTransitionTracer a)) = Logging.privacyFor (Logging.nsCast ns) (Just a)
  privacyFor ns (Just (ServerTracer a)) = Logging.privacyFor (Logging.nsCast ns) (Just a)
  privacyFor ns (Just (InboundGovernorTracer a)) = Logging.privacyFor (Logging.nsCast ns) (Just a)
  privacyFor ns (Just (InboundGovernorTransitionTracer a)) = Logging.privacyFor (Logging.nsCast ns) (Just a)
  privacyFor ns (Just (DnsTracer a)) = Logging.privacyFor (Logging.nsCast ns) (Just a)
  privacyFor ns (Just (LocalConnectionManagerTracer a)) = Logging.privacyFor (Logging.nsCast ns) (Just a)
  privacyFor ns (Just (LocalServerTracer a)) = Logging.privacyFor (Logging.nsCast ns) (Just a)
  privacyFor ns (Just (LocalInboundGovernorTracer a)) = Logging.privacyFor (Logging.nsCast ns) (Just a)
  privacyFor _ _ = Just Logging.Public
  detailsFor ns Nothing = Logging.detailsFor (Logging.nsCast ns :: Logging.Namespace tr) Nothing
  detailsFor ns (Just (MuxTracer a)) = Logging.detailsFor (Logging.nsCast ns) (Just a)
  detailsFor ns (Just (MuxTracer a)) = Logging.detailsFor (Logging.nsCast ns) (Just a)
  detailsFor ns (Just (ChannelTracer a)) = Logging.detailsFor (Logging.nsCast ns) (Just a)
  detailsFor ns (Just (BearerTracer a)) = Logging.detailsFor (Logging.nsCast ns) (Just a)
  detailsFor ns (Just (HandshakeTracer a)) = Logging.detailsFor (Logging.nsCast ns) (Just a)
  detailsFor ns (Just (LocalMuxTracer a)) = Logging.detailsFor (Logging.nsCast ns) (Just a)
  detailsFor ns (Just (LocalChannelTracer a)) = Logging.detailsFor (Logging.nsCast ns) (Just a)
  detailsFor ns (Just (LocalBearerTracer a)) = Logging.detailsFor (Logging.nsCast ns) (Just a)
  detailsFor ns (Just (LocalHandshakeTracer a)) = Logging.detailsFor (Logging.nsCast ns) (Just a)
  detailsFor ns (Just (DiffusionTracer a)) = Logging.detailsFor (Logging.nsCast ns) (Just a)
  detailsFor ns (Just (TraceLocalRootPeersTracer a)) = Logging.detailsFor (Logging.nsCast ns) (Just a)
  detailsFor ns (Just (TracePublicRootPeersTracer a)) = Logging.detailsFor (Logging.nsCast ns) (Just a)
  detailsFor ns (Just (TraceLedgerPeersTracer a)) = Logging.detailsFor (Logging.nsCast ns) (Just a)
  detailsFor ns (Just (TracePeerSelectionTracer a)) = Logging.detailsFor (Logging.nsCast ns) (Just a)
  detailsFor ns (Just (DebugPeerSelectionInitiatorTracer a)) = Logging.detailsFor (Logging.nsCast ns) (Just a)
  detailsFor ns (Just (DebugPeerSelectionInitiatorResponderTracer a)) = Logging.detailsFor (Logging.nsCast ns) (Just a)
  detailsFor ns (Just (TracePeerSelectionCounters a)) = Logging.detailsFor (Logging.nsCast ns) (Just a)
  detailsFor ns (Just (TraceChurnCounters a)) = Logging.detailsFor (Logging.nsCast ns) (Just a)
  detailsFor ns (Just (PeerSelectionActionsTracer a)) = Logging.detailsFor (Logging.nsCast ns) (Just a)
  detailsFor ns (Just (ConnectionManagerTracer a)) = Logging.detailsFor (Logging.nsCast ns) (Just a)
  detailsFor ns (Just (ConnectionManagerTransitionTracer a)) = Logging.detailsFor (Logging.nsCast ns) (Just a)
  detailsFor ns (Just (ServerTracer a)) = Logging.detailsFor (Logging.nsCast ns) (Just a)
  detailsFor ns (Just (InboundGovernorTracer a)) = Logging.detailsFor (Logging.nsCast ns) (Just a)
  detailsFor ns (Just (InboundGovernorTransitionTracer a)) = Logging.detailsFor (Logging.nsCast ns) (Just a)
  detailsFor ns (Just (DnsTracer a)) = Logging.detailsFor (Logging.nsCast ns) (Just a)
  detailsFor ns (Just (LocalConnectionManagerTracer a)) = Logging.detailsFor (Logging.nsCast ns) (Just a)
  detailsFor ns (Just (LocalServerTracer a)) = Logging.detailsFor (Logging.nsCast ns) (Just a)
  detailsFor ns (Just (LocalInboundGovernorTracer a)) = Logging.detailsFor (Logging.nsCast ns) (Just a)
  detailsFor _ _ =  Just Logging.DNormal
  documentFor ns = Logging.documentFor (Logging.nsCast ns :: Logging.Namespace tr)
  documentFor _ = Nothing
  metricsDocFor _ = []

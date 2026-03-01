{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Network.Tracing.PeerSelection.RootPeersDNS.PublicRootPeers () where

import Data.Aeson (Value (String), toJSON, toJSONList, (.=))
import Data.Text (pack)

import Cardano.Logging
import Ouroboros.Network.OrphanInstances qualified ()
import Ouroboros.Network.PeerSelection.RootPeersDNS.PublicRootPeers
           (TracePublicRootPeers (..))


--------------------------------------------------------------------------------
-- PublicRootPeers Tracer
--------------------------------------------------------------------------------

instance LogFormatting TracePublicRootPeers where
  forMachine _dtal (TracePublicRootRelayAccessPoint relays) =
    mconcat [ "kind" .= String "PublicRootRelayAddresses"
             , "relayAddresses" .= toJSON relays
             ]
  forMachine _dtal (TracePublicRootDomains domains) =
    mconcat [ "kind" .= String "PublicRootDomains"
             , "domainAddresses" .= toJSONList domains
             ]
  forHuman = pack . show

instance MetaTrace TracePublicRootPeers where
  namespaceFor TracePublicRootRelayAccessPoint {} = Namespace [] ["PublicRootRelayAccessPoint"]
  namespaceFor TracePublicRootDomains {} = Namespace [] ["PublicRootDomains"]

  severityFor (Namespace [] ["PublicRootRelayAccessPoint"]) _ = Just Info
  severityFor (Namespace [] ["PublicRootDomains"]) _          = Just Info
  severityFor _ _                                             = Nothing

  documentFor (Namespace [] ["PublicRootRelayAccessPoint"]) = Just
    ""
  documentFor (Namespace [] ["PublicRootDomains"]) = Just
    ""
  documentFor _ = Nothing

  allNamespaces = [
      Namespace [] ["PublicRootRelayAccessPoint"]
    , Namespace [] ["PublicRootDomains"]
    ]

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}

-------------------------------------------------------------------------------

-- Orphan instances module for Cardano tracer.
{-# OPTIONS_GHC -Wno-orphans #-}
-- Extracted from "cardano-node" `Cardano.Node.Tracing.Tracers.P2P`.
-- Branch "master" (2026-02-11, 85869e9dd21d9dac7c4381418346e97259c3303b).

--------------------------------------------------------------------------------

module Ouroboros.Network.Tracing.PeerSelection.RootPeersDNS.PublicRootPeers () where

--------------------------------------------------------------------------------

---------
-- base -
---------
--
---------------------
-- Package: "aeson" -
---------------------
import "aeson" Data.Aeson (Value (String), toJSON, toJSONList, (.=))
---------------------------------
-- Package: "ouroboros-network" -
---------------------------------
-- Needed for `ToJSONKey PeerSelection.RelayAccessPoint.RelayAccessPoint`
import "ouroboros-network" Ouroboros.Network.OrphanInstances qualified ()
import "ouroboros-network" Ouroboros.Network.PeerSelection.RootPeersDNS.PublicRootPeers
           (TracePublicRootPeers (..))
--------------------
-- Package: "text" -
--------------------
import "text" Data.Text (pack)
--------------------------------
-- Package: "trace-dispatcher" -
--------------------------------
import "trace-dispatcher" Cardano.Logging

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


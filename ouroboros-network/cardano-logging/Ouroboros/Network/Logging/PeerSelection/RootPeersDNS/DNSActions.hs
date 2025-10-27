{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

-- Orphan instances module for Cardano tracer.
{-# OPTIONS_GHC -Wno-orphans #-}
-- Extracted from "cardano-node" `Cardano.Node.Tracing.Tracers.P2P`.
-- Branch "ana/10.6-final-integration-mix"

--------------------------------------------------------------------------------

module Ouroboros.Network.Logging.PeerSelection.RootPeersDNS.DNSActions () where

--------------------------------------------------------------------------------

---------
-- base -
---------
--
---------------------
-- Package: "aeson" -
---------------------
import "aeson" Data.Aeson (ToJSON, toJSON, Value (String), (.=))
-----------------------
-- Package: "iproute" -
-----------------------
import qualified "iproute" Data.IP as IP
---------------------------------
-- Package: "ouroboros-network" -
---------------------------------
-- Needed for `ToJSON Network.Socket.Types.PortNumber`
import qualified "ouroboros-network" -- "ouroboros-network:orphan-instances"
  Ouroboros.Network.OrphanInstances ()
import           "ouroboros-network" -- "ouroboros-newtwork:ouroboros-network"
  Ouroboros.Network.PeerSelection.RootPeersDNS.DNSActions
    ( DNSTrace (..) )
--------------------
-- Package: "text" -
--------------------
import "text" Data.Text (pack)
--------------------------------
-- Package: "trace-dispatcher" -
--------------------------------
import "trace-dispatcher" Cardano.Logging

-------------------------------------------------------------------------------
-- Types.
-------------------------------------------------------------------------------

-- From: `Cardano.Tracing.OrphanInstances.Network`.
instance ToJSON IP.IP where
  toJSON ip = String (pack . show $ ip)

--------------------------------------------------------------------------------
-- DNSTrace Tracer
--------------------------------------------------------------------------------

instance LogFormatting DNSTrace where
  forMachine _dtal (DNSLookupResult peerKind domain Nothing results) =
    mconcat [ "kind" .= String "DNSLookupResult"
            , "peerKind" .= String (pack . show $ peerKind)
            , "domain" .= String (pack . show $ domain)
            , "results" .= results
            ]
  forMachine _dtal (DNSLookupResult peerKind domain (Just srv) results) =
    mconcat [ "kind" .= String "DNSLookupResult"
            , "peerKind" .= String (pack . show $ peerKind)
            , "domain" .= String (pack . show $ domain)
            , "srv" .= String (pack . show $ srv)
            , "results" .= results
            ]
  forMachine _dtal  (DNSLookupError peerKind lookupType domain dnsError) =
    mconcat [ "kind" .= String "DNSLookupError"
            , "peerKind" .= String (pack . show $ peerKind)
            , "lookupKind" .= String (pack . show $ lookupType)
            , "domain" .= String (pack . show $ domain)
            , "dnsError" .= String (pack . show $ dnsError)
            ]
  forMachine _dtal (SRVLookupResult peerKind domain results) =
    mconcat [ "kind" .= String "SRVLookupResult"
            , "peerKind" .= String (pack . show $ peerKind)
            , "domain" .= String (pack . show $ domain)
            , "results" .= [ (show a, b, c, d, e)
                           | (a, b, c, d, e) <- results
                           ]
            ]
  forMachine _dtal  (SRVLookupError peerKind domain) =
    mconcat [ "kind" .= String "SRVLookupError"
            , "peerKind" .= String (pack . show $ peerKind)
            , "domain" .= String (pack . show $ domain)
            ]

instance MetaTrace DNSTrace where
  namespaceFor DNSLookupResult {} =
    Namespace [] ["DNSLookupResult"]
  namespaceFor DNSLookupError {} =
    Namespace [] ["DNSLookupError"]
  namespaceFor SRVLookupResult {} =
    Namespace [] ["SRVLookupResult"]
  namespaceFor SRVLookupError {} =
    Namespace [] ["SRVLookupError"]

  severityFor _ (Just DNSLookupResult {}) = Just Info
  severityFor _ (Just DNSLookupError {}) = Just Info
  severityFor _ (Just SRVLookupResult{}) = Just Info
  severityFor _ (Just SRVLookupError{}) = Just Info
  severityFor _ Nothing = Nothing

  documentFor _ = Nothing

  allNamespaces = [
      Namespace [] ["DNSLookupResult"]
    , Namespace [] ["DNSLookupError"]
    , Namespace [] ["SRVLookupResult"]
    , Namespace [] ["SRVLookupError"]
    ]


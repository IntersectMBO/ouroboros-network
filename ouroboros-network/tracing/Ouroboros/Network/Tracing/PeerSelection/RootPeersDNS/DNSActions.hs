{-# LANGUAGE OverloadedStrings #-}


{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Network.Tracing.PeerSelection.RootPeersDNS.DNSActions () where

import Data.Aeson (ToJSON, Value (String), toJSON, (.=))
import Data.IP qualified as IP
import Data.Text (pack)

import Hermod.Tracing.API
import Ouroboros.Network.OrphanInstances qualified ()
import Ouroboros.Network.PeerSelection.RootPeersDNS.DNSActions (DNSTrace (..))


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

  severityFor (Namespace _ ["DNSLookupResult"]) _ = Just Info
  severityFor (Namespace _ ["DNSLookupError"]) _  = Just Info
  severityFor (Namespace _ ["SRVLookupResult"]) _ = Just Info
  severityFor (Namespace _ ["SRVLookupError"]) _  = Just Info
  severityFor _ _                                 = Nothing

  documentFor (Namespace _ ["DNSLookupResult"]) = Just
    "The addresses a DNS lookup of a root peer's domain returned."
  documentFor (Namespace _ ["DNSLookupError"])  = Just
    "A DNS lookup of a root peer's domain failed."
  documentFor (Namespace _ ["SRVLookupResult"]) = Just
    "The records an SRV lookup of a root peer's domain returned."
  documentFor (Namespace _ ["SRVLookupError"])  = Just
    "An SRV lookup of a root peer's domain failed."
  documentFor _                                 = Nothing

  allNamespaces = [
      Namespace [] ["DNSLookupResult"]
    , Namespace [] ["DNSLookupError"]
    , Namespace [] ["SRVLookupResult"]
    , Namespace [] ["SRVLookupError"]
    ]

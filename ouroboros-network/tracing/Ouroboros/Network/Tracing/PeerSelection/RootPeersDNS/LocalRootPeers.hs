{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Network.Tracing.PeerSelection.RootPeersDNS.LocalRootPeers () where

import Control.Exception (displayException)

import Data.Aeson (ToJSON, ToJSONKey, Value (String), toJSON, (.=))
import Data.Text (pack)

-- Needed for `ToJSON PeerSelection.State.LocalRootPeers.LocalRootConfig`
import Cardano.Logging
import Ouroboros.Network.OrphanInstances qualified ()
import Ouroboros.Network.PeerSelection.RootPeersDNS.LocalRootPeers
           (TraceLocalRootPeers (..))

instance
  ( ToJSONKey ntnAddr
  , ToJSON ntnAddr
  , ToJSON extraFlags
  , Show ntnAddr
  , Show extraFlags
  ) => LogFormatting (TraceLocalRootPeers extraFlags ntnAddr) where
  forMachine _dtal (TraceLocalRootDomains groups) =
    mconcat [ "kind" .= String "LocalRootDomains"
             , "localRootDomains" .= toJSON groups
             ]
  forMachine _dtal (TraceLocalRootWaiting d dt) =
    mconcat [ "kind" .= String "LocalRootWaiting"
             , "domainAddress" .= toJSON d
             , "diffTime" .= show dt
             ]
  forMachine _dtal (TraceLocalRootGroups groups) =
    mconcat [ "kind" .= String "LocalRootGroups"
             , "localRootGroups" .= toJSON groups
             ]
  forMachine _dtal (TraceLocalRootFailure d exception) =
    mconcat [ "kind" .= String "LocalRootFailure"
             , "domainAddress" .= toJSON d
             , "reason" .= displayException exception
             ]
  forMachine _dtal (TraceLocalRootError d exception) =
    mconcat [ "kind" .= String "LocalRootError"
             , "domainAddress" .= String (pack . show $ d)
             , "reason" .= displayException exception
             ]
  forMachine _dtal (TraceLocalRootReconfigured d exception) =
    mconcat [ "kind" .= String "LocalRootReconfigured"
             , "domainAddress" .= toJSON d
             , "reason" .= show exception
             ]
  forMachine _dtal (TraceLocalRootDNSMap dnsMap) =
    mconcat
      [ "kind" .= String "TraceLocalRootDNSMap"
      , "dnsMap" .= dnsMap
      ]

  forHuman = pack . show

instance MetaTrace (TraceLocalRootPeers ntnAddr extraFlags) where
  namespaceFor = \case
    TraceLocalRootDomains {}      -> Namespace [] ["LocalRootDomains"]
    TraceLocalRootWaiting {}      -> Namespace [] ["LocalRootWaiting"]
    TraceLocalRootGroups {}       -> Namespace [] ["LocalRootGroups"]
    TraceLocalRootFailure {}      -> Namespace [] ["LocalRootFailure"]
    TraceLocalRootError {}        -> Namespace [] ["LocalRootError"]
    TraceLocalRootReconfigured {} -> Namespace [] ["LocalRootReconfigured"]
    TraceLocalRootDNSMap {}       -> Namespace [] ["LocalRootDNSMap"]

  severityFor (Namespace [] ["LocalRootDomains"]) _      = Just Info
  severityFor (Namespace [] ["LocalRootWaiting"]) _      = Just Info
  severityFor (Namespace [] ["LocalRootGroups"]) _       = Just Info
  severityFor (Namespace [] ["LocalRootFailure"]) _      = Just Info
  severityFor (Namespace [] ["LocalRootError"]) _        = Just Info
  severityFor (Namespace [] ["LocalRootReconfigured"]) _ = Just Info
  severityFor (Namespace [] ["LocalRootDNSMap"]) _       = Just Info
  severityFor _ _                                        = Nothing

  documentFor (Namespace [] ["LocalRootDomains"]) = Just
    ""
  documentFor (Namespace [] ["LocalRootWaiting"]) = Just
    ""
  documentFor (Namespace [] ["LocalRootGroups"]) = Just
    ""
  documentFor (Namespace [] ["LocalRootFailure"]) = Just
    ""
  documentFor (Namespace [] ["LocalRootError"]) = Just
    ""
  documentFor (Namespace [] ["LocalRootReconfigured"]) = Just
    ""
  documentFor (Namespace [] ["LocalRootDNSMap"]) = Just
    ""
  documentFor _ = Nothing

  allNamespaces =
    [ Namespace [] ["LocalRootDomains"]
    , Namespace [] ["LocalRootWaiting"]
    , Namespace [] ["LocalRootGroups"]
    , Namespace [] ["LocalRootFailure"]
    , Namespace [] ["LocalRootError"]
    , Namespace [] ["LocalRootReconfigured"]
    , Namespace [] ["LocalRootDNSMap"]
    ]

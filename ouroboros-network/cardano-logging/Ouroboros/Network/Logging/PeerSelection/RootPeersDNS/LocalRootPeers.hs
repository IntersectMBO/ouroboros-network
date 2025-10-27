{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

-- Orphan instances module for Cardano tracer.
{-# OPTIONS_GHC -Wno-orphans #-}
-- Extracted from "cardano-node" `Cardano.Node.Tracing.Tracers.P2P`.
-- Branch "ana/10.6-final-integration-mix"

--------------------------------------------------------------------------------

module Ouroboros.Network.Logging.PeerSelection.RootPeersDNS.LocalRootPeers () where

--------------------------------------------------------------------------------

---------
-- base -
---------
import Control.Exception (displayException)
---------------------
-- Package: "aeson" -
---------------------
import "aeson" Data.Aeson (ToJSON, ToJSONKey, toJSON, Value (String), (.=))
---------------------------------
-- Package: "ouroboros-network" -
---------------------------------
-- Needed for `ToJSON PeerSelection.State.LocalRootPeers.LocalRootConfig`
import qualified "ouroboros-network" -- "ouroboros-network:orphan-instances"
  Ouroboros.Network.OrphanInstances ()
import           "ouroboros-network" -- "ouroboros-newtwork:ouroboros-newtwork"
  Ouroboros.Network.PeerSelection.RootPeersDNS.LocalRootPeers
    ( TraceLocalRootPeers (..)
    )
--------------------
-- Package: "text" -
--------------------
import "text" Data.Text (pack)
--------------------------------
-- Package: "trace-dispatcher" -
--------------------------------
import "trace-dispatcher" Cardano.Logging

--------------------------------------------------------------------------------
-- LocalRootPeers Tracer
--------------------------------------------------------------------------------

{-- TODO: In `cardano-node` it was, OK????:
instance
  ( ToJSONKey ntnAddr
  , ToJSON ntnAddr
  , ToJSONKey RelayAccessPoint
  , Show ntnAddr
  ) => LogFormatting (TraceLocalRootPeers PeerTrustable ntnAddr) where
--}

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

  severityFor (Namespace [] ["LocalRootDomains"]) _ = Just Info
  severityFor (Namespace [] ["LocalRootWaiting"]) _ = Just Info
  severityFor (Namespace [] ["LocalRootGroups"]) _ = Just Info
  severityFor (Namespace [] ["LocalRootFailure"]) _ = Just Info
  severityFor (Namespace [] ["LocalRootError"]) _ = Just Info
  severityFor (Namespace [] ["LocalRootReconfigured"]) _ = Just Info
  severityFor (Namespace [] ["LocalRootDNSMap"]) _ = Just Info
  severityFor _ _ = Nothing

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


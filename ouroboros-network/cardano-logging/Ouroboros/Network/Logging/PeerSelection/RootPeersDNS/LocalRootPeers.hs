{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}

--------------------------------------------------------------------------------

-- Orphan instances module for Cardano tracer.
{-# OPTIONS_GHC -Wno-orphans #-}
-- Extracted from "cardano-node" `Cardano.Node.Tracing.Tracers.P2P`.
-- Branch "master" (2025-02-28, 8c6a9f89fd8bb5b97dba2ae3a4c50873566fe14e).

{- TODO: All references to package "cardano-diffusion" were removed.
--       See all the TODO annotations.
--}

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
import "aeson" Data.Aeson (ToJSON, ToJSONKey, Value (String), toJSON, (.=))
---------------------------------
-- Package: "ouroboros-network" -
---------------------------------
-- Needed for `ToJSON PeerSelection.State.LocalRootPeers.LocalRootConfig`
import "ouroboros-network" Ouroboros.Network.OrphanInstances qualified ()
import "ouroboros-network" Ouroboros.Network.PeerSelection.RootPeersDNS.LocalRootPeers
           (TraceLocalRootPeers (..))
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

{-- TODO: Before "cardano-diffusion" removal:
instance
  ( ToJSONKey ntnAddr
  , ToJSON ntnAddr
  , ToJSONKey RelayAccessPoint
  , Show ntnAddr
  ) => LogFormatting (TraceLocalRootPeers PeerTrustable ntnAddr) where
 -- TODO: That later changed in f550a6eb503cc81807419795ab2360e6042ce9d5:
instance LogFormatting CardanoTraceLocalRootPeers where
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


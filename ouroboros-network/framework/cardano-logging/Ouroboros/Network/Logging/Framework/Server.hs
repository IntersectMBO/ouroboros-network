{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

--------------------------------------------------------------------------------

-- Orphan instances module for Cardano tracer.
{-# OPTIONS_GHC -Wno-orphans #-}
-- Extracted from "cardano-node" `Cardano.Node.Tracing.Tracers.P2P`.
-- Branch "master" (2025-02-28, 8c6a9f89fd8bb5b97dba2ae3a4c50873566fe14e).

--------------------------------------------------------------------------------

module Ouroboros.Network.Logging.Framework.Server () where

--------------------------------------------------------------------------------

---------
-- base -
---------
--
---------------------
-- Package: "aeson" -
---------------------
import "aeson" Data.Aeson (ToJSON, toJSON, Value (String), (.=))
---------------------------------
-- Package: "ouroboros-network" -
---------------------------------
import           "ouroboros-network" -- "ouroboros-newtwork:framework"
  Ouroboros.Network.Server as Server
import qualified "ouroboros-network" -- "ouroboros-newtwork:framework"
  Ouroboros.Network.Server.RateLimiting as SRL
-- Needed for `instance ToJSON (ConnectionId addr) where`
import qualified "ouroboros-network" -- "ouroboros-network:orphan-instances"
  Ouroboros.Network.OrphanInstances ()
--------------------
-- Package: "text" -
--------------------
import "text" Data.Text (pack)
--------------------------------
-- Package: "trace-dispatcher" -
--------------------------------
import "trace-dispatcher" Cardano.Logging
---------
-- Self -
---------
import Ouroboros.Network.Logging.Framework.ConnectionId ()

--------------------------------------------------------------------------------
-- AcceptPolicy Tracer
--------------------------------------------------------------------------------

instance LogFormatting SRL.AcceptConnectionsPolicyTrace where
    forMachine _dtal (SRL.ServerTraceAcceptConnectionRateLimiting delay numOfConnections) =
      mconcat [ "kind" .= String "ServerTraceAcceptConnectionRateLimiting"
               , "delay" .= show delay
               , "numberOfConnection" .= show numOfConnections
               ]
    forMachine _dtal (SRL.ServerTraceAcceptConnectionHardLimit softLimit) =
      mconcat [ "kind" .= String "ServerTraceAcceptConnectionHardLimit"
               , "softLimit" .= show softLimit
               ]
    forMachine _dtal (SRL.ServerTraceAcceptConnectionResume numOfConnections) =
      mconcat [ "kind" .= String "ServerTraceAcceptConnectionResume"
               , "numberOfConnection" .= show numOfConnections
               ]
    forHuman   = showT

instance MetaTrace SRL.AcceptConnectionsPolicyTrace where
    namespaceFor SRL.ServerTraceAcceptConnectionRateLimiting {} =
      Namespace [] ["ConnectionRateLimiting"]
    namespaceFor SRL.ServerTraceAcceptConnectionHardLimit {} =
      Namespace [] ["ConnectionHardLimit"]
    namespaceFor SRL.ServerTraceAcceptConnectionResume {} =
      Namespace [] ["ConnectionLimitResume"]

    severityFor (Namespace _ ["ConnectionRateLimiting"]) _ = Just Info
    severityFor (Namespace _ ["ConnectionHardLimit"]) _ = Just Warning
    severityFor (Namespace _ ["ConnectionLimitResume"]) _ = Just Info
    severityFor _ _ = Nothing

    documentFor (Namespace _ ["ConnectionRateLimiting"]) = Just $ mconcat
      [ "Rate limiting accepting connections,"
      , " delaying next accept for given time, currently serving n connections."
      ]
    documentFor (Namespace _ ["ConnectionHardLimit"]) = Just $ mconcat
      [ "Hard rate limit reached,"
      , " waiting until the number of connections drops below n."
      ]
    documentFor (Namespace _ ["ConnectionLimitResume"]) = Just
      ""
    documentFor _ = Nothing

    allNamespaces = [
        Namespace [] ["ConnectionRateLimiting"]
      , Namespace [] ["ConnectionHardLimit"]
      , Namespace [] ["ConnectionLimitResume"]
      ]

--------------------------------------------------------------------------------
-- Server Tracer
--------------------------------------------------------------------------------

instance (Show addr, LogFormatting addr, ToJSON addr)
      => LogFormatting (Server.Trace addr) where
  forMachine _dtal (TrAcceptConnection connId)     =
    mconcat [ "kind" .= String "AcceptConnection"
             , "address" .= toJSON connId
             ]
  forMachine _dtal (TrAcceptError exception)         =
    mconcat [ "kind" .= String "AcceptErroor"
             , "reason" .= show exception
             ]
  forMachine dtal (TrAcceptPolicyTrace policyTrace) =
    mconcat [ "kind" .= String "AcceptPolicyTrace"
             , "policy" .= forMachine dtal policyTrace
             ]
  forMachine dtal (TrServerStarted peerAddrs)       =
    mconcat [ "kind" .= String "AcceptPolicyTrace"
             , "addresses" .= toJSON (forMachine dtal `map` peerAddrs)
             ]
  forMachine _dtal TrServerStopped                   =
    mconcat [ "kind" .= String "ServerStopped"
             ]
  forMachine _dtal (TrServerError exception)         =
    mconcat [ "kind" .= String "ServerError"
             , "reason" .= show exception
             ]
  forHuman = pack . show

instance MetaTrace (Server.Trace addr) where
    namespaceFor TrAcceptConnection {} = Namespace [] ["AcceptConnection"]
    namespaceFor TrAcceptError {} = Namespace [] ["AcceptError"]
    namespaceFor TrAcceptPolicyTrace {} = Namespace [] ["AcceptPolicy"]
    namespaceFor TrServerStarted {} = Namespace [] ["Started"]
    namespaceFor TrServerStopped {} = Namespace [] ["Stopped"]
    namespaceFor TrServerError {} = Namespace [] ["Error"]

    severityFor (Namespace _ ["AcceptConnection"]) _ = Just Debug
    severityFor (Namespace _ ["AcceptError"]) _ = Just Error
    severityFor (Namespace _ ["AcceptPolicy"]) _ = Just Notice
    severityFor (Namespace _ ["Started"]) _ = Just Notice
    severityFor (Namespace _ ["Stopped"]) _ = Just Notice
    severityFor (Namespace _ ["Error"]) _ = Just Critical
    severityFor _ _ = Nothing

    documentFor (Namespace _ ["AcceptConnection"]) = Just ""
    documentFor (Namespace _ ["AcceptError"]) = Just ""
    documentFor (Namespace _ ["AcceptPolicy"]) = Just ""
    documentFor (Namespace _ ["Started"]) = Just ""
    documentFor (Namespace _ ["Stopped"]) = Just ""
    documentFor (Namespace _ ["Error"]) = Just ""
    documentFor _ = Nothing

    allNamespaces = [
        Namespace [] ["AcceptConnection"]
      , Namespace [] ["AcceptError"]
      , Namespace [] ["AcceptPolicy"]
      , Namespace [] ["Started"]
      , Namespace [] ["Stopped"]
      , Namespace [] ["Error"]
      ]

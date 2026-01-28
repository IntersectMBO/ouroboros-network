{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

--------------------------------------------------------------------------------

-- Orphan instances module for Cardano tracer.
{-# OPTIONS_GHC -Wno-orphans #-}
-- Extracted from "cardano-node" `Cardano.Node.Tracing.Tracers.P2P`.
-- Branch "master" (2026-02-11, 85869e9dd21d9dac7c4381418346e97259c3303b).

--------------------------------------------------------------------------------

module Ouroboros.Network.Logging.PeerSelection.PeerStateActions () where

--------------------------------------------------------------------------------

---------
-- base -
---------
import           Control.Exception (displayException)
---------------------
-- Package: "aeson" -
---------------------
import "aeson" Data.Aeson (toJSON, Value (String), (.=))
-----------------------
-- Package: "network" -
-----------------------
import "network" Network.Socket (SockAddr)
---------------------------------
-- Package: "ouroboros-network" -
---------------------------------
import "ouroboros-network" -- "ouroboros-network:ouroboros-network"
  Ouroboros.Network.PeerSelection.PeerStateActions
    ( PeerSelectionActionsTrace (..) )
-- Needed for `instance ToJSON ConnectionId`.
import "ouroboros-network" -- "ouroboros-network:orphan-instances"
  Ouroboros.Network.OrphanInstances ()
--------------------
-- Package: "text" -
--------------------
import "text" Data.Text (pack)
--------------------------------
-- Package: "trace-dispatcher" -
--------------------------------
import "trace-dispatcher" Cardano.Logging

--------------------------------------------------------------------------------
-- PeerSelectionActions Tracer.
--------------------------------------------------------------------------------

-- TODO: Write PeerStatusChangeType ToJSON at ouroboros-network
-- For that an export is needed at ouroboros-network
instance Show lAddr => LogFormatting (PeerSelectionActionsTrace SockAddr lAddr) where
  forMachine _dtal (PeerStatusChanged ps) =
    mconcat [ "kind" .= String "PeerStatusChanged"
             , "peerStatusChangeType" .= show ps
             ]
  forMachine _dtal (PeerStatusChangeFailure ps f) =
    mconcat [ "kind" .= String "PeerStatusChangeFailure"
             , "peerStatusChangeType" .= show ps
             , "reason" .= show f
             ]
  forMachine _dtal (PeerMonitoringError connId s) =
    mconcat [ "kind" .= String "PeerMonitoringError"
             , "connectionId" .= toJSON connId
             , "reason" .= show s
             ]
  forMachine _dtal (PeerMonitoringResult connId wf) =
    mconcat [ "kind" .= String "PeerMonitoringResult"
             , "connectionId" .= toJSON connId
             , "withProtocolTemp" .= show wf
             ]
  forMachine _dtal (AcquireConnectionError exception) =
    mconcat [ "kind" .= String "AcquireConnectionError"
            , "error" .= displayException exception
            ]
  forMachine _dtal (PeerHotDuration connId dt) =
    mconcat [ "kind" .= String "PeerHotDuration"
            , "connectionId" .= toJSON connId
            , "time" .= show dt]
  forHuman = pack . show

instance MetaTrace (PeerSelectionActionsTrace SockAddr lAddr) where
    namespaceFor PeerStatusChanged {} = Namespace [] ["StatusChanged"]
    namespaceFor PeerStatusChangeFailure {} = Namespace [] ["StatusChangeFailure"]
    namespaceFor PeerMonitoringError {} = Namespace [] ["MonitoringError"]
    namespaceFor PeerMonitoringResult {} = Namespace [] ["MonitoringResult"]
    namespaceFor AcquireConnectionError {} = Namespace [] ["ConnectionError"]
    namespaceFor PeerHotDuration {} = Namespace [] ["PeerHotDuration"]

    severityFor (Namespace _ ["StatusChanged"]) _ = Just Info
    severityFor (Namespace _ ["StatusChangeFailure"]) _ = Just Error
    severityFor (Namespace _ ["MonitoringError"]) _ = Just Error
    severityFor (Namespace _ ["MonitoringResult"]) _ = Just Debug
    severityFor (Namespace _ ["ConnectionError"]) _ = Just Error
    severityFor (Namespace _ ["PeerHotDuration"]) _ = Just Info
    severityFor _ _ = Nothing

    documentFor (Namespace _ ["StatusChanged"]) = Just
      ""
    documentFor (Namespace _ ["StatusChangeFailure"]) = Just
      ""
    documentFor (Namespace _ ["MonitoringError"]) = Just
      ""
    documentFor (Namespace _ ["MonitoringResult"]) = Just
      ""
    documentFor (Namespace _ ["ConnectionError"]) = Just
      ""
    documentFor (Namespace _ ["PeerHotDuration"]) = Just
      "Reports how long the outbound connection was in hot state"
    documentFor _ = Nothing

    allNamespaces = [
        Namespace [] ["StatusChanged"]
      , Namespace [] ["StatusChangeFailure"]
      , Namespace [] ["MonitoringError"]
      , Namespace [] ["MonitoringResult"]
      , Namespace [] ["ConnectionError"]
      , Namespace [] ["PeerHotDuration"]
      ]


{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Network.Tracing.PeerSelection.PeerStateActions () where

import Control.Exception (displayException)
import Data.Aeson (ToJSON (..), Value (String), (.=))
import Data.Text (pack)

import Cardano.Logging
import Ouroboros.Network.PeerSelection.PeerStateActions
           (PeerSelectionActionsTrace (..))
-- Needed for `instance ToJSON ConnectionId`.
import Ouroboros.Network.OrphanInstances ()

--------------------------------------------------------------------------------
-- PeerSelectionActions Tracer.
--------------------------------------------------------------------------------

-- TODO: Write PeerStatusChangeType ToJSON at ouroboros-network
-- For that an export is needed at ouroboros-network
instance (Show lAddr, Show peeraddr, ToJSON peeraddr)
      => LogFormatting (PeerSelectionActionsTrace peeraddr lAddr) where
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

instance MetaTrace (PeerSelectionActionsTrace peeraddr lAddr) where
    namespaceFor PeerStatusChanged {} = Namespace [] ["StatusChanged"]
    namespaceFor PeerStatusChangeFailure {} = Namespace [] ["StatusChangeFailure"]
    namespaceFor PeerMonitoringError {} = Namespace [] ["MonitoringError"]
    namespaceFor PeerMonitoringResult {} = Namespace [] ["MonitoringResult"]
    namespaceFor AcquireConnectionError {} = Namespace [] ["ConnectionError"]
    namespaceFor PeerHotDuration {} = Namespace [] ["PeerHotDuration"]

    severityFor (Namespace _ ["StatusChanged"]) _       = Just Info
    severityFor (Namespace _ ["StatusChangeFailure"]) _ = Just Error
    severityFor (Namespace _ ["MonitoringError"]) _     = Just Error
    severityFor (Namespace _ ["MonitoringResult"]) _    = Just Debug
    severityFor (Namespace _ ["ConnectionError"]) _     = Just Error
    severityFor (Namespace _ ["PeerHotDuration"]) _     = Just Info
    severityFor _ _                                     = Nothing

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

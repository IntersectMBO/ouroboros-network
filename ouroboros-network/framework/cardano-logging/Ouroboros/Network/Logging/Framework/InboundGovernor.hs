{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}

--------------------------------------------------------------------------------

-- Orphan instances module for Cardano tracer.
{-# OPTIONS_GHC -Wno-orphans #-}
-- Extracted from "cardano-node" `Cardano.Node.Tracing.Tracers.P2P`.
-- Branch "master" (2025-02-28, 8c6a9f89fd8bb5b97dba2ae3a4c50873566fe14e).

--------------------------------------------------------------------------------

module Ouroboros.Network.Logging.Framework.InboundGovernor () where

--------------------------------------------------------------------------------

---------
-- base -
---------
--
---------------------
-- Package: "aeson" -
---------------------
import "aeson" Data.Aeson (ToJSON, ToJSONKey, toJSON, Value (String), Object, (.=))
-----------------------
-- Package: "network" -
-----------------------
import "network" Network.Socket (SockAddr (..))
---------------------------------
-- Package: "ouroboros-network" -
---------------------------------
import qualified "ouroboros-network" -- "ouroboros-newtwork:framework"
  Ouroboros.Network.ConnectionManager.Types as ConnectionManager
import           "ouroboros-network" -- "ouroboros-newtwork:framework"
  Ouroboros.Network.InboundGovernor as InboundGovernor (Trace (..))
import qualified "ouroboros-network" -- "ouroboros-newtwork:framework"
  Ouroboros.Network.InboundGovernor as InboundGovernor
-- Needed for `ToJSON SockAddr`.
-- Needed for `ToJSON LocalAddress`
-- Needed for `ToJSON (ConnectionId adr)`
-- Needed for `ToJSON MiniProtocolNum`
-- Needed for `ToJSON (ConnectionManager.OperationResult, ConnectionManager.AbstractState)`
-- Needed for `ToJSONKey (ConnectionId adr)`
-- Needed for `ToJSON InboundGovernor.RemoteSt`
import qualified "ouroboros-network" -- "ouroboros-network:orphan-instances"
  Ouroboros.Network.OrphanInstances ()
import           "ouroboros-network" -- "ouroboros-newtwork:framework"
  Ouroboros.Network.Snocket (LocalAddress (..))
import           "ouroboros-network" -- "ouroboros-newtwork:framework"
  Ouroboros.Network.InboundGovernor.State as InboundGovernor
    ( Counters (..) )
--------------------
-- Package: "text" -
--------------------
import "text" Data.Text (pack)
--------------------------------
-- Package: "trace-dispatcher" -
--------------------------------
import "trace-dispatcher" Cardano.Logging

--------------------------------------------------------------------------------
-- InboundGovernor Tracer
--------------------------------------------------------------------------------

instance LogFormatting (InboundGovernor.Trace SockAddr) where
  forMachine = forMachineGov
  forHuman = pack . show
  asMetrics (TrInboundGovernorCounters InboundGovernor.Counters {..}) =
            [ IntM
                "inboundGovernor.idle"
                (fromIntegral idlePeersRemote)
            , IntM
                "inboundGovernor.cold"
                (fromIntegral coldPeersRemote)
            , IntM
                "inboundGovernor.warm"
                (fromIntegral warmPeersRemote)
            , IntM
                "inboundGovernor.hot"
                (fromIntegral hotPeersRemote)
              ]
  asMetrics _ = []

instance LogFormatting (InboundGovernor.Trace LocalAddress) where
  forMachine = forMachineGov
  forHuman = pack . show
  asMetrics (TrInboundGovernorCounters InboundGovernor.Counters {..}) =
            [ IntM
                "localInboundGovernor.idle"
                (fromIntegral idlePeersRemote)
            , IntM
                "localInboundGovernor.cold"
                (fromIntegral coldPeersRemote)
            , IntM
                "localInboundGovernor.warm"
                (fromIntegral warmPeersRemote)
            , IntM
                "localInboundGovernor.hot"
                (fromIntegral hotPeersRemote)
              ]
  asMetrics _ = []


forMachineGov :: (ToJSON adr, Show adr, ToJSONKey adr) => DetailLevel -> InboundGovernor.Trace adr -> Object
forMachineGov _dtal (TrNewConnection p connId)            =
  mconcat [ "kind" .= String "NewConnection"
            , "provenance" .= show p
            , "connectionId" .= toJSON connId
            ]
forMachineGov _dtal (TrResponderRestarted connId m)       =
  mconcat [ "kind" .= String "ResponderStarted"
            , "connectionId" .= toJSON connId
            , "miniProtocolNum" .= toJSON m
            ]
forMachineGov _dtal (TrResponderStartFailure connId m s)  =
  mconcat [ "kind" .= String "ResponderStartFailure"
            , "connectionId" .= toJSON connId
            , "miniProtocolNum" .= toJSON m
            , "reason" .= show s
            ]
forMachineGov _dtal (TrResponderErrored connId m s)       =
  mconcat [ "kind" .= String "ResponderErrored"
            , "connectionId" .= toJSON connId
            , "miniProtocolNum" .= toJSON m
            , "reason" .= show s
            ]
forMachineGov _dtal (TrResponderStarted connId m)         =
  mconcat [ "kind" .= String "ResponderStarted"
            , "connectionId" .= toJSON connId
            , "miniProtocolNum" .= toJSON m
            ]
forMachineGov _dtal (TrResponderTerminated connId m)      =
  mconcat [ "kind" .= String "ResponderTerminated"
            , "connectionId" .= toJSON connId
            , "miniProtocolNum" .= toJSON m
            ]
forMachineGov _dtal (TrPromotedToWarmRemote connId opRes) =
  mconcat [ "kind" .= String "PromotedToWarmRemote"
            , "connectionId" .= toJSON connId
            , "result" .= toJSON opRes
            ]
forMachineGov _dtal (TrPromotedToHotRemote connId)        =
  mconcat [ "kind" .= String "PromotedToHotRemote"
            , "connectionId" .= toJSON connId
            ]
forMachineGov _dtal (TrDemotedToColdRemote connId od)     =
  mconcat [ "kind" .= String "DemotedToColdRemote"
            , "connectionId" .= toJSON connId
            , "result" .= show od
            ]
forMachineGov _dtal (TrDemotedToWarmRemote connId)     =
  mconcat [ "kind" .= String "DemotedToWarmRemote"
            , "connectionId" .= toJSON connId
            ]
forMachineGov _dtal (TrWaitIdleRemote connId opRes) =
  mconcat [ "kind" .= String "WaitIdleRemote"
            , "connectionId" .= toJSON connId
            , "result" .= toJSON opRes
            ]
forMachineGov _dtal (TrMuxCleanExit connId)               =
  mconcat [ "kind" .= String "MuxCleanExit"
            , "connectionId" .= toJSON connId
            ]
forMachineGov _dtal (TrMuxErrored connId s)               =
  mconcat [ "kind" .= String "MuxErrored"
            , "connectionId" .= toJSON connId
            , "reason" .= show s
            ]
forMachineGov _dtal (TrInboundGovernorCounters counters) =
  mconcat [ "kind" .= String "InboundGovernorCounters"
            , "idlePeers" .= idlePeersRemote counters
            , "coldPeers" .= coldPeersRemote counters
            , "warmPeers" .= warmPeersRemote counters
            , "hotPeers" .= hotPeersRemote counters
            ]
forMachineGov _dtal (TrRemoteState st) =
  mconcat [ "kind" .= String "RemoteState"
            , "remoteSt" .= toJSON st
            ]
forMachineGov _dtal (InboundGovernor.TrUnexpectedlyFalseAssertion info) =
  mconcat [ "kind" .= String "UnexpectedlyFalseAssertion"
            , "remoteSt" .= String (pack . show $ info)
            ]
forMachineGov _dtal (InboundGovernor.TrInboundGovernorError err) =
  mconcat [ "kind" .= String "InboundGovernorError"
            , "remoteSt" .= String (pack . show $ err)
            ]
forMachineGov _dtal (InboundGovernor.TrMaturedConnections matured fresh) =
  mconcat [ "kind" .= String "MaturedConnections"
          , "matured" .= toJSON matured
          , "fresh" .= toJSON fresh
          ]
forMachineGov _dtal (InboundGovernor.TrInactive fresh) =
  mconcat [ "kind" .= String "Inactive"
          , "fresh" .= toJSON fresh
          ]

instance MetaTrace (InboundGovernor.Trace addr) where
    namespaceFor TrNewConnection {}         = Namespace [] ["NewConnection"]
    namespaceFor TrResponderRestarted {}    = Namespace [] ["ResponderRestarted"]
    namespaceFor TrResponderStartFailure {} = Namespace [] ["ResponderStartFailure"]
    namespaceFor TrResponderErrored {}      = Namespace [] ["ResponderErrored"]
    namespaceFor TrResponderStarted {}      = Namespace [] ["ResponderStarted"]
    namespaceFor TrResponderTerminated {}   = Namespace [] ["ResponderTerminated"]
    namespaceFor TrPromotedToWarmRemote {}  = Namespace [] ["PromotedToWarmRemote"]
    namespaceFor TrPromotedToHotRemote {}   = Namespace [] ["PromotedToHotRemote"]
    namespaceFor TrDemotedToColdRemote {}   = Namespace [] ["DemotedToColdRemote"]
    namespaceFor TrDemotedToWarmRemote {}   = Namespace [] ["DemotedToWarmRemote"]
    namespaceFor TrWaitIdleRemote {}        = Namespace [] ["WaitIdleRemote"]
    namespaceFor TrMuxCleanExit {}          = Namespace [] ["MuxCleanExit"]
    namespaceFor TrMuxErrored {}            = Namespace [] ["MuxErrored"]
    namespaceFor TrInboundGovernorCounters {} = Namespace [] ["InboundGovernorCounters"]
    namespaceFor TrRemoteState {}            = Namespace [] ["RemoteState"]
    namespaceFor InboundGovernor.TrUnexpectedlyFalseAssertion {} =
                                Namespace [] ["UnexpectedlyFalseAssertion"]
    namespaceFor InboundGovernor.TrInboundGovernorError {} =
                                Namespace [] ["InboundGovernorError"]
    namespaceFor InboundGovernor.TrMaturedConnections {} =
                                Namespace [] ["MaturedConnections"]
    namespaceFor InboundGovernor.TrInactive {} =
                                Namespace [] ["Inactive"]

    severityFor (Namespace _ ["NewConnection"]) _ = Just Debug
    severityFor (Namespace _ ["ResponderRestarted"]) _ = Just Debug
    severityFor (Namespace _ ["ResponderStartFailure"]) _ = Just Info
    severityFor (Namespace _ ["ResponderErrored"]) _ = Just Info
    severityFor (Namespace _ ["ResponderStarted"]) _ = Just Debug
    severityFor (Namespace _ ["ResponderTerminated"]) _ = Just Debug
    severityFor (Namespace _ ["PromotedToWarmRemote"]) _ = Just Info
    severityFor (Namespace _ ["PromotedToHotRemote"]) _ = Just Info
    severityFor (Namespace _ ["DemotedToColdRemote"]) _ = Just Info
    severityFor (Namespace _ ["DemotedToWarmRemote"]) _ = Just Info
    severityFor (Namespace _ ["WaitIdleRemote"]) _ = Just Debug
    severityFor (Namespace _ ["MuxCleanExit"]) _ = Just Debug
    severityFor (Namespace _ ["MuxErrored"]) _ = Just Info
    severityFor (Namespace _ ["InboundGovernorCounters"]) _ = Just Info
    severityFor (Namespace _ ["RemoteState"]) _ = Just Debug
    severityFor (Namespace _ ["UnexpectedlyFalseAssertion"]) _ = Just Error
    severityFor (Namespace _ ["InboundGovernorError"]) _ = Just Error
    severityFor (Namespace _ ["MaturedConnections"]) _ = Just Info
    severityFor (Namespace _ ["Inactive"]) _ = Just Debug
    severityFor _ _ = Nothing

    documentFor (Namespace _ ["NewConnection"]) = Just ""
    documentFor (Namespace _ ["ResponderRestarted"]) = Just ""
    documentFor (Namespace _ ["ResponderStartFailure"]) = Just ""
    documentFor (Namespace _ ["ResponderErrored"]) = Just ""
    documentFor (Namespace _ ["ResponderStarted"]) = Just ""
    documentFor (Namespace _ ["ResponderTerminated"]) = Just ""
    documentFor (Namespace _ ["PromotedToWarmRemote"]) = Just ""
    documentFor (Namespace _ ["PromotedToHotRemote"]) = Just ""
    documentFor (Namespace _ ["DemotedToColdRemote"]) = Just $ mconcat
      [ "All mini-protocols terminated.  The boolean is true if this connection"
      , " was not used by p2p-governor, and thus the connection will be terminated."
      ]
    documentFor (Namespace _ ["DemotedToWarmRemote"]) = Just $ mconcat
      [ "All mini-protocols terminated.  The boolean is true if this connection"
      , " was not used by p2p-governor, and thus the connection will be terminated."
      ]
    documentFor (Namespace _ ["WaitIdleRemote"]) = Just ""
    documentFor (Namespace _ ["MuxCleanExit"]) = Just ""
    documentFor (Namespace _ ["MuxErrored"]) = Just ""
    documentFor (Namespace _ ["InboundGovernorCounters"]) = Just ""
    documentFor (Namespace _ ["RemoteState"]) = Just ""
    documentFor (Namespace _ ["UnexpectedlyFalseAssertion"]) = Just ""
    documentFor (Namespace _ ["InboundGovernorError"]) = Just ""
    documentFor (Namespace _ ["MaturedConnections"]) = Just ""
    documentFor (Namespace _ ["Inactive"]) = Just ""
    documentFor _ = Nothing

    metricsDocFor (Namespace ons ["InboundGovernorCounters"])
      | null ons -- docu generation
        =
              [("localInboundGovernor.idle","")
              ,("localInboundGovernor.cold","")
              ,("localInboundGovernor.warm","")
              ,("localInboundGovernor.hot","")
              ,("inboundGovernor.Idle","")
              ,("inboundGovernor.Cold","")
              ,("inboundGovernor.Warm","")
              ,("inboundGovernor.Hot","")
              ]
      | last ons == "Local"
        =
              [("localInboundGovernor.idle","")
              ,("localInboundGovernor.cold","")
              ,("localInboundGovernor.warm","")
              ,("localInboundGovernor.hot","")
              ]
      | otherwise
        =
              [("inboundGovernor.Idle","")
              ,("inboundGovernor.Cold","")
              ,("inboundGovernor.Warm","")
              ,("inboundGovernor.Hot","")
              ]
    metricsDocFor _ = []

    allNamespaces = [
        Namespace [] ["NewConnection"]
      , Namespace [] ["ResponderRestarted"]
      , Namespace [] ["ResponderStartFailure"]
      , Namespace [] ["ResponderErrored"]
      , Namespace [] ["ResponderStarted"]
      , Namespace [] ["ResponderTerminated"]
      , Namespace [] ["PromotedToWarmRemote"]
      , Namespace [] ["PromotedToHotRemote"]
      , Namespace [] ["DemotedToColdRemote"]
      , Namespace [] ["DemotedToWarmRemote"]
      , Namespace [] ["WaitIdleRemote"]
      , Namespace [] ["MuxCleanExit"]
      , Namespace [] ["MuxErrored"]
      , Namespace [] ["InboundGovernorCounters"]
      , Namespace [] ["RemoteState"]
      , Namespace [] ["UnexpectedlyFalseAssertion"]
      , Namespace [] ["InboundGovernorError"]
      , Namespace [] ["MaturedConnections"]
      , Namespace [] ["Inactive"]
      ]

--------------------------------------------------------------------------------
-- InboundGovernor Transition Tracer
--------------------------------------------------------------------------------

instance (Show peerAddr, ToJSON peerAddr)
      => LogFormatting (InboundGovernor.RemoteTransitionTrace peerAddr) where
    forMachine _dtal (InboundGovernor.TransitionTrace peerAddr tr) =
      mconcat $ reverse
        [ "kind"    .= String "ConnectionManagerTransition"
        , "address" .= toJSON peerAddr
        , "from"    .= toJSON (ConnectionManager.fromState tr)
        , "to"      .= toJSON (ConnectionManager.toState   tr)
        ]
    forHuman = pack . show
    asMetrics _ = []

instance MetaTrace (InboundGovernor.RemoteTransitionTrace peerAddr) where
    namespaceFor InboundGovernor.TransitionTrace {} = Namespace [] ["Transition"]

    severityFor  (Namespace [] ["Transition"]) _ = Just Debug
    severityFor _ _ = Nothing

    documentFor  (Namespace [] ["Transition"]) = Just ""
    documentFor _ = Nothing

    allNamespaces = [Namespace [] ["Transition"]]

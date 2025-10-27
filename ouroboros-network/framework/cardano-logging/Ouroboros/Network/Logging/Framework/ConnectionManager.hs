{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}

--------------------------------------------------------------------------------

-- Orphan instances module for Cardano tracer.
{-# OPTIONS_GHC -Wno-orphans #-}
-- Extracted from "cardano-node" `Cardano.Node.Tracing.Tracers.P2P`.
-- Branch "master" (2025-02-28, 8c6a9f89fd8bb5b97dba2ae3a4c50873566fe14e).

--------------------------------------------------------------------------------

module Ouroboros.Network.Logging.Framework.ConnectionManager () where

--------------------------------------------------------------------------------

---------
-- base -
---------
--
---------------------
-- Package: "aeson" -
---------------------
import "aeson" Data.Aeson (ToJSON, toJSON, Value (String), object, (.=))
import "aeson" Data.Aeson.Types (listValue)
--------------------------
-- Package: "containers" -
--------------------------
import qualified "containers" Data.Map.Strict as Map
import qualified "containers" Data.Set as Set
---------------------------------
-- Package: "ouroboros-network" -
---------------------------------
import           "ouroboros-network" -- "ouroboros-newtwork:framework"
  Ouroboros.Network.ConnectionHandler
    ( ConnectionHandlerTrace (..) )
import           "ouroboros-network" -- "ouroboros-newtwork:framework"
  Ouroboros.Network.ConnectionId
    ( ConnectionId (..) )
import           "ouroboros-network" -- "ouroboros-newtwork:framework"
  Ouroboros.Network.ConnectionManager.ConnMap
    ( ConnMap (..) )
import           "ouroboros-network" -- "ouroboros-newtwork:framework"
  Ouroboros.Network.ConnectionManager.Core as ConnectionManager
    ( Trace (..) )
import           "ouroboros-network" -- "ouroboros-newtwork:framework"
  Ouroboros.Network.ConnectionManager.Types
    ( ConnectionManagerCounters (..) )
import qualified "ouroboros-network" -- "ouroboros-newtwork:framework"
  Ouroboros.Network.ConnectionManager.Types as ConnectionManager
import           "ouroboros-network" -- "ouroboros-newtwork:framework"
  Ouroboros.Network.RethrowPolicy
    ( ErrorCommand (..) )
-- Needed for `instance ToJSON ConnectionManager.AbstractState where`.
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

--------------------------------------------------------------------------------
-- Connection Manager Tracer.
--------------------------------------------------------------------------------

instance (Show addr, LogFormatting addr, ToJSON addr, LogFormatting handler, Show handler)
      => LogFormatting (ConnectionManager.Trace addr handler) where
    forMachine dtal (TrIncludeConnection prov peerAddr) =
        mconcat $ reverse
          [ "kind" .= String "IncludeConnection"
          , "remoteAddress" .= forMachine dtal peerAddr
          , "provenance" .= String (pack . show $ prov)
          ]
    forMachine dtal (TrInboundConnectionNotFound peerAddr) =
        mconcat $ reverse
          [ "kind" .= String "InboundConnectionNotFound"
          , "remoteAddress" .= forMachine dtal peerAddr
          ]
    forMachine _dtal (TrReleaseConnection prov connId) =
        mconcat $ reverse
          [ "kind" .= String "UnregisterConnection"
          , "remoteAddress" .= toJSON connId
          , "provenance" .= String (pack . show $ prov)
          ]
    forMachine _dtal (TrConnect (Just localAddress) remoteAddress diffusionMode) =
        mconcat
          [ "kind" .= String "Connect"
          , "connectionId" .= toJSON ConnectionId { localAddress, remoteAddress }
          , "diffusionMode" .= toJSON diffusionMode
          ]
    forMachine dtal (TrConnect Nothing remoteAddress diffusionMode) =
        mconcat
          [ "kind" .= String "Connect"
          , "remoteAddress" .= forMachine dtal remoteAddress
          , "diffusionMode" .= toJSON diffusionMode
          ]
    forMachine _dtal (TrConnectError (Just localAddress) remoteAddress err) =
        mconcat
          [ "kind" .= String "ConnectError"
          , "connectionId" .= toJSON ConnectionId { localAddress, remoteAddress }
          , "reason" .= String (pack . show $ err)
          ]
    forMachine dtal (TrConnectError Nothing remoteAddress err) =
        mconcat
          [ "kind" .= String "ConnectError"
          , "remoteAddress" .= forMachine dtal remoteAddress
          , "reason" .= String (pack . show $ err)
          ]
    forMachine _dtal (TrTerminatingConnection prov connId) =
        mconcat
          [ "kind" .= String "TerminatingConnection"
          , "provenance" .= String (pack . show $ prov)
          , "connectionId" .= toJSON connId
          ]
    forMachine dtal (TrTerminatedConnection prov remoteAddress) =
        mconcat
          [ "kind" .= String "TerminatedConnection"
          , "provenance" .= String (pack . show $ prov)
          , "remoteAddress" .= forMachine dtal remoteAddress
          ]
    forMachine dtal (TrConnectionHandler connId handler) =
        mconcat
          [ "kind" .= String "ConnectionHandler"
          , "connectionId" .= toJSON connId
          , "connectionHandler" .= forMachine dtal handler
          ]
    forMachine _dtal TrShutdown =
        mconcat
          [ "kind" .= String "Shutdown"
          ]
    forMachine dtal (TrConnectionExists prov remoteAddress inState) =
        mconcat
          [ "kind" .= String "ConnectionExists"
          , "provenance" .= String (pack . show $ prov)
          , "remoteAddress" .= forMachine dtal remoteAddress
          , "state" .= toJSON inState
          ]
    forMachine _dtal (TrForbiddenConnection connId) =
        mconcat
          [ "kind" .= String "ForbiddenConnection"
          , "connectionId" .= toJSON connId
          ]
    forMachine _dtal (TrConnectionFailure connId) =
        mconcat
          [ "kind" .= String "ConnectionFailure"
          , "connectionId" .= toJSON connId
          ]
    forMachine dtal (TrConnectionNotFound prov remoteAddress) =
        mconcat
          [ "kind" .= String "ConnectionNotFound"
          , "remoteAddress" .= forMachine dtal remoteAddress
          , "provenance" .= String (pack . show $ prov)
          ]
    forMachine dtal (TrForbiddenOperation remoteAddress connState) =
        mconcat
          [ "kind" .= String "ForbiddenOperation"
          , "remoteAddress" .= forMachine dtal remoteAddress
          , "connectionState" .= toJSON connState
          ]
    forMachine _dtal (TrPruneConnections pruningSet numberPruned chosenPeers) =
        mconcat
          [ "kind" .= String "PruneConnections"
          , "prunedPeers" .= toJSON pruningSet
          , "numberPrunedPeers" .= toJSON numberPruned
          , "choiceSet" .= toJSON (toJSON `Set.map` chosenPeers)
          ]
    forMachine _dtal (TrConnectionCleanup connId) =
        mconcat
          [ "kind" .= String "ConnectionCleanup"
          , "connectionId" .= toJSON connId
          ]
    forMachine _dtal (TrConnectionTimeWait connId) =
        mconcat
          [ "kind" .= String "ConnectionTimeWait"
          , "connectionId" .= toJSON connId
          ]
    forMachine _dtal (TrConnectionTimeWaitDone connId) =
        mconcat
          [ "kind" .= String "ConnectionTimeWaitDone"
          , "connectionId" .= toJSON connId
          ]
    forMachine _dtal (TrConnectionManagerCounters cmCounters) =
        mconcat
          [ "kind"  .= String "ConnectionManagerCounters"
          , "state" .= toJSON cmCounters
          ]
    forMachine _dtal (TrState cmState) =
        mconcat
          [ "kind"  .= String "ConnectionManagerState"
          , "state" .= listValue (\(remoteAddr, inner) ->
                                         object
                                           [ "connections" .=
                                             listValue (\(localAddr, connState) ->
                                                object
                                                  [ "localAddress" .= localAddr
                                                  , "state" .= toJSON connState
                                                  ]
                                             )
                                             (Map.toList inner)
                                           , "remoteAddress" .= toJSON remoteAddr
                                           ]
                                 )
                                 (Map.toList (getConnMap cmState))
          ]
    forMachine _dtal (ConnectionManager.TrUnexpectedlyFalseAssertion info) =
        mconcat
          [ "kind" .= String "UnexpectedlyFalseAssertion"
          , "info" .= String (pack . show $ info)
          ]
    forHuman = pack . show
    asMetrics (TrConnectionManagerCounters ConnectionManagerCounters {..}) =
          [ IntM
              "connectionManager.fullDuplexConns"
              (fromIntegral fullDuplexConns)
          , IntM
              "connectionManager.duplexConns"
              (fromIntegral duplexConns)
          , IntM
              "connectionManager.unidirectionalConns"
              (fromIntegral unidirectionalConns)
          , IntM
              "connectionManager.inboundConns"
              (fromIntegral inboundConns)
          , IntM
              "connectionManager.outboundConns"
              (fromIntegral outboundConns)
            ]
    asMetrics _ = []

instance (Show versionNumber, ToJSON versionNumber, ToJSON agreedOptions)
  => LogFormatting (ConnectionHandlerTrace versionNumber agreedOptions) where
    forMachine _dtal (TrHandshakeSuccess versionNumber agreedOptions) =
      mconcat
        [ "kind" .= String "HandshakeSuccess"
        , "versionNumber" .= toJSON versionNumber
        , "agreedOptions" .= toJSON agreedOptions
        ]
    forMachine _dtal (TrHandshakeQuery vMap) =
      mconcat
        [ "kind" .= String "HandshakeQuery"
        , "versions" .= toJSON ((\(k,v) -> object [
            "versionNumber" .= k
          , "options" .= v
          ]) <$> Map.toList vMap)
        ]
    forMachine _dtal (TrHandshakeClientError err) =
      mconcat
        [ "kind" .= String "HandshakeClientError"
        , "reason" .= toJSON err
        ]
    forMachine _dtal (TrHandshakeServerError err) =
      mconcat
        [ "kind" .= String "HandshakeServerError"
        , "reason" .= toJSON err
        ]
    forMachine _dtal (TrConnectionHandlerError e err cerr) =
      mconcat
        [ "kind" .= String "Error"
        , "context" .= show e
        , "reason" .= show err
        , "command" .= show cerr
        ]

instance MetaTrace handler => MetaTrace (ConnectionManager.Trace addr handler) where
    namespaceFor TrIncludeConnection {}  = Namespace [] ["IncludeConnection"]
    namespaceFor TrInboundConnectionNotFound {} = Namespace [] ["InboundConnectionNotFound"]
    namespaceFor TrReleaseConnection {}  = Namespace [] ["UnregisterConnection"]
    namespaceFor TrConnect {}  = Namespace [] ["Connect"]
    namespaceFor TrConnectError {}  = Namespace [] ["ConnectError"]
    namespaceFor TrTerminatingConnection {}  = Namespace [] ["TerminatingConnection"]
    namespaceFor TrTerminatedConnection {}  = Namespace [] ["TerminatedConnection"]
    namespaceFor (TrConnectionHandler _ hdl)  = 
      nsPrependInner "ConnectionHandler" (namespaceFor hdl)      
    namespaceFor TrShutdown {}  = Namespace [] ["Shutdown"]
    namespaceFor TrConnectionExists {}  = Namespace [] ["ConnectionExists"]
    namespaceFor TrForbiddenConnection {}  = Namespace [] ["ForbiddenConnection"]
    namespaceFor TrConnectionFailure {}  = Namespace [] ["ConnectionFailure"]
    namespaceFor TrConnectionNotFound {}  = Namespace [] ["ConnectionNotFound"]
    namespaceFor TrForbiddenOperation {}  = Namespace [] ["ForbiddenOperation"]
    namespaceFor TrPruneConnections {}  = Namespace [] ["PruneConnections"]
    namespaceFor TrConnectionCleanup {}  = Namespace [] ["ConnectionCleanup"]
    namespaceFor TrConnectionTimeWait {}  = Namespace [] ["ConnectionTimeWait"]
    namespaceFor TrConnectionTimeWaitDone {}  = Namespace [] ["ConnectionTimeWaitDone"]
    namespaceFor TrConnectionManagerCounters {}  = Namespace [] ["ConnectionManagerCounters"]
    namespaceFor TrState {}  = Namespace [] ["State"]
    namespaceFor ConnectionManager.TrUnexpectedlyFalseAssertion {}  =
      Namespace [] ["UnexpectedlyFalseAssertion"]

    severityFor (Namespace _  ["IncludeConnection"]) _ = Just Debug
    severityFor (Namespace _  ["UnregisterConnection"]) _ = Just Debug
    severityFor (Namespace _  ["Connect"]) _ = Just Debug
    severityFor (Namespace _  ["ConnectError"]) _ = Just Info
    severityFor (Namespace _  ["TerminatingConnection"]) _ = Just Debug
    severityFor (Namespace _  ["TerminatedConnection"]) _ = Just Debug
    severityFor (Namespace out ("ConnectionHandler" : tl)) (Just (TrConnectionHandler _ hdl)) =
      severityFor (Namespace out tl) (Just hdl)
    severityFor (Namespace _  ("ConnectionHandler" : _)) Nothing = Just Info  
    severityFor (Namespace _  ["Shutdown"]) _ = Just Info
    severityFor (Namespace _  ["ConnectionExists"]) _ = Just Info
    severityFor (Namespace _  ["ForbiddenConnection"]) _ = Just Info
    severityFor (Namespace _  ["ConnectionFailure"]) _ = Just Info
    severityFor (Namespace _  ["ConnectionNotFound"]) _ = Just Debug
    severityFor (Namespace _  ["ForbiddenOperation"]) _ = Just Info
    severityFor (Namespace _  ["PruneConnections"]) _ = Just Notice
    severityFor (Namespace _  ["ConnectionCleanup"]) _ = Just Debug
    severityFor (Namespace _  ["ConnectionTimeWait"]) _ = Just Debug
    severityFor (Namespace _  ["ConnectionTimeWaitDone"]) _ = Just Info
    severityFor (Namespace _  ["ConnectionManagerCounters"]) _ = Just Info
    severityFor (Namespace _  ["State"]) _ = Just Info
    severityFor (Namespace _  ["UnexpectedlyFalseAssertion"]) _ = Just Error
    severityFor _ _ = Nothing

    documentFor (Namespace _  ["IncludeConnection"]) = Just ""
    documentFor (Namespace _  ["UnregisterConnection"]) = Just ""
    documentFor (Namespace _  ["Connect"]) = Just ""
    documentFor (Namespace _  ["ConnectError"]) = Just ""
    documentFor (Namespace _  ["TerminatingConnection"]) = Just ""
    documentFor (Namespace _  ["TerminatedConnection"]) = Just ""
    documentFor (Namespace out ("ConnectionHandler" : tl)) =
      documentFor (Namespace out tl :: Namespace handler) 
    documentFor (Namespace _  ["Shutdown"]) = Just ""
    documentFor (Namespace _  ["ConnectionExists"]) = Just ""
    documentFor (Namespace _  ["ForbiddenConnection"]) = Just ""
    documentFor (Namespace _  ["ConnectionFailure"]) = Just ""
    documentFor (Namespace _  ["ConnectionNotFound"]) = Just ""
    documentFor (Namespace _  ["ForbiddenOperation"]) = Just ""
    documentFor (Namespace _  ["PruneConnections"]) = Just ""
    documentFor (Namespace _  ["ConnectionCleanup"]) = Just ""
    documentFor (Namespace _  ["ConnectionTimeWait"]) = Just ""
    documentFor (Namespace _  ["ConnectionTimeWaitDone"]) = Just ""
    documentFor (Namespace _  ["ConnectionManagerCounters"]) = Just ""
    documentFor (Namespace _  ["State"]) = Just ""
    documentFor (Namespace _  ["UnexpectedlyFalseAssertion"]) = Just ""
    documentFor _ = Nothing

    metricsDocFor (Namespace _  ["ConnectionManagerCounters"]) =
      [("connectionManager.fullDuplexConns","")
      ,("connectionManager.duplexConns","")
      ,("connectionManager.unidirectionalConns","")
      ,("connectionManager.inboundConns","")
      ,("connectionManager.outboundConns","")
      ,("connectionManager.prunableConns","")
      ]
    metricsDocFor _ = []

    allNamespaces = [
        Namespace [] ["IncludeConnection"]
      , Namespace [] ["UnregisterConnection"]
      , Namespace [] ["Connect"]
      , Namespace [] ["ConnectError"]
      , Namespace [] ["TerminatingConnection"]
      , Namespace [] ["TerminatedConnection"]
      , Namespace [] ["Shutdown"]
      , Namespace [] ["ConnectionExists"]
      , Namespace [] ["ForbiddenConnection"]
      , Namespace [] ["ConnectionFailure"]
      , Namespace [] ["ConnectionNotFound"]
      , Namespace [] ["ForbiddenOperation"]
      , Namespace [] ["PruneConnections"]
      , Namespace [] ["ConnectionCleanup"]
      , Namespace [] ["ConnectionTimeWait"]
      , Namespace [] ["ConnectionTimeWaitDone"]
      , Namespace [] ["ConnectionManagerCounters"]
      , Namespace [] ["State"]
      , Namespace [] ["UnexpectedlyFalseAssertion"]]
      ++ map  (nsPrependInner "ConnectionHandler")
                  (allNamespaces :: [Namespace handler])


instance MetaTrace (ConnectionHandlerTrace versionNumber agreedOptions) where     
    namespaceFor TrHandshakeSuccess {} = Namespace [] ["HandshakeSuccess"]
    namespaceFor TrHandshakeQuery {} = Namespace [] ["HandshakeQuery"]
    namespaceFor TrHandshakeClientError {} = Namespace [] ["HandshakeClientError"]
    namespaceFor TrHandshakeServerError {} = Namespace [] ["HandshakeServerError"]
    namespaceFor TrConnectionHandlerError {} = Namespace [] ["Error"]

    severityFor (Namespace _ ["HandshakeSuccess"]) _ = Just Info
    severityFor (Namespace _ ["HandshakeQuery"]) _ = Just Info
    severityFor (Namespace _ ["HandshakeClientError"]) _ = Just Notice
    severityFor (Namespace _ ["HandshakeServerError"]) _ = Just Info
    severityFor (Namespace _ ["Error"]) (Just (TrConnectionHandlerError _ _ ShutdownNode)) = Just Critical
    severityFor (Namespace _ ["Error"]) (Just (TrConnectionHandlerError _ _ ShutdownPeer)) = Just Info
    severityFor (Namespace _ ["Error"]) Nothing = Just Info
    severityFor _ _ = Nothing

    documentFor (Namespace _ ["HandshakeSuccess"]) = Just ""
    documentFor (Namespace _ ["HandshakeQuery"]) = Just ""
    documentFor (Namespace _ ["HandshakeClientError"]) = Just ""
    documentFor (Namespace _ ["HandshakeServerError"]) = Just ""
    documentFor (Namespace _ ["Error"]) = Just ""
    documentFor _ = Nothing

    allNamespaces = [
        Namespace [] ["HandshakeSuccess"]
      , Namespace [] ["HandshakeQuery"]
      , Namespace [] ["HandshakeClientError"]
      , Namespace [] ["HandshakeServerError"]
      , Namespace [] ["Error"]
      ]

--------------------------------------------------------------------------------
-- Connection Manager Transition Tracer.
--------------------------------------------------------------------------------

instance (Show peerAddr, ToJSON peerAddr)
      => LogFormatting (ConnectionManager.AbstractTransitionTrace peerAddr) where
    forMachine _dtal (ConnectionManager.TransitionTrace peerAddr tr) =
      mconcat $ reverse
        [ "kind"    .= String "ConnectionManagerTransition"
        , "address" .= toJSON peerAddr
        , "from"    .= toJSON (ConnectionManager.fromState tr)
        , "to"      .= toJSON (ConnectionManager.toState   tr)
        ]

    forHuman = pack . show

    asMetrics _ = []

instance MetaTrace (ConnectionManager.AbstractTransitionTrace peerAddr) where
    namespaceFor ConnectionManager.TransitionTrace {} =
      Namespace [] ["Transition"]

    severityFor (Namespace _  ["Transition"]) _ = Just Debug
    severityFor _ _ = Nothing

    documentFor (Namespace _  ["Transition"]) = Just ""
    documentFor _ = Nothing

    allNamespaces = [Namespace [] ["Transition"]]

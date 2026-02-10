{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}

--------------------------------------------------------------------------------

-- Orphan instances module for Cardano tracer.
{-# OPTIONS_GHC -Wno-orphans #-}
-- Extracted from "cardano-node" `Cardano.Node.Tracing.Tracers.Diffusion`.
-- Branch "master" (2025-02-28, 8c6a9f89fd8bb5b97dba2ae3a4c50873566fe14e).

--------------------------------------------------------------------------------

module Ouroboros.Network.Logging.Framework () where

--------------------------------------------------------------------------------

---------
-- base -
---------
--
---------------------
-- Package: "aeson" -
---------------------
import "aeson" Data.Aeson (Value (String), (.=))
-----------------------
-- Package: "iproute" -
-----------------------
import "iproute" Data.IP qualified as IP
-----------------------
-- Package: "network" -
-----------------------
import "network" Network.Socket (SockAddr (..))
--------------------
-- Package: "text" -
--------------------
import "text" Data.Text (Text, pack)
---------------------------------
-- Package: "ouroboros-network" -
---------------------------------
import "ouroboros-network" Ouroboros.Network.Protocol.Handshake.Type qualified as HS
import "ouroboros-network" Ouroboros.Network.Snocket (LocalAddress (..),
           RemoteAddress)
-------------------------------
-- Package: "typed-protocols" -
-------------------------------
import "typed-protocols" Network.TypedProtocol.Codec (AnyMessage (..))
--------------------------------
-- Package: "trace-dispatcher" -
--------------------------------
import "trace-dispatcher" Cardano.Logging
---------
-- Self -
---------
import Ouroboros.Network.Logging.Framework.ConnectionId ()
import Ouroboros.Network.Logging.Framework.ConnectionManager ()
import Ouroboros.Network.Logging.Framework.Driver ()
import Ouroboros.Network.Logging.Framework.InboundGovernor ()
import Ouroboros.Network.Logging.Framework.Server ()

--------------------------------------------------------------------------------
-- Addresses.
--------------------------------------------------------------------------------

-- From `Cardano.Node.Tracing.Tracers.P2P`
-- Branch "ana/10.6-final-integration-mix"

instance LogFormatting LocalAddress where
    forMachine _dtal (LocalAddress path) =
        mconcat ["path" .= path]

instance LogFormatting RemoteAddress where
    forMachine _dtal (SockAddrInet port addr) =
        let ip = IP.fromHostAddress addr in
        mconcat [ "addr" .= show ip
                 , "port" .= show port
                 ]
    forMachine _dtal (SockAddrInet6 port _ addr _) =
        let ip = IP.fromHostAddress6 addr in
        mconcat [ "addr" .= show ip
                 , "port" .= show port
                 ]
    forMachine _dtal (SockAddrUnix path) =
        mconcat [ "path" .= show path ]

--------------------------------------------------------------------------------
-- Handshake Tracer.
--------------------------------------------------------------------------------

-- From `Cardano.Node.Tracing.Tracers.Diffusion`
-- Branch "ana/10.6-final-integration-mix"

instance (Show term, Show ntcVersion) =>
  LogFormatting (AnyMessage (HS.Handshake ntcVersion term)) where
  forMachine _dtal (AnyMessageAndAgency stok msg) =
    mconcat [ "kind" .= String kind
            , "msg" .= (String . showT $ msg)
            , "agency" .= String (pack $ show stok)
            ]
    where
      kind = case msg of
        HS.MsgProposeVersions {} -> "ProposeVersions"
        HS.MsgReplyVersions   {} -> "ReplyVersions"
        HS.MsgQueryReply      {} -> "QueryReply"
        HS.MsgAcceptVersion   {} -> "AcceptVersion"
        HS.MsgRefuse          {} -> "Refuse"

  forHuman (AnyMessageAndAgency stok msg) =
    "Handshake (agency, message) = " <> "(" <> showT stok <> "," <> showT msg <> ")"

instance MetaTrace (AnyMessage (HS.Handshake a b)) where
  namespaceFor (AnyMessage msg) = Namespace [] $ case msg of
    HS.MsgProposeVersions {} -> ["ProposeVersions"]
    HS.MsgReplyVersions   {} -> ["ReplyVersions"]
    HS.MsgQueryReply      {} -> ["QueryReply"]
    HS.MsgAcceptVersion   {} -> ["AcceptVersion"]
    HS.MsgRefuse          {} -> ["Refuse"]

  severityFor (Namespace _ [sym]) _ = case sym of
    "ProposeVersions" -> Just Info
    "ReplyVersions"   -> Just Info
    "QueryReply"      -> Just Info
    "AcceptVersion"   -> Just Info
    "Refuse"          -> Just Info
    _otherwise        -> Nothing
  severityFor _ _ = Nothing

  documentFor (Namespace _ sym) = wrap . mconcat $ case sym of
    ["ProposeVersions"] ->
      [ "Propose versions together with version parameters.  It must be"
      , " encoded to a sorted list.."
      ]
    ["ReplyVersions"]   ->
      [ "`MsgReplyVersions` received as a response to 'MsgProposeVersions'.  It"
      , " is not supported to explicitly send this message. It can only be"
      , " received as a copy of 'MsgProposeVersions' in a simultaneous open"
      , " scenario."
      ]
    ["QueryReply"]      ->
      [ "`MsgQueryReply` received as a response to a handshake query in "
      , " 'MsgProposeVersions' and lists the supported versions."
      ]
    ["AcceptVersion"]   ->
      [ "The remote end decides which version to use and sends chosen version."
      , "The server is allowed to modify version parameters."
      ]
    ["Refuse"]          -> ["It refuses to run any version."]
    _otherwise          -> [] :: [Text]
    where
      wrap it = case it of
        ""  -> Nothing
        it' -> Just it'

  allNamespaces = [
      Namespace [] ["ProposeVersions"]
    , Namespace [] ["ReplyVersions"]
    , Namespace [] ["QueryReply"]
    , Namespace [] ["AcceptVersion"]
    , Namespace [] ["Refuse"]
    ]


{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Network.Tracing.LocalStateQuery where

import Cardano.Logging
import Ouroboros.Network.Protocol.LocalStateQuery.Type qualified as LSQ

import Data.Aeson (Value (String), (.=))
import Data.Text qualified as Text
import Network.TypedProtocol.Stateful.Codec qualified as Stateful

instance (forall result. Show (query result))
      => LogFormatting (Stateful.AnyMessage (LSQ.LocalStateQuery blk pt query) f) where
  forMachine _dtal (Stateful.AnyMessageAndAgency stok _ LSQ.MsgAcquire{}) =
    mconcat [ "kind" .= String "MsgAcquire"
             , "agency" .= String (Text.pack $ show stok)
             ]
  forMachine _dtal (Stateful.AnyMessageAndAgency stok _ LSQ.MsgAcquired{}) =
    mconcat [ "kind" .= String "MsgAcquired"
             , "agency" .= String (Text.pack $ show stok)
             ]
  forMachine _dtal (Stateful.AnyMessageAndAgency stok _ LSQ.MsgFailure{}) =
    mconcat [ "kind" .= String "MsgFailure"
             , "agency" .= String (Text.pack $ show stok)
             ]
  forMachine _dtal (Stateful.AnyMessageAndAgency stok _ LSQ.MsgQuery{}) =
    mconcat [ "kind" .= String "MsgQuery"
             , "agency" .= String (Text.pack $ show stok)
             ]
  forMachine _dtal (Stateful.AnyMessageAndAgency stok _ LSQ.MsgResult{}) =
    mconcat [ "kind" .= String "MsgResult"
             , "agency" .= String (Text.pack $ show stok)
             ]
  forMachine _dtal (Stateful.AnyMessageAndAgency stok _ LSQ.MsgRelease{}) =
    mconcat [ "kind" .= String "MsgRelease"
             , "agency" .= String (Text.pack $ show stok)
             ]
  forMachine _dtal (Stateful.AnyMessageAndAgency stok _ LSQ.MsgReAcquire{}) =
    mconcat [ "kind" .= String "MsgReAcquire"
             , "agency" .= String (Text.pack $ show stok)
             ]
  forMachine _dtal (Stateful.AnyMessageAndAgency stok _ LSQ.MsgDone{}) =
    mconcat [ "kind" .= String "MsgDone"
             , "agency" .= String (Text.pack $ show stok)
             ]

instance MetaTrace (Stateful.AnyMessage (LSQ.LocalStateQuery blk pt query) f) where
    namespaceFor (Stateful.AnyMessageAndAgency _agency _ LSQ.MsgAcquire{}) =
      Namespace [] ["Acquire"]
    namespaceFor (Stateful.AnyMessageAndAgency _agency _ LSQ.MsgAcquired{}) =
      Namespace [] ["Acquired"]
    namespaceFor (Stateful.AnyMessageAndAgency _agency _ LSQ.MsgFailure{}) =
      Namespace [] ["Failure"]
    namespaceFor (Stateful.AnyMessageAndAgency _agency _ LSQ.MsgQuery{}) =
      Namespace [] ["Query"]
    namespaceFor (Stateful.AnyMessageAndAgency _agency _ LSQ.MsgResult{}) =
      Namespace [] ["Result"]
    namespaceFor (Stateful.AnyMessageAndAgency _agency _ LSQ.MsgRelease{}) =
      Namespace [] ["Release"]
    namespaceFor (Stateful.AnyMessageAndAgency _agency _ LSQ.MsgReAcquire{}) =
      Namespace [] ["ReAcquire"]
    namespaceFor (Stateful.AnyMessageAndAgency _agency _ LSQ.MsgDone{}) =
      Namespace [] ["Done"]

    severityFor (Namespace _ ["Acquire"]) _ = Just Debug
    severityFor (Namespace _ ["Acquired"]) _ = Just Debug
    severityFor (Namespace _ ["Failure"]) _ = Just Warning
    severityFor (Namespace _ ["Query"]) _ = Just Debug
    severityFor (Namespace _ ["Result"]) _ = Just Debug
    severityFor (Namespace _ ["Release"]) _ = Just Debug
    severityFor (Namespace _ ["ReAcquire"]) _ = Just Debug
    severityFor (Namespace _ ["Done"]) _ = Just Debug
    severityFor _ _ = Nothing

    documentFor (Namespace _ ["Acquire"]) = Just $ mconcat
      [ "The client requests that the state as of a particular recent point on "
      , "the server's chain (within K of the tip) be made available to query, "
      , "and waits for confirmation or failure. "
      , "\n "
      , "From 'NodeToClient_V8' onwards if the point is not specified, current tip "
      , "will be acquired.  For previous versions of the protocol 'point' must be "
      , "given."
      ]
    documentFor (Namespace _ ["Acquired"]) = Just
      "The server can confirm that it has the state at the requested point."
    documentFor (Namespace _ ["Failure"]) = Just $ mconcat
      [ "The server can report that it cannot obtain the state for the "
      , "requested point."
      ]
    documentFor (Namespace _ ["Query"]) = Just
      "The client can perform queries on the current acquired state."
    documentFor (Namespace _ ["Result"]) = Just
      "The server must reply with the queries."
    documentFor (Namespace _ ["Release"]) = Just $ mconcat
      [ "The client can instruct the server to release the state. This lets "
      , "the server free resources."
      ]
    documentFor (Namespace _ ["ReAcquire"]) = Just $ mconcat
      [ "This is like 'MsgAcquire' but for when the client already has a "
      , "state. By moving to another state directly without a 'MsgRelease' it "
      , "enables optimisations on the server side (e.g. moving to the state for "
      , "the immediate next block). "
      , "\n "
      , "Note that failure to re-acquire is equivalent to 'MsgRelease', "
      , "rather than keeping the exiting acquired state. "
      , "\n "
      , "From 'NodeToClient_V8' onwards if the point is not specified, current tip "
      , "will be acquired.  For previous versions of the protocol 'point' must be "
      , "given."
      ]
    documentFor (Namespace _ ["Done"]) = Just
      "The client can terminate the protocol."
    documentFor _ = Nothing

    allNamespaces = [
        Namespace [] ["Acquire"]
      , Namespace [] ["Acquired"]
      , Namespace [] ["Failure"]
      , Namespace [] ["Query"]
      , Namespace [] ["Result"]
      , Namespace [] ["Release"]
      , Namespace [] ["ReAcquire"]
      , Namespace [] ["Done"]
      ]

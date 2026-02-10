{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}

--------------------------------------------------------------------------------

-- Orphan instances module for Cardano tracer.
{-# OPTIONS_GHC -Wno-orphans #-}
-- Extracted from "cardano-node" `Cardano.Node.Tracing.Tracers.Consensus`.
-- Branch "master" (2025-02-28, 8c6a9f89fd8bb5b97dba2ae3a4c50873566fe14e).

--------------------------------------------------------------------------------

module Ouroboros.Network.Logging.Framework.ConnectionId () where

--------------------------------------------------------------------------------

---------
-- base -
---------
--
---------------------
-- Package: "aeson" -
---------------------
import "aeson" Data.Aeson (Value (String), (.=))
---------------------------------
-- Package: "ouroboros-network" -
---------------------------------
import "ouroboros-network" Ouroboros.Network.ConnectionId (ConnectionId (..))
--------------------------------
-- Package: "trace-dispatcher" -
--------------------------------
import "trace-dispatcher" Cardano.Logging

--------------------------------------------------------------------------------
-- Types instances.
--------------------------------------------------------------------------------

instance (LogFormatting adr, Show adr) => LogFormatting (ConnectionId adr) where
  forMachine _dtal (ConnectionId local' remote) =
    mconcat [ "connectionId" .= String (showT local'
                                          <> " "
                                          <> showT remote)
    ]
  forHuman (ConnectionId local' remote) =
    "ConnectionId " <>  showT local' <> " " <> showT remote

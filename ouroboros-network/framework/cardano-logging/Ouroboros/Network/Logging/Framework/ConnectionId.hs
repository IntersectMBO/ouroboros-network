{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

--------------------------------------------------------------------------------

-- Orphan instances module for Cardano tracer.
{-# OPTIONS_GHC -Wno-orphans #-}
-- Extracted from "cardano-node" `Cardano.Node.Tracing.Tracers.Consensus`.
-- Branch "master" (2026-02-11, 85869e9dd21d9dac7c4381418346e97259c3303b).

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
import "ouroboros-network" -- "ouroboros-newtwork:framework"
  Ouroboros.Network.ConnectionId (ConnectionId (..))
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


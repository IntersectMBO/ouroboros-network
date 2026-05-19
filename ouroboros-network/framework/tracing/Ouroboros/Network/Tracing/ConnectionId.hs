{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Network.Tracing.ConnectionId () where

import Cardano.Logging
import Data.Aeson (Value (String), (.=))
import Ouroboros.Network.ConnectionId (ConnectionId (..))

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

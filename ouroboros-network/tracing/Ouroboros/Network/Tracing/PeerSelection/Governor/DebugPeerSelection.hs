{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}

-- Orphan instances module for Cardano tracer.
{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Network.Tracing.PeerSelection.Governor.DebugPeerSelection () where

import Cardano.Logging
import Data.Aeson (Value (String), (.=))
import Data.Text (pack)
import Ouroboros.Network.PeerSelection.Governor.Types

import Ouroboros.Network.Tracing.PeerSelection.Governor.Utils
           (peerSelectionTargetsToObject)

--------------------------------------------------------------------------------
-- DebugPeerSelection Tracer
--------------------------------------------------------------------------------

instance ( Show extraState
         , Show extraFlags
         , Show extraPeers
         , Show peeraddr
         , SupportsPeerSelectionState extraPeers peeraddr
         , LogFormatting (PeerSelectionCounters (ViewExtraPeers extraPeers))
         )
      => LogFormatting (DebugPeerSelection extraState extraFlags extraPeers peeraddr) where
  forMachine dtal@DNormal (TraceGovernorState blockedAt wakeupAfter
                   st@PeerSelectionState { targets }) =
    mconcat [ "kind" .= String "DebugPeerSelection"
            , "blockedAt" .= String (pack $ show blockedAt)
            , "wakeupAfter" .= String (pack $ show wakeupAfter)
            , "targets" .= peerSelectionTargetsToObject targets
            , "counters" .= forMachine dtal (peerSelectionStateToCounters st)
            ]

  forMachine _ (TraceGovernorState blockedAt wakeupAfter ev) =
    mconcat [ "kind" .= String "DebugPeerSelection"
            , "blockedAt" .= String (pack $ show blockedAt)
            , "wakeupAfter" .= String (pack $ show wakeupAfter)
            , "peerSelectionState" .= String (pack $ show ev)
            ]

  forHuman = pack . show

instance MetaTrace (DebugPeerSelection extraState extraFlags extraPeers peeraddr) where
    namespaceFor TraceGovernorState {} = Namespace [] ["GovernorState"]

    severityFor (Namespace _ ["GovernorState"]) _ = Just Debug
    severityFor _ _                               = Nothing

    documentFor (Namespace _ ["GovernorState"]) = Just "Outbound peer selection internal state"
    documentFor _                               = Nothing

    allNamespaces = [
      Namespace [] ["GovernorState"]
      ]

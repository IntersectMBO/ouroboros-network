{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE UndecidableInstances  #-}
--------------------------------------------------------------------------------

-- Orphan instances module for Cardano tracer.
{-# OPTIONS_GHC -Wno-orphans #-}
-- Extracted from "cardano-node" `Cardano.Node.Tracing.Tracers.P2P`.
-- Branch "master" (2026-02-11, 85869e9dd21d9dac7c4381418346e97259c3303b).

{- TODO: All references to package "cardano-diffusion" were removed.
--       See all the TODO annotations.
import           "cardano-diffusion" -- "cardano-diffusion:???"
  Cardano.Network.PeerSelection.Governor.Monitor
    ( ExtraTrace (TraceLedgerStateJudgementChanged, TraceUseBootstrapPeersChanged)
    )
--}

--------------------------------------------------------------------------------

module Ouroboros.Network.Tracing.PeerSelection.Governor.DebugPeerSelection () where

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
-- Package: "network" -
-----------------------
import "network" Network.Socket (SockAddr)
---------------------------------
-- Package: "ouroboros-network" -
---------------------------------
import "ouroboros-network" Ouroboros.Network.PeerSelection.Governor.Types
           (DebugPeerSelection (..), PeerSelectionState (..))
--------------------
-- Package: "text" -
--------------------
import "text" Data.Text (pack)
--------------------------------
-- Package: "trace-dispatcher" -
--------------------------------
import "trace-dispatcher" Cardano.Logging
---------
-- Self -
---------
import Ouroboros.Network.Tracing.PeerSelection.Governor.Utils
           (peerSelectionTargetsToObject)

--------------------------------------------------------------------------------
-- DebugPeerSelection Tracer
--------------------------------------------------------------------------------

{-- TODO: Before "cardano-diffusion" removal:
instance LogFormatting (DebugPeerSelection Cardano.ExtraState PeerTrustable (Cardano.PublicRootPeers.ExtraPeers SockAddr) SockAddr) where
 -- TODO: That later changed in f550a6eb503cc81807419795ab2360e6042ce9d5:
instance LogFormatting CardanoDebugPeerSelection where
--}
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

    documentFor (Namespace _ ["GovernorState"]) = Just ""
    documentFor _                               = Nothing

    allNamespaces = [
      Namespace [] ["GovernorState"]
      ]

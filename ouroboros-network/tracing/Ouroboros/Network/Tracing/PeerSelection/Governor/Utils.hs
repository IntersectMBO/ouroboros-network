{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

--------------------------------------------------------------------------------

-- Extracted from "cardano-node" `Cardano.Node.Tracing.Tracers.P2P`.
-- Branch "master" (2026-02-11, 85869e9dd21d9dac7c4381418346e97259c3303b).

--------------------------------------------------------------------------------

module Ouroboros.Network.Tracing.PeerSelection.Governor.Utils
  ( peerSelectionTargetsToObject
  )
  where

--------------------------------------------------------------------------------

---------
-- base -
---------
--
---------------------
-- Package: "aeson" -
---------------------
import "aeson" Data.Aeson (Value (Object), (.=))
---------------------------------
-- Package: "ouroboros-network" -
---------------------------------
-- Needed for `ToJSON Network.Socket.Types.PortNumber`
import qualified "ouroboros-network" -- "ouroboros-network:orphan-instances"
  Ouroboros.Network.OrphanInstances ()
import           "ouroboros-network" -- "ouroboros-network:ouroboros-network"
  Ouroboros.Network.PeerSelection.Governor.Types
    ( PeerSelectionTargets (..)
    )

--------------------------------------------------------------------------------

peerSelectionTargetsToObject :: PeerSelectionTargets -> Value
peerSelectionTargetsToObject
  PeerSelectionTargets { targetNumberOfRootPeers,
                         targetNumberOfKnownPeers,
                         targetNumberOfEstablishedPeers,
                         targetNumberOfActivePeers,
                         targetNumberOfKnownBigLedgerPeers,
                         targetNumberOfEstablishedBigLedgerPeers,
                         targetNumberOfActiveBigLedgerPeers
                       } =
    Object $
      mconcat [ "roots" .= targetNumberOfRootPeers
               , "knownPeers" .= targetNumberOfKnownPeers
               , "established" .= targetNumberOfEstablishedPeers
               , "active" .= targetNumberOfActivePeers
               , "knownBigLedgerPeers" .= targetNumberOfKnownBigLedgerPeers
               , "establishedBigLedgerPeers" .= targetNumberOfEstablishedBigLedgerPeers
               , "activeBigLedgerPeers" .= targetNumberOfActiveBigLedgerPeers
               ]


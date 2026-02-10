{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}

--------------------------------------------------------------------------------

-- Extracted from "cardano-node" `Cardano.Node.Tracing.Tracers.P2P`.
-- Branch "master" (2025-02-28, 8c6a9f89fd8bb5b97dba2ae3a4c50873566fe14e).

--------------------------------------------------------------------------------

module Ouroboros.Network.Logging.PeerSelection.Governor.Utils (peerSelectionTargetsToObject) where

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
import "ouroboros-network" Ouroboros.Network.OrphanInstances qualified ()
import "ouroboros-network" Ouroboros.Network.PeerSelection.Governor.Types
           (PeerSelectionTargets (..))

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


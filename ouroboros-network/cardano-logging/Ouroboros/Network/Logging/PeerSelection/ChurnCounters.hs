{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

--------------------------------------------------------------------------------

-- Orphan instances module for Cardano tracer.
{-# OPTIONS_GHC -Wno-orphans #-}
-- Extracted from "cardano-node" `Cardano.Node.Tracing.Tracers.P2P`.
-- Branch "master" (2025-02-28, 8c6a9f89fd8bb5b97dba2ae3a4c50873566fe14e).

--------------------------------------------------------------------------------

module Ouroboros.Network.Logging.PeerSelection.ChurnCounters () where

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
import "ouroboros-network" -- "ouroboros-network:ouroboros-network"
  Ouroboros.Network.PeerSelection.Churn
    ( ChurnCounters (..) )
--------------------
-- Package: "text" -
--------------------
import "text" Data.Text (pack)
--------------------------------
-- Package: "trace-dispatcher" -
--------------------------------
import "trace-dispatcher" Cardano.Logging

--------------------------------------------------------------------------------
-- ChurnCounters Tracer.
--------------------------------------------------------------------------------

instance LogFormatting ChurnCounters where
  forMachine _dtal (ChurnCounter action c) =
    mconcat [ "kind" .= String "ChurnCounter"
            , "action" .= String (pack $ show action)
            , "counter" .= c
            ]
  asMetrics (ChurnCounter action c) =
    [ IntM
        ("peerSelection.churn." <> pack (show action))
        (fromIntegral c)
    ]

instance MetaTrace ChurnCounters where
    namespaceFor ChurnCounter {} = Namespace [] ["ChurnCounters"]

    severityFor (Namespace _ ["ChurnCounters"]) _ = Just Info
    severityFor _ _ = Nothing

    documentFor (Namespace _ ["ChurnCounters"]) = Just
      "churn counters"
    documentFor _ = Nothing

    metricsDocFor (Namespace _ ["ChurnCounters"]) =
     [ ("peerSelection.churn.DecreasedActivePeers", "number of decreased active peers")
     , ("peerSelection.churn.IncreasedActivePeers", "number of increased active peers")
     , ("peerSelection.churn.DecreasedActiveBigLedgerPeers", "number of decreased active big ledger peers")
     , ("peerSelection.churn.IncreasedActiveBigLedgerPeers", "number of increased active big ledger peers")
     , ("peerSelection.churn.DecreasedEstablishedPeers", "number of decreased established peers")
     , ("peerSelection.churn.IncreasedEstablishedPeers", "number of increased established peers")
     , ("peerSelection.churn.IncreasedEstablishedBigLedgerPeers", "number of increased established big ledger peers")
     , ("peerSelection.churn.DecreasedEstablishedBigLedgerPeers", "number of decreased established big ledger peers")
     , ("peerSelection.churn.DecreasedKnownPeers", "number of decreased known peers")
     , ("peerSelection.churn.IncreasedKnownPeers", "number of increased known peers")
     , ("peerSelection.churn.DecreasedKnownBigLedgerPeers", "number of decreased known big ledger peers")
     , ("peerSelection.churn.IncreasedKnownBigLedgerPeers", "number of increased known big ledger peers")
     ]
    metricsDocFor _ = []

    allNamespaces =[
      Namespace [] ["ChurnCounters"]
      ]


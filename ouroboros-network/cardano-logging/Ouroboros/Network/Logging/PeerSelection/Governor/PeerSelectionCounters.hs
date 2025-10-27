{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}

--------------------------------------------------------------------------------

-- Orphan instances module for Cardano tracer.
{-# OPTIONS_GHC -Wno-orphans #-}
-- Extracted from "cardano-node" `Cardano.Node.Tracing.Tracers.P2P`.
-- Branch "ana/10.6-final-integration-mix"

{- TODO: All references to package "cardano-diffusion" were removed.
--       See all the TODO annotations.
import           "cardano-diffusion" -- "cardano-diffusion:???"
  Cardano.Network.PeerSelection.Governor.Monitor
    ( ExtraTrace (TraceLedgerStateJudgementChanged, TraceUseBootstrapPeersChanged)
    )
--}

--------------------------------------------------------------------------------

module Ouroboros.Network.Logging.PeerSelection.Governor.PeerSelectionCounters () where

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
  Ouroboros.Network.PeerSelection.Governor.Types
    ( PeerSelectionCounters
    , PeerSelectionView (..)
    )
--------------------------------
-- Package: "trace-dispatcher" -
--------------------------------
import "trace-dispatcher" Cardano.Logging

--------------------------------------------------------------------------------
-- PeerSelectionCounters
--------------------------------------------------------------------------------

{-- TODO: Before "cardano-diffusion" removal:
:
instance LogFormatting (PeerSelectionCounters (Cardano.ExtraPeerSelectionSetsWithSizes addr)) where
--}
instance LogFormatting (PeerSelectionCounters extraCounters) where
  forMachine _dtal PeerSelectionCounters {..} =
    mconcat [ "kind" .= String "PeerSelectionCounters"

            , "knownPeers" .= numberOfKnownPeers
            , "rootPeers" .= numberOfRootPeers
            , "coldPeersPromotions" .= numberOfColdPeersPromotions
            , "establishedPeers" .= numberOfEstablishedPeers
            , "warmPeersDemotions" .= numberOfWarmPeersDemotions
            , "warmPeersPromotions" .= numberOfWarmPeersPromotions
            , "activePeers" .= numberOfActivePeers
            , "activePeersDemotions" .= numberOfActivePeersDemotions

            , "knownBigLedgerPeers" .= numberOfKnownBigLedgerPeers
            , "coldBigLedgerPeersPromotions" .= numberOfColdBigLedgerPeersPromotions
            , "establishedBigLedgerPeers" .= numberOfEstablishedBigLedgerPeers
            , "warmBigLedgerPeersDemotions" .= numberOfWarmBigLedgerPeersDemotions
            , "warmBigLedgerPeersPromotions" .= numberOfWarmBigLedgerPeersPromotions
            , "activeBigLedgerPeers" .= numberOfActiveBigLedgerPeers
            , "activeBigLedgerPeersDemotions" .= numberOfActiveBigLedgerPeersDemotions

            , "knownLocalRootPeers" .= numberOfKnownLocalRootPeers
            , "establishedLocalRootPeers" .= numberOfEstablishedLocalRootPeers
            , "warmLocalRootPeersPromotions" .= numberOfWarmLocalRootPeersPromotions
            , "activeLocalRootPeers" .= numberOfActiveLocalRootPeers
            , "activeLocalRootPeersDemotions" .= numberOfActiveLocalRootPeersDemotions

            , "knownNonRootPeers" .= numberOfKnownNonRootPeers
            , "coldNonRootPeersPromotions" .= numberOfColdNonRootPeersPromotions
            , "establishedNonRootPeers" .= numberOfEstablishedNonRootPeers
            , "warmNonRootPeersDemotions" .= numberOfWarmNonRootPeersDemotions
            , "warmNonRootPeersPromotions" .= numberOfWarmNonRootPeersPromotions
            , "activeNonRootPeers" .= numberOfActiveNonRootPeers
            , "activeNonRootPeersDemotions" .= numberOfActiveNonRootPeersDemotions
{-- TODO: Before "cardano-diffusion" removal:
:
            , "knownBootstrapPeers" .= snd (Cardano.viewKnownBootstrapPeers extraCounters)
            , "coldBootstrapPeersPromotions" .= snd (Cardano.viewColdBootstrapPeersPromotions extraCounters)
            , "establishedBootstrapPeers" .= snd (Cardano.viewEstablishedBootstrapPeers extraCounters)
            , "warmBootstrapPeersDemotions" .= snd (Cardano.viewWarmBootstrapPeersDemotions extraCounters)
            , "warmBootstrapPeersPromotions" .= snd (Cardano.viewWarmBootstrapPeersPromotions extraCounters)
            , "activeBootstrapPeers" .= snd (Cardano.viewActiveBootstrapPeers extraCounters)
            , "ActiveBootstrapPeersDemotions" .= snd (Cardano.viewActiveBootstrapPeersDemotions extraCounters)
--}
            ]
  forHuman = forHumanFromMachine
  asMetrics psc =
    case psc of
      PeerSelectionCountersHWC {..} ->
        -- Deprecated metrics; they will be removed in a future version.
        [ IntM
            "peerSelection.Cold"
            (fromIntegral numberOfColdPeers)
        , IntM
            "peerSelection.Warm"
            (fromIntegral numberOfWarmPeers)
        , IntM
            "peerSelection.Hot"
            (fromIntegral numberOfHotPeers)
        , IntM
            "peerSelection.ColdBigLedgerPeers"
            (fromIntegral numberOfColdBigLedgerPeers)
        , IntM
            "peerSelection.WarmBigLedgerPeers"
            (fromIntegral numberOfWarmBigLedgerPeers)
        , IntM
            "peerSelection.HotBigLedgerPeers"
            (fromIntegral numberOfHotBigLedgerPeers)

        , IntM
            "peerSelection.WarmLocalRoots"
            (fromIntegral $ numberOfActiveLocalRootPeers psc)
        , IntM
            "peerSelection.HotLocalRoots"
            (fromIntegral $ numberOfEstablishedLocalRootPeers psc
                          - numberOfActiveLocalRootPeers psc)
        ]
    ++
    case psc of
      PeerSelectionCounters {..} ->
        [ IntM "peerSelection.RootPeers" (fromIntegral numberOfRootPeers)

        , IntM "peerSelection.KnownPeers" (fromIntegral numberOfKnownPeers)
        , IntM "peerSelection.ColdPeersPromotions" (fromIntegral numberOfColdPeersPromotions)
        , IntM "peerSelection.EstablishedPeers" (fromIntegral numberOfEstablishedPeers)
        , IntM "peerSelection.WarmPeersDemotions" (fromIntegral numberOfWarmPeersDemotions)
        , IntM "peerSelection.WarmPeersPromotions" (fromIntegral numberOfWarmPeersPromotions)
        , IntM "peerSelection.ActivePeers" (fromIntegral numberOfActivePeers)
        , IntM "peerSelection.ActivePeersDemotions" (fromIntegral numberOfActivePeersDemotions)

        , IntM "peerSelection.KnownBigLedgerPeers" (fromIntegral numberOfKnownBigLedgerPeers)
        , IntM "peerSelection.ColdBigLedgerPeersPromotions" (fromIntegral numberOfColdBigLedgerPeersPromotions)
        , IntM "peerSelection.EstablishedBigLedgerPeers" (fromIntegral numberOfEstablishedBigLedgerPeers)
        , IntM "peerSelection.WarmBigLedgerPeersDemotions" (fromIntegral numberOfWarmBigLedgerPeersDemotions)
        , IntM "peerSelection.WarmBigLedgerPeersPromotions" (fromIntegral numberOfWarmBigLedgerPeersPromotions)
        , IntM "peerSelection.ActiveBigLedgerPeers" (fromIntegral numberOfActiveBigLedgerPeers)
        , IntM "peerSelection.ActiveBigLedgerPeersDemotions" (fromIntegral numberOfActiveBigLedgerPeersDemotions)

        , IntM "peerSelection.KnownLocalRootPeers" (fromIntegral numberOfKnownLocalRootPeers)
        , IntM "peerSelection.EstablishedLocalRootPeers" (fromIntegral numberOfEstablishedLocalRootPeers)
        , IntM "peerSelection.WarmLocalRootPeersPromotions" (fromIntegral numberOfWarmLocalRootPeersPromotions)
        , IntM "peerSelection.ActiveLocalRootPeers" (fromIntegral numberOfActiveLocalRootPeers)
        , IntM "peerSelection.ActiveLocalRootPeersDemotions" (fromIntegral numberOfActiveLocalRootPeersDemotions)


        , IntM "peerSelection.KnownNonRootPeers" (fromIntegral numberOfKnownNonRootPeers)
        , IntM "peerSelection.ColdNonRootPeersPromotions" (fromIntegral numberOfColdNonRootPeersPromotions)
        , IntM "peerSelection.EstablishedNonRootPeers" (fromIntegral numberOfEstablishedNonRootPeers)
        , IntM "peerSelection.WarmNonRootPeersDemotions" (fromIntegral numberOfWarmNonRootPeersDemotions)
        , IntM "peerSelection.WarmNonRootPeersPromotions" (fromIntegral numberOfWarmNonRootPeersPromotions)
        , IntM "peerSelection.ActiveNonRootPeers" (fromIntegral numberOfActiveNonRootPeers)
        , IntM "peerSelection.ActiveNonRootPeersDemotions" (fromIntegral numberOfActiveNonRootPeersDemotions)
{-- TODO: Before "cardano-diffusion" removal:
:
        , IntM "peerSelection.KnownBootstrapPeers" (fromIntegral $ snd $ Cardano.viewKnownBootstrapPeers extraCounters)
        , IntM "peerSelection.ColdBootstrapPeersPromotions" (fromIntegral $ snd $ Cardano.viewColdBootstrapPeersPromotions extraCounters)
        , IntM "peerSelection.EstablishedBootstrapPeers" (fromIntegral $ snd $ Cardano.viewEstablishedBootstrapPeers extraCounters)
        , IntM "peerSelection.WarmBootstrapPeersDemotions" (fromIntegral $ snd $ Cardano.viewWarmBootstrapPeersDemotions extraCounters)
        , IntM "peerSelection.WarmBootstrapPeersPromotions" (fromIntegral $ snd $ Cardano.viewWarmBootstrapPeersPromotions extraCounters)
        , IntM "peerSelection.ActiveBootstrapPeers" (fromIntegral $ snd $ Cardano.viewActiveBootstrapPeers extraCounters)
        , IntM "peerSelection.ActiveBootstrapPeersDemotions" (fromIntegral $ snd $ Cardano.viewActiveBootstrapPeersDemotions extraCounters)
--}
        ]

instance MetaTrace (PeerSelectionCounters extraCounters) where
    namespaceFor PeerSelectionCounters {} = Namespace [] ["Counters"]

    severityFor (Namespace _ ["Counters"]) _ = Just Debug
    severityFor _ _ = Nothing

    documentFor (Namespace _ ["Counters"]) = Just
      "Counters of selected peers"
    documentFor _ = Nothing

    metricsDocFor (Namespace _ ["Counters"]) =
     [ ("peerSelection.Cold", "Number of cold peers")
     , ("peerSelection.Warm", "Number of warm peers")
     , ("peerSelection.Hot", "Number of hot peers")
     , ("peerSelection.ColdBigLedgerPeers", "Number of cold big ledger peers")
     , ("peerSelection.WarmBigLedgerPeers", "Number of warm big ledger peers")
     , ("peerSelection.HotBigLedgerPeers", "Number of hot big ledger peers")
     , ("peerSelection.LocalRoots", "Numbers of warm & hot local roots")

     , ("peerSelection.RootPeers", "Number of root peers")
      , ("peerSelection.KnownPeers", "Number of known peers")
      , ("peerSelection.ColdPeersPromotions", "Number of cold peers promotions")
      , ("peerSelection.EstablishedPeers", "Number of established peers")
      , ("peerSelection.WarmPeersDemotions", "Number of warm peers demotions")
      , ("peerSelection.WarmPeersPromotions", "Number of warm peers promotions")
      , ("peerSelection.ActivePeers", "Number of active peers")
      , ("peerSelection.ActivePeersDemotions", "Number of active peers demotions")

      , ("peerSelection.KnownBigLedgerPeers", "Number of known big ledger peers")
      , ("peerSelection.ColdBigLedgerPeersPromotions", "Number of cold big ledger peers promotions")
      , ("peerSelection.EstablishedBigLedgerPeers", "Number of established big ledger peers")
      , ("peerSelection.WarmBigLedgerPeersDemotions", "Number of warm big ledger peers demotions")
      , ("peerSelection.WarmBigLedgerPeersPromotions", "Number of warm big ledger peers promotions")
      , ("peerSelection.ActiveBigLedgerPeers", "Number of active big ledger peers")
      , ("peerSelection.ActiveBigLedgerPeersDemotions", "Number of active big ledger peers demotions")

      , ("peerSelection.KnownLocalRootPeers", "Number of known local root peers")
      , ("peerSelection.EstablishedLocalRootPeers", "Number of established local root peers")
      , ("peerSelection.WarmLocalRootPeersPromotions", "Number of warm local root peers promotions")
      , ("peerSelection.ActiveLocalRootPeers", "Number of active local root peers")
      , ("peerSelection.ActiveLocalRootPeersDemotions", "Number of active local root peers demotions")

      , ("peerSelection.KnownNonRootPeers", "Number of known non root peers")
      , ("peerSelection.ColdNonRootPeersPromotions", "Number of cold non root peers promotions")
      , ("peerSelection.EstablishedNonRootPeers", "Number of established non root peers")
      , ("peerSelection.WarmNonRootPeersDemotions", "Number of warm non root peers demotions")
      , ("peerSelection.WarmNonRootPeersPromotions", "Number of warm non root peers promotions")
      , ("peerSelection.ActiveNonRootPeers", "Number of active non root peers")
      , ("peerSelection.ActiveNonRootPeersDemotions", "Number of active non root peers demotions")

      , ("peerSelection.KnownBootstrapPeers", "Number of known bootstrap peers")
      , ("peerSelection.ColdBootstrapPeersPromotions", "Number of cold bootstrap peers promotions")
      , ("peerSelection.EstablishedBootstrapPeers", "Number of established bootstrap peers")
      , ("peerSelection.WarmBootstrapPeersDemotions", "Number of warm bootstrap peers demotions")
      , ("peerSelection.WarmBootstrapPeersPromotions", "Number of warm bootstrap peers promotions")
      , ("peerSelection.ActiveBootstrapPeers", "Number of active bootstrap peers")
      , ("peerSelection.ActiveBootstrapPeersDemotions", "Number of active bootstrap peers demotions")

     ]
    metricsDocFor _ = []

    allNamespaces =[
      Namespace [] ["Counters"]
      ]


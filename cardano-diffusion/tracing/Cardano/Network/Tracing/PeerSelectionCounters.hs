{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE RecordWildCards   #-}

-- Orphan instances module for Cardano tracer.
{-# OPTIONS_GHC -Wno-orphans #-}
module Cardano.Network.Tracing.PeerSelectionCounters () where

import "aeson" Data.Aeson
import "trace-dispatcher" Cardano.Logging
import "text" Data.Text qualified as Text

import Cardano.Network.PeerSelection.ExtraRootPeers
--------------------------------------------------------------------------------
-- PeerSelectionCounters
--------------------------------------------------------------------------------

instance Show peeraddr => LogFormatting (ViewExtraPeers (ExtraPeers peeraddr)) where
  forMachine _dtal ExtraPeerSelectionSetsWithSizes {..} =
    mconcat [ "knownBootstrapPeers" .= snd viewKnownBootstrapPeers
            , "coldBootstrapPeersPromotions" .= snd viewColdBootstrapPeersPromotions
            , "establishedBootstrapPeers" .= snd viewEstablishedBootstrapPeers
            , "warmBootstrapPeersDemotions" .= snd viewWarmBootstrapPeersDemotions
            , "warmBootstrapPeersPromotions" .= snd viewWarmBootstrapPeersPromotions
            , "activeBootstrapPeers" .= snd viewActiveBootstrapPeers
            , "ActiveBootstrapPeersDemotions" .= snd viewActiveBootstrapPeersDemotions
            ]

  forHuman = Text.pack . show

  asMetrics ExtraPeerSelectionSetsWithSizes {..} =
    [ IntM "peerSelection.KnownBootstrapPeers" (fromIntegral . snd $ viewKnownBootstrapPeers)
    , IntM "peerSelection.ColdBootstrapPeersPromotions" (fromIntegral . snd $ viewColdBootstrapPeersPromotions)
    , IntM "peerSelection.EstablishedBootstrapPeers" (fromIntegral . snd $ viewEstablishedBootstrapPeers)
    , IntM "peerSelection.WarmBootstrapPeersDemotions" (fromIntegral . snd $ viewWarmBootstrapPeersDemotions)
    , IntM "peerSelection.WarmBootstrapPeersPromotions" (fromIntegral . snd $ viewWarmBootstrapPeersPromotions)
    , IntM "peerSelection.ActiveBootstrapPeers" (fromIntegral . snd $ viewActiveBootstrapPeers)
    , IntM "peerSelection.ActiveBootstrapPeersDemotions" (fromIntegral . snd $ viewActiveBootstrapPeersDemotions)
    ]

instance MetaTrace (ViewExtraPeers (ExtraPeers peeraddr)) where
    namespaceFor ExtraPeerSelectionSetsWithSizes {}  = Namespace [] ["Counters"]

    severityFor (Namespace [] ["Counters"]) _ = Just Debug
    severityFor _ _ = Nothing

    documentFor (Namespace [] ["Counters"]) =
      Just "Cardano-specific extensions to peer types"
    documentFor _ = Nothing

    metricsDocFor _ =
     [ ("peerSelection.KnownBootstrapPeers", "Number of known bootstrap peers")
     , ("peerSelection.ColdBootstrapPeersPromotions", "Number of cold bootstrap peers promotions")
     , ("peerSelection.EstablishedBootstrapPeers", "Number of established bootstrap peers")
     , ("peerSelection.WarmBootstrapPeersDemotions", "Number of warm bootstrap peers demotions")
     , ("peerSelection.WarmBootstrapPeersPromotions", "Number of warm bootstrap peers promotions")
     , ("peerSelection.ActiveBootstrapPeers", "Number of active bootstrap peers")
     , ("peerSelection.ActiveBootstrapPeersDemotions", "Number of active bootstrap peers demotions")
     ]

    allNamespaces = [Namespace [] ["Counters"]]

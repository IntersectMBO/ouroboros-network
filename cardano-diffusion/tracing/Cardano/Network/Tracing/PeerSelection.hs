{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Cardano.Network.Tracing.PeerSelection () where

import Data.Aeson
import Data.Text qualified as Text

import Cardano.Logging
import Cardano.Network.LedgerPeerConsensusInterface
import Cardano.Network.PeerSelection.ExtraRootPeers
import Cardano.Network.Tracing.Churn ()

instance LogFormatting (ToExtraTrace (ExtraPeers peeraddr)) where
  forMachine _dtal (TraceLedgerStateJudgementChanged new) =
    mconcat [ "kind" .= String "LedgerStateJudgementChanged"
            , "new"  .= Text.pack (show new) ]

  forMachine _dtal (TraceUseBootstrapPeersChanged ubp) =
    mconcat [ "kind" .= String "UseBootstrapPeersChanged"
            , "useBootstrapPeers" .= toJSON ubp ]

  forHuman = Text.pack . show


instance MetaTrace (ToExtraTrace (ExtraPeers peeraddr)) where
  namespaceFor TraceLedgerStateJudgementChanged {} =
    Namespace [] ["LedgerStateJudgementChanged"]

  namespaceFor TraceUseBootstrapPeersChanged {} =
    Namespace [] ["UseBootstrapPeersChanged"]

  severityFor (Namespace [] ["LedgerStateJudgementChanged"])
              (Just (TraceLedgerStateJudgementChanged lsj))
                | TooOld <- lsj      = Just Warning
                | YoungEnough <- lsj = Just Notice
  severityFor (Namespace [] ["UseBootstrapPeersChanged"]) _ =
    Just Notice
  severityFor _ _ = Nothing

  documentFor (Namespace [] ["LedgerStateJudgementChanged"]) =
    Just "Indicates whether the node is caught up or needs to sync"
  documentFor (Namespace [] ["UseBootstrapPeersChanged"]) =
    Just "Reacts to configuration changes related to bootstrap peers in the topology file"
  documentFor _ = Nothing

  allNamespaces = [
    Namespace [] ["LedgerStateJudgementChanged"],
    Namespace [] ["UseBootstrapPeersChanged"]
    ]

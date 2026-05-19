{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Network.Tracing.TxSubmission.Inbound () where

import Data.Aeson
import Data.Text qualified as Text

import Cardano.Logging
import Ouroboros.Network.TxSubmission.Inbound.V2.Types

--------------------------------------------------------------------------------
-- TxInbound Tracer
--------------------------------------------------------------------------------

instance (Show txid, Show tx)
      => LogFormatting (TraceTxSubmissionInbound txid tx) where
  forMachine dtal (TraceTxSubmissionCollected txids) =
    mconcat [
        "kind" .= String "TraceTxSubmissionCollected"
      , "count" .= toJSON (length txids)
      ]
    <> case dtal of
         DDetailed  -> "txIds" .= Text.pack (show txids)
         _otherwise -> mempty

  forMachine _dtal (TraceTxSubmissionProcessed processed) =
    mconcat
      [ "kind" .= String "TraceTxSubmissionProcessed"
      , "accepted" .= toJSON (ptxcAccepted processed)
      , "rejected" .= toJSON (ptxcRejected processed)
      ]

  forMachine _dtal TraceTxInboundTerminated =
    mconcat
      [ "kind" .= String "TraceTxInboundTerminated"
      ]

  forMachine _dtal (TraceTxInboundCanRequestMoreTxs count) =
    mconcat
      [ "kind" .= String "TraceTxInboundCanRequestMoreTxs"
      , "count" .= toJSON count
      ]

  forMachine _dtal (TraceTxInboundCannotRequestMoreTxs count) =
    mconcat
      [ "kind" .= String "TraceTxInboundCannotRequestMoreTxs"
      , "count" .= toJSON count
      ]

  forMachine dtal (TraceTxInboundAddedToMempool txids dt) =
    mconcat
      [ "kind" .= String "TraceTxInboundAddedToMempool"
      , "count" .= toJSON (length txids)
      , "dt" .= toJSON dt]
    <> case dtal of
         DDetailed  -> "txIds" .= Text.pack (show txids)
         _otherwise -> mempty

  forMachine dtal (TraceTxInboundRejectedFromMempool txids dt) =
    mconcat
      [ "kind" .= String "TraceTxInboundRejectedFromMempool"
      , "count" .= toJSON (length txids)
      , "dt" .= toJSON dt]
    <> case dtal of
         DDetailed  -> "txIds" .= Text.pack (show txids)
         _otherwise -> mempty

  forMachine _dtal (TraceTxInboundError e) =
    mconcat [
        "kind" .= String "TraceTxInboundError"
      , "error" .= Text.pack (show e)
      ]

  forMachine _dtal (TraceTxInboundDecision decision) =
    mconcat [
        "kind" .= String "TraceTxInboundDecision"
      , "decision" .= Text.pack (show decision)
      ]

  asMetrics (TraceTxSubmissionCollected txids) =
    [CounterM "submissions.submitted" (Just (length txids))]
  asMetrics (TraceTxSubmissionProcessed processed) =
    [ CounterM "submissions.accepted"
        (Just (ptxcAccepted processed))
    , CounterM "submissions.rejected"
        (Just (ptxcRejected processed))
    ]
  asMetrics _ = []

instance MetaTrace (TraceTxSubmissionInbound txid tx) where
    namespaceFor TraceTxSubmissionCollected {} =
      Namespace [] ["Collected"]
    namespaceFor TraceTxSubmissionProcessed {} =
      Namespace [] ["Processed"]
    namespaceFor TraceTxInboundTerminated {} =
      Namespace [] ["Terminated"]
    namespaceFor TraceTxInboundCanRequestMoreTxs {} =
      Namespace [] ["CanRequestMoreTxs"]
    namespaceFor TraceTxInboundCannotRequestMoreTxs {} =
      Namespace [] ["CannotRequestMoreTxs"]
    namespaceFor TraceTxInboundAddedToMempool {} =
      Namespace [] ["AddedToMempool"]
    namespaceFor TraceTxInboundRejectedFromMempool {} =
      Namespace [] ["RejectedFromMempool"]
    namespaceFor TraceTxInboundError {} =
      Namespace [] ["Error"]
    namespaceFor TraceTxInboundDecision {} =
      Namespace [] ["Decision"]

    severityFor (Namespace _ ["Collected"]) _            = Just Debug
    severityFor (Namespace _ ["Processed"]) _            = Just Debug
    severityFor (Namespace _ ["Terminated"]) _           = Just Notice
    severityFor (Namespace _ ["CanRequestMoreTxs"]) _    = Just Debug
    severityFor (Namespace _ ["CannotRequestMoreTxs"]) _ = Just Debug
    severityFor (Namespace _ ["AddedToMempool"]) _       = Just Debug
    severityFor (Namespace _ ["RejectedFromMempool"]) _  = Just Debug
    severityFor (Namespace _ ["Error"]) _                = Just Debug
    severityFor (Namespace _ ["Decision"]) _             = Just Debug

    severityFor _ _                                      = Nothing

    metricsDocFor (Namespace _ ["Collected"]) =
      [ ("submissions.submitted", "")]
    metricsDocFor (Namespace _ ["Processed"]) =
      [ ("submissions.accepted", "")
      , ("submissions.rejected", "")
      ]
    metricsDocFor _ = []

    documentFor (Namespace _ ["Collected"]) = Just
      "Number of transactions just about to be inserted."
    documentFor (Namespace _ ["Processed"]) = Just
      "Just processed transaction pass/fail breakdown."
    documentFor (Namespace _ ["Terminated"]) = Just
      "Server received 'MsgDone'."
    documentFor (Namespace _ ["CanRequestMoreTxs"]) = Just $ mconcat
      [ "There are no replies in flight, but we do know some more txs we"
      , " can ask for, so lets ask for them and more txids."
      ]
    documentFor (Namespace _ ["CannotRequestMoreTxs"]) = Just $ mconcat
      [ "There's no replies in flight, and we have no more txs we can"
      , " ask for so the only remaining thing to do is to ask for more"
      , " txids. Since this is the only thing to do now, we make this a"
      , " blocking call."
      ]
    documentFor (Namespace _ ["AddedToMempool"]) = Just
      "Transactions added to the mempool and processing time"
    documentFor (Namespace _ ["RejectedFromMempool"]) = Just
      "Transactions rejected from mempool and processing time"
    documentFor (Namespace _ ["Error"]) = Just
      "Protocol violation causing connection reset"
    documentFor (Namespace _ ["Decision"]) = Just
      "Decision to advance the protocol"
    documentFor _ = Nothing

    allNamespaces = [
          Namespace [] ["Collected"]
        , Namespace [] ["Processed"]
        , Namespace [] ["Terminated"]
        , Namespace [] ["CanRequestMoreTxs"]
        , Namespace [] ["CannotRequestMoreTxs"]
        , Namespace [] ["AddedToMempool"]
        , Namespace [] ["RejectedFromMempool"]
        , Namespace [] ["Error"]
        , Namespace [] ["Decision"]
        ]

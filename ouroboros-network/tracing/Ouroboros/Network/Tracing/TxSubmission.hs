{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Network.Tracing.TxSubmission () where

import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text

import Cardano.Logging
import Ouroboros.Network.Tx (HasRawTxId)
import Ouroboros.Network.TxSubmission.Inbound.V2.Types

instance (Show txid, Show peeraddr, HasRawTxId txid) => LogFormatting (TraceTxLogic peeraddr txid tx) where
  forMachine dtal (TraceSharedTxState SharedTxState {..} counts) =
      mconcat $ [ "kind" .= String "TraceSharedTxState"
                , "sharedGeneration" .= sharedGeneration
                , "sharedRevision" .= sharedRevision
                , "sharedClaimable" .= sharedClaimable
                ]
             ++ [ Key.fromString name .= n
                | (name, n) <- sharedTxStateCountFields counts
                ]
             ++ more
    where
      renderTxId txKey =
        maybe "<missing-txid>" show (IntMap.lookup txKey sharedKeyToTxId)

      more = case dtal of
        DMaximum ->
                  [ "sharedTxTable" .= [ (renderTxId txKey, show txEntry)
                                       | (txKey, txEntry) <- IntMap.toList sharedTxTable
                                       ]
                  , "sharedRetainedTxs" .= [ (renderTxId txKey, retainTimeSeconds retainUntil)
                                           | (txKey, retainUntil) <- retainedToList sharedRetainedTxs
                                           ]
                  , "internedTxIds" .= fmap show (Map.keys sharedTxIdToKey)
                  ]
        _otherwise -> []

  forMachine _dtal (TraceTxTiers census) =
    mconcat $ ("kind" .= String "TraceTxTiers")
            : [ Key.fromString name .= n | (name, n) <- tierCensusFields census ]

  -- The counts as gauges, under the trace's cadence, whatever the trace's
  -- severity: the write counter (revision), the wake counter (generation),
  -- the slow tier's wake counter (claimable) and the sizes.
  asMetrics (TraceSharedTxState SharedTxState {..} counts) =
       IntM (sharedStateMetric "revision") (fromIntegral sharedRevision)
     : IntM (sharedStateMetric "generation") (fromIntegral sharedGeneration)
     : IntM (sharedStateMetric "claimable") (fromIntegral sharedClaimable)
     : [ IntM (sharedStateMetric name) (fromIntegral n)
       | (name, n) <- sharedTxStateCountFields counts
       ]
  asMetrics (TraceTxTiers census) =
    [ IntM (tiersMetric name) (fromIntegral n) | (name, n) <- tierCensusFields census ]

-- | What each tier census count is documented as, by its field's name in
-- 'tierCensusFields'.
tierCensusDocs :: [(String, Text.Text)]
tierCensusDocs =
  [ ("fastByRoot",   "peers polled at full speed by standing: local roots")
  , ("fastByCoin",   "peers polled at full speed by the coin's salted order")
  , ("fastBySource", "peers polled at full speed as tx sources while the coin left them slow")
  , ("slow",         "peers polled on the slow tier's cadence")
  ]

-- | The metric name of a tier census count.
tiersMetric :: String -> Text.Text
tiersMetric name = "txSubmission.tiers." <> Text.pack name

-- | The metric name of a shared-state count, by its field's name.
sharedStateMetric :: String -> Text.Text
sharedStateMetric name = "txSubmission.sharedState." <> Text.pack name

instance MetaTrace (TraceTxLogic peeraddr txid tx) where
  namespaceFor TraceSharedTxState {} =
    Namespace [] ["TraceSharedTxState"]
  namespaceFor TraceTxTiers {} =
    Namespace [] ["TraceTxTiers"]

  severityFor _ _ = Just Debug

  documentFor (Namespace [] ["TraceSharedTxState"]) =
    Just "Internal bookkeeping of tx-submission shared state for peer coordination"
  documentFor (Namespace [] ["TraceTxTiers"]) =
    Just "The inbound peers by the fetch tier standing they are served on"
  documentFor _ = Nothing

  metricsDocFor (Namespace [] ["TraceSharedTxState"]) =
    [ (sharedStateMetric "revision", "writes of the shared tx state, one per transaction")
    , (sharedStateMetric "generation", "wake-ups of the parked peers")
    , (sharedStateMetric "claimable", "wake-ups of the slow tier's parked peers: an entry became claimable, a lease was released or a stuck cap raised")
    , (sharedStateMetric "activeTxCount", "entries of the active tx table")
    , (sharedStateMetric "retainedTxCount", "keys retained after reaching the mempool")
    , (sharedStateMetric "internedTxCount", "lookup entries in all")
    , (sharedStateMetric "leasedTxCount", "active entries leased for download")
    , (sharedStateMetric "claimableTxCount", "active entries nobody is downloading")
    , (sharedStateMetric "totalAttemptCount", "download attempts over the active entries")
    , (sharedStateMetric "submittingTxCount", "active entries being added to the mempool")
    , (sharedStateMetric "doomedTxCount", "active entries under review for abandonment, dropped at the next review unless a peer still advertises them")
    ]
  metricsDocFor (Namespace [] ["TraceTxTiers"]) =
    [ (tiersMetric name, doc) | (name, doc) <- tierCensusDocs ]
  metricsDocFor _ = []

  allNamespaces = [
    Namespace [] ["TraceSharedTxState"]
    , Namespace [] ["TraceTxTiers"]
    ]

instance LogFormatting TxSubmissionCounters where
  forMachine _dtal TxSubmissionCounters {..} =
    mconcat [ "kind" .= String "TxSubmissionCounters"
            , "txIdMessagesSent" .= txIdMessagesSent
            , "txIdsRequested" .= txIdsRequested
            , "txIdRepliesReceived" .= txIdRepliesReceived
            , "txIdsReceived" .= txIdsReceived
            , "txMessagesSent" .= txMessagesSent
            , "txsRequested" .= txsRequested
            , "txRepliesReceived" .= txRepliesReceived
            , "txsReceived" .= txsReceived
            , "txsOmitted" .= txsOmitted
            , "lateBodies" .= lateBodies
            , "txsAccepted" .= txsAccepted
            , "txsRejected" .= txsRejected
            , "txIdBlockingReqsSent" .= txIdBlockingReqsSent
            , "txIdPipelinedReqsSent" .= txIdPipelinedReqsSent
            , "txIdSlowReqsSent" .= txIdSlowReqsSent
            , "txIdsReceivedSlow" .= txIdsReceivedSlow
            , "txIdReplies0" .= txIdReplies0
            , "txIdReplies1" .= txIdReplies1
            , "txIdReplies2to4" .= txIdReplies2to4
            , "txIdReplies5to9" .= txIdReplies5to9
            , "txIdReplies10" .= txIdReplies10
            , "txIdBlockingWaitMs" .= txIdBlockingWait
            , "txPipelineWaitMs" .= txPipelineWait
            , "txSubmissionWaitMs" .= txSubmissionWait
            ]

  asMetrics TxSubmissionCounters {..} =
    [ IntM "txSubmission.txIdMessagesSent" (fromIntegral txIdMessagesSent)
    , IntM "txSubmission.txIdsRequested" (fromIntegral txIdsRequested)
    , IntM "txSubmission.txIdRepliesReceived" (fromIntegral txIdRepliesReceived)
    , IntM "txSubmission.txIdsReceived" (fromIntegral txIdsReceived)
    , IntM "txSubmission.txMessagesSent" (fromIntegral txMessagesSent)
    , IntM "txSubmission.txsRequested" (fromIntegral txsRequested)
    , IntM "txSubmission.txRepliesReceived" (fromIntegral txRepliesReceived)
    , IntM "txSubmission.txsReceived" (fromIntegral txsReceived)
    , IntM "txSubmission.txsOmitted" (fromIntegral txsOmitted)
    , IntM "txSubmission.lateBodies" (fromIntegral lateBodies)
    , IntM "txSubmission.txsAccepted" (fromIntegral txsAccepted)
    , IntM "txSubmission.txsRejected" (fromIntegral txsRejected)
    , IntM "txSubmission.txIdBlockingReqsSent"  (fromIntegral txIdBlockingReqsSent)
    , IntM "txSubmission.txIdPipelinedReqsSent" (fromIntegral txIdPipelinedReqsSent)
    , IntM "txSubmission.txIdSlowReqsSent"      (fromIntegral txIdSlowReqsSent)
    , IntM "txSubmission.txIdsReceivedSlow"     (fromIntegral txIdsReceivedSlow)
    , IntM "txSubmission.txIdReplies.0"         (fromIntegral txIdReplies0)
    , IntM "txSubmission.txIdReplies.1"         (fromIntegral txIdReplies1)
    , IntM "txSubmission.txIdReplies.2to4"      (fromIntegral txIdReplies2to4)
    , IntM "txSubmission.txIdReplies.5to9"      (fromIntegral txIdReplies5to9)
    , IntM "txSubmission.txIdReplies.10"        (fromIntegral txIdReplies10)
    , IntM "txSubmission.txIdBlockingWaitMs" (fromIntegral txIdBlockingWait)
    , IntM "txSubmission.txPipelineWaitMs"   (fromIntegral txPipelineWait)
    , IntM "txSubmission.txSubmissionWaitMs" (fromIntegral txSubmissionWait)
    ]

instance MetaTrace TxSubmissionCounters where
  namespaceFor TxSubmissionCounters {} = Namespace [] ["Counters"]
  severityFor _ _ = Just Debug

  documentFor (Namespace [] ["Counters"]) = Just "Counters for TxSubmission v2"
  documentFor _                           = Nothing

  metricsDocFor (Namespace [] ["Counters"]) =
    [ ("txSubmission.txIdMessagesSent", "number of txid request messages sent")
    , ("txSubmission.txIdsRequested", "number of txids requested from remote peers")
    , ("txSubmission.txIdRepliesReceived", "number of txid reply messages received")
    , ("txSubmission.txIdsReceived", "number of txids received in reply batches")
    , ("txSubmission.txMessagesSent", "number of tx body request messages sent")
    , ("txSubmission.txsRequested", "number of tx bodies requested from remote peers")
    , ("txSubmission.txRepliesReceived", "number of tx body reply messages received")
    , ("txSubmission.txsReceived", "number of tx bodies received")
    , ("txSubmission.txsOmitted", "number of requested tx bodies omitted from replies")
    , ("txSubmission.lateBodies", "number of tx bodies received after local resolution")
    , ("txSubmission.txsAccepted", "number of tx bodies resolved into the mempool")
    , ("txSubmission.txsRejected", "number of tx bodies rejected by the mempool")
    , ("txSubmission.txIdBlockingReqsSent",  "number of blocking txid request messages sent")
    , ("txSubmission.txIdPipelinedReqsSent", "number of pipelined txid request messages sent")
    , ("txSubmission.txIdSlowReqsSent",      "number of txid request messages sent to peers on the slow fetch tier")
    , ("txSubmission.txIdsReceivedSlow",     "number of txids received in replies to requests sent on the slow fetch tier")
    , ("txSubmission.txIdReplies.0",         "number of txid replies carrying no txid")
    , ("txSubmission.txIdReplies.1",         "number of txid replies carrying one txid")
    , ("txSubmission.txIdReplies.2to4",      "number of txid replies carrying two to four txids")
    , ("txSubmission.txIdReplies.5to9",      "number of txid replies carrying five to nine txids")
    , ("txSubmission.txIdReplies.10",        "number of txid replies carrying ten or more txids")
    , ("txSubmission.txIdBlockingWaitMs", "cumulative milliseconds spent waiting for blocking txid replies (idle state proxy)")
    , ("txSubmission.txPipelineWaitMs",   "cumulative milliseconds the pipeline was active from first body request until full drain (loading state proxy)")
    , ("txSubmission.txSubmissionWaitMs", "cumulative milliseconds spent in mempoolAddTxs; high values indicate mempool backpressure")
    ]
  metricsDocFor _ = []

  allNamespaces = [
    Namespace [] ["Counters"]
    ]

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
  forMachine dtal (TraceSharedTxState st@SharedTxState {..}) =
      mconcat $ [ "kind" .= String "TraceSharedTxState"
                , "sharedGeneration" .= sharedGeneration
                , "sharedRevision" .= sharedRevision
                , "sharedClaimable" .= sharedClaimable
                ]
             ++ [ Key.fromString name .= n
                | (name, n) <- sharedTxStateCountFields (sharedTxStateCounts st)
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
                  , "sharedRetainedTxs" .= [ (renderTxId txKey, fromIntegral retainUntil / 1e9 :: Double)
                                           | (txKey, retainUntil) <- retainedToList sharedRetainedTxs
                                           ]
                  , "internedTxIds" .= fmap show (Map.keys sharedTxIdToKey)
                  ]
        _otherwise -> []

  -- The counts as gauges, under the trace's cadence, whatever the trace's
  -- severity: the write counter (revision), the wake counter (generation)
  -- and the sizes.
  forMachine _dtal (TraceTxTiers TierCensus {..}) =
    mconcat [ "kind" .= String "TraceTxTiers"
            , "fastByRoot" .= censusFastByRoot
            , "fastByCoin" .= censusFastByCoin
            , "fastBySource" .= censusFastBySource
            , "slow" .= censusSlow
            ]

  asMetrics (TraceSharedTxState st@SharedTxState {..}) =
       IntM (sharedStateMetric "revision") (fromIntegral sharedRevision)
     : IntM (sharedStateMetric "generation") (fromIntegral sharedGeneration)
     : IntM (sharedStateMetric "claimable") (fromIntegral sharedClaimable)
     : [ IntM (sharedStateMetric name) (fromIntegral n)
       | (name, n) <- sharedTxStateCountFields (sharedTxStateCounts st)
       ]
  asMetrics (TraceTxTiers TierCensus {..}) =
    [ IntM (tiersMetric "fastByRoot")   (fromIntegral censusFastByRoot)
    , IntM (tiersMetric "fastByCoin")   (fromIntegral censusFastByCoin)
    , IntM (tiersMetric "fastBySource") (fromIntegral censusFastBySource)
    , IntM (tiersMetric "slow")         (fromIntegral censusSlow)
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
    , (sharedStateMetric "activeTxCount", "entries of the active tx table")
    , (sharedStateMetric "retainedTxCount", "keys retained after reaching the mempool")
    , (sharedStateMetric "internedTxCount", "lookup entries in all")
    , (sharedStateMetric "leasedTxCount", "active entries leased for download")
    , (sharedStateMetric "claimableTxCount", "active entries nobody is downloading")
    , (sharedStateMetric "totalAttemptCount", "download attempts over the active entries")
    , (sharedStateMetric "submittingTxCount", "active entries being added to the mempool")
    , (sharedStateMetric "advertiserCount", "advertisers over the active entries, one per peer and key")
    ]
  metricsDocFor (Namespace [] ["TraceTxTiers"]) =
    [ (tiersMetric "fastByRoot",   "peers polled at full speed by standing: local roots")
    , (tiersMetric "fastByCoin",   "peers polled at full speed by the coin's draw")
    , (tiersMetric "fastBySource", "peers polled at full speed as tx sources while the coin left them slow")
    , (tiersMetric "slow",         "peers polled on the slow tier's cadence")
    ]
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
    , ("txSubmission.txIdBlockingWaitMs", "cumulative milliseconds spent waiting for blocking txid replies (idle state proxy)")
    , ("txSubmission.txPipelineWaitMs",   "cumulative milliseconds the pipeline was active from first body request until full drain (loading state proxy)")
    , ("txSubmission.txSubmissionWaitMs", "cumulative milliseconds spent in mempoolAddTxs; high values indicate mempool backpressure")
    ]
  metricsDocFor _ = []

  allNamespaces = [
    Namespace [] ["Counters"]
    ]

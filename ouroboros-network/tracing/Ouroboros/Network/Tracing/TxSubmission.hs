{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Network.Tracing.TxSubmission () where

import Data.Aeson
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map

import Cardano.Logging
import Ouroboros.Network.Tx (HasRawTxId)
import Ouroboros.Network.TxSubmission.Inbound.V2.Types

instance (Show txid, Show peeraddr, HasRawTxId txid) => LogFormatting (TraceTxLogic peeraddr txid tx) where
  forMachine dtal (TraceSharedTxState SharedTxState {..}) =
      mconcat $ [ "kind" .= String "TraceSharedTxState"
                , "sharedGeneration" .= sharedGeneration
                , "activeTxCount" .= IntMap.size sharedTxTable
                , "retainedTxCount" .= retainedSize sharedRetainedTxs
                , "internedTxCount" .= Map.size sharedTxIdToKey
                , "leasedTxCount" .= leasedTxCount
                , "claimableTxCount" .= claimableTxCount
                , "totalAttemptCount" .= totalAttemptCount
                , "submittingTxCount" .= submittingTxCount
                ] ++ more
    where
      activeEntries = IntMap.elems sharedTxTable

      leasedTxCount =
        length [ () | TxEntry { txLease = TxLeased _ _ } <- activeEntries ]

      claimableTxCount =
        length [ () | TxEntry { txLease = TxClaimable _ } <- activeEntries ]

      totalAttemptCount =
        sum [ txAttempt entry | entry <- activeEntries ]

      submittingTxCount =
        length [ () | TxEntry { txInSubmission = True } <- activeEntries ]

      renderTxId txKey =
        maybe "<missing-txid>" show (IntMap.lookup txKey sharedKeyToTxId)

      more = case dtal of
        DMaximum ->
                  [ "sharedTxTable" .= [ (renderTxId txKey, show txEntry)
                                       | (txKey, txEntry) <- IntMap.toList sharedTxTable
                                       ]
                  , "sharedRetainedTxs" .= [ (renderTxId txKey, show retainUntil)
                                           | (txKey, retainUntil) <- retainedToList sharedRetainedTxs
                                           ]
                  , "internedTxIds" .= fmap show (Map.keys sharedTxIdToKey)
                  ]
        _otherwise -> []

  forMachine _dtal (TraceTxLogicRtt txIdRtt txBodyRtt) =
      mconcat [ "kind"      .= String "TraceTxLogicRtt"
              , "txIdRtt"   .= rttObject txIdRtt
              , "txBodyRtt" .= rttObject txBodyRtt
              ]
    where
      rttObject RttStats {..} =
        object [ "count"  .= rttCount
               , "p50Ms"  .= rttP50Ms
               , "p90Ms"  .= rttP90Ms
               , "p95Ms"  .= rttP95Ms
               , "p99Ms"  .= rttP99Ms
               , "meanMs" .= rttMeanMs
               ]


instance MetaTrace (TraceTxLogic peeraddr txid tx) where
  namespaceFor TraceSharedTxState {} =
    Namespace [] ["TraceSharedTxState"]
  namespaceFor TraceTxLogicRtt {} =
    Namespace [] ["TraceTxLogicRtt"]

  severityFor _ _ = Just Debug

  documentFor (Namespace [] ["TraceSharedTxState"]) =
    Just "Internal bookkeeping of tx-submission shared state for peer coordination"
  documentFor (Namespace [] ["TraceTxLogicRtt"]) =
    Just "Global request-to-reply round-trip-time summary over a sliding window"
  documentFor _ = Nothing

  allNamespaces = [
      Namespace [] ["TraceSharedTxState"]
    , Namespace [] ["TraceTxLogicRtt"]
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
            , "txIdBlockingWaitMs" .= txIdBlockingWaitMs
            , "txPipelineWaitMs" .= txPipelineWaitMs
            , "txSubmissionWaitMs" .= txSubmissionWaitMs
            , "reclaims" .= reclaims
            , "capBumps" .= capBumps
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
    , IntM "txSubmission.txIdBlockingWaitMs" (fromIntegral txIdBlockingWaitMs)
    , IntM "txSubmission.txPipelineWaitMs"   (fromIntegral txPipelineWaitMs)
    , IntM "txSubmission.txSubmissionWaitMs" (fromIntegral txSubmissionWaitMs)
    , IntM "txSubmission.reclaims" (fromIntegral reclaims)
    , IntM "txSubmission.capBumps" (fromIntegral capBumps)
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
    , ("txSubmission.txIdBlockingWaitMs", "cumulative milliseconds spent waiting for blocking txid replies (idle state proxy)")
    , ("txSubmission.txPipelineWaitMs",   "cumulative milliseconds the pipeline was active from first body request until full drain (loading state proxy)")
    , ("txSubmission.txSubmissionWaitMs", "cumulative milliseconds spent in mempoolAddTxs; high values indicate mempool backpressure")
    , ("txSubmission.reclaims", "tx-body leases stolen from another peer after lease expiry (serial retry)")
    , ("txSubmission.capBumps", "inflight-multiplicity cap bumps (parallel retry for a stuck leaseholder)")
    ]
  metricsDocFor _ = []

  allNamespaces = [
    Namespace [] ["Counters"]
    ]

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Network.Tracing.TxSubmission () where

import Data.Aeson
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map

import Cardano.Logging
import Ouroboros.Network.TxSubmission.Inbound.V2.Types
import Ouroboros.Network.Tx

instance (Show txid, HasRawTxId txid, Show peeraddr) => LogFormatting (TraceTxLogic peeraddr txid tx) where
  forMachine dtal (TraceSharedTxState label SharedTxState {..}) =
      mconcat $ [ "kind" .= String "TraceSharedTxState"
                , "label" .= label
                , "sharedGeneration" .= sharedGeneration
                , "peerCount" .= Map.size sharedPeers
                , "activeTxCount" .= IntMap.size sharedTxTable
                , "retainedTxCount" .= retainedSize sharedRetainedTxs
                , "internedTxCount" .= Map.size sharedTxIdToKey
                , "leasedTxCount" .= leasedTxCount
                , "claimableTxCount" .= claimableTxCount
                , "resolvedTxCount" .= resolvedTxCount
                , "downloadingAttemptCount" .= downloadingAttemptCount
                , "bufferedAttemptCount" .= bufferedAttemptCount
                , "submittingAttemptCount" .= submittingAttemptCount
                , "peerPhases" .= peerPhases
                ] ++ more
    where
      activeEntries = IntMap.elems sharedTxTable

      leasedTxCount =
        length [ () | TxEntry { txLease = TxLeased _ _ } <- activeEntries ]

      claimableTxCount =
        length [ () | TxEntry { txLease = TxClaimable _ } <- activeEntries ]

      resolvedTxCount = 0 :: Int

      downloadingAttemptCount =
        sum [ length [ () | TxDownloading <- Map.elems txAttempts' ]
            | TxEntry { txAttempts = txAttempts' } <- activeEntries
            ]

      bufferedAttemptCount =
        sum [ length [ () | TxBuffered <- Map.elems txAttempts' ]
            | TxEntry { txAttempts = txAttempts' } <- activeEntries
            ]

      submittingAttemptCount =
        sum [ length [ () | TxSubmitting <- Map.elems txAttempts' ]
            | TxEntry { txAttempts = txAttempts' } <- activeEntries
            ]

      peerPhases =
        Map.toList $
          Map.fromListWith (+)
            [ (show sharedPeerPhase', 1 :: Int)
            | SharedPeerState { sharedPeerPhase = sharedPeerPhase' } <- Map.elems sharedPeers
            ]

      renderTxId txKey =
        maybe "<missing-txid>" show (IntMap.lookup txKey sharedKeyToTxId)

      more = case dtal of
        DMaximum ->
                  [ "sharedPeers" .= [ (show peeraddr, show peerState)
                                     | (peeraddr, peerState) <- Map.toList sharedPeers
                                     ]
                  , "sharedTxTable" .= [ (renderTxId txKey, show txEntry)
                                       | (txKey, txEntry) <- IntMap.toList sharedTxTable
                                       ]
                  , "sharedRetainedTxs" .= [ (renderTxId txKey, show retainUntil)
                                           | (txKey, retainUntil) <- retainedToList sharedRetainedTxs
                                           ]
                  , "internedTxIds" .= fmap show (Map.keys sharedTxIdToKey)
                  ]
        _otherwise -> []


instance MetaTrace (TraceTxLogic peeraddr txid tx) where
  namespaceFor TraceSharedTxState {} =
    Namespace [] ["TraceSharedTxState"]

  severityFor _ _ = Just Debug

  documentFor (Namespace [] ["TraceSharedTxState"]) =
    Just "Internal bookkeeping of tx-submission shared state for peer coordination"
  documentFor _ = Nothing

  allNamespaces = [
    Namespace [] ["TraceSharedTxState"]
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
    ]
  metricsDocFor _ = []

  allNamespaces = [
    Namespace [] ["Counters"]
    ]

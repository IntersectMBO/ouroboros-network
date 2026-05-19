{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Network.Tracing.TxSubmission () where

import Control.Arrow
import Control.Monad.Class.MonadTime.SI
import Data.Aeson
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import Cardano.Logging
import Ouroboros.Network.TxSubmission.Inbound.V2.Types

instance (Show txid, Show peeraddr) => LogFormatting (TraceTxLogic peeraddr txid tx) where
  forMachine dtal (TraceSharedTxState label SharedTxState {..}) =
      mconcat $ [ "kind" .= String "TraceSharedTxState"
                , "label" .= label
                , "inflightTxs" .= (fmap (first show) . Map.toList $ inflightTxs)
                , "bufferedTxs" .= (fmap show . Set.toList . Map.keysSet $ bufferedTxs)
                , "timedTxs"    .= (fmap (\(Time t, txids) -> (t, fmap show txids)) . Map.toList $ timedTxs)
                , "inSubmissionToMempoolTxs" .= (fmap (first show) . Map.toList $ inSubmissionToMempoolTxs)
                ] ++ more
    where
      more = case dtal of
        DMaximum ->
                  [ "peerTxStates" .= (fmap (first show) . Map.toList $ inflightTxs)
                  , "referenceCounts" .= (fmap (first show) . Map.toList $ referenceCounts)
                  ]
        _otherwise -> []

  forMachine dtal (TraceTxDecisions decisionMap) =
       ("kind" .= String "TraceTxDecisions")
    <> case dtal of
         DMaximum -> "decisions" .=
           let g (TxsToMempool txs) = map (show . fst) txs
               f TxDecision {..} =
                 [( fromIntegral txdTxIdsToAcknowledge :: Int, fromIntegral txdTxIdsToRequest :: Int
                  , map (first show) . Map.toList $ txdTxsToRequest, g txdTxsToMempool)]
            in map (\(peer, decision) -> (show peer, f decision)) . Map.toList $ decisionMap
         _otherwise ->
           let f TxDecision {..} = txdTxIdsToAcknowledge == 0 && txdTxIdsToRequest == 0 &&
                                   Map.null txdTxsToRequest
            in "decision-count" .= Map.size (Map.filter (not . f) decisionMap)


instance MetaTrace (TraceTxLogic peeraddr txid tx) where
  namespaceFor TraceSharedTxState {} =
    Namespace [] ["TraceSharedTxState"]
  namespaceFor TraceTxDecisions {} =
    Namespace [] ["TraceTxDecisions"]

  severityFor _ _ = Just Debug

  documentFor (Namespace [] ["TraceSharedTxState"]) =
    Just "Internal bookkeeping of tx-submission shared state for determining fetch decisions"
  documentFor _ = Nothing

  allNamespaces = [
    Namespace [] ["TraceSharedTxState"],
    Namespace [] ["TraceTxDecisions"]
    ]

instance LogFormatting TxSubmissionCounters where
  forMachine _dtal TxSubmissionCounters {..} =
    mconcat [ "kind" .= String "TxSubmissionCounters"
            , "numOfOutstandingTxIds" .= numOfOutstandingTxIds
            , "numOfBufferedTxs" .= numOfBufferedTxs
            , "numOfInSubmissionToMempoolTxs" .= numOfInSubmissionToMempoolTxs
            , "numOfTxIdsInflight" .= numOfTxIdsInflight
            ]

  asMetrics TxSubmissionCounters {..} =
    [ IntM "txSubmission.numOfOutstandingTxIds" (fromIntegral numOfOutstandingTxIds)
    , IntM "txSubmission.numOfBufferedTxs" (fromIntegral numOfBufferedTxs)
    , IntM "txSubmission.numOfInSubmissionToMempoolTxs" (fromIntegral numOfInSubmissionToMempoolTxs)
    , IntM "txSubmission.numOfTxIdsInflight" (fromIntegral numOfTxIdsInflight)
    ]

instance MetaTrace TxSubmissionCounters where
  namespaceFor TxSubmissionCounters {} = Namespace [] ["Counters"]
  severityFor _ _ = Just Debug

  documentFor (Namespace [] ["Counters"]) = Just "Counters for TxSubmission v2"
  documentFor _                           = Nothing

  metricsDocFor (Namespace [] ["Counters"]) =
    [ ("txSubmission.numOfOutstandingTxIds", "txid's which are not yet downloaded")
    , ("txSubmission.numOfBufferedTxs", "tx's which have been recently successfully applied to the mempool")
    , ("txSubmission.numOfInSubmissionToMempoolTxs", "number of all tx's which are enqueued to the mempool")
    , ("txSubmission.numOfTxIdsInflight", "number of all in-flight txid's")
    ]
  metricsDocFor _ = []

  allNamespaces = [
    Namespace [] ["Counters"]
    ]

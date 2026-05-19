{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Network.Tracing.TxSubmission.Outbound () where

import Data.Aeson
import Data.Text qualified as Text

import Cardano.Logging
import Ouroboros.Network.TxSubmission.Outbound

--------------------------------------------------------------------------------
-- TxOutbound Tracer
--------------------------------------------------------------------------------

instance (Show txid, Show tx)
      => LogFormatting (TraceTxSubmissionOutbound txid tx) where
  forMachine dtal (TraceTxSubmissionOutboundRecvMsgRequestTxs txids) =
       ("kind" .= String "TraceTxSubmissionOutboundRecvMsgRequestTxs")
    <> case dtal of
         DDetailed  -> "txIds" .= Text.pack (show txids)
         _otherwise -> mempty

  forMachine dtal (TraceTxSubmissionOutboundSendMsgReplyTxs txs) =
       ("kind" .= String "TraceTxSubmissionOutboundSendMsgReplyTxs")
    <> case dtal of
             DDetailed  -> "txs" .= String (Text.pack $ show txs)
             _otherwise -> mempty

  forMachine _dtal (TraceControlMessage msg) =
    "kind" .= String ("TraceControlMessage" <> Text.pack (show msg))

instance MetaTrace (TraceTxSubmissionOutbound txid tx) where
    namespaceFor TraceTxSubmissionOutboundRecvMsgRequestTxs {} =
      Namespace [] ["RecvMsgRequest"]
    namespaceFor TraceTxSubmissionOutboundSendMsgReplyTxs {} =
      Namespace [] ["SendMsgReply"]
    namespaceFor TraceControlMessage {} =
      Namespace [] ["ControlMessage"]

    severityFor (Namespace _ ["RecvMsgRequest"]) _ =
      Just Info
    severityFor (Namespace _ ["SendMsgReply"]) _ =
      Just Info
    severityFor (Namespace _ ["ControlMessage"]) _ =
      Just Info
    severityFor _ _ = Nothing

    documentFor (Namespace _ ["RecvMsgRequest"]) = Just
      "The IDs of the transactions requested."
    documentFor (Namespace _ ["SendMsgReply"]) = Just
      "The transactions to be sent in the response."
    documentFor (Namespace _ ["ControlMessage"]) = Just
      "Peer selection control instruction"
    documentFor _ = Nothing

    allNamespaces =
      [ Namespace [] ["RecvMsgRequest"]
      , Namespace [] ["SendMsgReply"]
      , Namespace [] ["ControlMessage"]
      ]

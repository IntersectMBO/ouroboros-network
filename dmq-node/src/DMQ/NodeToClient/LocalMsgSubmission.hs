{-# LANGUAGE OverloadedStrings #-}

module DMQ.NodeToClient.LocalMsgSubmission where

import Control.Concurrent.Class.MonadSTM
import Control.Tracer
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Aeson qualified as Aeson
import Data.Maybe

import DMQ.Protocol.LocalMsgSubmission.Server
import DMQ.Protocol.LocalMsgSubmission.Type
import Ouroboros.Network.TxSubmission.Inbound.V2

-- | Local transaction submission server, for adding txs to the 'Mempool'
--
localMsgSubmissionServer ::
     MonadSTM m
  => Tracer m (TraceLocalMsgSubmission msg msgid SigMempoolFail)
  -> TxSubmissionMempoolWriter msgid msg idx m
  -> m (LocalMsgSubmissionServer msg m ())
localMsgSubmissionServer tracer TxSubmissionMempoolWriter { mempoolAddTxs } =
    pure server
  where
    failure =
      -- TODO remove dummy hardcode when mempool returns reason
      (SubmitFail SigExpired, server) <$ traceWith tracer (TraceSubmitFailure SigExpired)
    success msgid =
      (SubmitSuccess, server) <$ traceWith tracer (TraceSubmitAccept msgid)

    server = LocalTxSubmissionServer {
      recvMsgSubmitTx = \msg -> do
        traceWith tracer $ TraceReceivedMsg msg
        -- TODO mempool should return 'SubmitResult'
        maybe failure success . listToMaybe =<< mempoolAddTxs [msg]

    , recvMsgDone = ()
    }


data TraceLocalMsgSubmission msg msgid reject =
    TraceReceivedMsg msg
  -- ^ A transaction was received.
  | TraceSubmitFailure reject
  | TraceSubmitAccept msgid
  deriving Show

instance ( ToJSON msg
         , ToJSON msgid
         , ToJSON reject
         )
      => ToJSON (TraceLocalMsgSubmission msg msgid reject) where
  toJSON (TraceReceivedMsg msg) =
    object [ "kind" .= Aeson.String "TraceReceivedMsg"
           , "msg" .= msg
           ]
  toJSON (TraceSubmitFailure reject) =
    object [ "kind" .= Aeson.String "TraceSubmitFailure"
           , "reason" .= reject
           ]
  toJSON (TraceSubmitAccept msgid) =
    object [ "kind" .= Aeson.String "TraceSubmitAccept"
           , "id" .= msgid
           ]

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}

module DMQ.NodeToClient.LocalMsgSubmission where

import Control.Concurrent.Class.MonadSTM
import "contra-tracer" Control.Tracer
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Aeson qualified as Aeson
import Data.Maybe

import Ouroboros.Network.TxSubmission.Inbound.V2

import DMQ.Protocol.LocalMsgSubmission.Server
import DMQ.Protocol.LocalMsgSubmission.Type

-- | Local transaction submission server, for adding txs to the 'Mempool'
--
localMsgSubmissionServer ::
     MonadSTM m
  => (msg -> msgid)
  -- ^ get message id
  -> Tracer m (TraceLocalMsgSubmission msgid)
  -> TxSubmissionMempoolWriter msgid msg idx m
  -> m (LocalMsgSubmissionServer msg m ())
localMsgSubmissionServer getMsgId tracer TxSubmissionMempoolWriter { mempoolAddTxs } =
    pure server
  where
    failure =
      -- TODO remove dummy hardcode when mempool returns reason
      (SubmitFail SigExpired, server) <$ traceWith tracer (TraceSubmitFailure SigExpired)
    success msgid =
      (SubmitSuccess, server) <$ traceWith tracer (TraceSubmitAccept msgid)

    server = LocalTxSubmissionServer {
      recvMsgSubmitTx = \msg -> do
        traceWith tracer $ TraceReceivedMsg (getMsgId msg)
        -- TODO mempool should return 'SubmitResult'
        maybe failure success . listToMaybe =<< mempoolAddTxs [msg]

    , recvMsgDone = ()
    }


data TraceLocalMsgSubmission msgid =
    TraceReceivedMsg msgid
  -- ^ A transaction was received.
  | TraceSubmitFailure SigMempoolFail
  | TraceSubmitAccept msgid
  deriving Show

instance ToJSON msgid
      => ToJSON (TraceLocalMsgSubmission msgid) where
  toJSON (TraceReceivedMsg msgid) =
    -- TODO: once we have verbosity levels, we could include the full tx, for
    -- now one can use `TraceSendRecv` tracer for the mini-protocol to see full
    -- msgs.
    object [ "kind" .= Aeson.String "TraceReceivedMsg"
           , "sigId" .= msgid
           ]
  toJSON (TraceSubmitFailure reject) =
    object [ "kind" .= Aeson.String "TraceSubmitFailure"
           , "reason" .= reject
           ]
  toJSON (TraceSubmitAccept msgid) =
    object [ "kind" .= Aeson.String "TraceSubmitAccept"
           , "sigId" .= msgid
           ]

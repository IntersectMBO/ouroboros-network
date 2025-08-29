module DMQ.NodeToClient.LocalMsgSubmission where

import Control.Tracer
import Control.Concurrent.Class.MonadSTM
import DMQ.Protocol.LocalMsgSubmission.Server
import Ouroboros.Network.TxSubmission.Inbound.V2
import Data.Maybe
import DMQ.Protocol.LocalMsgSubmission.Type

-- | Local transaction submission server, for adding txs to the 'Mempool'
--
localMsgSubmissionServer ::
     MonadSTM m
  => Tracer m (TraceLocalMsgSubmission msg msgid reject)
  -> reject
  -> TxSubmissionMempoolWriter msgid msg idx m
  -> m (LocalMsgSubmissionServer msg reject m ())
localMsgSubmissionServer tracer reject TxSubmissionMempoolWriter { mempoolAddTxs } =
    pure server
  where
    failure =
      (SubmitFail reject, server) <$ traceWith tracer (TraceSubmitFailure reject)
    success msgid =
      (SubmitSuccess, server) <$ traceWith tracer (TraceSubmitAccept msgid)

    server = LocalTxSubmissionServer {
      recvMsgSubmitTx = \msg -> do
        traceWith tracer $ TraceReceivedMsg msg
        maybe failure success . listToMaybe =<< mempoolAddTxs [msg]

    , recvMsgDone = ()
    }


data TraceLocalMsgSubmission msg msgid reject =
    TraceReceivedMsg msg
  -- ^ A transaction was received.
  | TraceSubmitFailure reject
  | TraceSubmitAccept msgid
  deriving Show

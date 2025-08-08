module DMQ.NodeToClient.LocalMsgSubmission where

import Control.Tracer
import Control.Concurrent.Class.MonadSTM
import Ouroboros.Network.TxSubmission.Mempool.Simple
import DMQ.Protocol.LocalMsgSubmission.Server
import Ouroboros.Network.TxSubmission.Inbound.V2
import Data.Maybe
import DMQ.Protocol.LocalMsgSubmission.Type

-- | Local transaction submission server, for adding txs to the 'Mempool'
--
localMsgSubmissionServer ::
     MonadSTM m
  => Tracer m TraceLocalMsgSubmission
  -> TxSubmissionMempoolWriter msgid msg idx m
  -> m (LocalMsgSubmissionServer msg reject m ())
localMsgSubmissionServer tracer TxSubmissionMempoolWriter { mempoolAddTxs } =
    pure server
  where
    failure = (SubmitFail undefined, server)
    success = const (SubmitSuccess, server)

    server = LocalTxSubmissionServer {
      recvMsgSubmitTx = \msg -> do
        traceWith tracer $ TraceReceivedMsg {- TODO: add msg argument -}
        maybe failure success . listToMaybe <$> mempoolAddTxs [msg]
        -- case addTxRes of
        --   MempoolTxAdded _tx             -> return (SubmitSuccess, server)
        --   MempoolTxRejected _tx addTxErr -> return (SubmitFail addTxErr, server)

    , recvMsgDone = ()
    }


data TraceLocalMsgSubmission
  = TraceReceivedMsg
    -- ^ A transaction was received.

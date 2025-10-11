{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE StandaloneDeriving #-}

module DMQ.NodeToClient.LocalMsgSubmission where

import Control.Concurrent.Class.MonadSTM
import Control.Tracer

import DMQ.Protocol.LocalMsgSubmission.Server
import DMQ.Protocol.LocalMsgSubmission.Type
import Ouroboros.Network.TxSubmission.Mempool.Simple

-- | Local transaction submission server, for adding txs to the 'Mempool'
--
localMsgSubmissionServer ::
     MonadSTM m
  => Tracer m (TraceLocalMsgSubmission sig sigid)
  -> MempoolWriter sigid sig failure idx m
  -- ^ duplicate error tag in case the mempool returns the empty list on failure
  -> m (LocalMsgSubmissionServer sig m ())
localMsgSubmissionServer tracer MempoolWriter { mempoolAddTxs } =
    pure server
  where
    process (sigid, e@(SubmitFail reason)) =
      (e, server) <$ traceWith tracer (TraceSubmitFailure sigid reason)
    process (sigid, success) =
      (success, server) <$ traceWith tracer (TraceSubmitAccept sigid)

    server = LocalTxSubmissionServer {
      recvMsgSubmitTx = \sig -> do
        traceWith tracer $ TraceReceivedMsg sig
        process . head =<< mempoolAddTxs [sig]

    , recvMsgDone = ()
    }


data TraceLocalMsgSubmission sig sigid =
    TraceReceivedMsg sig
  -- ^ A transaction was received.
  | TraceSubmitFailure sigid (MempoolAddFail sig)
  | TraceSubmitAccept sigid

deriving instance
     (Show sig, Show sigid, Show (MempoolAddFail sig))
  => Show (TraceLocalMsgSubmission sig sigid)

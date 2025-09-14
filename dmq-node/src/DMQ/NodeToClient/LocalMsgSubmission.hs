{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}

module DMQ.NodeToClient.LocalMsgSubmission where

import Control.Concurrent.Class.MonadSTM
import Control.Tracer
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Aeson qualified as Aeson

import DMQ.Protocol.LocalMsgSubmission.Server
import DMQ.Protocol.LocalMsgSubmission.Type
import Ouroboros.Network.TxSubmission.Mempool.Simple

-- | Local transaction submission server, for adding txs to the 'Mempool'
--
localMsgSubmissionServer ::
     MonadSTM m
  => (sig -> sigid)
  -- ^ get message id
  -> Tracer m (TraceLocalMsgSubmission sig sigid)
  -> MempoolWriter sigid sig failure idx m
  -- ^ duplicate error tag in case the mempool returns the empty list on failure
  -> m (LocalMsgSubmissionServer sig m ())
localMsgSubmissionServer getMsgId tracer MempoolWriter { mempoolAddTxs } =
    pure server
  where
    process (sigid, e@(SubmitFail reason)) =
      (e, server) <$ traceWith tracer (TraceSubmitFailure sigid reason)
    process (sigid, success) =
      (success, server) <$ traceWith tracer (TraceSubmitAccept sigid)

    server = LocalTxSubmissionServer {
      recvMsgSubmitTx = \sig -> do
        traceWith tracer $ TraceReceivedMsg (getMsgId sig)
        process . head =<< mempoolAddTxs [sig]

    , recvMsgDone = ()
    }


data TraceLocalMsgSubmission sig sigid =
    TraceReceivedMsg sigid
  -- ^ A signature was received.
  | TraceSubmitFailure sigid (MempoolAddFail sig)
  | TraceSubmitAccept sigid

deriving instance
     (Show sig, Show sigid, Show (MempoolAddFail sig))
  => Show (TraceLocalMsgSubmission sig sigid)

instance (ToJSON sigid, ToJSON (MempoolAddFail sig))
      => ToJSON (TraceLocalMsgSubmission sig sigid) where
  toJSON (TraceReceivedMsg sigid) =
    -- TODO: once we have verbosity levels, we could include the full tx, for
    -- now one can use `TraceSendRecv` tracer for the mini-protocol to see full
    -- msgs.
    object [ "kind" .= Aeson.String "TraceReceivedMsg"
           , "sigId" .= sigid
           ]
  toJSON (TraceSubmitFailure sigid reject) =
    object [ "kind" .= Aeson.String "TraceSubmitFailure"
           , "sigId" .= sigid
           , "reason" .= reject
           ]
  toJSON (TraceSubmitAccept sigid) =
    object [ "kind" .= Aeson.String "TraceSubmitAccept"
           , "sigId" .= sigid
           ]

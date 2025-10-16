{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

module DMQ.NodeToClient.LocalMsgSubmission where

import Control.Concurrent.Class.MonadSTM
import Control.Monad.Class.MonadThrow
import Control.Tracer
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Aeson qualified as Aeson
import Data.Typeable

import DMQ.Protocol.LocalMsgSubmission.Server
import DMQ.Protocol.LocalMsgSubmission.Type
import Ouroboros.Network.TxSubmission.Mempool.Simple

-- | Local transaction submission server, for adding txs to the 'Mempool'
--
localMsgSubmissionServer ::
     forall msgid msg idx m.
     ( MonadSTM m
     , MonadThrow m
     , Typeable msgid
     , Typeable msg
     , Show msgid
     , Show (MempoolAddFail msg))
  => (msg -> msgid)
  -- ^ get message id
  -> Tracer m (TraceLocalMsgSubmission msg msgid)
  -> MempoolWriter msgid msg idx m
  -- ^ duplicate error tag in case the mempool returns the empty list on failure
  -> m (LocalMsgSubmissionServer msg m ())
localMsgSubmissionServer getMsgId tracer MempoolWriter { mempoolAddTxs } =
    pure server
  where
    process (Left (msgid, reason)) = do
      traceWith tracer (TraceSubmitFailure msgid reason)
      throwIO $ MsgValidationException msgid reason
    process (Right [(msgid, e@(SubmitFail reason))]) =
      (e, server) <$ traceWith tracer (TraceSubmitFailure msgid reason)
    process (Right [(msgid, SubmitSuccess)]) =
      (SubmitSuccess, server) <$ traceWith tracer (TraceSubmitAccept msgid)
    process _ = throwIO (TooManyMessages @msgid @msg)

    server = LocalTxSubmissionServer {
      recvMsgSubmitTx = \msg -> do
        traceWith tracer $ TraceReceivedMsg (getMsgId msg)
        process =<< mempoolAddTxs [msg]

    , recvMsgDone = ()
    }


data TraceLocalMsgSubmission msg msgid =
    TraceReceivedMsg msgid
  -- ^ A signature was received.
  | TraceSubmitFailure msgid (MempoolAddFail msg)
  | TraceSubmitAccept msgid

deriving instance
     (Show msg, Show msgid, Show (MempoolAddFail msg))
  => Show (TraceLocalMsgSubmission msg msgid)



data MsgSubmissionServerException msgid msg =
    MsgValidationException msgid (MempoolAddFail msg)
  | TooManyMessages

deriving instance (Show (MempoolAddFail msg), Show msgid)
  => Show (MsgSubmissionServerException msgid msg)

instance (Typeable msgid, Typeable msg, Show (MempoolAddFail msg), Show msgid)
  => Exception (MsgSubmissionServerException msgid msg) where


instance (ToJSON msgid, ToJSON (MempoolAddFail msg))
      => ToJSON (TraceLocalMsgSubmission msg msgid) where
  toJSON (TraceReceivedMsg msgid) =
    -- TODO: once we have verbosity levels, we could include the full tx, for
    -- now one can use `TraceSendRecv` tracer for the mini-protocol to see full
    -- msgs.
    object [ "kind" .= Aeson.String "TraceReceivedMsg"
           , "sigId" .= msgid
           ]
  toJSON (TraceSubmitFailure msgid reject) =
    object [ "kind" .= Aeson.String "TraceSubmitFailure"
           , "sigId" .= msgid
           , "reason" .= reject
           ]
  toJSON (TraceSubmitAccept msgid) =
    object [ "kind" .= Aeson.String "TraceSubmitAccept"
           , "sigId" .= msgid
           ]

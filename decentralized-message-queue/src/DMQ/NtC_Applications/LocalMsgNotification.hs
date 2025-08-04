{-# LANGUAGE OverloadedRecordDot #-}

module DMQ.NtC_Applications.LocalMsgNotification where

import Control.Concurrent.Class.MonadSTM
import Control.Tracer
import Data.Maybe (fromJust)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Traversable (mapAccumR)

import DMQ.Protocol.LocalMsgNotification.Type
import DMQ.Protocol.LocalMsgNotification.Server
import Ouroboros.Network.TxSubmission.Mempool.Reader

-- | Local Message Notification server application
--
localMsgNotificationServer
  :: forall m msg msgid idx a b. (MonadSTM m)
  => a -- TODO: remove dummy placeholder
  -> Tracer m (TraceMessageNotificationServer msg)
  -> m b
  -> TxSubmissionMempoolReader msgid msg idx m
  -> LocalMsgNotificationServer m msg b
localMsgNotificationServer _a tracer mb
                           TxSubmissionMempoolReader {
                             mempoolZeroIdx
                           , mempoolGetSnapshot
                           } =
  LocalMsgNotificationServer . pure $ serverIdle undefined mempoolZeroIdx
  where
    serverIdle :: c -> idx -> ServerIdle m msg b
    serverIdle _c !lastIdx = ServerIdle { msgRequestHandler, msgDoneHandler }
      where
        -- TODO: add tracing, raise protocol errors
        msgRequestHandler :: forall blocking.
                             SingBlockingStyle blocking
                          -> m (ServerResponse m blocking msg b)
        msgRequestHandler blocking = do
          let process :: MempoolSnapshot msgid msg idx
                      -> (idx, [msg]) -- last index and extracted messages
              process m =
                let f _lastIdx (_id, idx, _size) = (idx, fromJust . m.mempoolLookupTx $ idx)
                in mapAccumR f lastIdx $ m.mempoolTxIdsAfter lastIdx
          case blocking of
            SingBlocking -> do
              -- TODO: wrap in timeout
              (lastIdx', msgs) <- atomically do
                snapshot <- mempoolGetSnapshot
                let (lastIdx', msgs) = process snapshot
                check (not $ null msgs)
                return (lastIdx', msgs)
              traceWith tracer (TraceMsgSubmissionServerReply msgs)
              return $ ServerReply (BlockingReply (NonEmpty.fromList msgs))
                                   DoesNotHaveMore
                                   (serverIdle undefined lastIdx')
            SingNonBlocking -> do
              snapshot <- atomically mempoolGetSnapshot
              let (lastIdx', msgs) = process snapshot
              traceWith tracer (TraceMsgSubmissionServerReply msgs)
              return $ ServerReply (NonBlockingReply msgs) DoesNotHaveMore (serverIdle undefined lastIdx')

        msgDoneHandler = mb

data TraceMessageNotificationServer msg
  = TraceMsgSubmissionServerReply
      [msg]
      -- ^ The transactions to be sent in the response.
  -- | TraceControlMessage ControlMessage
  deriving Show

data MsgSubmissionProtocolError
  -- deriving Show

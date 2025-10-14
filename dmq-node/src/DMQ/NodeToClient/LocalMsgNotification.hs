module DMQ.NodeToClient.LocalMsgNotification
  ( localMsgNotificationServer
  , LocalMsgNotificationProtocolError (..)
  ) where

import Control.Concurrent.Class.MonadSTM
import Control.Monad.Class.MonadThrow
import Control.Tracer
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromJust)
import Data.Traversable (mapAccumR)
import Data.Word

import DMQ.Protocol.LocalMsgNotification.Server
import DMQ.Protocol.LocalMsgNotification.Type
import Ouroboros.Network.TxSubmission.Mempool.Reader

data LocalMsgNotificationProtocolError =
    ProtocolErrorUnexpectedBlockingRequest
  | ProtocolErrorUnexpectedNonBlockingRequest
  deriving Show

instance Exception LocalMsgNotificationProtocolError where
  displayException ProtocolErrorUnexpectedBlockingRequest =
    "The client issued a blocking request when a non-blocking request was expected."
  displayException ProtocolErrorUnexpectedNonBlockingRequest =
    "The client issued a non-blocking request when a blocking request was expected."

-- | Local Message Notification server application
--
localMsgNotificationServer
  :: forall m msg msgid idx a. (MonadSTM m {-, MonadThrow m -})
  => Tracer m (TraceMessageNotificationServer msg)
  -> m a
  -> Word16
  -> TxSubmissionMempoolReader msgid msg idx m
  -> LocalMsgNotificationServer m msg a
localMsgNotificationServer tracer mdone maxMsgs0
                           TxSubmissionMempoolReader {
                             mempoolZeroIdx
                           , mempoolGetSnapshot
                           } =
  LocalMsgNotificationServer . pure $ serverIdle mempoolZeroIdx DoesNotHaveMore
  where
    maxMsgs = fromIntegral maxMsgs0

    serverIdle :: idx -> HasMore -> ServerIdle m msg a
    serverIdle !lastIdx _hasMore = ServerIdle { msgRequestHandler, msgDoneHandler }
      where
        msgRequestHandler :: forall blocking.
                             SingBlockingStyle blocking
                          -> m (ServerResponse m blocking msg a)
        msgRequestHandler blocking = do
          let process :: MempoolSnapshot msgid msg idx
                      -> (idx, HasMore, [msg]) -- last index and extracted messages
              process ms =
                let f _lastIdx (_id, idx, _size) = (idx, fromJust $ mempoolLookupTx ms idx)
                    (prefix, rest) = splitAt maxMsgs . mempoolTxIdsAfter ms $ lastIdx
                    hasMore' = if null rest then DoesNotHaveMore else HasMore
                    (lastIdx', msgs) = mapAccumR f lastIdx prefix
                in (lastIdx', hasMore', msgs)
          case blocking of
            SingBlocking
              -- | HasMore <- hasMore ->
              --     throwIO ProtocolErrorUnexpectedBlockingRequest
              | otherwise -> do
                  (lastIdx', hasMore', msgs) <- atomically do
                    snapshot <- mempoolGetSnapshot
                    let (lastIdx', hasMore', msgs) = process snapshot
                    check . not . null $ msgs
                    return (lastIdx', hasMore', msgs)

                  traceWith tracer (TraceMsgNotificationServerReply hasMore' msgs)
                  return $ ServerReply (BlockingReply (NonEmpty.fromList msgs))
                                       hasMore'
                                       (serverIdle lastIdx' hasMore')
            SingNonBlocking
              -- | DoesNotHaveMore <- hasMore ->
              --     throwIO ProtocolErrorUnexpectedNonBlockingRequest
              | otherwise -> do
                  snapshot <- atomically mempoolGetSnapshot
                  let (lastIdx', hasMore', msgs) = process snapshot
                  traceWith tracer (TraceMsgNotificationServerReply hasMore' msgs)
                  return $ ServerReply (NonBlockingReply msgs) hasMore' (serverIdle lastIdx' hasMore')

        msgDoneHandler =
          traceWith tracer TraceMsgNotificationServerHandleDone >> mdone


data TraceMessageNotificationServer msg =
    TraceMsgNotificationServerReply HasMore [msg]
    -- ^ The transactions to be sent in the response.
  | TraceMsgNotificationServerHandleDone
    -- ^ client terminates
  deriving Show

module DMQ.Protocol.LocalMsgNotification.Examples where

import Control.Exception (assert)
import Control.Tracer
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Word

import DMQ.Protocol.LocalMsgNotification.Client
import DMQ.Protocol.LocalMsgNotification.Server
import DMQ.Protocol.LocalMsgNotification.Type

data TraceEventServer msg =
  TraceServerReply [msg] HasMore
  deriving Show

data TraceEventClient msg =
    TraceClientDone [msg] -- ^ all messages received
  | TraceClientRequestNonBlocking [msg] -- ^ messages from last reply
  | TraceClientRequestBlocking [msg] -- ^ messages from last reply
  deriving Show


-- | An example transaction submission client.
--
-- It collects and returns all the transactions that the server presents. This
-- is suitable for tests and using as a starting template for a full version.
--
-- Note that this example does not respect any overall byte limit on pipelining
-- and does not make any delta-Q info to optimises the pipelining decisions.
--
msgNotificationClient
  :: forall msg m. (Monad m)
  => Tracer m (TraceEventClient msg)
  -> Word16  -- ^ Maximum number of messages
  -> LocalMsgNotificationClient m msg [msg]
msgNotificationClient tracer maxMsgs =
    LocalMsgNotificationClient . return $
      SendMsgRequestBlocking (clientReceiveBlocking [])
  where
    clientReceiveBlocking :: [msg] -> NonEmpty msg -> HasMore
                          -> m (LocalMsgNotificationClientStIdle m msg [msg])
    clientReceiveBlocking msgs0 msgs hasMore =
      let acc = msgs0 <> NonEmpty.toList msgs
       in assert (NonEmpty.length msgs <= fromIntegral maxMsgs)
          case hasMore of
            DoesNotHaveMore -> do
              traceWith tracer $ TraceClientDone acc
              return . SendMsgDone $ pure acc
            HasMore -> do
              traceWith tracer $ TraceClientRequestNonBlocking acc
              return . SendMsgRequestNonBlocking $ clientReceiveNonBlocking acc

    clientReceiveNonBlocking :: [msg] -> [msg] -> HasMore
                             -> m (LocalMsgNotificationClientStIdle m msg [msg])
    clientReceiveNonBlocking msgs0 msgs hasMore =
      let acc = msgs0 <> msgs
       in assert (length msgs <= fromIntegral maxMsgs)
          case hasMore of
            DoesNotHaveMore -> do
              traceWith tracer $ TraceClientRequestBlocking acc
              return $ SendMsgRequestBlocking (clientReceiveBlocking acc)
            HasMore -> do
              traceWith tracer $ TraceClientRequestNonBlocking acc
              return . SendMsgRequestNonBlocking $ clientReceiveNonBlocking acc


-- | An example @'LocalMsgNotificationServer'@ which sends transactions from a fixed
-- list of transactions.
--
-- It is intended to illustrate the protocol or for use in tests, with enforcement
-- of some invariants.
--
msgNotificationServer
  :: forall msg m.
     (Monad m)
  => Tracer m (TraceEventServer msg)
  -> Word16  -- ^ Maximum number of txs to request in any one reply
  -> NonEmpty msg
  -> LocalMsgNotificationServer m msg ()
msgNotificationServer tracer maxMsgs neMsgs0 =
  LocalMsgNotificationServer . pure $ server HasMore msgs0
  where
    msgs0 = NonEmpty.toList neMsgs0

    server :: HasMore -> [msg] -> ServerIdle m msg ()
    server hasMore msgs =
      assert invariant
      ServerIdle { msgRequestHandler = msgRequestHandler hasMore msgs, msgDoneHandler = pure () }
      where
        invariant =
          case (hasMore, msgs) of
            (HasMore, []) -> False
            _otherwise    -> True

    msgRequestHandler :: forall blocking.
                         HasMore
                      -> [msg]
                      -> SingBlockingStyle blocking
                      -> m (ServerResponse m blocking msg ())
    msgRequestHandler DoesNotHaveMore [] block =
      error $ "LocalMsgNotificationServer:msgRequestHandler: client violation DoesNotHaveMore []" <> show block
    msgRequestHandler HasMore [] _blocking =
      error "LocalMsgNotificationServer:msgRequestHandler: server error HasMore []"
    msgRequestHandler DoesNotHaveMore _msgs SingNonBlocking =
      error "LocalMsgNotificationServer:msgRequestHandler: client violation DoesNotHaveMore _msgs SingNonBlocking"
    msgRequestHandler _ msgs@(_:_) SingNonBlocking = do
      let leading = init msgs
          prefix  = take (fromIntegral maxMsgs) leading
          msgs'   = drop (length prefix) leading <> [last msgs]
          hasMore' | not (null prefix || null msgs') = HasMore
                   | otherwise = DoesNotHaveMore
      traceWith tracer $ TraceServerReply prefix hasMore'
      return $
        ServerReply (NonBlockingReply prefix) hasMore'
                    (server hasMore' msgs')
    msgRequestHandler _ msgs@(_:_) SingBlocking = do
      let prefix = take (fromIntegral maxMsgs) msgs
          msgs'  = drop (length prefix) msgs
          hasMore' | null msgs' = DoesNotHaveMore
                   | otherwise  = HasMore
      traceWith tracer $ TraceServerReply prefix hasMore'

      return $
        ServerReply (BlockingReply . NonEmpty.fromList $ prefix) hasMore'
                    (server hasMore' msgs')

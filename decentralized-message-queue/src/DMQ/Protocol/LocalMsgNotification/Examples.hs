module DMQ.Protocol.LocalMsgNotification.Examples where

import Control.Exception (assert)
import Control.Tracer
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Word
import System.Random

import DMQ.Protocol.LocalMsgNotification.Client
import DMQ.Protocol.LocalMsgNotification.Server
import DMQ.Protocol.LocalMsgNotification.Type

data TraceEventServer msg =
    TraceServerDone
  | TraceServerReply [msg] HasMore
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
      SendMsgRequestBlocking (pure []) (clientReceiveBlocking [])
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
              return $ SendMsgRequestBlocking (pure acc) (clientReceiveBlocking acc)
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
  -> StdGen  -- ^ random 'HasMore'
  -> Word16  -- ^ Maximum number of txs to request in any one reply
  -> [msg]
  -> LocalMsgNotificationServer m msg ()
msgNotificationServer tracer rnd0 maxMsgs msgs0 =
  LocalMsgNotificationServer . pure $ server rnd0 DoesNotHaveMore msgs0
  where
    server :: StdGen -> HasMore -> [msg] -> ServerIdle m msg ()
    server rnd hasMore msgs =
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
        msgRequestHandler DoesNotHaveMore [] SingBlocking = do
          traceWith tracer TraceServerDone
          return $ ServerDone ()
        msgRequestHandler DoesNotHaveMore [] SingNonBlocking =
          error "LocalMsgNotificationServer:msgRequestHandler: DoesNotHaveMore [] SingNonBlocking"
        msgRequestHandler HasMore [] _blocking =
          error "LocalMsgNotificationServer:msgRequestHandler: impossible HasMore []"
        msgRequestHandler HasMore _msgs SingBlocking =
          error "LocalMsgNotificationServer:msgRequestHandler: HasMore SingBlocking"
        msgRequestHandler DoesNotHaveMore _msgs SingNonBlocking =
          error "LocalMsgNotificationServer:msgRequestHandler: DoesNotHaveMore _msgs SingNonBlocking"
        msgRequestHandler _ msgs@(_:_) blocking = do
          let prefix = take (fromIntegral maxMsgs) msgs
              msgs'  = drop (length prefix) msgs
              (toss, rnd') = random rnd
              hasMore' | null msgs' = DoesNotHaveMore
                       | SingBlocking <- blocking = HasMore
                       -- we cheat here a little bit to pretend we're out temporarily
                       -- to excercise the client
                       | toss       = HasMore
                       | otherwise  = DoesNotHaveMore
          traceWith tracer $ TraceServerReply prefix hasMore'
          case blocking of
            SingBlocking ->
              return $ ServerReply (BlockingReply . NonEmpty.fromList $ prefix) hasMore'
                                   (server rnd' hasMore' msgs')
            SingNonBlocking ->
              return $ ServerReply (NonBlockingReply prefix) hasMore'
                                   (server rnd' hasMore' msgs')

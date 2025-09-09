{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE PolyKinds  #-}
{-# LANGUAGE RankNTypes #-}

-- | A higher level API to implement server for local message notification
-- miniprotocol.
--
-- For execution, 'localMsgNotificationServerPeer' reinterprets this high level
-- description into the underlying typed protocol representation.
--
module DMQ.Protocol.LocalMsgNotification.Server
  ( -- * Server API types
    LocalMsgNotificationServer (..)
  , ServerIdle (..)
  , ServerResponse (..)
    -- * Translates the server into a typed protocol
  , localMsgNotificationServerPeer
  ) where

import DMQ.Protocol.LocalMsgNotification.Type
import Network.TypedProtocol.Peer.Server

-- | The high level server wrapper
--
newtype LocalMsgNotificationServer m msg a =
  LocalMsgNotificationServer (m (ServerIdle m msg a))


-- | The server high level message handlers
--
data ServerIdle m msg a = ServerIdle {
  msgRequestHandler :: forall blocking.
                       SingBlockingStyle blocking
                    -> m (ServerResponse m blocking msg a),
  msgDoneHandler    :: !(m a)
  }


-- | The server high level response type
--
data ServerResponse m blocking msg a where
  -- | The server provides a response to the client's query
  --
  ServerReply :: BlockingReplyList blocking msg -- ^ received messages
              -> HasMore
              -> ServerIdle m msg a         -- ^ a continuation
              -> ServerResponse m blocking msg a


-- | tranlates the server into the typed protocol representation
--
localMsgNotificationServerPeer
  :: forall m msg a. Monad m
  => LocalMsgNotificationServer m msg a
  -> Server (LocalMsgNotification msg) NonPipelined StIdle m a
localMsgNotificationServerPeer (LocalMsgNotificationServer handler) =
  Effect $ go <$> handler
  where
    go  :: ServerIdle m msg a
        -> Server (LocalMsgNotification msg) NonPipelined StIdle m a
    go ServerIdle { msgRequestHandler, msgDoneHandler } =
      Await \case
        MsgRequest blocking -> Effect do
          ServerReply msgs more k <- msgRequestHandler blocking
          pure $ Yield (MsgReply msgs more) (go k)

        MsgClientDone -> Effect $ Done <$> msgDoneHandler

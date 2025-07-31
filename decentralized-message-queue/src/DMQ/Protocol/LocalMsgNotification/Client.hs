{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

-- | A higher level API to implement clients for local message notification
-- miniprotocol.
--
-- For execution, 'localMsgNotificationClientPeer' reinterprets this high level
-- description into the underlying typed protocol representation.
--
module DMQ.Protocol.LocalMsgNotification.Client
  ( -- * Client API types
    LocalMsgNotificationClient (..)
  , LocalMsgNotificationClientStIdle (..)
    -- * Translates the client into a typed protocol
  , localMsgNotificationClientPeer
  ) where

import Data.List.NonEmpty (NonEmpty)

import DMQ.Protocol.LocalMsgNotification.Type
import Network.TypedProtocol.Peer.Client

-- | The high level client wrapper
--
newtype LocalMsgNotificationClient m msg a = LocalMsgNotificationClient {
  runMsgNotificationClient :: m (LocalMsgNotificationClientStIdle m msg a)
  }


-- | The client API message types
--
data LocalMsgNotificationClientStIdle m msg a =
    SendMsgRequestBlocking
      !(   NonEmpty msg
        -> HasMore
        -> m (LocalMsgNotificationClientStIdle m msg a)) -- ^ a continuation
  | SendMsgRequestNonBlocking
      !(   [msg]
        -> HasMore
        -> m (LocalMsgNotificationClientStIdle m msg a))
  | SendMsgDone !(m a)


-- | A non-pipelined 'Peer' representing the 'LocalMsgNotificationClient'.
--
-- Translates the client into the typed protocol representation.
--
localMsgNotificationClientPeer
  :: forall m msg a. (Monad m)
  => LocalMsgNotificationClient m msg a
  -> Client (LocalMsgNotification msg) NonPipelined StIdle m a
localMsgNotificationClientPeer (LocalMsgNotificationClient client) =
  Effect $ go <$> client
  where
    go :: LocalMsgNotificationClientStIdle m msg a
       -> Client (LocalMsgNotification msg) NonPipelined StIdle m a
    go (SendMsgRequestBlocking k) =
      Yield (MsgRequest SingBlocking)
      $ Await \case
          MsgReply (BlockingReply msgs) more -> Effect $ go <$> k msgs more

    go (SendMsgRequestNonBlocking k) =
      Yield (MsgRequest SingNonBlocking)
      $ Await \case
          MsgReply (NonBlockingReply msgs) more -> Effect $ go <$> k msgs more

    go (SendMsgDone done) =
      Yield MsgClientDone . Effect $ Done <$> done

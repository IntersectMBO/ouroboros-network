{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

module Ouroboros.Network.Protocol.KeepAlive.Client (
    KeepAliveClient (..),
    keepAliveClientPeer
  ) where

import           Network.TypedProtocol.Core
import           Ouroboros.Network.Protocol.KeepAlive.Type


data KeepAliveClient m a where
    SendMsgKeepAlive
      :: (m (KeepAliveClient m a))
      -> KeepAliveClient m a

    SendMsgDone
      :: m a
      -> KeepAliveClient m a


-- | Interpret a particular client action sequence into the client side of the
-- 'KeepAlive' protocol.
--
keepAliveClientPeer
  :: Functor m
  => KeepAliveClient m a
  -> Peer KeepAlive AsClient StClient m a

keepAliveClientPeer (SendMsgDone mresult) =
    Yield (ClientAgency TokClient) MsgDone $
      Effect (Done TokDone <$> mresult)

keepAliveClientPeer (SendMsgKeepAlive next) =
    Yield (ClientAgency TokClient) MsgKeepAlive $
      Await (ServerAgency TokServer) $ \MsgKeepAliveResponse ->
        Effect $ keepAliveClientPeer <$> next

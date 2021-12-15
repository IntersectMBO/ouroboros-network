{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

module Ouroboros.Network.Protocol.KeepAlive.Client
  ( KeepAliveClient (..)
  , keepAliveClientPeer
  ) where

import           Control.Monad.Class.MonadThrow
import           Network.TypedProtocol.Core
import           Ouroboros.Network.Protocol.KeepAlive.Type


data KeepAliveClient m a where
    SendMsgKeepAlive
      :: Cookie
      -> (m (KeepAliveClient m a))
      -> KeepAliveClient m a

    SendMsgDone
      :: m a
      -> KeepAliveClient m a


-- | Interpret a particular client action sequence into the client side of the
-- 'KeepAlive' protocol.
--
keepAliveClientPeer
  :: MonadThrow m
  => KeepAliveClient m a
  -> Peer KeepAlive AsClient StClient m a

keepAliveClientPeer (SendMsgDone mresult) =
    Yield (ClientAgency TokClient) MsgDone $
      Effect (Done TokDone <$> mresult)

keepAliveClientPeer (SendMsgKeepAlive cookieReq next) =
    Yield (ClientAgency TokClient) (MsgKeepAlive cookieReq) $
      Await (ServerAgency TokServer) $ \(MsgKeepAliveResponse cookieRsp) ->
        if cookieReq == cookieRsp then Effect $ keepAliveClientPeer <$> next
                                  else Effect $ throwIO $ KeepAliveCookieMissmatch cookieReq cookieRsp

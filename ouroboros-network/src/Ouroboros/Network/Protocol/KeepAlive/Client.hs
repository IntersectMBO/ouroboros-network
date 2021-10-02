{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

module Ouroboros.Network.Protocol.KeepAlive.Client
  ( KeepAliveClient (..)
  , keepAliveClientPeer
  ) where

import           Control.Monad.Class.MonadThrow
import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Peer.Client
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
  -> Client KeepAlive 'NonPipelined Empty StClient m stm a

keepAliveClientPeer (SendMsgDone mresult) =
    Yield MsgDone $
      Effect (Done <$> mresult)

keepAliveClientPeer (SendMsgKeepAlive cookieReq next) =
    Yield (MsgKeepAlive cookieReq) $
      Await $ \(MsgKeepAliveResponse cookieRsp) ->
        if cookieReq == cookieRsp then Effect $ keepAliveClientPeer <$> next
                                  else Effect $ throwIO $ KeepAliveCookieMissmatch cookieReq cookieRsp

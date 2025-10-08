{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.Protocol.KeepAlive.Client
  ( KeepAliveClient (..)
  , KeepAliveClientSt (..)
  , keepAliveClientPeer
  ) where

import Control.Monad.Class.MonadThrow
import Network.TypedProtocol.Core
import Network.TypedProtocol.Peer.Client
import Ouroboros.Network.Protocol.KeepAlive.Type


newtype KeepAliveClient m a = KeepAliveClient (m (KeepAliveClientSt m a))

data KeepAliveClientSt m a where
    SendMsgKeepAlive
      :: Cookie
      -> m (KeepAliveClientSt m a)
      -> KeepAliveClientSt m a

    SendMsgDone
      :: m a
      -> KeepAliveClientSt m a


-- | Interpret a particular client action sequence into the client side of the
-- 'KeepAlive' protocol.
--
keepAliveClientPeer
  :: forall m a. MonadThrow m
  => KeepAliveClient m a
  -> Client KeepAlive NonPipelined StClient m a
keepAliveClientPeer (KeepAliveClient client) =
   Effect $ keepAliveClientStPeer <$> client
 where

   keepAliveClientStPeer
     :: KeepAliveClientSt m a
     -> Client KeepAlive NonPipelined StClient m a

   keepAliveClientStPeer (SendMsgDone mresult) =
     Yield MsgDone $
       Effect (Done <$> mresult)

   keepAliveClientStPeer (SendMsgKeepAlive cookieReq next) =
     Yield (MsgKeepAlive cookieReq) $
       Await $ \(MsgKeepAliveResponse cookieRsp) ->
         if cookieReq == cookieRsp then Effect $ keepAliveClientStPeer <$> next
                                   else Effect $ throwIO $ KeepAliveCookieMissmatch cookieReq cookieRsp

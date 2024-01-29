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
  -> Peer KeepAlive AsClient StClient m a
keepAliveClientPeer (KeepAliveClient client) =
   Effect $ keepAliveClientStPeer <$> client
 where

   keepAliveClientStPeer
     :: KeepAliveClientSt m a
     -> Peer KeepAlive AsClient StClient m a

   keepAliveClientStPeer (SendMsgDone mresult) =
     Yield (ClientAgency TokClient) MsgDone $
       Effect (Done TokDone <$> mresult)

   keepAliveClientStPeer (SendMsgKeepAlive cookieReq next) =
     Yield (ClientAgency TokClient) (MsgKeepAlive cookieReq) $
       Await (ServerAgency TokServer) $ \(MsgKeepAliveResponse cookieRsp) ->
         if cookieReq == cookieRsp then Effect $ keepAliveClientStPeer <$> next
                                   else Effect $ throwIO $ KeepAliveCookieMissmatch cookieReq cookieRsp

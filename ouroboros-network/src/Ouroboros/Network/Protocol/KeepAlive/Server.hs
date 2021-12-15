{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE NamedFieldPuns #-}

module Ouroboros.Network.Protocol.KeepAlive.Server
  ( KeepAliveServer (..)
  , keepAliveServerPeer
  ) where

import           Network.TypedProtocol.Core
import           Ouroboros.Network.Protocol.KeepAlive.Type


data KeepAliveServer m a = KeepAliveServer {
    recvMsgKeepAlive :: m (KeepAliveServer m a),

    recvMsgDone      :: m a
  }


keepAliveServerPeer
    :: Functor m
    => KeepAliveServer m a
    -> Peer KeepAlive AsServer StClient m a
keepAliveServerPeer KeepAliveServer { recvMsgKeepAlive, recvMsgDone } =
    Await (ClientAgency TokClient) $ \msg ->
      case msg of
        MsgDone -> Effect $ Done TokDone <$> recvMsgDone

        MsgKeepAlive cookie ->
          Effect $
            fmap (\server ->
                    Yield (ServerAgency TokServer)
                          (MsgKeepAliveResponse cookie)
                          (keepAliveServerPeer server))
                 recvMsgKeepAlive

{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE NamedFieldPuns #-}

module Ouroboros.Network.Protocol.KeepAlive.Server
  ( KeepAliveServer (..)
  , keepAliveServerPeer
  ) where

import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Peer.Server
import           Ouroboros.Network.Protocol.KeepAlive.Type


data KeepAliveServer m a = KeepAliveServer {
    recvMsgKeepAlive :: m (KeepAliveServer m a),

    recvMsgDone      :: m a
  }


keepAliveServerPeer
    :: Functor m
    => KeepAliveServer m a
    -> Server KeepAlive 'NonPipelined Empty StClient m stm a
keepAliveServerPeer KeepAliveServer { recvMsgKeepAlive, recvMsgDone } =
    Await $ \msg ->
      case msg of
        MsgDone -> Effect $ Done <$> recvMsgDone

        MsgKeepAlive cookie ->
          Effect $
            fmap (\server ->
                    Yield (MsgKeepAliveResponse cookie)
                          (keepAliveServerPeer server))
                 recvMsgKeepAlive

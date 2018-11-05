{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Ouroboros.Network.Protocol.Get.Server where

import Ouroboros.Network.Protocol.Typed
import Ouroboros.Network.Protocol.Get.Type

data Server m resource resourceId a = Server {
    -- | The client requested data identified by `resourceId`.
    getData :: resourceId -> m (Maybe resource),

    -- | The terminal value returned by the server.
    handleDone :: a
  }

-- | Create server side of the @'GetProtocol'@.
--
streamServer
  :: Monad m
  => Server m resource resourceId a
  -> Peer GetProtocol (GetMessage resource resourceId)
          (Awaiting StIdle) (Finished StDone)
          m a
streamServer Server {..} = await $ \msg ->
  case msg of
    MsgRequest rid -> hole $ do
      mr <- getData rid
      case mr of
        Just r  -> pure $ out (MsgResponse r) (done handleDone)
        Nothing -> pure $ out MsgNoData (done handleDone)

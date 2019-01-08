{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Protocol.Get.Server where

import Protocol.Core
import Protocol.Get.Type

data Server m resource resourceId a = Server {
    -- | The client requested data identified by `resourceId`.
    serverRequest :: resourceId -> m (Maybe resource),

    -- | The terminal value returned by the server.
    serverDone :: a
  }

-- | Create server side of the @'GetProtocol'@.
--
streamServer
  :: Monad m
  => Server m resource resourceId a
  -> Peer GetProtocol (GetMessage resource resourceId)
          (Awaiting StIdle) (Finished StDone)
          m a
streamServer Server {..} =
  await $ \msg ->
  case msg of
    MsgRequest rid -> lift $ do
      mr <- serverRequest rid
      case mr of
        Just r  -> pure $ out (MsgResponse r) (done serverDone)
        Nothing -> pure $ out MsgNoData (done serverDone)

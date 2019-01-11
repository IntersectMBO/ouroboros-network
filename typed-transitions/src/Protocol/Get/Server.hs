{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Protocol.Get.Server where

import Protocol.Core
import Protocol.Get.Type

newtype Server m request response a = Server {
    -- | The client requested data identified by `resourceId`.
    runServer :: request -> m (response, a)
  }

-- | Create server side of the @'GetProtocol'@.
--
streamServer
  :: Monad m
  => Server m request response a
  -> Peer (GetProtocol request response) (GetMessage request response)
          (Awaiting StIdle) (Finished StDone)
          m a
streamServer server =
  await $ \msg ->
  case msg of
    MsgRequest request -> lift $ do
      (resp, a) <- runServer server request
      return $ out (MsgResponse resp) (done a)

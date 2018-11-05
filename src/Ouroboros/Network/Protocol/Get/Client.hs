{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Ouroboros.Network.Protocol.Get.Client where

import Ouroboros.Network.Protocol.Typed
import Ouroboros.Network.Protocol.Get.Type

data Client m resource resourceId a = Client {
    -- | Handle incoming data.
    handleData :: Maybe resource -> m a,
    -- | Request resource indenfied by @'getResourceId'@
    getResourceId :: resourceId
  }

-- | Interpret @'Client'@ as a client side of the typed @'GetProtocol'@
--
streamClient
  :: Monad m
  => Client m resource resourceId a
  -> Peer GetProtocol (GetMessage resource resourceId)
          (Yielding StIdle) (Finished StDone)
          m a
streamClient Client {..} = over (MsgRequest getResourceId) $ await $ \msg -> 
  case msg of
    MsgResponse r -> hole (done <$> handleData (Just r))
    MsgNoData     -> hole (done <$> handleData Nothing)

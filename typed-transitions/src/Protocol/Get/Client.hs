{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Protocol.Get.Client where

import Protocol.Core
import Protocol.Get.Type

-- | Client reqeusting a resource identified by a resouce id.
data Client m resource resourceId a where
    Request :: resourceId -> (Maybe resource -> m a) -> Client m resource resourceId a

-- | Interpret @'Client'@ as a client side of the typed @'GetProtocol'@
--
streamClient
  :: Monad m
  => Client m resource resourceId a
  -> Peer GetProtocol (GetMessage resource resourceId)
          ('Yielding 'StIdle) ('Finished 'StDone)
          m a
streamClient (Request resourceId handleData) = over (MsgRequest resourceId) $ await $ \msg ->
  case msg of
    MsgResponse r -> lift (done <$> handleData (Just r))
    MsgNoData     -> lift (done <$> handleData Nothing)

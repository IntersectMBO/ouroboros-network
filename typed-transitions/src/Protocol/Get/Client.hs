{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Protocol.Get.Client where

import Protocol.Core
import Protocol.Get.Type

-- | Client reqeust with a handle for a response.
--
data Client m request response a where
    Request :: request
            -> (response -> m a)
            -> Client m request response a

-- | Interpret @'Client'@ as a client side of the typed @'GetProtocol'@
--
streamClient
  :: Monad m
  => Client m request response a
  -> Peer (GetProtocol request response) (GetMessage request response)
          (Yielding StIdle) (Finished StDone)
          m a
streamClient (Request request handleResponse) =
  over (MsgRequest request) $
  await $ \msg ->
  case msg of
    MsgResponse r -> lift (done <$> handleResponse r)

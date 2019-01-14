{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Ouroboros.Network.Protocol.ReqResp.Client where

import Protocol.Core
import Ouroboros.Network.Protocol.ReqResp.Type

-- | Client reqeust with a handle for a response.
--
data Client m request response a where
    Request :: request
            -> (response -> m a)
            -> Client m request response a

-- | Interpret @'Client'@ as a client side of the typed @'ReqRespProtocol'@
--
reqRespClientPeer
  :: Monad m
  => Client m request response a
  -> Peer (ReqRespProtocol request response) (ReqRespMessage request response)
          (Yielding StIdle) (Finished StDone)
          m a
reqRespClientPeer (Request request handleResponse) =
  over (MsgRequest request) $
  await $ \msg ->
  case msg of
    MsgResponse r -> lift (done <$> handleResponse r)

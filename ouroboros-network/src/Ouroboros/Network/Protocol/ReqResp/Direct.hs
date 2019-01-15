{-# LANGUAGE TupleSections #-}

module Ouroboros.Network.Protocol.ReqResp.Direct where

import Ouroboros.Network.Protocol.ReqResp.Client
import Ouroboros.Network.Protocol.ReqResp.Server

-- | The @'Ouroboros.Network.Protocol.Get.Client'@ and
-- @'Ouroboros.Network.Protocol.Get.Server'@ are dual (complementary) to each
-- other.
--
direct
  :: Monad m
  => ReqRespServer m request response a
  -> ReqRespClient m request response b
  -> m (a, b)
direct server (Request request handleResponse) = do
  (resp, a) <- runReqRespServer server request
  (a,) <$> handleResponse resp

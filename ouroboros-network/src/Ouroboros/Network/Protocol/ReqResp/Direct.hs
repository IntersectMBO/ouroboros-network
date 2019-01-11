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
  => Server m request response a
  -> Client m request response b
  -> m (a, b)
direct server (Request request handleResponse) = do
  (resp, a) <- runServer server request
  (a,) <$> handleResponse resp

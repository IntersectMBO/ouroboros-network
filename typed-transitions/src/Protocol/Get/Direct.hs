{-# LANGUAGE TupleSections #-}

module Protocol.Get.Direct where

import Protocol.Get.Client
import Protocol.Get.Server

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

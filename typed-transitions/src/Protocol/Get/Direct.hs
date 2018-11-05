{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}

module Protocol.Get.Direct where

import Protocol.Get.Client
import Protocol.Get.Server

-- | The 'Ouroboros.Network.Protocol.Get.Client' and
-- 'Ouroboros.Network.Protocol.Get.Server' are dual (complementary).
direct
  :: Monad m
  => Server m resource resourceId a
  -> Client m resource resourceId b
  -> m (a, b)
direct Server {..} (Request resourceId req) = do
  mr <- serverRequest resourceId
  (serverDone,) <$> req mr
  

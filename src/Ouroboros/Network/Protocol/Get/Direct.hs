{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}

module Ouroboros.Network.Protocol.Get.Direct where

import Ouroboros.Network.Protocol.Get.Client
import Ouroboros.Network.Protocol.Get.Server

-- | The 'Ouroboros.Network.Protocol.Get.Client' and
-- 'Ouroboros.Network.Protocol.Get.Server' are dual (complementary).
direct
  :: Monad m
  => Server m resource resourceId a
  -> Client m resource resourceId b
  -> m (a, b)
direct Server {..} Client {..} = do
  mr <- getData getResourceId
  (handleDone,) <$> handleData mr
  

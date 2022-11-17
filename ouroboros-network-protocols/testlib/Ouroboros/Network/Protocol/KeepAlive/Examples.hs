{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.Protocol.KeepAlive.Examples where

import           Ouroboros.Network.Protocol.KeepAlive.Client
import           Ouroboros.Network.Protocol.KeepAlive.Server
import           Ouroboros.Network.Protocol.KeepAlive.Type


-- | A client which applies a function whenever it receives
-- 'MsgKeepAliveResponse' and returns the result.
--
keepAliveClientApply :: forall acc m. Monad m
                    => (acc -> acc)
                    -> acc
                    -> Int
                    -- ^ count of number of requests
                    -> KeepAliveClient m acc
keepAliveClientApply f = go
  where
    go :: acc -> Int -> KeepAliveClient m acc
    go acc n
      | n <= 0
      = SendMsgDone (pure acc)

      | otherwise
      = SendMsgKeepAlive (Cookie $ fromIntegral n) $
          pure $ go (f acc) (pred n)


-- | A server which counts number received of 'MsgKeepAlive'.
--
keepAliveServerCount :: forall m. Applicative m
                     => KeepAliveServer m Int
keepAliveServerCount = go 0
  where
    go n =
      KeepAliveServer {
          recvMsgDone      = pure n,
          recvMsgKeepAlive = pure (go (succ n))
        }


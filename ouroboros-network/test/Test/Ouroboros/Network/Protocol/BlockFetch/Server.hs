module Test.Ouroboros.Network.Protocol.BlockFetch.Server where

import Ouroboros.Network.Protocol.BlockFetch.Server

-- | Testing server which accumulates received value in its return value.
--
accumulatingBlockFetchServerReceiver
  :: Monad m
  => BlockFetchServerReceiver range m [range]
accumulatingBlockFetchServerReceiver = go []
 where
  go acc =
    BlockFetchServerReceiver {
      recvMessageRequestRange = \range -> return $ go (range : acc),
      recvMessageDone         = return (reverse acc)
    }

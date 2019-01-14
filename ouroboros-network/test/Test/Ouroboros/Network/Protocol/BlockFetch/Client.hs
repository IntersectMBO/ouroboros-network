module Test.Ouroboros.Network.Protocol.BlockFetch.Client where

import Ouroboros.Network.Protocol.BlockFetch.Client

-- | @'BlockFetchClientReceiver'@ which accumulates received blocks.
--
blockFetchClientReceiver
  :: Applicative m
  => BlockFetchClientReceiver block m [block]
blockFetchClientReceiver = receiver []
 where
  receiver acc = BlockFetchClientReceiver {
      recvMsgStartBatch = pure (blockReceiver acc),
      recvMsgNoBlocks   = pure (receiver acc),
      recvMsgDoneClient = acc
    }
  blockReceiver acc = BlockFetchClientReceiveBlocks {
      recvMsgBlock       = \b -> pure (blockReceiver (b : acc)),
      recvMsgBatchDone   = pure (receiver acc),
      recvMsgServerError = pure (receiver acc)
    }

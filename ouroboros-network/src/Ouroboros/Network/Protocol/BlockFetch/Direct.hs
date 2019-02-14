{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ouroboros.Network.Protocol.BlockFetch.Direct
  ( direct
  ) where

import Control.Monad (join)

import Ouroboros.Network.Protocol.BlockFetch.Client
import Ouroboros.Network.Protocol.BlockFetch.Server

-- | Run @'BlockFetchClient'@ and @'BlockFetchServer'@ directly against each
-- other.  This includes running it in any pure monad (e.g. @'Identity'@), and
-- return the result of client and the server.
--
direct
  :: forall header body m a b.
     Monad m
  => BlockFetchClient header body m a
  -> BlockFetchServer header body m b
  -> m (a, b)
direct (BlockFetchClient mclient) server = mclient >>= \client -> direct' client server

direct'
  :: forall header body m a b.
     Monad m
  => BlockFetchRequest header body m a
  -> BlockFetchServer header body m b
  -> m (a, b)
direct' (SendMsgClientDone a) (BlockFetchServer _requestHandler b) = return (a, b)
direct' (SendMsgRequestRange range resp client) (BlockFetchServer requestHandler _b) =
  requestHandler range >>= sendBatch resp
 where
  sendBatch
    :: BlockFetchResponse header body m a
    -> BlockFetchBlockSender header body m b
    -> m (a, b)
  sendBatch BlockFetchResponse {handleStartBatch} (SendMsgStartBatch mblock ) =
    join $ sendBlocks <$> handleStartBatch <*> mblock
  sendBatch BlockFetchResponse {handleNoBlocks} (SendMsgNoBlocks mserver) =
    handleNoBlocks >> mserver >>= direct client

  sendBlocks
    :: BlockFetchReceiver header body m
    -> BlockFetchSendBlocks header body m b
    -> m (a, b)
  sendBlocks BlockFetchReceiver {handleBlock} (SendMsgBlock body mblock) =
    join $ sendBlocks <$> handleBlock body <*> mblock
  sendBlocks BlockFetchReceiver {handleBatchDone} (SendMsgBatchDone mserver) =
    handleBatchDone >> mserver >>= direct client

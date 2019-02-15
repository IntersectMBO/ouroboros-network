{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ouroboros.Network.Protocol.BlockFetch.Direct
  ( direct
  ) where

import           Control.Monad (join)

import           Network.TypedProtocol.Pipelined
import           Network.TypedProtocol.Proofs

import           Ouroboros.Network.Protocol.BlockFetch.Client
import           Ouroboros.Network.Protocol.BlockFetch.Server


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
direct (BlockFetchClient mclient) server = mclient >>= flip go server
 where
  go :: BlockFetchRequest header body m a
     -> BlockFetchServer header body m b
     -> m (a, b)

  go (SendMsgRequestRange range resp client) (BlockFetchServer requestHandler _b) =
    requestHandler range >>= sendBatch client resp

  go (SendMsgClientDone a) (BlockFetchServer _requestHandler b) = return (a, b)


  sendBatch
    :: BlockFetchClient header body m a
    -> BlockFetchResponse header body m a
    -> BlockFetchBlockSender header body m b
    -> m (a, b)
  sendBatch client BlockFetchResponse {handleStartBatch} (SendMsgStartBatch mblock ) =
    join $ sendBlocks client <$> handleStartBatch <*> mblock

  sendBatch client BlockFetchResponse {handleNoBlocks} (SendMsgNoBlocks mserver) =
    handleNoBlocks >> mserver >>= direct client


  sendBlocks
    :: BlockFetchClient header body m a
    -> BlockFetchReceiver header body m
    -> BlockFetchSendBlocks header body m b
    -> m (a, b)

  sendBlocks client BlockFetchReceiver {handleBlock} (SendMsgBlock body mblock) =
    join $ sendBlocks client <$> handleBlock body <*> mblock

  sendBlocks client BlockFetchReceiver {handleBatchDone} (SendMsgBatchDone mserver) =
    handleBatchDone >> mserver >>= direct client

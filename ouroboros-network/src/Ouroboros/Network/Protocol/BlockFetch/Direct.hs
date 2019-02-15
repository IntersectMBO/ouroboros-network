{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes          #-}
module Ouroboros.Network.Protocol.BlockFetch.Direct
  ( direct
  , directPipelined
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


-- | Run a pipelined client against a server, directly, and return the result of
-- both client and the server.
--
directPipelined
  :: forall header body m a b. Monad m
  => BlockFetchClientPipelined header body m a
  -> BlockFetchServer header body m b
  -> m (a, b)
directPipelined (BlockFetchClientPipelined client0) server =
  go EmptyQ client0 server
 where
  go :: Queue n c
     -> BlockFetchSender n c header body m a
     -> BlockFetchServer     header body m b
     -> m (a, b)

  go q (SendMsgRequestRangePipelined range c receive next) (BlockFetchServer requestHandler _) =
    requestHandler range >>= sendBatch q c receive next

  go (ConsQ c q) (CollectBlocksPipelined _ k) srv = 
    go q (k c) srv

  go EmptyQ (SendMsgDonePipelined a) (BlockFetchServer _ b) =
    return (a, b)


  -- The server is will send a batch of block bodies.  At this point the
  -- @'BlockFetchSender'@ is a head of the queue.  After sending all blocks the
  -- client will enqueue the computed result @c@, which will match the @n@
  -- parameter back again.
  sendBatch
    :: Queue n c
    -> c
    -> (Maybe body -> c -> m c)
    -> BlockFetchSender (S n) c header body m a
    -> BlockFetchBlockSender    header body m b
    -> m (a, b)
  sendBatch q c  receive client (SendMsgStartBatch next) =
    next >>= sendBlocks q c receive client

  sendBatch q c _receive client (SendMsgNoBlocks next) =
    next >>= go (enqueue c q) client


  -- Loop through received block bodies until we are done. At each step update
  -- @c@ using the @receive@ function and enqueue it when we received all block
  -- bodies.
  sendBlocks
    :: Queue n c
    -> c
    -> (Maybe body -> c -> m c)
    -> BlockFetchSender (S n) c header body m a
    -> BlockFetchSendBlocks     header body m b
    -> m (a, b)

  sendBlocks q c receive  client (SendMsgBlock b next) = do
    c' <- receive (Just b) c
    next >>= sendBlocks q c' receive client

  sendBlocks q c _receive client (SendMsgBatchDone next) =
    -- after receiving all the block bodies, we calculated the final value of
    -- @c@ which we can enqueue now
    next >>= go (enqueue c q) client

{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.Protocol.BlockFetch.Server where

import Network.TypedProtocol.Core
import Network.TypedProtocol.Peer.Server

import Ouroboros.Network.Protocol.BlockFetch.Type


data BlockFetchServer block point m a where
  BlockFetchServer
    :: (ChainRange point -> m (BlockFetchBlockSender block point m a))
    -> a
    -> BlockFetchServer block point m a

-- | Send batches of blocks, when a batch is sent loop using
-- @'BlockFetchServer'@.
--
data BlockFetchBlockSender block point m a where

  -- | Initiate a batch of blocks.
  SendMsgStartBatch
    :: m (BlockFetchSendBlocks block point m a)
    -> BlockFetchBlockSender block point m a

  SendMsgNoBlocks
    :: m (BlockFetchServer block point m a)
    -> BlockFetchBlockSender block point m a

-- | Stream batch of blocks
--
data BlockFetchSendBlocks block point m a where

  -- | Send a single block and recurse.
  --
  SendMsgBlock
    :: block
    -> m (BlockFetchSendBlocks block point m a)
    -> BlockFetchSendBlocks block point m a

  -- | End of the stream of block bodies.
  --
  SendMsgBatchDone
    :: m (BlockFetchServer block point m a)
    -> BlockFetchSendBlocks block point m a

blockFetchServerPeer
  :: forall block point m a.
     Functor m
  => BlockFetchServer block point m a
  -> Server (BlockFetch block point) NonPipelined BFIdle m a
blockFetchServerPeer (BlockFetchServer requestHandler result) =
    Await \case
      MsgRequestRange range -> Effect $ sendBatch <$> requestHandler range
      MsgClientDone         -> Done result
 where
  sendBatch
    :: BlockFetchBlockSender block point m a
    -> Server (BlockFetch block point) NonPipelined BFBusy m a

  sendBatch (SendMsgStartBatch mblocks) =
    Yield MsgStartBatch $
    Effect $
      sendBlocks <$> mblocks

  sendBatch (SendMsgNoBlocks next) =
    Yield MsgNoBlocks $
    Effect $
      blockFetchServerPeer <$> next


  sendBlocks
    :: BlockFetchSendBlocks block point m a
    -> Server (BlockFetch block point) NonPipelined BFStreaming m a

  sendBlocks (SendMsgBlock block next') =
    Yield (MsgBlock block) $
    Effect $
      sendBlocks <$> next'

  sendBlocks (SendMsgBatchDone next) =
    Yield MsgBatchDone $
    Effect $
      blockFetchServerPeer <$> next

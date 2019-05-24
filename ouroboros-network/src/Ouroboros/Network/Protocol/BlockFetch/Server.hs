{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ouroboros.Network.Protocol.BlockFetch.Server where

import Network.TypedProtocol.Core
        ( Peer (..)
        , PeerRole (..)
        , PeerHasAgency (..)
        )

import Ouroboros.Network.Protocol.BlockFetch.Type


data BlockFetchServer block m a where
  BlockFetchServer
    :: (ChainRange block -> m (BlockFetchBlockSender block m a))
    -> a
    -> BlockFetchServer block m a

-- | Send batches of blocks, when a batch is sent loop using
-- @'BlockFetchServer'@.
--
data BlockFetchBlockSender block m a where

  -- | Initiate a batch of blocks.
  SendMsgStartBatch
    :: m (BlockFetchSendBlocks block m a)
    -> BlockFetchBlockSender block m a

  SendMsgNoBlocks
    :: m (BlockFetchServer block m a)
    -> BlockFetchBlockSender block m a

-- | Stream batch of blocks
--
data BlockFetchSendBlocks block m a where

  -- | Send a single block and recurse.
  --
  SendMsgBlock
    :: block
    -> m (BlockFetchSendBlocks block m a)
    -> BlockFetchSendBlocks block m a

  -- | End of the stream of block bodies.
  --
  SendMsgBatchDone
    :: m (BlockFetchServer block m a)
    -> BlockFetchSendBlocks block m a

blockFetchServerPeer
  :: forall block m a.
     Functor m
  => BlockFetchServer block m a
  -> Peer (BlockFetch block) AsServer BFIdle m a
blockFetchServerPeer (BlockFetchServer requestHandler result) =
    Await (ClientAgency TokIdle) $ \msg -> case msg of
      MsgRequestRange range -> Effect $ sendBatch <$> requestHandler range
      MsgClientDone         -> Done TokDone result
 where
  sendBatch
    :: BlockFetchBlockSender block m a
    -> Peer (BlockFetch block) AsServer BFBusy m a

  sendBatch (SendMsgStartBatch mblocks) =
    Yield (ServerAgency TokBusy) MsgStartBatch $
    Effect $
      sendBlocks <$> mblocks

  sendBatch (SendMsgNoBlocks next) =
    Yield (ServerAgency TokBusy) MsgNoBlocks $
    Effect $
      blockFetchServerPeer <$> next


  sendBlocks
    :: BlockFetchSendBlocks block m a
    -> Peer (BlockFetch block) AsServer BFStreaming m a

  sendBlocks (SendMsgBlock block next') =
    Yield (ServerAgency TokStreaming) (MsgBlock block) $
    Effect $
      sendBlocks <$> next'

  sendBlocks (SendMsgBatchDone next) =
    Yield (ServerAgency TokStreaming) MsgBatchDone $
    Effect $
      blockFetchServerPeer <$> next

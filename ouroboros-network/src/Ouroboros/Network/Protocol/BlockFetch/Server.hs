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


data BlockFetchServer header body m a where
  BlockFetchServer
    :: (ChainRange header -> m (BlockFetchBlockSender header body m a))
    -> a
    -> BlockFetchServer header body m a

-- | Send batches of blocks, when a batch is sent loop using
-- @'BlockFetchServer'@.
--
data BlockFetchBlockSender header body m a where

  -- | Initiate a batch of blocks.
  SendMsgStartBatch
    :: m (BlockFetchSendBlocks header body m a)
    -> BlockFetchBlockSender header body m a

  SendMsgNoBlocks
    :: m (BlockFetchServer header body m a)
    -> BlockFetchBlockSender header body m a

-- | Stream batch of blocks
--
data BlockFetchSendBlocks header body m a where

  -- | Send a single block body and recurse.
  --
  SendMsgBlock
    :: body
    -> m (BlockFetchSendBlocks header body m a)
    -> BlockFetchSendBlocks header body m a

  -- | End of the stream of block bodies.
  --
  SendMsgBatchDone
    :: m (BlockFetchServer header body m a)
    -> BlockFetchSendBlocks header body m a

blockFetchServerPeer
  :: forall header body m a.
     Functor m
  => BlockFetchServer header body m a
  -> Peer (BlockFetch header body) AsServer BFIdle m a
blockFetchServerPeer (BlockFetchServer requestHandler result) = Await (ClientAgency TokIdle) $ \msg -> case msg of
  MsgRequestRange range -> Effect $ sendBatch <$> requestHandler range
  MsgClientDone         -> Done TokDone result
 where
  sendBatch
    :: BlockFetchBlockSender header body m a
    -> Peer (BlockFetch header body) AsServer BFBusy m a

  sendBatch (SendMsgStartBatch mblocks) =
    Yield (ServerAgency TokBusy) MsgStartBatch (Effect $ sendBlocks <$> mblocks)

  sendBatch (SendMsgNoBlocks next) =
    Yield (ServerAgency TokBusy) MsgNoBlocks (Effect $ blockFetchServerPeer <$> next)


  sendBlocks
    :: BlockFetchSendBlocks header body m a
    -> Peer (BlockFetch header body) AsServer BFStreaming m a

  sendBlocks (SendMsgBlock body next') =
    Yield (ServerAgency TokStreaming) (MsgBlock body) (Effect $ sendBlocks <$> next')

  sendBlocks (SendMsgBatchDone next) =
    Yield (ServerAgency TokStreaming) MsgBatchDone (Effect $ blockFetchServerPeer <$> next)

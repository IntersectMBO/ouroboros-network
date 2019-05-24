{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ouroboros.Network.Protocol.BlockFetch.Client where

import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Pipelined

import           Ouroboros.Network.Block (StandardHash)
import           Ouroboros.Network.Protocol.BlockFetch.Type


-- | Block fetch client type for requesting ranges of blocks and handling
-- responses.
--
newtype BlockFetchClient block m a = BlockFetchClient {
    runBlockFetchClient :: m (BlockFetchRequest block m a)
  }

data BlockFetchRequest block m a where
  -- | Request a chain range, supply handler for incoming blocks and
  -- a continuation.
  --
  SendMsgRequestRange
    :: ChainRange block
    -> BlockFetchResponse block m a
    -> BlockFetchClient   block m a
    -> BlockFetchRequest  block m a

  -- | Client terminating the block-fetch protocol.
  SendMsgClientDone
    :: a
    -> BlockFetchRequest block m a

data BlockFetchResponse block m a = BlockFetchResponse {
    handleStartBatch :: m (BlockFetchReceiver block m),
    handleNoBlocks   :: m ()
  }

-- | Block are streamed and block receiver will handle each one when it comes,
-- it also needs to handle errors send back from the server.
--
data BlockFetchReceiver block m = BlockFetchReceiver {
    handleBlock      :: block -> m (BlockFetchReceiver block m),
    handleBatchDone  :: m ()
  }

blockFetchClientPeer
  :: forall block m a.
     ( StandardHash block
     , Monad m
     )
  => BlockFetchClient block m a
  -> Peer (BlockFetch block) AsClient BFIdle m a
blockFetchClientPeer (BlockFetchClient mclient) =
  Effect $ blockFetchRequestPeer <$> mclient
 where
  blockFetchRequestPeer
    :: BlockFetchRequest block m a
    -> Peer (BlockFetch block) AsClient BFIdle m a

  blockFetchRequestPeer (SendMsgClientDone result) =
    Yield (ClientAgency TokIdle) MsgClientDone (Done TokDone result)

  blockFetchRequestPeer (SendMsgRequestRange range resp next) =
    Yield
      (ClientAgency TokIdle)
      (MsgRequestRange range)
      (blockFetchResponsePeer next resp)


  blockFetchResponsePeer
    :: BlockFetchClient block m a
    -> BlockFetchResponse block m a
    -> Peer (BlockFetch block) AsClient BFBusy m a
  blockFetchResponsePeer next BlockFetchResponse{handleNoBlocks, handleStartBatch} =
    Await (ServerAgency TokBusy) $ \msg -> case msg of
      MsgStartBatch -> Effect $ blockReceiver next <$> handleStartBatch
      MsgNoBlocks   -> Effect $ handleNoBlocks >> (blockFetchRequestPeer <$> runBlockFetchClient next)

  blockReceiver
    :: BlockFetchClient block m a
    -> BlockFetchReceiver block m
    -> Peer (BlockFetch block) AsClient BFStreaming m a
  blockReceiver next BlockFetchReceiver{handleBlock, handleBatchDone} =
    Await (ServerAgency TokStreaming) $ \msg -> case msg of
      MsgBlock block -> Effect $ blockReceiver next <$> handleBlock block
      MsgBatchDone   -> Effect $ do
        handleBatchDone
        blockFetchRequestPeer <$> runBlockFetchClient next

--
-- Pipelined client
--

-- | A BlockFetch client designed for running the protcol in a pipelined way.
--
data BlockFetchClientPipelined block m a where
   -- | A 'BlockFetchSender', but starting with zero outstanding pipelined
   -- responses, and for any internal collect type @c@.
   BlockFetchClientPipelined
     :: BlockFetchSender      Z c block m a
     -> BlockFetchClientPipelined block m a

-- | A 'BlockFetchSender' with @n@ outstanding stream of block bodies.
--
data BlockFetchSender n c block m a where

  -- | Send a `MsgRequestRange` but do not wait for response.  Supply a monadic
  -- action which runs on each received block and which updates the internal
  -- received value @c@.  @c@ could be a Monoid, though it's more genral this
  -- way.
  --
  SendMsgRequestRangePipelined
    :: ChainRange block
    -> c
    -> (Maybe block -> c -> m c)
    -> BlockFetchSender (S n) c block m a
    -> BlockFetchSender    n  c block m a

  -- | Collect the result of a previous pipelined receive action
  --
  CollectBlocksPipelined
    :: Maybe (BlockFetchSender (S n) c block m a)
    -> (c ->  BlockFetchSender    n  c block m a)
    ->        BlockFetchSender (S n) c block m a

  -- | Termination of the block-fetch protocol.
  SendMsgDonePipelined
    :: a -> BlockFetchSender Z c block m a

blockFetchClientPeerPipelined
  :: forall block m a.
     Monad m
  => BlockFetchClientPipelined block m a
  -> PeerPipelined (BlockFetch block) AsClient BFIdle m a
blockFetchClientPeerPipelined (BlockFetchClientPipelined sender) =
  PeerPipelined (blockFetchClientPeerSender sender)

blockFetchClientPeerSender
  :: forall n block c m a.
     Monad m
  => BlockFetchSender n c block m a
  -> PeerSender (BlockFetch block) AsClient BFIdle n c m a

blockFetchClientPeerSender (SendMsgDonePipelined result) =
  -- Send `MsgClientDone` and complete the protocol
  SenderYield
    (ClientAgency TokIdle)
    MsgClientDone
      (SenderDone TokDone result)

blockFetchClientPeerSender (SendMsgRequestRangePipelined range c0 receive next) =
  -- Pipelined yield: send `MsgRequestRange`, return receicer which will
  -- consume a stream of blocks.
  SenderPipeline
    (ClientAgency TokIdle)
    (MsgRequestRange range)
    (ReceiverAwait (ServerAgency TokBusy) $ \msg -> case msg of
      MsgStartBatch -> receiveBlocks c0
      MsgNoBlocks   -> ReceiverDone c0)
    (blockFetchClientPeerSender next)
 where
  receiveBlocks
    :: c
    -> PeerReceiver (BlockFetch block) AsClient BFStreaming BFIdle m c
  receiveBlocks c = ReceiverAwait (ServerAgency TokStreaming) $ \msg -> case msg of
    -- received a block, run an acction and compute the result
    MsgBlock block -> ReceiverEffect $ do
      c' <- receive (Just block) c
      return $ receiveBlocks c'
    MsgBatchDone  -> ReceiverDone c

blockFetchClientPeerSender (CollectBlocksPipelined mNone collect) =
  SenderCollect
    (fmap blockFetchClientPeerSender mNone)
    (blockFetchClientPeerSender . collect)

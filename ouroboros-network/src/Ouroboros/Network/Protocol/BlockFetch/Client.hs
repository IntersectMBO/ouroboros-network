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
newtype BlockFetchClient header block m a = BlockFetchClient {
    runBlockFetchClient :: m (BlockFetchRequest header block m a)
  }

data BlockFetchRequest header block m a where
  -- | Request a chain range, supply handler for incoming blocks and
  -- a continuation.
  --
  SendMsgRequestRange
    :: ChainRange header
    -> BlockFetchResponse header block m a
    -> BlockFetchClient header block m a
    -> BlockFetchRequest header block m a

  -- | Client terminating the block-fetch protocol.
  SendMsgClientDone
    :: a
    -> BlockFetchRequest header block m a

data BlockFetchResponse header block m a = BlockFetchResponse {
    handleStartBatch :: m (BlockFetchReceiver header block m),
    handleNoBlocks   :: m ()
  }

-- | Block are streamed and block receiver will handle each one when it comes,
-- it also needs to handle errors send back from the server.
--
data BlockFetchReceiver header block m = BlockFetchReceiver {
    handleBlock      :: block -> m (BlockFetchReceiver header block m),
    handleBatchDone  :: m ()
  }

blockFetchClientPeer
  :: forall header body m a.
     ( StandardHash header
     , Monad m
     )
  => BlockFetchClient header body m a
  -> Peer (BlockFetch header body) AsClient BFIdle m a
blockFetchClientPeer (BlockFetchClient mclient) =
  Effect $ blockFetchRequestPeer <$> mclient
 where
  blockFetchRequestPeer
    :: BlockFetchRequest header body m a
    -> Peer (BlockFetch header body) AsClient BFIdle m a

  blockFetchRequestPeer (SendMsgClientDone result) =
    Yield (ClientAgency TokIdle) MsgClientDone (Done TokDone result)

  blockFetchRequestPeer (SendMsgRequestRange range resp next) =
    Yield
      (ClientAgency TokIdle)
      (MsgRequestRange range)
      (blockFetchResponsePeer next resp)


  blockFetchResponsePeer
    :: BlockFetchClient header body m a
    -> BlockFetchResponse header body m a
    -> Peer (BlockFetch header body) AsClient BFBusy m a
  blockFetchResponsePeer next BlockFetchResponse{handleNoBlocks, handleStartBatch} =
    Await (ServerAgency TokBusy) $ \msg -> case msg of
      MsgStartBatch -> Effect $ blockReceiver next <$> handleStartBatch
      MsgNoBlocks   -> Effect $ handleNoBlocks >> (blockFetchRequestPeer <$> runBlockFetchClient next)

  blockReceiver
    :: BlockFetchClient header body m a
    -> BlockFetchReceiver header body m
    -> Peer (BlockFetch header body) AsClient BFStreaming m a
  blockReceiver next BlockFetchReceiver{handleBlock, handleBatchDone} =
    Await (ServerAgency TokStreaming) $ \msg -> case msg of
      MsgBlock body -> Effect $ blockReceiver next <$> handleBlock body
      MsgBatchDone  -> Effect $ do
        handleBatchDone
        blockFetchRequestPeer <$> runBlockFetchClient next

--
-- Pipelined client
--

-- | A BlockFetch client designed for running the protcol in a pipelined way.
--
data BlockFetchClientPipelined header block c m a where
  -- | A 'BlockFetchSender' which starts with not outstanding pipelined responses.
  --
  BlockFetchClientPipelined
    :: BlockFetchSender          Z header block c m a
    -> BlockFetchClientPipelined   header block c m a

-- | A 'BlockFetchSender' with @n@ outstanding stream of block bodies.
--
data BlockFetchSender n header body c m a where

  -- | Send a `MsgRequestRange` but do not wait for response.  Supply a monadic
  -- action which runs on each received body and which updates the internal
  -- received value @c@.  @c@ could be a Monoid, though it's more genral this
  -- way.
  --
  SendMsgRequestRangePipeliend
    :: ChainRange header
    -> c
    -> (Maybe body -> c -> m c)
    -> BlockFetchSender (S n) header body c m a
    -> BlockFetchSender    n  header body c m a

  -- | Collect the result of a previous pipelined receive action
  --
  CollectBlocksPipelined
    :: Maybe (BlockFetchSender (S n) header body c m a)
    -> (c ->  BlockFetchSender    n  header body c m a)
    ->        BlockFetchSender (S n) header body c m a

  -- | Termination of the block-fetch protocol.
  SendMsgDonePipelined
    :: a -> BlockFetchSender Z c header body m a


blockFetchClientPeerSender
  :: forall n header body c m a.
     Monad m
  => BlockFetchSender n header body c m a
  -> PeerSender (BlockFetch header body) AsClient BFIdle n c m a

blockFetchClientPeerSender (SendMsgDonePipelined result) =
  -- Send `MsgClientDone` and complete the protocol
  SenderYield
    (ClientAgency TokIdle)
    MsgClientDone
      (SenderDone TokDone result)

blockFetchClientPeerSender (SendMsgRequestRangePipeliend range c0 receive next) =
  -- Pipeliend yield: send `MsgRequestRange`, return receicer which will
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
    -> PeerReceiver (BlockFetch header body) AsClient BFStreaming BFIdle m c
  receiveBlocks c = ReceiverAwait (ServerAgency TokStreaming) $ \msg -> case msg of
    -- received a block, run an acction and compute the result
    MsgBlock body -> ReceiverEffect $ do
      c' <- receive (Just body) c
      return $ receiveBlocks c'
    MsgBatchDone  -> ReceiverDone c

blockFetchClientPeerSender (CollectBlocksPipelined mNone collect) =
  SenderCollect
    (fmap blockFetchClientPeerSender mNone)
    (blockFetchClientPeerSender . collect)

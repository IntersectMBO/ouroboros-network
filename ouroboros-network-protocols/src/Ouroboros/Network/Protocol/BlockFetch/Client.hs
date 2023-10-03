{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module Ouroboros.Network.Protocol.BlockFetch.Client where

import           Control.Monad.Class.MonadSTM (STM)

import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Peer.Client

import           Ouroboros.Network.Protocol.BlockFetch.Type


-- | Block fetch client type for requesting ranges of blocks and handling
-- responses.
--
newtype BlockFetchClient block point m a = BlockFetchClient {
    runBlockFetchClient :: m (BlockFetchRequest block point m a)
  }

data BlockFetchRequest block point m a where
  -- | Request a chain range, supply handler for incoming blocks and
  -- a continuation.
  --
  SendMsgRequestRange
    :: ChainRange point
    -> BlockFetchResponse block m a
    -> BlockFetchClient   block point m a
    -> BlockFetchRequest  block point m a

  -- | Client terminating the block-fetch protocol.
  SendMsgClientDone
    :: a
    -> BlockFetchRequest block point m a

data BlockFetchResponse block m a = BlockFetchResponse {
    handleStartBatch :: m (BlockFetchReceiver block m),
    handleNoBlocks   :: m ()
  }

-- | Blocks are streamed and block receiver will handle each one when it comes,
-- it also needs to handle errors sent back from the server.
--
data BlockFetchReceiver block m = BlockFetchReceiver {
    handleBlock     :: block -> m (BlockFetchReceiver block m),
    handleBatchDone :: m ()
  }

blockFetchClientPeer
  :: forall block point m a.
     Monad m
  => BlockFetchClient block point m a
  -> Client (BlockFetch block point) NonPipelined Empty BFIdle m (STM m) a
blockFetchClientPeer (BlockFetchClient mclient) =
  Effect $ blockFetchRequestPeer <$> mclient
 where
  blockFetchRequestPeer
    :: BlockFetchRequest block point m a
    -> Client (BlockFetch block point) NonPipelined Empty BFIdle m (STM m) a

  blockFetchRequestPeer (SendMsgClientDone result) =
    Yield MsgClientDone (Done result)

  blockFetchRequestPeer (SendMsgRequestRange range resp next) =
    Yield
      (MsgRequestRange range)
      (blockFetchResponsePeer next resp)


  blockFetchResponsePeer
    :: BlockFetchClient block point m a
    -> BlockFetchResponse block m a
    -> Client (BlockFetch block point) NonPipelined Empty BFBusy m (STM m) a
  blockFetchResponsePeer next BlockFetchResponse{handleNoBlocks, handleStartBatch} =
    Await $ \msg -> case msg of
      MsgStartBatch -> Effect $ blockReceiver next <$> handleStartBatch
      MsgNoBlocks   -> Effect $ handleNoBlocks
                             >> blockFetchRequestPeer <$> runBlockFetchClient next

  blockReceiver
    :: BlockFetchClient block point m a
    -> BlockFetchReceiver block m
    -> Client (BlockFetch block point) NonPipelined Empty BFStreaming m (STM m) a
  blockReceiver next BlockFetchReceiver{handleBlock, handleBatchDone} =
    Await $ \msg -> case msg of
      MsgBlock block -> Effect $ blockReceiver next <$> handleBlock block
      MsgBatchDone   -> Effect $ handleBatchDone
                              >> blockFetchRequestPeer <$> runBlockFetchClient next

--
-- Pipelined client
--

type BFQueue block point = Queue (BlockFetch block point)

-- | A BlockFetch client designed for running the protocol in a pipelined way.
--
data BlockFetchClientPipelined block point m a where
   -- | A 'BlockFetchIdle', but starting with zero outstanding pipelined
   -- responses, and for any internal collect type @c@.
   BlockFetchClientPipelined
     :: m (BlockFetchIdle         block point   Empty m a)
     -> BlockFetchClientPipelined block point       m a


-- | A 'BlockFetchIdle' with @n@ outstanding stream of block bodies.
--
-- TODO: rename this type, it's not only sending but also collecting responses.
--
data BlockFetchIdle block point (q :: BFQueue block point) m a where

  -- | Send a `MsgRequestRange` but do not wait for response.  Supply a monadic
  -- action which runs on each received block and which updates the internal
  -- received value @c@.  @c@ could be a Monoid, though it's more general this
  -- way.
  --
  SendMsgRequestRangePipelined
    :: ChainRange point
    -> m (BlockFetchIdle block point (q |> Tr BFBusy BFIdle) m a)
    ->    BlockFetchIdle block point  q                      m a

  -- | Collect the result of a previous pipelined receive action
  --
  CollectStartBatch
    :: Maybe
         (m (BlockFetchIdle block point (Tr BFBusy      BFIdle <| q) m a))
    ->    m (BlockFetchIdle block point (Tr BFStreaming BFIdle <| q) m a)
    -- ^ continuation run when 'MsgStartBatch' was received
    ->    m (BlockFetchIdle block point                           q  m a)
    -- ^ continuation run when 'MsgNoBlocks' was received
    ->       BlockFetchIdle block point (Tr BFBusy      BFIdle <| q) m a

  CollectBlock
    :: Maybe
         (m (BlockFetchIdle block point (Tr BFStreaming BFIdle <| q) m a))
    -> ( block ->
          m (BlockFetchIdle block point (Tr BFStreaming BFIdle <| q) m a))
       -- ^ continuation run when a 'MsgBlock' was received
    ->    m (BlockFetchIdle block point                           q  m a)
       -- ^ continuation run when 'MsgBatchDone' was received (all blocks were
       -- received)
    ->       BlockFetchIdle block point (Tr BFStreaming BFIdle <| q) m a

  CollectStartBatchSTM
    :: STM m
          (m (BlockFetchIdle block point (Tr BFBusy      BFIdle <| q) m a))
    ->     m (BlockFetchIdle block point (Tr BFStreaming BFIdle <| q) m a)
    -- ^ continuation run when 'MsgStartBatch' was received
    ->     m (BlockFetchIdle block point                           q  m a)
    -- ^ continuation run when 'MsgNoBlocks' was received
    ->        BlockFetchIdle block point (Tr BFBusy      BFIdle <| q) m a

  CollectBlockSTM
    :: STM m
          (m (BlockFetchIdle block point (Tr BFStreaming BFIdle <| q) m a))
    -> ( block ->
           m (BlockFetchIdle block point (Tr BFStreaming BFIdle <| q) m a))
       -- ^ continuation run when a 'MsgBlock' was received
    ->     m (BlockFetchIdle block point                           q  m a)
       -- ^ continuation run when 'MsgBatchDone' was received (all blocks were
       -- received)
    ->        BlockFetchIdle block point (Tr BFStreaming BFIdle <| q) m a


  -- | Termination of the block-fetch protocol.
  SendMsgDonePipelined
    :: a -> BlockFetchIdle block point Empty m a

blockFetchClientPeerPipelined
  :: forall block point m a.
     ( Monad m, Functor (STM m) )
  => BlockFetchClientPipelined block point m a
  -> Client (BlockFetch block point) 'Pipelined Empty BFIdle m (STM m) a
blockFetchClientPeerPipelined (BlockFetchClientPipelined sender) =
  Effect (blockFetchClientPeerSender <$> sender)

blockFetchClientPeerSender
  :: forall block point (q :: BFQueue block point) m a.
     ( Monad m, Functor (STM m) )
  => BlockFetchIdle block point q m a
  -> Client (BlockFetch block point) 'Pipelined q BFIdle m (STM m) a

blockFetchClientPeerSender (SendMsgDonePipelined result) =
  -- Send `MsgClientDone` and complete the protocol
  Yield
    MsgClientDone
    (Done result)

blockFetchClientPeerSender (SendMsgRequestRangePipelined range next) =
  YieldPipelined
    (MsgRequestRange range)
    (Effect $ blockFetchClientPeerSender <$> next)

blockFetchClientPeerSender (CollectStartBatch mNone kStartBatch kNoBlocks) =
  Collect
    (Effect . fmap blockFetchClientPeerSender <$> mNone)
    (\msg -> case msg of
      MsgStartBatch -> Effect (blockFetchClientPeerSender <$> kStartBatch)
      MsgNoBlocks   -> CollectDone
                     $ Effect (blockFetchClientPeerSender <$> kNoBlocks)
    )

blockFetchClientPeerSender (CollectBlock mNone kBlock kBatchDone) =
  Collect
    (Effect . fmap blockFetchClientPeerSender <$> mNone)
    (\msg -> case msg of
      MsgBlock block -> Effect (blockFetchClientPeerSender <$> kBlock block)
      MsgBatchDone   -> CollectDone
                      $ Effect (blockFetchClientPeerSender <$> kBatchDone)
    )

blockFetchClientPeerSender (CollectStartBatchSTM stmNone kStartBatch kNoBlocks) =
  CollectSTM
    (Effect . fmap blockFetchClientPeerSender <$> stmNone)
    (\msg -> case msg of
      MsgStartBatch -> Effect (blockFetchClientPeerSender <$> kStartBatch)
      MsgNoBlocks   -> CollectDone
                     $ Effect (blockFetchClientPeerSender <$> kNoBlocks)
    )

blockFetchClientPeerSender (CollectBlockSTM stmNone kBlock kBatchDone) =
  CollectSTM
    (Effect . fmap blockFetchClientPeerSender <$> stmNone)
    (\msg -> case msg of
      MsgBlock block -> Effect (blockFetchClientPeerSender <$> kBlock block)
      MsgBatchDone   -> CollectDone
                      $ Effect (blockFetchClientPeerSender <$> kBatchDone)
    )

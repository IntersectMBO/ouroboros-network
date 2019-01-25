{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ouroboros.Network.Protocol.BlockFetch.Client where

import           Network.TypedProtocol.Core

import           Ouroboros.Network.Block (StandardHash)
import           Ouroboros.Network.Protocol.BlockFetch.Type


-- | Block fetch client type for requesting ranges of blocks and handling
-- responses.
--
newtype BlockFetchClient header block m a = BlockFetchClient {
    runBlockFetchClient :: m (BlockFetchRequest header block m a)
  }

data BlockFetchRequest header block m a where
  -- | Request a chain range, supply handler for incoming blocks and possibly
  -- a continuation.
  --
  SendMsgRequestRange
    :: ChainRange header
    -> BlockFetchResponse header block m a
    -> BlockFetchClient header block m a
    -> BlockFetchRequest header block m a

  -- | Client terminating the protocol.
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

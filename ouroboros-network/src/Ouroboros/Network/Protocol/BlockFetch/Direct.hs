{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.Protocol.BlockFetch.Direct where

import Ouroboros.Network.Protocol.BlockFetch.Client
import Ouroboros.Network.Protocol.BlockFetch.Server

-- | Directly connect server and client adts of @'BlockFetchClientProtocol'@.
--
directBlockFetchClient
  :: Monad m
  => BlockFetchServerReceiver header m a
  -- ^ server of the @'BlockFetchClientProtocol'@ protocol
  -> BlockFetchClientSender header m b
  -- ^ client of the @'BlockFetchClientProtocol'@ protocol
  -> m (a, b)
directBlockFetchClient BlockFetchServerReceiver {recvMessageRequestRange} (SendBlockRequestMsg range msender) = do
  server <- recvMessageRequestRange range
  sender <- msender
  directBlockFetchClient server sender
directBlockFetchClient BlockFetchServerReceiver {recvMessageDone} (SendMsgDone b) = return (recvMessageDone, b)
       
-- | Direclty connect server and client adts of @'BlockFetchServerProtocol'@.
--
directBlockFetchServer
  :: forall header block m a b.
     Monad m
  => BlockFetchServer header block m a
  -- ^ server of the @'BlockFetchServerProtocol'@ protocol
  -> BlockFetchClientReceiver block m b
  -- ^ client of the @'BlockFetchServerProtocol'@ protocol
  -> m (a, b)
directBlockFetchServer server client = do
  sender <- runBlockFetchServer server
  directSender sender client
 where
  directSender
    :: BlockFetchSender header block m a
    -> BlockFetchClientReceiver block m b
    -> m (a, b)
  directSender (SendMessageStartBatch mBlockStream) BlockFetchClientReceiver {recvMsgStartBatch} = do
    blockStream <- mBlockStream
    receiveBlocks <- recvMsgStartBatch
    directBlocks blockStream receiveBlocks
  directSender (SendMessageNoBlocks server') BlockFetchClientReceiver {recvMsgNoBlocks} = do
    client' <- recvMsgNoBlocks
    directBlockFetchServer server' client'
  directSender (SendMessageDone a) BlockFetchClientReceiver {recvMsgDoneClient} = return (a, recvMsgDoneClient)

  directBlocks
    :: BlockFetchSendBlocks header block m a
    -> BlockFetchClientReceiveBlocks block m b
    -> m (a, b)
  directBlocks (SendMessageBlock block mBlockStream) BlockFetchClientReceiveBlocks {recvMsgBlock} = do
    blockStream   <- mBlockStream
    receiveBlocks <- recvMsgBlock block
    directBlocks blockStream receiveBlocks
  directBlocks (SendMessageBatchDone server') BlockFetchClientReceiveBlocks {recvMsgBatchDone} =
    recvMsgBatchDone >>= directBlockFetchServer server'

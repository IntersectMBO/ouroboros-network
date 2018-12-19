{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Ouroboros.Network.Protocol.BlockFetch.Direct where

import Ouroboros.Network.Protocol.BlockFetch.Client
import Ouroboros.Network.Protocol.BlockFetch.Server

-- | Directly connect server and client adts of @'BlockFetchClientProtocol'@.
--
directBlockFetchClient
  :: Monad m
  => BlockFetchServerReceiver range m a
  -- ^ server of the @'BlockFetchClientProtocol'@ protocol
  -> BlockFetchClientSender range m b
  -- ^ client of the @'BlockFetchClientProtocol'@ protocol
  -> m (a, b)
directBlockFetchClient BlockFetchServerReceiver {recvMessageRequestRange,recvMessageDone} (BlockFetchClientSender sender) =
  sender >>= request
 where
  request (BlockFetchClientRange range sender') = do
    server <- recvMessageRequestRange range
    directBlockFetchClient server sender'
  request (BlockFetchClientDone b) = (,b) <$> recvMessageDone

-- | Direclty connect server and client adts of @'BlockFetchServerProtocol'@.
--
directBlockFetchServer
  :: forall block m a b.
     Monad m
  => BlockFetchServerSender block m a
  -- ^ server of the @'BlockFetchServerProtocol'@ protocol
  -> BlockFetchClientReceiver block m b
  -- ^ client of the @'BlockFetchServerProtocol'@ protocol
  -> m (a, b)
directBlockFetchServer server client = do
  sender <- runBlockFetchServerSender server
  directSender sender client
 where
  directSender
    :: BlockFetchSender block m a
    -> BlockFetchClientReceiver block m b
    -> m (a, b)
  directSender (SendMessageStartBatch mBlockStream) BlockFetchClientReceiver {recvMsgStartBatch} = do
    blockStream <- mBlockStream
    receiveBlocks <- recvMsgStartBatch
    directBlocks blockStream receiveBlocks
  directSender (SendMessageNoBlocks server') BlockFetchClientReceiver {recvMsgNoBlocks} = do
    client' <- recvMsgNoBlocks
    directBlockFetchServer server' client'
  directSender (SendMessageServerDone a) BlockFetchClientReceiver {recvMsgDoneClient} = return (a, recvMsgDoneClient)

  directBlocks
    :: BlockFetchSendBlocks block m a
    -> BlockFetchClientReceiveBlocks block m b
    -> m (a, b)
  directBlocks (SendMessageBlock block mBlockStream) BlockFetchClientReceiveBlocks {recvMsgBlock} = do
    blockStream   <- mBlockStream
    receiveBlocks <- recvMsgBlock block
    directBlocks blockStream receiveBlocks
  directBlocks (SendMessageBatchDone server') BlockFetchClientReceiveBlocks {recvMsgBatchDone} =
    recvMsgBatchDone >>= directBlockFetchServer server'

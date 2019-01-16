{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Ouroboros.Network.Protocol.BlockFetch.Direct where

import Ouroboros.Network.Protocol.BlockFetch.Client
import Ouroboros.Network.Protocol.BlockFetch.Server

-- | Directly connect server and client adts of @'BlockRequestProtocol'@.
--
directBlockRequest
  :: Monad m
  => BlockRequestReceiver range m a
  -- ^ server of the @'BlockRequestProtocol'@ protocol
  -> BlockRequestSender range m b
  -- ^ client of the @'BlockRequestProtocol'@ protocol
  -> m (a, b)
directBlockRequest BlockRequestReceiver {recvMessageRequestRange,recvMessageDone} (BlockRequestSender sender) =
  sender >>= request
 where
  request (BlockFetchClientRange range sender') = do
    server <- recvMessageRequestRange range
    directBlockRequest server sender'
  request (BlockFetchClientDone b) = (,b) <$> recvMessageDone

-- | Direclty connect server and client adts of @'BlockFetchProtocol'@.
--
directBlockFetch
  :: forall block m a b.
     Monad m
  => BlockFetchServerSender block m a
  -- ^ server of the @'BlockFetchProtocol'@ protocol
  -> BlockFetchReceiver block m b
  -- ^ client of the @'BlockFetchProtocol'@ protocol
  -> m (a, b)
directBlockFetch server client = do
  sender <- runBlockFetchServerSender server
  directSender sender client
 where
  directSender
    :: BlockFetchSender block m a
    -> BlockFetchReceiver block m b
    -> m (a, b)
  directSender (SendMessageStartBatch mBlockStream) BlockFetchReceiver {recvMsgStartBatch} = do
    blockStream <- mBlockStream
    receiveBlocks <- recvMsgStartBatch
    directBlocks blockStream receiveBlocks
  directSender (SendMessageNoBlocks server') BlockFetchReceiver {recvMsgNoBlocks} = do
    client' <- recvMsgNoBlocks
    directBlockFetch server' client'
  directSender (SendMessageServerDone a) BlockFetchReceiver {recvMsgDoneClient} = return (a, recvMsgDoneClient)

  directBlocks
    :: BlockFetchSendBlocks block m a
    -> BlockFetchReceiveBlocks block m b
    -> m (a, b)
  directBlocks (SendMessageBlock block mBlockStream) BlockFetchReceiveBlocks {recvMsgBlock} = do
    blockStream   <- mBlockStream
    receiveBlocks <- recvMsgBlock block
    directBlocks blockStream receiveBlocks
  directBlocks (SendMessageBatchDone server') BlockFetchReceiveBlocks {recvMsgBatchDone} =
    recvMsgBatchDone >>= directBlockFetch server'

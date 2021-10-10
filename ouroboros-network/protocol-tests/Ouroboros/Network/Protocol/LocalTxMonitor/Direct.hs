{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.Protocol.LocalTxMonitor.Direct
  ( direct
  ) where

import           Control.Monad (join)

import           Ouroboros.Network.Protocol.LocalTxMonitor.Client
import           Ouroboros.Network.Protocol.LocalTxMonitor.Server

direct :: forall m txid tx slot a b.
          ( Monad m
          )
       => LocalTxMonitorClient txid tx slot m a
       -> LocalTxMonitorServer txid tx slot m b
       -> m (a, b)
direct (LocalTxMonitorClient mClient) (LocalTxMonitorServer mServer) = do
    join (directIdle <$> mServer <*> mClient)
  where
    directIdle ::
         ServerStIdle txid tx slot m b
      -> ClientStIdle txid tx slot m a
      -> m (a, b)
    directIdle ServerStIdle
      { recvMsgDone
      , recvMsgAcquire
      } = \case
        SendMsgDone a -> do
          b <- recvMsgDone
          pure (a, b)
        SendMsgAcquire mClientStAcquired -> do
          SendMsgAcquired slot serverStAcquired <- recvMsgAcquire
          clientStAcquired <- mClientStAcquired slot
          directAcquired serverStAcquired clientStAcquired

    directAcquired ::
         ServerStAcquired txid tx slot m b
      -> ClientStAcquired txid tx slot m a
      -> m (a, b)
    directAcquired ServerStAcquired
      { recvMsgRelease
      , recvMsgAwaitAcquire
      , recvMsgNextTx
      , recvMsgHasTx
      , recvMsgGetSizes
      } = \case
        SendMsgRelease mClientStIdle -> do
          serverStIdle <- recvMsgRelease
          clientStIdle <- mClientStIdle
          directIdle serverStIdle clientStIdle
        SendMsgAwaitAcquire mClientStAcquired -> do
          SendMsgAcquired slot serverStAcquired <- recvMsgAwaitAcquire
          clientStAcquired <- mClientStAcquired slot
          directAcquired serverStAcquired clientStAcquired
        SendMsgNextTx mClientStAcquired -> do
          recvMsgNextTx >>= \case
            SendMsgReplyNextTx result serverStAcquired -> do
              clientStAcquired <- mClientStAcquired result
              directAcquired serverStAcquired clientStAcquired
        SendMsgHasTx txid mClientStAcquired -> do
          recvMsgHasTx txid >>= \case
            SendMsgReplyHasTx result serverStAcquired -> do
              clientStAcquired <- mClientStAcquired result
              directAcquired serverStAcquired clientStAcquired
        SendMsgGetSizes mClientStAcquired -> do
          recvMsgGetSizes >>= \case
            SendMsgReplyGetSizes result serverStAcquired -> do
              clientStAcquired <- mClientStAcquired result
              directAcquired serverStAcquired clientStAcquired

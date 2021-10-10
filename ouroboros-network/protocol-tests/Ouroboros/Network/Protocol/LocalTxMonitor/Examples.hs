{-# LANGUAGE GADTs               #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.Protocol.LocalTxMonitor.Examples (
    localTxMonitorClient,
    localTxMonitorServer,
  ) where

import           Data.List (find)
import           Data.Maybe (isJust)

import           Ouroboros.Network.Protocol.LocalTxMonitor.Type
import           Ouroboros.Network.Protocol.LocalTxMonitor.Client
import           Ouroboros.Network.Protocol.LocalTxMonitor.Server

-- | An example client which acquire a snapshot from the server and fetches all transactions
-- from it, and check presence of each of these transactions.
--
localTxMonitorClient ::
     forall txid tx slot m.
     ( Applicative m
     )
  => (tx -> txid)
  -> LocalTxMonitorClient txid tx slot m ([(tx, Bool)], MempoolSizeAndCapacity)
localTxMonitorClient txId =
    LocalTxMonitorClient (pure clientStIdle)
  where
    clientStIdle ::
      ClientStIdle txid tx slot m ([(tx, Bool)], MempoolSizeAndCapacity)
    clientStIdle =
      SendMsgAcquire $ \_slot ->
        pure $ clientStAcquired []

    clientStAcquired ::
         [(tx, Bool)]
      -> ClientStAcquired txid tx slot m ([(tx, Bool)], MempoolSizeAndCapacity)
    clientStAcquired txs =
      SendMsgNextTx $ \case
        Nothing -> do
          pure $ SendMsgGetSizes $ \sizes ->
            pure $ SendMsgRelease $
              pure $ SendMsgDone (reverse txs, sizes)
        Just tx -> do
          pure $ SendMsgHasTx (txId tx) $ \result ->
            pure $ clientStAcquired ((tx, result):txs)

-- | An example server which streams predefined transactions to a client. The preset is the
-- only snapshot of the server, so acquiring/re-acquiring always yield the same transactions.
--
localTxMonitorServer ::
     forall txid tx slot m.
     ( Applicative m
     , Eq txid
     )
  => (tx -> txid)
  -> (slot, [tx])
  -> LocalTxMonitorServer txid tx slot m ()
localTxMonitorServer txId (slot, allTxs) =
    LocalTxMonitorServer (pure serverStIdle)
  where
    serverStIdle ::
      ServerStIdle txid tx slot m ()
    serverStIdle =
      ServerStIdle
        { recvMsgDone =
            pure ()
        , recvMsgAcquire =
            pure serverStAcquiring
        }

    serverStAcquiring ::
      ServerStAcquiring txid tx slot m ()
    serverStAcquiring =
      SendMsgAcquired slot (serverStAcquired allTxs)

    serverStAcquired ::
         [tx]
      -> ServerStAcquired txid tx slot m ()
    serverStAcquired txs =
      ServerStAcquired
      { recvMsgAwaitAcquire =
          pure serverStAcquiring
      , recvMsgRelease =
          pure serverStIdle
      , recvMsgNextTx =
          case txs of
            []    -> pure $ SendMsgReplyNextTx Nothing (serverStAcquired [])
            (h:q) -> pure $ SendMsgReplyNextTx (Just h) (serverStAcquired q)
      , recvMsgHasTx = \ix ->
          let result = isJust $ find ((== ix) . txId) allTxs
           in pure $ SendMsgReplyHasTx result (serverStAcquired txs)
      , recvMsgGetSizes =
          let sizes = MempoolSizeAndCapacity
                { capacityInBytes = fromIntegral (length allTxs)
                , sizeInBytes     = fromIntegral (length allTxs)
                , numberOfTxs     = fromIntegral (length allTxs)
                }
           in pure $ SendMsgReplyGetSizes sizes (serverStAcquired txs)
      }

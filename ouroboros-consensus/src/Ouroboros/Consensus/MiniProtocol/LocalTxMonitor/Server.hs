{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}

module Ouroboros.Consensus.MiniProtocol.LocalTxMonitor.Server (localTxMonitorServer) where

import           Ouroboros.Network.Protocol.LocalTxMonitor.Server
import           Ouroboros.Network.Protocol.LocalTxMonitor.Type

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Mempool.API
import           Ouroboros.Consensus.Util.IOLike

-- | Local transaction monitoring server, for inspecting the mempool.
--
localTxMonitorServer ::
     forall blk idx m.
     ( MonadSTM m
     , LedgerSupportsMempool blk
     , Eq idx
     )
  => Mempool m blk idx
  -> LocalTxMonitorServer (GenTxId blk) (GenTx blk) SlotNo m ()
localTxMonitorServer mempool =
    LocalTxMonitorServer (pure serverStIdle)
  where
    serverStIdle
      :: ServerStIdle (GenTxId blk) (GenTx blk) SlotNo m ()
    serverStIdle =
      ServerStIdle
      { recvMsgDone = do
          pure ()
      , recvMsgAcquire = do
          s <- atomically $ (,) <$> getCapacity mempool <*> getSnapshot mempool
          pure $ serverStAcquiring s
      }

    serverStAcquiring
      :: (MempoolCapacityBytes, MempoolSnapshot blk idx)
      -> ServerStAcquiring (GenTxId blk) (GenTx blk) SlotNo m ()
    serverStAcquiring s@(_, snapshot) =
      SendMsgAcquired (snapshotSlotNo snapshot) (serverStAcquired s (snapshotTxs snapshot))

    serverStAcquired
      :: (MempoolCapacityBytes, MempoolSnapshot blk idx)
      -> [(Validated (GenTx blk), idx)]
      -> ServerStAcquired (GenTxId blk) (GenTx blk) SlotNo m ()
    serverStAcquired s@(capacity, snapshot) txs =
      ServerStAcquired
      { recvMsgNextTx =
          case txs of
            []  ->
              pure $ SendMsgReplyNextTx Nothing (serverStAcquired s [])
            (txForgetValidated -> h, _):q ->
              pure $ SendMsgReplyNextTx (Just h) (serverStAcquired s q)
      , recvMsgHasTx = \txid ->
          pure $ SendMsgReplyHasTx (snapshotHasTx snapshot txid) (serverStAcquired s txs)
      , recvMsgGetSizes = do
          let MempoolSize{msNumTxs,msNumBytes} = snapshotMempoolSize snapshot
          let sizes = MempoolSizeAndCapacity
                { capacityInBytes = getMempoolCapacityBytes capacity
                , sizeInBytes     = msNumBytes
                , numberOfTxs     = msNumTxs
                }
          pure $ SendMsgReplyGetSizes sizes (serverStAcquired s txs)
      , recvMsgAwaitAcquire = do
          s' <- atomically $ do
            s'@(_, snapshot') <- (,) <$> getCapacity mempool <*> getSnapshot mempool
            s' <$ check (not (snapshot `isSameSnapshot` snapshot'))
          pure $ serverStAcquiring s'
      , recvMsgRelease =
          pure serverStIdle
      }

    -- Are two snapshots equal? (from the perspective of this protocol)
    isSameSnapshot
      :: MempoolSnapshot blk idx
      -> MempoolSnapshot blk idx
      -> Bool
    isSameSnapshot a b =
      (snd <$> snapshotTxs a) == (snd <$> snapshotTxs b)
      &&
      snapshotSlotNo a == snapshotSlotNo b

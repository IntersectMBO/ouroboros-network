{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}

module Ouroboros.Consensus.MiniProtocol.LocalTxMonitor.Server (localTxMonitorServer) where

import           Ouroboros.Network.Protocol.LocalTxMonitor.Server

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Mempool.API
import           Ouroboros.Consensus.Util.IOLike


-- | Local transaction monitoring server, for inspecting the mempool.
--
localTxMonitorServer
  :: forall blk idx m. (MonadSTM m, LedgerSupportsMempool blk)
  => Mempool m blk idx
  -> LocalTxMonitorServer (GenTxId blk) (GenTx blk) SlotNo m ()
localTxMonitorServer mempool =
    LocalTxMonitorServer (pure serverStIdle)
  where
    serverStIdle
      :: ServerStIdle (GenTxId blk) (GenTx blk) SlotNo m ()
    serverStIdle =
      ServerStIdle { recvMsgDone, recvMsgAcquire }

    serverStAcquiring
      :: MempoolSnapshot blk idx
      -> ServerStAcquiring (GenTxId blk) (GenTx blk) SlotNo m ()
    serverStAcquiring s =
      SendMsgAcquired (snapshotSlotNo s) (serverStAcquired s (snapshotTxs s))

    serverStAcquired
      :: MempoolSnapshot blk idx
      -> [(Validated (GenTx blk), idx)]
      -> ServerStAcquired (GenTxId blk) (GenTx blk) SlotNo m ()
    serverStAcquired s txs =
      ServerStAcquired
      { recvMsgNextTx =
          case txs of
            []  ->
              pure $ SendMsgReplyNextTx Nothing (serverStAcquired s [])
            (txForgetValidated -> h, _):q ->
              pure $ SendMsgReplyNextTx (Just h) (serverStAcquired s q)
      , recvMsgHasTx = \txid ->
          pure $ SendMsgReplyHasTx (snapshotHasTx s txid) (serverStAcquired s txs)
      , recvMsgReAcquire =
          recvMsgAcquire
      , recvMsgRelease =
          pure serverStIdle
      }

    recvMsgAcquire
      :: m (ServerStAcquiring (GenTxId blk) (GenTx blk) SlotNo m ())
    recvMsgAcquire = do
        s <- atomically (getSnapshot mempool)
        pure $ serverStAcquiring s

    recvMsgDone
      :: m ()
    recvMsgDone =
      pure ()

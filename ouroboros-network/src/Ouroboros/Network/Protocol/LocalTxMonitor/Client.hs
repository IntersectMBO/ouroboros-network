{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes          #-}


-- | A view of the transaction monitor protocol from the point of view of
-- the client.
--
-- This provides simple access to the local mempool snapshots, to allow building
-- more monitoring logic from the client side after submitting transactions.
--
-- For execution, 'localTxMonitorClientPeer' is provided for conversion
-- into the typed protocol.
--
module Ouroboros.Network.Protocol.LocalTxMonitor.Client (
    -- * Protocol type for the client
    -- | The protocol states from the point of view of the client.
    LocalTxMonitorClient(..)
  , ClientStIdle (..)
  , ClientStAcquired (..)

    -- * Execution as a typed protocol
  , localTxMonitorClientPeer
  ) where

import           Network.TypedProtocol.Core

import           Ouroboros.Network.Protocol.LocalTxMonitor.Type

-- | A tx monitor client, on top of some effect 'm'.
--
newtype LocalTxMonitorClient txid tx slot m a = LocalTxMonitorClient {
      runLocalTxMonitorClient :: m (ClientStIdle txid tx slot m a)
    }

-- | In the 'StIdle' protocol state, the client has agency and can proceed to acquire
-- a mempool snapshot, or end the protocol.
--
data ClientStIdle txid tx slot m a where

  -- | Send the 'MsgAcquire', with handlers for the replies.
  --
  -- This request cannot timeout and cannot fail, it'll acquire the latest mempool snapshot
  -- available on the server and hang on to it. This allows to run any subsequent queries
  -- against the same view of the mempool.
  --
  -- The snapshot is acquired for a particular slot number which materializes the 'virtual block'
  -- under construction.
  --
  SendMsgAcquire
    :: (slot -> m (ClientStAcquired txid tx slot m a))
    -> ClientStIdle txid tx slot m a

  -- | The client decided to end the protocol
  --
  SendMsgDone
    :: a
    -> ClientStIdle txid tx slot m a

-- | In the 'StAcquired' protocol state, the client has agency and can query the server against
-- the acquired snapshot. Alternatively, it can also (re)acquire a more recent snapshot.
--
data ClientStAcquired txid tx slot m a where
  -- | The mempool is modeled as an ordered list of transactions and thus, can be traversed
  -- linearly. 'MsgNextTx' requests the next transaction from the current list. This must be
  -- a transaction that was not previously sent to the client for this particular snapshot.
  --
  SendMsgNextTx
    :: (Maybe tx -> m (ClientStAcquired txid tx slot m a))
    -> ClientStAcquired txid tx slot m a

  -- | For some cases where clients do not wish to traverse the entire mempool but look for a
  -- specific transaction, they can assess the presence of such transaction directly.
  -- Note that, the absence of a transaction does not imply anything about how the transaction
  -- was processed: it may have been dropped, or inserted in a block. 'False' simply means that
  -- it is no longer in the mempool.
  --
  SendMsgHasTx
    :: txid
    -> (Bool -> m (ClientStAcquired txid tx slot m a))
    -> ClientStAcquired txid tx slot m a

  -- | Re-acquire a more recent snapshot, or the same one if nothing has changed.
  --
  SendMsgReAcquire
    :: (slot ->  m (ClientStAcquired txid tx slot m a))
    -> ClientStAcquired txid tx slot m a

  -- | Release the acquired snapshot, in order to loop back to the idle state.
  --
  SendMsgRelease
    :: m (ClientStIdle txid tx slot m a)
    -> ClientStAcquired txid tx slot m a

-- | Interpret a 'LocalTxMonitorClient' action sequence as a 'Peer' on the client
-- side of the 'LocalTxMonitor' protocol.
--
localTxMonitorClientPeer
  :: forall txid tx slot m a.
     Monad m
  => LocalTxMonitorClient txid tx slot m a
  -> Peer (LocalTxMonitor txid tx slot) AsClient StIdle m a
localTxMonitorClientPeer (LocalTxMonitorClient mClient) =
    Effect $ handleStIdle <$> mClient
  where
    handleStIdle
      :: ClientStIdle txid tx slot m a
      -> Peer (LocalTxMonitor txid tx slot) AsClient StIdle m a
    handleStIdle = \case
      SendMsgAcquire stAcquired ->
        Yield (ClientAgency TokIdle) MsgAcquire $
          Await (ServerAgency TokAcquiring) $ \case
            MsgAcquired slot -> Effect $ handleStAcquired <$> stAcquired slot
      SendMsgDone a ->
        Yield (ClientAgency TokIdle) MsgDone (Done TokDone a)

    handleStAcquired
      :: ClientStAcquired txid tx slot m a
      -> Peer (LocalTxMonitor txid tx slot) AsClient StAcquired m a
    handleStAcquired = \case
      SendMsgNextTx stAcquired ->
        Yield (ClientAgency TokAcquired) MsgNextTx $
          Await (ServerAgency (TokBusy TokBusyNext)) $ \case
            MsgReplyNextTx tx -> Effect $ handleStAcquired <$> stAcquired tx
      SendMsgHasTx txid stAcquired ->
        Yield (ClientAgency TokAcquired) (MsgHasTx txid) $
          Await (ServerAgency (TokBusy TokBusyHas)) $ \case
            MsgReplyHasTx res -> Effect $ handleStAcquired <$> stAcquired res
      SendMsgReAcquire stAcquired ->
        Yield (ClientAgency TokAcquired) MsgReAcquire $
          Await (ServerAgency TokAcquiring) $ \case
            MsgAcquired slot -> Effect $ handleStAcquired <$> stAcquired slot
      SendMsgRelease stIdle ->
        Yield (ClientAgency TokAcquired) MsgRelease $
          Effect $ handleStIdle <$> stIdle

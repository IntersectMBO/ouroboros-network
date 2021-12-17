{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A view of the transaction monitor protocol from the point of view of
-- the server.
--
-- This provides simple access to the local mempool snapshots, to allow building
-- more monitoring logic from the client side after submitting transactions.
--
-- For execution, 'localTxMonitorServerPeer' is provided for conversion
-- into the typed protocol.
--
module Ouroboros.Network.Protocol.LocalTxMonitor.Server
  ( -- * Protocol type for the server
    -- | The protocol states from the point of view of the server.
    LocalTxMonitorServer (..)
  , ServerStIdle (..)
  , ServerStAcquiring (..)
  , ServerStAcquired (..)
  , ServerStBusy (..)
    -- * Execution as a typed protocol
  , localTxMonitorServerPeer
  ) where

import           Network.TypedProtocol.Core

import           Ouroboros.Network.Protocol.LocalTxMonitor.Type

-- | A local tx monitor protocol server, on top of some effect 'm'.
--
newtype LocalTxMonitorServer txid tx slot m a = LocalTxMonitorServer {
      runLocalTxMonitorServer :: m (ServerStIdle txid tx slot m a)
    }

-- | In the 'StIdle' protocol state, the server does not have agency. Instead,
-- it is waiting for:
--
-- * an acquire request,
-- * a termination message.
--
data ServerStIdle txid tx slot m a = ServerStIdle
    { recvMsgAcquire :: m (ServerStAcquiring txid tx slot m a)
    , recvMsgDone    :: m a
    }

-- | In the 'StAcquiring' protocol state, the server has agency and must acquire,
-- and hold on to, the current / latest snapshot of its mempool.
--
data ServerStAcquiring txid tx slot m a where
  SendMsgAcquired
    :: slot
    -> ServerStAcquired txid tx slot m a
    -> ServerStAcquiring txid tx slot m a

-- | In the 'StAcquired' protocol state, the server does not have agency and is
-- waiting for a client to either:
--
-- * request the next transaction from the snapshot;
-- * check the presence of a given transaction, by its id;
-- * await a change in the snapshot and acquire it;
-- * release and go back to the 'StIdle' state;
--
data ServerStAcquired txid tx slot m a = ServerStAcquired
    { recvMsgNextTx       :: m (ServerStBusy NextTx txid tx slot m a)
    , recvMsgHasTx        :: txid -> m (ServerStBusy HasTx txid tx slot m a)
    , recvMsgGetSizes     :: m (ServerStBusy GetSizes txid tx slot m a)
    , recvMsgAwaitAcquire :: m (ServerStAcquiring txid tx slot m a)
    , recvMsgRelease      :: m (ServerStIdle txid tx slot m a)
    }

-- In the 'StBusy' protocol state, the server has agency and is responding to
-- one of the client request. The state is parameterized by a kind 'StBusyKind'
-- to highlight the fact that, the server is in a busy state in response to a
-- particular query, and only responses for this query may be sent back to the
-- client.
--
data ServerStBusy (kind :: StBusyKind) txid tx slot m a where
  SendMsgReplyNextTx
    :: Maybe tx
    -> ServerStAcquired txid tx slot m a
    -> ServerStBusy NextTx txid tx slot m a

  SendMsgReplyHasTx
    :: Bool
    -> ServerStAcquired txid tx slot m a
    -> ServerStBusy HasTx txid tx slot m a

  SendMsgReplyGetSizes
    :: MempoolSizeAndCapacity
    -> ServerStAcquired txid tx slot m a
    -> ServerStBusy GetSizes txid tx slot m a

-- | Interpret a 'LocalTxMonitorServer' action sequence as a 'Peer' on the
-- client-side of the 'LocalTxMonitor' protocol.
--
localTxMonitorServerPeer ::
     forall txid tx slot m a.
     ( Monad m
     )
  => LocalTxMonitorServer txid tx slot m a
  -> Peer (LocalTxMonitor txid tx slot) AsServer StIdle m a
localTxMonitorServerPeer (LocalTxMonitorServer mServer) =
    Effect $ handleStIdle <$> mServer
  where
    handleStIdle ::
         ServerStIdle txid tx slot m a
      -> Peer (LocalTxMonitor txid tx slot) AsServer StIdle m a
    handleStIdle = \case
      ServerStIdle{recvMsgDone, recvMsgAcquire} ->
        Await (ClientAgency TokIdle) $ \case
          MsgAcquire ->
            Effect $ handleStAcquiring <$> recvMsgAcquire
          MsgDone ->
            Effect $ Done TokDone <$> recvMsgDone

    handleStAcquiring ::
         ServerStAcquiring txid tx slot m a
      -> Peer (LocalTxMonitor txid tx slot) AsServer StAcquiring m a
    handleStAcquiring = \case
      SendMsgAcquired slot serverStAcquired ->
        Yield (ServerAgency TokAcquiring) (MsgAcquired slot) $
          handleStAcquired serverStAcquired

    handleStAcquired ::
         ServerStAcquired txid tx slot m a
      -> Peer (LocalTxMonitor txid tx slot) AsServer StAcquired m a
    handleStAcquired = \case
      ServerStAcquired
        { recvMsgNextTx
        , recvMsgHasTx
        , recvMsgGetSizes
        , recvMsgAwaitAcquire
        , recvMsgRelease
        } -> Await (ClientAgency TokAcquired) $ \case
          MsgNextTx ->
            Effect $ handleNextTx <$> recvMsgNextTx
          MsgHasTx txid ->
            Effect $ handleHasTx <$> recvMsgHasTx txid
          MsgGetSizes ->
            Effect $ handleGetSizes <$> recvMsgGetSizes
          MsgAwaitAcquire ->
            Effect $ handleStAcquiring <$> recvMsgAwaitAcquire
          MsgRelease ->
            Effect $ handleStIdle <$> recvMsgRelease

    handleNextTx ::
         ServerStBusy NextTx txid tx slot m a
      -> Peer (LocalTxMonitor txid tx slot) AsServer (StBusy NextTx) m a
    handleNextTx = \case
      SendMsgReplyNextTx tx serverStAcquired ->
        Yield (ServerAgency (TokBusy TokNextTx)) (MsgReplyNextTx tx) $
          handleStAcquired serverStAcquired

    handleHasTx ::
         ServerStBusy HasTx txid tx slot m a
      -> Peer (LocalTxMonitor txid tx slot) AsServer (StBusy HasTx) m a
    handleHasTx = \case
      SendMsgReplyHasTx res serverStAcquired ->
        Yield (ServerAgency (TokBusy TokHasTx)) (MsgReplyHasTx res) $
          handleStAcquired serverStAcquired

    handleGetSizes ::
         ServerStBusy GetSizes txid tx slot m a
      -> Peer (LocalTxMonitor txid tx slot) AsServer (StBusy GetSizes) m a
    handleGetSizes = \case
      SendMsgReplyGetSizes sizes serverStAcquired ->
        Yield (ServerAgency (TokBusy TokGetSizes)) (MsgReplyGetSizes sizes) $
          handleStAcquired serverStAcquired

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}


-- | A view of the transaction submission protocol from the point of view of
-- the client.
--
-- This provides a view that uses less complex types and should be easier to
-- use than the underlying typed protocol itself.
--
-- For execution, 'localTxSubmissionClientPeer' is provided for conversion
-- into the typed protocol.
--
module Ouroboros.Network.Protocol.LocalTxSubmission.Client
  ( -- * Protocol type for the client
    -- | The protocol states from the point of view of the client.
    LocalTxSubmissionClient (..)
  , LocalTxClientStIdle (..)
    -- * The result from a transaction submission.
  , SubmitResult (..)
    -- * Execution as a typed protocol
  , localTxSubmissionClientPeer
    -- * Null local tx submission client
  , localTxSubmissionClientNull
    -- * Utilities
  , mapLocalTxSubmissionClient
  ) where

import           Control.Monad (forever)
import           Control.Monad.Class.MonadTimer

import           Network.TypedProtocol.Core

import           Ouroboros.Network.Protocol.LocalTxSubmission.Type


newtype LocalTxSubmissionClient tx reject m a = LocalTxSubmissionClient {
      runLocalTxSubmissionClient :: m (LocalTxClientStIdle tx reject m a)
    }

-- | A local tx submission client which never sends any message.
--
localTxSubmissionClientNull :: MonadTimer m => LocalTxSubmissionClient tx reject m a
localTxSubmissionClientNull =
    LocalTxSubmissionClient $ forever $ threadDelay 43200 {- day in seconds -}

{-# DEPRECATED localTxSubmissionClientNull "Use Ouroboros.Network.NodeToClient.localTxSubmissionPeerNull" #-}

-- | The client side of the local transaction submission protocol.
--
-- The peer in the client role submits transactions to the peer in the server
-- role.
--
data LocalTxClientStIdle tx reject m a where

     -- | The client submits a single transaction and waits a reply.
     --
     -- The server replies to inform the client that it has either accepted the
     -- transaction or rejected it. In the rejection case a reason for the
     -- rejection is included.
     --
     SendMsgSubmitTx
       :: tx
       -> (SubmitResult reject -> m (LocalTxClientStIdle tx reject m a))
       -> LocalTxClientStIdle tx reject m a

     -- | The client can terminate the protocol.
     --
     SendMsgDone
       :: a -> LocalTxClientStIdle tx reject m a


-- | Transform a 'LocalTxSubmissionClient' by mapping over the tx and the
-- rejection errors.
--
-- Note the direction of the individual mapping functions corresponds to
-- whether the types are used as protocol inputs or outputs.
--
mapLocalTxSubmissionClient :: forall tx tx' reject reject' m a.
                              Functor m
                           => (tx -> tx')
                           -> (reject' -> reject)
                           -> LocalTxSubmissionClient tx  reject  m a
                           -> LocalTxSubmissionClient tx' reject' m a
mapLocalTxSubmissionClient ftx frej =
    \(LocalTxSubmissionClient c) -> LocalTxSubmissionClient (fmap go c)
  where
    go :: LocalTxClientStIdle tx  reject  m a
       -> LocalTxClientStIdle tx' reject' m a
    go (SendMsgSubmitTx tx k) =
      SendMsgSubmitTx (ftx tx) (\res -> fmap go (k (fmap frej res)))

    go (SendMsgDone a) = SendMsgDone a


-- | A non-pipelined 'Peer' representing the 'LocalTxSubmissionClient'.
--
localTxSubmissionClientPeer
  :: forall tx reject m a. Monad m
  => LocalTxSubmissionClient tx reject m a
  -> Peer (LocalTxSubmission tx reject) AsClient StIdle m a
localTxSubmissionClientPeer (LocalTxSubmissionClient client) =
    Effect $ go <$> client
  where
    go :: LocalTxClientStIdle tx reject m a
       -> Peer (LocalTxSubmission tx reject) AsClient StIdle m a
    go (SendMsgSubmitTx tx k) =
      Yield (ClientAgency TokIdle)
            (MsgSubmitTx tx) $
      Await (ServerAgency TokBusy) $ \msg -> case msg of
        MsgAcceptTx        -> Effect (go <$> k SubmitSuccess)
        MsgRejectTx reject -> Effect (go <$> k (SubmitFail reject))

    go (SendMsgDone a) =
      Yield (ClientAgency TokIdle)
            MsgDone
            (Done TokDone a)


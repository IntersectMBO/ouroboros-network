{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes          #-}


-- | A view of the transaction submission protocol from the point of view of
-- the client.
--
-- This provides a view that uses less complex types and should be easier to
-- use than the underlying typed protocol itself.
--
-- For execution, 'localTxSubmissionClientPeer' is provided for conversion
-- into the typed protocol.
--
module Ouroboros.Network.Protocol.LocalTxSubmission.Client (
    -- * Protocol type for the client
    -- | The protocol states from the point of view of the client.
    LocalTxSubmissionClient(..)
  , LocalTxClientStIdle (..)

    -- * Execution as a typed protocol
  , localTxSubmissionClientPeer
  ) where

import           Network.TypedProtocol.Core

import           Ouroboros.Network.Protocol.LocalTxSubmission.Type


newtype LocalTxSubmissionClient tx reject m a = LocalTxSubmissionClient {
      runLocalTxSubmissionClient :: m (LocalTxClientStIdle tx reject m a)
    }

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
       -> (Maybe reject -> m (LocalTxClientStIdle tx reject m a))
       -> LocalTxClientStIdle tx reject m a

     -- | The client can terminate the protocol.
     --
     SendMsgDone
       :: a -> LocalTxClientStIdle tx reject m a


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
        MsgAcceptTx        -> Effect (go <$> k Nothing)
        MsgRejectTx reject -> Effect (go <$> k (Just reject))

    go (SendMsgDone a) =
      Yield (ClientAgency TokIdle)
            MsgDone
            (Done TokDone a)


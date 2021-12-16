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
-- For execution, 'txSubmissionClientPeer' is provided for conversion
-- into the typed protocol.
--
module Ouroboros.Network.Protocol.TxSubmission.Client
  ( -- * Protocol type for the client
    -- | The protocol states from the point of view of the client.
    TxSubmissionClient (..)
  , ClientStIdle (..)
  , ClientStTxIds (..)
  , ClientStTxs (..)
  , TxSizeInBytes
  , TokBlockingStyle (..)
  , BlockingReplyList (..)
    -- * Execution as a typed protocol
  , txSubmissionClientPeer
  ) where

import           Data.Word (Word16)

import           Network.TypedProtocol.Core

import           Ouroboros.Network.Protocol.TxSubmission.Type


-- | The client side of the transaction submission protocol.
--
-- The peer in the client role submits transactions to the peer in the server
-- role.
--
newtype TxSubmissionClient txid tx m a = TxSubmissionClient {
    runTxSubmissionClient :: m (ClientStIdle txid tx m a)
  }

-- | In the 'StIdle' protocol state, the client does not have agency. Instead
-- it is waiting for:
--
-- * a request for transaction ids (blocking or non-blocking)
-- * a request for a given list of transactions
-- * a termination message
--
-- It must be prepared to handle any of these.
--
data ClientStIdle txid tx m a = ClientStIdle {

    recvMsgRequestTxIds      :: forall blocking.
                                TokBlockingStyle blocking
                             -> Word16
                             -> Word16
                             -> m (ClientStTxIds blocking txid tx m a),

    recvMsgRequestTxs        :: [txid]
                             -> m (ClientStTxs txid tx m a)
  }

data ClientStTxIds blocking txid tx m a where
  SendMsgReplyTxIds :: BlockingReplyList blocking (txid, TxSizeInBytes)
                    -> ClientStIdle           txid tx m a
                    -> ClientStTxIds blocking txid tx m a

  -- | In the blocking case, the client can terminate the protocol. This could
  -- be used when the client knows there will be no more transactions to submit.
  --
  SendMsgDone       :: a -> ClientStTxIds StBlocking txid tx m a


data ClientStTxs txid tx m a where
  SendMsgReplyTxs   :: [tx]
                    -> ClientStIdle txid tx m a
                    -> ClientStTxs  txid tx m a


-- | A non-pipelined 'Peer' representing the 'TxSubmissionClient'.
--
txSubmissionClientPeer :: forall txid tx m a. Monad m
                       => TxSubmissionClient txid tx m a
                       -> Peer (TxSubmission txid tx) AsClient StIdle m a
txSubmissionClientPeer (TxSubmissionClient client) =
    Effect $ go <$> client
  where
    go :: ClientStIdle txid tx m a
       -> Peer (TxSubmission txid tx) AsClient StIdle m a
    go ClientStIdle {recvMsgRequestTxIds, recvMsgRequestTxs} =
      Await (ServerAgency TokIdle) $ \msg -> case msg of
        MsgRequestTxIds blocking ackNo reqNo -> Effect $ do
          reply <- recvMsgRequestTxIds blocking ackNo reqNo
          case reply of
            SendMsgReplyTxIds txids k ->
              return $ Yield (ClientAgency (TokTxIds blocking))
                             (MsgReplyTxIds txids)
                             (go k)

            SendMsgDone result ->
              return $ Yield (ClientAgency (TokTxIds TokBlocking))
                              MsgDone
                             (Done TokDone result)

        MsgRequestTxs txids -> Effect $ do
          SendMsgReplyTxs txs k <- recvMsgRequestTxs txids
          return $ Yield (ClientAgency TokTxs)
                         (MsgReplyTxs txs)
                         (go k)

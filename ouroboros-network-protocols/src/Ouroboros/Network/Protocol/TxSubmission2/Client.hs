{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
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
module Ouroboros.Network.Protocol.TxSubmission2.Client
  ( -- * Protocol type for the client
    -- | The protocol states from the point of view of the client.
    TxSubmissionClient (..)
  , ClientStIdle (..)
  , ClientStTxIds (..)
  , ClientStTxs (..)
  , TxSizeInBytes
  , SingBlockingStyle (..)
  , BlockingReplyList (..)
    -- * Execution as a typed protocol
  , txSubmissionClientPeer
  ) where

import Network.TypedProtocol.Core
import Network.TypedProtocol.Peer.Client

import Ouroboros.Network.Protocol.TxSubmission2.Type


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
                                SingBlockingStyle blocking
                             -> NumTxIdsToAck
                             -> NumTxIdsToReq
                             -> m (ClientStTxIds blocking txid tx m a),

    recvMsgRequestTxs        :: [txid]
                             -> m (ClientStTxs txid tx m a)
  }

data ClientStTxIds blocking txid tx m a where
  SendMsgReplyTxIds :: BlockingReplyList blocking (txid, SizeInBytes)
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
                       -> Client (TxSubmission2 txid tx) NonPipelined StInit m a
txSubmissionClientPeer (TxSubmissionClient client) =
    Yield MsgInit $
    Effect $ go <$> client
  where
    go :: ClientStIdle txid tx m a
       -> Client (TxSubmission2 txid tx) NonPipelined StIdle m a
    go ClientStIdle {recvMsgRequestTxIds, recvMsgRequestTxs} =
      Await $ \msg -> case msg of
        MsgRequestTxIds blocking ackNo reqNo -> Effect $ do
          reply <- recvMsgRequestTxIds blocking ackNo reqNo
          case reply of
            SendMsgReplyTxIds txids k ->
              -- TODO: investigate why GHC cannot infer `SingI`; it used to in
              -- `coot/typed-protocols-rewrite` branch
              return $ case blocking of
                SingBlocking    -> Yield (MsgReplyTxIds txids)
                                         (go k)
                SingNonBlocking -> Yield (MsgReplyTxIds txids)
                                         (go k)

            SendMsgDone result ->
              return $ Yield MsgDone
                             (Done result)

        MsgRequestTxs txids -> Effect $ do
          SendMsgReplyTxs txs k <- recvMsgRequestTxs txids
          return $ Yield (MsgReplyTxs txs)
                         (go k)

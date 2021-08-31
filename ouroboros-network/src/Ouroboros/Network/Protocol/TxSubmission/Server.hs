{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A view of the transaction submission protocol from the point of view of
-- the server.
--
-- This provides a view that uses less complex types and should be easier to
-- use than the underlying typed protocol itself.
--
-- For execution, a conversion into the typed protocol is provided.
--
module Ouroboros.Network.Protocol.TxSubmission.Server (
    -- * Protocol type for the server
    -- | The protocol states from the point of view of the server.
    TxSubmissionServerPipelined (..)
  , ServerStIdle(..)
  , Collect(..)
  , TxSizeInBytes

    -- * Execution as a typed protocol
  , txSubmissionServerPeerPipelined
  ) where

import           Data.Word (Word16)
import           Data.List.NonEmpty (NonEmpty)

import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Pipelined

import           Ouroboros.Network.Protocol.TxSubmission.Type


data TxSubmissionServerPipelined txid tx m a where
  TxSubmissionServerPipelined
    :: m (ServerStIdle              Z txid tx m a)
    ->    TxSubmissionServerPipelined txid tx m a


-- | This is the type of the pipelined results, collected by 'CollectPipelined'.
-- This protocol can pipeline requests for transaction ids and transactions,
-- so we use a sum of either for collecting the responses.
--
data Collect txid tx =
       -- | The result of 'SendMsgRequestTxIdsPipelined'. It also carries
       -- the number of txids originally requested.
       CollectTxIds Word16 [(txid, TxSizeInBytes)]

       -- | The result of 'SendMsgRequestTxsPipelined'. The actual reply only
       -- contains the transactions sent, but this pairs them up with the
       -- transactions requested. This is because the peer can determine that
       -- some transactions are no longer needed.
     | CollectTxs [txid] [tx]


data ServerStIdle (n :: N) txid tx m a where

  -- |
  --
  SendMsgRequestTxIdsBlocking
    :: Word16                               -- ^ number of txids to acknowledge
    -> Word16                               -- ^ number of txids to request
    -> m a                                  -- ^ Result if done
    -> (NonEmpty (txid, TxSizeInBytes)
        -> m (ServerStIdle Z txid tx m a))
    -> ServerStIdle        Z txid tx m a

  -- |
  --
  SendMsgRequestTxIdsPipelined
    :: Word16
    -> Word16
    -> m (ServerStIdle (S n) txid tx m a)
    -> ServerStIdle       n  txid tx m a

  -- |
  --
  SendMsgRequestTxsPipelined
    :: [txid]
    -> m (ServerStIdle (S n) txid tx m a)
    -> ServerStIdle       n  txid tx m a

  -- | Collect a pipelined result.
  --
  CollectPipelined
    :: Maybe                 (ServerStIdle (S n) txid tx m a)
    -> (Collect txid tx -> m (ServerStIdle    n  txid tx m a))
    ->                        ServerStIdle (S n) txid tx m a


-- | Transform a 'TxSubmissionServerPipelined' into a 'PeerPipelined'.
--
txSubmissionServerPeerPipelined
    :: forall txid tx m a.
       Functor m
    => TxSubmissionServerPipelined txid tx m a
    -> PeerPipelined (TxSubmission txid tx) AsServer StIdle m a
txSubmissionServerPeerPipelined (TxSubmissionServerPipelined server) =
    PeerPipelined $ SenderEffect (go <$> server)
  where
    go :: forall (n :: N).
          ServerStIdle n txid tx m a
       -> PeerSender (TxSubmission txid tx) AsServer StIdle
                     n (Collect txid tx) m a

    go (SendMsgRequestTxIdsBlocking ackNo reqNo kDone k) =
      SenderYield
        (ServerAgency TokIdle)
        (MsgRequestTxIds TokBlocking ackNo reqNo) $
      SenderAwait
        (ClientAgency (TokTxIds TokBlocking)) $ \msg ->
        case msg of
          MsgDone ->
            SenderEffect (SenderDone TokDone <$> kDone)

          MsgReplyTxIds (BlockingReply txids) ->
            SenderEffect (go <$> k txids)

    go (SendMsgRequestTxIdsPipelined ackNo reqNo k) =
      SenderPipeline
        (ServerAgency TokIdle)
        (MsgRequestTxIds TokNonBlocking ackNo reqNo)
        (ReceiverAwait (ClientAgency (TokTxIds TokNonBlocking)) $ \msg ->
           case msg of
             MsgReplyTxIds (NonBlockingReply txids) ->
               ReceiverDone (CollectTxIds reqNo txids))
        (SenderEffect (go <$> k))

    go (SendMsgRequestTxsPipelined txids k) =
      SenderPipeline
        (ServerAgency TokIdle)
        (MsgRequestTxs txids)
        (ReceiverAwait (ClientAgency TokTxs) $ \msg ->
           case msg of
             MsgReplyTxs txs -> ReceiverDone (CollectTxs txids txs))
        (SenderEffect (go <$> k))

    go (CollectPipelined mNone collect) =
      SenderCollect
        (fmap go mNone)
        (SenderEffect . fmap go . collect)

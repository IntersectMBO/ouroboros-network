{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- | A view of the transaction submission protocol from the point of view of
-- the server.
--
-- This provides a view that uses less complex types and should be easier to
-- use than the underlying typed protocol itself.
--
-- For execution, a conversion into the typed protocol is provided.
--
module Ouroboros.Network.Protocol.TxSubmission2.Server
  ( -- * Protocol type for the server
    -- | The protocol states from the point of view of the server.
    TxSubmissionServerPipelined (..)
  , ServerStIdle (..)
  , Collect (..)
    -- * Execution as a typed protocol
  , txSubmissionServerPeerPipelined
  ) where

import Data.List.NonEmpty (NonEmpty)

import Network.TypedProtocol.Core
import Network.TypedProtocol.Peer.Server

import Ouroboros.Network.Protocol.TxSubmission2.Type


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
       CollectTxIds NumTxIdsToReq [(txid, SizeInBytes)]

       -- | The result of 'SendMsgRequestTxsPipelined'. The actual reply only
       -- contains the transactions sent, but this pairs them up with the
       -- transactions requested. This is because the peer can determine that
       -- some transactions are no longer needed.
     | CollectTxs [txid] [tx]


data ServerStIdle (n :: N) txid tx m a where

  -- |
  --
  SendMsgRequestTxIdsBlocking
    :: NumTxIdsToAck                        -- ^ number of txids to acknowledge
    -> NumTxIdsToReq                        -- ^ number of txids to request
    -> m a                                  -- ^ Result if done
    -> (NonEmpty (txid, SizeInBytes)
        -> m (ServerStIdle Z txid tx m a))
    -> ServerStIdle        Z txid tx m a

  -- |
  --
  SendMsgRequestTxIdsPipelined
    :: NumTxIdsToAck
    -> NumTxIdsToReq
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
    -> ServerPipelined (TxSubmission2 txid tx) StInit m a
txSubmissionServerPeerPipelined (TxSubmissionServerPipelined server) =
    ServerPipelined $
      -- We need to assist GHC to infer the existentially quantified `c` as
      -- `Collect txid tx`
      Await @_ @(Pipelined Z (Collect txid tx))
            (\MsgInit -> Effect (go <$> server))
  where
    go :: forall (n :: N).
          ServerStIdle n txid tx m a
       -> Server (TxSubmission2 txid tx) (Pipelined n (Collect txid tx)) StIdle m a

    go (SendMsgRequestTxIdsBlocking ackNo reqNo kDone k) =
      Yield
        (MsgRequestTxIds SingBlocking ackNo reqNo) $
      Await \case
        MsgDone ->
          Effect (Done <$> kDone)

        MsgReplyTxIds (BlockingReply txids) ->
          Effect (go <$> k txids)

    go (SendMsgRequestTxIdsPipelined ackNo reqNo k) =
      YieldPipelined
        (MsgRequestTxIds SingNonBlocking ackNo reqNo)
        (ReceiverAwait \case
           MsgReplyTxIds (NonBlockingReply txids) ->
             ReceiverDone (CollectTxIds reqNo txids))
        (Effect (go <$> k))

    go (SendMsgRequestTxsPipelined txids k) =
      YieldPipelined
        (MsgRequestTxs txids)
        (ReceiverAwait \case
           MsgReplyTxs txs -> ReceiverDone (CollectTxs txids txs))
        (Effect (go <$> k))

    go (CollectPipelined mNone collect) =
      Collect (fmap go mNone)
              (Effect . fmap go . collect)


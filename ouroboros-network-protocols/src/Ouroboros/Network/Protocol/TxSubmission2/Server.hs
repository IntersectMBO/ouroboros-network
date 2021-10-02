{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators            #-}

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
  , TxSizeInBytes
    -- * Execution as a typed protocol
  , txSubmissionServerPeerPipelined
  ) where

import           Control.Monad.Class.MonadSTM (STM)

import           Data.List.NonEmpty (NonEmpty)
import           Data.Type.Queue
import           Data.Word (Word16)

import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Peer.Server

import           Ouroboros.Network.Protocol.TxSubmission2.Type


data TxSubmissionServerPipelined txid tx m a where
  TxSubmissionServerPipelined
    :: forall txid tx m a.
       m (ServerStIdle                txid tx Empty m a)
    ->    TxSubmissionServerPipelined txid tx       m a


data F txid st st' where
    FTxIds :: Word16 -> F txid (StTxIds NonBlocking) StIdle
    FTxs   :: [txid] -> F txid StTxs                 StIdle


data ServerStIdle txid tx (q :: Queue (TxSubmission2 txid tx)) m a where

  -- |
  --
  SendMsgRequestTxIdsBlocking
    :: Word16                               -- ^ number of txids to acknowledge
    -> Word16                               -- ^ number of txids to request
    -> m a                                  -- ^ Result if done
    -> (NonEmpty (txid, TxSizeInBytes)
        -> m (ServerStIdle txid tx Empty m a))
    -> ServerStIdle        txid tx Empty m a

  -- |
  --
  SendMsgRequestTxIdsPipelined
    :: Word16
    -> Word16
    -> m (ServerStIdle txid tx (q |> Tr (StTxIds NonBlocking) StIdle) m a)
    -> ServerStIdle    txid tx  q                                     m a

  -- |
  --
  SendMsgRequestTxsPipelined
    :: [txid]
    -> m (ServerStIdle txid tx (q |> Tr StTxs StIdle) m a)
    -> ServerStIdle    txid tx  q                     m a

  -- | Collect a pipelined result.
  --
  CollectPipelined
    :: Maybe         (ServerStIdle txid tx (Tr st StIdle <| q) m a)
    -> (Word16 -> [(txid, TxSizeInBytes)]
               -> m  (ServerStIdle txid tx                  q  m a))
    -> ([txid] -> [tx]
               -> m  (ServerStIdle txid tx                  q  m a))
    ->                ServerStIdle txid tx (Tr st StIdle <| q) m a


-- | Transform a 'TxSubmissionServerPipelined' into a 'PeerPipelined'.
--
txSubmissionServerPeerPipelined
    :: forall txid tx m a.
       Functor m
    => TxSubmissionServerPipelined txid tx m a
    -> Server (TxSubmission2 txid tx) 'Pipelined Empty StInit m (STM m) a
txSubmissionServerPeerPipelined (TxSubmissionServerPipelined server) =
    Await $ \MsgInit -> Effect (go SingEmptyF <$> server)
  where
    go :: forall (q :: Queue (TxSubmission2 txid tx)).
          SingQueueF (F txid) q
       -> ServerStIdle txid tx q m a
       -> Server (TxSubmission2 txid tx) 'Pipelined q StIdle m (STM m) a

    go q (SendMsgRequestTxIdsBlocking ackNo reqNo kDone k) =
      Yield (MsgRequestTxIds SingBlocking ackNo reqNo) $
      Await $ \msg ->
        case msg of
          MsgDone ->
            Effect (Done <$> kDone)

          MsgReplyTxIds (BlockingReply txids) ->
            Effect (go q <$> k txids)

    go q (SendMsgRequestTxIdsPipelined ackNo reqNo k) =
      YieldPipelined
        (MsgRequestTxIds SingNonBlocking ackNo reqNo)
          (Effect (go (q |> FTxIds reqNo) <$> k))

    go q (SendMsgRequestTxsPipelined txids k) =
      YieldPipelined
        (MsgRequestTxs txids)
        (Effect (go (q |> FTxs txids)  <$> k))

    go q@(FTxIds reqNo `SingConsF` q') (CollectPipelined mNone collect _) =
      Collect (fmap (go q) mNone)
              (\(MsgReplyTxIds (NonBlockingReply txids)) ->
                      CollectDone (Effect $ go q' <$> collect reqNo txids)
              )

    go q@(FTxs txids `SingConsF` q') (CollectPipelined mNone _ collect) =
      Collect (fmap (go q) mNone)
              (\(MsgReplyTxs txs) ->
                  CollectDone (Effect $ go q' <$> collect txids txs))

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-partial-fields #-}

module Ouroboros.Network.TxSubmission.Inbound
  ( txSubmissionInbound
  , TxSubmissionMempoolWriter (..)
  , TraceTxSubmissionInbound (..)
  , TxSubmissionProtocolError (..)
  , ProcessedTxCount (..)
  ) where

import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence.Strict qualified as Seq
import Data.Set qualified as Set
import Data.Word (Word16)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..), unsafeNoThunks)

import Control.Concurrent.Class.MonadMVar (MonadMVar)
import Control.Concurrent.Class.MonadSTM.Strict.TVar.Checked
import Control.Monad (unless)
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadThrow
import Control.Tracer (Tracer, traceWith)

import Network.TypedProtocol.Pipelined (N, Nat (..), natToInt)

import Ouroboros.Network.NodeToNode.Version (NodeToNodeVersion)
import Ouroboros.Network.Protocol.TxSubmission2.Server
import Ouroboros.Network.TxSubmission.Inbound.State
import Ouroboros.Network.TxSubmission.Mempool.Reader
           (TxSubmissionMempoolReader (..))

-- | The consensus layer functionality that the inbound side of the tx
-- submission logic requires.
--
-- This is provided to the tx submission logic by the consensus layer.
--
data TxSubmissionMempoolWriter txid tx idx m =
     TxSubmissionMempoolWriter {

       -- | Compute the transaction id from a transaction.
       --
       -- This is used in the protocol handler to verify a full transaction
       -- matches a previously given transaction id.
       --
       txId          :: tx -> txid,

       -- | Supply a batch of transactions to the mempool. They are either
       -- accepted or rejected individually, but in the order supplied.
       --
       -- The 'txid's of all transactions that were added successfully are
       -- returned.
       mempoolAddTxs :: [tx] -> m [txid]
    }

data ProcessedTxCount = ProcessedTxCount {
      -- | Just accepted this many transactions.
      ptxcAccepted :: Int
      -- | Just rejected this many transactions.
    , ptxcRejected :: Int
    }
  deriving (Eq, Show)

data TraceTxSubmissionInbound txid tx =
    -- | Number of transactions just about to be inserted.
    TraceTxSubmissionCollected Int
    -- | Just processed transaction pass/fail breakdown.
  | TraceTxSubmissionProcessed ProcessedTxCount
    -- | Server received 'MsgDone'
  | TraceTxInboundTerminated
  | TraceTxInboundCanRequestMoreTxs Int
  | TraceTxInboundCannotRequestMoreTxs Int
  deriving (Eq, Show)

data TxSubmissionProtocolError =
       ProtocolErrorTxNotRequested
     | ProtocolErrorTxIdsNotRequested
  deriving Show

instance Exception TxSubmissionProtocolError where
  displayException ProtocolErrorTxNotRequested =
      "The peer replied with a transaction we did not ask for."
  displayException ProtocolErrorTxIdsNotRequested =
      "The peer replied with more txids than we asked for."

-- | Information maintained internally in the 'txSubmissionInbound' server
-- implementation.
--
newtype ServerState = ServerState {
       -- | The number of transactions we can acknowledge on our next request
       -- for more transactions. The number here have already been removed from
       -- 'unacknowledgedTxIds'.
       --
       numTxsToAcknowledge :: Word16
     }
  deriving (Show, Generic)

instance NoThunks ServerState


initialServerState :: ServerState
initialServerState = ServerState { numTxsToAcknowledge = 0 }

txSubmissionInbound
  :: forall peeraddr txid tx idx m.
     ( Ord txid
     , Ord peeraddr
     , MonadMVar m
     , MonadSTM m
     , MonadThrow m
     )
  => Tracer m (TraceTxSubmissionInbound txid tx)
  -> Word16         -- ^ Maximum number of unacknowledged txids allowed
  -> TxSubmissionMempoolReader txid tx idx m
  -> TxSubmissionMempoolWriter txid tx idx m
  -> SharedTxStateVar m peeraddr txid tx
  -> PeerTxStateAPI   m peeraddr txid tx
  -> peeraddr
  -> NodeToNodeVersion
  -> TxSubmissionServerPipelined txid tx m ()
txSubmissionInbound tracer maxUnacked mpReader mpWriter sharedVar peerStateAPI peeraddr _version =
    TxSubmissionServerPipelined $
      continueWithStateM (serverIdle Zero) initialServerState
  where
    -- TODO #1656: replace these fixed limits by policies based on
    -- SizeInBytes and delta-Q and the bandwidth/delay product.
    -- These numbers are for demo purposes only, the throughput will be low.
    maxTxIdsToRequest = 3 :: Word16
    maxTxToRequest    = 2 :: Word16

    TxSubmissionMempoolReader{mempoolGetSnapshot} = mpReader

    TxSubmissionMempoolWriter
      { txId
      , mempoolAddTxs
      } = mpWriter

    -- !!! TODO !!!
    --
    -- Some txs might be downloaded by other `tx-submission` clients.  When we
    -- block we should also race if there are some txids to acknowledge,
    -- otherwise it could happen that we are blocked on requesting more txs,
    -- but some were already downloaded by some other client and we could
    -- submit them to the mempool.
    --

    serverIdle :: forall (n :: N).
                  Nat n
               -> StatefulM ServerState n txid tx m
    serverIdle n = StatefulM $ \st -> case n of
        Zero -> do
          b <- canRequestMoreTxs sharedVar peeraddr
          if b
          then do
            -- There are no replies in flight, but we do know some more txs we
            -- can ask for, so lets ask for them and more txids.
            traceWith tracer (TraceTxInboundCanRequestMoreTxs (natToInt n))
            continueWithStateM (serverReqTxs Zero) st

          else do
            traceWith tracer (TraceTxInboundCannotRequestMoreTxs (natToInt n))
            -- There's no replies in flight, and we have no more txs we can
            -- ask for so the only remaining thing to do is to ask for more
            -- txids. Since this is the only thing to do now, we make this a
            -- blocking call.
            let numTxIdsToRequest = maxTxIdsToRequest `min` maxUnacked
            -- assert (requestedTxIdsInflight st == 0
            --       && Seq.null (unacknowledgedTxIds st)
            --       && Map.null (availableTxids st)
            --       && Map.null (bufferedTxs st)) $
            pure $
              SendMsgRequestTxIdsBlocking
                (numTxsToAcknowledge st)
                numTxIdsToRequest
                -- Our result if the client terminates the protocol
                (traceWith tracer TraceTxInboundTerminated)
                (\txids -> do
                  setRequestInflight peerStateAPI numTxIdsToRequest
                  collectAndContinueWithState (handleReply Zero)
                    st { numTxsToAcknowledge = 0 }
                    . CollectTxIds numTxIdsToRequest
                    . NonEmpty.toList
                    $ txids
                )

        Succ n' -> do
          b <- canRequestMoreTxs sharedVar peeraddr
          if b
          then do
            -- We have replies in flight and we should eagerly collect them if
            -- available, but there are transactions to request too so we
            -- should not block waiting for replies.
            --
            -- Having requested more transactions, we opportunistically ask
            -- for more txids in a non-blocking way. This is how we pipeline
            -- asking for both txs and txids.
            --
            -- It's important not to pipeline more requests for txids when we
            -- have no txs to ask for, since (with no other guard) this will
            -- put us into a busy-polling loop.
            --
            traceWith tracer (TraceTxInboundCanRequestMoreTxs (natToInt n))
            pure $ CollectPipelined
              (Just (continueWithStateM (serverReqTxs (Succ n')) st))
              (collectAndContinueWithState (handleReply n') st)

          else do
            traceWith tracer (TraceTxInboundCannotRequestMoreTxs (natToInt n))
            -- In this case there is nothing else to do so we block until we
            -- collect a reply.
            pure $ CollectPipelined
              Nothing
              (collectAndContinueWithState (handleReply n') st)

    handleReply :: forall (n :: N).
                   Nat n
                -> StatefulCollect ServerState n txid tx m
    handleReply n = StatefulCollect $ \st collect -> case collect of
      CollectTxIds reqNo txids -> do
        -- Check they didn't send more than we asked for. We don't need to
        -- check for a minimum: the blocking case checks for non-zero
        -- elsewhere, and for the non-blocking case it is quite normal for
        -- them to send us none.
        let txidsSeq = Seq.fromList (map fst txids)
            txidsMap = Map.fromList txids

        unless (Seq.length txidsSeq <= fromIntegral reqNo) $
          throwIO ProtocolErrorTxIdsNotRequested

        -- Upon receiving a batch of new txids we extend our available set,
        -- and extended the unacknowledged sequence.
        --
        -- We also pre-emptively acknowledge those txids that are already in
        -- the mempool. This prevents us from requesting their corresponding
        -- transactions again in the future.
        mpSnapshot <- atomically mempoolGetSnapshot
        numTxsToAck <- receivedTxIds sharedVar
                                     mpSnapshot peeraddr
                                     reqNo txidsSeq txidsMap
        let st' = st {
          numTxsToAcknowledge = numTxsToAcknowledge st
                              + fromIntegral numTxsToAck
        }
        continueWithStateM (serverIdle n) st'

      CollectTxs txids txs -> do
        -- To start with we have to verify that the txs they have sent us do
        -- correspond to the txs we asked for. This is slightly complicated by
        -- the fact that in general we get a subset of the txs that we asked
        -- for. We should never get a tx we did not ask for. We take a strict
        -- approach to this and check it.
        --
        let txsMap :: Map txid tx
            txsMap = Map.fromList [ (txId tx, tx) | tx <- txs ]

            txidsReceived  = Map.keysSet txsMap
            txidsRequested = Set.fromList txids

        unless (txidsReceived `Set.isSubsetOf` txidsRequested) $
          throwIO ProtocolErrorTxNotRequested

        (numTxToAck, txsReady) <- undefined -- collectTxs sharedVar peeraddr txidsRequested txsMap

        let !collected = length txs
        traceWith tracer $
          TraceTxSubmissionCollected collected

        txidsAccepted <- mempoolAddTxs txsReady

        let !accepted = length txidsAccepted

        traceWith tracer $ TraceTxSubmissionProcessed ProcessedTxCount {
            ptxcAccepted = accepted
          , ptxcRejected = collected - accepted
          }

        continueWithStateM (serverIdle n) st {
          numTxsToAcknowledge = numTxsToAcknowledge st
                              + fromIntegral numTxToAck
        }

    serverReqTxs :: forall (n :: N).
                    Nat n
                 -> StatefulM ServerState n txid tx m
    serverReqTxs n = StatefulM $ \st -> do
        -- TODO: This implementation is deliberately naive, we pick in an
        -- arbitrary order and up to a fixed limit. This is to illustrate
        -- that we can request txs out of order. In the final version we will
        -- try to pick in-order and only pick out of order when we have to.
        -- We will also uses the size of txs in bytes as our limit for
        -- upper and lower watermarks for pipelining. We'll also use the
        -- amount in flight and delta-Q to estimate when we're in danger of
        -- becoming idle, and need to request stalled txs.
        --
        txsToRequest <- getTxsToRequest sharedVar maxTxToRequest peeraddr

        return $
          SendMsgRequestTxsPipelined
          (Set.toList txsToRequest)
          (continueWithStateM (serverReqTxIds (Succ n)) st)

    serverReqTxIds :: forall (n :: N).
                      Nat n
                   -> StatefulM ServerState n txid tx m
    serverReqTxIds n = StatefulM $ \st -> do
          -- This definition is justified by the fact that the
          -- 'numTxsToAcknowledge' are not included in the
          -- 'unacknowledgedTxIds'.
      numTxIdsToRequest
        <- getNumTxIdsToRequest peerStateAPI maxUnacked maxTxIdsToRequest

      if numTxIdsToRequest > 0
        then pure $ SendMsgRequestTxIdsPipelined
          (numTxsToAcknowledge st)
          numTxIdsToRequest
          (do
             continueWithStateM (serverIdle (Succ n)) st {
                numTxsToAcknowledge = 0
              })
        else continueWithStateM (serverIdle n) st

newtype Stateful s n txid tx m = Stateful (s -> ServerStIdle n txid tx m ())

newtype StatefulM s n txid tx m
  = StatefulM (s -> m (ServerStIdle n txid tx m ()))

newtype StatefulCollect s n txid tx m
  = StatefulCollect (s -> Collect txid tx -> m (ServerStIdle n txid tx m ()))

-- | After checking that there are no unexpected thunks in the provided state,
-- pass it to the provided function.
--
-- See 'checkInvariant' and 'unsafeNoThunks'.
continueWithState :: NoThunks s
                  => Stateful s n txid tx m
                  -> s
                  -> ServerStIdle n txid tx m ()
continueWithState (Stateful f) !st =
    checkInvariant (show <$> unsafeNoThunks st) (f st)

-- | A variant of 'continueWithState' to be more easily utilized with
-- 'serverIdle' and 'serverReqTxIds'.
continueWithStateM :: NoThunks s
                   => StatefulM s n txid tx m
                   -> s
                   -> m (ServerStIdle n txid tx m ())
continueWithStateM (StatefulM f) !st =
    checkInvariant (show <$> unsafeNoThunks st) (f st)
{-# NOINLINE continueWithStateM #-}

-- | A variant of 'continueWithState' to be more easily utilized with
-- 'handleReply'.
collectAndContinueWithState :: NoThunks s
                            => StatefulCollect s n txid tx m
                            -> s
                            -> Collect txid tx
                            -> m (ServerStIdle n txid tx m ())
collectAndContinueWithState (StatefulCollect f) !st c =
    checkInvariant (show <$> unsafeNoThunks st) (f st c)
{-# NOINLINE collectAndContinueWithState #-}

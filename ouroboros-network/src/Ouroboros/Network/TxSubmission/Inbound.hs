{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
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

import Data.Foldable as Foldable (foldl', toList)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Sequence.Strict (StrictSeq)
import Data.Sequence.Strict qualified as Seq
import Data.Set qualified as Set
import Data.Word (Word16)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..), unsafeNoThunks)

import Cardano.Prelude (forceElemsToWHNF)

import Control.Concurrent.Class.MonadSTM.Strict.TVar.Checked
import Control.Exception (assert)
import Control.Monad (unless)
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTimer.SI
import Control.Tracer (Tracer, traceWith)

import Network.TypedProtocol.Pipelined (N, Nat (..), natToInt)

import Ouroboros.Network.NodeToNode.Version (NodeToNodeVersion)
import Ouroboros.Network.Protocol.Limits
import Ouroboros.Network.Protocol.TxSubmission2.Server
import Ouroboros.Network.Protocol.TxSubmission2.Type
import Ouroboros.Network.TxSubmission.Inbound.Types (ProcessedTxCount (..),
         TxSubmissionMempoolWriter (..), TraceTxSubmissionInbound (..),
         TxSubmissionProtocolError (..))
import Ouroboros.Network.TxSubmission.Mempool.Reader (MempoolSnapshot (..),
           TxSubmissionMempoolReader (..))

-- | Information maintained internally in the 'txSubmissionInbound' server
-- implementation.
--
data ServerState txid tx = ServerState {
       -- | The number of transaction identifiers that we have requested but
       -- which have not yet been replied to. We need to track this it keep
       -- our requests within the limit on the number of unacknowledged txids.
       --
       requestedTxIdsInFlight :: !Word16,

       -- | Those transactions (by their identifier) that the client has told
       -- us about, and which we have not yet acknowledged. This is kept in
       -- the order in which the client gave them to us. This is the same order
       -- in which we submit them to the mempool (or for this example, the final
       -- result order). It is also the order we acknowledge in.
       unacknowledgedTxIds    :: !(StrictSeq txid),

       -- | Those transactions (by their identifier) that we can request. These
       -- are a subset of the 'unacknowledgedTxIds' that we have not yet
       -- requested. This is not ordered to illustrate the fact that we can
       -- request txs out of order. We also remember the size.
       availableTxids         :: !(Map txid SizeInBytes),

       -- | Transactions we have successfully downloaded but have not yet added
       -- to the mempool or acknowledged. This needed because we can request
       -- transactions out of order but must use the original order when adding
       -- to the mempool or acknowledging transactions.
       --
       -- However, it's worth noting that, in a few situations, some of the
       -- transaction IDs in this 'Map' may be mapped to 'Nothing':
       --
       -- * Transaction IDs mapped to 'Nothing' can represent transaction IDs
       --   that were requested, but not received. This can occur because the
       --   client will not necessarily send all of the transactions that we
       --   asked for, but we still need to acknowledge those transactions.
       --
       --   For example, if we request a transaction that no longer exists in
       --   the client's mempool, the client will just exclude it from the
       --   response. However, we still need to acknowledge it (i.e. remove it
       --   from the 'unacknowledgedTxIds') in order to note that we're no
       --   longer awaiting receipt of that transaction.
       --
       -- * Transaction IDs mapped to 'Nothing' can represent transactions
       --   that were not requested from the client because they're already
       --   in the mempool.
       --
       --   For example, if we request some transaction IDs and notice that
       --   some subset of them have are already in the mempool, we wouldn't
       --   want to bother asking for those specific transactions. Therefore,
       --   we would just insert those transaction IDs mapped to 'Nothing' to
       --   the 'bufferedTxs' such that those transactions are acknowledged,
       --   but never actually requested.
       --
       bufferedTxs            :: !(Map txid (Maybe tx)),

       -- | The number of transactions we can acknowledge on our next request
       -- for more transactions. The number here have already been removed from
       -- 'unacknowledgedTxIds'.
       --
       numTxsToAcknowledge    :: !Word16
     }
  deriving (Show, Generic)

instance ( NoThunks txid
         , NoThunks tx
         ) => NoThunks (ServerState txid tx)

initialServerState :: ServerState txid tx
initialServerState = ServerState 0 Seq.empty Map.empty Map.empty 0


txSubmissionInbound
  :: forall txid tx idx m.
     ( Ord txid
     , NoThunks txid
     , NoThunks tx
     , MonadSTM m
     , MonadThrow m
     , MonadDelay m
     )
  => Tracer m (TraceTxSubmissionInbound txid tx)
  -> NumTxIdsToAck  -- ^ Maximum number of unacknowledged txids allowed
  -> TxSubmissionMempoolReader txid tx idx m
  -> TxSubmissionMempoolWriter txid tx idx m
  -> NodeToNodeVersion
  -> TxSubmissionServerPipelined txid tx m ()
txSubmissionInbound tracer (NumTxIdsToAck maxUnacked) mpReader mpWriter _version =
    TxSubmissionServerPipelined $ do
#ifdef TXSUBMISSION_DELAY
      -- make the client linger before asking for tx's and expending
      -- our resources as well, as he may disconnect for some reason
      threadDelay (fromMaybe (-1) longWait)
#endif
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

    serverIdle :: forall (n :: N).
                  Nat n
               -> StatefulM (ServerState txid tx) n txid tx m
    serverIdle n = StatefulM $ \st -> case n of
        Zero -> do
          if canRequestMoreTxs st
          then do
            -- There are no replies in flight, but we do know some more txs we
            -- can ask for, so lets ask for them and more txids.
            traceWith tracer (TraceTxInboundCanRequestMoreTxs (natToInt n))
            pure $ continueWithState (serverReqTxs Zero) st

          else do
            traceWith tracer (TraceTxInboundCannotRequestMoreTxs (natToInt n))
            -- There's no replies in flight, and we have no more txs we can
            -- ask for so the only remaining thing to do is to ask for more
            -- txids. Since this is the only thing to do now, we make this a
            -- blocking call.
            let numTxIdsToRequest = maxTxIdsToRequest `min` maxUnacked
            assert (requestedTxIdsInFlight st == 0
                  && Seq.null (unacknowledgedTxIds st)
                  && Map.null (availableTxids st)
                  && Map.null (bufferedTxs st)) $
              pure $
              SendMsgRequestTxIdsBlocking
                (NumTxIdsToAck (numTxsToAcknowledge st))
                (NumTxIdsToReq numTxIdsToRequest)
                -- Our result if the client terminates the protocol
                (traceWith tracer TraceTxInboundTerminated)
                ( collectAndContinueWithState (handleReply Zero) st {
                    numTxsToAcknowledge    = 0,
                    requestedTxIdsInFlight = numTxIdsToRequest
                  }
                . CollectTxIds (NumTxIdsToReq numTxIdsToRequest)
                . NonEmpty.toList)

        Succ n' -> if canRequestMoreTxs st
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
              (Just (pure $ continueWithState (serverReqTxs (Succ n')) st))
              (collectAndContinueWithState (handleReply n') st)

          else do
            traceWith tracer (TraceTxInboundCannotRequestMoreTxs (natToInt n))
            -- In this case there is nothing else to do so we block until we
            -- collect a reply.
            pure $ CollectPipelined
              Nothing
              (collectAndContinueWithState (handleReply n') st)
      where
        canRequestMoreTxs :: ServerState k tx -> Bool
        canRequestMoreTxs st =
            not (Map.null (availableTxids st))

    handleReply :: forall (n :: N).
                   Nat n
                -> StatefulCollect (ServerState txid tx) n txid tx m
    handleReply n = StatefulCollect $ \st collect -> case collect of
      CollectTxIds (NumTxIdsToReq reqNo) txids -> do
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
        let st' = st {
          requestedTxIdsInFlight = requestedTxIdsInFlight st - reqNo
        }
        mpSnapshot <- atomically mempoolGetSnapshot
        continueWithStateM
          (serverIdle n)
          (acknowledgeTxIds st' txidsSeq txidsMap mpSnapshot)

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

            -- We can match up all the txids we requested, with those we
            -- received.
        let txIdsRequestedWithTxsReceived :: Map txid (Maybe tx)
            txIdsRequestedWithTxsReceived =
                Map.map Just txsMap
             <> Map.fromSet (const Nothing) txidsRequested

            -- We still have to acknowledge the txids we were given. This
            -- combined with the fact that we request txs out of order means
            -- our bufferedTxs has to track all the txids we asked for, even
            -- though not all have replies.
            bufferedTxs1 = bufferedTxs st <> txIdsRequestedWithTxsReceived

            -- We have to update the unacknowledgedTxIds here eagerly and not
            -- delay it to serverReqTxs, otherwise we could end up blocking in
            -- serverIdle on more pipelined results rather than being able to
            -- move on.

            -- Check if having received more txs we can now confirm any (in
            -- strict order in the unacknowledgedTxIds sequence).
            (acknowledgedTxIds, unacknowledgedTxIds') =
              Seq.spanl (`Map.member` bufferedTxs1) (unacknowledgedTxIds st)

            -- If so we can submit the acknowledged txs to our local mempool
            txsReady = foldr (\txid r -> maybe r (:r) (bufferedTxs1 Map.! txid))
                             [] acknowledgedTxIds

            -- And remove acknowledged txs from our buffer
            bufferedTxs2 = Foldable.foldl' (flip Map.delete)
                                   bufferedTxs1 acknowledgedTxIds

            -- If we are acknowledging transactions that are still in
            -- unacknowledgedTxIds' we need to re-add them so that we also can
            -- acknowledge them again later. This will happen in case of
            -- duplicate txids within the same window.
            live = filter (`elem` unacknowledgedTxIds') $ toList acknowledgedTxIds
            bufferedTxs3 = forceElemsToWHNF $ bufferedTxs2 <>
                               Map.fromList (zip live (repeat Nothing))

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
          bufferedTxs         = bufferedTxs3,
          unacknowledgedTxIds = unacknowledgedTxIds',
          numTxsToAcknowledge = numTxsToAcknowledge st
                              + fromIntegral (Seq.length acknowledgedTxIds)
        }

    -- Pre-emptively acknowledge those of the available transaction IDs that
    -- are already in the mempool and return the updated 'ServerState'.
    --
    -- This enables us to effectively filter out transactions that we don't
    -- need to bother requesting from the client since they're already in the
    -- mempool.
    --
    acknowledgeTxIds :: ServerState txid tx
                     -> StrictSeq txid
                     -> Map txid SizeInBytes
                     -> MempoolSnapshot txid tx idx
                     -> ServerState txid tx
    acknowledgeTxIds st txidsSeq _ _ | Seq.null txidsSeq  = st
    acknowledgeTxIds st txidsSeq txidsMap MempoolSnapshot{mempoolHasTx} =
        -- Return the next 'ServerState'
        st {
          availableTxids      = availableTxids',
          bufferedTxs         = bufferedTxs'',
          unacknowledgedTxIds = unacknowledgedTxIds'',
          numTxsToAcknowledge = numTxsToAcknowledge st
                              + fromIntegral (Seq.length acknowledgedTxIds)
        }
      where

        -- Divide the new txids in two: those that are already in the
        -- mempool or in flight and those that are not. We'll request some txs from the
        -- latter.
        (ignoredTxids, availableTxidsMp) =
              Map.partitionWithKey
                (\txid _ -> mempoolHasTx txid)
                txidsMap

        availableTxidsU =
              Map.filterWithKey
                (\txid _ -> notElem txid (unacknowledgedTxIds st))
                txidsMap

        availableTxids' = availableTxids st <> Map.intersection availableTxidsMp availableTxidsU

        -- The txs that we intentionally don't request, because they are
        -- already in the mempool, need to be acknowledged.
        --
        -- So we extend bufferedTxs with those txs (so of course they have
        -- no corresponding reply).
        bufferedTxs' = bufferedTxs st
                    <> Map.map (const Nothing) ignoredTxids

        unacknowledgedTxIds' = unacknowledgedTxIds st <> txidsSeq

        -- Check if having decided not to request more txs we can now
        -- confirm any txids (in strict order in the unacknowledgedTxIds
        -- sequence). This is used in the 'numTxsToAcknowledge' below
        -- which will then be used next time we SendMsgRequestTxIds.
        --
        (acknowledgedTxIds, unacknowledgedTxIds'') =
          Seq.spanl (`Map.member` bufferedTxs') unacknowledgedTxIds'


        -- If so we can remove acknowledged txs from our buffer provided that they
        -- are not still in unacknowledgedTxIds''. This happens in case of duplicate
        -- txids.
        bufferedTxs'' = forceElemsToWHNF $ Foldable.foldl' (\m txid -> if elem txid unacknowledgedTxIds''
                                              then m
                                              else Map.delete txid m)
                                bufferedTxs' acknowledgedTxIds

    serverReqTxs :: forall (n :: N).
                    Nat n
                 -> Stateful (ServerState txid tx) n txid tx m
    serverReqTxs n = Stateful $ \st -> do
        -- TODO: This implementation is deliberately naive, we pick in an
        -- arbitrary order and up to a fixed limit. This is to illustrate
        -- that we can request txs out of order. In the final version we will
        -- try to pick in-order and only pick out of order when we have to.
        -- We will also uses the size of txs in bytes as our limit for
        -- upper and lower watermarks for pipelining. We'll also use the
        -- amount in flight and delta-Q to estimate when we're in danger of
        -- becoming idle, and need to request stalled txs.
        --
        let (txsToRequest, availableTxids') =
              Map.splitAt (fromIntegral maxTxToRequest) (availableTxids st)

        SendMsgRequestTxsPipelined
          (Map.keys txsToRequest)
          (continueWithStateM (serverReqTxIds (Succ n)) st {
             availableTxids = availableTxids'
           })

    serverReqTxIds :: forall (n :: N).
                      Nat n
                   -> StatefulM (ServerState txid tx) n txid tx m
    serverReqTxIds n = StatefulM $ \st -> do
          -- This definition is justified by the fact that the
          -- 'numTxsToAcknowledge' are not included in the
          -- 'unacknowledgedTxIds'.
      let numTxIdsToRequest =
                  (maxUnacked
                    - fromIntegral (Seq.length (unacknowledgedTxIds st))
                    - requestedTxIdsInFlight st)
            `min` maxTxIdsToRequest

      if numTxIdsToRequest > 0
        then pure $ SendMsgRequestTxIdsPipelined
          (NumTxIdsToAck (numTxsToAcknowledge st))
          (NumTxIdsToReq numTxIdsToRequest)
          (continueWithStateM (serverIdle (Succ n)) st {
                requestedTxIdsInFlight = requestedTxIdsInFlight st
                                       + numTxIdsToRequest,
                numTxsToAcknowledge    = 0
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

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}

module Ouroboros.Network.Protocol.TxSubmission.Examples
  ( txSubmissionClient
  , txSubmissionServer
  , TraceEventClient (..)
  , TraceEventServer (..)
  ) where

import           Data.Foldable as Foldable (foldl', toList)
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as Seq
import           Data.Word (Word16)

import           Control.Exception (assert)
import           Control.Monad (when)
import           Control.Tracer (Tracer, traceWith)

import           Network.TypedProtocol.Pipelined (N, Nat (..))

import           Ouroboros.Network.Protocol.TxSubmission.Client
import           Ouroboros.Network.Protocol.TxSubmission.Server


--
-- Example client
--

data TraceEventClient txid tx =
     EventRecvMsgRequestTxIds (StrictSeq txid) (Map txid tx) [tx] Word16 Word16
   | EventRecvMsgRequestTxs   (StrictSeq txid) (Map txid tx) [tx] [txid]
  deriving Show

-- | An example @'TxSubmissionClient'@ which sends transactions from a fixed
-- list of transactions.
--
-- It is intended to illustrate the protocol or for use in tests. The client
-- enforces aspects of the protocol. It will fail with a protocol error if
-- the peer asks for a transaction which is not in the unacknowledged set.
-- The unacknowledged set is managed such that things are removed after having
-- been requested. The net effect is that the peer can only ask for
-- * If a server will ask for
-- the same transaction twice.
--
txSubmissionClient
  :: forall txid tx m.
     (Ord txid, Show txid, Monad m)
  => Tracer m (TraceEventClient txid tx)
  -> (tx -> txid)
  -> (tx -> TxSizeInBytes)
  -> Word16  -- ^ Maximum number of unacknowledged txids allowed
  -> [tx]
  -> TxSubmissionClient txid tx m ()
txSubmissionClient tracer txId txSize maxUnacked =
    TxSubmissionClient . pure . client Seq.empty Map.empty
  where
    client :: StrictSeq txid -> Map txid tx -> [tx] -> ClientStIdle txid tx m ()
    client !unackedSeq !unackedMap remainingTxs =
        assert invariant
        ClientStIdle { recvMsgRequestTxIds, recvMsgRequestTxs }
      where
        -- The entries in the unackedMap are a subset of those in the
        -- unackedSeq. The sequence is all of them, whereas we remove
        -- entries from the map once they are requested, to enforce
        -- that each tx can be requested at most once.
        invariant =
          Map.isSubmapOfBy
            (\_ _ -> True)
            unackedMap
            (Map.fromList [ (x, ()) | x <- Foldable.toList unackedSeq ])

        recvMsgRequestTxIds :: forall blocking.
                               TokBlockingStyle blocking
                            -> Word16
                            -> Word16
                            -> m (ClientStTxIds blocking txid tx m ())
        recvMsgRequestTxIds blocking ackNo reqNo = do
          traceWith tracer (EventRecvMsgRequestTxIds unackedSeq unackedMap
                                                     remainingTxs ackNo reqNo)
          when (ackNo > fromIntegral (Seq.length unackedSeq)) $
            error $ "txSubmissionClientConst.recvMsgRequestTxIds: "
                 ++ "peer acknowledged more txids than possible"

          when (  fromIntegral (Seq.length unackedSeq)
                - ackNo
                + fromIntegral reqNo
                > maxUnacked) $
            error $ "txSubmissionClientConst.recvMsgRequestTxIds: "
                 ++ "peer requested more txids than permitted"

          let unackedSeq' = Seq.drop (fromIntegral ackNo) unackedSeq
              unackedMap' = foldl' (flip Map.delete) unackedMap
                                   (Seq.take (fromIntegral ackNo) unackedSeq)

          case blocking of
            TokBlocking | not (Seq.null unackedSeq')
              -> error $ "txSubmissionClientConst.recvMsgRequestTxIds: "
                      ++ "peer made a blocking request for more txids when "
                      ++ "there are still unacknowledged txids."
            _ -> return ()

          -- This example is eager, it always provides as many as asked for,
          -- up to the number remaining available.
          let unackedExtra   = take (fromIntegral reqNo) remainingTxs
              unackedSeq''   = unackedSeq'
                            <> Seq.fromList (map txId unackedExtra)
              unackedMap''   = unackedMap'
                            <> Map.fromList [ (txId tx, tx)
                                            | tx <- unackedExtra ]
              remainingTxs'  = drop (fromIntegral reqNo) remainingTxs
              txIdAndSize tx = (txId tx, txSize tx)

          return $! case (blocking, unackedExtra) of
            (TokBlocking, []) ->
              SendMsgDone ()

            (TokBlocking, tx:txs) ->
              SendMsgReplyTxIds
                (BlockingReply (fmap txIdAndSize (tx :| txs)))
                (client unackedSeq'' unackedMap'' remainingTxs')

            (TokNonBlocking, txs) ->
              SendMsgReplyTxIds
                (NonBlockingReply (map txIdAndSize txs))
                (client unackedSeq'' unackedMap'' remainingTxs')

        recvMsgRequestTxs :: [txid]
                          -> m (ClientStTxs txid tx m ())
        recvMsgRequestTxs txids = do
          traceWith tracer (EventRecvMsgRequestTxs unackedSeq unackedMap
                                                   remainingTxs txids)
          case [ txid | txid <- txids, txid `Map.notMember` unackedMap ] of
            [] -> pure (SendMsgReplyTxs txs client')
              where
                txs         = map (unackedMap Map.!) txids
                client'     = client unackedSeq unackedMap' remainingTxs
                unackedMap' = foldr Map.delete unackedMap txids
                -- Here we remove from the map, while the seq stays unchanged.
                -- This enforces that each tx can be requested at most once.

            missing -> error $ "txSubmissionClientConst.recvMsgRequestTxs: "
                            ++ "requested missing TxIds: " ++ show missing


--
-- Example server
--

data TraceEventServer txid tx =
     EventRequestTxIdsBlocking  (ServerState txid tx) Word16 Word16
   | EventRequestTxIdsPipelined (ServerState txid tx) Word16 Word16
   | EventRequestTxsPipelined   (ServerState txid tx) [txid]

deriving instance (Show txid, Show tx) => Show (TraceEventServer txid tx)

data ServerState txid tx = ServerState {
       -- | The number of transaction identifiers that we have requested but
       -- which have not yet been replied to. We need to track this it keep
       -- our requests within the limit on the number of unacknowledged txids.
       --
       requestedTxIdsInFlight :: Word16,

       -- | Those transactions (by their identifier) that the client has told
       -- us about, and which we have not yet acknowledged. This is kept in
       -- the order in which the client gave them to us. This is the same order
       -- in which we submit them to the mempool (or for this example, the final
       -- result order). It is also the order we acknowledge in.
       unacknowledgedTxIds    :: StrictSeq txid,

       -- | Those transactions (by their identifier) that we can request. These
       -- are a subset of the 'unacknowledgedTxIds' that we have not yet
       -- requested. This is not ordered to illustrate the fact that we can
       -- request txs out of order. We also remember the sizes, though this
       -- example does not make use of the size information.
       availableTxids         :: Map txid TxSizeInBytes,

       -- | Transactions we have successfully downloaded but have not yet added
       -- to the mempool or acknowledged. This is needed because we request
       -- transactions out of order but we must use the original order when
       -- adding to the mempool or acknowledging transactions.
       --
       bufferedTxs            :: Map txid (Maybe tx),

       -- | The number of transactions we can acknowledge on our next request
       -- for more transactions. The number here have already been removed from
       -- 'unacknowledgedTxIds'.
       --
       numTxsToAcknowledge    :: Word16
     }
  deriving Show

initialServerState :: ServerState txid tx
initialServerState = ServerState 0 Seq.empty Map.empty Map.empty 0


-- | An example transaction submission server.
--
-- It collects and returns all the transactions that the client submits. This
-- is suitable for tests and using as a starting template for a full version.
--
-- Note that this example does not respect any overall byte limit on pipelining
-- and does not make any delta-Q info to optimises the pipelining decisions.
--
txSubmissionServer
  :: forall txid tx m.
     (Ord txid, Monad m)
  => Tracer m (TraceEventServer txid tx)
  -> (tx -> txid)
  -> Word16  -- ^ Maximum number of unacknowledged txids
  -> Word16  -- ^ Maximum number of txids to request in any one go
  -> Word16  -- ^ Maximum number of txs to request in any one go
  -> TxSubmissionServerPipelined txid tx m [tx]
txSubmissionServer tracer txId maxUnacked maxTxIdsToRequest maxTxToRequest =
    TxSubmissionServerPipelined (pure $ serverIdle [] Zero initialServerState)
  where
    serverIdle :: forall (n :: N).
                  [tx]
               -> Nat n
               -> ServerState txid tx
               -> ServerStIdle n txid tx m [tx]
    serverIdle accum Zero st
        -- There are no replies in flight, but we do know some more txs we can
        -- ask for, so lets ask for them and more txids.
      | canRequestMoreTxs st
      = serverReqTxs accum Zero st

        -- There's no replies in flight, and we have no more txs we can ask for
        -- so the only remaining thing to do is to ask for more txids. Since
        -- this is the only thing to do now, we make this a blocking call.
      | otherwise
      , let numTxIdsToRequest = maxTxIdsToRequest `min` maxUnacked
      = assert (requestedTxIdsInFlight st == 0
             && Seq.null (unacknowledgedTxIds st)
             && Map.null (availableTxids st)
             && Map.null (bufferedTxs st)) $
        SendMsgRequestTxIdsBlocking
          (numTxsToAcknowledge st)
          numTxIdsToRequest
          (pure accum)               -- result if the client reports we're done
          (\txids -> do
              traceWith tracer (EventRequestTxIdsBlocking st (numTxsToAcknowledge st) numTxIdsToRequest)
              handleReply accum Zero st {
                 numTxsToAcknowledge    = 0,
                 requestedTxIdsInFlight = numTxIdsToRequest
               }
               . CollectTxIds numTxIdsToRequest
               . NonEmpty.toList $ txids)

    serverIdle accum (Succ n) st
        -- We have replies in flight and we should eagerly collect them if
        -- available, but there are transactions to request too so we should
        -- not block waiting for replies.
        --
        -- Having requested more transactions, we opportunistically ask for
        -- more txids in a non-blocking way. This is how we pipeline asking for
        -- both txs and txids.
        --
        -- It's important not to pipeline more requests for txids when we have
        -- no txs to ask for, since (with no other guard) this will put us into
        -- a busy-polling loop.
        --
      | canRequestMoreTxs st
      = CollectPipelined
          (Just (serverReqTxs accum (Succ n) st))
          (handleReply accum n st)

        -- In this case there is nothing else to do so we block until we
        -- collect a reply.
      | otherwise
      = CollectPipelined
          Nothing
          (handleReply accum n st)

    canRequestMoreTxs :: ServerState k tx -> Bool
    canRequestMoreTxs st =
        not (Map.null (availableTxids st))

    handleReply :: forall (n :: N).
                   [tx]
                -> Nat n
                -> ServerState txid tx
                -> Collect txid tx
                -> m (ServerStIdle n txid tx m [tx])
    handleReply accum n st (CollectTxIds reqNo txids) =
      -- Upon receiving a batch of new txids we extend our available set,
      -- and extended the unacknowledged sequence.
      return $ serverIdle accum n st {
        requestedTxIdsInFlight = requestedTxIdsInFlight st - reqNo,
        unacknowledgedTxIds    = unacknowledgedTxIds st
                              <> Seq.fromList (map fst txids),
        availableTxids         = availableTxids st
                              <> Map.fromList txids
      }

    handleReply accum n st (CollectTxs txids txs) =
      -- When we receive a batch of transactions, in general we get a subset of
      -- those that we asked for, with the remainder now deemed unnecessary.
      -- But we still have to acknowledge the txids we were given. This combined
      -- with the fact that we request txs out of order means our bufferedTxs
      -- has to track all the txids we asked for, even though not all have
      -- replies.
      --
      -- We have to update the unacknowledgedTxIds here eagerly and not delay it
      -- to serverReqTxs, otherwise we could end up blocking in serverIdle on
      -- more pipelined results rather than being able to move on.
      return $ serverIdle accum' n st {
        bufferedTxs         = bufferedTxs'',
        unacknowledgedTxIds = unacknowledgedTxIds',
        numTxsToAcknowledge = numTxsToAcknowledge st
                            + fromIntegral (Seq.length acknowledgedTxIds)
      }
      where
        txIdsRequestedWithTxsReceived :: [(txid, Maybe tx)]
        txIdsRequestedWithTxsReceived =
          [ (txid, mbTx)
          | let txsMap :: Map txid tx
                txsMap = Map.fromList [ (txId tx, tx) | tx <- txs ]
          , txid <- txids
          , let !mbTx = Map.lookup txid txsMap
          ]

        bufferedTxs'  = bufferedTxs st
                     <> Map.fromList txIdsRequestedWithTxsReceived

        -- Check if having received more txs we can now confirm any (in strict
        -- order in the unacknowledgedTxIds sequence).
        (acknowledgedTxIds, unacknowledgedTxIds') =
          Seq.spanl (`Map.member` bufferedTxs') (unacknowledgedTxIds st)

        -- If so we can add the acknowledged txs to our accumulating result
        accum' = accum
              ++ foldr (\txid r -> maybe r (:r) (bufferedTxs' Map.! txid)) []
                       acknowledgedTxIds

        -- And remove acknowledged txs from our buffer
        bufferedTxs'' = foldl' (flip Map.delete) bufferedTxs' acknowledgedTxIds


    serverReqTxs :: forall (n :: N).
                    [tx]
                 -> Nat n
                 -> ServerState txid tx
                 -> ServerStIdle n txid tx m [tx]
    serverReqTxs accum n st =
        SendMsgRequestTxsPipelined
          (Map.keys txsToRequest)
          (do traceWith tracer (EventRequestTxsPipelined st (Map.keys txsToRequest))
              pure $ serverReqTxIds accum (Succ n) st {
                availableTxids = availableTxids'
              })
      where
        -- This implementation is deliberately naive, we pick in an arbitrary
        -- order and up to a fixed limit. The real thing should take account of
        -- the expected transaction sizes, to pipeline well and keep within
        -- pipelining byte limits.
        (txsToRequest, availableTxids') =
          Map.splitAt (fromIntegral maxTxToRequest) (availableTxids st)

    serverReqTxIds :: forall (n :: N).
                      [tx]
                   -> Nat n
                   -> ServerState txid tx
                   -> ServerStIdle n txid tx m [tx]
    serverReqTxIds accum n st
      | numTxIdsToRequest > 0
      = SendMsgRequestTxIdsPipelined
          (numTxsToAcknowledge st)
          numTxIdsToRequest
          (do traceWith tracer (EventRequestTxIdsPipelined st (numTxsToAcknowledge st) numTxIdsToRequest)
              pure $ serverIdle accum (Succ n) st {
                requestedTxIdsInFlight = requestedTxIdsInFlight st
                                       + numTxIdsToRequest,
                numTxsToAcknowledge    = 0
              })

      | otherwise
      = serverIdle accum n st
      where
        -- This definition is justified by the fact that the
        -- 'numTxsToAcknowledge' are not included in the 'unacknowledgedTxIds'.
        numTxIdsToRequest =
                (maxUnacked
                  - fromIntegral (Seq.length (unacknowledgedTxIds st))
                  - requestedTxIdsInFlight st)
          `min` maxTxIdsToRequest

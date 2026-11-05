{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Unit properties of `txSubmissionOutbound`, driven directly by the
-- requests of the inbound side.
--
module Test.Ouroboros.Network.TxSubmission.Outbound (tests) where

import Control.Monad.Class.MonadThrow (try)
import Control.Monad.IOSim (runSimOrThrow)
import Control.Tracer (nullTracer)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Word (Word16)

import Ouroboros.Network.ControlMessage (ControlMessage (..))
import Ouroboros.Network.Protocol.TxSubmission2.Client
import Ouroboros.Network.Protocol.TxSubmission2.Type
import Ouroboros.Network.TxSubmission.Outbound

import Test.Ouroboros.Network.TxSubmission.Types

import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)


tests :: TestTree
tests =
  testGroup "Outbound"
    [ testProperty "blocking request with a zero window is a protocol error"
                   prop_blockingRequest_zeroWindow
    , testProperty "blocking request with a positive window is replied"
                   prop_blockingRequest_positiveWindow
    , testProperty "non-blocking requests are clamped to the window"
                   prop_nonBlockingRequests_clampedToWindow
    ]


-- | Send a blocking `MsgRequestTxIds` for `reqNo` txids to a fresh
-- `txSubmissionOutbound` whose mempool holds `n` txs.  Returns the protocol
-- error, or the txids of the reply (`Nothing` for `MsgDone`).
--
blockingRequest :: TxSubmissionConfig
                -> TxOutboundVersion
                -> NumTxIdsToReq
                -> Word16
                -> Either TxSubmissionProtocolError (Maybe [TxId])
blockingRequest config version reqNo n = runSimOrThrow $ do
    mempool <- newMempool
                 [ Tx { getTxId      = txid,
                        getTxSize    = 100,
                        getTxAdvSize = 100,
                        getTxValid   = True,
                        getTxParent  = Nothing }
                 | txid <- [1 .. fromIntegral n] ]
    ClientStIdle { recvMsgRequestTxIds } <-
      runTxSubmissionClient $
        txSubmissionOutbound nullTracer config (getMempoolReader mempool)
                             version (pure Continue)
    reply <- try (recvMsgRequestTxIds SingBlocking 0 reqNo)
    pure $ case reply of
      Left err -> Left err
      Right (SendMsgReplyTxIds (BlockingReply txids) _) ->
        Right (Just (fst <$> NonEmpty.toList txids))
      Right (SendMsgDone ()) -> Right Nothing


-- | A zero window is a misconfiguration of our own.  The first blocking
-- request is a protocol error under both versions: under `TxOutboundV_1`
-- because the request exceeds the window, under `TxOutboundV_2` because the
-- request is clamped to zero txids, which a blocking reply cannot carry.
--
prop_blockingRequest_zeroWindow :: ArbTxOutboundVersion
                                -> Positive (Small Word16)
                                -> Positive (Small Word16)
                                -> Property
prop_blockingRequest_zeroWindow (ArbTxOutboundVersion version)
                                (Positive (Small reqNo))
                                (Positive (Small n)) =
    let config = TxSubmissionConfig {
                   maxNumUnacknowledgedTxIds = 0,
                   maxNumTxIdsToRequest      = NumTxIdsToReq reqNo
                 }
    in label (show version) $
       case blockingRequest config version (NumTxIdsToReq reqNo) n of
         Left ProtocolErrorRequestedTooManyTxids {} -> property True
         result -> counterexample (show result) False


-- | With a positive window, a blocking request within it is replied with
-- the first txs in the mempool.  Under `TxOutboundV_2` a request above the
-- window is clamped to it.
--
prop_blockingRequest_positiveWindow :: ArbTxOutboundVersion
                                    -> Positive (Small Word16)
                                    -> Positive (Small Word16)
                                    -> Positive (Small Word16)
                                    -> Property
prop_blockingRequest_positiveWindow (ArbTxOutboundVersion version)
                                    (Positive (Small window))
                                    (Positive (Small reqNo0))
                                    (Positive (Small n)) =
    let config = TxSubmissionConfig {
                   maxNumUnacknowledgedTxIds = NumTxIdsToAck window,
                   maxNumTxIdsToRequest      = NumTxIdsToReq reqNo
                 }
        -- under `TxOutboundV_1` a request above the window is a protocol
        -- error
        reqNo = case version of
          TxOutboundV_1 -> min reqNo0 window
          TxOutboundV_2 -> reqNo0
        expected = take (fromIntegral (min reqNo window)) [1 .. fromIntegral n]
    in label (show version) $
       classify (reqNo > window) "request clamped" $
       case blockingRequest config version (NumTxIdsToReq reqNo) n of
         Right (Just txids) -> txids === expected
         result             -> counterexample (show result) False


-- | A `MsgRequestTxIds`.
--
data Request = Blocking    NumTxIdsToAck NumTxIdsToReq
             | NonBlocking NumTxIdsToAck NumTxIdsToReq
  deriving Show

-- | Send the requests in turn to a fresh `txSubmissionOutbound` whose mempool
-- holds `n` txs.  Returns the txids of each reply, up to the first protocol
-- error or `MsgDone`.
--
runRequests :: TxSubmissionConfig
            -> TxOutboundVersion
            -> Word16
            -> [Request]
            -> ([[TxId]], Maybe TxSubmissionProtocolError)
runRequests config version n requests = runSimOrThrow $ do
    mempool <- newMempool
                 [ Tx { getTxId      = txid,
                        getTxSize    = 100,
                        getTxAdvSize = 100,
                        getTxValid   = True,
                        getTxParent  = Nothing }
                 | txid <- [1 .. fromIntegral n] ]
    client <-
      runTxSubmissionClient $
        txSubmissionOutbound nullTracer config (getMempoolReader mempool)
                             version (pure Continue)
    go client requests []
  where
    go _ [] replies = pure (reverse replies, Nothing)
    go ClientStIdle { recvMsgRequestTxIds } (request : rest) replies = do
      reply <- try $ case request of
        Blocking ackNo reqNo -> do
          r <- recvMsgRequestTxIds SingBlocking ackNo reqNo
          pure $ case r of
            SendMsgReplyTxIds (BlockingReply txids) k ->
              Just (fst <$> NonEmpty.toList txids, k)
            SendMsgDone () -> Nothing
        NonBlocking ackNo reqNo -> do
          SendMsgReplyTxIds (NonBlockingReply txids) k <-
            recvMsgRequestTxIds SingNonBlocking ackNo reqNo
          pure (Just (fst <$> txids, k))
      case reply of
        Left err                -> pure (reverse replies, Just err)
        Right Nothing           -> pure (reverse replies, Nothing)
        Right (Just (txids, k)) -> go k rest (txids : replies)


-- | With a full window, `TxOutboundV_2` clamps non-blocking requests to the
-- room left in the window:
--
-- 1. a blocking request fills the window of `w` txids;
-- 2. a request which acknowledges nothing is clamped to zero: the reply is
--    empty and the window stays full;
-- 3. a request which acknowledges `k` txids gets at most `k` new txids;
-- 4. a request which only acknowledges a txid gets an empty reply;
-- 5. the next request gets the room freed by steps 3 and 4.
--
prop_nonBlockingRequests_clampedToWindow :: Positive (Small Word16)
                                         -> Positive (Small Word16)
                                         -> Positive (Small Word16)
                                         -> Property
prop_nonBlockingRequests_clampedToWindow (Positive (Small w0))
                                         (Positive (Small k0))
                                         (Positive (Small r)) =
    let -- `w >= 2` and `k < w` keep at least two txids unacknowledged
        -- after step 3, so that step 4 leaves the queue non-empty, as
        -- a non-blocking request requires
        w = 1 + w0
        k = 1 + (k0 - 1) `mod` (w - 1)
        config = TxSubmissionConfig {
                   maxNumUnacknowledgedTxIds = NumTxIdsToAck w,
                   maxNumTxIdsToRequest      = NumTxIdsToReq r
                 }
        txid :: Word16 -> TxId
        txid = fromIntegral
        -- txids returned in step 3
        m = min r k
        -- room left after step 4: `w - (w - k + m - 1)`
        room = k - m + 1
        requests =
          [ Blocking    0                 (NumTxIdsToReq w)
          , NonBlocking 0                 (NumTxIdsToReq r)
          , NonBlocking (NumTxIdsToAck k) (NumTxIdsToReq r)
          , NonBlocking 1                 0
          , NonBlocking 0                 (NumTxIdsToReq r)
          ]
        expected =
          [ txid <$> [1 .. w]
          , []
          , txid <$> [w + 1 .. w + m]
          , []
          , txid <$> [w + m + 1 .. w + m + min r room]
          ]
    in classify (r > k) "step 3 clamped" $
       classify (r > room) "step 5 clamped" $
       case runRequests config TxOutboundV_2 (3 * w + r) requests of
         (replies, Nothing) -> replies === expected
         result             -> counterexample (show result) False

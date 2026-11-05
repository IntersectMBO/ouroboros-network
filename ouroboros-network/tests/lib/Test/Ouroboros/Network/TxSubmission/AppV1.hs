{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Ouroboros.Network.TxSubmission.AppV1 (tests) where

import Prelude hiding (seq)

import NoThunks.Class

import Control.Concurrent.Class.MonadMVar (MonadMVar)
import Control.Concurrent.Class.MonadSTM
import Control.Monad (unless, void)
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadSay
import Control.Monad.Class.MonadST
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import Control.Monad.IOSim hiding (SimResult)
import Control.Tracer (Tracer (..), contramap, nullTracer)

import Data.ByteString.Lazy qualified as BSL
import Data.Function (on)
import Data.List (intercalate, nubBy)
import Data.Maybe (fromMaybe)

import Network.TypedProtocol.Codec (CodecF (..))

import Ouroboros.Network.Channel
import Ouroboros.Network.ControlMessage (ControlMessage (..), ControlMessageSTM)
import Ouroboros.Network.Driver
import Ouroboros.Network.Protocol.Limits (sizeLimitForState)
import Ouroboros.Network.Protocol.TxSubmission2.Client
import Ouroboros.Network.Protocol.TxSubmission2.Codec
import Ouroboros.Network.Protocol.TxSubmission2.Server
import Ouroboros.Network.Protocol.TxSubmission2.Type
import Ouroboros.Network.TxSubmission.Inbound.V1
import Ouroboros.Network.TxSubmission.Inbound.V2.Policy (max_TX_SIZE)
import Ouroboros.Network.TxSubmission.Inbound.V2.Types
           (const_MAX_TX_SIZE_DISCREPANCY)
import Ouroboros.Network.TxSubmission.Outbound
import Ouroboros.Network.Util.ShowProxy

import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Test.Ouroboros.Network.TxSubmission.SizedBytes (SizedBytes (..),
           codec_sized)
import Test.Ouroboros.Network.TxSubmission.Types
import Test.Ouroboros.Network.Utils


tests :: TestTree
tests = testGroup "AppV1"
  [ testProperty "txSubmission" (checkCoverage prop_txSubmission)
  , testProperty "maxNumTxsToRequest is clamped to the sane range"
                 prop_clampMaxNumTxsToRequest
  , testProperty "maxSaneNumTxsToRequest txs fit within the size limit"
                 prop_maxSaneNumTxsToRequest_fitsSizeLimit
  ]

data TestVersion = TestVersion
  deriving (Eq, Ord, Bounded, Enum, Show)

txSubmissionSimulation
  :: forall m txid.
     ( MonadAsync m
     , MonadDelay m
     , MonadEvaluate m
     , MonadFork  m
     , MonadMask  m
     , MonadMVar  m
     , MonadSay   m
     , MonadST    m
     , MonadSTM   m
     , MonadTime  m
     , MonadTimer m
     , MonadThrow m
     , MonadThrow (STM m)
     , MonadMonotonicTime m
     , Ord txid
     , Eq  txid
     , ShowProxy txid
     , NoThunks (Tx txid)

     , txid ~ Int
     )
  => Tracer m (String, TraceSendRecv (TxSubmission2 txid (Tx txid)))
  -> Tracer m OutboundRequest
  -- ^ requests received by the outbound, see `traceOutboundRequests`
  -> TxSubmissionConfig
  -- ^ inbound config
  -> TxSubmissionConfig
  -- ^ outbound config
  -> NumTxsToReq
  -> TxOutboundVersion
  -> [Tx txid]
  -> Maybe Int
  -- ^ number of txs expected in the inbound mempool; the simulation stops
  -- once they arrived, or at the deadline
  -> DiffTime
  -- ^ deadline
  -> Maybe DiffTime
  -> Maybe DiffTime
  -> m ([Tx txid], [Tx txid])
txSubmissionSimulation tracer
                       requestTracer
                       inboundConfig
                       outboundConfig
                       maxNumTxsToRequest
                       version
                       outboundTxs
                       expectedTxs
                       deadline
                       inboundDelay
                       outboundDelay = do

    controlMessageVar <- newTVarIO Continue
    let controlMessageSTM = readTVar controlMessageVar
    inboundMempool  <- emptyMempool
    duplicateTxIdsVar <- newTVarIO []
    outboundMempool <- newMempool outboundTxs
    (outboundChannel, inboundChannel) <- createConnectedChannels
    outboundAsync <-
      async $ runPeerWithLimits
                (("OUTBOUND",) `contramap` tracer)
                txSubmissionCodec2
                (byteLimitsTxSubmission2 (fromIntegral . BSL.length))
                timeLimitsTxSubmission2
                (maybe id delayChannel outboundDelay outboundChannel)
                (txSubmissionClientPeer
                  (traceOutboundRequests
                    requestTracer
                    (maxNumUnacknowledgedTxIds outboundConfig)
                    (outboundPeer controlMessageSTM outboundMempool)))

    inboundAsync <-
      async $ runPipelinedPeerWithLimits
                (("INBOUND",) `contramap` verboseTracer)
                txSubmissionCodec2
                (byteLimitsTxSubmission2 (fromIntegral . BSL.length))
                timeLimitsTxSubmission2
                (maybe id delayChannel inboundDelay inboundChannel)
                (txSubmissionServerPeerPipelined (inboundPeer duplicateTxIdsVar inboundMempool))

    -- terminate the outbound once the expected txs arrived, or at the
    -- deadline
    terminateAsync <-
      async $ do
        let delivered expected = do
              inmp <- readMempool inboundMempool
              unless (length inmp >= expected) $
                threadDelay 1 >> delivered expected
        case expectedTxs of
          Nothing       -> threadDelay deadline
          Just expected -> void $ timeout deadline (delivered expected)
        atomically (writeTVar controlMessageVar Terminate)

    _ <- waitAnyCancel [ outboundAsync, inboundAsync ]
    cancel terminateAsync

    inmp <- readMempool inboundMempool
    outmp <- readMempool outboundMempool
    return (inmp, outmp)
  where
    outboundPeer :: ControlMessageSTM m
                 -> Mempool m txid (Tx txid)
                 -> TxSubmissionClient txid (Tx txid) m ()
    outboundPeer controlMessageSTM outboundMempool =
      txSubmissionOutbound
        nullTracer
        outboundConfig
        (getMempoolReader outboundMempool)
        version
        controlMessageSTM

    inboundPeer :: TVar m [txid]
                -> Mempool m txid (Tx txid)
                -> TxSubmissionServerPipelined txid (Tx txid) m ()
    inboundPeer duplicateTxIdsVar inboundMempool =
      txSubmissionInbound
        (("INBOUND",) `contramap` verboseTracer)
        NoTxSubmissionInitDelay
        inboundConfig
        maxNumTxsToRequest
        (getMempoolReader inboundMempool)
        (getMempoolWriter duplicateTxIdsVar inboundMempool)



prop_txSubmission :: ArbTxSubmissionConfigs
                  -> ArbNumTxsToRequest
                  -> NonEmptyList (Tx Int)
                  -> Maybe (Positive SmallDelay)
                  -- ^ The delay must be smaller (<) than 5s, so that overall
                  -- delay is less than 10s, otherwise 'smallDelay' in
                  -- 'timeLimitsTxSubmission2' will kick in.
                  -> Property
prop_txSubmission
    (ArbTxSubmissionConfigs version inboundConfig outboundConfig)
    (ArbNumTxsToRequest maxNumTxsToRequest)
    (NonEmpty outboundTxs)
    delay
    =
    let mbDelayTime = getSmallDelay . getPositive <$> delay
        outUniqueTxIds = nubBy (on (==) getTxId) outboundTxs
        outValidTxs    = filter getTxValid outboundTxs
        uniqueTxIds    = length outUniqueTxIds == length outboundTxs
        -- with unique txids every valid tx must arrive; otherwise run until
        -- the deadline
        expectedTxs | uniqueTxIds = Just (length outValidTxs)
                    | otherwise   = Nothing
        -- generous: each tx takes a few round trips
        deadline = 60 + 20 * fromMaybe 1 mbDelayTime
                            * realToFrac (length outboundTxs)
        tr = runSimTrace $
               txSubmissionSimulation
                 verboseTracer
                 dynamicTracer
                 inboundConfig
                 outboundConfig
                 maxNumTxsToRequest
                 version
                 outboundTxs
                 expectedTxs
                 deadline
                 mbDelayTime
                 mbDelayTime
        -- `selectTraceEventsDynamic'` doesn't throw if the simulation
        -- failed; failures are reported by `evaluateTrace`
        events = requestTxIdsEvents (selectTraceEventsDynamic' tr)
        inboundWindow  = maxNumUnacknowledgedTxIds inboundConfig
        outboundWindow = maxNumUnacknowledgedTxIds outboundConfig
    in
    label (show version) $
    classify (outboundWindow < inboundWindow) "smaller outbound window" $
    ioProperty $ do
        tr' <- evaluateTrace tr
        case tr' of
             SimException e trace -> do
                return $ counterexample (intercalate "\n" $ show e : trace) False
             SimDeadLock trace -> do
                 return $ counterexample (intercalate "\n" $ "Deadlock" : trace) False
             SimReturn (inmp, outmp) trace -> do
                 fmap ( tabulate "MsgRequestTxIds" (show <$> events)
                      . cover 10 (Clamped `elem` events) "clamped"
                      . cover 10 (ClampedToZero `elem` events) "clamped to zero"
                      ) $
                   -- the protocol only reads the outbound mempool
                   case (uniqueTxIds, length outValidTxs == length outmp) of
                      a@(True, True) ->
                          -- If we are presented with a stream of unique txids for valid
                          -- transactions the inbound transactions should match the outbound
                          -- transactions exactly: all of them arrived, in order.
                          return $ counterexample (intercalate "\n" trace)
                                 $ counterexample (show a)
                                 $ inmp === outValidTxs
                      a@(True, False) ->
                          -- If we are presented with a stream of unique txids then we should have
                          -- fetched all valid transactions.
                          return $ counterexample (intercalate "\n" trace)
                                 $ counterexample (show a)
                                 $ inmp === outValidTxs
                      a@(False, True) ->
                          -- If we are presented with a stream of valid txids then we should have
                          -- fetched some version of those transactions.
                          return $ counterexample (intercalate "\n" trace)
                                 $ counterexample (show a)
                                 $ counterexample (show (inmp, outmp))
                                 $ map getTxId inmp === take (length inmp)
                                                             (map getTxId $ filter getTxValid outUniqueTxIds)
                      (False, False)
                           -- If we are presented with a stream of valid and invalid Txs with
                           -- duplicate txids we're content with completing the protocol
                           -- without error.
                           -> return $ property True


-- | The number of txs the inbound requests at a time, including values
-- outside of the range checked by `saneMaxNumTxsToRequest`: `0` and values
-- above `maxSaneNumTxsToRequest`.
--
newtype ArbNumTxsToRequest = ArbNumTxsToRequest NumTxsToReq
  deriving Show

instance Arbitrary ArbNumTxsToRequest where
  arbitrary = ArbNumTxsToRequest . NumTxsToReq <$> frequency
    [ (3, pure (getNumTxsToReq defaultMaxNumTxsToRequest))
    , (1, pure 0)
    , (2, choose (1, getNumTxsToReq maxSaneNumTxsToRequest))
    , (1, choose (getNumTxsToReq maxSaneNumTxsToRequest + 1, maxBound))
    ]
  shrink (ArbNumTxsToRequest (NumTxsToReq n)) =
    ArbNumTxsToRequest . NumTxsToReq <$> shrink n


-- | `clampMaxNumTxsToRequest` returns a value accepted by
-- `saneMaxNumTxsToRequest`, and keeps such values unchanged.
--
prop_clampMaxNumTxsToRequest :: ArbNumTxsToRequest -> Property
prop_clampMaxNumTxsToRequest (ArbNumTxsToRequest n) =
    let sane = saneMaxNumTxsToRequest n
    in classify sane "sane" $
       counterexample (show (clampMaxNumTxsToRequest n)) $
            saneMaxNumTxsToRequest (clampMaxNumTxsToRequest n)
       .&&. (not sane || clampMaxNumTxsToRequest n == n)


-- | `maxSaneNumTxsToRequest` is the largest number of txs of `max_TX_SIZE`,
-- each `const_MAX_TX_SIZE_DISCREPANCY` larger, for which a `MsgReplyTxs`
-- encoded with `codecTxSubmission2` fits within the `StTxs` size limit.
--
prop_maxSaneNumTxsToRequest_fitsSizeLimit :: Property
prop_maxSaneNumTxsToRequest_fitsSizeLimit =
    once $
         counterexample ("reply with " ++ show n ++ " txs: " ++ show (replySize n))
           (replySize n <= limit)
    .&&. counterexample ("reply with " ++ show (n + 1) ++ " txs: " ++ show (replySize (n + 1)))
           (replySize (n + 1) > limit)
  where
    n :: Int
    n = fromIntegral maxSaneNumTxsToRequest

    tx :: SizedBytes
    tx = SizedBytes (fromIntegral (max_TX_SIZE + const_MAX_TX_SIZE_DISCREPANCY))

    replySize :: Int -> Integer
    replySize k =
      toInteger . BSL.length . encode (codec_sized @IO)
        $ (MsgReplyTxs (replicate k tx)
            :: Message (TxSubmission2 SizedBytes SizedBytes) StTxs StIdle)

    limit :: Integer
    limit = toInteger
          $ sizeLimitForState (byteLimitsTxSubmission2 (error "unused")) SingTxs

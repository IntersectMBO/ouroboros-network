{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Ouroboros.Network.TxSubmission.AppV1 (tests) where

import Prelude hiding (seq)

import NoThunks.Class

import Control.Concurrent.Class.MonadMVar (MonadMVar)
import Control.Concurrent.Class.MonadSTM
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
import Data.Word (Word16)

import Ouroboros.Network.Channel
import Ouroboros.Network.ControlMessage (ControlMessage (..), ControlMessageSTM)
import Ouroboros.Network.Driver
import Ouroboros.Network.NodeToNode (NodeToNodeVersion (..))
import Ouroboros.Network.Protocol.TxSubmission2.Client
import Ouroboros.Network.Protocol.TxSubmission2.Codec
import Ouroboros.Network.Protocol.TxSubmission2.Server
import Ouroboros.Network.Protocol.TxSubmission2.Type
import Ouroboros.Network.TxSubmission.Inbound
import Ouroboros.Network.TxSubmission.Outbound
import Ouroboros.Network.Util.ShowProxy

import Ouroboros.Network.Testing.Utils
import Test.Ouroboros.Network.TxSubmission.Types

import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

-- import Test.Ouroboros.Network.TxSubmission.Common hiding (tests)


tests :: TestTree
tests = testGroup "AppV1"
  [ testProperty "txSubmission" prop_txSubmission
  , testProperty "x" prop_x
  ]

txSubmissionSimulation
  :: forall m txid.
     ( MonadAsync m
     , MonadDelay m
     , MonadFork  m
     , MonadMask  m
     , MonadMVar  m
     , MonadSay   m
     , MonadST    m
     , MonadSTM   m
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
  -> NumTxIdsToAck
  -> [Tx txid]
  -> ControlMessageSTM m
  -> Maybe DiffTime
  -> Maybe DiffTime
  -> m ([Tx txid], [Tx txid])
txSubmissionSimulation tracer maxUnacked outboundTxs
                       controlMessageSTM
                       inboundDelay outboundDelay = do

    inboundMempool  <- emptyMempool
    outboundMempool <- newMempool outboundTxs
    (outboundChannel, inboundChannel) <- createConnectedChannels
    outboundAsync <-
      async $ runPeerWithLimits
                (("OUTBOUND",) `contramap` tracer)
                txSubmissionCodec2
                (byteLimitsTxSubmission2 (fromIntegral . BSL.length))
                timeLimitsTxSubmission2
                (maybe id delayChannel outboundDelay outboundChannel)
                (txSubmissionClientPeer (outboundPeer outboundMempool))

    inboundAsync <-
      async $ runPipelinedPeerWithLimits
                (("INBOUND",) `contramap` verboseTracer)
                txSubmissionCodec2
                (byteLimitsTxSubmission2 (fromIntegral . BSL.length))
                timeLimitsTxSubmission2
                (maybe id delayChannel inboundDelay inboundChannel)
                (txSubmissionServerPeerPipelined (inboundPeer inboundMempool))

    _ <- waitAnyCancel [ outboundAsync, inboundAsync ]

    inmp <- readMempool inboundMempool
    outmp <- readMempool outboundMempool
    return (inmp, outmp)
  where

    outboundPeer :: Mempool m txid -> TxSubmissionClient txid (Tx txid) m ()
    outboundPeer outboundMempool =
      txSubmissionOutbound
        nullTracer
        maxUnacked
        (getMempoolReader outboundMempool)
        NodeToNodeV_13
        controlMessageSTM

    inboundPeer :: Mempool m txid -> TxSubmissionServerPipelined txid (Tx txid) m ()
    inboundPeer inboundMempool =
      txSubmissionInbound
        nullTracer
        maxUnacked
        (getMempoolReader inboundMempool)
        (getMempoolWriter inboundMempool)
        NodeToNodeV_13

prop_txSubmission :: Positive Word16
                  -> NonEmptyList (Tx Int)
                  -> Maybe (Positive SmallDelay)
                  -- ^ The delay must be smaller (<) than 5s, so that overall
                  -- delay is less than 10s, otherwise 'smallDelay' in
                  -- 'timeLimitsTxSubmission2' will kick in.
                  -> Property
prop_txSubmission (Positive maxUnacked) (NonEmpty outboundTxs) delay =
    let mbDelayTime = getSmallDelay . getPositive <$> delay
        tr = (runSimTrace $ do
            controlMessageVar <- newTVarIO Continue
            _ <-
              async $ do
                threadDelay
                  (fromMaybe 1 mbDelayTime
                    * realToFrac (length outboundTxs `div` 4))
                atomically (writeTVar controlMessageVar Terminate)
            txSubmissionSimulation
              verboseTracer
              (NumTxIdsToAck maxUnacked) outboundTxs
              (readTVar controlMessageVar)
              mbDelayTime mbDelayTime
            ) in
    ioProperty $ do
        tr' <- evaluateTrace tr
        case tr' of
             SimException e trace -> do
                return $ counterexample (intercalate "\n" $ show e : trace) False
             SimDeadLock trace -> do
                 return $ counterexample (intercalate "\n" $ "Deadlock" : trace) False
             SimReturn (inmp, outmp) _trace -> do
                 -- printf "Log: %s\n" (intercalate "\n" _trace)
                 let outUniqueTxIds = nubBy (on (==) getTxId) outmp
                     outValidTxs    = filter getTxValid outmp
                 case (length outUniqueTxIds == length outmp, length outValidTxs == length outmp) of
                      (True, True) ->
                          -- If we are presented with a stream of unique txids for valid
                          -- transactions the inbound transactions should match the outbound
                          -- transactions exactly.
                          return $ inmp === take (length inmp) outValidTxs
                      (True, False) ->
                          -- If we are presented with a stream of unique txids then we should have
                          -- fetched all valid transactions.
                          return $ inmp === take (length inmp) outValidTxs
                      (False, True) ->
                          -- If we are presented with a stream of valid txids then we should have
                          -- fetched some version of those transactions.
                          return $ map getTxId inmp === take (length inmp) (map getTxId $
                              filter getTxValid outUniqueTxIds)
                      (False, False)
                           -- If we are presented with a stream of valid and invalid Txs with
                           -- duplicate txids we're content with completing the protocol
                           -- without error.
                           -> return $ property True

prop_x :: Property
prop_x = prop_txSubmission
  Positive {getPositive = 3}
  NonEmpty {getNonEmpty = [Tx {getTxId = -83, getTxSize = SizeInBytes 62352, getTxValid = True},Tx {getTxId = 66, getTxSize = SizeInBytes 37084, getTxValid = True},Tx {getTxId = 55, getTxSize = SizeInBytes 54825, getTxValid = False},Tx {getTxId = -94, getTxSize = SizeInBytes 54298, getTxValid = True},Tx {getTxId = -83, getTxSize = SizeInBytes 30932, getTxValid = True},Tx {getTxId = 33, getTxSize = SizeInBytes 40377, getTxValid = True},Tx {getTxId = 87, getTxSize = SizeInBytes 42883, getTxValid = False},Tx {getTxId = -87, getTxSize = SizeInBytes 21529, getTxValid = True},Tx {getTxId = 85, getTxSize = SizeInBytes 15222, getTxValid = True},Tx {getTxId = -13, getTxSize = SizeInBytes 529, getTxValid = True},Tx {getTxId = -21, getTxSize = SizeInBytes 14755, getTxValid = True},Tx {getTxId = 37, getTxSize = SizeInBytes 3921, getTxValid = True},Tx {getTxId = -44, getTxSize = SizeInBytes 42390, getTxValid = True},Tx {getTxId = 47, getTxSize = SizeInBytes 27061, getTxValid = False},Tx {getTxId = 64, getTxSize = SizeInBytes 8540, getTxValid = True},Tx {getTxId = -85, getTxSize = SizeInBytes 15138, getTxValid = False},Tx {getTxId = -23, getTxSize = SizeInBytes 16317, getTxValid = False},Tx {getTxId = -35, getTxSize = SizeInBytes 4372, getTxValid = True},Tx {getTxId = -11, getTxSize = SizeInBytes 13524, getTxValid = True},Tx {getTxId = 98, getTxSize = SizeInBytes 62024, getTxValid = True},Tx {getTxId = -42, getTxSize = SizeInBytes 63227, getTxValid = False},Tx {getTxId = 74, getTxSize = SizeInBytes 31476, getTxValid = True},Tx {getTxId = 72, getTxSize = SizeInBytes 42959, getTxValid = True},Tx {getTxId = 72, getTxSize = SizeInBytes 53084, getTxValid = True},Tx {getTxId = 6, getTxSize = SizeInBytes 5013, getTxValid = True},Tx {getTxId = -62, getTxSize = SizeInBytes 52590, getTxValid = True},Tx {getTxId = -18, getTxSize = SizeInBytes 59325, getTxValid = False},Tx {getTxId = 70, getTxSize = SizeInBytes 40956, getTxValid = True},Tx {getTxId = -82, getTxSize = SizeInBytes 33213, getTxValid = True},Tx {getTxId = -73, getTxSize = SizeInBytes 31026, getTxValid = True},Tx {getTxId = -4, getTxSize = SizeInBytes 19421, getTxValid = True},Tx {getTxId = 68, getTxSize = SizeInBytes 37501, getTxValid = False},Tx {getTxId = 47, getTxSize = SizeInBytes 25707, getTxValid = False},Tx {getTxId = -99, getTxSize = SizeInBytes 58538, getTxValid = False},Tx {getTxId = 86, getTxSize = SizeInBytes 63432, getTxValid = False},Tx {getTxId = -73, getTxSize = SizeInBytes 32185, getTxValid = True},Tx {getTxId = 52, getTxSize = SizeInBytes 55174, getTxValid = False},Tx {getTxId = 52, getTxSize = SizeInBytes 20715, getTxValid = False},Tx {getTxId = -21, getTxSize = SizeInBytes 37063, getTxValid = False},Tx {getTxId = 15, getTxSize = SizeInBytes 63172, getTxValid = True},Tx {getTxId = -26, getTxSize = SizeInBytes 51314, getTxValid = True},Tx {getTxId = 19, getTxSize = SizeInBytes 5042, getTxValid = True},Tx {getTxId = 36, getTxSize = SizeInBytes 40532, getTxValid = True},Tx {getTxId = -30, getTxSize = SizeInBytes 18812, getTxValid = True},Tx {getTxId = 22, getTxSize = SizeInBytes 61634, getTxValid = True},Tx {getTxId = 89, getTxSize = SizeInBytes 44309, getTxValid = True},Tx {getTxId = -98, getTxSize = SizeInBytes 61700, getTxValid = True},Tx {getTxId = -17, getTxSize = SizeInBytes 46606, getTxValid = True},Tx {getTxId = -37, getTxSize = SizeInBytes 25004, getTxValid = False},Tx {getTxId = -53, getTxSize = SizeInBytes 51991, getTxValid = False},Tx {getTxId = -88, getTxSize = SizeInBytes 17941, getTxValid = True},Tx {getTxId = 24, getTxSize = SizeInBytes 19866, getTxValid = True},Tx {getTxId = -99, getTxSize = SizeInBytes 52082, getTxValid = True},Tx {getTxId = 50, getTxSize = SizeInBytes 48715, getTxValid = True},Tx {getTxId = -8, getTxSize = SizeInBytes 24522, getTxValid = True},Tx {getTxId = 92, getTxSize = SizeInBytes 53516, getTxValid = True},Tx {getTxId = 59, getTxSize = SizeInBytes 16151, getTxValid = False},Tx {getTxId = -85, getTxSize = SizeInBytes 57386, getTxValid = True},Tx {getTxId = 23, getTxSize = SizeInBytes 36444, getTxValid = False},Tx {getTxId = -59, getTxSize = SizeInBytes 63727, getTxValid = False},Tx {getTxId = -59, getTxSize = SizeInBytes 12656, getTxValid = True},Tx {getTxId = 13, getTxSize = SizeInBytes 19160, getTxValid = False},Tx {getTxId = -35, getTxSize = SizeInBytes 1681, getTxValid = True},Tx {getTxId = -13, getTxSize = SizeInBytes 46705, getTxValid = False}]}
  (Just (Positive {getPositive = SmallDelay {getSmallDelay = 4.3}}))

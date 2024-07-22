{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Ouroboros.Network.TxSubmission.TxSubmissionV1 (tests) where

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

import Ouroboros.Network.NodeToNode (NodeToNodeVersion (..))

import Ouroboros.Network.Channel
import Ouroboros.Network.ControlMessage (ControlMessage (..), ControlMessageSTM)
import Ouroboros.Network.Driver
import Ouroboros.Network.Protocol.TxSubmission2.Client
import Ouroboros.Network.Protocol.TxSubmission2.Codec
import Ouroboros.Network.Protocol.TxSubmission2.Server
import Ouroboros.Network.Protocol.TxSubmission2.Type
import Ouroboros.Network.TxSubmission.Inbound
import Ouroboros.Network.TxSubmission.Outbound
import Ouroboros.Network.Util.ShowProxy


import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Test.Ouroboros.Network.TxSubmission.Common hiding (tests)
import Test.Ouroboros.Network.Utils


tests :: TestTree
tests = testGroup "Ouroboros.Network.TxSubmission.TxSubmissionV1"
  [ testProperty "txSubmission" prop_txSubmission
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
        (maxBound :: NodeToNodeVersion)
        controlMessageSTM

    inboundPeer :: Mempool m txid -> TxSubmissionServerPipelined txid (Tx txid) m ()
    inboundPeer inboundMempool =
      txSubmissionInbound
        nullTracer
        maxUnacked
        (getMempoolReader inboundMempool)
        (getMempoolWriter inboundMempool)
        (maxBound :: NodeToNodeVersion)

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

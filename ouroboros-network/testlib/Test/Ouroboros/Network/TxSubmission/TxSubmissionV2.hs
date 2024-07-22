{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Ouroboros.Network.TxSubmission.TxSubmissionV2 (tests) where

import Prelude hiding (seq)

import NoThunks.Class

import Control.Concurrent.Class.MonadMVar (MonadMVar)
import Control.Concurrent.Class.MonadMVar.Strict qualified as Strict
import Control.Concurrent.Class.MonadSTM
import Control.Concurrent.Class.MonadSTM.Strict (StrictTVar)
import Control.Concurrent.Class.MonadSTM.Strict qualified as Strict
import Control.Monad (forM)
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadSay
import Control.Monad.Class.MonadST
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import Control.Monad.IOSim hiding (SimResult)
import Control.Tracer (Tracer (..), contramap, nullTracer)


import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Function (on)
import Data.List (intercalate, nubBy)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Void (Void)
import Data.Word (Word16)

import Ouroboros.Network.Channel
import Ouroboros.Network.ControlMessage (ControlMessage (..), ControlMessageSTM)
import Ouroboros.Network.DeltaQ (PeerGSV)
import Ouroboros.Network.Driver
import Ouroboros.Network.NodeToNode (NodeToNodeVersion (..))
import Ouroboros.Network.Protocol.TxSubmission2.Client
import Ouroboros.Network.Protocol.TxSubmission2.Codec
import Ouroboros.Network.Protocol.TxSubmission2.Server
import Ouroboros.Network.Protocol.TxSubmission2.Type
import Ouroboros.Network.Snocket (TestAddress (..))
import Ouroboros.Network.TxSubmission.Inbound.Policy
import Ouroboros.Network.TxSubmission.Inbound.Registry
import Ouroboros.Network.TxSubmission.Inbound.Server (txSubmissionInboundV2)
import Ouroboros.Network.TxSubmission.Inbound.State
import Ouroboros.Network.TxSubmission.Outbound
import Ouroboros.Network.Util.ShowProxy

import Test.Ouroboros.Network.TxSubmission.Common hiding (tests)
import Test.Ouroboros.Network.Utils

import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)


tests :: TestTree
tests = testGroup "Ouroboros.Network.TxSubmission.TxSubmissionV2"
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
  -> Tracer m (DebugSharedTxState (TestAddress Int) txid (Tx txid))
  -> NumTxIdsToAck
  -> [Tx txid]
  -> ControlMessageSTM m
  -> Maybe DiffTime
  -> Maybe DiffTime
  -> m ([Tx txid], [Tx txid])
txSubmissionSimulation tracer tracerDST maxUnacked outboundTxs
                       controlMessageSTM
                       inboundDelay outboundDelay = do

    inboundMempool  <- emptyMempool
    outboundMempool <- newMempool outboundTxs
    (outboundChannel, inboundChannel) <- createConnectedChannels

    txChannelsMVar <- Strict.newMVar (TxChannels Map.empty)
    sharedTxStateVar <- newSharedTxStateVar
    gsvVar <- Strict.newTVarIO Map.empty

    asyncs <- runTxSubmission [(TestAddress 0, outboundChannel, inboundChannel)]
                              txChannelsMVar
                              sharedTxStateVar
                              (outboundMempool, inboundMempool)
                              gsvVar
                              undefined
                              (pure . snd)

    _ <- waitAnyCancel asyncs

    inmp <- readMempool inboundMempool
    outmp <- readMempool outboundMempool
    return (inmp, outmp)
  where

    runTxSubmission :: [(TestAddress Int, Channel m ByteString, Channel m ByteString)]
                    -> TxChannelsVar m (TestAddress Int) txid (Tx txid)
                    -> SharedTxStateVar m (TestAddress Int) txid (Tx txid)
                    -> (Mempool m txid, Mempool m txid)
                    -> StrictTVar m (Map (TestAddress Int) PeerGSV)
                    -> TxDecisionPolicy
                    -> ((Async m Void, [Async m ((), Maybe ByteString)]) -> m b)
                    -> m b
    runTxSubmission addrs txChannelsVar sharedTxStateVar
                    (outboundMempool, inboundMempool) gsvVar txDecisionPolicy k =
      withAsync (decisionLogicThread tracerDST txDecisionPolicy gsvVar txChannelsVar sharedTxStateVar) $ \a -> do
        -- Construct txSubmission outbound client
        let clients =
              (\(addr, outboundChannel, _) ->
                ( addr
                , outboundChannel
                , txSubmissionOutbound nullTracer
                                       maxUnacked
                                       (getMempoolReader outboundMempool)
                                       (maxBound :: NodeToNodeVersion)
                                       controlMessageSTM
                )) <$> addrs

        -- Construct txSubmission inbound server
        servers <- forM addrs
                       (\(addr, _, inboundChannel) ->
                         withPeer
                           tracerDST
                           txChannelsVar
                           sharedTxStateVar
                           (getMempoolReader inboundMempool)
                           addr $ \api ->
                             pure $
                               ( addr
                               , inboundChannel
                               , txSubmissionInboundV2 nullTracer
                                                       (getMempoolWriter inboundMempool)
                                                       api
                               )
                       )

            -- Construct txSubmission outbound client miniprotocol peer runner
        let runPeerClients =
             (\(_, channel, client) ->
               runPeerWithLimits
                 (("OUTBOUND",) `contramap` tracer)
                 txSubmissionCodec2
                 (byteLimitsTxSubmission2 (fromIntegral . BSL.length))
                 timeLimitsTxSubmission2
                 (maybe id delayChannel outboundDelay channel)
                 (txSubmissionClientPeer client))
                <$> clients
            -- Construct txSubmission inbound server miniprotocol peer runner
            runPeerServers =
              (\(addr, channel, server) ->
                runPipelinedPeerWithLimits
                  (("INBOUND " ++ show addr,) `contramap` verboseTracer)
                  txSubmissionCodec2
                  (byteLimitsTxSubmission2 (fromIntegral . BSL.length))
                  timeLimitsTxSubmission2
                  (maybe id delayChannel inboundDelay channel)
                  (txSubmissionServerPeerPipelined server))
                <$> servers

        -- Run clients and servers
        withAsyncAll (runPeerClients ++ runPeerServers) (\asyncs -> k (a, asyncs))

    withAsyncAll :: MonadAsync m => [m a] -> ([Async m a] -> m b) -> m b
    withAsyncAll xs0 action = go [] xs0
      where
        go as []     = action (reverse as)
        go as (x:xs) = withAsync x (\a -> go (a:as) xs)


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

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

module Test.Ouroboros.Network.TxSubmission.TxSubmissionV2 (tests) where

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


import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Function (on)
import Data.List (intercalate, nubBy)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
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
import Ouroboros.Network.TxSubmission.Inbound.Policy
import Ouroboros.Network.TxSubmission.Outbound
import Ouroboros.Network.Util.ShowProxy

import Ouroboros.Network.Testing.Utils

import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Control.Concurrent.Class.MonadMVar.Strict qualified as Strict
import Control.Concurrent.Class.MonadSTM.Strict (StrictTVar)
import Control.Concurrent.Class.MonadSTM.Strict qualified as Strict
import Control.Monad (forM)
import Data.Foldable (traverse_)
import Data.Void (Void)
import Ouroboros.Network.DeltaQ (PeerGSV)
import Ouroboros.Network.TxSubmission.Inbound.Registry
import Ouroboros.Network.TxSubmission.Inbound.Server (txSubmissionInboundV2)
import Ouroboros.Network.TxSubmission.Inbound.State
import Test.Ouroboros.Network.TxSubmission.Common hiding (tests)


tests :: TestTree
tests = testGroup "Ouroboros.Network.TxSubmission.TxSubmissionV2"
  [ testProperty "txSubmission" prop_txSubmission
  ]

txSubmissionSimulation
  :: forall m peeraddr txid.
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
     , Show peeraddr
     , Ord peeraddr

     , txid ~ Int
     )
  => Tracer m (String, TraceSendRecv (TxSubmission2 txid (Tx txid)))
  -> Tracer m (DebugSharedTxState peeraddr txid (Tx txid))
  -> Map peeraddr ( NumTxIdsToAck
                  , [Tx txid]
                  , ControlMessageSTM m
                  , Maybe DiffTime
                  , Maybe DiffTime
                  )
  -> TxDecisionPolicy
  -> m ([Tx txid], [[Tx txid]])
txSubmissionSimulation tracer tracerDST state txDecisionPolicy = do
    state' <- traverse (\(a, b, c, d, e) -> do
                                  mempool <- newMempool b
                                  (outChannel, inChannel) <- createConnectedChannels
                                  return (a, mempool, c, d, e, outChannel, inChannel)
                                ) state

    inboundMempool  <- emptyMempool

    txChannelsMVar <- Strict.newMVar (TxChannels Map.empty)
    sharedTxStateVar <- newSharedTxStateVar
    gsvVar <- Strict.newTVarIO Map.empty

    asyncs <- runTxSubmission state'
                              txChannelsMVar
                              sharedTxStateVar
                              inboundMempool
                              gsvVar
                              (pure . snd)

    _ <- waitAnyCancel asyncs

    inmp <- readMempool inboundMempool
    outmp <- forM (Map.elems state')
                 (\(_, outMempool, _, _, _, _, _) -> readMempool outMempool)
    return (inmp, outmp)
  where
    runTxSubmission :: Map peeraddr ( NumTxIdsToAck
                                    , Mempool m txid -- ^ Outbound mempool
                                    , ControlMessageSTM m
                                    , Maybe DiffTime -- ^ Outbound delay
                                    , Maybe DiffTime -- ^ Inbound delay
                                    , Channel m ByteString -- ^ Outbound channel
                                    , Channel m ByteString -- ^ Inbound channel
                                    )
                    -> TxChannelsVar m peeraddr txid (Tx txid)
                    -> SharedTxStateVar m peeraddr txid (Tx txid)
                    -> Mempool m txid -- ^ Inbound mempool
                    -> StrictTVar m (Map peeraddr PeerGSV)
                    -> ((Async m Void, [Async m ((), Maybe ByteString)]) -> m b)
                    -> m b
    runTxSubmission st txChannelsVar sharedTxStateVar
                    inboundMempool gsvVar k =
      withAsync (decisionLogicThread tracerDST txDecisionPolicy gsvVar txChannelsVar sharedTxStateVar) $ \a -> do
        -- Construct txSubmission outbound client
        let clients = (\(maxUnacked, mempool, controlMessageSTM, outDelay, _, outChannel, _) ->
                        ( outDelay
                        , outChannel
                        , txSubmissionOutbound nullTracer
                                               maxUnacked
                                               (getMempoolReader mempool)
                                               NodeToNodeV_7
                                               controlMessageSTM
                        )
                      )
                     <$> st

        -- Construct txSubmission inbound server
        servers <- Map.traverseWithKey
                       (\addr (_, _, _, _, inDelay, _, inboundChannel) ->
                         withPeer tracerDST
                                  txChannelsVar
                                  sharedTxStateVar
                                  (getMempoolReader inboundMempool)
                                  addr $ \api ->
                                    pure ( inDelay
                                         , inboundChannel
                                         , txSubmissionInboundV2 nullTracer
                                                                 (getMempoolWriter inboundMempool)
                                                                 api
                                         )
                       ) st

            -- Construct txSubmission outbound client miniprotocol peer runner
        let runPeerClients =
             (\(addr, (outboundDelay, channel, client)) ->
               runPeerWithLimits
                 (("OUTBOUND " ++ show addr,) `contramap` tracer)
                 txSubmissionCodec2
                 (byteLimitsTxSubmission2 (fromIntegral . BSL.length))
                 timeLimitsTxSubmission2
                 (maybe id delayChannel outboundDelay channel)
                 (txSubmissionClientPeer client))
                <$> Map.assocs clients
            -- Construct txSubmission inbound server miniprotocol peer runner
            runPeerServers =
              (\(addr, (inboundDelay, channel, server)) ->
                runPipelinedPeerWithLimits
                  (("INBOUND " ++ show addr,) `contramap` verboseTracer)
                  txSubmissionCodec2
                  (byteLimitsTxSubmission2 (fromIntegral . BSL.length))
                  timeLimitsTxSubmission2
                  (maybe id delayChannel inboundDelay channel)
                  (txSubmissionServerPeerPipelined server))
                <$> Map.assocs servers

        -- Run clients and servers
        withAsyncAll (runPeerClients ++ runPeerServers) (\asyncs -> k (a, asyncs))

    withAsyncAll :: MonadAsync m => [m a] -> ([Async m a] -> m b) -> m b
    withAsyncAll xs0 action = go [] xs0
      where
        go as []     = action (reverse as)
        go as (x:xs) = withAsync x (\a -> go (a:as) xs)

prop_txSubmission
  :: Map Int ( Positive Word16
             , NonEmptyList (Tx Int)
             , Maybe (Positive SmallDelay)
             , Maybe (Positive SmallDelay)
             -- ^ The delay must be smaller (<) than 5s, so that overall
             -- delay is less than 10s, otherwise 'smallDelay' in
             -- 'timeLimitsTxSubmission2' will kick in.
             )
  -> ArbTxDecisionPolicy
  -> Property
prop_txSubmission state (ArbTxDecisionPolicy txDecisionPolicy) =
  ioProperty $ do
    tr' <- evaluateTrace (runSimTrace sim)
    case tr' of
         SimException e trace -> do
            return $ counterexample (intercalate "\n" $ show e : trace) False
         SimDeadLock trace -> do
             return $ counterexample (intercalate "\n" $ "Deadlock" : trace) False
         SimReturn (inmp, outmp) _trace -> do
             -- printf "Log: %s\n" (intercalate "\n" _trace)
             let outUniqueTxIds = map (nubBy (on (==) getTxId)) outmp
                 outValidTxs    = map (filter getTxValid) outmp
             case ( length outUniqueTxIds == length outmp
                  -- , all (\(a, b) -> length a == length b) (zip outUniqueTxIds outmp) ?
                  , length outValidTxs == length outmp
                  -- , all (\(a, b) -> length a == length b) (zip outValidTxs outmp) ?
                  ) of
                  (True, True) ->
                    -- If we are presented with a stream of unique txids for valid
                    -- transactions the inbound transactions should match the outbound
                    -- transactions exactly.
                    return $ conjoin
                           $ (\(a, b) -> (a === take (length a) b))
                         <$> (zip (repeat inmp) (outValidTxs))

                  (True, False) ->
                    -- If we are presented with a stream of unique txids then we should have
                    -- fetched all valid transactions.
                    return $ conjoin
                           $ (\(a, b) -> a === take (length a) b)
                         <$> (zip (repeat inmp) (outValidTxs))

                  (False, True) ->
                      -- If we are presented with a stream of valid txids then we should have
                      -- fetched some version of those transactions.
                      return $ conjoin
                             $ (\(a, b) -> a === take (length a) b)
                           <$> (zip (repeat (map getTxId inmp)) ((map getTxId . filter getTxValid) <$> outValidTxs))

                  (False, False)
                       -- If we are presented with a stream of valid and invalid Txs with
                       -- duplicate txids we're content with completing the protocol
                       -- without error.
                       -> return $ property True
  where
    sim :: forall s . IOSim s ([Tx Int], [[Tx Int]])
    sim = do
      state' <- traverse (\(Positive maxUnacked, NonEmpty txs, mbOutDelay, mbInDelay) -> do
                          let mbOutDelayTime = getSmallDelay . getPositive <$> mbOutDelay
                              mbInDelayTime  = getSmallDelay . getPositive <$> mbInDelay
                          controlMessageVar <- newTVarIO Continue
                          return ( NumTxIdsToAck maxUnacked
                                 , txs
                                 , controlMessageVar
                                 , mbOutDelayTime
                                 , mbInDelayTime
                                 )
                        )
                        state

      state'' <- traverse (\(maxUnacked, txs, var, mbOutDelay, mbInDelay) -> do
                           return ( maxUnacked
                                  , txs
                                  , readTVar var
                                  , mbOutDelay
                                  , mbInDelay
                                  )
                        )
                        state'

      let simDelayTime = Map.foldl' (\m (_, txs, _, mbInDelay, mbOutDelay) ->
                                      max m ( fromMaybe 1 (max <$> mbInDelay <*> mbOutDelay)
                                            * realToFrac (length txs `div` 4)
                                            )
                                    )
                                    0
                       $ state'
          controlMessageVars = (\(_, _, x, _, _) -> x)
                            <$> Map.elems state'

      _ <- async $ do
            threadDelay simDelayTime
            atomically (traverse_ (`writeTVar` Terminate) controlMessageVars)

      txSubmissionSimulation verboseTracer verboseTracer state'' txDecisionPolicy

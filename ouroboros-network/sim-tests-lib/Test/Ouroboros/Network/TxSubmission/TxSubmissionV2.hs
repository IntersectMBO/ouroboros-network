{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
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
import Control.Tracer (Tracer (..), contramap)


import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Function (on)
import Data.Foldable (traverse_)
import Data.List (intercalate, nubBy)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Void (Void)

import Ouroboros.Network.Channel
import Ouroboros.Network.ControlMessage (ControlMessage (..), ControlMessageSTM)
import Ouroboros.Network.DeltaQ (PeerGSV)
import Ouroboros.Network.Driver
import Ouroboros.Network.NodeToNode (NodeToNodeVersion (..))
import Ouroboros.Network.Protocol.TxSubmission2.Client
import Ouroboros.Network.Protocol.TxSubmission2.Codec
import Ouroboros.Network.Protocol.TxSubmission2.Server
import Ouroboros.Network.Protocol.TxSubmission2.Type
import Ouroboros.Network.TxSubmission.Inbound.Policy
import Ouroboros.Network.TxSubmission.Inbound.Registry
import Ouroboros.Network.TxSubmission.Inbound.Server (txSubmissionInboundV2)
import Ouroboros.Network.TxSubmission.Inbound.State
import Ouroboros.Network.TxSubmission.Outbound
import Ouroboros.Network.Util.ShowProxy

import Ouroboros.Network.Testing.Utils hiding (debugTracer)
import Test.Ouroboros.Network.TxSubmission.Common hiding (tests)

import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)


tests :: TestTree
tests = testGroup "Ouroboros.Network.TxSubmission.TxSubmissionV2"
  [ testProperty "txSubmission" prop_txSubmission
  ]

data TxSubmissionV2State =
  TxSubmissionV2State {
      peerMap :: Map Int ( [Tx Int]
                         , Maybe (Positive SmallDelay)
                         , Maybe (Positive SmallDelay)
                         -- ^ The delay must be smaller (<) than 5s, so that overall
                         -- delay is less than 10s, otherwise 'smallDelay' in
                         -- 'timeLimitsTxSubmission2' will kick in.
                         )
    , decisionPolicy :: TxDecisionPolicy
  } deriving (Show)

instance Arbitrary TxSubmissionV2State where
  arbitrary = do
    ArbTxDecisionPolicy decisionPolicy <- arbitrary
    peersN <- choose (1, 10)
    txsN <- choose (1, 10)
    txs <- divvy txsN . nubBy (on (==) getTxId) <$> vectorOf (peersN * txsN) arbitrary
    peers <- vectorOf peersN arbitrary
    peersState <- map (\(a, (b, c)) -> (a, b, c))
                . zip txs
              <$> vectorOf peersN arbitrary
    return (TxSubmissionV2State (Map.fromList (zip peers peersState)) decisionPolicy)
  shrink (TxSubmissionV2State peerMap decisionPolicy) =
    TxSubmissionV2State <$> shrinkMap1 peerMap
                        <*> [ policy
                          | ArbTxDecisionPolicy policy <- shrink (ArbTxDecisionPolicy decisionPolicy)
                            ]
    where
      shrinkMap1 :: (Ord k, Arbitrary k, Arbitrary v) => Map k v -> [Map k v]
      shrinkMap1 m
        | Map.size m <= 1 = [m]
        | otherwise       = [Map.delete k m | k <- Map.keys m] ++ singletonMaps
        where
          singletonMaps = [Map.singleton k v | (k, v) <- Map.toList m]

runTxSubmissionV2
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
  -> Map peeraddr ( [Tx txid]
                  , ControlMessageSTM m
                  , Maybe DiffTime
                  , Maybe DiffTime
                  )
  -> TxDecisionPolicy
  -> m ([Tx txid], [[Tx txid]])
runTxSubmissionV2 tracer tracerDST state txDecisionPolicy = do

    state' <- traverse (\(b, c, d, e) -> do
        mempool <- newMempool b
        (outChannel, inChannel) <- createConnectedChannels
        return (mempool, c, d, e, outChannel, inChannel)
        ) state

    inboundMempool <- emptyMempool

    txChannelsMVar <- Strict.newMVar (TxChannels Map.empty)
    sharedTxStateVar <- newSharedTxStateVar
    gsvVar <- Strict.newTVarIO Map.empty

    runTxSubmission state'
                    txChannelsMVar
                    sharedTxStateVar
                    inboundMempool
                    gsvVar
                    (\(a, as) -> do
                      _ <- waitAnyCancel as
                      cancel a

                      inmp <- readMempool inboundMempool
                      outmp <- forM (Map.elems state')
                                   (\(outMempool, _, _, _, _, _) -> readMempool outMempool)
                      return (inmp, outmp)
                    )

  where
    runTxSubmission :: Map peeraddr ( Mempool m txid -- ^ Outbound mempool
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
      withAsync (decisionLogicThread tracerDST txDecisionPolicy (Strict.readTVar gsvVar) txChannelsVar sharedTxStateVar) $ \a -> do
            -- Construct txSubmission outbound client
        let clients = (\(addr, (mempool, ctrlMsgSTM, outDelay, _, outChannel, _)) -> do
                        let client = txSubmissionOutbound verboseTracer
                                               (NumTxIdsToAck $ getNumTxIdsToReq
                                                              $ maxUnacknowledgedTxIds
                                                              $ txDecisionPolicy)
                                               (getMempoolReader mempool)
                                               NodeToNodeV_7
                                               ctrlMsgSTM
                        runPeerWithLimits (("OUTBOUND " ++ show addr,) `contramap` tracer)
                                          txSubmissionCodec2
                                          (byteLimitsTxSubmission2 (fromIntegral . BSL.length))
                                          timeLimitsTxSubmission2
                                          (maybe id delayChannel outDelay outChannel)
                                          (txSubmissionClientPeer client)
                      )
                     <$> Map.assocs st

            -- Construct txSubmission inbound server
            servers = (\(addr, (_, _, _, inDelay, _, inChannel)) ->
                         withPeer tracerDST
                                  txChannelsVar
                                  sharedTxStateVar
                                  (getMempoolReader inboundMempool)
                                  addr $ \api -> do
                                    let server = txSubmissionInboundV2 verboseTracer
                                                                       (getMempoolWriter inboundMempool)
                                                                       api
                                    runPipelinedPeerWithLimits
                                      (("INBOUND " ++ show addr,) `contramap` verboseTracer)
                                      txSubmissionCodec2
                                      (byteLimitsTxSubmission2 (fromIntegral . BSL.length))
                                      timeLimitsTxSubmission2
                                      (maybe id delayChannel inDelay inChannel)
                                      (txSubmissionServerPeerPipelined server)
                      ) <$> Map.assocs st

        -- Run clients and servers
        withAsyncAll (clients ++ servers) (\asyncs -> k (a, asyncs))

    withAsyncAll :: MonadAsync m => [m a] -> ([Async m a] -> m b) -> m b
    withAsyncAll xs0 action = go [] xs0
      where
        go as []     = action (reverse as)
        go as (x:xs) = withAsync x (\a -> go (a:as) xs)

txSubmissionV2Simulation :: forall s . TxSubmissionV2State -> IOSim s ([Tx Int], [[Tx Int]])
txSubmissionV2Simulation (TxSubmissionV2State state txDecisionPolicy) = do
  state' <- traverse (\(txs, mbOutDelay, mbInDelay) -> do
                      let mbOutDelayTime = getSmallDelay . getPositive <$> mbOutDelay
                          mbInDelayTime  = getSmallDelay . getPositive <$> mbInDelay
                      controlMessageVar <- newTVarIO Continue
                      return ( txs
                             , controlMessageVar
                             , mbOutDelayTime
                             , mbInDelayTime
                             )
                    )
                    state

  state'' <- traverse (\(txs, var, mbOutDelay, mbInDelay) -> do
                       return ( txs
                              , readTVar var
                              , mbOutDelay
                              , mbInDelay
                              )
                    )
                    state'

  let simDelayTime = Map.foldl' (\m (txs, _, mbInDelay, mbOutDelay) ->
                                  max m ( fromMaybe 1 (max <$> mbInDelay <*> mbOutDelay)
                                        * realToFrac (length txs `div` 4)
                                        )
                                )
                                0
                   $ state''
      controlMessageVars = (\(_, x, _, _) -> x)
                        <$> Map.elems state'

  _ <- async do
        threadDelay (simDelayTime + 1000)
        atomically (traverse_ (`writeTVar` Terminate) controlMessageVars)

  let tracer = verboseTracer <> debugTracer
      tracer' = verboseTracer <> debugTracer
  runTxSubmissionV2 tracer tracer' state'' txDecisionPolicy

-- | Tests overall tx submission semantics. The properties checked in this
-- property test are the same as for tx submission v1. We need this to know we
-- didn't regress.
--
prop_txSubmission :: TxSubmissionV2State -> Property
prop_txSubmission st =
  ioProperty $ do
    tr' <- evaluateTrace (runSimTrace (txSubmissionV2Simulation st))
    case tr' of
         SimException e trace -> do
            return $ counterexample (intercalate "\n" $ show e : trace) False
         SimDeadLock trace -> do
             return $ counterexample (intercalate "\n" $ "Deadlock" : trace) False
         SimReturn (inmp, outmps) _trace -> do
             r <- mapM (\outmp -> do
               let outUniqueTxIds = nubBy (on (==) getTxId) outmp
                   outValidTxs    = filter getTxValid outmp
               case ( length outUniqueTxIds == length outmp
                    , length outValidTxs == length outmp
                    ) of
                 (True, True) ->
                   -- If we are presented with a stream of unique txids for valid
                   -- transactions the inbound transactions should match the outbound
                   -- transactions exactly.
                   return $ counterexample ("(True, True) " ++ show outmp)
                          $ checkMempools inmp (take (length inmp) outValidTxs)

                 (True, False) ->
                   -- If we are presented with a stream of unique txids then we should have
                   -- fetched all valid transactions.
                   return $ counterexample ("(True, False) " ++ show outmp)
                          $ checkMempools inmp (take (length inmp) outValidTxs)

                 (False, True) ->
                   -- If we are presented with a stream of valid txids then we should have
                   -- fetched some version of those transactions.
                   return $ counterexample ("(False, True) " ++ show outmp)
                          $ checkMempools (map getTxId inmp)
                                          (take (length inmp)
                                                (map getTxId $ filter getTxValid outUniqueTxIds))

                 (False, False) ->
                   -- If we are presented with a stream of valid and invalid Txs with
                   -- duplicate txids we're content with completing the protocol
                   -- without error.
                   return $ property True)
                     outmps
             return $ counterexample (intercalate "\n" _trace)
                    $ conjoin r

checkMempools :: (Eq a, Show a) => [a] -> [a] -> Property
checkMempools [] [] = property True
checkMempools _ [] = property True
checkMempools [] _ = property False
checkMempools inp@(i : is) outp@(o : os) =
  if o == i then counterexample (show inp ++ " " ++ show outp)
               $ checkMempools is os
           else counterexample (show inp ++ " " ++ show outp)
              $ checkMempools is outp

-- | Split a list into sub list of at most `n` elements.
--
divvy :: Int -> [a] -> [[a]]
divvy _ [] = []
divvy n as = take n as : divvy n (drop n as)

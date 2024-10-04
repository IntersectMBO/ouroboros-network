{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Ouroboros.Network.TxSubmission.AppV2 (tests) where

import Prelude hiding (seq)

import NoThunks.Class

import Control.Concurrent.Class.MonadMVar.Strict
import Control.Concurrent.Class.MonadSTM.Strict
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
import Data.Foldable (traverse_)
import Data.Function (on)
import Data.Hashable
import Data.List (nubBy)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Void (Void)
import System.Random (mkStdGen)

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
import Ouroboros.Network.TxSubmission.Inbound.Types (TraceTxLogic)
import Ouroboros.Network.TxSubmission.Outbound
import Ouroboros.Network.Util.ShowProxy

import Test.Ouroboros.Network.TxSubmission.TxLogic hiding (tests)
import Test.Ouroboros.Network.TxSubmission.Types
import Test.Ouroboros.Network.Utils hiding (debugTracer)

import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)


tests :: TestTree
tests = testGroup "AppV2"
  [ testProperty "txSubmission" prop_txSubmission
  , testProperty "txSubmission inflight" prop_txSubmission_inflight
  ]

data TxSubmissionState =
  TxSubmissionState {
      peerMap :: Map Int ( [Tx Int]
                         , Maybe (Positive SmallDelay)
                         , Maybe (Positive SmallDelay)
                         -- ^ The delay must be smaller (<) than 5s, so that overall
                         -- delay is less than 10s, otherwise 'smallDelay' in
                         -- 'timeLimitsTxSubmission2' will kick in.
                         )
    , decisionPolicy :: TxDecisionPolicy
  } deriving (Show)

instance Arbitrary TxSubmissionState where
  arbitrary = do
    ArbTxDecisionPolicy decisionPolicy <- arbitrary
    peersN <- choose (1, 10)
    txsN <- choose (1, 10)
    txs <- divvy txsN . nubBy (on (==) getTxId) <$> vectorOf (peersN * txsN) arbitrary
    peers <- vectorOf peersN arbitrary
    peersState <- map (\(a, (b, c)) -> (a, b, c))
                . zip txs
              <$> vectorOf peersN arbitrary
    return TxSubmissionState  { peerMap = Map.fromList (zip peers peersState),
                                decisionPolicy
                              }
  shrink TxSubmissionState { peerMap, decisionPolicy } =
    TxSubmissionState <$> shrinkMap1 peerMap
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

runTxSubmission
  :: forall m peeraddr txid.
     ( MonadAsync m
     , MonadDelay m
     , MonadFork  m
     , MonadMask  m
     , MonadMVar  m
     , MonadSay   m
     , MonadST    m
     , MonadLabelledSTM m
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
     , Hashable peeraddr

     , txid ~ Int
     )
  => Tracer m (String, TraceSendRecv (TxSubmission2 txid (Tx txid)))
  -> Tracer m (TraceTxLogic peeraddr txid (Tx txid))
  -> Map peeraddr ( [Tx txid]
                  , ControlMessageSTM m
                  , Maybe DiffTime
                  , Maybe DiffTime
                  )
  -> TxDecisionPolicy
  -> m ([Tx txid], [[Tx txid]])
runTxSubmission tracer tracerTxLogic state txDecisionPolicy = do

    state' <- traverse (\(b, c, d, e) -> do
        mempool <- newMempool b
        (outChannel, inChannel) <- createConnectedChannels
        return (mempool, c, d, e, outChannel, inChannel)
        ) state

    inboundMempool <- emptyMempool
    let txRng = mkStdGen 42 -- TODO

    txChannelsMVar <- newMVar (TxChannels Map.empty)
    txMempoolSem <- newTxMempoolSem
    sharedTxStateVar <- newSharedTxStateVar txRng
    labelTVarIO sharedTxStateVar "shared-tx-state"
    gsvVar <- newTVarIO Map.empty
    labelTVarIO gsvVar "gsv"

    run state'
        txChannelsMVar
        txMempoolSem
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
    run :: Map peeraddr ( Mempool m txid -- ^ Outbound mempool
                        , ControlMessageSTM m
                        , Maybe DiffTime -- ^ Outbound delay
                        , Maybe DiffTime -- ^ Inbound delay
                        , Channel m ByteString -- ^ Outbound channel
                        , Channel m ByteString -- ^ Inbound channel
                        )
        -> TxChannelsVar m peeraddr txid (Tx txid)
        -> TxMempoolSem m
        -> SharedTxStateVar m peeraddr txid (Tx txid)
        -> Mempool m txid -- ^ Inbound mempool
        -> StrictTVar m (Map peeraddr PeerGSV)
        -> ((Async m Void, [Async m ((), Maybe ByteString)]) -> m b)
        -> m b
    run st txChannelsVar txMempoolSem sharedTxStateVar
        inboundMempool gsvVar k =
      withAsync (decisionLogicThread tracerTxLogic txDecisionPolicy (readTVar gsvVar) txChannelsVar sharedTxStateVar) $ \a -> do
            -- Construct txSubmission outbound client
        let clients = (\(addr, (mempool, ctrlMsgSTM, outDelay, _, outChannel, _)) -> do
                        let client = txSubmissionOutbound (Tracer $ say . show)
                                               (NumTxIdsToAck $ getNumTxIdsToReq
                                                              $ maxUnacknowledgedTxIds
                                                              $ txDecisionPolicy)
                                               (getMempoolReader mempool)
                                               (maxBound :: NodeToNodeVersion)
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
                         withPeer tracerTxLogic
                                  txChannelsVar
                                  txMempoolSem
                                  txDecisionPolicy
                                  sharedTxStateVar
                                  (getMempoolReader inboundMempool)
                                  getTxSize
                                  addr $ \api -> do
                                    let server = txSubmissionInboundV2 verboseTracer
                                                                       (getMempoolReader inboundMempool)
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

txSubmissionSimulation :: forall s . TxSubmissionState -> IOSim s ([Tx Int], [[Tx Int]])
txSubmissionSimulation (TxSubmissionState state txDecisionPolicy) = do
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

  let tracer :: forall a. Show a => Tracer (IOSim s) a
      tracer = verboseTracer <> debugTracer
  runTxSubmission tracer tracer state'' txDecisionPolicy

-- | Tests overall tx submission semantics. The properties checked in this
-- property test are the same as for tx submission v1. We need this to know we
-- didn't regress.
--
prop_txSubmission :: TxSubmissionState -> Property
prop_txSubmission st =
    let tr = runSimTrace (txSubmissionSimulation st) in
    case traceResult True tr of
         Left e ->
             counterexample (show e)
           . counterexample (ppTrace tr)
           $ False
         Right (inmp, outmps) ->
             counterexample (ppTrace tr)
           $ conjoin (validate inmp `map` outmps)
  where
    validate :: [Tx Int] -- the inbound mempool
             -> [Tx Int] -- one of the outbound mempools
             -> Property
    validate inmp outmp =
       let outUniqueTxIds = nubBy (on (==) getTxId) outmp
           outValidTxs    = filter getTxValid outmp
       in
       case ( length outUniqueTxIds == length outmp
            , length outValidTxs == length outmp
            ) of
         x@(True, True) ->
           -- If we are presented with a stream of unique txids for valid
           -- transactions the inbound transactions should match the outbound
           -- transactions exactly.
             counterexample (show x)
           . counterexample (show inmp)
           . counterexample (show outmp)
           $ checkMempools inmp (take (length inmp) outValidTxs)

         x@(True, False) ->
           -- If we are presented with a stream of unique txids then we should have
           -- fetched all valid transactions.
             counterexample (show x)
           . counterexample (show inmp)
           . counterexample (show outmp)
           $ checkMempools inmp (take (length inmp) outValidTxs)

         x@(False, True) ->
           -- If we are presented with a stream of valid txids then we should have
           -- fetched some version of those transactions.
             counterexample (show x)
           . counterexample (show inmp)
           . counterexample (show outmp)
           $ checkMempools (map getTxId inmp)
                           (take (length inmp)
                                 (map getTxId $ filter getTxValid outUniqueTxIds))

         (False, False) ->
           -- If we are presented with a stream of valid and invalid Txs with
           -- duplicate txids we're content with completing the protocol
           -- without error.
           property True

-- | This test checks that all txs are downloaded from all available peers if
-- available.
--
-- This test takes advantage of the fact that the mempool implementation
-- allows duplicates.
--
prop_txSubmission_inflight :: TxSubmissionState -> Property
prop_txSubmission_inflight st@(TxSubmissionState state _) =
  let trace = runSimTrace (txSubmissionSimulation st)
      maxRepeatedValidTxs = Map.foldr (\(txs, _, _) r ->
                                        foldr (\tx rr ->
                                                if Map.member tx rr && getTxValid tx
                                                   then Map.update (Just . succ @Int) tx rr
                                                   else if getTxValid tx
                                                           then Map.insert tx 1 rr
                                                           else rr
                                              )
                                              r
                                              txs
                                      )
                                      Map.empty
                                      state

   in case traceResult True trace of
        Left err -> counterexample (ppTrace trace)
                 $ counterexample (show err)
                 $ property False
        Right (inmp, _) ->
          let resultRepeatedValidTxs =
                foldr (\tx rr ->
                        if Map.member tx rr && getTxValid tx
                           then Map.update (Just . succ @Int) tx rr
                           else if getTxValid tx
                                   then Map.insert tx 1 rr
                                   else rr
                      )
                      Map.empty
                      inmp
           in resultRepeatedValidTxs === maxRepeatedValidTxs


-- | Check that the inbound mempool contains all outbound `tx`s as a proper
-- subsequence.  It might contain more `tx`s from other peers.
--
checkMempools :: Eq tx
              => [tx] -- inbound mempool
              -> [tx] -- outbound mempool
              -> Bool
checkMempools _  []    = True  -- all outbound `tx` were found in the inbound
                               -- mempool
checkMempools [] (_:_) = False -- outbound mempool contains `tx`s which were
                               -- not transferred to the inbound mempool
checkMempools (i : is') os@(o : os')
  | i == o
  = checkMempools is' os'

  | otherwise
  -- `_i` is not present in the outbound mempool, we can skip it.
  = checkMempools is' os


-- | Split a list into sub list of at most `n` elements.
--
divvy :: Int -> [a] -> [[a]]
divvy _ [] = []
divvy n as = take n as : divvy n (drop n as)

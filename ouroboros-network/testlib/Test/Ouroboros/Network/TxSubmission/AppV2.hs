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
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, fromMaybe)
import Data.Void (Void)
import System.Random (mkStdGen)

import Cardano.Network.NodeToNode (NodeToNodeVersion (..))

import Ouroboros.Network.Channel
import Ouroboros.Network.ControlMessage (ControlMessage (..), ControlMessageSTM)
import Ouroboros.Network.Driver
import Ouroboros.Network.Protocol.TxSubmission2.Client
import Ouroboros.Network.Protocol.TxSubmission2.Codec
import Ouroboros.Network.Protocol.TxSubmission2.Server
import Ouroboros.Network.Protocol.TxSubmission2.Type
import Ouroboros.Network.TxSubmission.Inbound.V2 (TxSubmissionInitDelay (..),
           txSubmissionInboundV2)
import Ouroboros.Network.TxSubmission.Inbound.V2.Policy
import Ouroboros.Network.TxSubmission.Inbound.V2.Registry
import Ouroboros.Network.TxSubmission.Inbound.V2.Types (TraceTxLogic)
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
  , testProperty "inflight" prop_txSubmission_inflight
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
    -- NOTE: using sortOn would forces tx-decision logic to download txs in the
    -- order of unacknowledgedTxIds.  This could be useful to get better
    -- properties when wrongly sized txs are present.
    txs <- divvy txsN . nubBy (on (==) getTxId) {- . List.sortOn getTxId -} <$> vectorOf (peersN * txsN) arbitrary
    peers <- vectorOf peersN arbitrary
    peersState <- zipWith (curry (\(a, (b, c)) -> (a, b, c))) txs
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
  -- ^ inbound and outbound mempools
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

    run state'
        txChannelsMVar
        txMempoolSem
        sharedTxStateVar
        inboundMempool
        (\(a, as) -> do
          _ <- waitAllServers as
          -- cancel decision logic thread
          cancel a

          inmp <- readMempool inboundMempool
          let outmp = map (\(txs, _, _, _) -> txs)
                    $ Map.elems state
                       
          return (inmp, outmp)
        )

  where
    waitAllServers :: [(Async m x, Async m x)] -> m [Either SomeException x]
    waitAllServers [] = return []
    waitAllServers ((client, server):as) = do
      r <- waitCatch server
      -- cancel client as soon as the server exits
      cancel client
      rs <- waitAllServers as
      return (r : rs)

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
        -> ((Async m Void, [(Async m ((), Maybe ByteString), Async m ((), Maybe ByteString))]) -> m b)
        -> m b
    run st txChannelsVar txMempoolSem sharedTxStateVar
        inboundMempool k =
      withAsync (decisionLogicThread tracerTxLogic sayTracer
                                     txDecisionPolicy txChannelsVar sharedTxStateVar) $ \a -> do
            -- Construct txSubmission outbound client
        let clients = (\(addr, (mempool {- txs -}, ctrlMsgSTM, outDelay, _, outChannel, _)) -> do
                        let client = txSubmissionOutbound
                                       (Tracer $ say . show)
                                       (NumTxIdsToAck $ getNumTxIdsToReq
                                         $ maxUnacknowledgedTxIds txDecisionPolicy)
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
                                  (getMempoolWriter inboundMempool)
                                  getTxSize
                                  addr $ \api -> do
                                    let server = txSubmissionInboundV2 verboseTracer
                                                                       NoTxSubmissionInitDelay
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
        withAsyncAll (zip clients servers) (\asyncs -> k (a, asyncs))

    withAsyncAll :: MonadAsync m
                 => [(m a, m a)]
                 -> ([(Async m a, Async m a)] -> m b)
                 -> m b
    withAsyncAll xs0 action = go [] xs0
      where
        go as []         = action (reverse as)
        go as ((x,y):xs) = withAsync x (\a -> withAsync y (\b -> go ((a, b):as) xs))


txSubmissionSimulation :: forall s . TxSubmissionState
                       -> IOSim s ([Tx Int], [[Tx Int]])
                       -- ^ inbound & outbound mempools
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
                                state''
      controlMessageVars = (\(_, x, _, _) -> x)
                        <$> Map.elems state'

  withAsync
    (do threadDelay (simDelayTime + 1000)
        atomically (traverse_ (`writeTVar` Terminate) controlMessageVars)
    ) \_ -> do
      let tracer :: forall a. Show a => Tracer (IOSim s) a
          tracer = verboseTracer <> debugTracer
      runTxSubmission tracer tracer state'' txDecisionPolicy

filterValidTxs :: [Tx txid] -> [Tx txid]
filterValidTxs
  = filter getTxValid
  . takeWhile (\Tx{getTxSize, getTxAdvSize} -> getTxSize == getTxAdvSize)

-- | Tests overall tx submission semantics. The properties checked in this
-- property test are the same as for tx submission v1. We need this to know we
-- didn't regress.
--
prop_txSubmission :: TxSubmissionState -> Property
prop_txSubmission st@(TxSubmissionState peers _) =
    let tr = runSimTrace (txSubmissionSimulation st)
        numPeersWithWronglySizedTx :: Int
        numPeersWithWronglySizedTx =
          foldr
            (\(txs, _, _) r ->
              case List.find (\tx -> getTxSize tx /= getTxAdvSize tx) txs of
                Just {} -> r + 1
                Nothing -> r
            ) 0 peers
    in
        label ("number of peers: " ++ renderRanges 3 (Map.size peers))
      . label ("number of peers with wrongly sized tx: "
             ++ show numPeersWithWronglySizedTx)
      $ case traceResult True tr of
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
           outValidTxs    = filterValidTxs outmp
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

         x@(True, False) | Nothing <- List.find (\tx -> getTxAdvSize tx /= getTxSize tx) outmp  ->
           -- If we are presented with a stream of unique txids then we should have
           -- fetched all valid transactions if all txs have valid sizes.
             counterexample (show x)
           . counterexample (show inmp)
           . counterexample (show outValidTxs)

           $ checkMempools inmp (take (length inmp) outValidTxs)
                         | otherwise ->
             -- If there's one tx with an invalid size, we will download only
             -- some of them, but we don't guarantee how many we will download.
             --
             -- This is ok, the peer is cheating.
             property True
         

         x@(False, True) ->
           -- If we are presented with a stream of valid txids then we should have
           -- fetched some version of those transactions.
             counterexample (show x)
           . counterexample (show inmp)
           . counterexample (show outmp)
           $ checkMempools (map getTxId inmp)
                           (take (length inmp)
                                 (getTxId <$> filterValidTxs outUniqueTxIds))

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
-- TODO: do we generated enough outbound mempools which intersect in interesting
-- ways?
prop_txSubmission_inflight :: TxSubmissionState -> Property
prop_txSubmission_inflight st@(TxSubmissionState state _) =
  let maxRepeatedValidTxs = Map.foldr (\(txs, _, _) r -> foldr (fn r) r txs)
                                      Map.empty
                                      state
      hasInvalidSize =
          isJust
        $ List.find (\(txs, _, _) ->
                    isJust $ List.find (\tx -> getTxAdvSize tx /= getTxSize tx) txs
                  )
                  state
      trace = runSimTrace (txSubmissionSimulation st)
  in case traceResult True trace of
       Left err -> counterexample (ppTrace trace)
                 $ counterexample (show err)
                 $ property False
       Right (inmp, _) ->
         let resultRepeatedValidTxs =
               foldr (fn Map.empty) Map.empty inmp
         in label (if hasInvalidSize then "has wrongly sized tx" else "has no wrongly sized tx")
          . counterexample (ppTrace trace)
          . counterexample (show resultRepeatedValidTxs)
          . counterexample (show maxRepeatedValidTxs)
          $ if hasInvalidSize
              then resultRepeatedValidTxs `Map.isSubmapOf` maxRepeatedValidTxs
              else resultRepeatedValidTxs == maxRepeatedValidTxs
  where
    fn empty tx rr  | getTxAdvSize tx /= getTxSize tx
                    = empty
                    | Map.member tx rr
                    , getTxValid tx
                    = Map.update (Just . succ @Int) tx rr
                    | getTxValid tx
                    = Map.insert tx 1 rr
                    | otherwise
                    = rr

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

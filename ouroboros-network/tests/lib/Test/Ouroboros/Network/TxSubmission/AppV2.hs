{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Ouroboros.Network.TxSubmission.AppV2 (tests) where

import Prelude hiding (seq)

import NoThunks.Class

import Control.Concurrent.Class.MonadSTM qualified as Lazy
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadSay
import Control.Monad.Class.MonadST
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import Control.Monad.IOSim
import Control.Tracer (Tracer (..), contramap)

import Data.ByteString.Lazy qualified as BSL
import Data.Foldable (traverse_)
import Data.Function (on)
import Data.List (nubBy)
import Data.List qualified as List
import Data.List.Trace qualified as Trace
import Data.Map.Merge.Strict
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Monoid (Sum (..))
import Data.Set qualified as Set
import Data.Typeable (Typeable)
import Data.Word (Word64)

import Ouroboros.Network.Channel
import Ouroboros.Network.ControlMessage (ControlMessage (..), ControlMessageSTM)
import Ouroboros.Network.Driver
import Ouroboros.Network.Protocol.TxSubmission2.Client
import Ouroboros.Network.Protocol.TxSubmission2.Codec
import Ouroboros.Network.Protocol.TxSubmission2.Server
import Ouroboros.Network.Protocol.TxSubmission2.Type
import Ouroboros.Network.TxSubmission.Inbound.V2 (txSubmissionInboundV2)
import Ouroboros.Network.TxSubmission.Inbound.V2.Policy
import Ouroboros.Network.TxSubmission.Inbound.V2.Registry
import Ouroboros.Network.TxSubmission.Inbound.V2.Types
import Ouroboros.Network.TxSubmission.Outbound
import Ouroboros.Network.Util.ShowProxy

import Test.Ouroboros.Network.TxSubmission.Impaired (Impairment (..),
           applyImpairment, noImpairment)
import Test.Ouroboros.Network.TxSubmission.TxLogic hiding (tests)
import Test.Ouroboros.Network.TxSubmission.Types
import Test.Ouroboros.Network.Utils hiding (debugTracer)

import Test.QuickCheck
#if !MIN_VERSION_QuickCheck(2,16,0)
import "quickcheck-monoids" Test.QuickCheck.Monoids
#endif
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)


tests :: TestTree
tests = testGroup "AppV2"
  [ testGroup "Generators"
      [ testProperty "TxSubmissionState/validGen"      prop_TxSubmissionState_validGen
      , testProperty "TxSubmissionState/shrinkValid"
          $  prop_TxSubmissionState_shrinkValid
      , testProperty "TxSubmissionState/shrinkSmaller"
          $  prop_TxSubmissionState_shrinkSmaller
      , testProperty "TxSubmissionState/shrinkNoDups"
          $  prop_TxSubmissionState_shrinkNoDups
      ]
  , testProperty "txSubmission"          prop_txSubmission
  , testProperty "inflight"              prop_txSubmission_inflight
  , testProperty "resilientToImpairment" prop_txSubmission_resilientToImpairment
  , testProperty "SharedTxState" $ withMaxSize 25
                                 $ withMaxSuccess 25
                                 prop_sharedTxStateInvariant
  ]

data TestVersion = TestVersion
  deriving (Eq, Ord, Bounded, Enum, Show)

data TxSubmissionState =
  TxSubmissionState {
      peerMap :: Map Int ( [Tx Int]
                         , Maybe (Positive SmallDelay)
                         , Maybe (Positive SmallDelay)
                         -- ^ The delay must be smaller (<) than 5s, so that overall
                         -- delay is less than 10s, otherwise 'smallDelay' in
                         -- 'timeLimitsTxSubmission2' will kick in.
                         )
    , peerImpairment :: Map Int Impairment
    , decisionPolicy :: TxDecisionPolicy
  } deriving (Eq, Show)

instance Arbitrary TxSubmissionState where
  arbitrary = do
    ArbTxDecisionPolicy decisionPolicy <- arbitrary
    peersN <- choose (1, 10)
    txsN <- choose (1, 10)
    -- NOTE: using sortOn would forces tx-decision logic to download txs in the
    -- order of unacknowledgedTxIds.  This could be useful to get better
    -- properties when wrongly sized txs are present.
    txs <- fmap (nubBy (on (==) getTxId)) . divvy txsN {- . List.sortOn getTxId -} <$> vectorOf (peersN * txsN) arbitrary
    peers <- vectorOf peersN arbitrary
    peersState <- zipWith (curry (\(a, (b, c)) -> (a, b, c))) txs
              <$> vectorOf peersN arbitrary
    return TxSubmissionState  { peerMap = Map.fromList (zip peers peersState),
                                peerImpairment = Map.empty,
                                decisionPolicy
                              }
  shrink TxSubmissionState { peerMap, peerImpairment, decisionPolicy } =
       [ TxSubmissionState peerMap' peerImpairment decisionPolicy
       | peerMap' <- shrinkMap1 peerMap
       ]
    ++ [ TxSubmissionState peerMap peerImpairment policy
       | ArbTxDecisionPolicy policy <- shrink (ArbTxDecisionPolicy decisionPolicy)
       ]
    where
      shrinkMap1 :: (Eq v, Ord k, Arbitrary k, Arbitrary v) => Map k v -> [Map k v]
      shrinkMap1 m
        | Map.size m <= 1 = []
        | otherwise       =
            List.nub $ [Map.delete k m | k <- Map.keys m] ++ singletonMaps
        where
          singletonMaps = [Map.singleton k v | (k, v) <- Map.toList m]


newtype TxStateTrace peeraddr txid =
    TxStateTrace (SharedTxState peeraddr txid)
type TxStateTraceType = TxStateTrace PeerAddr TxId


runTxSubmission
  :: forall m peeraddr txid.
     ( MonadAsync m
     , MonadDelay m
     , MonadEvaluate m
     , MonadFork  m
     , MonadMask  m
     , MonadSay   m
     , MonadST    m
     , MonadLabelledSTM m
     , MonadTime  m
     , MonadTimer m
     , MonadThrow m
     , MonadThrow (STM m)
     , MonadMonotonicTime m
     , MonadTraceSTM m
     , Ord txid
     , Eq  txid
     , ShowProxy txid
     , NoThunks (Tx txid)
     , Typeable txid
     , Show peeraddr
     , Ord peeraddr
     , Typeable peeraddr

     , txid ~ Int
     )
  => Tracer m (String, TraceSendRecv (TxSubmission2 txid (Tx txid)))
  -> Tracer m (TraceTxLogic peeraddr txid (Tx txid))
  -> Tracer m TxSubmissionCounters
  -> Map peeraddr ( [Tx txid]
                  , ControlMessageSTM m
                  , Maybe DiffTime
                  , Maybe DiffTime
                  )
  -> Map peeraddr Impairment
  -> TxDecisionPolicy
  -> m ([Tx txid], [[Tx txid]])
  -- ^ inbound and outbound mempools
runTxSubmission tracer _tracerTxLogic countersTracer st0 peerImpairmentMap txDecisionPolicy = do
    st <- traverse (\(b, c, d, e) -> do
        mempool <- newMempool b
        (outChannel, inChannel) <- createConnectedChannels
        return (mempool, c, d, e, outChannel, inChannel)
        ) st0
    inboundMempool <- emptyMempool
    let txMap = Map.fromList [ (getTxId tx, tx)
                             | (txs, _, _, _) <- Map.elems st0
                             , tx <- txs]

    duplicateTxIdsVar <- Lazy.newTVarIO []
    sharedTxStateVar <- newSharedTxStateVar emptySharedTxState
    inFlightRegistry <- newPeerTxInFlightRegistry
    txCountersVar <- newTxSubmissionCountersVar mempty
    traceTVarIO sharedTxStateVar \_ -> return . TraceDynamic . TxStateTrace
    labelTVarIO sharedTxStateVar "shared-tx-state"

    let clients = (\(addr, (mempool {- txs -}, ctrlMsgSTM, outDelay, _, outChannel, _)) -> do
                    let baseClient = txSubmissionOutbound
                                       (Tracer $ say . show)
                                       (NumTxIdsToAck $ getNumTxIdsToReq
                                         $ maxUnacknowledgedTxIds txDecisionPolicy)
                                       (getMempoolReader mempool)
                                       (maxBound :: TestVersion)
                                       ctrlMsgSTM
                        imp        = Map.findWithDefault noImpairment addr peerImpairmentMap
                    client <- applyImpairment imp baseClient
                    runPeerWithLimits (("OUTBOUND " ++ show addr,) `contramap` tracer)
                                      txSubmissionCodec2
                                      (byteLimitsTxSubmission2 (fromIntegral . BSL.length))
                                      timeLimitsTxSubmission2
                                      (maybe id delayChannel outDelay outChannel)
                                      (txSubmissionClientPeer client)
                  )
                 <$> Map.assocs st

        servers = (\(addr, (_, _, _, inDelay, _, inChannel)) ->
                     withPeer txDecisionPolicy
                              (getMempoolReader inboundMempool)
                              sharedTxStateVar
                              inFlightRegistry
                              txCountersVar
                              addr $ \api -> do
                                let server =
                                      txSubmissionInboundV2 sayTracer
                                                            NoTxSubmissionInitDelay
                                                            txDecisionPolicy
                                                            (getMempoolWriter duplicateTxIdsVar inboundMempool)
                                                            getTxSize
                                                            api
                                runPipelinedPeerWithLimits
                                  (("INBOUND " ++ show addr,) `contramap` sayTracer)
                                  txSubmissionCodec2
                                  (byteLimitsTxSubmission2 (fromIntegral . BSL.length))
                                  timeLimitsTxSubmission2
                                  (maybe id delayChannel inDelay inChannel)
                                  (txSubmissionServerPeerPipelined server)
                  ) <$> Map.assocs st

    withAsync (txCountersThreadV2 txDecisionPolicy countersTracer
                                  txCountersVar sharedTxStateVar inFlightRegistry)
              \countersAid ->
      withAsyncAll (zip clients servers) $ \as -> do
        _ <- waitAllServers as
        cancel countersAid

        inmp <- readMempool inboundMempool
        dupTxIds <- Lazy.readTVarIO duplicateTxIdsVar
        let outmp = map (\(txs, _, _, _) -> txs)
                  $ Map.elems st0
            dupTxs = [ txMap Map.! txid | txid <- dupTxIds]

        return (inmp <> dupTxs, outmp)
  where
    waitAllServers :: [(Async m x, Async m x)] -> m [Either SomeException x]
    waitAllServers [] = return []
    waitAllServers ((client, server):as) = do
      r <- waitCatch server
      -- cancel client as soon as the server exits
      cancel client
      rs <- waitAllServers as
      return (r : rs)

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
txSubmissionSimulation (TxSubmissionState state peerImpairment txDecisionPolicy) = do
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
      let tracer :: forall a. (Show a, Typeable a) => Tracer (IOSim s) a
          tracer = dynamicTracer <> sayTracer -- <> verboseTracer <> debugTracer
      runTxSubmission tracer tracer tracer state'' peerImpairment txDecisionPolicy

filterValidTxs :: [Tx txid] -> [Tx txid]
filterValidTxs
  = filter getTxValid
  . takeWhile (\Tx{getTxSize, getTxAdvSize} -> getTxSize == getTxAdvSize)

-- | Pretty-print only the say-tracer events of an IOSim trace, prefixed
-- with their simulation time.  The full 'ppTrace' includes scheduling
-- noise (ThreadDelay, Deschedule, TxCommitted, etc.) that obscures the
-- protocol-relevant events; this helper restricts the output to the
-- events that the test infrastructure forwards through 'sayTracer'
-- (protocol messages, counter snapshots, shared-state changes).
ppSayTrace :: SimTrace a -> String
ppSayTrace tr =
    List.intercalate "\n"
  $ map (\(Time t, ev) -> show t <> " " <> ev)
  $ selectTraceEventsSayWithTime' tr

-- | Documented invariants of a 'TxSubmissionState' produced by the
-- 'Arbitrary' instance.  Used by the generator/shrinker meta-tests.
validTxSubmissionState :: TxSubmissionState -> Bool
validTxSubmissionState (TxSubmissionState peerMap peerImpairment policy) =
     not (Map.null peerMap)
  && Map.keysSet peerImpairment `Set.isSubsetOf` Map.keysSet peerMap
  && all validPeer (Map.elems peerMap)
  && validPolicy policy
  where
    validPeer (txs, _, _) =
      let txids = getTxId <$> txs in
         not (null txs)
      && length txids == Set.size (Set.fromList txids)

validPolicy :: TxDecisionPolicy -> Bool
validPolicy TxDecisionPolicy
            { maxNumTxIdsToRequest, maxUnacknowledgedTxIds
            , txsSizeInflightPerPeer, maxOutstandingTxBatchesPerPeer
            , txInflightMultiplicity, bufferedTxsMinLifetime
            , scoreRate, scoreMax, interTxSpace, inflightTimeout
            } =
     getNumTxIdsToReq maxNumTxIdsToRequest    >= 1
  && getNumTxIdsToReq maxUnacknowledgedTxIds  >= 1
  && getSizeInBytes txsSizeInflightPerPeer    >= 1
  && maxOutstandingTxBatchesPerPeer           >= 1
  && txInflightMultiplicity                   >= 1
  && bufferedTxsMinLifetime                   >= 0
  && scoreRate                                >= 0
  && scoreMax                                 >= 0
  && interTxSpace                             >= 0
  && inflightTimeout                          >  interTxSpace

prop_TxSubmissionState_validGen :: TxSubmissionState -> Property
prop_TxSubmissionState_validGen st =
    counterexample (show st)
  $ validTxSubmissionState st

prop_TxSubmissionState_shrinkValid :: TxSubmissionState -> Property
prop_TxSubmissionState_shrinkValid st = conjoin
  [ counterexample (show s) (validTxSubmissionState s)
  | s <- shrink st
  ]

-- | Every shrunk state differs from the input. Catches base cases like
-- 'shrinkMap1 = [m]' that re-emit the original.
prop_TxSubmissionState_shrinkSmaller :: TxSubmissionState -> Property
prop_TxSubmissionState_shrinkSmaller st = conjoin
  [ counterexample ("shrink emitted self: " ++ show s) (s /= st)
  | s <- shrink st
  ]

-- | Shrink output contains no duplicates. Catches 'shrinkMap1' generating
-- the same singleton via different paths and the cross-product producing
-- equivalent candidates.
prop_TxSubmissionState_shrinkNoDups :: TxSubmissionState -> Property
prop_TxSubmissionState_shrinkNoDups st =
  let shrunk = shrink st in
      counterexample ("duplicates: " ++ show (shrunk List.\\ List.nub shrunk))
    $ length (List.nub shrunk) === length shrunk


-- | Invariants over the counter snapshots emitted by 'txCountersThreadV2'.
-- Asserts monotonicity of every field, protocol-level causality bounds,
-- decomposition of total txid sends into blocking and pipelined, and body
-- accounting (received bounded by requested, classified bounded by received).
prop_counterInvariants :: SimTrace a -> Property
prop_counterInvariants tr =
    let snapshots :: [TxSubmissionCounters]
        snapshots = selectTraceEventsDynamic tr in
        counterexample ("snapshots: " ++ show (length snapshots))
      $ conjoin
          [ counterexample "monotonicity"  (checkMonotonic snapshots)
          , counterexample "causality"     (conjoin (checkCausality       <$> snapshots))
          , counterexample "decomposition" (conjoin (checkDecomp          <$> snapshots))
          , counterexample "body-accounting"
              (conjoin (checkBodyAccounting <$> snapshots))
          ]
  where
    counterFields :: [(String, TxSubmissionCounters -> Word64)]
    counterFields =
      [ ("txIdMessagesSent",      txIdMessagesSent)
      , ("txIdsRequested",        txIdsRequested)
      , ("txIdRepliesReceived",   txIdRepliesReceived)
      , ("txIdsReceived",         txIdsReceived)
      , ("txMessagesSent",        txMessagesSent)
      , ("txsRequested",          txsRequested)
      , ("txRepliesReceived",     txRepliesReceived)
      , ("txsReceived",           txsReceived)
      , ("txsOmitted",            txsOmitted)
      , ("lateBodies",            lateBodies)
      , ("txsAccepted",           txsAccepted)
      , ("txsRejected",           txsRejected)
      , ("txIdBlockingReqsSent",  txIdBlockingReqsSent)
      , ("txIdPipelinedReqsSent", txIdPipelinedReqsSent)
      , ("txIdBlockingWaitMs",    txIdBlockingWaitMs)
      , ("txPipelineWaitMs",      txPipelineWaitMs)
      , ("txSubmissionWaitMs",    txSubmissionWaitMs)
      ]

    checkMonotonic xs = conjoin
      [ counterexample (name ++ ": " ++ show (f a) ++ " > " ++ show (f b))
          (f a <= f b)
      | (name, f) <- counterFields
      , (a, b)    <- zip xs (drop 1 xs)
      ]

    checkCausality s = conjoin
      [ counterexample "txIdRepliesReceived > txIdMessagesSent"
          (txIdRepliesReceived s <= txIdMessagesSent s)
      , counterexample "txRepliesReceived > txMessagesSent"
          (txRepliesReceived s <= txMessagesSent s)
      , counterexample "txIdsReceived > txIdsRequested"
          (txIdsReceived s <= txIdsRequested s)
      , counterexample "txsReceived > txsRequested"
          (txsReceived s <= txsRequested s)
      ]

    checkDecomp s =
        counterexample "txIdMessagesSent /= blocking + pipelined"
      $ txIdMessagesSent s
      === txIdBlockingReqsSent s + txIdPipelinedReqsSent s

    checkBodyAccounting s =
        counterexample
          ("accepted + rejected + late > received: "
             ++ show (txsAccepted s, txsRejected s, lateBodies s, txsReceived s))
          (txsAccepted s + txsRejected s + lateBodies s <= txsReceived s)


-- | Tests overall tx submission semantics. The properties checked in this
-- property test are the same as for tx submission v1. We need this to know we
-- didn't regress.
--
prop_txSubmission :: TxSubmissionState -> Property
prop_txSubmission st@(TxSubmissionState peers _ _) =
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
      . label ("number of txs: "
              ++
              renderRanges 10
                ( Set.size
                . foldMap (Set.fromList . (\(txs, _, _) -> getTxId <$> txs))
                $ Map.elems peers
                ))
      . label ("number of peers with wrongly sized tx: "
             ++ show numPeersWithWronglySizedTx)
      $ case traceResult True tr of
         Left e ->
             counterexample (show e)
           . counterexample (ppSayTrace tr)
           $ False
         Right (inmp, outmps) ->
             counterexample (ppSayTrace tr)
           $ conjoin (validate inmp `map` outmps) .&&. prop_counterInvariants tr
  where
    -- | Asserts that every txid produced is present in the consumer set.
    -- On failure the counterexample names the missing txids so the
    -- diagnostic points directly at what's wrong.
    checkMempools :: [Tx Int] -> [Tx Int] -> Property
    checkMempools consumer producer =
      let producer' = Set.fromList (getTxId <$> producer)
          consumer' = Set.fromList (getTxId <$> consumer)
          missing   = producer' `Set.difference` consumer' in
        counterexample ("missing from inbound mempool: " ++ show (Set.toList missing))
      $ Set.null missing

    validate :: [Tx Int] -- the inbound mempool
             -> [Tx Int] -- one of the outbound mempools
             -> Property
    validate inmp outmp =
       let outUniqueTxIds = nubBy (on (==) getTxId) outmp
           outValidTxs    = filterValidTxs outmp in
         counterexample ("inbound mempool: "  ++ show (getTxId <$> inmp))
       . counterexample ("outbound mempool: " ++ show (getTxId <$> outmp))
       $ case ( length outUniqueTxIds == length outmp
              , length outValidTxs    == length outmp
              ) of
         (True, True) ->
             counterexample
               "case (unique-txids, all-valid): every valid tx must reach inbound"
           $ checkMempools inmp outValidTxs

         (True, False) | Nothing <- List.find (\tx -> getTxAdvSize tx /= getTxSize tx) outmp ->
             counterexample
               "case (unique-txids, has-invalid, sizes-match): every valid tx must reach inbound"
           $ checkMempools inmp outValidTxs

         (True, False) ->
             -- One tx has a wrong advertised size: the peer is cheating, no
             -- guarantee on how much we download.
             counterexample
               "case (unique-txids, has-invalid, has-size-mismatch): peer cheating, no guarantee"
           $ property True

         (False, True) ->
             counterexample
               "case (duplicate-txids, all-valid): some version of every valid txid must reach inbound"
           $ checkMempools inmp (filterValidTxs outUniqueTxIds)

         (False, False) ->
           -- If we are presented with a stream of valid and invalid Txs with
           -- duplicate txids we're content with completing the protocol
           -- without error.
           property True


-- | This test checks that all txs are downloaded from all available peers if
-- available.
--
-- TODO: have we generated enough outbound mempools which interact in interesting
-- ways?
prop_txSubmission_inflight :: TxSubmissionState -> Property
prop_txSubmission_inflight st@(TxSubmissionState state _ policy) =
  let maxRepeatedValidTxs = Map.foldr (\(txs, _, _) r -> foldr fn r txs)
                                      Map.empty
                                      state
      hasInvalidSize =
          isJust
        $ List.find (\(txs, _, _) ->
                    isJust $ List.find (\tx -> getTxAdvSize tx /= getTxSize tx) txs
                  )
                  state
      trace = runSimTrace (txSubmissionSimulation st)
      pTrace = ppSayTrace trace
  in case traceResult True trace of
       Left err -> counterexample pTrace
                 $ counterexample (show err)
                 $ property False
       Right (inmp, _) ->
         let resultRepeatedValidTxs =
               foldr fn Map.empty inmp
         in label (if hasInvalidSize then "has wrongly sized tx" else "has no wrongly sized tx")
          . counterexample pTrace
          . counterexample ("hasInvalidSize: " <> show hasInvalidSize)
          . counterexample ("Result valid [(txid, repeated)]:\n" <> show resultRepeatedValidTxs)
          . counterexample ("Testcase max valid [(txid, repeated)]:\n" <> show maxRepeatedValidTxs)
          . (\p -> p .&&. prop_counterInvariants trace)
          . conjoin . Map.elems $ if hasInvalidSize
              then merge (mapMissing \_txid _left  -> error "impossible")
                         (mapMissing \_txid _right -> True)
                         (zipWithMatched \_txid left right ->
                             left <= right `min` inflightLimit)
                         resultRepeatedValidTxs
                         maxRepeatedValidTxs
              else merge (mapMissing \_txid _left  -> error "impossible")
                         (mapMissing \_txid _right -> False)
                         (zipWithMatched \_txid left right ->
                             left <= right `min` inflightLimit)
                         resultRepeatedValidTxs
                         maxRepeatedValidTxs
  where
    -- Loosened from txInflightMultiplicity to account for the per-tx
    -- 'currentMaxInflightMultiplicity' bumps that fire when a peer holds
    -- a lease past 'inflightTimeout'. Each peer can contribute at most one
    -- bump per stuck claim, so 'cap + peers' is a safe static upper bound.
    inflightLimit = txInflightMultiplicity policy + Map.size state

    -- we work with txid's because a repeated tx may have different advertised/actual
    -- byte size by different peers in this test, but otherwise multiplicity
    -- should be determined by txid.
    fn :: Tx TxId -> Map TxId Int -> Map TxId Int
    fn tx r' -- | getTxAdvSize tx /= getTxSize tx
             -- = empty
             -- ^ that is too severe
             | getTxValid tx
             = Map.alter (Just . maybe 1 succ) (getTxId tx) r'
             | otherwise
             = r'


-- | Resilience to per-peer impairment. With a non-empty subset of peers
-- wrapped in any combination of 'omitBodies' and 'delayBodies', every tx
-- contributed by a well-behaved peer must still reach the inbound mempool.
-- Exercises V2's cross-peer retry path (omission) and stuck-leaseholder
-- bump path (delay).
--
-- Contributions from an impaired peer are not asserted: omitted bodies
-- may never reach the mempool, and severely delayed bodies may not arrive
-- before the simulation terminates.
prop_txSubmission_resilientToImpairment :: TxSubmissionState -> Property
prop_txSubmission_resilientToImpairment baseSt =
    forAll (genImpairment (Map.keys (peerMap baseSt))) $ \imp ->
      not (Map.null imp) ==>
      let st         = baseSt { peerImpairment = imp }
          allAddrs   = Map.keysSet (peerMap st)
          wbAddrs    = allAddrs `Set.difference` Map.keysSet imp
          wbPeerTxs  = [ txs
                       | addr <- Set.toList wbAddrs
                       , let (txs, _, _) = peerMap st Map.! addr ]
          tr         = runSimTrace (txSubmissionSimulation st)
      in not (Set.null wbAddrs) ==>
         label ("impaired peers: "     ++ show (Map.size imp)) $
         label ("well-behaved peers: " ++ show (Set.size wbAddrs)) $
         tabulate "impairment kind" (kindOf <$> Map.elems imp) $
         case traceResult True tr of
           Left e ->
               counterexample (show e)
             . counterexample (ppSayTrace tr)
             $ False
           Right (inmp, _) ->
               counterexample (ppSayTrace tr)
             $ conjoin (validateWellBehaved inmp `map` wbPeerTxs)
               .&&. prop_counterInvariants tr
  where
    -- Pick a non-empty proper subset of peers to impair. Each impaired
    -- peer gets some mix of body delay and per-body omission (at least
    -- one of the two).
    genImpairment :: [Int] -> Gen (Map Int Impairment)
    genImpairment addrs
      | length addrs < 2 = pure Map.empty
      | otherwise = do
          n        <- choose (1, length addrs - 1)
          shuffled <- shuffle addrs
          let impaired = take n shuffled
          imps <- traverse (const genOneImpairment) impaired
          pure (Map.fromList (zip impaired imps))

    genOneImpairment :: Gen Impairment
    genOneImpairment = oneof [genOmit, genDelay, genBoth]

    genOmit = do
      p    <- choose (0.1 :: Double, 0.9)
      seed <- arbitrary
      pure Impairment { impairBodyDelay = Nothing
                      , impairOmitProb  = p
                      , impairSeed      = seed
                      }
    genDelay = do
      d <- choose (0.1 :: Double, 2.0)
      pure Impairment { impairBodyDelay = Just (realToFrac d)
                      , impairOmitProb  = 0
                      , impairSeed      = 0
                      }
    genBoth = do
      p    <- choose (0.1 :: Double, 0.9)
      seed <- arbitrary
      d    <- choose (0.1 :: Double, 2.0)
      pure Impairment { impairBodyDelay = Just (realToFrac d)
                      , impairOmitProb  = p
                      , impairSeed      = seed
                      }

    kindOf Impairment { impairBodyDelay = Nothing, impairOmitProb = _ } = "omit-only"
    kindOf Impairment { impairBodyDelay = Just _,  impairOmitProb = 0 } = "delay-only"
    kindOf Impairment { impairBodyDelay = Just _,  impairOmitProb = _ } = "delay+omit"

    -- Same shape as 'validate' inside 'prop_txSubmission'. Only assert
    -- coverage when the peer's stream is all-unique-and-all-valid; the
    -- duplicate / invalid-prefix cases are out of scope here and covered
    -- by the existing 'prop_txSubmission'.
    validateWellBehaved :: [Tx Int] -> [Tx Int] -> Property
    validateWellBehaved inmp outmp =
      let outUnique = nubBy ((==) `on` getTxId) outmp
          outValid  = filterValidTxs outmp in
      if length outUnique == length outmp && length outValid == length outmp
        then
          let outIds  = Set.fromList (getTxId <$> outValid)
              inIds   = Set.fromList (getTxId <$> inmp)
              missing = outIds `Set.difference` inIds in
            counterexample ("missing: " ++ show (Set.toList missing))
          $ property (Set.null missing)
        else
          property True


prop_sharedTxStateInvariant :: TxSubmissionState -> Property
prop_sharedTxStateInvariant initialState@(TxSubmissionState st0 _ _) =
  let tr = runSimTrace (() <$ txSubmissionSimulation initialState)
      pTrace = ppSayTrace tr
  in case traceResult True tr of
    Left err -> counterexample pTrace
              . counterexample (show err)
              $ False
    Right _ ->
      let tracedStates :: [TxStateTraceType]
          tracedStates = Trace.toList $ traceSelectTraceEventsDynamic tr
      in counterexample pTrace case
           foldMap (\case
                      TxStateTrace st ->
                        ( Every . counterexample (show st) $
                            sharedTxStateInvariant WeakInvariant st
                        , Sum 1
                        )
                   )
                   tracedStates
           of (p, Sum c) ->
                 label ("number of txs: "
                         ++
                         renderRanges 10
                           ( Set.size
                           . foldMap (Set.fromList . (\(txs, _, _) -> getTxId <$> txs))
                           $ Map.elems st0
                           ))
               . label ("number of evaluated states: "
                         ++ renderRanges 100 c)
               $ p

--
-- Utils
--

-- | Split a list into sub list of at most `n` elements.
--
divvy :: Int -> [a] -> [[a]]
divvy _ [] = []
divvy n as = take n as : divvy n (drop n as)

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
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Ouroboros.Network.TxSubmission.AppV2 (tests) where

import Prelude hiding (seq)

import NoThunks.Class

import Control.Concurrent.Class.MonadSTM qualified as Lazy
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad (void)
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadSay
import Control.Monad.Class.MonadST
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import Control.Monad.IOSim
import Control.Tracer (Tracer (..), contramap, nullTracer)

import Data.ByteString.Lazy qualified as BSL
import Data.Foldable (traverse_)
import Data.Function (on)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
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
import Ouroboros.Network.Tx (HasRawTxId)
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
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, testCase)
import Test.Tasty.QuickCheck (testProperty)


tests :: TestTree
tests = testGroup "AppV2"
  [ testGroup "Generators"
      [ testProperty "TxSubmissionState/validGen"
          prop_TxSubmissionState_validGen
      , testProperty "TxSubmissionState/shrinkValid"
          prop_TxSubmissionState_shrinkValid
      , testProperty "TxSubmissionState/shrinkSmaller"
          prop_TxSubmissionState_shrinkSmaller
      , testProperty "TxSubmissionState/shrinkNoDups"
          prop_TxSubmissionState_shrinkNoDups
      , testProperty "TxSubmissionImpairmentState/validGen"
          prop_TxSubmissionImpairmentState_validGen
      , testProperty "TxSubmissionImpairmentState/shrinkValid"
          prop_TxSubmissionImpairmentState_shrinkValid
      , testProperty "TxSubmissionImpairmentState/shrinkSmaller"
          prop_TxSubmissionImpairmentState_shrinkSmaller
      , testProperty "TxSubmissionImpairmentState/shrinkNoDups"
          prop_TxSubmissionImpairmentState_shrinkNoDups
      , testProperty "TxSubmissionDisconnectState/validGen"
          prop_TxSubmissionDisconnectState_validGen
      , testProperty "TxSubmissionDisconnectState/shrinkValid"
          prop_TxSubmissionDisconnectState_shrinkValid
      , testProperty "TxSubmissionDisconnectState/shrinkSmaller"
          prop_TxSubmissionDisconnectState_shrinkSmaller
      , testProperty "TxSubmissionDisconnectState/shrinkNoDups"
          prop_TxSubmissionDisconnectState_shrinkNoDups
      ]
  , testProperty "txSubmission"          prop_txSubmission
  , testProperty "inflight"              prop_txSubmission_inflight
  , testProperty "resilientToImpairment" prop_txSubmission_resilientToImpairment
  , testProperty "peerDisconnect"        prop_txSubmission_peerDisconnect
  , testProperty "SharedTxState" $ withMaxSize 25
                                 $ withMaxSuccess 25
                                 prop_sharedTxStateInvariant
  , testCase     "counterEmission/cadence"      unit_counterEmission_cadence
  , testCase     "score/wellBehavedStaysAtZero" unit_score_wellBehavedStaysAtZero
  , testCase     "score/persistentBadStaysHigh" unit_score_persistentBadStaysHigh
  , testCase     "score/recoversAfterBurst"     unit_score_recoversAfterBurst
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

-- | Per-peer inbound trace event, tagging each 'TraceTxSubmissionInbound'
-- value with the peer it was emitted for.  Tests extract these from the
-- simulation trace via 'traceSelectTraceEventsDynamic' to observe per-peer
-- score evolution (carried in 'TraceTxSubmissionProcessed') and other
-- inbound protocol events.
data PeerInboundTrace peeraddr txid tx =
    PeerInboundTrace peeraddr (TraceTxSubmissionInbound txid tx)
  deriving Show

type PeerInboundTraceType = PeerInboundTrace PeerAddr TxId (Tx TxId)


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
  -> Tracer m (PeerInboundTrace peeraddr txid (Tx txid))
  -> Map peeraddr ( [Tx txid]
                  , ControlMessageSTM m
                  , Maybe DiffTime
                  , Maybe DiffTime
                  )
  -> Map peeraddr Impairment
  -> Map peeraddr DiffTime
     -- ^ cancel the server async for each listed peer at the given
     --   time, simulating an abrupt disconnect mid-protocol
  -> TxDecisionPolicy
  -> m ([Tx txid], [[Tx txid]], SharedTxState peeraddr txid)
  -- ^ inbound mempool, outbound mempools, final shared state
runTxSubmission tracer tracerTxLogic countersTracer inboundTracer st0
                peerImpairmentMap cancelSchedule txDecisionPolicy = do
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
                    client <- applyImpairment imp mkUnrequested baseClient
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
                                      txSubmissionInboundV2
                                        (PeerInboundTrace addr `contramap` inboundTracer)
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

    withAsync (txCountersThreadV2 txDecisionPolicy countersTracer tracerTxLogic
                                  txCountersVar sharedTxStateVar inFlightRegistry)
              \countersAid ->
      withAsyncAll (zip clients servers) $ \as -> do
        let serverAsyncs = Map.fromList
                         $ zip (Map.keys st0) (snd <$> as)
        cancelAids <- traverse
                        (\(addr, t) -> async $ do
                           threadDelay t
                           mapM_ cancel (Map.lookup addr serverAsyncs))
                        (Map.toList cancelSchedule)
        _ <- waitAllServers as

        -- All peers have scrubbed their per-peer state via the
        -- 'withPeer' bracket finalizer.  Wait long enough for retained
        -- entries to expire so a sweep tick (still firing inside the
        -- running counters thread) can clear them along with any
        -- orphans and stale lookups.
        threadDelay (bufferedTxsMinLifetime txDecisionPolicy + 1)

        cancel countersAid
        traverse_ cancel cancelAids

        finalSharedState <- readTVarIO sharedTxStateVar
        inmp     <- readMempool inboundMempool
        dupTxIds <- Lazy.readTVarIO duplicateTxIdsVar
        let outmp  = map (\(txs, _, _, _) -> txs)
                   $ Map.elems st0
            dupTxs = [ txMap Map.! txid | txid <- dupTxIds]

        return (inmp <> dupTxs, outmp, finalSharedState)
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
                       -> IOSim s ([Tx Int], [[Tx Int]], SharedTxState PeerAddr TxId)
                       -- ^ inbound mempool, outbound mempools, final shared state
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
      runTxSubmission tracer tracer tracer tracer state'' peerImpairment
                      Map.empty txDecisionPolicy

filterValidTxs :: [Tx txid] -> [Tx txid]
filterValidTxs
  = filter getTxValid
  . takeWhile (\Tx{getTxSize, getTxAdvSize} -> getTxSize == getTxAdvSize)

-- | Mutator passed to 'applyImpairment' for the @unrequestedTx@ wrapper.
-- Returns a clone of the given body with its txid replaced by 'maxBound',
-- which is well outside the @Arbitrary Int@ range used by the generators
-- and so is guaranteed not to collide with any requested txid.
mkUnrequested :: [Int] -> Tx Int -> Tx Int
mkUnrequested _reqs tx = tx { getTxId = maxBound }

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


-- | Asserts that the 'SharedTxState' has been fully reclaimed after
-- all peers have exited and retained entries have been allowed to
-- expire.  Catches leaks in scrub-on-disconnect, sweep orphan
-- detection, retained-set expiry, and lookup-table cleanup.
prop_sharedStateClean :: (Show peeraddr, Show txid, HasRawTxId txid)
                      => SharedTxState peeraddr txid -> Property
prop_sharedStateClean st =
    counterexample ("final shared state: " ++ show st)
  $ conjoin
      [ counterexample "sharedTxTable not empty" $
          IntMap.null (sharedTxTable st)
      , counterexample "sharedRetainedTxs not empty" $
          IntSet.null (retainedKeysSet (sharedRetainedTxs st))
      , counterexample "sharedTxIdToKey not empty" $
          Map.null (sharedTxIdToKey st)
      , counterexample "sharedKeyToTxId not empty" $
          IntMap.null (sharedKeyToTxId st)
      ]


-- | Pin the cadence of 'txCountersThreadV2'.  After a single counter
-- bump at thread start, exactly one emission must fire at exactly the
-- first 'countersInterval' deadline.  Beyond that, with no further
-- counter activity, the 'current /= previous' guard must suppress
-- subsequent emissions.  Targets:
--
-- * the 'now >= nextEmitAt' boundary,
-- * the 'when (current /= previous)' guard,
-- * the 'previous' update.
unit_counterEmission_cadence :: Assertion
unit_counterEmission_cadence =
    let (threadStart, timeline) = runSimOrThrow simulation
        timestamps              = map fst timeline in
    assertEqual ("timeline: " ++ show timeline)
                [addTime 7 threadStart]
                timestamps
  where
    simulation :: forall s. IOSim s (Time, [(Time, TxSubmissionCounters)])
    simulation = do
      sharedTxStateVar <- newSharedTxStateVar
                            (emptySharedTxState :: SharedTxState Int Int)
      inFlightRegistry <- newPeerTxInFlightRegistry
                            :: IOSim s (PeerTxInFlightRegistry (IOSim s) Int)
      txCountersVar    <- newTxSubmissionCountersVar mempty
      recorder         <- newTVarIO []

      let policy = defaultTxDecisionPolicy
          tracer = Tracer $ \counters -> do
                     now <- getMonotonicTime
                     atomically (modifyTVar recorder ((now, counters):))

      threadStart <- getMonotonicTime
      atomically (modifyTVar txCountersVar
                    (<> mempty { txIdMessagesSent = 1 }))

      withAsync (txCountersThreadV2 policy tracer nullTracer txCountersVar
                                    sharedTxStateVar inFlightRegistry)
                \countersAid -> do
        -- Run past the second emission boundary (threadStart + 14s) so
        -- any spurious second emission shows up in the recorder.
        threadDelay 14.5
        cancel countersAid

      timeline <- readTVarIO recorder
      pure (threadStart, reverse timeline)


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
         Right (inmp, outmps, finalState) ->
             counterexample (ppSayTrace tr)
           $ conjoin (validate inmp `map` outmps)
             .&&. prop_counterInvariants tr
             .&&. prop_sharedStateClean finalState
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
       Right (inmp, _, finalState) ->
         let resultRepeatedValidTxs =
               foldr fn Map.empty inmp
         in label (if hasInvalidSize then "has wrongly sized tx" else "has no wrongly sized tx")
          . counterexample pTrace
          . counterexample ("hasInvalidSize: " <> show hasInvalidSize)
          . counterexample ("Result valid [(txid, repeated)]:\n" <> show resultRepeatedValidTxs)
          . counterexample ("Testcase max valid [(txid, repeated)]:\n" <> show maxRepeatedValidTxs)
          . (\p -> p .&&. prop_counterInvariants trace
                     .&&. prop_sharedStateClean finalState)
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
-- | A 'TxSubmissionState' carrying the additional invariants required by
-- 'prop_txSubmission_resilientToImpairment': at least two peers, with a
-- non-empty proper subset of them assigned an 'Impairment'.
newtype TxSubmissionImpairmentState =
    TxSubmissionImpairmentState
      { unTxSubmissionImpairmentState :: TxSubmissionState }
  deriving (Eq, Show)

instance Arbitrary TxSubmissionImpairmentState where
  arbitrary = do
    base <- arbitrary `suchThat` ((>= 2) . Map.size . peerMap)
    imp  <- genImpairment (Map.keys (peerMap base))
    pure $ TxSubmissionImpairmentState base { peerImpairment = imp }

  shrink (TxSubmissionImpairmentState st) =
       [ TxSubmissionImpairmentState st' { peerImpairment = imp' }
       | st' <- shrink st
       , Map.size (peerMap st') >= 2
       , let imp' = Map.restrictKeys (peerImpairment st)
                                     (Map.keysSet (peerMap st'))
       , not (Map.null imp')
       , Map.size imp' < Map.size (peerMap st')
       ]
    ++ [ TxSubmissionImpairmentState st { peerImpairment = imp' }
       | imp' <- shrinkImpairmentMap (peerImpairment st)
       , not (Map.null imp')
       ]

validTxSubmissionImpairmentState :: TxSubmissionImpairmentState -> Bool
validTxSubmissionImpairmentState (TxSubmissionImpairmentState st) =
     validTxSubmissionState st
  && Map.size (peerMap st) >= 2
  && not (Map.null (peerImpairment st))
  && Map.size (peerImpairment st) < Map.size (peerMap st)

-- Pick a non-empty proper subset of peers to impair, each given some
-- mix of body delay, per-body omission, txid-overflow, or unrequested
-- body injection.
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
genOneImpairment = oneof [genOmit, genDelay, genBoth, genExtraTxIds, genUnrequestedTx]
  where
    genOmit = do
      p    <- choose (0.1 :: Double, 0.9)
      seed <- arbitrary
      pure noImpairment { impairOmitProb = p, impairSeed = seed }
    genDelay = do
      d <- choose (0.1 :: Double, 2.0)
      pure noImpairment { impairBodyDelay = Just (realToFrac d) }
    genBoth = do
      p    <- choose (0.1 :: Double, 0.9)
      seed <- arbitrary
      d    <- choose (0.1 :: Double, 2.0)
      pure noImpairment { impairBodyDelay = Just (realToFrac d)
                        , impairOmitProb  = p
                        , impairSeed      = seed
                        }
    genExtraTxIds = do
      n <- choose (1, 5 :: Int)
      pure noImpairment { impairExtraTxIds = fromIntegral n }
    genUnrequestedTx = pure noImpairment { impairUnrequestedTx = True }

shrinkImpairmentMap :: Map Int Impairment -> [Map Int Impairment]
shrinkImpairmentMap m =
     [ Map.delete k m | Map.size m > 1, k <- Map.keys m ]
  ++ [ Map.insert k imp' m
     | (k, imp) <- Map.toList m
     , imp' <- shrinkImpairment imp
     ]

shrinkImpairment :: Impairment -> [Impairment]
shrinkImpairment imp = List.nub $
     [ imp { impairUnrequestedTx = False } | impairUnrequestedTx imp ]
  ++ [ imp { impairExtraTxIds = 0 }        | impairExtraTxIds imp > 0 ]
  ++ [ imp { impairBodyDelay = Nothing }   | isJust (impairBodyDelay imp) ]
  ++ [ imp { impairOmitProb = 0 }          | impairOmitProb imp > 0 ]
  ++ [ imp { impairExtraTxIds = n }
     | n <- shrink (impairExtraTxIds imp)
     , n > 0
     , n /= impairExtraTxIds imp
     ]
  ++ [ imp { impairBodyDelay = Just (realToFrac d') }
     | Just d <- [impairBodyDelay imp]
     , d' <- shrink (realToFrac d :: Double)
     , d' > 0
     , realToFrac d' /= d
     ]
  ++ [ imp { impairOmitProb = p' }
     | impairOmitProb imp > 0
     , p' <- shrink (impairOmitProb imp)
     , p' > 0
     , p' /= impairOmitProb imp
     ]

kindOf :: Impairment -> String
kindOf Impairment { impairUnrequestedTx = True }                    = "unrequested-tx"
kindOf Impairment { impairExtraTxIds = n }                | n > 0   = "extra-txids"
kindOf Impairment { impairBodyDelay = Just _,  impairOmitProb = 0 } = "delay-only"
kindOf Impairment { impairBodyDelay = Just _,  impairOmitProb = _ } = "delay+omit"
kindOf Impairment { impairBodyDelay = Nothing, impairOmitProb = _ } = "omit-only"

prop_txSubmission_resilientToImpairment :: TxSubmissionImpairmentState -> Property
prop_txSubmission_resilientToImpairment (TxSubmissionImpairmentState st) =
    let imp       = peerImpairment st
        allAddrs  = Map.keysSet (peerMap st)
        wbAddrs   = allAddrs `Set.difference` Map.keysSet imp
        wbPeerTxs = [ txs | addr <- Set.toList wbAddrs
                          , let (txs, _, _) = peerMap st Map.! addr ]
        allOutIds = Set.fromList
                  $ concatMap (\(txs, _, _) -> getTxId <$> txs)
                  $ Map.elems (peerMap st)
        tr        = runSimTrace (txSubmissionSimulation st)
    in label ("impaired peers: "     ++ show (Map.size imp))
     $ label ("well-behaved peers: " ++ show (Set.size wbAddrs))
     $ tabulate "impairment kind" (kindOf <$> Map.elems imp)
     $ case traceResult True tr of
         Left e -> counterexample (show e)
                 . counterexample (ppSayTrace tr)
                 $ False
         Right (inmp, _, finalState) ->
             counterexample (ppSayTrace tr)
           $ conjoin (validateWellBehaved inmp `map` wbPeerTxs)
             .&&. noContamination allOutIds inmp
             .&&. prop_counterInvariants tr
             .&&. prop_sharedStateClean finalState
  where
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

    -- Inbound mempool must not contain any txid no peer ever advertised.
    -- Catches adversarial bodies (e.g. injected by 'unrequestedTx') that
    -- would slip past the inbound's protocol checks.
    noContamination :: Set.Set Int -> [Tx Int] -> Property
    noContamination allOutIds inmp =
      let inIds  = Set.fromList (getTxId <$> inmp)
          extras = inIds `Set.difference` allOutIds in
        counterexample ("contaminating txids: " ++ show (Set.toList extras))
      $ property (Set.null extras)

prop_TxSubmissionImpairmentState_validGen :: TxSubmissionImpairmentState -> Property
prop_TxSubmissionImpairmentState_validGen st =
    counterexample (show st)
  $ validTxSubmissionImpairmentState st

prop_TxSubmissionImpairmentState_shrinkValid :: TxSubmissionImpairmentState -> Property
prop_TxSubmissionImpairmentState_shrinkValid st = conjoin
  [ counterexample (show s) (validTxSubmissionImpairmentState s)
  | s <- shrink st
  ]

prop_TxSubmissionImpairmentState_shrinkSmaller :: TxSubmissionImpairmentState -> Property
prop_TxSubmissionImpairmentState_shrinkSmaller st = conjoin
  [ counterexample ("shrink emitted self: " ++ show s) (s /= st)
  | s <- shrink st
  ]

prop_TxSubmissionImpairmentState_shrinkNoDups :: TxSubmissionImpairmentState -> Property
prop_TxSubmissionImpairmentState_shrinkNoDups st =
  let shrunk = shrink st in
      counterexample ("duplicates: " ++ show (shrunk List.\\ List.nub shrunk))
    $ length (List.nub shrunk) === length shrunk


-- | How a disconnected peer leaves the simulation.
data ExitMethod = ExitClean | ExitCancel
  deriving (Eq, Show, Bounded, Enum)

-- | A 'TxSubmissionState' plus a per-peer early-exit schedule.
-- Invariants (encoded in the 'Arbitrary' instance):
--
-- * @peerMap@ has at least two peers.
-- * The schedule covers a non-empty proper subset of @peerMap@'s keys
--   (at least one peer runs to the end).
-- * Each scheduled time is in @[0.5, 5.0]@ seconds.
data TxSubmissionDisconnectState =
    TxSubmissionDisconnectState
      { tdsBase     :: TxSubmissionState
      , tdsSchedule :: Map Int (DiffTime, ExitMethod)
      }
  deriving (Eq, Show)

instance Arbitrary TxSubmissionDisconnectState where
  arbitrary = do
    base <- arbitrary `suchThat` ((>= 2) . Map.size . peerMap)
    sch  <- genDisconnectSchedule (Map.keys (peerMap base))
    pure (TxSubmissionDisconnectState base sch)

  shrink (TxSubmissionDisconnectState st sch) =
       [ TxSubmissionDisconnectState st' sch'
       | st' <- shrink st
       , Map.size (peerMap st') >= 2
       , let sch' = Map.restrictKeys sch (Map.keysSet (peerMap st'))
       , not (Map.null sch')
       , Map.size sch' < Map.size (peerMap st')
       ]
    ++ [ TxSubmissionDisconnectState st sch'
       | sch' <- shrinkDisconnectSchedule sch
       , not (Map.null sch')
       ]

validTxSubmissionDisconnectState :: TxSubmissionDisconnectState -> Bool
validTxSubmissionDisconnectState (TxSubmissionDisconnectState st sch) =
     validTxSubmissionState st
  && Map.size (peerMap st) >= 2
  && not (Map.null sch)
  && Map.keysSet sch `Set.isSubsetOf` Map.keysSet (peerMap st)
  && Map.size sch < Map.size (peerMap st)
  && all (\(t, _) -> t >= 0.5) (Map.elems sch)

genDisconnectSchedule :: [Int] -> Gen (Map Int (DiffTime, ExitMethod))
genDisconnectSchedule addrs
  | length addrs < 2 = pure Map.empty
  | otherwise = do
      n        <- choose (1, length addrs - 1)
      shuffled <- shuffle addrs
      let chosen = take n shuffled
      entries <- traverse (const genEntry) chosen
      pure (Map.fromList (zip chosen entries))
  where
    genEntry = do
      t <- frequency
             [ (1, choose (0.5 :: Double,  3.0))
             , (1, choose (3.0 :: Double,  5.0))
             , (3, choose (5.0 :: Double, 10.0))
             ]
      m <- elements [ExitClean, ExitCancel]
      pure (realToFrac t, m)

-- Drop a scheduled peer, simplify 'ExitCancel' to 'ExitClean', or
-- shrink the scheduled time toward 0.5 (the generator floor).
shrinkDisconnectSchedule :: Map Int (DiffTime, ExitMethod)
                    -> [Map Int (DiffTime, ExitMethod)]
shrinkDisconnectSchedule m = List.nub $
     [ Map.delete k m | Map.size m > 1, k <- Map.keys m ]
  ++ [ Map.insert k (t, ExitClean) m
     | (k, (t, ExitCancel)) <- Map.toList m
     ]
  ++ [ Map.insert k (realToFrac t', method) m
     | (k, (t, method)) <- Map.toList m
     , t' <- shrink (realToFrac t :: Double)
     , t' >= 0.5
     , realToFrac t' /= t
     ]

-- | Variant of 'txSubmissionSimulation' that lets each peer exit early
-- via the supplied schedule.
--
-- * 'ExitClean' peers get a second per-peer control TVar; their control
--   STM combines it with the global one so a 'Terminate' from either
--   source makes the outbound send 'MsgDone'.
-- * 'ExitCancel' peers are passed through to 'runTxSubmission' which
--   cancels the server async at the scheduled time, simulating an
--   abrupt disconnect.
txSubmissionSimulationDisconnect
  :: forall s. TxSubmissionDisconnectState
  -> IOSim s ( [Tx Int], [[Tx Int]], SharedTxState PeerAddr TxId )
txSubmissionSimulationDisconnect
    (TxSubmissionDisconnectState
       (TxSubmissionState state peerImpairment txDecisionPolicy) schedule) = do
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

  -- Per-peer disconnect TVar for the clean-exit subset.
  disconnectVars <- Map.fromList <$> sequence
                  [ (addr,) <$> newTVarIO Continue
                  | (addr, (_, ExitClean)) <- Map.toList schedule
                  ]

  let cancelSchedule = Map.fromList
                         [ (addr, t)
                         | (addr, (t, ExitCancel)) <- Map.toList schedule
                         ]

      -- Build a per-peer combined control STM that yields 'Terminate'
      -- when either the global or the per-peer disconnect TVar says so.
      combinedState :: Map PeerAddr ([Tx TxId], ControlMessageSTM (IOSim s), Maybe DiffTime, Maybe DiffTime)
      combinedState = Map.mapWithKey
        (\addr (txs, var, mbOutDelay, mbInDelay) ->
           let stm = case Map.lookup addr disconnectVars of
                       Nothing       -> readTVar var
                       Just disconnectVar -> do
                         g <- readTVar var
                         c <- readTVar disconnectVar
                         pure (if g == Terminate || c == Terminate
                                  then Terminate else g)
           in (txs, stm, mbOutDelay, mbInDelay))
        state'

      simDelayTime = Map.foldl' (\m (txs, _, mbInDelay, mbOutDelay) ->
                                  max m ( fromMaybe 1 (max <$> mbInDelay <*> mbOutDelay)
                                        * realToFrac (length txs `div` 4)
                                        )
                                )
                                0
                                state'
      controlMessageVars = (\(_, x, _, _) -> x) <$> Map.elems state'

  withAsync
    (do threadDelay (simDelayTime + 1000)
        atomically (traverse_ (`writeTVar` Terminate) controlMessageVars)
    ) \_ -> do
      disconnectAids <-
        traverse (\(addr, (t, _)) ->
                    case Map.lookup addr disconnectVars of
                      Just v -> async $ do
                                  threadDelay t
                                  atomically (writeTVar v Terminate)
                      Nothing -> async (pure ()))
                 [ x | x@(_, (_, ExitClean)) <- Map.toList schedule ]
      let tracer :: forall a. (Show a, Typeable a) => Tracer (IOSim s) a
          tracer = dynamicTracer <> sayTracer
      result <- runTxSubmission tracer tracer tracer tracer combinedState peerImpairment
                                cancelSchedule txDecisionPolicy
      traverse_ cancel disconnectAids
      pure result


-- | Resilience to peer disconnects: with a non-empty proper subset of peers
-- exiting early (cleanly via 'MsgDone' or abruptly via cancellation),
-- the surviving peers' valid txs still reach the inbound mempool, no
-- adversarial txid leaks in, counters stay consistent, and the
-- 'SharedTxState' is fully reclaimed at end of simulation.
--
-- Exercises the 'withPeer' bracket finalizer scrub on mid-protocol
-- exit, including 'pifSubmitting'-non-empty paths reached when a peer
-- is cancelled while bodies are in submission.
prop_txSubmission_peerDisconnect :: TxSubmissionDisconnectState -> Property
prop_txSubmission_peerDisconnect cs@(TxSubmissionDisconnectState st schedule) =
    let allAddrs    = Map.keysSet (peerMap st)
        disconnected = Map.keysSet schedule
        survivors    = allAddrs `Set.difference` disconnected
        survivorTxs = [ txs | addr <- Set.toList survivors
                            , let (txs, _, _) = peerMap st Map.! addr ]
        allOutIds   = Set.fromList
                    $ concatMap (\(txs, _, _) -> getTxId <$> txs)
                    $ Map.elems (peerMap st)
        tr          = runSimTrace (txSubmissionSimulationDisconnect cs)
        -- Earliest inbound trace time per peer. A 'cancel-active'
        -- classification means the inbound emitted at least one event
        -- before the scheduled cancel, so the bracket finalizer
        -- exercised the scrub-with-non-empty-state path; 'cancel-idle'
        -- means the cancel hit a server that had not progressed past
        -- 'serverIdle'.
        inboundFirstActivity :: Map Int Time
        inboundFirstActivity =
          Map.fromListWith min
            [ (addr, t)
            | (t, PeerInboundTrace addr _)
                <- selectTraceEventsDynamicWithTime tr
                    :: [(Time, PeerInboundTraceType)]
            ]
        cancelClassifications :: [String]
        cancelClassifications =
          [ case Map.lookup addr inboundFirstActivity of
              Just firstActivity
                | firstActivity < Time t -> "cancel-active"
              _                          -> "cancel-idle"
          | (addr, (t, ExitCancel)) <- Map.toList schedule
          ]
    in label ("disconnected peers: " ++ show (Map.size schedule))
     $ label ("surviving peers: " ++ show (Set.size survivors))
     $ tabulate "exit method" (show . snd <$> Map.elems schedule)
     $ tabulate "cancel timing" cancelClassifications
     $ case traceResult True tr of
         Left e -> counterexample (show e)
                 . counterexample (ppSayTrace tr)
                 $ False
         Right (inmp, _, finalState) ->
             counterexample (ppSayTrace tr)
           $ conjoin (validateSurvivor inmp `map` survivorTxs)
             .&&. noContamination allOutIds inmp
             .&&. prop_counterInvariants tr
             .&&. prop_sharedStateClean finalState
  where
    validateSurvivor :: [Tx Int] -> [Tx Int] -> Property
    validateSurvivor inmp outmp =
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

    noContamination :: Set.Set Int -> [Tx Int] -> Property
    noContamination allOutIds inmp =
      let inIds  = Set.fromList (getTxId <$> inmp)
          extras = inIds `Set.difference` allOutIds in
        counterexample ("contaminating txids: " ++ show (Set.toList extras))
      $ property (Set.null extras)

prop_TxSubmissionDisconnectState_validGen :: TxSubmissionDisconnectState -> Property
prop_TxSubmissionDisconnectState_validGen st =
    counterexample (show st) $ validTxSubmissionDisconnectState st

prop_TxSubmissionDisconnectState_shrinkValid :: TxSubmissionDisconnectState -> Property
prop_TxSubmissionDisconnectState_shrinkValid st = conjoin
  [ counterexample (show s) (validTxSubmissionDisconnectState s) | s <- shrink st ]

prop_TxSubmissionDisconnectState_shrinkSmaller :: TxSubmissionDisconnectState -> Property
prop_TxSubmissionDisconnectState_shrinkSmaller st = conjoin
  [ counterexample ("shrink emitted self: " ++ show s) (s /= st) | s <- shrink st ]

prop_TxSubmissionDisconnectState_shrinkNoDups :: TxSubmissionDisconnectState -> Property
prop_TxSubmissionDisconnectState_shrinkNoDups st =
  let shrunk = shrink st in
      counterexample ("duplicates: " ++ show (shrunk List.\\ List.nub shrunk))
    $ length (List.nub shrunk) === length shrunk


--
-- Score-related unit tests (decrement-on-accept tuning of peerScore).
--

-- | Build a 'Tx' with the given id, size, and validity.  Used by the
-- score tests to construct deterministic per-peer tx lists that
-- include known-valid and known-invalid bodies (the test mempool
-- rejects @getTxValid = False@ via 'getMempoolWriter').
mkTx :: TxId -> Bool -> Tx TxId
mkTx i valid = Tx { getTxId      = i
                  , getTxSize    = 100
                  , getTxAdvSize = 100
                  , getTxValid   = valid
                  , getTxParent  = Nothing
                  }

-- | Pick out the per-peer @ptxcScore@ values from the simulation
-- trace, keyed by the peer the inbound side attributed the event to.
-- The aggregator @combine@ is invoked as @combine new old@ (matching
-- 'Map.insertWith' semantics) and controls reduction across multiple
-- events for the same peer.
peerScoresBy :: (Double -> Double -> Double)
             -> SimTrace a
             -> Map PeerAddr Double
peerScoresBy combine tr =
    Map.fromListWith combine
      [ (peer, ptxcScore ptxc)
      | PeerInboundTrace peer (TraceTxSubmissionProcessed ptxc) <- events
      ]
  where
    events :: [PeerInboundTraceType]
    events = Trace.toList $ traceSelectTraceEventsDynamic tr

peerPeakScore, peerFinalScore :: SimTrace a -> Map PeerAddr Double
peerPeakScore  = peerScoresBy max
peerFinalScore = peerScoresBy const


-- | A peer that delivers only invalid bodies accumulates a non-zero
-- score from the mempool-reject penalty path. Sole advertiser, so no
-- multiplicity races: the score gain comes purely from the rejection
-- count attributed to this peer. With no accepts to offset, the score
-- cannot drain back to zero before the run ends.
unit_score_persistentBadStaysHigh :: Assertion
unit_score_persistentBadStaysHigh = do
    let st = TxSubmissionState
              { peerMap        = Map.singleton 1
                                   ([ mkTx i False | i <- [0..9] ], Nothing, Nothing)
              , peerImpairment = Map.empty
              , decisionPolicy = defaultTxDecisionPolicy
              }
        tr          = runSimTrace (void $ txSubmissionSimulation st)
        peakScores  = peerPeakScore tr
        finalScores = peerFinalScore tr
        peakPeer1   = Map.findWithDefault 0 1 peakScores
        finalPeer1  = Map.findWithDefault 0 1 finalScores
        ctx :: String
        ctx = "peak score:  " ++ show peakScores
           ++ "\nfinal score: " ++ show finalScores
    assertBool (ctx ++ "\npeer1 must accumulate score from mempool rejects")
               (peakPeer1 > 0)
    assertBool (ctx ++ "\npeer1 score must stay within scoreMax")
               (peakPeer1 <= scoreMax defaultTxDecisionPolicy)
    assertBool (ctx ++ "\npeer1 has no accepts to offset the rejections, score must stay above zero")
               (finalPeer1 > 0)

-- | A peer that delivers a burst of invalid bodies and then valid
-- bodies recovers via decrement-on-accept: the score climbs during
-- the invalid phase, then drops back to zero as the accepts cancel
-- the rejection accumulation (with default 'scoreAcceptDecrement' of
-- 1 and equal-or-greater valid count).
unit_score_recoversAfterBurst :: Assertion
unit_score_recoversAfterBurst = do
    let invalids = 5
        valids   = 10
        st = TxSubmissionState
              { peerMap        = Map.singleton 1
                                   ( [ mkTx i False | i <- [0..invalids - 1] ]
                                  ++ [ mkTx i True  | i <- [invalids .. invalids + valids - 1] ]
                                   , Nothing, Nothing)
              , peerImpairment = Map.empty
              , decisionPolicy = defaultTxDecisionPolicy
              }
        tr          = runSimTrace (void $ txSubmissionSimulation st)
        peakScores  = peerPeakScore tr
        finalScores = peerFinalScore tr
        peakPeer1   = Map.findWithDefault 0 1 peakScores
        finalPeer1  = Map.findWithDefault 0 1 finalScores
        ctx :: String
        ctx = "peak score:  " ++ show peakScores
           ++ "\nfinal score: " ++ show finalScores
    assertBool (ctx ++ "\npeer1 must briefly accumulate score during the invalid burst")
               (peakPeer1 > 0)
    assertEqual (ctx ++ "\npeer1 must end at zero once valid accepts have drained the rejections")
                0
                finalPeer1

-- | A well-behaved peer that is the sole advertiser of every tx it
-- carries never sees a late or rejected event, so its score stays at
-- zero throughout the run.  Documents that decrement-on-accept does
-- not push the score below the floor when there is nothing to drain.
unit_score_wellBehavedStaysAtZero :: Assertion
unit_score_wellBehavedStaysAtZero = do
    let st = TxSubmissionState
              { peerMap        = Map.singleton 1
                                   ([ mkTx i True | i <- [0..9] ], Nothing, Nothing)
              , peerImpairment = Map.empty
              , decisionPolicy = defaultTxDecisionPolicy
              }
        tr         = runSimTrace (void $ txSubmissionSimulation st)
        peakScores = peerPeakScore tr
        peakPeer1  = Map.findWithDefault 0 1 peakScores
    assertEqual ("peak score: " ++ show peakScores
                  ++ "\npeer1 must never accumulate any score")
                0
                peakPeer1


prop_sharedTxStateInvariant :: TxSubmissionState -> Property
prop_sharedTxStateInvariant initialState@(TxSubmissionState st0 _ _) =
  let tr = runSimTrace (void $ txSubmissionSimulation initialState)
      pTrace = ppSayTrace tr
  in case traceResult True tr of
    Left err -> counterexample pTrace
              . counterexample (show err)
              $ False
    Right _ ->
      let tracedStates :: [TxStateTraceType]
          tracedStates = Trace.toList $ traceSelectTraceEventsDynamic tr

          -- Fold over the trace, tracking the previous state so we can
          -- assert 'sharedGenerationBumpInvariant' between each pair of
          -- consecutive writes captured by 'traceTVarIO' on the shared
          -- state TVar.
          step
            :: (Maybe (SharedTxState PeerAddr TxId), (Every, Sum Int))
            -> TxStateTraceType
            -> (Maybe (SharedTxState PeerAddr TxId), (Every, Sum Int))
          step (mPrev, acc) (TxStateTrace st) =
            let stateInv = Every (counterexample (show st)
                                    (sharedTxStateInvariant st))
                bumpInv  = case mPrev of
                             Nothing   -> mempty
                             Just prev ->
                               Every (sharedGenerationBumpInvariant prev st) in
            (Just st, acc <> (stateInv <> bumpInv, Sum 1))

          (_, (p, Sum c)) =
            List.foldl' step (Nothing, mempty) tracedStates in
      counterexample pTrace
       $ label ("number of txs: "
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

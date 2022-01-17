{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Ouroboros.Network.BlockFetch (tests) where

import           Test.ChainGenerators (TestChainFork (..))
import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck (testProperty)

import           Data.List
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (mapMaybe)
import           Data.Proxy (Proxy (..))
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Typeable (Typeable)

import           Control.Exception (AssertionFailed (..), throw)
import           Control.Monad (unless)
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTimer
import           Control.Monad.IOSim
import           Control.Tracer (Tracer (Tracer), contramap, nullTracer)

--TODO: could re-export some of the trace types from more convenient places:
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AnchoredFragment
import           Ouroboros.Network.Block
import           Ouroboros.Network.BlockFetch
import           Ouroboros.Network.BlockFetch.ClientRegistry
import           Ouroboros.Network.BlockFetch.ClientState
import           Ouroboros.Network.BlockFetch.Examples
import           Ouroboros.Network.Driver (TraceSendRecv)
import qualified Ouroboros.Network.MockChain.Chain as Chain
import           Ouroboros.Network.Mux (ControlMessage (..), continueForever)
import           Ouroboros.Network.Protocol.BlockFetch.Type (BlockFetch)
import           Ouroboros.Network.Testing.ConcreteBlock

import           Ouroboros.Network.Testing.Utils


--
-- The list of all tests
--

tests :: TestTree
tests = testGroup "BlockFetch"
  [ testProperty "static chains without overlap"
                 prop_blockFetchStaticNoOverlap

  , testProperty "static chains with overlap"
                 prop_blockFetchStaticWithOverlap

  --, testCaseSteps "bracketSyncWithFetchClient"
  --                unit_bracketSyncWithFetchClient

  --TODO: test where for any given delta-Q, check that we do achieve full
  -- pipelining to keep the server busy and get decent enough batching of
  -- requests (testing the high/low watermark mechanism).
  , testProperty "termination"
                 prop_terminate
  ]


--
-- Properties
--

-- | In this test we have two candidates chains that are static throughout the
-- run. The two chains share some common prefix (genesis in the degenerate
-- case). The test runs the block fetch logic to download all of both chain
-- candidates.
--
-- In this variant we set up the common prefix of the two candidates as the
-- \"current\" chain. This means the block fetch only has to download the
-- suffixes of the two candidates. This also means that the two suffixes are
-- guaranteed not to have any overlap in their blocks. We rely on this
-- guarantee in this special case to check stronger properties.
--
-- This runs the block fetch and then checks that the trace of the events in
-- that run satisfy the trace properties:
--
-- * 'tracePropertyBlocksRequestedAndRecievedPerPeer'
-- * 'tracePropertyClientStateSanity'
-- * 'tracePropertyInFlight'
--
prop_blockFetchStaticNoOverlap :: TestChainFork -> Property
prop_blockFetchStaticNoOverlap (TestChainFork common fork1 fork2) =
    let trace = selectTraceEventsDynamic (runSimTrace simulation)

     in counterexample ("\nTrace:\n" ++ unlines (map show trace)) $

        -- For fetch reqs added and received, we observe exactly the sequence
        -- of blocks we expect, which is the whole fork suffix.
        tracePropertyBlocksRequestedAndRecievedPerPeer fork1'' fork2'' trace

        -- state sanity check
   .&&. property (tracePropertyClientStateSanity trace)

        -- check in-flight requests
   .&&. tracePropertyInFlight trace

  where
    simulation :: IOSim s ()
    simulation =
      blockFetchExample1
        (contramap TraceFetchDecision       dynamicTracer)
        (contramap TraceFetchClientState    dynamicTracer)
        (contramap TraceFetchClientSendRecv dynamicTracer)
        Nothing Nothing
        (continueForever (Proxy :: Proxy (IOSim s)))
        common' forks

    -- TODO: consider making a specific generator for anchored fragment forks
    common' = chainToAnchoredFragment common
    fork1'  = chainToAnchoredFragment fork1
    fork2'  = chainToAnchoredFragment fork2
    forks   = [fork1', fork2']
    -- And just the extensions
    Just (_, _, fork1'', fork2'') = AnchoredFragment.intersect fork1' fork2'


-- | In this test we have two candidates chains that are static throughout the
-- run. The two chains share some common prefix (genesis in the degenerate
-- case). The test runs the block fetch logic to download all of both chain
-- candidates.
--
-- In this variant we set up the \"current\" chain as empty (genesis). This
-- means the block has to download the whole of both candidates. This also
-- means that we typically expect there to be overlap in the blocks in the two
-- chains. We rely on there typically being overlap to check properties of that
-- overlap.
--
-- This runs the block fetch and then checks that the trace of the events in
-- that run satisfy the trace properties:
--
-- * 'tracePropertyBlocksRequestedAndRecievedAllPeers'
-- * 'tracePropertyNoDuplicateBlocksBetweenPeers'
-- * 'tracePropertyClientStateSanity'
-- * 'tracePropertyInFlight'
--
-- TODO: 'prop_blockFetchStaticWithOverlap' fails if we introduce delays. issue #2622
--
prop_blockFetchStaticWithOverlap :: TestChainFork -> Property
prop_blockFetchStaticWithOverlap (TestChainFork _common fork1 fork2) =
    let trace = selectTraceEventsDynamic (runSimTrace simulation)

     in counterexample ("\nTrace:\n" ++ unlines (map show trace)) $

        -- For fetch reqs added and received, between the two peers we observe
        -- the set of blocks we expect, which is the union of the two chains.
        tracePropertyBlocksRequestedAndRecievedAllPeers fork1' fork2' trace

        -- For fetch reqs added, the set of blocks added for the two peers
        -- should not intersect
   .&&. tracePropertyNoDuplicateBlocksBetweenPeers fork1' fork2' trace

        -- state sanity check
   .&&. property (tracePropertyClientStateSanity trace)

        -- check in-flight requests
   .&&. tracePropertyInFlight trace

  where
    simulation :: forall s. IOSim s ()
    simulation =
      blockFetchExample1
        (contramap TraceFetchDecision       dynamicTracer)
        (contramap TraceFetchClientState    dynamicTracer)
        (contramap TraceFetchClientSendRecv dynamicTracer)
        Nothing Nothing
        (continueForever (Proxy :: Proxy (IOSim s)))
        (AnchoredFragment.Empty AnchoredFragment.AnchorGenesis)
        forks

    -- TODO: consider making a specific generator for anchored fragment forks
    fork1'  = chainToAnchoredFragment fork1
    fork2'  = chainToAnchoredFragment fork2
    forks   = [fork1', fork2']

chainToAnchoredFragment :: Chain.Chain Block -> AnchoredFragment Block
chainToAnchoredFragment =
    AnchoredFragment.fromNewestFirst AnchoredFragment.AnchorGenesis
  . Chain.chainToList

-- TODO: move elsewhere and generalise
chainPoints :: AnchoredFragment Block -> [Point BlockHeader]
chainPoints = map (castPoint . blockPoint)
            . AnchoredFragment.toOldestFirst

data Example1TraceEvent =
     TraceFetchDecision       [TraceLabelPeer Int
                                (FetchDecision [Point BlockHeader])]
   | TraceFetchClientState    (TraceLabelPeer Int
                                (TraceFetchClientState BlockHeader))
   | TraceFetchClientSendRecv (TraceLabelPeer Int
                                (TraceSendRecv (BlockFetch Block (Point Block))))

instance Show Example1TraceEvent where
  show (TraceFetchDecision       x) = "TraceFetchDecision " ++ show x
  show (TraceFetchClientState    x) = show x
  show (TraceFetchClientSendRecv x) = show x


-- | Check the execution trace for a particular property: we observe all the
-- blocks in the 'FetchRequest's added by the decision logic and the blocks
-- received by the fetch clients; check that the ordered sequence of blocks
-- requested and completed by both fetch clients is exactly the sequence
-- expected. The expected sequence is exactly the chain suffixes in order.
--
-- This property is stronger than 'tracePropertyBlocksRequestedAndRecievedAllPeers'
-- since it works with sequences rather than sets and for each chain
-- individually rather than both chains together. It only holds for the
-- situation where the suffixes of the chains that need to be fetched are
-- disjoint, sharing no common prefix.
--
-- It turns out that no duplicates part is not trivial. Earlier versions of the
-- block fetch logic did not satisfy this in all cases.
--
tracePropertyBlocksRequestedAndRecievedPerPeer
  :: AnchoredFragment Block
  -> AnchoredFragment Block
  -> [Example1TraceEvent]
  -> Property
tracePropertyBlocksRequestedAndRecievedPerPeer fork1 fork2 es =
      requestedFetchPoints === requiredFetchPoints
 .&&. receivedFetchPoints  === requiredFetchPoints
  where
    requiredFetchPoints =
      Map.filter (not . Prelude.null) $
      Map.fromList $
        [ (1, chainPoints fork1)
        , (2, chainPoints fork2)
        ]

    requestedFetchPoints :: Map Int [Point BlockHeader]
    requestedFetchPoints =
      Map.fromListWith (flip (++))
        [ (peer, map blockPoint (AnchoredFragment.toOldestFirst fragment))
        | TraceFetchClientState
            (TraceLabelPeer peer
              (AddedFetchRequest
                (FetchRequest fragments) _ _ _)) <- es
        , fragment <- fragments
        ]

    receivedFetchPoints :: Map Int [Point BlockHeader]
    receivedFetchPoints =
      Map.fromListWith (flip (++))
        [ (peer, [pt])
        | TraceFetchClientState
            (TraceLabelPeer peer (CompletedBlockFetch pt _ _ _ _ _)) <- es
        ]


-- | Check the execution trace for a particular property: we observe all the
-- blocks in the 'FetchRequest's added by the decision logic and the blocks
-- received by the fetch clients; check that the set of all blocks requested
-- across the two peers is the set of blocks we expect, and similarly for the
-- set of all blocks received. The expected set of blocks is the union of the
-- blocks on the two candidate chains.
--
-- This property is weaker than 'tracePropertyBlocksRequestedAndRecievedPerPeer'
-- since it does not involve order or frequency, but it holds for the general
-- case of multiple chains with common prefixes.
--
tracePropertyBlocksRequestedAndRecievedAllPeers
  :: AnchoredFragment Block
  -> AnchoredFragment Block
  -> [Example1TraceEvent]
  -> Property
tracePropertyBlocksRequestedAndRecievedAllPeers fork1 fork2 es =
      requestedFetchPoints === requiredFetchPoints
 .&&. receivedFetchPoints  === requiredFetchPoints
  where
    requiredFetchPoints =
      Set.fromList (chainPoints fork1 ++ chainPoints fork2)

    requestedFetchPoints :: Set (Point BlockHeader)
    requestedFetchPoints =
      Set.fromList
        [ blockPoint block
        | TraceFetchClientState
            (TraceLabelPeer _
              (AddedFetchRequest
                (FetchRequest fragments) _ _ _)) <- es
        , fragment <- fragments
        , block    <- AnchoredFragment.toOldestFirst fragment
        ]

    receivedFetchPoints :: Set (Point BlockHeader)
    receivedFetchPoints =
      Set.fromList
        [ pt
        | TraceFetchClientState
            (TraceLabelPeer _ (CompletedBlockFetch pt _ _ _ _ _)) <- es
        ]


-- | Check the execution trace for a particular property: we observe all the
-- blocks in the 'FetchRequest's added by the decision logic; check that the
-- set blocks requested for one peer and for the other do not intersect.
--
-- This is a non-trivial property because in the general case, the chain
-- suffixes do intersect by sharing a common prefix. This property therefore
-- demonstrates that in the 'FetchModeBulkSync' the decision logic is properly
-- allocating fetch requests to different peers to avoid asking for duplicate
-- blocks from different peers.
--
tracePropertyNoDuplicateBlocksBetweenPeers
  :: AnchoredFragment Block
  -> AnchoredFragment Block
  -> [Example1TraceEvent]
  -> Property
tracePropertyNoDuplicateBlocksBetweenPeers fork1 fork2 es =
    classify (hasDupes requiredFetchPoints)
             "Overlapping chains between peers" $

    Set.null $

    Map.findWithDefault Set.empty 1 requestedFetchPoints
      `Set.intersection`
    Map.findWithDefault Set.empty 2 requestedFetchPoints

  where
    hasDupes = not . null . filter ((>1) . length)  . group . sort

    requiredFetchPoints =
      nub (chainPoints fork1 ++ chainPoints fork2)

    requestedFetchPoints :: Map Int (Set (Point BlockHeader))
    requestedFetchPoints =
      Map.fromListWith Set.union
        [ (peer, points fragment)
        | TraceFetchClientState
            (TraceLabelPeer peer
              (AddedFetchRequest
                (FetchRequest fragments) _ _ _)) <- es
        , fragment <- fragments
        , let points = Set.fromList . map blockPoint
                     . AnchoredFragment.toOldestFirst
        ]


-- | This is just a sanity check on the in-flight statistics maintained between
-- the decision logic thread and the block fetch client threads.
--
tracePropertyClientStateSanity :: [Example1TraceEvent] -> Bool
tracePropertyClientStateSanity es =
    and [ saneStateValues inflight status
        | TraceFetchClientState
            (TraceLabelPeer _
              (AddedFetchRequest _ inflight _ status)) <- es
        ]
  where
    saneStateValues PeerFetchInFlight {..} status =
        -- Here we know the fixed dummy block size so we know exactly what
        -- the bytes in flight should be.
           Set.size peerFetchBlocksInFlight * 2000
        == fromIntegral peerFetchBytesInFlight

     && case status of
          PeerFetchStatusReady{} -> True
          PeerFetchStatusBusy    -> True
          _                      -> False -- not used in this test

     && if peerFetchReqsInFlight == 0
           then Set.null peerFetchBlocksInFlight
           else True


-- TODO: the idea of this property was that we check that we're not making too
-- many decisions and waking up unnecessarily. But it's not trivial since
-- towards the end of a download we can have multiple occurences of decisions
-- where it's just the trailing data that's in-flight. Need to think about
-- whether there's any concise and robust way of expressing this.
--
-- tracePropertyDecisions _fork1 _fork2 _es = True

data FetchRequestTrace
    = AddedFetchRequestTrace
        (FetchRequest BlockHeader)
        (PeerFetchInFlight BlockHeader)
    | AcknowledgedFetchRequestTrace
    | CompletedFetchBatchTrace
    | RejectedFetchBatchTrace
  deriving Show

fetchRequestTrace :: [Example1TraceEvent] -> [TraceLabelPeer Int FetchRequestTrace]
fetchRequestTrace = mapMaybe f
  where
    f (TraceFetchClientState (TraceLabelPeer peerid (AddedFetchRequest request inflight _ _))) =
      Just (TraceLabelPeer peerid (AddedFetchRequestTrace request inflight))
    f (TraceFetchClientState (TraceLabelPeer peerid (AcknowledgedFetchRequest{}))) =
      Just (TraceLabelPeer peerid AcknowledgedFetchRequestTrace)
    f (TraceFetchClientState (TraceLabelPeer peerid CompletedFetchBatch{}))
      = Just (TraceLabelPeer peerid CompletedFetchBatchTrace)
    f (TraceFetchClientState (TraceLabelPeer peerid RejectedFetchBatch{}))
      = Just (TraceLabelPeer peerid RejectedFetchBatchTrace)
    f _ = Nothing


-- | This property verifies that the number of in-flight requests is computed
-- according to the following algorithm:
--
-- * when adding requests to 'fetchClientRequestVar' using semigroup instance of
--   'FetchRequest' to calculate the number of requests to add to the number of
--   requests in flight
-- * when finishing receiving a batch, subtract one from the number of requests
--   in flight.
--
-- This tests reconstructs the value of 'fetchClientRequestVar' and
-- 'peerFetchReqsInFlight' from the trace and compares the expected value with
-- the actual value logged in the trace.
--
-- This property also assures that when the client terminates, there are no
-- outstanding in-flight requests.
--
-- Note: the implementation calls in-flight requests the requests that are
-- ordered to be sent (and they may not be sent immediately).  This test tracks
-- requests added to 'fetchClientRequestVar' and the number or requests that
-- were sent (acknowledged) by the client.  The sum of these two values gives
-- in-flight requests.
--
tracePropertyInFlight :: [Example1TraceEvent] -> Property
tracePropertyInFlight =
      foldr (\tr c -> checkTrace Nothing 0 tr .&&. c) (property True)
    . Map.fromListWith (flip (++))
    . map (\(TraceLabelPeer peerid a) -> (peerid, [a]))
    . fetchRequestTrace
  where
    checkTrace :: Maybe (FetchRequest BlockHeader)
               --  not yet acknowledged 'FetchRequest', but ones that already
               --  added to 'fetchClientRequestVar';  This value simulates the
               --  content of 'fetchClientRequestVar'
               -> Int
               -- number of requests that were already sent (acknowledged);
               -> [FetchRequestTrace]
               -> Property

    -- 'AddedFetchRequest' when there 'fetchClientRequestVar' is empty
    checkTrace Nothing reqsInFlight ((AddedFetchRequestTrace r PeerFetchInFlight {peerFetchReqsInFlight}) : tr)
      | reqsInFlight + length (fetchRequestFragments r) == fromIntegral peerFetchReqsInFlight
      = checkTrace (Just r) reqsInFlight tr
      | otherwise
      = counterexample ("tracePropertyInFlight: "
                       ++ show reqsInFlight
                       ++ " + "
                       ++ show (length (fetchRequestFragments r))
                       ++ " /= "
                       ++ show peerFetchReqsInFlight
                      ) False

    -- 'AddedFetchRequest' when there are 'fetchClientRequestVar' is non-empty
    -- in this case we use 'FetchRequest' Semigroup instance to combine new and
    -- old requests.
    checkTrace (Just r0) reqsInFlight ((AddedFetchRequestTrace r1 PeerFetchInFlight {peerFetchReqsInFlight}) : tr)
      | reqsInFlight + length (fetchRequestFragments (r0 <> r1)) == fromIntegral peerFetchReqsInFlight
      = checkTrace (Just (r0 <> r1)) reqsInFlight tr
      | otherwise
      = counterexample ("tracePropertyInFlight: "
                       ++ show reqsInFlight
                       ++ " + "
                       ++ show (length (fetchRequestFragments (r0 <> r1)))
                       ++ " /= "
                       ++ show peerFetchReqsInFlight
                       ) False

    -- acknowledged fetch requests: update 'reqsInFlight' and continue
    -- traversing the trace
    checkTrace (Just r) reqsInFlight (AcknowledgedFetchRequestTrace : tr)
      = checkTrace Nothing (reqsInFlight + length (fetchRequestFragments r)) tr
    checkTrace Nothing reqsInFlight (AcknowledgedFetchRequestTrace : tr)
      = checkTrace Nothing reqsInFlight tr

    -- batch completed, we subtract `1` from requests in flight
    checkTrace mr reqsInFlight (CompletedFetchBatchTrace : tr)
      = checkTrace mr (reqsInFlight - 1) tr
    checkTrace mr reqsInFlight (RejectedFetchBatchTrace : tr)
      = checkTrace mr (reqsInFlight - 1) tr

    -- check that by the end of the trace there are no requests in flight
    checkTrace (Just _) _ []
      = counterexample
          "tracePropertyInFlight: fetch requests in flight"
          False
    checkTrace Nothing reqsInFlight []
      | reqsInFlight > 0
      = counterexample
          ("traceProeprtyInFlight: reqsInFlight = " ++ show reqsInFlight ++ " ≠ 0")
          False
      | otherwise
      = property True


--
-- Unit tests
--

_unit_bracketSyncWithFetchClient :: (String -> IO ()) -> Assertion
_unit_bracketSyncWithFetchClient step = do

    step "Starting fetch before sync"
    checkResult =<< testSkeleton
      (\action -> threadDelay 0.1 >> action (threadDelay 0.2))
      (\action -> threadDelay 0.2 >> action (threadDelay 0.2))

    step "Starting sync before fetch"
    checkResult =<< testSkeleton
      (\action -> threadDelay 0.2 >> action (threadDelay 0.2))
      (\action -> threadDelay 0.1 >> action (threadDelay 0.2))

    step "Stopping fetch before sync"
    checkResult =<< testSkeleton
      (\action -> action (threadDelay 0.1))
      (\action -> action (threadDelay 0.2))

    step "Stopping sync before fetch"
    checkResult =<< testSkeleton
      (\action -> action (threadDelay 0.2))
      (\action -> action (threadDelay 0.1))

    step "Exception in fetch"
    Left (Left _) <- testSkeleton
      (\action -> action (threadDelay 0.1 >> throwIO AsyncCancelled))
      (\action -> action (threadDelay 0.2))

    step "Exception in sync"
    Right (Left _) <- testSkeleton
      (\action -> action (threadDelay 0.2))
      (\action -> action (threadDelay 0.1 >> throwIO AsyncCancelled))

    return ()

  where
    checkResult (Left  (Right _)) = return ()
    checkResult (Right (Right _)) = return ()
    checkResult _                 = assertFailure "unexpected result"

    testSkeleton :: forall m a b.
                    (MonadAsync m, MonadFork m, MonadMask m, MonadSTM m,
                     MonadTimer m, MonadThrow m, MonadThrow (STM m))
                 => ((forall c. m c -> m c) -> m a)
                 -> ((forall c. m c -> m c) -> m b)
                 -> m (Either (Either SomeException a)
                              (Either SomeException b))
    testSkeleton withFetchTestAction withSyncTestAction = do
      registry <- newFetchClientRegistry
      setFetchClientContext registry nullTracer (error "no policy")

      fetchStatePeerChainsVar <- newTVarIO Map.empty

      let peer  = "thepeer"
          fetch :: m a
          fetch = withFetchTestAction $ \body ->
                    bracketFetchClient registry peer $ \_ ->
                      body

          sync :: m b
          sync  = withSyncTestAction $ \body ->
                    bracketSyncWithFetchClient registry peer $
                      bracket_
                        (atomically (modifyTVar fetchStatePeerChainsVar
                                                (Map.insert peer ())))
                        (atomically (modifyTVar fetchStatePeerChainsVar
                                                (Map.delete peer)))
                        body

          logic :: (Map String (PeerFetchStatus BlockHeader), Map String ())
                -> m ()
          logic fingerprint = do
            fingerprint' <- atomically $ do
              fetchStatePeerStates <- readFetchClientsStatus registry
              fetchStatePeerChains <- readTVar fetchStatePeerChainsVar
              let fingerprint' = (fetchStatePeerStates, fetchStatePeerChains)
              check (fingerprint' /= fingerprint)
              return fingerprint'

            let (fetchStatePeerStates, fetchStatePeerChains) = fingerprint'
            unless (                 Map.keysSet fetchStatePeerChains
                    `Set.isSubsetOf` Map.keysSet fetchStatePeerStates) $
              throwIO (AssertionFailed "detected state mismatch")

            logic fingerprint'

      withAsync     fetch $ \fetchAsync ->
        withAsync   sync  $ \syncAsync  ->
          withAsync (logic (Map.empty, Map.empty)) $ \logicAsync -> do
            res <- atomically $ do
              res <- pollSTM logicAsync
              case res of
                Nothing         -> waitEitherCatchSTM fetchAsync syncAsync
                Just (Left  e)  -> throwIO e
                Just (Right ()) -> error "impossible"

            threadDelay 0.1
            -- give the logic thread a chance to detect any final problems
            atomically $ do
              x <- pollSTM logicAsync
              case x of
                Just (Left e) -> throwIO e
                _             -> return ()
            return res

-- | Check that the client can terminate using `ControlMessage` mechanism.
--
-- The 'awaitDelay' of @100 * delay@ is a bit arbitrary.  It would be nicer to
-- make a proper calucation what should it be.  At the moment this test shows
-- that the block fetch protocol can exit within some large time limit.
--
prop_terminate :: TestChainFork -> Positive SmallDelay -> Property
prop_terminate (TestChainFork _commonChain forkChain _forkChain) (Positive (SmallDelay delay)) =
    let tr = runSimTrace simulation
        trace :: [FetchRequestTrace]
        trace  = selectTraceEventsDynamic tr
    in counterexample
        ("Trace: \n" ++ unlines (map show trace))
        (case traceResult True tr of
           Left e  -> throw e
           Right x -> counterexample "block-fetch was unstoppable" x)
  where
    simulation :: forall s. IOSim s Bool
    simulation = do
      controlMessageVar <- newTVarIO Continue
      result <-
        race
          (do
            threadId <- myThreadId
            labelThread threadId "control-message"
            let terminateDelay =
                  realToFrac (Chain.length forkChain) * delay / 2
            threadDelay terminateDelay
            atomically (writeTVar controlMessageVar Terminate)
            let awaitDelay = delay * 100
            threadDelay awaitDelay)
          (do
            threadId <- myThreadId
            labelThread threadId "block-fetch"
            blockFetchExample0
              (contramap TraceFetchDecision       dynamicTracer)
              (contramap TraceFetchClientState    dynamicTracer)
              (contramap TraceFetchClientSendRecv dynamicTracer)
              (Just delay) (Just delay)
              (readTVar controlMessageVar)
              (AnchoredFragment.Empty AnchoredFragment.AnchorGenesis)
              fork')
      -- `IOSim` on `Windows` is using `defaultRegisterTimeout`.  It does not
      -- cancel forked threads.   The timeout which leaves running thread comes
      -- from 'runPipelinedPeerWithLimits'.
      -- threadDelay 60
      return $ case result of
        Left _  -> False
        Right _ -> True

    fork'  = chainToAnchoredFragment forkChain



--
-- Trace utils
--

dynamicTracer :: Typeable a => Tracer (IOSim s) a
dynamicTracer = Tracer traceM

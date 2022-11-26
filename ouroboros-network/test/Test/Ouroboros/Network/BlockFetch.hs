{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

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

import           Control.Concurrent.Class.MonadSTM.Strict
import           Control.Exception (AssertionFailed (..), throw)
import           Control.Monad (unless)
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime (Time (..))
import           Control.Monad.Class.MonadTimer
import           Control.Monad.IOSim
import           Control.Tracer (Tracer (Tracer), contramap, nullTracer)

import           Ouroboros.Network.DeltaQ
--TODO: could re-export some of the trace types from more convenient places:
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AnchoredFragment
import           Ouroboros.Network.Block
import           Ouroboros.Network.BlockFetch
import           Ouroboros.Network.BlockFetch.ClientRegistry
import           Ouroboros.Network.BlockFetch.ClientState
import           Ouroboros.Network.BlockFetch.DeltaQ
import           Ouroboros.Network.BlockFetch.Examples
import           Ouroboros.Network.Driver (TraceSendRecv)
<<<<<<< Updated upstream
import qualified Ouroboros.Network.MockChain.Chain as Chain
import           Ouroboros.Network.Mux (ControlMessage (..), continueForever)
import           Ouroboros.Network.NodeToNode (isPipeliningEnabled)
||||||| Stash base
import qualified Ouroboros.Network.Mock.Chain as Chain
import           Ouroboros.Network.Mock.ConcreteBlock
import           Ouroboros.Network.NodeToNode.Version (isPipeliningEnabled)
=======
import           Ouroboros.Network.Mock.Chain (Chain (..))
import qualified Ouroboros.Network.Mock.Chain as Chain
import           Ouroboros.Network.Mock.ConcreteBlock
import           Ouroboros.Network.NodeToNode.Version (isPipeliningEnabled)
>>>>>>> Stashed changes
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
  , testProperty "compare comparePeerGSV" prop_comparePeerGSV
  , testProperty "eq comparePeerGSV" prop_comparePeerGSVEq
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
                    bracketFetchClient registry maxBound isPipeliningEnabled peer $ \_ ->
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
-- make a proper calculation what should it be.  At the moment this test shows
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
            let awaitDelay = delay * 100.1
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

unit_f :: Property
unit_f = prop_terminate
      (TestChainFork
          (Genesis
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-6427038944772666313), headerPrevHash = GenesisHash, headerSlot = SlotNo 1, headerBlockNo = BlockNo 0, headerBodyHash = BodyHash 4789151561354856977}, blockBody = BlockBody "EMPTY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 4328474877338882864, headerPrevHash = BlockHash (HeaderHash (-6427038944772666313)), headerSlot = SlotNo 2, headerBlockNo = BlockNo 1, headerBodyHash = BodyHash (-5068394865740066918)}, blockBody = BlockBody "GAGP"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 8892268781031230107, headerPrevHash = BlockHash (HeaderHash 4328474877338882864), headerSlot = SlotNo 3, headerBlockNo = BlockNo 2, headerBodyHash = BodyHash 4289418857123185028}, blockBody = BlockBody "TDPW"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 2007850722161460407, headerPrevHash = BlockHash (HeaderHash 8892268781031230107), headerSlot = SlotNo 4, headerBlockNo = BlockNo 3, headerBodyHash = BodyHash 6900878820210575189}, blockBody = BlockBody "XTKW"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-7549456504976611626), headerPrevHash = BlockHash (HeaderHash 2007850722161460407), headerSlot = SlotNo 5, headerBlockNo = BlockNo 4, headerBodyHash = BodyHash 8174063807321472320}, blockBody = BlockBody "ZNZE"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-4770565631072145400), headerPrevHash = BlockHash (HeaderHash (-7549456504976611626)), headerSlot = SlotNo 6, headerBlockNo = BlockNo 5, headerBodyHash = BodyHash 3040942095316525617}, blockBody = BlockBody "RLUO"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 5703695282222863984, headerPrevHash = BlockHash (HeaderHash (-4770565631072145400)), headerSlot = SlotNo 6, headerBlockNo = BlockNo 6, headerBodyHash = BodyHash 3773367472612108498}, blockBody = BlockBody "UUEX"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 6280493029723419884, headerPrevHash = BlockHash (HeaderHash 5703695282222863984), headerSlot = SlotNo 7, headerBlockNo = BlockNo 7, headerBodyHash = BodyHash (-6813658468823702600)}, blockBody = BlockBody "BFTC"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-1370011859436889178), headerPrevHash = BlockHash (HeaderHash 6280493029723419884), headerSlot = SlotNo 9, headerBlockNo = BlockNo 8, headerBodyHash = BodyHash (-8667037050287943331)}, blockBody = BlockBody "AZCJ"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 7467534390125292631, headerPrevHash = BlockHash (HeaderHash (-1370011859436889178)), headerSlot = SlotNo 10, headerBlockNo = BlockNo 9, headerBodyHash = BodyHash (-3179488864552073538)}, blockBody = BlockBody "HVTW"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-4999917498025294311), headerPrevHash = BlockHash (HeaderHash 7467534390125292631), headerSlot = SlotNo 10, headerBlockNo = BlockNo 10, headerBodyHash = BodyHash (-3701436929924110354)}, blockBody = BlockBody "IMPC"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-8260587414278630090), headerPrevHash = BlockHash (HeaderHash (-4999917498025294311)), headerSlot = SlotNo 11, headerBlockNo = BlockNo 11, headerBodyHash = BodyHash 4789151561354856977}, blockBody = BlockBody "EMPTY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-4432645259351133920), headerPrevHash = BlockHash (HeaderHash (-8260587414278630090)), headerSlot = SlotNo 12, headerBlockNo = BlockNo 12, headerBodyHash = BodyHash 1158906942291898364}, blockBody = BlockBody "QROW"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 5896773643798259577, headerPrevHash = BlockHash (HeaderHash (-4432645259351133920)), headerSlot = SlotNo 13, headerBlockNo = BlockNo 13, headerBodyHash = BodyHash 5056140201119113317}, blockBody = BlockBody "WQXF"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 3782071223601480464, headerPrevHash = BlockHash (HeaderHash 5896773643798259577), headerSlot = SlotNo 15, headerBlockNo = BlockNo 14, headerBodyHash = BodyHash (-4320699470954917292)}, blockBody = BlockBody "FBGR"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-6013924744432734110), headerPrevHash = BlockHash (HeaderHash 3782071223601480464), headerSlot = SlotNo 16, headerBlockNo = BlockNo 15, headerBodyHash = BodyHash (-5582419850320729187)}, blockBody = BlockBody "DLAE"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 5361317579406728751, headerPrevHash = BlockHash (HeaderHash (-6013924744432734110)), headerSlot = SlotNo 17, headerBlockNo = BlockNo 16, headerBodyHash = BodyHash (-59751064163544706)}, blockBody = BlockBody "OQTQ"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-5575097281837967916), headerPrevHash = BlockHash (HeaderHash 5361317579406728751), headerSlot = SlotNo 18, headerBlockNo = BlockNo 17, headerBodyHash = BodyHash (-5077029330554704874)}, blockBody = BlockBody "GXRR"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-1816990299732855081), headerPrevHash = BlockHash (HeaderHash (-5575097281837967916)), headerSlot = SlotNo 19, headerBlockNo = BlockNo 18, headerBodyHash = BodyHash (-5082757786136548836)}, blockBody = BlockBody "GVHP"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-7490831801032048172), headerPrevHash = BlockHash (HeaderHash (-1816990299732855081)), headerSlot = SlotNo 19, headerBlockNo = BlockNo 19, headerBodyHash = BodyHash 6174232476031734105}, blockBody = BlockBody "YYKA"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 627861834957497294, headerPrevHash = BlockHash (HeaderHash (-7490831801032048172)), headerSlot = SlotNo 20, headerBlockNo = BlockNo 20, headerBodyHash = BodyHash 3779100326240465156}, blockBody = BlockBody "USGJ"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-4240104898043199169), headerPrevHash = BlockHash (HeaderHash 627861834957497294), headerSlot = SlotNo 21, headerBlockNo = BlockNo 21, headerBodyHash = BodyHash (-1204808861752742139)}, blockBody = BlockBody "MEVZ"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 4728399419798400899, headerPrevHash = BlockHash (HeaderHash (-4240104898043199169)), headerSlot = SlotNo 22, headerBlockNo = BlockNo 22, headerBodyHash = BodyHash 5659094785683511524}, blockBody = BlockBody "VEQW"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-567528840456220035), headerPrevHash = BlockHash (HeaderHash 4728399419798400899), headerSlot = SlotNo 23, headerBlockNo = BlockNo 23, headerBodyHash = BodyHash (-2462522620746109740)}, blockBody = BlockBody "KOXU"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-5800825685882995973), headerPrevHash = BlockHash (HeaderHash (-567528840456220035)), headerSlot = SlotNo 23, headerBlockNo = BlockNo 24, headerBodyHash = BodyHash 4789151561354856977}, blockBody = BlockBody "EMPTY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-7977217156345972292), headerPrevHash = BlockHash (HeaderHash (-5800825685882995973)), headerSlot = SlotNo 24, headerBlockNo = BlockNo 25, headerBodyHash = BodyHash (-1942649333816128540)}, blockBody = BlockBody "JFCF"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 5367641256448617983, headerPrevHash = BlockHash (HeaderHash (-7977217156345972292)), headerSlot = SlotNo 25, headerBlockNo = BlockNo 26, headerBodyHash = BodyHash (-1196991334077675570)}, blockBody = BlockBody "MMXG"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-5540071596211906859), headerPrevHash = BlockHash (HeaderHash 5367641256448617983), headerSlot = SlotNo 25, headerBlockNo = BlockNo 27, headerBodyHash = BodyHash (-5083567026694722914)}, blockBody = BlockBody "GQNK"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-1495776734629695130), headerPrevHash = BlockHash (HeaderHash (-5540071596211906859)), headerSlot = SlotNo 26, headerBlockNo = BlockNo 28, headerBodyHash = BodyHash 5644775845752158077}, blockBody = BlockBody "VTLP"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-5288413224986553487), headerPrevHash = BlockHash (HeaderHash (-1495776734629695130)), headerSlot = SlotNo 27, headerBlockNo = BlockNo 29, headerBodyHash = BodyHash 4789151561354856977}, blockBody = BlockBody "EMPTY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-2381388809850177519), headerPrevHash = BlockHash (HeaderHash (-5288413224986553487)), headerSlot = SlotNo 27, headerBlockNo = BlockNo 30, headerBodyHash = BodyHash 4789151561354856977}, blockBody = BlockBody "EMPTY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-6659201155065996478), headerPrevHash = BlockHash (HeaderHash (-2381388809850177519)), headerSlot = SlotNo 27, headerBlockNo = BlockNo 31, headerBodyHash = BodyHash 4789151561354856977}, blockBody = BlockBody "EMPTY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-8378594247862146715), headerPrevHash = BlockHash (HeaderHash (-6659201155065996478)), headerSlot = SlotNo 28, headerBlockNo = BlockNo 32, headerBodyHash = BodyHash 4789151561354856977}, blockBody = BlockBody "EMPTY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-169687412501327813), headerPrevHash = BlockHash (HeaderHash (-8378594247862146715)), headerSlot = SlotNo 30, headerBlockNo = BlockNo 33, headerBodyHash = BodyHash (-4300753230511515293)}, blockBody = BlockBody "FYDS"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 6287810161833886734, headerPrevHash = BlockHash (HeaderHash (-169687412501327813)), headerSlot = SlotNo 30, headerBlockNo = BlockNo 34, headerBodyHash = BodyHash (-3194952396088205343)}, blockBody = BlockBody "HFDH"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 4679954912459070431, headerPrevHash = BlockHash (HeaderHash 6287810161833886734), headerSlot = SlotNo 31, headerBlockNo = BlockNo 35, headerBodyHash = BodyHash 4789151561354856977}, blockBody = BlockBody "EMPTY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-3106016354284443919), headerPrevHash = BlockHash (HeaderHash 4679954912459070431), headerSlot = SlotNo 32, headerBlockNo = BlockNo 36, headerBodyHash = BodyHash (-7438794600971277222)}, blockBody = BlockBody "CMAJ"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-1762699776291766253), headerPrevHash = BlockHash (HeaderHash (-3106016354284443919)), headerSlot = SlotNo 32, headerBlockNo = BlockNo 37, headerBodyHash = BodyHash (-78902357699939551)}, blockBody = BlockBody "OMBD"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-767019252039038623), headerPrevHash = BlockHash (HeaderHash (-1762699776291766253)), headerSlot = SlotNo 34, headerBlockNo = BlockNo 38, headerBodyHash = BodyHash (-5062662012111709994)}, blockBody = BlockBody "GKAX"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-5637659325550844901), headerPrevHash = BlockHash (HeaderHash (-767019252039038623)), headerSlot = SlotNo 35, headerBlockNo = BlockNo 39, headerBodyHash = BodyHash (-5083559330113325336)}, blockBody = BlockBody "GQGR"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 6517087232344273573, headerPrevHash = BlockHash (HeaderHash (-5637659325550844901)), headerSlot = SlotNo 36, headerBlockNo = BlockNo 40, headerBodyHash = BodyHash 5655422416846043554}, blockBody = BlockBody "VYEQ"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 2379843637725626457, headerPrevHash = BlockHash (HeaderHash 6517087232344273573), headerSlot = SlotNo 38, headerBlockNo = BlockNo 41, headerBodyHash = BodyHash 4789151561354856977}, blockBody = BlockBody "EMPTY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 8198884557753145275, headerPrevHash = BlockHash (HeaderHash 2379843637725626457), headerSlot = SlotNo 38, headerBlockNo = BlockNo 42, headerBodyHash = BodyHash 3777027746821666014}, blockBody = BlockBody "UQTG"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 3174737263014445702, headerPrevHash = BlockHash (HeaderHash 8198884557753145275), headerSlot = SlotNo 39, headerBlockNo = BlockNo 43, headerBodyHash = BodyHash 5669788635777410259}, blockBody = BlockBody "VNOY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-6298645420252920385), headerPrevHash = BlockHash (HeaderHash 3174737263014445702), headerSlot = SlotNo 40, headerBlockNo = BlockNo 44, headerBodyHash = BodyHash (-6821173630801010857)}, blockBody = BlockBody "BNKM"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-9077760516575866659), headerPrevHash = BlockHash (HeaderHash (-6298645420252920385)), headerSlot = SlotNo 41, headerBlockNo = BlockNo 45, headerBodyHash = BodyHash (-4320697271931661024)}, blockBody = BlockBody "FBAL"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-234160861388118505), headerPrevHash = BlockHash (HeaderHash (-9077760516575866659)), headerSlot = SlotNo 42, headerBlockNo = BlockNo 46, headerBodyHash = BodyHash (-572826070697621890)}, blockBody = BlockBody "LKHJ"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-1143977236685610684), headerPrevHash = BlockHash (HeaderHash (-234160861388118505)), headerSlot = SlotNo 42, headerBlockNo = BlockNo 47, headerBodyHash = BodyHash (-4305541603651428164)}, blockBody = BlockBody "FRYX"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-8690606911552283272), headerPrevHash = BlockHash (HeaderHash (-1143977236685610684)), headerSlot = SlotNo 43, headerBlockNo = BlockNo 48, headerBodyHash = BodyHash 4789151561354856977}, blockBody = BlockBody "EMPTY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 3338148184774960918, headerPrevHash = BlockHash (HeaderHash (-8690606911552283272)), headerSlot = SlotNo 44, headerBlockNo = BlockNo 49, headerBodyHash = BodyHash (-7439637926389925973)}, blockBody = BlockBody "CLZG"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-8536406153759081365), headerPrevHash = BlockHash (HeaderHash 3338148184774960918), headerSlot = SlotNo 45, headerBlockNo = BlockNo 50, headerBodyHash = BodyHash 2540200310713631440}, blockBody = BlockBody "SEBA"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 8925331030893873114, headerPrevHash = BlockHash (HeaderHash (-8536406153759081365)), headerSlot = SlotNo 46, headerBlockNo = BlockNo 51, headerBodyHash = BodyHash 5673446710963711350}, blockBody = BlockBody "VJLE"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-3588332081810274554), headerPrevHash = BlockHash (HeaderHash 8925331030893873114), headerSlot = SlotNo 47, headerBlockNo = BlockNo 52, headerBodyHash = BodyHash (-6318513198407266931)}, blockBody = BlockBody "EGYU"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 8036236910378077090, headerPrevHash = BlockHash (HeaderHash (-3588332081810274554)), headerSlot = SlotNo 47, headerBlockNo = BlockNo 53, headerBodyHash = BodyHash (-78890263072029090)}, blockBody = BlockBody "OMWH"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 1007326028982932726, headerPrevHash = BlockHash (HeaderHash 8036236910378077090), headerSlot = SlotNo 48, headerBlockNo = BlockNo 54, headerBodyHash = BodyHash 1928446334590363596}, blockBody = BlockBody "PNTU"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 7706344778471058708, headerPrevHash = BlockHash (HeaderHash 1007326028982932726), headerSlot = SlotNo 48, headerBlockNo = BlockNo 55, headerBodyHash = BodyHash (-7445356486367115931)}, blockBody = BlockBody "CBIZ"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 8046227379714939032, headerPrevHash = BlockHash (HeaderHash 7706344778471058708), headerSlot = SlotNo 49, headerBlockNo = BlockNo 56, headerBodyHash = BodyHash (-61713692419523037)}, blockBody = BlockBody "OSYE"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 8835981034579839003, headerPrevHash = BlockHash (HeaderHash 8046227379714939032), headerSlot = SlotNo 49, headerBlockNo = BlockNo 57, headerBodyHash = BodyHash 1182826817758898047}, blockBody = BlockBody "QKHT"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 615484714761720674, headerPrevHash = BlockHash (HeaderHash 8835981034579839003), headerSlot = SlotNo 50, headerBlockNo = BlockNo 58, headerBodyHash = BodyHash (-6828858117569063735)}, blockBody = BlockBody "BVTB"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-6946888157183811554), headerPrevHash = BlockHash (HeaderHash 615484714761720674), headerSlot = SlotNo 50, headerBlockNo = BlockNo 59, headerBodyHash = BodyHash 4789151561354856977}, blockBody = BlockBody "EMPTY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 4906175707123868200, headerPrevHash = BlockHash (HeaderHash (-6946888157183811554)), headerSlot = SlotNo 51, headerBlockNo = BlockNo 60, headerBodyHash = BodyHash 5054222652839891787}, blockBody = BlockBody "WSDN"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-9171138975173368304), headerPrevHash = BlockHash (HeaderHash 4906175707123868200), headerSlot = SlotNo 52, headerBlockNo = BlockNo 61, headerBodyHash = BodyHash 4789151561354856977}, blockBody = BlockBody "EMPTY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-8511284803975569067), headerPrevHash = BlockHash (HeaderHash (-9171138975173368304)), headerSlot = SlotNo 53, headerBlockNo = BlockNo 62, headerBodyHash = BodyHash (-6835534352174236344)}, blockBody = BlockBody "BQNJ"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-3073738949907974846), headerPrevHash = BlockHash (HeaderHash (-8511284803975569067)), headerSlot = SlotNo 55, headerBlockNo = BlockNo 63, headerBodyHash = BodyHash 4789151561354856977}, blockBody = BlockBody "EMPTY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-8386910132267310575), headerPrevHash = BlockHash (HeaderHash (-3073738949907974846)), headerSlot = SlotNo 56, headerBlockNo = BlockNo 64, headerBodyHash = BodyHash 3781031068659225217}, blockBody = BlockBody "UMSQ"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 5580664522661511703, headerPrevHash = BlockHash (HeaderHash (-8386910132267310575)), headerSlot = SlotNo 58, headerBlockNo = BlockNo 65, headerBodyHash = BodyHash 5060916479631115751}, blockBody = BlockBody "WZBY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-8029120831442175421), headerPrevHash = BlockHash (HeaderHash 5580664522661511703), headerSlot = SlotNo 59, headerBlockNo = BlockNo 66, headerBodyHash = BodyHash 4789151561354856977}, blockBody = BlockBody "EMPTY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 3913937602254152853, headerPrevHash = BlockHash (HeaderHash (-8029120831442175421)), headerSlot = SlotNo 60, headerBlockNo = BlockNo 67, headerBodyHash = BodyHash 1938018682823676199}, blockBody = BlockBody "PTRB"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-1282934301868054500), headerPrevHash = BlockHash (HeaderHash 3913937602254152853), headerSlot = SlotNo 61, headerBlockNo = BlockNo 68, headerBodyHash = BodyHash 2524081470247274992}, blockBody = BlockBody "STLN"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 1810292822992857481, headerPrevHash = BlockHash (HeaderHash (-1282934301868054500)), headerSlot = SlotNo 62, headerBlockNo = BlockNo 69, headerBodyHash = BodyHash 653365789432809018}, blockBody = BlockBody "NVMN"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-3782318953827077315), headerPrevHash = BlockHash (HeaderHash 1810292822992857481), headerSlot = SlotNo 63, headerBlockNo = BlockNo 70, headerBodyHash = BodyHash 4789151561354856977}, blockBody = BlockBody "EMPTY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-8262490009810830918), headerPrevHash = BlockHash (HeaderHash (-3782318953827077315)), headerSlot = SlotNo 66, headerBlockNo = BlockNo 71, headerBodyHash = BodyHash 8179693306856777308}, blockBody = BlockBody "ZTZS"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 6634768913544106750, headerPrevHash = BlockHash (HeaderHash (-8262490009810830918)), headerSlot = SlotNo 67, headerBlockNo = BlockNo 72, headerBodyHash = BodyHash (-570115774534649571)}, blockBody = BlockBody "LHIQ"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-3071948418829990572), headerPrevHash = BlockHash (HeaderHash 6634768913544106750), headerSlot = SlotNo 69, headerBlockNo = BlockNo 73, headerBodyHash = BodyHash 3782933223775651954}, blockBody = BlockBody "UOEV"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 574235327930959882, headerPrevHash = BlockHash (HeaderHash (-3071948418829990572)), headerSlot = SlotNo 70, headerBlockNo = BlockNo 74, headerBodyHash = BodyHash (-6824068644917522752)}, blockBody = BlockBody "BMRJ"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 4241363020223521474, headerPrevHash = BlockHash (HeaderHash 574235327930959882), headerSlot = SlotNo 71, headerBlockNo = BlockNo 75, headerBodyHash = BodyHash 660047521596122680}, blockBody = BlockBody "NOBD"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 5180641329206919492, headerPrevHash = BlockHash (HeaderHash 4241363020223521474), headerSlot = SlotNo 73, headerBlockNo = BlockNo 76, headerBodyHash = BodyHash 6172309430194371369}, blockBody = BlockBody "YGDR"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 156443077183015534, headerPrevHash = BlockHash (HeaderHash 5180641329206919492), headerSlot = SlotNo 74, headerBlockNo = BlockNo 77, headerBodyHash = BodyHash 5045450749071727433}, blockBody = BlockBody "WJPQ"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-2785873744845830961), headerPrevHash = BlockHash (HeaderHash 156443077183015534), headerSlot = SlotNo 75, headerBlockNo = BlockNo 78, headerBodyHash = BodyHash 655255849921325390}, blockBody = BlockBody "NPZY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-7059405090773489378), headerPrevHash = BlockHash (HeaderHash (-2785873744845830961)), headerSlot = SlotNo 76, headerBlockNo = BlockNo 79, headerBodyHash = BodyHash (-2462536914397276612)}, blockBody = BlockBody "KOMV"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-100613380702580732), headerPrevHash = BlockHash (HeaderHash (-7059405090773489378)), headerSlot = SlotNo 77, headerBlockNo = BlockNo 80, headerBodyHash = BodyHash 649696719130225821}, blockBody = BlockBody "NZZP"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-1072059194744106675), headerPrevHash = BlockHash (HeaderHash (-100613380702580732)), headerSlot = SlotNo 78, headerBlockNo = BlockNo 81, headerBodyHash = BodyHash (-575733179442044230)}, blockBody = BlockBody "LNZI"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 2078213555099420979, headerPrevHash = BlockHash (HeaderHash (-1072059194744106675)), headerSlot = SlotNo 79, headerBlockNo = BlockNo 82, headerBodyHash = BodyHash (-6813669463939984729)}, blockBody = BlockBody "BFBZ"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 514715113000451136, headerPrevHash = BlockHash (HeaderHash 2078213555099420979), headerSlot = SlotNo 80, headerBlockNo = BlockNo 83, headerBodyHash = BodyHash (-61688403652074186)}, blockBody = BlockBody "OSBM"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 2489894815258065418, headerPrevHash = BlockHash (HeaderHash 514715113000451136), headerSlot = SlotNo 81, headerBlockNo = BlockNo 84, headerBodyHash = BodyHash 4789151561354856977}, blockBody = BlockBody "EMPTY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-9219554255083953111), headerPrevHash = BlockHash (HeaderHash 2489894815258065418), headerSlot = SlotNo 82, headerBlockNo = BlockNo 85, headerBodyHash = BodyHash 2548950224249231552}, blockBody = BlockBody "SJBB"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-7946966273777578849), headerPrevHash = BlockHash (HeaderHash (-9219554255083953111)), headerSlot = SlotNo 82, headerBlockNo = BlockNo 86, headerBodyHash = BodyHash 3047657912340313962}, blockBody = BlockBody "REEY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 8570523866722540363, headerPrevHash = BlockHash (HeaderHash (-7946966273777578849)), headerSlot = SlotNo 83, headerBlockNo = BlockNo 87, headerBodyHash = BodyHash (-3708977380668867614)}, blockBody = BlockBody "IUZU"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-2025225923125225756), headerPrevHash = BlockHash (HeaderHash 8570523866722540363), headerSlot = SlotNo 84, headerBlockNo = BlockNo 88, headerBodyHash = BodyHash 2525014955619436915}, blockBody = BlockBody "SUQK"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 2165853342051868555, headerPrevHash = BlockHash (HeaderHash (-2025225923125225756)), headerSlot = SlotNo 85, headerBlockNo = BlockNo 89, headerBodyHash = BodyHash 1186799353270867335}, blockBody = BlockBody "QOWI"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-4725372417266741475), headerPrevHash = BlockHash (HeaderHash 2165853342051868555), headerSlot = SlotNo 86, headerBlockNo = BlockNo 90, headerBodyHash = BodyHash 649664833293007826}, blockBody = BlockBody "NZGT"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 8165187270093753717, headerPrevHash = BlockHash (HeaderHash (-4725372417266741475)), headerSlot = SlotNo 87, headerBlockNo = BlockNo 91, headerBodyHash = BodyHash 2536503752620342818}, blockBody = BlockBody "SYHM"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-1657835997199340279), headerPrevHash = BlockHash (HeaderHash 8165187270093753717), headerSlot = SlotNo 88, headerBlockNo = BlockNo 92, headerBodyHash = BodyHash (-2457738645652709808)}, blockBody = BlockBody "KHYQ"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-3521356196696222797), headerPrevHash = BlockHash (HeaderHash (-1657835997199340279)), headerSlot = SlotNo 89, headerBlockNo = BlockNo 93, headerBodyHash = BodyHash (-4305547101209569244)}, blockBody = BlockBody "FRRA"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-2974053371854879401), headerPrevHash = BlockHash (HeaderHash (-3521356196696222797)), headerSlot = SlotNo 90, headerBlockNo = BlockNo 94, headerBodyHash = BodyHash (-5064448718507174416)}, blockBody = BlockBody "GERU"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 303296124194095639, headerPrevHash = BlockHash (HeaderHash (-2974053371854879401)), headerSlot = SlotNo 91, headerBlockNo = BlockNo 95, headerBodyHash = BodyHash (-6825986193196744394)}, blockBody = BlockBody "BKRZ"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 1831111038898129916, headerPrevHash = BlockHash (HeaderHash 303296124194095639), headerSlot = SlotNo 92, headerBlockNo = BlockNo 96, headerBodyHash = BodyHash (-58817578791382785)}, blockBody = BlockBody "OVGV"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 8151795743315401931, headerPrevHash = BlockHash (HeaderHash 1831111038898129916), headerSlot = SlotNo 94, headerBlockNo = BlockNo 97, headerBodyHash = BodyHash 2535406440015577492}, blockBody = BlockBody "SXHR"})
      
          (Genesis
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-6427038944772666313), headerPrevHash = GenesisHash, headerSlot = SlotNo 1, headerBlockNo = BlockNo 0, headerBodyHash = BodyHash 4789151561354856977}, blockBody = BlockBody "EMPTY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 4328474877338882864, headerPrevHash = BlockHash (HeaderHash (-6427038944772666313)), headerSlot = SlotNo 2, headerBlockNo = BlockNo 1, headerBodyHash = BodyHash (-5068394865740066918)}, blockBody = BlockBody "GAGP"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 8892268781031230107, headerPrevHash = BlockHash (HeaderHash 4328474877338882864), headerSlot = SlotNo 3, headerBlockNo = BlockNo 2, headerBodyHash = BodyHash 4289418857123185028}, blockBody = BlockBody "TDPW"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 2007850722161460407, headerPrevHash = BlockHash (HeaderHash 8892268781031230107), headerSlot = SlotNo 4, headerBlockNo = BlockNo 3, headerBodyHash = BodyHash 6900878820210575189}, blockBody = BlockBody "XTKW"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-7549456504976611626), headerPrevHash = BlockHash (HeaderHash 2007850722161460407), headerSlot = SlotNo 5, headerBlockNo = BlockNo 4, headerBodyHash = BodyHash 8174063807321472320}, blockBody = BlockBody "ZNZE"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-4770565631072145400), headerPrevHash = BlockHash (HeaderHash (-7549456504976611626)), headerSlot = SlotNo 6, headerBlockNo = BlockNo 5, headerBodyHash = BodyHash 3040942095316525617}, blockBody = BlockBody "RLUO"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 5703695282222863984, headerPrevHash = BlockHash (HeaderHash (-4770565631072145400)), headerSlot = SlotNo 6, headerBlockNo = BlockNo 6, headerBodyHash = BodyHash 3773367472612108498}, blockBody = BlockBody "UUEX"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 6280493029723419884, headerPrevHash = BlockHash (HeaderHash 5703695282222863984), headerSlot = SlotNo 7, headerBlockNo = BlockNo 7, headerBodyHash = BodyHash (-6813658468823702600)}, blockBody = BlockBody "BFTC"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-1370011859436889178), headerPrevHash = BlockHash (HeaderHash 6280493029723419884), headerSlot = SlotNo 9, headerBlockNo = BlockNo 8, headerBodyHash = BodyHash (-8667037050287943331)}, blockBody = BlockBody "AZCJ"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 7467534390125292631, headerPrevHash = BlockHash (HeaderHash (-1370011859436889178)), headerSlot = SlotNo 10, headerBlockNo = BlockNo 9, headerBodyHash = BodyHash (-3179488864552073538)}, blockBody = BlockBody "HVTW"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-4999917498025294311), headerPrevHash = BlockHash (HeaderHash 7467534390125292631), headerSlot = SlotNo 10, headerBlockNo = BlockNo 10, headerBodyHash = BodyHash (-3701436929924110354)}, blockBody = BlockBody "IMPC"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-8260587414278630090), headerPrevHash = BlockHash (HeaderHash (-4999917498025294311)), headerSlot = SlotNo 11, headerBlockNo = BlockNo 11, headerBodyHash = BodyHash 4789151561354856977}, blockBody = BlockBody "EMPTY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-4432645259351133920), headerPrevHash = BlockHash (HeaderHash (-8260587414278630090)), headerSlot = SlotNo 12, headerBlockNo = BlockNo 12, headerBodyHash = BodyHash 1158906942291898364}, blockBody = BlockBody "QROW"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 5896773643798259577, headerPrevHash = BlockHash (HeaderHash (-4432645259351133920)), headerSlot = SlotNo 13, headerBlockNo = BlockNo 13, headerBodyHash = BodyHash 5056140201119113317}, blockBody = BlockBody "WQXF"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 3782071223601480464, headerPrevHash = BlockHash (HeaderHash 5896773643798259577), headerSlot = SlotNo 15, headerBlockNo = BlockNo 14, headerBodyHash = BodyHash (-4320699470954917292)}, blockBody = BlockBody "FBGR"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-6013924744432734110), headerPrevHash = BlockHash (HeaderHash 3782071223601480464), headerSlot = SlotNo 16, headerBlockNo = BlockNo 15, headerBodyHash = BodyHash (-5582419850320729187)}, blockBody = BlockBody "DLAE"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 5361317579406728751, headerPrevHash = BlockHash (HeaderHash (-6013924744432734110)), headerSlot = SlotNo 17, headerBlockNo = BlockNo 16, headerBodyHash = BodyHash (-59751064163544706)}, blockBody = BlockBody "OQTQ"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-5575097281837967916), headerPrevHash = BlockHash (HeaderHash 5361317579406728751), headerSlot = SlotNo 18, headerBlockNo = BlockNo 17, headerBodyHash = BodyHash (-5077029330554704874)}, blockBody = BlockBody "GXRR"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-1816990299732855081), headerPrevHash = BlockHash (HeaderHash (-5575097281837967916)), headerSlot = SlotNo 19, headerBlockNo = BlockNo 18, headerBodyHash = BodyHash (-5082757786136548836)}, blockBody = BlockBody "GVHP"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-7490831801032048172), headerPrevHash = BlockHash (HeaderHash (-1816990299732855081)), headerSlot = SlotNo 19, headerBlockNo = BlockNo 19, headerBodyHash = BodyHash 6174232476031734105}, blockBody = BlockBody "YYKA"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 627861834957497294, headerPrevHash = BlockHash (HeaderHash (-7490831801032048172)), headerSlot = SlotNo 20, headerBlockNo = BlockNo 20, headerBodyHash = BodyHash 3779100326240465156}, blockBody = BlockBody "USGJ"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-4240104898043199169), headerPrevHash = BlockHash (HeaderHash 627861834957497294), headerSlot = SlotNo 21, headerBlockNo = BlockNo 21, headerBodyHash = BodyHash (-1204808861752742139)}, blockBody = BlockBody "MEVZ"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 4728399419798400899, headerPrevHash = BlockHash (HeaderHash (-4240104898043199169)), headerSlot = SlotNo 22, headerBlockNo = BlockNo 22, headerBodyHash = BodyHash 5659094785683511524}, blockBody = BlockBody "VEQW"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-567528840456220035), headerPrevHash = BlockHash (HeaderHash 4728399419798400899), headerSlot = SlotNo 23, headerBlockNo = BlockNo 23, headerBodyHash = BodyHash (-2462522620746109740)}, blockBody = BlockBody "KOXU"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-5800825685882995973), headerPrevHash = BlockHash (HeaderHash (-567528840456220035)), headerSlot = SlotNo 23, headerBlockNo = BlockNo 24, headerBodyHash = BodyHash 4789151561354856977}, blockBody = BlockBody "EMPTY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-7977217156345972292), headerPrevHash = BlockHash (HeaderHash (-5800825685882995973)), headerSlot = SlotNo 24, headerBlockNo = BlockNo 25, headerBodyHash = BodyHash (-1942649333816128540)}, blockBody = BlockBody "JFCF"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 5367641256448617983, headerPrevHash = BlockHash (HeaderHash (-7977217156345972292)), headerSlot = SlotNo 25, headerBlockNo = BlockNo 26, headerBodyHash = BodyHash (-1196991334077675570)}, blockBody = BlockBody "MMXG"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-5540071596211906859), headerPrevHash = BlockHash (HeaderHash 5367641256448617983), headerSlot = SlotNo 25, headerBlockNo = BlockNo 27, headerBodyHash = BodyHash (-5083567026694722914)}, blockBody = BlockBody "GQNK"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-1495776734629695130), headerPrevHash = BlockHash (HeaderHash (-5540071596211906859)), headerSlot = SlotNo 26, headerBlockNo = BlockNo 28, headerBodyHash = BodyHash 5644775845752158077}, blockBody = BlockBody "VTLP"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-5288413224986553487), headerPrevHash = BlockHash (HeaderHash (-1495776734629695130)), headerSlot = SlotNo 27, headerBlockNo = BlockNo 29, headerBodyHash = BodyHash 4789151561354856977}, blockBody = BlockBody "EMPTY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-2381388809850177519), headerPrevHash = BlockHash (HeaderHash (-5288413224986553487)), headerSlot = SlotNo 27, headerBlockNo = BlockNo 30, headerBodyHash = BodyHash 4789151561354856977}, blockBody = BlockBody "EMPTY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-6659201155065996478), headerPrevHash = BlockHash (HeaderHash (-2381388809850177519)), headerSlot = SlotNo 27, headerBlockNo = BlockNo 31, headerBodyHash = BodyHash 4789151561354856977}, blockBody = BlockBody "EMPTY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-8378594247862146715), headerPrevHash = BlockHash (HeaderHash (-6659201155065996478)), headerSlot = SlotNo 28, headerBlockNo = BlockNo 32, headerBodyHash = BodyHash 4789151561354856977}, blockBody = BlockBody "EMPTY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-169687412501327813), headerPrevHash = BlockHash (HeaderHash (-8378594247862146715)), headerSlot = SlotNo 30, headerBlockNo = BlockNo 33, headerBodyHash = BodyHash (-4300753230511515293)}, blockBody = BlockBody "FYDS"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 6287810161833886734, headerPrevHash = BlockHash (HeaderHash (-169687412501327813)), headerSlot = SlotNo 30, headerBlockNo = BlockNo 34, headerBodyHash = BodyHash (-3194952396088205343)}, blockBody = BlockBody "HFDH"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 4679954912459070431, headerPrevHash = BlockHash (HeaderHash 6287810161833886734), headerSlot = SlotNo 31, headerBlockNo = BlockNo 35, headerBodyHash = BodyHash 4789151561354856977}, blockBody = BlockBody "EMPTY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-3106016354284443919), headerPrevHash = BlockHash (HeaderHash 4679954912459070431), headerSlot = SlotNo 32, headerBlockNo = BlockNo 36, headerBodyHash = BodyHash (-7438794600971277222)}, blockBody = BlockBody "CMAJ"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-1762699776291766253), headerPrevHash = BlockHash (HeaderHash (-3106016354284443919)), headerSlot = SlotNo 32, headerBlockNo = BlockNo 37, headerBodyHash = BodyHash (-78902357699939551)}, blockBody = BlockBody "OMBD"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-767019252039038623), headerPrevHash = BlockHash (HeaderHash (-1762699776291766253)), headerSlot = SlotNo 34, headerBlockNo = BlockNo 38, headerBodyHash = BodyHash (-5062662012111709994)}, blockBody = BlockBody "GKAX"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-5637659325550844901), headerPrevHash = BlockHash (HeaderHash (-767019252039038623)), headerSlot = SlotNo 35, headerBlockNo = BlockNo 39, headerBodyHash = BodyHash (-5083559330113325336)}, blockBody = BlockBody "GQGR"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 6517087232344273573, headerPrevHash = BlockHash (HeaderHash (-5637659325550844901)), headerSlot = SlotNo 36, headerBlockNo = BlockNo 40, headerBodyHash = BodyHash 5655422416846043554}, blockBody = BlockBody "VYEQ"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 2379843637725626457, headerPrevHash = BlockHash (HeaderHash 6517087232344273573), headerSlot = SlotNo 38, headerBlockNo = BlockNo 41, headerBodyHash = BodyHash 4789151561354856977}, blockBody = BlockBody "EMPTY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 8198884557753145275, headerPrevHash = BlockHash (HeaderHash 2379843637725626457), headerSlot = SlotNo 38, headerBlockNo = BlockNo 42, headerBodyHash = BodyHash 3777027746821666014}, blockBody = BlockBody "UQTG"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 3174737263014445702, headerPrevHash = BlockHash (HeaderHash 8198884557753145275), headerSlot = SlotNo 39, headerBlockNo = BlockNo 43, headerBodyHash = BodyHash 5669788635777410259}, blockBody = BlockBody "VNOY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-6298645420252920385), headerPrevHash = BlockHash (HeaderHash 3174737263014445702), headerSlot = SlotNo 40, headerBlockNo = BlockNo 44, headerBodyHash = BodyHash (-6821173630801010857)}, blockBody = BlockBody "BNKM"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-9077760516575866659), headerPrevHash = BlockHash (HeaderHash (-6298645420252920385)), headerSlot = SlotNo 41, headerBlockNo = BlockNo 45, headerBodyHash = BodyHash (-4320697271931661024)}, blockBody = BlockBody "FBAL"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-234160861388118505), headerPrevHash = BlockHash (HeaderHash (-9077760516575866659)), headerSlot = SlotNo 42, headerBlockNo = BlockNo 46, headerBodyHash = BodyHash (-572826070697621890)}, blockBody = BlockBody "LKHJ"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-1143977236685610684), headerPrevHash = BlockHash (HeaderHash (-234160861388118505)), headerSlot = SlotNo 42, headerBlockNo = BlockNo 47, headerBodyHash = BodyHash (-4305541603651428164)}, blockBody = BlockBody "FRYX"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-8690606911552283272), headerPrevHash = BlockHash (HeaderHash (-1143977236685610684)), headerSlot = SlotNo 43, headerBlockNo = BlockNo 48, headerBodyHash = BodyHash 4789151561354856977}, blockBody = BlockBody "EMPTY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 3338148184774960918, headerPrevHash = BlockHash (HeaderHash (-8690606911552283272)), headerSlot = SlotNo 44, headerBlockNo = BlockNo 49, headerBodyHash = BodyHash (-7439637926389925973)}, blockBody = BlockBody "CLZG"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-8536406153759081365), headerPrevHash = BlockHash (HeaderHash 3338148184774960918), headerSlot = SlotNo 45, headerBlockNo = BlockNo 50, headerBodyHash = BodyHash 2540200310713631440}, blockBody = BlockBody "SEBA"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 8925331030893873114, headerPrevHash = BlockHash (HeaderHash (-8536406153759081365)), headerSlot = SlotNo 46, headerBlockNo = BlockNo 51, headerBodyHash = BodyHash 5673446710963711350}, blockBody = BlockBody "VJLE"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-3588332081810274554), headerPrevHash = BlockHash (HeaderHash 8925331030893873114), headerSlot = SlotNo 47, headerBlockNo = BlockNo 52, headerBodyHash = BodyHash (-6318513198407266931)}, blockBody = BlockBody "EGYU"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 8036236910378077090, headerPrevHash = BlockHash (HeaderHash (-3588332081810274554)), headerSlot = SlotNo 47, headerBlockNo = BlockNo 53, headerBodyHash = BodyHash (-78890263072029090)}, blockBody = BlockBody "OMWH"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 1007326028982932726, headerPrevHash = BlockHash (HeaderHash 8036236910378077090), headerSlot = SlotNo 48, headerBlockNo = BlockNo 54, headerBodyHash = BodyHash 1928446334590363596}, blockBody = BlockBody "PNTU"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 7706344778471058708, headerPrevHash = BlockHash (HeaderHash 1007326028982932726), headerSlot = SlotNo 48, headerBlockNo = BlockNo 55, headerBodyHash = BodyHash (-7445356486367115931)}, blockBody = BlockBody "CBIZ"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 8046227379714939032, headerPrevHash = BlockHash (HeaderHash 7706344778471058708), headerSlot = SlotNo 49, headerBlockNo = BlockNo 56, headerBodyHash = BodyHash (-61713692419523037)}, blockBody = BlockBody "OSYE"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 8835981034579839003, headerPrevHash = BlockHash (HeaderHash 8046227379714939032), headerSlot = SlotNo 49, headerBlockNo = BlockNo 57, headerBodyHash = BodyHash 1182826817758898047}, blockBody = BlockBody "QKHT"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 615484714761720674, headerPrevHash = BlockHash (HeaderHash 8835981034579839003), headerSlot = SlotNo 50, headerBlockNo = BlockNo 58, headerBodyHash = BodyHash (-6828858117569063735)}, blockBody = BlockBody "BVTB"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-6946888157183811554), headerPrevHash = BlockHash (HeaderHash 615484714761720674), headerSlot = SlotNo 50, headerBlockNo = BlockNo 59, headerBodyHash = BodyHash 4789151561354856977}, blockBody = BlockBody "EMPTY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 4906175707123868200, headerPrevHash = BlockHash (HeaderHash (-6946888157183811554)), headerSlot = SlotNo 51, headerBlockNo = BlockNo 60, headerBodyHash = BodyHash 5054222652839891787}, blockBody = BlockBody "WSDN"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-9171138975173368304), headerPrevHash = BlockHash (HeaderHash 4906175707123868200), headerSlot = SlotNo 52, headerBlockNo = BlockNo 61, headerBodyHash = BodyHash 4789151561354856977}, blockBody = BlockBody "EMPTY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-8511284803975569067), headerPrevHash = BlockHash (HeaderHash (-9171138975173368304)), headerSlot = SlotNo 53, headerBlockNo = BlockNo 62, headerBodyHash = BodyHash (-6835534352174236344)}, blockBody = BlockBody "BQNJ"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-3073738949907974846), headerPrevHash = BlockHash (HeaderHash (-8511284803975569067)), headerSlot = SlotNo 55, headerBlockNo = BlockNo 63, headerBodyHash = BodyHash 4789151561354856977}, blockBody = BlockBody "EMPTY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-8386910132267310575), headerPrevHash = BlockHash (HeaderHash (-3073738949907974846)), headerSlot = SlotNo 56, headerBlockNo = BlockNo 64, headerBodyHash = BodyHash 3781031068659225217}, blockBody = BlockBody "UMSQ"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 5580664522661511703, headerPrevHash = BlockHash (HeaderHash (-8386910132267310575)), headerSlot = SlotNo 58, headerBlockNo = BlockNo 65, headerBodyHash = BodyHash 5060916479631115751}, blockBody = BlockBody "WZBY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-8029120831442175421), headerPrevHash = BlockHash (HeaderHash 5580664522661511703), headerSlot = SlotNo 59, headerBlockNo = BlockNo 66, headerBodyHash = BodyHash 4789151561354856977}, blockBody = BlockBody "EMPTY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 3913937602254152853, headerPrevHash = BlockHash (HeaderHash (-8029120831442175421)), headerSlot = SlotNo 60, headerBlockNo = BlockNo 67, headerBodyHash = BodyHash 1938018682823676199}, blockBody = BlockBody "PTRB"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-1282934301868054500), headerPrevHash = BlockHash (HeaderHash 3913937602254152853), headerSlot = SlotNo 61, headerBlockNo = BlockNo 68, headerBodyHash = BodyHash 2524081470247274992}, blockBody = BlockBody "STLN"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 1810292822992857481, headerPrevHash = BlockHash (HeaderHash (-1282934301868054500)), headerSlot = SlotNo 62, headerBlockNo = BlockNo 69, headerBodyHash = BodyHash 653365789432809018}, blockBody = BlockBody "NVMN"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-3782318953827077315), headerPrevHash = BlockHash (HeaderHash 1810292822992857481), headerSlot = SlotNo 63, headerBlockNo = BlockNo 70, headerBodyHash = BodyHash 4789151561354856977}, blockBody = BlockBody "EMPTY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-8262490009810830918), headerPrevHash = BlockHash (HeaderHash (-3782318953827077315)), headerSlot = SlotNo 66, headerBlockNo = BlockNo 71, headerBodyHash = BodyHash 8179693306856777308}, blockBody = BlockBody "ZTZS"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 6634768913544106750, headerPrevHash = BlockHash (HeaderHash (-8262490009810830918)), headerSlot = SlotNo 67, headerBlockNo = BlockNo 72, headerBodyHash = BodyHash (-570115774534649571)}, blockBody = BlockBody "LHIQ"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-3071948418829990572), headerPrevHash = BlockHash (HeaderHash 6634768913544106750), headerSlot = SlotNo 69, headerBlockNo = BlockNo 73, headerBodyHash = BodyHash 3782933223775651954}, blockBody = BlockBody "UOEV"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 574235327930959882, headerPrevHash = BlockHash (HeaderHash (-3071948418829990572)), headerSlot = SlotNo 70, headerBlockNo = BlockNo 74, headerBodyHash = BodyHash (-6824068644917522752)}, blockBody = BlockBody "BMRJ"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 4241363020223521474, headerPrevHash = BlockHash (HeaderHash 574235327930959882), headerSlot = SlotNo 71, headerBlockNo = BlockNo 75, headerBodyHash = BodyHash 660047521596122680}, blockBody = BlockBody "NOBD"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 5180641329206919492, headerPrevHash = BlockHash (HeaderHash 4241363020223521474), headerSlot = SlotNo 73, headerBlockNo = BlockNo 76, headerBodyHash = BodyHash 6172309430194371369}, blockBody = BlockBody "YGDR"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 156443077183015534, headerPrevHash = BlockHash (HeaderHash 5180641329206919492), headerSlot = SlotNo 74, headerBlockNo = BlockNo 77, headerBodyHash = BodyHash 5045450749071727433}, blockBody = BlockBody "WJPQ"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-2785873744845830961), headerPrevHash = BlockHash (HeaderHash 156443077183015534), headerSlot = SlotNo 75, headerBlockNo = BlockNo 78, headerBodyHash = BodyHash 655255849921325390}, blockBody = BlockBody "NPZY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-7059405090773489378), headerPrevHash = BlockHash (HeaderHash (-2785873744845830961)), headerSlot = SlotNo 76, headerBlockNo = BlockNo 79, headerBodyHash = BodyHash (-2462536914397276612)}, blockBody = BlockBody "KOMV"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-100613380702580732), headerPrevHash = BlockHash (HeaderHash (-7059405090773489378)), headerSlot = SlotNo 77, headerBlockNo = BlockNo 80, headerBodyHash = BodyHash 649696719130225821}, blockBody = BlockBody "NZZP"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-1072059194744106675), headerPrevHash = BlockHash (HeaderHash (-100613380702580732)), headerSlot = SlotNo 78, headerBlockNo = BlockNo 81, headerBodyHash = BodyHash (-575733179442044230)}, blockBody = BlockBody "LNZI"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 2078213555099420979, headerPrevHash = BlockHash (HeaderHash (-1072059194744106675)), headerSlot = SlotNo 79, headerBlockNo = BlockNo 82, headerBodyHash = BodyHash (-6813669463939984729)}, blockBody = BlockBody "BFBZ"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 514715113000451136, headerPrevHash = BlockHash (HeaderHash 2078213555099420979), headerSlot = SlotNo 80, headerBlockNo = BlockNo 83, headerBodyHash = BodyHash (-61688403652074186)}, blockBody = BlockBody "OSBM"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 2489894815258065418, headerPrevHash = BlockHash (HeaderHash 514715113000451136), headerSlot = SlotNo 81, headerBlockNo = BlockNo 84, headerBodyHash = BodyHash 4789151561354856977}, blockBody = BlockBody "EMPTY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-9219554255083953111), headerPrevHash = BlockHash (HeaderHash 2489894815258065418), headerSlot = SlotNo 82, headerBlockNo = BlockNo 85, headerBodyHash = BodyHash 2548950224249231552}, blockBody = BlockBody "SJBB"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-7946966273777578849), headerPrevHash = BlockHash (HeaderHash (-9219554255083953111)), headerSlot = SlotNo 82, headerBlockNo = BlockNo 86, headerBodyHash = BodyHash 3047657912340313962}, blockBody = BlockBody "REEY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 8570523866722540363, headerPrevHash = BlockHash (HeaderHash (-7946966273777578849)), headerSlot = SlotNo 83, headerBlockNo = BlockNo 87, headerBodyHash = BodyHash (-3708977380668867614)}, blockBody = BlockBody "IUZU"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-2025225923125225756), headerPrevHash = BlockHash (HeaderHash 8570523866722540363), headerSlot = SlotNo 84, headerBlockNo = BlockNo 88, headerBodyHash = BodyHash 2525014955619436915}, blockBody = BlockBody "SUQK"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 2165853342051868555, headerPrevHash = BlockHash (HeaderHash (-2025225923125225756)), headerSlot = SlotNo 85, headerBlockNo = BlockNo 89, headerBodyHash = BodyHash 1186799353270867335}, blockBody = BlockBody "QOWI"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-4725372417266741475), headerPrevHash = BlockHash (HeaderHash 2165853342051868555), headerSlot = SlotNo 86, headerBlockNo = BlockNo 90, headerBodyHash = BodyHash 649664833293007826}, blockBody = BlockBody "NZGT"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 8165187270093753717, headerPrevHash = BlockHash (HeaderHash (-4725372417266741475)), headerSlot = SlotNo 87, headerBlockNo = BlockNo 91, headerBodyHash = BodyHash 2536503752620342818}, blockBody = BlockBody "SYHM"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-1657835997199340279), headerPrevHash = BlockHash (HeaderHash 8165187270093753717), headerSlot = SlotNo 88, headerBlockNo = BlockNo 92, headerBodyHash = BodyHash (-2457738645652709808)}, blockBody = BlockBody "KHYQ"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-3521356196696222797), headerPrevHash = BlockHash (HeaderHash (-1657835997199340279)), headerSlot = SlotNo 89, headerBlockNo = BlockNo 93, headerBodyHash = BodyHash (-4305547101209569244)}, blockBody = BlockBody "FRRA"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-2974053371854879401), headerPrevHash = BlockHash (HeaderHash (-3521356196696222797)), headerSlot = SlotNo 90, headerBlockNo = BlockNo 94, headerBodyHash = BodyHash (-5064448718507174416)}, blockBody = BlockBody "GERU"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 303296124194095639, headerPrevHash = BlockHash (HeaderHash (-2974053371854879401)), headerSlot = SlotNo 91, headerBlockNo = BlockNo 95, headerBodyHash = BodyHash (-6825986193196744394)}, blockBody = BlockBody "BKRZ"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 1831111038898129916, headerPrevHash = BlockHash (HeaderHash 303296124194095639), headerSlot = SlotNo 92, headerBlockNo = BlockNo 96, headerBodyHash = BodyHash (-58817578791382785)}, blockBody = BlockBody "OVGV"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 8151795743315401931, headerPrevHash = BlockHash (HeaderHash 1831111038898129916), headerSlot = SlotNo 94, headerBlockNo = BlockNo 97, headerBodyHash = BodyHash 2535406440015577492}, blockBody = BlockBody "SXHR"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 6412153648231604222, headerPrevHash = BlockHash (HeaderHash 8151795743315401931), headerSlot = SlotNo 96, headerBlockNo = BlockNo 98, headerBodyHash = BodyHash 3042881633828311519}, blockBody = BlockBody "RNMG"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-5123058436441829411), headerPrevHash = BlockHash (HeaderHash 6412153648231604222), headerSlot = SlotNo 99, headerBlockNo = BlockNo 99, headerBodyHash = BodyHash 6896929374442798151}, blockBody = BlockBody "XPKI"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-1493903062738626065), headerPrevHash = BlockHash (HeaderHash (-5123058436441829411)), headerSlot = SlotNo 100, headerBlockNo = BlockNo 100, headerBodyHash = BodyHash 6171515582798992282}, blockBody = BlockBody "YFPT"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 2012415056138052904, headerPrevHash = BlockHash (HeaderHash (-1493903062738626065)), headerSlot = SlotNo 101, headerBlockNo = BlockNo 101, headerBodyHash = BodyHash (-4307432763651572535)}, blockBody = BlockBody "FPUG"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 3253699990468388581, headerPrevHash = BlockHash (HeaderHash 2012415056138052904), headerSlot = SlotNo 102, headerBlockNo = BlockNo 102, headerBodyHash = BodyHash 8181593262949947303}, blockBody = BlockBody "ZVZZ"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-707375383727290378), headerPrevHash = BlockHash (HeaderHash 3253699990468388581), headerSlot = SlotNo 104, headerBlockNo = BlockNo 103, headerBodyHash = BodyHash (-4300737837348720450)}, blockBody = BlockBody "FYJD"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 6003472231920219141, headerPrevHash = BlockHash (HeaderHash (-707375383727290378)), headerSlot = SlotNo 105, headerBlockNo = BlockNo 104, headerBodyHash = BodyHash (-5084657742229719032)}, blockBody = BlockBody "GPLV"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 6103000410555104861, headerPrevHash = BlockHash (HeaderHash 6003472231920219141), headerSlot = SlotNo 106, headerBlockNo = BlockNo 105, headerBodyHash = BodyHash (-2469267025072231463)}, blockBody = BlockBody "KTTG"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 8681401619987144529, headerPrevHash = BlockHash (HeaderHash 6103000410555104861), headerSlot = SlotNo 107, headerBlockNo = BlockNo 106, headerBodyHash = BodyHash 6904734807489954257}, blockBody = BlockBody "XHTV"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-878274840044853707), headerPrevHash = BlockHash (HeaderHash 8681401619987144529), headerSlot = SlotNo 108, headerBlockNo = BlockNo 107, headerBodyHash = BodyHash 4789151561354856977}, blockBody = BlockBody "EMPTY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-4364849471685537223), headerPrevHash = BlockHash (HeaderHash (-878274840044853707)), headerSlot = SlotNo 109, headerBlockNo = BlockNo 108, headerBodyHash = BodyHash 4279875096192205933}, blockBody = BlockBody "TNDX"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-6627146289321754884), headerPrevHash = BlockHash (HeaderHash (-4364849471685537223)), headerSlot = SlotNo 110, headerBlockNo = BlockNo 109, headerBodyHash = BodyHash (-585294532559074700)}, blockBody = BlockBody "LXZQ"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-8226875839215130229), headerPrevHash = BlockHash (HeaderHash (-6627146289321754884)), headerSlot = SlotNo 112, headerBlockNo = BlockNo 110, headerBodyHash = BodyHash 6908396181211139859}, blockBody = BlockBody "XLFZ"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-1750443020039302550), headerPrevHash = BlockHash (HeaderHash (-8226875839215130229)), headerSlot = SlotNo 113, headerBlockNo = BlockNo 111, headerBodyHash = BodyHash 1921779995589844753}, blockBody = BlockBody "PEKJ"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-3919097623294816103), headerPrevHash = BlockHash (HeaderHash (-1750443020039302550)), headerSlot = SlotNo 113, headerBlockNo = BlockNo 112, headerBodyHash = BodyHash (-2455835391024655040)}, blockBody = BlockBody "KFPH"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 2013968353265403089, headerPrevHash = BlockHash (HeaderHash (-3919097623294816103)), headerSlot = SlotNo 114, headerBlockNo = BlockNo 113, headerBodyHash = BodyHash 4789151561354856977}, blockBody = BlockBody "EMPTY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-6130653855106511923), headerPrevHash = BlockHash (HeaderHash 2013968353265403089), headerSlot = SlotNo 115, headerBlockNo = BlockNo 114, headerBodyHash = BodyHash 4789151561354856977}, blockBody = BlockBody "EMPTY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-384042927234278767), headerPrevHash = BlockHash (HeaderHash (-6130653855106511923)), headerSlot = SlotNo 116, headerBlockNo = BlockNo 115, headerBodyHash = BodyHash 4789151561354856977}, blockBody = BlockBody "EMPTY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-8663339170830828823), headerPrevHash = BlockHash (HeaderHash (-384042927234278767)), headerSlot = SlotNo 117, headerBlockNo = BlockNo 116, headerBodyHash = BodyHash 5661167365102310689}, blockBody = BlockBody "VGNI"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 4688865511004958306, headerPrevHash = BlockHash (HeaderHash (-8663339170830828823)), headerSlot = SlotNo 117, headerBlockNo = BlockNo 117, headerBodyHash = BodyHash 4789151561354856977}, blockBody = BlockBody "EMPTY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 7555759584685446504, headerPrevHash = BlockHash (HeaderHash 4688865511004958306), headerSlot = SlotNo 118, headerBlockNo = BlockNo 118, headerBodyHash = BodyHash 2537333883899453036}, blockBody = BlockBody "SFEC"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-7139285410243154639), headerPrevHash = BlockHash (HeaderHash 7555759584685446504), headerSlot = SlotNo 119, headerBlockNo = BlockNo 119, headerBodyHash = BodyHash (-73156309932044067)}, blockBody = BlockBody "OGHP"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-3955131969092706291), headerPrevHash = BlockHash (HeaderHash (-7139285410243154639)), headerSlot = SlotNo 120, headerBlockNo = BlockNo 120, headerBodyHash = BodyHash (-5078791847694348572)}, blockBody = BlockBody "GZWY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-2909739951577687979), headerPrevHash = BlockHash (HeaderHash (-3955131969092706291)), headerSlot = SlotNo 122, headerBlockNo = BlockNo 121, headerBodyHash = BodyHash 8162461760622860441}, blockBody = BlockBody "ZBJP"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 1791855424853833577, headerPrevHash = BlockHash (HeaderHash (-2909739951577687979)), headerSlot = SlotNo 123, headerBlockNo = BlockNo 122, headerBodyHash = BodyHash 4789151561354856977}, blockBody = BlockBody "EMPTY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-7755301653358042675), headerPrevHash = BlockHash (HeaderHash 1791855424853833577), headerSlot = SlotNo 124, headerBlockNo = BlockNo 123, headerBodyHash = BodyHash 4789151561354856977}, blockBody = BlockBody "EMPTY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 3034645834864333305, headerPrevHash = BlockHash (HeaderHash (-7755301653358042675)), headerSlot = SlotNo 126, headerBlockNo = BlockNo 124, headerBodyHash = BodyHash 1179111567967929966}, blockBody = BlockBody "QGWH"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-6797345005778027591), headerPrevHash = BlockHash (HeaderHash 3034645834864333305), headerSlot = SlotNo 127, headerBlockNo = BlockNo 125, headerBodyHash = BodyHash (-2460628162211080796)}, blockBody = BlockBody "KMYT"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-2715608056180510354), headerPrevHash = BlockHash (HeaderHash (-6797345005778027591)), headerSlot = SlotNo 128, headerBlockNo = BlockNo 126, headerBodyHash = BodyHash 4789151561354856977}, blockBody = BlockBody "EMPTY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-5860098290521989467), headerPrevHash = BlockHash (HeaderHash (-2715608056180510354)), headerSlot = SlotNo 128, headerBlockNo = BlockNo 127, headerBodyHash = BodyHash (-4318780823164067692)}, blockBody = BlockBody "FLFC"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 2712580367977097773, headerPrevHash = BlockHash (HeaderHash (-5860098290521989467)), headerSlot = SlotNo 129, headerBlockNo = BlockNo 128, headerBodyHash = BodyHash (-6817346230823965407)}, blockBody = BlockBody "BBJP"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 8208569487403756352, headerPrevHash = BlockHash (HeaderHash 2712580367977097773), headerSlot = SlotNo 130, headerBlockNo = BlockNo 129, headerBodyHash = BodyHash (-8686154258963863620)}, blockBody = BlockBody "AFLF"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 4046762891741627655, headerPrevHash = BlockHash (HeaderHash 8208569487403756352), headerSlot = SlotNo 131, headerBlockNo = BlockNo 130, headerBodyHash = BodyHash (-68374533861900568)}, blockBody = BlockBody "OXKO"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 3593534007086037718, headerPrevHash = BlockHash (HeaderHash 4046762891741627655), headerSlot = SlotNo 133, headerBlockNo = BlockNo 131, headerBodyHash = BodyHash 3060084592759894761}, blockBody = BlockBody "RPKY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 3938619223435333868, headerPrevHash = BlockHash (HeaderHash 3593534007086037718), headerSlot = SlotNo 134, headerBlockNo = BlockNo 132, headerBodyHash = BodyHash 3065964780946431850}, blockBody = BlockBody "RVOL"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-1639642751204430497), headerPrevHash = BlockHash (HeaderHash 3938619223435333868), headerSlot = SlotNo 135, headerBlockNo = BlockNo 133, headerBodyHash = BodyHash 8182689476043084465}, blockBody = BlockBody "ZYIB"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 6549766510494727693, headerPrevHash = BlockHash (HeaderHash (-1639642751204430497)), headerSlot = SlotNo 137, headerBlockNo = BlockNo 134, headerBodyHash = BodyHash 6162720589286635617}, blockBody = BlockBody "YMYS"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 734379313276501521, headerPrevHash = BlockHash (HeaderHash 6549766510494727693), headerSlot = SlotNo 138, headerBlockNo = BlockNo 135, headerBodyHash = BodyHash 3787732592031846703}, blockBody = BlockBody "UDRA"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 1260167145758093635, headerPrevHash = BlockHash (HeaderHash 734379313276501521), headerSlot = SlotNo 139, headerBlockNo = BlockNo 136, headerBodyHash = BodyHash (-6821182426894036527)}, blockBody = BlockBody "BNCS"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 961667494763797642, headerPrevHash = BlockHash (HeaderHash 1260167145758093635), headerSlot = SlotNo 139, headerBlockNo = BlockNo 137, headerBodyHash = BodyHash (-67278320768763439)}, blockBody = BlockBody "OYFL"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 5515984460172720021, headerPrevHash = BlockHash (HeaderHash 961667494763797642), headerSlot = SlotNo 140, headerBlockNo = BlockNo 138, headerBodyHash = BodyHash (-585285736466049041)}, blockBody = BlockBody "LXRR"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-1079594677753967728), headerPrevHash = BlockHash (HeaderHash 5515984460172720021), headerSlot = SlotNo 141, headerBlockNo = BlockNo 139, headerBodyHash = BodyHash 4789151561354856977}, blockBody = BlockBody "EMPTY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-8168888451569087218), headerPrevHash = BlockHash (HeaderHash (-1079594677753967728)), headerSlot = SlotNo 142, headerBlockNo = BlockNo 140, headerBodyHash = BodyHash (-2470185117281598534)}, blockBody = BlockBody "KWQT"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-2178350375699339590), headerPrevHash = BlockHash (HeaderHash (-8168888451569087218)), headerSlot = SlotNo 144, headerBlockNo = BlockNo 141, headerBodyHash = BodyHash 4789151561354856977}, blockBody = BlockBody "EMPTY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-4728614526377859590), headerPrevHash = BlockHash (HeaderHash (-2178350375699339590)), headerSlot = SlotNo 145, headerBlockNo = BlockNo 142, headerBodyHash = BodyHash (-8690971219406109747)}, blockBody = BlockBody "AASM"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 1609958724527118224, headerPrevHash = BlockHash (HeaderHash (-4728614526377859590)), headerSlot = SlotNo 146, headerBlockNo = BlockNo 143, headerBodyHash = BodyHash 5668669332940080555}, blockBody = BlockBody "VOIN"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-3719623131168204046), headerPrevHash = BlockHash (HeaderHash 1609958724527118224), headerSlot = SlotNo 147, headerBlockNo = BlockNo 144, headerBodyHash = BodyHash 3785819441799138135}, blockBody = BlockBody "UJVK"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-3993225176955497156), headerPrevHash = BlockHash (HeaderHash (-3719623131168204046)), headerSlot = SlotNo 149, headerBlockNo = BlockNo 145, headerBodyHash = BodyHash 6889248186209629902}, blockBody = BlockBody "XXQV"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 4681600006718091657, headerPrevHash = BlockHash (HeaderHash (-3993225176955497156)), headerSlot = SlotNo 151, headerBlockNo = BlockNo 146, headerBodyHash = BodyHash (-4309200778349357346)}, blockBody = BlockBody "FVQV"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 4509933808402135777, headerPrevHash = BlockHash (HeaderHash 4681600006718091657), headerSlot = SlotNo 152, headerBlockNo = BlockNo 147, headerBodyHash = BodyHash 4789151561354856977}, blockBody = BlockBody "EMPTY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-380484260320629574), headerPrevHash = BlockHash (HeaderHash 4509933808402135777), headerSlot = SlotNo 154, headerBlockNo = BlockNo 148, headerBodyHash = BodyHash 3047671106479852501}, blockBody = BlockBody "REIB"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 3952445034941420240, headerPrevHash = BlockHash (HeaderHash (-380484260320629574)), headerSlot = SlotNo 154, headerBlockNo = BlockNo 149, headerBodyHash = BodyHash 4789151561354856977}, blockBody = BlockBody "EMPTY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-7092763075045801713), headerPrevHash = BlockHash (HeaderHash 3952445034941420240), headerSlot = SlotNo 155, headerBlockNo = BlockNo 150, headerBodyHash = BodyHash (-7446450500436996636)}, blockBody = BlockBody "CELK"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-5056562336946350784), headerPrevHash = BlockHash (HeaderHash (-7092763075045801713)), headerSlot = SlotNo 155, headerBlockNo = BlockNo 151, headerBodyHash = BodyHash 4789151561354856977}, blockBody = BlockBody "EMPTY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-6845153578321315169), headerPrevHash = BlockHash (HeaderHash (-5056562336946350784)), headerSlot = SlotNo 155, headerBlockNo = BlockNo 152, headerBodyHash = BodyHash 8176825780530970538}, blockBody = BlockBody "ZSHP"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-4017793320969722352), headerPrevHash = BlockHash (HeaderHash (-6845153578321315169)), headerSlot = SlotNo 156, headerBlockNo = BlockNo 153, headerBodyHash = BodyHash (-1936111637676110512)}, blockBody = BlockBody "JOQA"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 7901249882737552869, headerPrevHash = BlockHash (HeaderHash (-4017793320969722352)), headerSlot = SlotNo 156, headerBlockNo = BlockNo 154, headerBodyHash = BodyHash 4789151561354856977}, blockBody = BlockBody "EMPTY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-8887332801077449706), headerPrevHash = BlockHash (HeaderHash 7901249882737552869), headerSlot = SlotNo 157, headerBlockNo = BlockNo 155, headerBodyHash = BodyHash (-62510838349786649)}, blockBody = BlockBody "ORHG"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-1370128205843823087), headerPrevHash = BlockHash (HeaderHash (-8887332801077449706)), headerSlot = SlotNo 158, headerBlockNo = BlockNo 156, headerBodyHash = BodyHash (-1178963741425123417)}, blockBody = BlockBody "MXBE"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-6895403357582523122), headerPrevHash = BlockHash (HeaderHash (-1370128205843823087)), headerSlot = SlotNo 159, headerBlockNo = BlockNo 157, headerBodyHash = BodyHash (-4313056765628736421)}, blockBody = BlockBody "FJLX"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 2724207257801174681, headerPrevHash = BlockHash (HeaderHash (-6895403357582523122)), headerSlot = SlotNo 160, headerBlockNo = BlockNo 158, headerBodyHash = BodyHash (-6301281652173350073)}, blockBody = BlockBody "EQMY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 855338984966309011, headerPrevHash = BlockHash (HeaderHash 2724207257801174681), headerSlot = SlotNo 161, headerBlockNo = BlockNo 159, headerBodyHash = BodyHash 2524084768782159494}, blockBody = BlockBody "STKQ"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 7076799066068039872, headerPrevHash = BlockHash (HeaderHash 855338984966309011), headerSlot = SlotNo 162, headerBlockNo = BlockNo 160, headerBodyHash = BodyHash 4789151561354856977}, blockBody = BlockBody "EMPTY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-8756156334103330695), headerPrevHash = BlockHash (HeaderHash 7076799066068039872), headerSlot = SlotNo 163, headerBlockNo = BlockNo 161, headerBodyHash = BodyHash (-4313041372465941605)}, blockBody = BlockBody "FJBV"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-3212565282410119673), headerPrevHash = BlockHash (HeaderHash (-8756156334103330695)), headerSlot = SlotNo 164, headerBlockNo = BlockNo 162, headerBodyHash = BodyHash (-3708141751831616584)}, blockBody = BlockBody "IVXN"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 1456726977676130134, headerPrevHash = BlockHash (HeaderHash (-3212565282410119673)), headerSlot = SlotNo 164, headerBlockNo = BlockNo 163, headerBodyHash = BodyHash (-592918546187575976)}, blockBody = BlockBody "LPLS"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 3750873179897437340, headerPrevHash = BlockHash (HeaderHash 1456726977676130134), headerSlot = SlotNo 165, headerBlockNo = BlockNo 164, headerBodyHash = BodyHash (-3201512082460787607)}, blockBody = BlockBody "HMXW"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 5934448704612135998, headerPrevHash = BlockHash (HeaderHash 3750873179897437340), headerSlot = SlotNo 167, headerBlockNo = BlockNo 165, headerBodyHash = BodyHash (-3204525843833146272)}, blockBody = BlockBody "HHWJ"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-8070719333180913461), headerPrevHash = BlockHash (HeaderHash 5934448704612135998), headerSlot = SlotNo 168, headerBlockNo = BlockNo 166, headerBodyHash = BodyHash (-6817341832777452427)}, blockBody = BlockBody "BBNH"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 2458891412593543371, headerPrevHash = BlockHash (HeaderHash (-8070719333180913461)), headerSlot = SlotNo 168, headerBlockNo = BlockNo 167, headerBodyHash = BodyHash 6901712250024569737}, blockBody = BlockBody "XKIR"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-4058330811380230531), headerPrevHash = BlockHash (HeaderHash 2458891412593543371), headerSlot = SlotNo 170, headerBlockNo = BlockNo 168, headerBodyHash = BodyHash 1166544150059938148}, blockBody = BlockBody "QZMM"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 3182311933384000745, headerPrevHash = BlockHash (HeaderHash (-4058330811380230531)), headerSlot = SlotNo 172, headerBlockNo = BlockNo 169, headerBodyHash = BodyHash 4303772981426641391}, blockBody = BlockBody "TUMV"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-3759806092796042077), headerPrevHash = BlockHash (HeaderHash 3182311933384000745), headerSlot = SlotNo 173, headerBlockNo = BlockNo 170, headerBodyHash = BodyHash 4789151561354856977}, blockBody = BlockBody "EMPTY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-1969815150867964514), headerPrevHash = BlockHash (HeaderHash (-3759806092796042077)), headerSlot = SlotNo 174, headerBlockNo = BlockNo 171, headerBodyHash = BodyHash 655267944549235602}, blockBody = BlockBody "NPOZ"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 3879633566770809157, headerPrevHash = BlockHash (HeaderHash (-1969815150867964514)), headerSlot = SlotNo 175, headerBlockNo = BlockNo 172, headerBodyHash = BodyHash (-7437681895203716918)}, blockBody = BlockBody "CJUI"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-2325165492393518232), headerPrevHash = BlockHash (HeaderHash 3879633566770809157), headerSlot = SlotNo 176, headerBlockNo = BlockNo 173, headerBodyHash = BodyHash 1162872880734098518}, blockBody = BlockBody "QVFJ"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-1142270898580846104), headerPrevHash = BlockHash (HeaderHash (-2325165492393518232)), headerSlot = SlotNo 177, headerBlockNo = BlockNo 174, headerBodyHash = BodyHash (-6823225319498874119)}, blockBody = BlockBody "BLUW"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 3270346997088534573, headerPrevHash = BlockHash (HeaderHash (-1142270898580846104)), headerSlot = SlotNo 178, headerBlockNo = BlockNo 175, headerBodyHash = BodyHash (-8674572003474559500)}, blockBody = BlockBody "ARVT"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-7244966862811338556), headerPrevHash = BlockHash (HeaderHash 3270346997088534573), headerSlot = SlotNo 178, headerBlockNo = BlockNo 176, headerBodyHash = BodyHash (-5081804509555079157)}, blockBody = BlockBody "GWKW"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-2729888983177923927), headerPrevHash = BlockHash (HeaderHash (-7244966862811338556)), headerSlot = SlotNo 179, headerBlockNo = BlockNo 177, headerBodyHash = BodyHash (-1937049521094785401)}, blockBody = BlockBody "JLNL"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 2604206733679410979, headerPrevHash = BlockHash (HeaderHash (-2729888983177923927)), headerSlot = SlotNo 181, headerBlockNo = BlockNo 178, headerBodyHash = BodyHash 4789151561354856977}, blockBody = BlockBody "EMPTY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 8729647381118641789, headerPrevHash = BlockHash (HeaderHash 2604206733679410979), headerSlot = SlotNo 182, headerBlockNo = BlockNo 179, headerBodyHash = BodyHash 4789151561354856977}, blockBody = BlockBody "EMPTY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-3639308010239911062), headerPrevHash = BlockHash (HeaderHash 8729647381118641789), headerSlot = SlotNo 184, headerBlockNo = BlockNo 180, headerBodyHash = BodyHash 3793441256404382851}, blockBody = BlockBody "UBJS"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-7292380893282018920), headerPrevHash = BlockHash (HeaderHash (-3639308010239911062)), headerSlot = SlotNo 185, headerBlockNo = BlockNo 181, headerBodyHash = BodyHash (-58804384651844286)}, blockBody = BlockBody "OVKO"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-165695404375863607), headerPrevHash = BlockHash (HeaderHash (-7292380893282018920)), headerSlot = SlotNo 187, headerBlockNo = BlockNo 182, headerBodyHash = BodyHash (-1949355255235262870)}, blockBody = BlockBody "JAXR"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-8883893160731984831), headerPrevHash = BlockHash (HeaderHash (-165695404375863607)), headerSlot = SlotNo 188, headerBlockNo = BlockNo 183, headerBodyHash = BodyHash (-3708160443529296055)}, blockBody = BlockBody "IVKZ"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 7337747309776605086, headerPrevHash = BlockHash (HeaderHash (-8883893160731984831)), headerSlot = SlotNo 189, headerBlockNo = BlockNo 184, headerBodyHash = BodyHash 662082717619562810}, blockBody = BlockBody "NIUQ"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-5772554636785993729), headerPrevHash = BlockHash (HeaderHash 7337747309776605086), headerSlot = SlotNo 189, headerBlockNo = BlockNo 185, headerBodyHash = BodyHash 4789151561354856977}, blockBody = BlockBody "EMPTY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-8735873887691237588), headerPrevHash = BlockHash (HeaderHash (-5772554636785993729)), headerSlot = SlotNo 190, headerBlockNo = BlockNo 186, headerBodyHash = BodyHash 5044647006071694439}, blockBody = BlockBody "WEEG"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-339089106582867287), headerPrevHash = BlockHash (HeaderHash (-8735873887691237588)), headerSlot = SlotNo 191, headerBlockNo = BlockNo 187, headerBodyHash = BodyHash 1937223735916669021}, blockBody = BlockBody "PUEX"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 1646637510642484498, headerPrevHash = BlockHash (HeaderHash (-339089106582867287)), headerSlot = SlotNo 194, headerBlockNo = BlockNo 188, headerBodyHash = BodyHash (-6328064655919643643)}, blockBody = BlockBody "EMDP"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-3547071950084255153), headerPrevHash = BlockHash (HeaderHash 1646637510642484498), headerSlot = SlotNo 195, headerBlockNo = BlockNo 189, headerBodyHash = BodyHash 2548020037411954299}, blockBody = BlockBody "SMZZ"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 5010611153011054956, headerPrevHash = BlockHash (HeaderHash (-3547071950084255153)), headerSlot = SlotNo 196, headerBlockNo = BlockNo 190, headerBodyHash = BodyHash (-4316996315791859695)}, blockBody = BlockBody "FNOK"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 2319612914410740202, headerPrevHash = BlockHash (HeaderHash 5010611153011054956), headerSlot = SlotNo 197, headerBlockNo = BlockNo 191, headerBodyHash = BodyHash (-59762059279826844)}, blockBody = BlockBody "OQJU"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-3821811514114999305), headerPrevHash = BlockHash (HeaderHash 2319612914410740202), headerSlot = SlotNo 198, headerBlockNo = BlockNo 192, headerBodyHash = BodyHash 4789151561354856977}, blockBody = BlockBody "EMPTY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-3535234405928538197), headerPrevHash = BlockHash (HeaderHash (-3821811514114999305)), headerSlot = SlotNo 199, headerBlockNo = BlockNo 193, headerBodyHash = BodyHash (-7444394413692620632)}, blockBody = BlockBody "CCBO"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 2902647806029683637, headerPrevHash = BlockHash (HeaderHash (-3535234405928538197)), headerSlot = SlotNo 199, headerBlockNo = BlockNo 194, headerBodyHash = BodyHash (-1943721357653445025)}, blockBody = BlockBody "JGXQ"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-3334412849366346842), headerPrevHash = BlockHash (HeaderHash 2902647806029683637), headerSlot = SlotNo 200, headerBlockNo = BlockNo 195, headerBodyHash = BodyHash (-5059647151227723262)}, blockBody = BlockBody "GNCW"})
      
          (Genesis
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-6427038944772666313), headerPrevHash = GenesisHash, headerSlot = SlotNo 1, headerBlockNo = BlockNo 0, headerBodyHash = BodyHash 4789151561354856977}, blockBody = BlockBody "EMPTY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 4328474877338882864, headerPrevHash = BlockHash (HeaderHash (-6427038944772666313)), headerSlot = SlotNo 2, headerBlockNo = BlockNo 1, headerBodyHash = BodyHash (-5068394865740066918)}, blockBody = BlockBody "GAGP"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 8892268781031230107, headerPrevHash = BlockHash (HeaderHash 4328474877338882864), headerSlot = SlotNo 3, headerBlockNo = BlockNo 2, headerBodyHash = BodyHash 4289418857123185028}, blockBody = BlockBody "TDPW"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 2007850722161460407, headerPrevHash = BlockHash (HeaderHash 8892268781031230107), headerSlot = SlotNo 4, headerBlockNo = BlockNo 3, headerBodyHash = BodyHash 6900878820210575189}, blockBody = BlockBody "XTKW"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-7549456504976611626), headerPrevHash = BlockHash (HeaderHash 2007850722161460407), headerSlot = SlotNo 5, headerBlockNo = BlockNo 4, headerBodyHash = BodyHash 8174063807321472320}, blockBody = BlockBody "ZNZE"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-4770565631072145400), headerPrevHash = BlockHash (HeaderHash (-7549456504976611626)), headerSlot = SlotNo 6, headerBlockNo = BlockNo 5, headerBodyHash = BodyHash 3040942095316525617}, blockBody = BlockBody "RLUO"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 5703695282222863984, headerPrevHash = BlockHash (HeaderHash (-4770565631072145400)), headerSlot = SlotNo 6, headerBlockNo = BlockNo 6, headerBodyHash = BodyHash 3773367472612108498}, blockBody = BlockBody "UUEX"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 6280493029723419884, headerPrevHash = BlockHash (HeaderHash 5703695282222863984), headerSlot = SlotNo 7, headerBlockNo = BlockNo 7, headerBodyHash = BodyHash (-6813658468823702600)}, blockBody = BlockBody "BFTC"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-1370011859436889178), headerPrevHash = BlockHash (HeaderHash 6280493029723419884), headerSlot = SlotNo 9, headerBlockNo = BlockNo 8, headerBodyHash = BodyHash (-8667037050287943331)}, blockBody = BlockBody "AZCJ"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 7467534390125292631, headerPrevHash = BlockHash (HeaderHash (-1370011859436889178)), headerSlot = SlotNo 10, headerBlockNo = BlockNo 9, headerBodyHash = BodyHash (-3179488864552073538)}, blockBody = BlockBody "HVTW"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-4999917498025294311), headerPrevHash = BlockHash (HeaderHash 7467534390125292631), headerSlot = SlotNo 10, headerBlockNo = BlockNo 10, headerBodyHash = BodyHash (-3701436929924110354)}, blockBody = BlockBody "IMPC"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-8260587414278630090), headerPrevHash = BlockHash (HeaderHash (-4999917498025294311)), headerSlot = SlotNo 11, headerBlockNo = BlockNo 11, headerBodyHash = BodyHash 4789151561354856977}, blockBody = BlockBody "EMPTY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-4432645259351133920), headerPrevHash = BlockHash (HeaderHash (-8260587414278630090)), headerSlot = SlotNo 12, headerBlockNo = BlockNo 12, headerBodyHash = BodyHash 1158906942291898364}, blockBody = BlockBody "QROW"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 5896773643798259577, headerPrevHash = BlockHash (HeaderHash (-4432645259351133920)), headerSlot = SlotNo 13, headerBlockNo = BlockNo 13, headerBodyHash = BodyHash 5056140201119113317}, blockBody = BlockBody "WQXF"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 3782071223601480464, headerPrevHash = BlockHash (HeaderHash 5896773643798259577), headerSlot = SlotNo 15, headerBlockNo = BlockNo 14, headerBodyHash = BodyHash (-4320699470954917292)}, blockBody = BlockBody "FBGR"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-6013924744432734110), headerPrevHash = BlockHash (HeaderHash 3782071223601480464), headerSlot = SlotNo 16, headerBlockNo = BlockNo 15, headerBodyHash = BodyHash (-5582419850320729187)}, blockBody = BlockBody "DLAE"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 5361317579406728751, headerPrevHash = BlockHash (HeaderHash (-6013924744432734110)), headerSlot = SlotNo 17, headerBlockNo = BlockNo 16, headerBodyHash = BodyHash (-59751064163544706)}, blockBody = BlockBody "OQTQ"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-5575097281837967916), headerPrevHash = BlockHash (HeaderHash 5361317579406728751), headerSlot = SlotNo 18, headerBlockNo = BlockNo 17, headerBodyHash = BodyHash (-5077029330554704874)}, blockBody = BlockBody "GXRR"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-1816990299732855081), headerPrevHash = BlockHash (HeaderHash (-5575097281837967916)), headerSlot = SlotNo 19, headerBlockNo = BlockNo 18, headerBodyHash = BodyHash (-5082757786136548836)}, blockBody = BlockBody "GVHP"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-7490831801032048172), headerPrevHash = BlockHash (HeaderHash (-1816990299732855081)), headerSlot = SlotNo 19, headerBlockNo = BlockNo 19, headerBodyHash = BodyHash 6174232476031734105}, blockBody = BlockBody "YYKA"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 627861834957497294, headerPrevHash = BlockHash (HeaderHash (-7490831801032048172)), headerSlot = SlotNo 20, headerBlockNo = BlockNo 20, headerBodyHash = BodyHash 3779100326240465156}, blockBody = BlockBody "USGJ"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-4240104898043199169), headerPrevHash = BlockHash (HeaderHash 627861834957497294), headerSlot = SlotNo 21, headerBlockNo = BlockNo 21, headerBodyHash = BodyHash (-1204808861752742139)}, blockBody = BlockBody "MEVZ"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 4728399419798400899, headerPrevHash = BlockHash (HeaderHash (-4240104898043199169)), headerSlot = SlotNo 22, headerBlockNo = BlockNo 22, headerBodyHash = BodyHash 5659094785683511524}, blockBody = BlockBody "VEQW"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-567528840456220035), headerPrevHash = BlockHash (HeaderHash 4728399419798400899), headerSlot = SlotNo 23, headerBlockNo = BlockNo 23, headerBodyHash = BodyHash (-2462522620746109740)}, blockBody = BlockBody "KOXU"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-5800825685882995973), headerPrevHash = BlockHash (HeaderHash (-567528840456220035)), headerSlot = SlotNo 23, headerBlockNo = BlockNo 24, headerBodyHash = BodyHash 4789151561354856977}, blockBody = BlockBody "EMPTY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-7977217156345972292), headerPrevHash = BlockHash (HeaderHash (-5800825685882995973)), headerSlot = SlotNo 24, headerBlockNo = BlockNo 25, headerBodyHash = BodyHash (-1942649333816128540)}, blockBody = BlockBody "JFCF"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 5367641256448617983, headerPrevHash = BlockHash (HeaderHash (-7977217156345972292)), headerSlot = SlotNo 25, headerBlockNo = BlockNo 26, headerBodyHash = BodyHash (-1196991334077675570)}, blockBody = BlockBody "MMXG"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-5540071596211906859), headerPrevHash = BlockHash (HeaderHash 5367641256448617983), headerSlot = SlotNo 25, headerBlockNo = BlockNo 27, headerBodyHash = BodyHash (-5083567026694722914)}, blockBody = BlockBody "GQNK"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-1495776734629695130), headerPrevHash = BlockHash (HeaderHash (-5540071596211906859)), headerSlot = SlotNo 26, headerBlockNo = BlockNo 28, headerBodyHash = BodyHash 5644775845752158077}, blockBody = BlockBody "VTLP"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-5288413224986553487), headerPrevHash = BlockHash (HeaderHash (-1495776734629695130)), headerSlot = SlotNo 27, headerBlockNo = BlockNo 29, headerBodyHash = BodyHash 4789151561354856977}, blockBody = BlockBody "EMPTY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-2381388809850177519), headerPrevHash = BlockHash (HeaderHash (-5288413224986553487)), headerSlot = SlotNo 27, headerBlockNo = BlockNo 30, headerBodyHash = BodyHash 4789151561354856977}, blockBody = BlockBody "EMPTY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-6659201155065996478), headerPrevHash = BlockHash (HeaderHash (-2381388809850177519)), headerSlot = SlotNo 27, headerBlockNo = BlockNo 31, headerBodyHash = BodyHash 4789151561354856977}, blockBody = BlockBody "EMPTY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-8378594247862146715), headerPrevHash = BlockHash (HeaderHash (-6659201155065996478)), headerSlot = SlotNo 28, headerBlockNo = BlockNo 32, headerBodyHash = BodyHash 4789151561354856977}, blockBody = BlockBody "EMPTY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-169687412501327813), headerPrevHash = BlockHash (HeaderHash (-8378594247862146715)), headerSlot = SlotNo 30, headerBlockNo = BlockNo 33, headerBodyHash = BodyHash (-4300753230511515293)}, blockBody = BlockBody "FYDS"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 6287810161833886734, headerPrevHash = BlockHash (HeaderHash (-169687412501327813)), headerSlot = SlotNo 30, headerBlockNo = BlockNo 34, headerBodyHash = BodyHash (-3194952396088205343)}, blockBody = BlockBody "HFDH"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 4679954912459070431, headerPrevHash = BlockHash (HeaderHash 6287810161833886734), headerSlot = SlotNo 31, headerBlockNo = BlockNo 35, headerBodyHash = BodyHash 4789151561354856977}, blockBody = BlockBody "EMPTY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-3106016354284443919), headerPrevHash = BlockHash (HeaderHash 4679954912459070431), headerSlot = SlotNo 32, headerBlockNo = BlockNo 36, headerBodyHash = BodyHash (-7438794600971277222)}, blockBody = BlockBody "CMAJ"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-1762699776291766253), headerPrevHash = BlockHash (HeaderHash (-3106016354284443919)), headerSlot = SlotNo 32, headerBlockNo = BlockNo 37, headerBodyHash = BodyHash (-78902357699939551)}, blockBody = BlockBody "OMBD"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-767019252039038623), headerPrevHash = BlockHash (HeaderHash (-1762699776291766253)), headerSlot = SlotNo 34, headerBlockNo = BlockNo 38, headerBodyHash = BodyHash (-5062662012111709994)}, blockBody = BlockBody "GKAX"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-5637659325550844901), headerPrevHash = BlockHash (HeaderHash (-767019252039038623)), headerSlot = SlotNo 35, headerBlockNo = BlockNo 39, headerBodyHash = BodyHash (-5083559330113325336)}, blockBody = BlockBody "GQGR"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 6517087232344273573, headerPrevHash = BlockHash (HeaderHash (-5637659325550844901)), headerSlot = SlotNo 36, headerBlockNo = BlockNo 40, headerBodyHash = BodyHash 5655422416846043554}, blockBody = BlockBody "VYEQ"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 2379843637725626457, headerPrevHash = BlockHash (HeaderHash 6517087232344273573), headerSlot = SlotNo 38, headerBlockNo = BlockNo 41, headerBodyHash = BodyHash 4789151561354856977}, blockBody = BlockBody "EMPTY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 8198884557753145275, headerPrevHash = BlockHash (HeaderHash 2379843637725626457), headerSlot = SlotNo 38, headerBlockNo = BlockNo 42, headerBodyHash = BodyHash 3777027746821666014}, blockBody = BlockBody "UQTG"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 3174737263014445702, headerPrevHash = BlockHash (HeaderHash 8198884557753145275), headerSlot = SlotNo 39, headerBlockNo = BlockNo 43, headerBodyHash = BodyHash 5669788635777410259}, blockBody = BlockBody "VNOY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-6298645420252920385), headerPrevHash = BlockHash (HeaderHash 3174737263014445702), headerSlot = SlotNo 40, headerBlockNo = BlockNo 44, headerBodyHash = BodyHash (-6821173630801010857)}, blockBody = BlockBody "BNKM"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-9077760516575866659), headerPrevHash = BlockHash (HeaderHash (-6298645420252920385)), headerSlot = SlotNo 41, headerBlockNo = BlockNo 45, headerBodyHash = BodyHash (-4320697271931661024)}, blockBody = BlockBody "FBAL"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-234160861388118505), headerPrevHash = BlockHash (HeaderHash (-9077760516575866659)), headerSlot = SlotNo 42, headerBlockNo = BlockNo 46, headerBodyHash = BodyHash (-572826070697621890)}, blockBody = BlockBody "LKHJ"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-1143977236685610684), headerPrevHash = BlockHash (HeaderHash (-234160861388118505)), headerSlot = SlotNo 42, headerBlockNo = BlockNo 47, headerBodyHash = BodyHash (-4305541603651428164)}, blockBody = BlockBody "FRYX"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-8690606911552283272), headerPrevHash = BlockHash (HeaderHash (-1143977236685610684)), headerSlot = SlotNo 43, headerBlockNo = BlockNo 48, headerBodyHash = BodyHash 4789151561354856977}, blockBody = BlockBody "EMPTY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 3338148184774960918, headerPrevHash = BlockHash (HeaderHash (-8690606911552283272)), headerSlot = SlotNo 44, headerBlockNo = BlockNo 49, headerBodyHash = BodyHash (-7439637926389925973)}, blockBody = BlockBody "CLZG"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-8536406153759081365), headerPrevHash = BlockHash (HeaderHash 3338148184774960918), headerSlot = SlotNo 45, headerBlockNo = BlockNo 50, headerBodyHash = BodyHash 2540200310713631440}, blockBody = BlockBody "SEBA"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 8925331030893873114, headerPrevHash = BlockHash (HeaderHash (-8536406153759081365)), headerSlot = SlotNo 46, headerBlockNo = BlockNo 51, headerBodyHash = BodyHash 5673446710963711350}, blockBody = BlockBody "VJLE"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-3588332081810274554), headerPrevHash = BlockHash (HeaderHash 8925331030893873114), headerSlot = SlotNo 47, headerBlockNo = BlockNo 52, headerBodyHash = BodyHash (-6318513198407266931)}, blockBody = BlockBody "EGYU"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 8036236910378077090, headerPrevHash = BlockHash (HeaderHash (-3588332081810274554)), headerSlot = SlotNo 47, headerBlockNo = BlockNo 53, headerBodyHash = BodyHash (-78890263072029090)}, blockBody = BlockBody "OMWH"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 1007326028982932726, headerPrevHash = BlockHash (HeaderHash 8036236910378077090), headerSlot = SlotNo 48, headerBlockNo = BlockNo 54, headerBodyHash = BodyHash 1928446334590363596}, blockBody = BlockBody "PNTU"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 7706344778471058708, headerPrevHash = BlockHash (HeaderHash 1007326028982932726), headerSlot = SlotNo 48, headerBlockNo = BlockNo 55, headerBodyHash = BodyHash (-7445356486367115931)}, blockBody = BlockBody "CBIZ"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 8046227379714939032, headerPrevHash = BlockHash (HeaderHash 7706344778471058708), headerSlot = SlotNo 49, headerBlockNo = BlockNo 56, headerBodyHash = BodyHash (-61713692419523037)}, blockBody = BlockBody "OSYE"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 8835981034579839003, headerPrevHash = BlockHash (HeaderHash 8046227379714939032), headerSlot = SlotNo 49, headerBlockNo = BlockNo 57, headerBodyHash = BodyHash 1182826817758898047}, blockBody = BlockBody "QKHT"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 615484714761720674, headerPrevHash = BlockHash (HeaderHash 8835981034579839003), headerSlot = SlotNo 50, headerBlockNo = BlockNo 58, headerBodyHash = BodyHash (-6828858117569063735)}, blockBody = BlockBody "BVTB"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-6946888157183811554), headerPrevHash = BlockHash (HeaderHash 615484714761720674), headerSlot = SlotNo 50, headerBlockNo = BlockNo 59, headerBodyHash = BodyHash 4789151561354856977}, blockBody = BlockBody "EMPTY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 4906175707123868200, headerPrevHash = BlockHash (HeaderHash (-6946888157183811554)), headerSlot = SlotNo 51, headerBlockNo = BlockNo 60, headerBodyHash = BodyHash 5054222652839891787}, blockBody = BlockBody "WSDN"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-9171138975173368304), headerPrevHash = BlockHash (HeaderHash 4906175707123868200), headerSlot = SlotNo 52, headerBlockNo = BlockNo 61, headerBodyHash = BodyHash 4789151561354856977}, blockBody = BlockBody "EMPTY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-8511284803975569067), headerPrevHash = BlockHash (HeaderHash (-9171138975173368304)), headerSlot = SlotNo 53, headerBlockNo = BlockNo 62, headerBodyHash = BodyHash (-6835534352174236344)}, blockBody = BlockBody "BQNJ"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-3073738949907974846), headerPrevHash = BlockHash (HeaderHash (-8511284803975569067)), headerSlot = SlotNo 55, headerBlockNo = BlockNo 63, headerBodyHash = BodyHash 4789151561354856977}, blockBody = BlockBody "EMPTY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-8386910132267310575), headerPrevHash = BlockHash (HeaderHash (-3073738949907974846)), headerSlot = SlotNo 56, headerBlockNo = BlockNo 64, headerBodyHash = BodyHash 3781031068659225217}, blockBody = BlockBody "UMSQ"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 5580664522661511703, headerPrevHash = BlockHash (HeaderHash (-8386910132267310575)), headerSlot = SlotNo 58, headerBlockNo = BlockNo 65, headerBodyHash = BodyHash 5060916479631115751}, blockBody = BlockBody "WZBY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-8029120831442175421), headerPrevHash = BlockHash (HeaderHash 5580664522661511703), headerSlot = SlotNo 59, headerBlockNo = BlockNo 66, headerBodyHash = BodyHash 4789151561354856977}, blockBody = BlockBody "EMPTY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 3913937602254152853, headerPrevHash = BlockHash (HeaderHash (-8029120831442175421)), headerSlot = SlotNo 60, headerBlockNo = BlockNo 67, headerBodyHash = BodyHash 1938018682823676199}, blockBody = BlockBody "PTRB"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-1282934301868054500), headerPrevHash = BlockHash (HeaderHash 3913937602254152853), headerSlot = SlotNo 61, headerBlockNo = BlockNo 68, headerBodyHash = BodyHash 2524081470247274992}, blockBody = BlockBody "STLN"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 1810292822992857481, headerPrevHash = BlockHash (HeaderHash (-1282934301868054500)), headerSlot = SlotNo 62, headerBlockNo = BlockNo 69, headerBodyHash = BodyHash 653365789432809018}, blockBody = BlockBody "NVMN"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-3782318953827077315), headerPrevHash = BlockHash (HeaderHash 1810292822992857481), headerSlot = SlotNo 63, headerBlockNo = BlockNo 70, headerBodyHash = BodyHash 4789151561354856977}, blockBody = BlockBody "EMPTY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-8262490009810830918), headerPrevHash = BlockHash (HeaderHash (-3782318953827077315)), headerSlot = SlotNo 66, headerBlockNo = BlockNo 71, headerBodyHash = BodyHash 8179693306856777308}, blockBody = BlockBody "ZTZS"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 6634768913544106750, headerPrevHash = BlockHash (HeaderHash (-8262490009810830918)), headerSlot = SlotNo 67, headerBlockNo = BlockNo 72, headerBodyHash = BodyHash (-570115774534649571)}, blockBody = BlockBody "LHIQ"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-3071948418829990572), headerPrevHash = BlockHash (HeaderHash 6634768913544106750), headerSlot = SlotNo 69, headerBlockNo = BlockNo 73, headerBodyHash = BodyHash 3782933223775651954}, blockBody = BlockBody "UOEV"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 574235327930959882, headerPrevHash = BlockHash (HeaderHash (-3071948418829990572)), headerSlot = SlotNo 70, headerBlockNo = BlockNo 74, headerBodyHash = BodyHash (-6824068644917522752)}, blockBody = BlockBody "BMRJ"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 4241363020223521474, headerPrevHash = BlockHash (HeaderHash 574235327930959882), headerSlot = SlotNo 71, headerBlockNo = BlockNo 75, headerBodyHash = BodyHash 660047521596122680}, blockBody = BlockBody "NOBD"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 5180641329206919492, headerPrevHash = BlockHash (HeaderHash 4241363020223521474), headerSlot = SlotNo 73, headerBlockNo = BlockNo 76, headerBodyHash = BodyHash 6172309430194371369}, blockBody = BlockBody "YGDR"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 156443077183015534, headerPrevHash = BlockHash (HeaderHash 5180641329206919492), headerSlot = SlotNo 74, headerBlockNo = BlockNo 77, headerBodyHash = BodyHash 5045450749071727433}, blockBody = BlockBody "WJPQ"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-2785873744845830961), headerPrevHash = BlockHash (HeaderHash 156443077183015534), headerSlot = SlotNo 75, headerBlockNo = BlockNo 78, headerBodyHash = BodyHash 655255849921325390}, blockBody = BlockBody "NPZY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-7059405090773489378), headerPrevHash = BlockHash (HeaderHash (-2785873744845830961)), headerSlot = SlotNo 76, headerBlockNo = BlockNo 79, headerBodyHash = BodyHash (-2462536914397276612)}, blockBody = BlockBody "KOMV"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-100613380702580732), headerPrevHash = BlockHash (HeaderHash (-7059405090773489378)), headerSlot = SlotNo 77, headerBlockNo = BlockNo 80, headerBodyHash = BodyHash 649696719130225821}, blockBody = BlockBody "NZZP"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-1072059194744106675), headerPrevHash = BlockHash (HeaderHash (-100613380702580732)), headerSlot = SlotNo 78, headerBlockNo = BlockNo 81, headerBodyHash = BodyHash (-575733179442044230)}, blockBody = BlockBody "LNZI"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 2078213555099420979, headerPrevHash = BlockHash (HeaderHash (-1072059194744106675)), headerSlot = SlotNo 79, headerBlockNo = BlockNo 82, headerBodyHash = BodyHash (-6813669463939984729)}, blockBody = BlockBody "BFBZ"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 514715113000451136, headerPrevHash = BlockHash (HeaderHash 2078213555099420979), headerSlot = SlotNo 80, headerBlockNo = BlockNo 83, headerBodyHash = BodyHash (-61688403652074186)}, blockBody = BlockBody "OSBM"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 2489894815258065418, headerPrevHash = BlockHash (HeaderHash 514715113000451136), headerSlot = SlotNo 81, headerBlockNo = BlockNo 84, headerBodyHash = BodyHash 4789151561354856977}, blockBody = BlockBody "EMPTY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-9219554255083953111), headerPrevHash = BlockHash (HeaderHash 2489894815258065418), headerSlot = SlotNo 82, headerBlockNo = BlockNo 85, headerBodyHash = BodyHash 2548950224249231552}, blockBody = BlockBody "SJBB"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-7946966273777578849), headerPrevHash = BlockHash (HeaderHash (-9219554255083953111)), headerSlot = SlotNo 82, headerBlockNo = BlockNo 86, headerBodyHash = BodyHash 3047657912340313962}, blockBody = BlockBody "REEY"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 8570523866722540363, headerPrevHash = BlockHash (HeaderHash (-7946966273777578849)), headerSlot = SlotNo 83, headerBlockNo = BlockNo 87, headerBodyHash = BodyHash (-3708977380668867614)}, blockBody = BlockBody "IUZU"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-2025225923125225756), headerPrevHash = BlockHash (HeaderHash 8570523866722540363), headerSlot = SlotNo 84, headerBlockNo = BlockNo 88, headerBodyHash = BodyHash 2525014955619436915}, blockBody = BlockBody "SUQK"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 2165853342051868555, headerPrevHash = BlockHash (HeaderHash (-2025225923125225756)), headerSlot = SlotNo 85, headerBlockNo = BlockNo 89, headerBodyHash = BodyHash 1186799353270867335}, blockBody = BlockBody "QOWI"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-4725372417266741475), headerPrevHash = BlockHash (HeaderHash 2165853342051868555), headerSlot = SlotNo 86, headerBlockNo = BlockNo 90, headerBodyHash = BodyHash 649664833293007826}, blockBody = BlockBody "NZGT"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 8165187270093753717, headerPrevHash = BlockHash (HeaderHash (-4725372417266741475)), headerSlot = SlotNo 87, headerBlockNo = BlockNo 91, headerBodyHash = BodyHash 2536503752620342818}, blockBody = BlockBody "SYHM"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-1657835997199340279), headerPrevHash = BlockHash (HeaderHash 8165187270093753717), headerSlot = SlotNo 88, headerBlockNo = BlockNo 92, headerBodyHash = BodyHash (-2457738645652709808)}, blockBody = BlockBody "KHYQ"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-3521356196696222797), headerPrevHash = BlockHash (HeaderHash (-1657835997199340279)), headerSlot = SlotNo 89, headerBlockNo = BlockNo 93, headerBodyHash = BodyHash (-4305547101209569244)}, blockBody = BlockBody "FRRA"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash (-2974053371854879401), headerPrevHash = BlockHash (HeaderHash (-3521356196696222797)), headerSlot = SlotNo 90, headerBlockNo = BlockNo 94, headerBodyHash = BodyHash (-5064448718507174416)}, blockBody = BlockBody "GERU"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 303296124194095639, headerPrevHash = BlockHash (HeaderHash (-2974053371854879401)), headerSlot = SlotNo 91, headerBlockNo = BlockNo 95, headerBodyHash = BodyHash (-6825986193196744394)}, blockBody = BlockBody "BKRZ"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 1831111038898129916, headerPrevHash = BlockHash (HeaderHash 303296124194095639), headerSlot = SlotNo 92, headerBlockNo = BlockNo 96, headerBodyHash = BodyHash (-58817578791382785)}, blockBody = BlockBody "OVGV"}
            :>  Block {blockHeader = BlockHeader {headerHash = HeaderHash 8151795743315401931, headerPrevHash = BlockHash (HeaderHash 1831111038898129916), headerSlot = SlotNo 94, headerBlockNo = BlockNo 97, headerBodyHash = BodyHash 2535406440015577492}, blockBody = BlockBody "SXHR"}))
      Positive {getPositive = SmallDelay {getSmallDelay = 0.025}}

newtype PeerGSVT = PeerGSVT {
      unPeerGSVT :: PeerGSV
    } deriving Show

instance Arbitrary PeerGSVT where
    arbitrary = do
        Delay gIn <- resize 1000 arbitrary
        Delay gOut <- resize 1000 arbitrary
        let gsvIn  = ballisticGSV gIn  2e-6 (degenerateDistribution 0)
            gsvOut = ballisticGSV gOut 2e-6 (degenerateDistribution 0)
        return $ PeerGSVT $ PeerGSV (Time 0) gsvOut gsvIn

    shrink (PeerGSVT (PeerGSV ts (GSV gOut sOut vOut) (GSV gIn sIn vIn))) =
        [PeerGSVT (PeerGSV ts  (GSV gOut' sOut vOut) (GSV gIn' sIn vIn))
         | (Delay gIn', Delay gOut') <- shrink (Delay gIn, Delay gOut)]


-- | Check that comparePeerGSV satisfies Ord axioms
prop_comparePeerGSV :: Int -> Int -> Int -> PeerGSVT -> PeerGSVT -> Bool -> Bool -> Property
prop_comparePeerGSV salt pa pb (PeerGSVT a) (PeerGSVT b) aActive bActive =
    let peerSet = case (aActive, bActive) of
                       (False, False) -> Set.empty
                       (True, False)  -> Set.singleton pa
                       (False, True)  -> Set.singleton pb
                       (True, True)   -> Set.fromList [pa, pb] in
    case comparePeerGSV peerSet salt (a, pa) (b, pb) of
         LT -> comparePeerGSV peerSet salt (b, pb) (a, pa) === GT
         GT -> comparePeerGSV peerSet salt (b, pb) (a, pa) === LT
         EQ -> comparePeerGSV peerSet salt (b, pb) (a, pa) === EQ

-- | Check that identical peers are equal
prop_comparePeerGSVEq :: Int -> Int -> PeerGSVT -> Bool -> Property
prop_comparePeerGSVEq salt p (PeerGSVT a) aActive =
    let peerSet = if aActive then Set.singleton p
                             else Set.empty in
    comparePeerGSV peerSet salt (a, p) (a, p) === EQ


--
-- Trace utils
--

dynamicTracer :: Typeable a => Tracer (IOSim s) a
dynamicTracer = Tracer traceM

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.Ouroboros.Network.BlockFetch
  ( PeerGSVT (..)
  , tests
  ) where

import Test.ChainGenerators (TestChainFork (..))
import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty)

import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Typeable (Typeable)

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Exception (AssertionFailed (..), throw)
import Control.Monad (unless, void)
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTimer.SI
import Control.Monad.IOSim
import Control.Tracer (Tracer (Tracer), contramap, nullTracer)

import Ouroboros.Network.ControlMessage (ControlMessage (..))
import Ouroboros.Network.DeltaQ
--TODO: could re-export some of the trace types from more convenient places:
import Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import Ouroboros.Network.AnchoredFragment qualified as AnchoredFragment
import Ouroboros.Network.Block
import Ouroboros.Network.BlockFetch
import Ouroboros.Network.BlockFetch.ClientRegistry
import Ouroboros.Network.BlockFetch.ClientState
import Ouroboros.Network.BlockFetch.Decision.Trace (TraceDecisionEvent)
import Ouroboros.Network.BlockFetch.DeltaQ
import Ouroboros.Network.BlockFetch.Examples
import Ouroboros.Network.Driver (TraceSendRecv)
import Ouroboros.Network.Mock.Chain qualified as Chain
import Ouroboros.Network.Mock.ConcreteBlock
import Ouroboros.Network.NodeToNode.Version (NodeToNodeVersion)
import Ouroboros.Network.Protocol.BlockFetch.Type (BlockFetch)

import Test.Ouroboros.Network.Utils


--
-- The list of all tests
--

tests :: TestTree
tests = testGroup "BlockFetch"
  [ testGroup "BulkSync"
    [ testProperty "static chains without overlap"
                   prop_blockFetchStaticNoOverlapPraos

    , testProperty "static chains with overlap"
                   prop_blockFetchStaticWithOverlapPraos

    --TODO: test where for any given delta-Q, check that we do achieve full
    -- pipelining to keep the server busy and get decent enough batching of
    -- requests (testing the high/low watermark mechanism).
    , testProperty "termination"
                   prop_terminatePraos
    ]
  , testGroup "Genesis"
    [ testProperty "static chains without overlap"
                   prop_blockFetchStaticNoOverlapGenesis

    , testProperty "static chains with overlap"
                   prop_blockFetchStaticWithOverlapGenesis

    , testProperty "termination"
                   prop_terminateGenesis
    ]
  , testCaseSteps "bracketSyncWithFetchClient"
                  unit_bracketSyncWithFetchClient
  , testProperty "compare comparePeerGSV" prop_comparePeerGSV
  , testProperty "eq comparePeerGSV" prop_comparePeerGSVEq
  ]


--
-- Properties
--

prop_blockFetchStaticNoOverlapPraos :: TestChainFork -> Property
prop_blockFetchStaticNoOverlapPraos =
    prop_blockFetchStaticNoOverlap (PraosFetchMode FetchModeBulkSync)

prop_blockFetchStaticNoOverlapGenesis :: TestChainFork -> Property
prop_blockFetchStaticNoOverlapGenesis =
    prop_blockFetchStaticNoOverlap FetchModeGenesis

-- | In this test we have two candidates chains that are static throughout the
-- run. The two chains share some common prefix (genesis in the degenerate
-- case).
--
-- With a Praos fetch mode, the test runs the block fetch logic to download all
-- blocks of both chain candidates.
--
-- With the Genesis fetch mode, the test runs the block fetch logic to download all
-- blocks of the longest candidate chain (either of them if they are of equal
-- length).
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
-- * 'tracePropertyBlocksRequestedAndReceivedPerPeerPraos'
-- * 'tracePropertyClientStateSanity'
-- * 'tracePropertyInFlight'
--
prop_blockFetchStaticNoOverlap :: FetchMode -> TestChainFork -> Property
prop_blockFetchStaticNoOverlap fetchMode (TestChainFork common fork1 fork2) =
    let trace = selectTraceEventsDynamic (runSimTrace simulation)

     in counterexample ("\nTrace:\n" ++ unlines (map show trace)) $

        -- For fetch reqs added and received, we observe exactly the sequence
        -- of blocks we expect, which is the whole fork suffix.
        case fetchMode of
          FetchModeGenesis ->
            tracePropertyBlocksRequestedAndReceivedPerPeerGenesis fork1'' fork2'' trace
          PraosFetchMode{} ->
            tracePropertyBlocksRequestedAndReceivedPerPeerPraos fork1'' fork2'' trace

        -- state sanity check
   .&&. property (tracePropertyClientStateSanity trace)

        -- check in-flight requests
   .&&. tracePropertyInFlight trace

  where
    simulation :: IOSim s ()
    simulation =
      blockFetchExample1
        fetchMode
        (contramap TraceFetchDecision       dynamicTracer)
        (contramap TraceFetchClientState    dynamicTracer)
        (contramap TraceFetchClientSendRecv dynamicTracer)
        Nothing Nothing
        common' forks

    -- TODO: consider making a specific generator for anchored fragment forks
    common' = chainToAnchoredFragment common
    fork1'  = chainToAnchoredFragment fork1
    fork2'  = chainToAnchoredFragment fork2
    forks   = [fork1', fork2']
    -- And just the extensions
    Just (_, _, fork1'', fork2'') = AnchoredFragment.intersect fork1' fork2'

prop_blockFetchStaticWithOverlapPraos :: TestChainFork -> Property
prop_blockFetchStaticWithOverlapPraos =
    prop_blockFetchStaticWithOverlap (PraosFetchMode FetchModeBulkSync)

prop_blockFetchStaticWithOverlapGenesis :: TestChainFork -> Property
prop_blockFetchStaticWithOverlapGenesis =
    prop_blockFetchStaticWithOverlap FetchModeGenesis

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
-- * 'tracePropertyBlocksRequestedAndReceivedAllPeersPraos'
-- * 'tracePropertyNoDuplicateBlocksBetweenPeers'
-- * 'tracePropertyClientStateSanity'
-- * 'tracePropertyInFlight'
--
-- TODO: 'prop_blockFetchBulkSyncStaticWithOverlap' fails if we introduce delays. issue #2622
--
prop_blockFetchStaticWithOverlap :: FetchMode -> TestChainFork -> Property
prop_blockFetchStaticWithOverlap fetchMode (TestChainFork _common fork1 fork2) =
    let trace = selectTraceEventsDynamic (runSimTrace simulation)

     in counterexample ("\nTrace:\n" ++ unlines (map show trace)) $

        -- For fetch reqs added and received, between the two peers we observe
        -- the set of blocks we expect, which is the union of the two chains.
        case fetchMode of
          FetchModeGenesis ->
            tracePropertyBlocksRequestedAndReceivedAllPeersGenesis fork1' fork2' trace
          PraosFetchMode{} ->
            tracePropertyBlocksRequestedAndReceivedAllPeersPraos fork1' fork2' trace

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
        fetchMode
        (contramap TraceFetchDecision       dynamicTracer)
        (contramap TraceFetchClientState    dynamicTracer)
        (contramap TraceFetchClientSendRecv dynamicTracer)
        Nothing Nothing
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
     TraceFetchDecision       (TraceDecisionEvent Int BlockHeader)
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
-- This property is stronger than 'tracePropertyBlocksRequestedAndReceivedAllPeersPraos'
-- since it works with sequences rather than sets and for each chain
-- individually rather than both chains together. It only holds for the
-- situation where the suffixes of the chains that need to be fetched are
-- disjoint, sharing no common prefix.
--
-- It turns out that no duplicates part is not trivial. Earlier versions of the
-- block fetch logic did not satisfy this in all cases.
--
tracePropertyBlocksRequestedAndReceivedPerPeerPraos
  :: AnchoredFragment Block
  -> AnchoredFragment Block
  -> [Example1TraceEvent]
  -> Property
tracePropertyBlocksRequestedAndReceivedPerPeerPraos fork1 fork2 es =
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
-- received by the fetch clients; check that the ordered sequence of blocks
-- requested and completed by both fetch clients is exactly the sequence
-- expected. The expected sequence is exactly the longest chain suffix, or
-- either of them if they are of equal length.
--
-- This property is stronger than 'tracePropertyBlocksRequestedAndReceivedAllPeersGenesis'
-- since it works with sequences rather than sets and for each chain
-- individually rather than both chains together. It only holds for the
-- situation where the suffixes of the chains that need to be fetched are
-- disjoint, sharing no common prefix.
--
-- It turns out that no duplicates part is not trivial. Earlier versions of the
-- block fetch logic did not satisfy this in all cases.
--
tracePropertyBlocksRequestedAndReceivedPerPeerGenesis
  :: AnchoredFragment Block
  -> AnchoredFragment Block
  -> [Example1TraceEvent]
  -> Property
tracePropertyBlocksRequestedAndReceivedPerPeerGenesis fork1 fork2 es =
       counterexample "should request the expected blocks"
         (disjoin $ map (requestedFetchPoints ===) requiredFetchPoints)
  .&&. counterexample "should receive the expected blocks"
         (disjoin $ map (receivedFetchPoints ===) requiredFetchPoints)
  where
    requiredFetchPoints =
      if AnchoredFragment.length fork1 == AnchoredFragment.length fork2
        then [ requiredFetchPointsFor 1 fork1
             , requiredFetchPointsFor 2 fork2
             , Map.union (requiredFetchPointsFor 1 fork1) (requiredFetchPointsFor 2 fork2)
             ]
        else if AnchoredFragment.length fork1 < AnchoredFragment.length fork2
          then [requiredFetchPointsFor 2 fork2]
          else [requiredFetchPointsFor 1 fork1]

    requiredFetchPointsFor peer fork =
      Map.fromList [ (peer, points) | let points = chainPoints fork
                                    , not $ Prelude.null points ]

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
-- This property is weaker than 'tracePropertyBlocksRequestedAndReceivedPerPeerPraos'
-- since it does not involve order or frequency, but it holds for the general
-- case of multiple chains with common prefixes.
--
tracePropertyBlocksRequestedAndReceivedAllPeersPraos
  :: AnchoredFragment Block
  -> AnchoredFragment Block
  -> [Example1TraceEvent]
  -> Property
tracePropertyBlocksRequestedAndReceivedAllPeersPraos fork1 fork2 es =
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
-- blocks in the 'FetchRequest's added by the decision logic and the blocks
-- received by the fetch clients; check that the set of all blocks requested
-- across the two peers is the set of blocks we expect, and similarly for the
-- set of all blocks received. The expected set of blocks is the block of the
-- longest candidate chain, or either of them if they have the same size.
--
-- This property is weaker than 'tracePropertyBlocksRequestedAndReceivedPerPeerGenesis'
-- since it does not involve order or frequency, but it holds for the general
-- case of multiple chains with common prefixes.
--
tracePropertyBlocksRequestedAndReceivedAllPeersGenesis
  :: AnchoredFragment Block
  -> AnchoredFragment Block
  -> [Example1TraceEvent]
  -> Property
tracePropertyBlocksRequestedAndReceivedAllPeersGenesis fork1 fork2 es =
       counterexample "should request the expected blocks"
         (disjoin $ map (requestedFetchPoints ===) requiredFetchPoints)
  .&&. counterexample "should receive the expected blocks"
         (disjoin $ map (receivedFetchPoints ===) requiredFetchPoints)
  where
    requiredFetchPoints =
      if AnchoredFragment.length fork1 == AnchoredFragment.length fork2
        then [ requiredFetchPointsFor fork1
             , requiredFetchPointsFor fork2
             , Set.union (requiredFetchPointsFor fork1) (requiredFetchPointsFor fork2)
             ]
        else if AnchoredFragment.length fork1 < AnchoredFragment.length fork2
          then [requiredFetchPointsFor fork2]
          else [requiredFetchPointsFor fork1]

    requiredFetchPointsFor fork =
      Set.fromList $ chainPoints fork

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
    hasDupes = not . any ((>1) . length)  . List.group . List.sort

    requiredFetchPoints =
      List.nub (chainPoints fork1 ++ chainPoints fork2)

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
          ("tracePropertyInFlight: reqsInFlight = " ++ show reqsInFlight ++ " ≠ 0")
          False
      | otherwise
      = property True


--
-- Unit tests
--

unit_bracketSyncWithFetchClient :: (String -> IO ()) -> Assertion
unit_bracketSyncWithFetchClient step = do

    step "Starting fetch before sync"

    checkResultA =<< case runSimStrictShutdown (testSkeleton
                            (\action -> threadDelay 0.1 >> action (threadDelay 0.2))
                            (\action -> threadDelay 0.2 >> action (threadDelay 0.2))
                            (\action -> threadDelay 0.1 >> action (threadDelay 1.1))) of
                          Left e  -> error $ "sim failed with " <> show e
                          Right r -> return r

    step "Start and kill fetch before sync"
    checkResultB =<< case runSimStrictShutdown (testSkeleton
                            (\action -> action (threadDelay 3))
                            (\_action -> threadDelay 3)
                            (\action -> threadDelay 0.1 >> action (threadDelay 1.1))) of
                          Left e  -> error $ "sim failed with " <> show e
                          Right r -> return r

    step "Starting sync before fetch"
    checkResultA =<< case runSimStrictShutdown (testSkeleton
                            (\action -> threadDelay 0.2 >> action (threadDelay 0.2))
                            (\action -> threadDelay 0.1 >> action (threadDelay 0.2))
                            (\action -> threadDelay 0.1 >> action (threadDelay 0.5))) of
                          Left e  -> error $ "sim failed with " <> show e
                          Right r -> return r

    step "Stopping fetch before sync"
    checkResultD =<< case runSimStrictShutdown (testSkeleton
                            (\action -> action (threadDelay 0.1))
                            (\action -> action (threadDelay 10))
                            (\action -> threadDelay 0.1 >> action (threadDelay 300))) of
                          Left e  -> error $ "sim failed with " <> show e
                          Right r -> return r

    step "Stopping fetch before sync, sync timeout"
    checkResultC =<< case runSimStrictShutdown (testSkeleton
                            (\action -> action (threadDelay 0.1))
                            (\action -> action (threadDelay 600))
                            (\action -> threadDelay 0.1 >> action (threadDelay 400))) of
                          Left e  -> error $ "sim failed with " <> show e
                          Right r -> return r

    step "Stopping fetch before sync, keepalive exits"
    checkResultE =<< case runSimStrictShutdown (testSkeleton
                            (\action -> action (threadDelay 0.1))
                            (\action -> action (threadDelay 600))
                            (\action -> threadDelay 0.1 >> action (threadDelay 1))) of
                          Left e  -> error $ "sim failed with " <> show e
                          Right r -> return r

    step "Stopping sync before fetch"
    checkResultD =<< case runSimStrictShutdown (testSkeleton
                            (\action -> action (threadDelay 1.0))
                            (\action -> action (threadDelay 0.1))
                            (\action -> threadDelay 0 >> action (threadDelay 1.5))) of
                          Left e  -> error $ "sim failed with " <> show e
                          Right r -> return r

    step "Exception in fetch"
    checkResultB =<< case runSimStrictShutdown (testSkeleton
                            (\action -> action (threadDelay 0.1 >> throwIO AsyncCancelled))
                            (\action -> action (threadDelay 0.2))
                            (\action -> threadDelay 0.1 >> action (threadDelay 0.3))) of
                          Left e  -> error $ "sim failed with " <> show e
                          Right r -> return r

    step "Exception in sync"
    checkResultC =<< case runSimStrictShutdown (testSkeleton
                            (\action -> action (threadDelay 0.2))
                            (\action -> action (threadDelay 0.1 >> throwIO AsyncCancelled))
                            (\action -> threadDelay 0.1 >> action (threadDelay 0.3))) of
                          Left e  -> error $ "sim failed with " <> show e
                          Right r -> return r

    step "Exception in keepalive"
    checkResultE =<< case runSimStrictShutdown (testSkeleton
                            (\action -> action (threadDelay 0.2))
                            (\action -> action (threadDelay 0.2))
                            (\action -> action (threadDelay 0.1 >> throwIO AsyncCancelled))) of
                          Left e  -> error $ "sim failed with " <> show e
                          Right r -> return r

    step "Keep alive kills fetch"
    checkResultE =<< case runSimStrictShutdown (testSkeleton
                            (\action -> threadDelay 0.1 >> action (threadDelay 60))
                            (\action -> threadDelay 0.1 >> action (threadDelay 60))
                            (\action -> threadDelay 0.1 >> action (threadDelay 1.3))) of
                          Left e  -> error $ "sim failed with " <> show e
                          Right r -> return r

    step "Deadlock without keep alive"
    case runSimStrictShutdown (testSkeleton
           (\action -> threadDelay 0.1 >> action (threadDelay 60))
           (\action -> threadDelay 0.1 >> action (threadDelay 60))
           (\_action -> threadDelay 3.1 )) of
         Left (FailureDeadlock _) -> return ()
         Left e                   -> error $ "sim failed with " <> show e
         Right _                  -> error "unexpected success"

    step "Deadlock without fetch"
    case runSimStrictShutdown (testSkeleton
           (\_action -> threadDelay 3)
           (\action -> threadDelay 0.1 >> action (threadDelay 60))
           (\action -> threadDelay 0.1 >> action (threadDelay 1.3))) of
         Left (FailureDeadlock _) -> return ()
         Left e                   -> error $ "sim failed with " <> show e
         Right _                  -> error "unexpected success"


    return ()

  where
    dummyPolicy :: forall b h m. (MonadSTM m) => STM m (FetchClientPolicy h b m)
    dummyPolicy =
      let addFetchedBlock _ _ = return ()
          forgeTime _ = read "2000-01-01 00:00:00 UTC"
          bfSize _ = 1024
          matchesHeader _ _ = True in
      pure $ FetchClientPolicy
          bfSize
          matchesHeader
          addFetchedBlock
          forgeTime


    -- Fetch success
    checkResultA (Right _, _) = return ()
    checkResultA (Left e, _)  = assertFailure $ "unexpected fetch failure " ++ show e

    -- Fetch Failure
    checkResultB (Right _, _) = assertFailure "unexpected fetch success"
    checkResultB (Left _, _)  = return ()

    -- Fetch success and Sync failure
    checkResultC (Right _, Left _)  = return ()
    checkResultC (Right _, Right _) = assertFailure "unexpected sync success"
    checkResultC (Left e, _)        = assertFailure $ "unexpected fetch failure " ++ show e

    -- Fetch and Sync sucess
    checkResultD (Right _, Right _) = return ()
    checkResultD (Left e, _)        = assertFailure $ "unexpected fetch failure " ++ show e
    checkResultD (Right _, Left e)  = assertFailure $ "unexpected sync failure " ++ show e

    -- Fetch and Sync failure
    checkResultE (Left _, Left _) = return ()
    checkResultE _                = assertFailure "unexpected success"

    testSkeleton :: forall m a b d.
                    (MonadAsync m, MonadDelay m, MonadFork m, MonadMask m,
                     MonadThrow (STM m), MonadTimer m)
                 => ((forall c. m c -> m c) -> m a)
                 -> ((forall c. m c -> m c) -> m b)
                 -> ((forall c. m c -> m c) -> m d)
                 -> m (Either SomeException a, Either SomeException b)
    testSkeleton withFetchTestAction withSyncTestAction withKeepAliveTestAction = do
      registry <- newFetchClientRegistry
      setFetchClientContext registry nullTracer dummyPolicy

      fetchStatePeerChainsVar <- newTVarIO Map.empty

      let peer  = "thepeer"
          fetch :: m a
          fetch = withFetchTestAction $ \body ->
                    bracketFetchClient registry (maxBound @NodeToNodeVersion) peer $ \_ ->
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

          keep :: m d
          keep  = withKeepAliveTestAction $ \body ->
                    bracketKeepAliveClient registry peer $ const body

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

      withAsync     keep  $ \keepAsync  ->
        withAsync   fetch $ \fetchAsync ->
          withAsync sync  $ \syncAsync  ->
            withAsync (logic (Map.empty, Map.empty)) $ \logicAsync -> do
              void $ atomically $ do
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

              fetchRes <- waitCatch fetchAsync
              syncRes  <- waitCatch syncAsync
              void $ waitCatch keepAsync
              atomically $ do
                fr <- readTVar $ fcrFetchRegistry registry
                sr <- readTVar $ fcrSyncRegistry  registry
                dr <- readTVar $ fcrDqRegistry    registry
                kr <- readTVar $ fcrKeepRegistry  registry
                yr <- readTVar $ fcrDying         registry
                if and [Map.null fr, Map.null sr, Map.null dr, Map.null kr, Set.null yr]
                   then return ()
                   else error "state leak"
              return (fetchRes, syncRes)

prop_terminatePraos :: TestChainFork -> Positive SmallDelay -> Property
prop_terminatePraos = prop_terminate (PraosFetchMode FetchModeBulkSync)

prop_terminateGenesis :: TestChainFork -> Positive SmallDelay -> Property
prop_terminateGenesis = prop_terminate FetchModeGenesis

-- | Check that the client can terminate using `ControlMessage` mechanism.
--
-- The 'awaitDelay' of @100 * delay@ is a bit arbitrary.  It would be nicer to
-- make a proper calculation what should it be.  At the moment this test shows
-- that the block fetch protocol can exit within some large time limit.
--
prop_terminate :: FetchMode -> TestChainFork -> Positive SmallDelay -> Property
prop_terminate fetchMode (TestChainFork _commonChain forkChain _forkChain) (Positive (SmallDelay delay)) =
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
            let awaitDelay = delay * 100 + 500
            threadDelay awaitDelay)
          (do
            threadId <- myThreadId
            labelThread threadId "block-fetch"
            blockFetchExample0
              fetchMode
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

-- TODO: moved to some shared place (cannot be moved to
-- `ouroboros-network-testing` which doesn't depend on `ouroboros-network`)
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

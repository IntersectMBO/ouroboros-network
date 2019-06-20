{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE RankNTypes                 #-}

module Test.Ouroboros.Network.BlockFetch (tests) where

import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck (testProperty)
import           Test.ChainGenerators (TestChainFork(..))

import           Data.List
import qualified Data.Set as Set
import           Data.Set (Set)
import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.Typeable (Typeable)

import           Control.Monad (unless)
import           Control.Monad.IOSim
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTimer
import           Control.Tracer (Tracer(Tracer), contramap, nullTracer)
import           Control.Exception (SomeException, AssertionFailed(..))

--TODO: could re-export some of the trace types from more convenient places:
import           Ouroboros.Network.Block
import           Network.TypedProtocol.Driver (TraceSendRecv)
import qualified Ouroboros.Network.Chain            as Chain
import qualified Ouroboros.Network.ChainFragment    as ChainFragment
import qualified Ouroboros.Network.AnchoredFragment as AnchoredFragment
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import           Ouroboros.Network.Testing.ConcreteBlock
import           Ouroboros.Network.Protocol.BlockFetch.Type (BlockFetch)
import           Ouroboros.Network.BlockFetch
import           Ouroboros.Network.BlockFetch.ClientState
import           Ouroboros.Network.BlockFetch.ClientRegistry
import           Ouroboros.Network.BlockFetch.Examples


--
-- The list of all tests
--

tests :: TestTree
tests = testGroup "BlockFetch"
  [ testProperty "static chains without overlap"
                 prop_blockFetchStaticNoOverlap

  , testProperty "static chains with overlap"
                 prop_blockFetchStaticWithOverlap

  , testCaseSteps "bracketSyncWithFetchClient"
                  unit_bracketSyncWithFetchClient
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
--
prop_blockFetchStaticNoOverlap :: TestChainFork -> Property
prop_blockFetchStaticNoOverlap (TestChainFork common fork1 fork2) =
    let trace = selectTraceEventsDynamic $
                runSimTrace $
                  blockFetchExample1
                    (contramap TraceFetchDecision       dynamicTracer)
                    (contramap TraceFetchClientState    dynamicTracer)
                    (contramap TraceFetchClientSendRecv dynamicTracer)
                    common' forks

     in counterexample ("\nTrace:\n" ++ unlines (map show trace)) $

        -- For fetch reqs added and received, we observe exactly the sequence
        -- of blocks we expect, which is the whole fork suffix.
        tracePropertyBlocksRequestedAndRecievedPerPeer fork1'' fork2'' trace

        -- state sanity check
   .&&. property (tracePropertyClientStateSanity trace)

  where
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
--
prop_blockFetchStaticWithOverlap :: TestChainFork -> Property
prop_blockFetchStaticWithOverlap (TestChainFork _common fork1 fork2) =
    let trace = selectTraceEventsDynamic $
                runSimTrace $
                  blockFetchExample1
                    (contramap TraceFetchDecision       dynamicTracer)
                    (contramap TraceFetchClientState    dynamicTracer)
                    (contramap TraceFetchClientSendRecv dynamicTracer)
                    (AnchoredFragment.Empty Chain.genesisPoint) forks

     in counterexample ("\nTrace:\n" ++ unlines (map show trace)) $

        -- For fetch reqs added and received, between the two peers we observe
        -- the set of blocks we expect, which is the union of the two chains.
        tracePropertyBlocksRequestedAndRecievedAllPeers fork1' fork2' trace

        -- For fetch reqs added, the set of blocks added for the two peers
        -- should not intersect
   .&&. tracePropertyNoDuplicateBlocksBetweenPeers fork1' fork2' trace

        -- state sanity check
   .&&. property (tracePropertyClientStateSanity trace)

  where
    -- TODO: consider making a specific generator for anchored fragment forks
    fork1'  = chainToAnchoredFragment fork1
    fork2'  = chainToAnchoredFragment fork2
    forks   = [fork1', fork2']

chainToAnchoredFragment :: Chain.Chain Block -> AnchoredFragment Block
chainToAnchoredFragment =
    AnchoredFragment.fromNewestFirst Chain.genesisPoint
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
                                (TraceSendRecv (BlockFetch Block)))

instance Show Example1TraceEvent where
  show (TraceFetchDecision       x) = "TraceFetchDecision " ++ show x
  show (TraceFetchClientState    x) = show x
  show (TraceFetchClientSendRecv x) = show (fmap (\_ -> "msg") x)


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
        [ (peer, map blockPoint (ChainFragment.toOldestFirst fragment))
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
            (TraceLabelPeer peer (CompletedBlockFetch pt _ _ _)) <- es
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
        , block    <- ChainFragment.toOldestFirst fragment
        ]

    receivedFetchPoints :: Set (Point BlockHeader)
    receivedFetchPoints =
      Set.fromList
        [ pt
        | TraceFetchClientState
            (TraceLabelPeer _ (CompletedBlockFetch pt _ _ _)) <- es
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
                     . ChainFragment.toOldestFirst
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
          PeerFetchStatusReady _ -> Set.size peerFetchBlocksInFlight <  3
          PeerFetchStatusBusy    -> Set.size peerFetchBlocksInFlight == 3
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


--
-- Unit tests
--

unit_bracketSyncWithFetchClient :: (String -> IO ()) -> Assertion
unit_bracketSyncWithFetchClient step = do

    step "Starting fetch before sync"
    checkResult =<< testSkeleton
      (\action -> threadDelay 0.01 >> action (threadDelay 0.02))
      (\action -> threadDelay 0.02 >> action (threadDelay 0.02))

    step "Starting sync before fetch"
    checkResult =<< testSkeleton
      (\action -> threadDelay 0.02 >> action (threadDelay 0.02))
      (\action -> threadDelay 0.01 >> action (threadDelay 0.02))

    step "Stopping fetch before sync"
    checkResult =<< testSkeleton
      (\action -> action (threadDelay 0.01))
      (\action -> action (threadDelay 0.02))

    step "Stopping sync before fetch"
    checkResult =<< testSkeleton
      (\action -> action (threadDelay 0.02))
      (\action -> action (threadDelay 0.01))

    step "Exception in fetch"
    Left (Left _) <- testSkeleton
      (\action -> action (threadDelay 0.01 >> throwM AsyncCancelled))
      (\action -> action (threadDelay 0.02))

    step "Exception in sync"
    Right (Left _) <- testSkeleton
      (\action -> action (threadDelay 0.02))
      (\action -> action (threadDelay 0.01 >> throwM AsyncCancelled))

    return ()

  where
    checkResult (Left  (Right _)) = return ()
    checkResult (Right (Right _)) = return ()
    checkResult _                 = assertFailure "unexpected result"

    testSkeleton :: forall m a b.
                    (MonadAsync m, MonadFork m, MonadSTM m, MonadTimer m,
                     MonadThrow m, MonadThrow (STM m))
                 => ((forall c. m c -> m c) -> m a)
                 -> ((forall c. m c -> m c) -> m b)
                 -> m (Either (Either SomeException a)
                              (Either SomeException b))
    testSkeleton withFetchTestAction withSyncTestAction = do
      registry <- newFetchClientRegistry
      setFetchClientContext registry nullTracer (error "no policy")

      fetchStatePeerChainsVar <- newTVarM Map.empty

      let peer  = "thepeer"
          fetch :: m a
          fetch = withFetchTestAction $ \body ->
                    bracketFetchClient registry peer $ \_ ->
                      body

          sync :: m b
          sync  = withSyncTestAction $ \body ->
                    bracketSyncWithFetchClient registry peer $
                      bracket_
                        (atomically (modifyTVar' fetchStatePeerChainsVar
                                                 (Map.insert peer ())))
                        (atomically (modifyTVar' fetchStatePeerChainsVar
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
              throwM (AssertionFailed "detected state mismatch")

            logic fingerprint'

      withAsync     fetch $ \fetchAsync ->
        withAsync   sync  $ \syncAsync  ->
          withAsync (logic (Map.empty, Map.empty)) $ \logicAsync -> do
            res <- atomically $ do
              res <- pollSTM logicAsync
              case res of
                Nothing         -> waitEitherCatchSTM fetchAsync syncAsync
                Just (Left  e)  -> throwM e
                Just (Right ()) -> error "impossible"

            threadDelay 0.01
            -- give the logic thread a chance to detect any final problems
            atomically $ do
              x <- pollSTM logicAsync
              case x of
                Just (Left e) -> throwM e
                _             -> return ()
            return res


--
-- Trace utils
--

dynamicTracer :: Typeable a => Tracer (SimM s) a
dynamicTracer = Tracer traceM

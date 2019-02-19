{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE GADTs               #-}

module Test.Ouroboros.Network.Node where

import           Control.Monad (forM, forM_, replicateM, filterM, unless)
import           Control.Monad.State (execStateT, lift, modify')
import           Data.Array
import           Data.Fixed (Micro)
import           Data.Functor (void)
import           Data.Graph
import           Data.List (foldl')
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (isNothing, listToMaybe)
import           Data.Semigroup ((<>))
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Tuple (swap)
import           Data.Void (Void)

import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadTimer
import qualified Control.Monad.IOSim as Sim

import           Ouroboros.Network.Block
import           Ouroboros.Network.Chain (Chain (..), chainToList)
import qualified Ouroboros.Network.Chain as Chain
import           Ouroboros.Network.ChainProducerState (ChainProducerState (..))
import           Ouroboros.Network.Node
import           Ouroboros.Network.Testing.ConcreteBlock as ConcreteBlock

import           Test.Chain (TestBlockChain (..), TestChainFork (..),
                             genNonNegative, genHeaderChain)
import           Ouroboros.Network.Protocol.Chain.Node

tests :: TestTree
tests =
  testGroup "Node"
  [ testGroup "fixed graph topology"
    [ testProperty "core -> relay" prop_coreToRelay
    , testProperty "core -> relay -> relay" prop_coreToRelay2
    , -- This fails with cases where the two core nodes end up with different
      -- chains. The termination condition also does not work. Often deadlocks.
      testProperty "core <-> relay <-> core" $
        expectFailure prop_coreToCoreViaRelay
    ]
  , testProperty "arbtirary node graph" (withMaxSuccess 50 prop_networkGraph)
  , testProperty "blockGenerator invariant SimM" prop_blockGenerator_ST
  , testProperty "blockGenerator invariant IO" prop_blockGenerator_IO
  , testProperty "consensus in arbitrary graph" $
      -- We need a common genesis block, or we actually would not expect
      -- consensus to be reached.
      let genesis = BlockHeader (HeaderHash 0) (BlockHash (ConcreteBlock.HeaderHash 0)) (Slot 0) (BlockNo 0) (BlockSigner 0) (BodyHash 0)
          graphGen = resize 42 genConnectedBidirectionalGraph
          nameGen = pure . show
          chainGen = const (resize 42 (genNonEmptyHeaderChain genesis))
      in  forAll (genStaticNetDesc graphGen nameGen chainGen) prop_consensus
  ]


-- NOTE: it reverses the order of probes
partitionProbe :: [(NodeId, a)] -> Map NodeId [a]
partitionProbe
  = Map.fromListWith (++) . map (\(nid, a) -> (nid, [a]))

-- | Block generator should generate blocks in the correct slot time.
--
test_blockGenerator
  :: forall m.
     ( MonadSTM m
     , MonadFork m
     , MonadTimer m
     , Show (Time m)
     )
  => Chain Block
  -> Duration (Time m)
  -> m Property
test_blockGenerator chain slotDuration = isValid <$> withProbe (experiment slotDuration)
  where
    isValid :: [(Time m, Block)] -> Property
    isValid = foldl'
        (\r (t, b) -> r .&&. t === fromStart ((fromIntegral . getSlot . blockSlot $ b) `mult` slotDuration))
        (property True)

    experiment
      :: ( MonadSTM m
         , MonadFork m
         , MonadTimer m
         )
      => Duration (Time m)
      -> Probe m (Time m, Block)
      -> m ()
    experiment slotDur p = do
      getBlock <- blockGenerator slotDur (Chain.toOldestFirst chain)
      void $ fork $ go getBlock
     where
      go getBlock = do
        mb <- atomically $ getBlock
        case mb of
          Just b  -> do t <- getMonotonicTime
                        probeOutput p (t, b)
                        go getBlock
          Nothing -> return ()

prop_blockGenerator_ST :: TestBlockChain -> Positive Micro -> Property
prop_blockGenerator_ST (TestBlockChain chain) (Positive slotDuration) =
    Sim.runSimOrThrow $
      test_blockGenerator chain (Sim.VTimeDuration slotDuration)

prop_blockGenerator_IO :: TestBlockChain -> Positive Int -> Property
prop_blockGenerator_IO (TestBlockChain chain) (Positive slotDuration) =
    ioProperty $ test_blockGenerator chain (slotDuration * 100)

coreToRelaySim :: ( MonadSTM m
                  , MonadFork m
                  , MonadTimer m
                  , MonadSay m
                  , MonadTimer m
                  )
               => Bool              -- ^ two way subscription
               -> Chain Block
               -> Duration (Time m) -- ^ slot duration
               -> Duration (Time m) -- ^ core transport delay
               -> Duration (Time m) -- ^ relay transport delay
               -> Probe m (NodeId, Chain Block)
               -> m ()
coreToRelaySim duplex chain slotDuration coreTrDelay relayTrDelay probe = do
  donevar <- newTVarM False
  (coreChans, relayChans) <- if duplex
    then createTwoWaySubscriptionChannels relayTrDelay coreTrDelay
    else createOneWaySubscriptionChannels coreTrDelay relayTrDelay

  void $ fork $ do
    cps <- coreNode (CoreId 0) slotDuration (Chain.toOldestFirst chain) coreChans
    void $ fork $ observeChainProducerState (CoreId 0) probe cps
  void $ fork $ void $ do
    cps <- relayNode (RelayId 0) Genesis relayChans
    void $ fork $ observeChainProducerState (RelayId 0) probe cps
    atomically $ do
      chain' <- chainState <$> readTVar cps
      unless (chain == chain') retry
      writeTVar donevar True

  atomically $ do
    done <- readTVar donevar
    unless done retry


data TestNodeSim = TestNodeSim
  { testChain               :: Chain Block
  , testSlotDuration        :: Sim.VTimeDuration
  , testCoreTransportDelay  :: Sim.VTimeDuration
  , testRealyTransportDelay :: Sim.VTimeDuration
  }
  deriving (Show, Eq)

instance Arbitrary TestNodeSim where
  arbitrary = do
    TestBlockChain testChain <- arbitrary
    -- at least twice as much as testCoreDelay
    Positive slotDuration <- arbitrary
    Positive testCoreTransportDelay <- arbitrary
    Positive testRelayTransportDelay <- arbitrary
    return $ TestNodeSim testChain (Sim.VTimeDuration slotDuration) (Sim.VTimeDuration testCoreTransportDelay) (Sim.VTimeDuration testRelayTransportDelay)

  -- TODO: shrink

-- this test relies on the property that when there is a single core node,
-- it will never have to use @'fixupBlock'@ function (which mangles blocks
-- picked up from the generator).  This is because all the nodes start with
-- @'Genesis'@ chain, hence the core node is a single source of truth.
prop_coreToRelay :: TestNodeSim -> Property
prop_coreToRelay (TestNodeSim chain slotDuration coreTrDelay relayTrDelay) =
  let probes  = Sim.runSimOrThrow $ withProbe $
                  coreToRelaySim False chain
                                 slotDuration coreTrDelay relayTrDelay
      dict    :: Map NodeId [Chain Block]
      dict    = partitionProbe probes
      mchain1 :: Maybe (Chain Block)
      mchain1 = RelayId 0 `Map.lookup` dict >>= listToMaybe
  in counterexample (show mchain1) $
    if Chain.null chain
        -- when a chain is null, the relay observer will never be triggered,
        -- since its chain never is never updated
      then property $ isNothing mchain1
      else mchain1 === Just chain

-- Node graph: c → r → r
coreToRelaySim2 :: ( MonadSTM m
                   , MonadFork m
                   , MonadTimer m
                   , MonadSay m
                   , MonadTimer m
                   )
                => Chain Block
                -> Duration (Time m)
                -- ^ slot length
                -> Duration (Time m)
                -- ^ core transport delay
                -> Duration (Time m)
                -- ^ relay transport delay
                -> Probe m (NodeId, Chain Block)
                -> m ()
coreToRelaySim2 chain slotDuration coreTrDelay relayTrDelay probe = do
  donevar <- newTVarM False
  (cr1, r1c) <- createOneWaySubscriptionChannels coreTrDelay relayTrDelay
  (r1r2, r2r1) <- createOneWaySubscriptionChannels relayTrDelay relayTrDelay

  void $ fork $ void $ do
    cps <- coreNode (CoreId 0) slotDuration (Chain.toOldestFirst chain) cr1
    void $ fork $ observeChainProducerState (CoreId 0) probe cps
  void $ fork $ void $ do
    cps <- relayNode (RelayId 1) Genesis(r1c <> r1r2)
    void $ fork $ observeChainProducerState (RelayId 1) probe cps
  void $ fork $ void $ do
    cps <- relayNode (RelayId 2) Genesis r2r1
    void $ fork $ observeChainProducerState (RelayId 2) probe cps

    atomically $ do
      chain' <- chainState <$> readTVar cps
      unless (chain == chain') retry
      writeTVar donevar True

  atomically $ do
    done <- readTVar donevar
    unless done retry


prop_coreToRelay2 :: TestNodeSim -> Property
prop_coreToRelay2 (TestNodeSim chain slotDuration coreTrDelay relayTrDelay) =
  let probes  = Sim.runSimOrThrow $ withProbe $
                  coreToRelaySim2 chain slotDuration coreTrDelay relayTrDelay
      dict    = partitionProbe probes
      mchain1 = RelayId 1 `Map.lookup` dict >>= listToMaybe
      mchain2 = RelayId 2 `Map.lookup` dict >>= listToMaybe
  in counterexample (show mchain1) $
    if Chain.null chain
        -- when a chain is null, the relay observer will never be triggered,
        -- since its chain never is never updated
      then isNothing mchain1 .&&. isNothing mchain2
      else
            mchain1 === Just chain
        .&&.
            mchain2 === Just chain


-- | Node graph: c ↔ r ↔ c
--
-- This test assumes that @chain1@ and @chain2@ ends on a different slot.  Under
-- this assumption, we can detect all the three nodes can be terminated when
-- each nodes' chain reaches max slot length of @chain1@ and @chain2@.
coreToCoreViaRelaySim
  :: forall m.
     ( MonadSTM m
     , MonadFork m
     , MonadTimer m
     , MonadSay m
     , MonadTimer m
     )
  => Chain Block
  -> Chain Block
  -> Duration (Time m)
  -> Duration (Time m)
  -> Duration (Time m)
  -> Probe m (NodeId, Chain Block)
  -> m ()
coreToCoreViaRelaySim chain1 chain2 slotDuration coreTrDelay relayTrDelay probe = do
  let -- we compute last block body, under the assumption that one of the chain has
      -- more blocks than the other one, we can determine from which chain the last
      -- block will come in all the nodes.
      lastBlockBody = if Chain.headBlockNo chain1 > Chain.headBlockNo chain2
                        then blockBody <$> Chain.head chain1
                        else blockBody <$> Chain.head chain2
      -- the slot at whcih the simulation will end
      lastSlot = max (Chain.headSlot chain1) (Chain.headSlot chain2)
  donevar <- newTVarM (0 :: Int)
  (c1r1, r1c1) <- createTwoWaySubscriptionChannels coreTrDelay relayTrDelay
  (r1c2, c2r1) <- createTwoWaySubscriptionChannels relayTrDelay coreTrDelay

  void $ fork $ void $ do
    cps <- coreNode (CoreId 1) slotDuration (Chain.toOldestFirst chain1) c1r1
    void $ fork $ observeChainProducerState (CoreId 1) probe cps
    checkTermination donevar cps lastSlot lastBlockBody

  void $ fork $ void $ do
    cps <- relayNode (RelayId 1) Genesis (r1c1 <> r1c2)
    void $ fork $ observeChainProducerState (RelayId 1) probe cps
    checkTermination donevar cps lastSlot lastBlockBody

  void $ fork $ void $ do
    cps <- coreNode (CoreId 2) slotDuration (Chain.toOldestFirst chain2) c2r1
    void $ fork $ observeChainProducerState (CoreId 2) probe cps
    checkTermination donevar cps lastSlot lastBlockBody
    
  -- wait until all the nodes are ready
  atomically $ readTVar donevar >>= check . (>=3)
 where
  -- check if a node can terminate, if so bump the `donevar` value
  checkTermination
    :: TVar m Int
    -> TVar m (ChainProducerState Block)
    -> Slot
    -> Maybe BlockBody
    -> m ()
  checkTermination donevar cps lastSlot lastBlockBody = atomically $ do
    c <- chainState <$> readTVar cps
    let slot = Chain.headSlot c
        bb   = blockBody <$> Chain.head c
    check (slot >= lastSlot && bb == lastBlockBody)
    modifyTVar donevar succ


-- | This properties guarantees that all the nodes picked up the best chain from
-- all the possible chains.  In this setup there are two producers, hence there
-- are only two possible chains that each node can finish with.  Note that this
-- chain might not be the original chain that was passed from the quickcheck
-- generator: it may happen that a core node will start to build up a chain on
-- some block supplied by the other node, which will force the generated blocks
-- to get modified.
--
-- We use a precondition to only test against forks with a different number of
-- blocks.  This way we can determine upfront the last block in the resulting
-- chain on each of the nodes (thanks to simplicity of `Chain.selectChain`
-- function).
--
-- TODO: add a generator which generates chains with a diffrent number of
-- blocks.
prop_coreToCoreViaRelay :: TestChainFork -> Property
prop_coreToCoreViaRelay (TestChainFork _ chain1 chain2) =
  Chain.headBlockNo chain1 /= Chain.headBlockNo chain2 ==>
  let probes  = Sim.runSimOrThrow $ withProbe $
                  coreToCoreViaRelaySim chain1 chain2
                                        (Sim.VTimeDuration 3)
                                        (Sim.VTimeDuration 1)
                                        (Sim.VTimeDuration 1)

      isValid :: Maybe (Chain Block)
              -> Maybe (Chain Block)
              -> Property
      isValid Nothing   Nothing   = chain1 === Genesis .&&. chain2 === Genesis  -- under the slot length assumption, this is impossible
      isValid (Just _)  Nothing   = property False
      isValid Nothing   (Just _)  = property False
      isValid (Just c1) (Just c2) = compareChains c1 c2
  in
        let dict    = partitionProbe probes
            chainC1 = CoreId 1  `Map.lookup` dict >>= listToMaybe
            chainR1 = RelayId 1 `Map.lookup` dict >>= listToMaybe
            chainC2 = CoreId 2  `Map.lookup` dict >>= listToMaybe
        in
            isValid chainC1 chainR1 .&&. isValid chainC2 chainR1
  where
    compareChains :: Chain Block
                  -> Chain Block
                  -> Property
    compareChains c1 c2 =
       (Chain.selectChain c1 c2 === c1) .&&. (Chain.selectChain c2 c1 === c2)

data TestNetworkGraph = TestNetworkGraph Graph [(Int, Chain Block)]
    deriving Show

-- Connect disconnected graph components; randomly chose nodes through which
-- connect them.
connectGraphG :: Graph -> Gen Graph
connectGraphG g = do
    let ts  = scc g
    vs <- traverse (oneof . map return . treeVertices) ts
    return $ accum (flip (:)) g [(i, j) | i <- vs, j <- vs]
    where
    treeVertices :: Tree Vertex -> [Vertex]
    treeVertices (Node i ns) = i : concatMap treeVertices ns

instance Arbitrary TestNetworkGraph where
    arbitrary = resize 20 $ do
        g <- arbitraryAcyclicGraphSmall
        let g' = accum (++) g (assocs $ transposeG g)
            vs = (vertices g)
        cs  <- genCoreNodes vs
        c   <- oneof (map return vs)
        let cs' = if null cs then [c] else cs
        g'' <- connectGraphG g'
        chains <- map getTestBlockChain <$> replicateM (length cs') arbitrary
        return $ TestNetworkGraph g'' (zip cs' chains)
     where
        genCoreNodes :: [Int] -> Gen [Int]
        genCoreNodes []       = return []
        genCoreNodes (x : xs) = do
            t <- frequency [(2, return True), (1, return False)]
            if t
                then (x:) <$> genCoreNodes xs
                else genCoreNodes xs

    shrink (TestNetworkGraph g cs) =
        [ TestNetworkGraph g cs' | cs' <- shrinkList (:[]) cs, not (null cs') ]

networkGraphSim :: forall m.
                  ( MonadSTM m
                  , MonadFork m
                  , MonadTimer m
                  , MonadSay m
                  , MonadTimer m
                  )
                => TestNetworkGraph
                -> Duration (Time m) -- ^ slot duration
                -> Duration (Time m) -- ^ core transport delay
                -> Duration (Time m) -- ^ relay transport delay
                -> Probe m (NodeId, Chain Block)
                -> m ()
networkGraphSim (TestNetworkGraph g cs) slotDuration coreTrDelay relayTrDelay probe = do
  let vs = vertices g
      channs :: Map Vertex (NodeChannels m block)
      channs = Map.fromList (map (,mempty) vs)

  -- construct communication channels based on the graph
  channs' <- flip execStateT channs $ forM (assocs g) $ \(i, peers) -> do
    let isCore = i `elem` map fst cs
        delay  = if isCore then coreTrDelay else relayTrDelay
    forM peers $ \j -> do
      let isCore' = j `elem` map fst cs
          delay'  = if isCore' then coreTrDelay else relayTrDelay
      (cij, cji) <- lift $ createOneWaySubscriptionChannels delay delay'
      modify' (Map.adjust (<> cij) i . Map.adjust (<> cji) j)

  -- run each node
  forM_ vs $ \i ->
    fork $ void $
      case i `lookup` cs of
        Just chain ->
          coreNode  (CoreId i) slotDuration (Chain.toOldestFirst chain) (channs' Map.! i)
          >>= observeChainProducerState (CoreId i) probe
        Nothing ->
          relayNode (RelayId i) Genesis (channs' Map.! i)
          >>= observeChainProducerState (RelayId i) probe

  --FIXME: we have to wait for these nodes to finish!
  -- As written, this is going to termiate immediately

data NetworkTest = NetworkTest
  { networkTestGraph        :: TestNetworkGraph
  , networkTestSlotDuration :: Sim.VTimeDuration
  , networkTestCoreTrDelay  :: Sim.VTimeDuration
  , networkTestRelayTrDelay :: Sim.VTimeDuration
  }

instance Arbitrary NetworkTest where
  arbitrary = NetworkTest <$> arbitrary <*> duration <*> duration <*> duration
    where
      duration = Sim.VTimeDuration . getPositive <$> arbitrary

instance Show NetworkTest where
  show (NetworkTest g slotDuration coreDelay relayDelay) =
      "NetworkTest { networkTestGraph=" ++ show g
      ++ ", networkTestSlotDuration=" ++ show slotDuration
      ++ ", networkTestCoreTrDelay=" ++ show coreDelay
      ++ ", networkTestRelayTrDealy=" ++ show relayDelay ++ "}"

prop_networkGraph :: NetworkTest -> Property
prop_networkGraph (NetworkTest g@(TestNetworkGraph graph cs) slotDuration coreTrDelay relayTrDelay) =
  let vs = vertices graph
      es = edges graph
      gs = map (\i -> removeEdge (minimum vs, maximum vs) (es !! i) es) [0..length es - 1]
      (cc :: Int) = foldl' (\x y -> if isDisconnected y then x + 1 else x) 0 gs

      probes = Sim.runSimOrThrow $ withProbe $
                 networkGraphSim g slotDuration coreTrDelay relayTrDelay
      dict :: Map NodeId (Chain Block)
      dict = Map.mapMaybe listToMaybe (partitionProbe probes)
      chains = Map.elems dict
  in  cover 50 (length vs > 10) "more than 10 vertices"
    $ cover 75 (100 * length cs `div` length vs > 50) "more than 50% of core nodes"
    -- Let call a bidirectional connection (two edges `e` and `swap e`) critical
    -- iff when removed the graph becomes disconnected. The distribution looks
    -- not that bad:
    -- 28% 4
    -- 21% 0
    -- 13% 6
    -- 11% 10
    -- 11% 2
    -- 10% 8
    --  3% 14
    --  2% 16
    --  1% 20
    $ cover 50 (cc > 0) "has more than one critical connection (when removed the network graph becomes disconnected)"
    -- TODO: It might be good to check [closness
    -- centrality](https://en.wikipedia.org/wiki/Closeness_centrality) of
    -- generated graphs; we'd like to have some nodes that are on average very far
    -- from other nodes.
    $ Map.foldl' (\v c -> foldl' Chain.selectChain c chains == c && v) True dict
  where
  -- remove two edges: `a -> b` and `b -> a`
  removeEdge :: Bounds -> Edge -> [Edge] -> Graph
  removeEdge bs e es =
    let es' = filter (\e' -> e /= e' && swap e /= e') es
    in buildG bs es'

-- graph is disconnected if it has strictly more than one component
isDisconnected :: Graph -> Bool
isDisconnected gr = case components gr of
  []       -> False
  (_ : []) -> False
  _        -> True

arbitraryAcyclicGraph :: Gen Int -> Gen Int -> Float -> Gen Graph
arbitraryAcyclicGraph genNRanks genNPerRank edgeChance = do
    nranks    <- genNRanks
    rankSizes <- replicateM nranks genNPerRank
    let rankStarts = scanl (+) 0 rankSizes
        rankRanges = drop 1 (zip rankStarts (tail rankStarts))
        totalRange = sum rankSizes
    rankEdges <- mapM (uncurry genRank) rankRanges
    return $ buildG (0, totalRange-1) (concat rankEdges)
  where
    genRank :: Vertex -> Vertex -> Gen [Edge]
    genRank rankStart rankEnd =
      filterM (const (pick edgeChance))
        [ (i,j)
        | i <- [0..rankStart-1]
        , j <- [rankStart..rankEnd-1]
        ]

    pick :: Float -> Gen Bool
    pick chance = (< chance) <$> choose (0,1)


arbitraryAcyclicGraphSmall :: Gen Graph
arbitraryAcyclicGraphSmall =
    sized $ \sz ->
    arbitraryAcyclicGraph (choose (2, 8 `min` (sz `div` 3)))
                          (choose (1, 8 `min` (sz `div` 3)))
                          0.3

genConnectedBidirectionalGraph :: Gen Graph
genConnectedBidirectionalGraph = do
  g <- arbitraryAcyclicGraphSmall
  let g' = accum (++) g (assocs $ transposeG g)
  connectGraphG g'

-- | Generate a non-empty chain starting with this block header.
genNonEmptyHeaderChain :: BlockHeader -> Gen (NonEmpty BlockHeader)
genNonEmptyHeaderChain genesis = do
  n <- genNonNegative
  chain <- reverse . chainToList <$> genHeaderChain n
  pure $ genesis NE.:| chain

-- | Convert a 'TestNetworkGraph p' into a 'StaticNetDesc BlockHeader x'.
-- They both have the same adjacency. The mismatch is that the former only gives
-- chains for the core nodes, and it uses 'Chain (Block p)' which has a
-- genesis block built-in implicitly. The latter requires a
-- 'NonEmpty BlockHeader' for _each_ node, which morally should always be
-- possible because every node must have _a_ genesis block, and there is no
-- one true genesis block. 
genStaticNetDesc
  :: forall m .
     ( Applicative m )
  => Gen Graph                               -- ^ Generate adjacency.
                                             -- Out-edge means "consume from".
  -> (Int -> Gen String)                     -- ^ Name assignment.
  -> (Int -> Gen (NonEmpty BlockHeader))     -- ^ Generate header chains.
  -> Gen (StaticNetDesc BlockHeader m ())
genStaticNetDesc genGraph genName genChain = do
  gr <- genGraph
  let vs = vertices gr
  vs' <- forM vs $ \v -> do
    desc <- genStaticNodeDesc (genName v) (genChain v)
    pure (v, desc)
  let nodeDescs :: Map Vertex (StaticNodeDesc Void BlockHeader m ())
      nodeDescs = Map.fromList vs'
  pure $ StaticNetDesc gr nodeDescs

genStaticNodeDesc
  :: ( Applicative m )
  => Gen String
  -> Gen (NonEmpty BlockHeader)
  -> Gen (StaticNodeDesc Void BlockHeader m ())
genStaticNodeDesc genName genChain = staticNodeDesc <$> genName <*> (staticChain <$> genChain)

-- | Asserts that all nodes in a connected graph reach consensus up to
-- block count.
prop_consensus :: StaticNetDesc BlockHeader IO () -> Property
prop_consensus netDesc@(StaticNetDesc graph nodes) =
  not (isDisconnected graph) ==> ioProperty $ do
    -- Take the longest chain in the net and use that as a stop condition. This
    -- means cycles in the graph are OK: every node should stop producing when
    -- they get a chain that meets the length condition.
    let newestBlockNos :: Map Int BlockNo
        newestBlockNos = fmap (blockNo . NE.last . ecInitial . nodeDescChain) nodes
        highestBlockNo = foldl max (BlockNo 0) newestBlockNos
        chainSelection :: forall m . Monad m => ChainSelection BlockHeader m (Seq BlockHeader)
        chainSelection = chainSelectionUntil $ \chain -> case Seq.viewr chain of
          Seq.EmptyR -> pure Nothing
          _ Seq.:> newest -> pure $
            if blockNo newest >= highestBlockNo
            then Just chain
            else Nothing
    chainAssocs :: Map Vertex (Seq BlockHeader) <- (fmap . fmap) fst $
      runNetDescStandardIO
        netDesc
        (\bh1 bh2 -> headerHash bh1 == headerHash bh2)
        blockNo
        Chain.blockPoint
        chainSelection
    let chains = Map.elems chainAssocs
    -- subtract 1 from the length of the chain, because the genesis block has
    -- block number 0.
    pure $ counterexample (show highestBlockNo)
         $ counterexample (show ((fmap . fmap) headerHash chainAssocs))
         $ all (== highestBlockNo) (fmap (BlockNo . fromIntegral . (flip (-) 1) . length) chains)


--
-- Probe mini-abstraction
--

-- | Where returning results directly is not convenient, we can build up
-- a trace of events we want to observe, and can do probe output from
-- multiple threads.
--
type Probe m x = TVar m [x]

withProbe :: MonadSTM m => (Probe m x -> m ()) -> m [x]
withProbe action = do
    probe <- newTVarM []
    action probe
    reverse <$> atomically (readTVar probe)

probeOutput :: MonadSTM m => Probe m x -> x -> m ()
probeOutput probe x = atomically (modifyTVar probe (x:))


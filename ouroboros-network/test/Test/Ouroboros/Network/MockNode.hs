{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeOperators       #-}

module Test.Ouroboros.Network.MockNode where

import           Control.Monad (filterM, forM, forM_, replicateM, unless)
import           Control.Monad.State (execStateT, lift, modify')
import           Data.Array
import           Data.Fixed (Micro)
import           Data.Functor (void)
import           Data.Graph
import           Data.List (foldl')
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (isNothing, listToMaybe)
import           Data.Tuple (swap)

import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import qualified Control.Monad.IOSim as Sim

import           Ouroboros.Network.Block
import           Ouroboros.Network.MockChain.Chain (Chain (..))
import qualified Ouroboros.Network.MockChain.Chain as Chain
import           Ouroboros.Network.MockChain.ProducerState
                     (ChainProducerState (..))
import           Ouroboros.Network.MockNode
import           Ouroboros.Network.Testing.ConcreteBlock as ConcreteBlock

import           Test.ChainGenerators (TestBlockChain (..))


tests :: TestTree
tests =
  testGroup "MockNode"
  [ testGroup "fixed graph topology"
    [ testProperty "core -> relay" prop_coreToRelay
    , testProperty "core -> relay -> relay" prop_coreToRelay2
    ]
  , testProperty "arbtirary node graph" (withMaxSuccess 50 prop_networkGraph)
  , testProperty "blockGenerator invariant IOSim" prop_blockGenerator_ST
  , testProperty "blockGenerator invariant IO" prop_blockGenerator_IO
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
     , MonadTime m
     , MonadTimer m
     )
  => Chain Block
  -> DiffTime
  -> m Property
test_blockGenerator chain slotDuration = do
    startTime <- getMonotonicTime
    isValid startTime <$> withProbe (experiment slotDuration)
  where
    isValid :: Time -> [(Time, Block)] -> Property
    isValid startTime = foldl'
      (\r (t, b) -> r .&&. counterexample (show t
                                           ++ " ≱ "
                                           ++ show (slotTime (blockSlot b)))
                                           (t >= slotTime (blockSlot b))
                      .&&. counterexample (show t
                                           ++ " ≮ "
                                           ++ show (slotTime (succ $ blockSlot b)))
                                           (t <  (slotTime (succ $ blockSlot b))))
        (property True)
      where
        slotTime :: SlotNo -> Time
        slotTime s = (realToFrac (unSlotNo s) * slotDuration) `addTime` startTime

    experiment
      :: ( MonadSTM m
         , MonadFork m
         , MonadTime m
         , MonadTimer m
         )
      => DiffTime
      -> Probe m (Time, Block)
      -> m ()
    experiment slotDur p = do
      getBlock <- blockGenerator slotDur (Chain.toOldestFirst chain)
      void $ forkIO $ go getBlock
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
      test_blockGenerator chain (realToFrac slotDuration)

prop_blockGenerator_IO :: TestBlockChain -> Positive Int -> Property
prop_blockGenerator_IO (TestBlockChain chain) (Positive slotDuration) =
    ioProperty $
      test_blockGenerator chain slotDuration'
  where
    slotDuration' :: DiffTime
    slotDuration' = fromIntegral slotDuration

coreToRelaySim :: ( MonadSTM m
                  , MonadFork m
                  , MonadThrow m
                  , MonadSay m
                  , MonadTime m
                  , MonadTimer m
                  )
               => Bool              -- ^ two way subscription
               -> Chain Block
               -> DiffTime          -- ^ slot duration
               -> DiffTime          -- ^ core transport delay
               -> DiffTime          -- ^ relay transport delay
               -> Probe m (NodeId, Chain Block)
               -> m ()
coreToRelaySim duplex chain slotDuration coreTrDelay relayTrDelay probe = do
  donevar <- newTVarIO False
  (coreChans, relayChans) <- if duplex
    then createTwoWaySubscriptionChannels relayTrDelay coreTrDelay
    else createOneWaySubscriptionChannels coreTrDelay relayTrDelay

  void $ forkIO $ do
    cps <- coreNode (CoreId 0) slotDuration (Chain.toOldestFirst chain) coreChans
    void $ forkIO $ observeChainProducerState (CoreId 0) probe cps
  void $ forkIO $ void $ do
    cps <- relayNode (RelayId 0) Genesis relayChans
    void $ forkIO $ observeChainProducerState (RelayId 0) probe cps
    atomically $ do
      chain' <- chainState <$> readTVar cps
      unless (chain == chain') retry
      writeTVar donevar True

  atomically $ do
    done <- readTVar donevar
    unless done retry


data TestNodeSim = TestNodeSim
  { testChain               :: Chain Block
  , testSlotDuration        :: DiffTime
  , testCoreTransportDelay  :: DiffTime
  , testRealyTransportDelay :: DiffTime
  }
  deriving (Show, Eq)

instance Arbitrary TestNodeSim where
  arbitrary = do
    TestBlockChain testChain <- arbitrary
    -- at least twice as much as testCoreDelay
    Positive slotDuration <- arbitrary
    Positive testCoreTransportDelay <- arbitrary
    Positive testRelayTransportDelay <- arbitrary
    let secondsToDiffTime :: Micro -> DiffTime
        secondsToDiffTime = realToFrac
    return $ TestNodeSim testChain (secondsToDiffTime slotDuration)
                                   (secondsToDiffTime testCoreTransportDelay)
                                   (secondsToDiffTime testRelayTransportDelay)

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
                   , MonadThrow m
                   , MonadSay m
                   , MonadTime m
                   , MonadTimer m
                   )
                => Chain Block
                -> DiffTime
                -- ^ slot length
                -> DiffTime
                -- ^ core transport delay
                -> DiffTime
                -- ^ relay transport delay
                -> Probe m (NodeId, Chain Block)
                -> m ()
coreToRelaySim2 chain slotDuration coreTrDelay relayTrDelay probe = do
  donevar <- newTVarIO False
  (cr1, r1c) <- createOneWaySubscriptionChannels coreTrDelay relayTrDelay
  (r1r2, r2r1) <- createOneWaySubscriptionChannels relayTrDelay relayTrDelay

  void $ forkIO $ void $ do
    cps <- coreNode (CoreId 0) slotDuration (Chain.toOldestFirst chain) cr1
    void $ forkIO $ observeChainProducerState (CoreId 0) probe cps
  void $ forkIO $ void $ do
    cps <- relayNode (RelayId 1) Genesis(r1c <> r1r2)
    void $ forkIO $ observeChainProducerState (RelayId 1) probe cps
  void $ forkIO $ void $ do
    cps <- relayNode (RelayId 2) Genesis r2r1
    void $ forkIO $ observeChainProducerState (RelayId 2) probe cps

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
                  , MonadThrow m
                  , MonadSay m
                  , MonadTime m
                  , MonadTimer m
                  )
                => TestNetworkGraph
                -> DiffTime          -- ^ slot duration
                -> DiffTime          -- ^ core transport delay
                -> DiffTime          -- ^ relay transport delay
                -> Probe m (NodeId, Chain Block)
                -> m ()
networkGraphSim (TestNetworkGraph g cs) slotDuration coreTrDelay relayTrDelay probe = do
  let vs = vertices g
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
    forkIO $ void $
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
  , networkTestSlotDuration :: DiffTime
  , networkTestCoreTrDelay  :: DiffTime
  , networkTestRelayTrDelay :: DiffTime
  }

instance Arbitrary NetworkTest where
  arbitrary = NetworkTest <$> arbitrary <*> duration <*> duration <*> duration
    where
      duration = (realToFrac :: Micro -> DiffTime)
               . getPositive <$> arbitrary

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


--
-- Probe mini-abstraction
--

-- | Where returning results directly is not convenient, we can build up
-- a trace of events we want to observe, and can do probe output from
-- multiple threads.
--
type Probe m x = StrictTVar m [x]

withProbe :: MonadSTM m => (Probe m x -> m ()) -> m [x]
withProbe action = do
    probe <- newTVarIO []
    action probe
    reverse <$> atomically (readTVar probe)

probeOutput :: MonadSTM m => Probe m x -> x -> m ()
probeOutput probe x = atomically (modifyTVar probe (x:))

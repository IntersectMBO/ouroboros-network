{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeOperators       #-}
module Test.Node where

import           Control.Monad (forM, forM_, forever, replicateM)
import           Control.Monad.ST.Lazy (runST)
import           Control.Monad.State (execStateT, lift, modify')
import           Data.Array
import           Data.Functor (void)
import           Data.Graph
import           Data.List (foldl')
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (isNothing, listToMaybe)
import           Data.Semigroup ((<>))
import           Data.Tuple (swap)

import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           Block
import           Chain (Chain (..))
import qualified Chain
import           MonadClass
import           Node
import           Ouroboros
import           Protocol (MsgConsumer, MsgProducer)
import qualified Sim

import           Test.Chain (TestBlockChain (..), TestChainFork (..))
import           Test.DepFn
import           Test.Ouroboros
import           Test.Sim (TestThreadGraph (..))

tests :: TestTree
tests =
  testGroup "Node"
  [ testGroup "fixed graph topology"
    [ testProperty "core -> relay" prop_coreToRelay
    , testProperty "core -> relay -> relay" prop_coreToRelay2
    , testProperty "core <-> relay <-> core" prop_coreToCoreViaRelay
    ]
  , testProperty "arbtirary node graph" (withMaxSuccess 50 prop_networkGraph)
  , testProperty "blockGenerator invariant (SimM)" prop_blockGenerator_ST
  , testProperty "blockGenerator invariant (IO)" prop_blockGenerator_IO
  ]


-- NOTE: it reverses the order of probes
partitionProbe :: [(NodeId, a)] -> Map NodeId [a]
partitionProbe
  = Map.fromListWith (++) . map (\(nid, a) -> (nid, [a]))

-- | Block generator should generate blocks in the correct slot time.
--
test_blockGenerator
  :: forall p m stm n.
     ( MonadSTM m stm
     , MonadTimer m
     , MonadProbe m
     , MonadRunProbe m n
     , Show (Time m)
     )
  => Chain (Block p)
  -> Duration (Time m)
  -> n Property
test_blockGenerator chain slotDuration = isValid <$> withProbe (experiment slotDuration)
  where
    isValid :: [(Time m, Block p)] -> Property
    isValid = foldl'
        (\r (t, b) -> r .&&. t === fromStart ((fromIntegral . getSlot . blockSlot $ b) `mult` slotDuration))
        (property True)

    experiment
      :: ( MonadSTM m stm
         , MonadTimer m
         , MonadProbe m
         )
      => Duration (Time m)
      -> Probe m (Block p)
      -> m ()
    experiment slotDur p = do
      v <- blockGenerator slotDur (Chain.toOldestFirst chain)
      fork $ forever $ do
        b <- atomically $ getBlock v
        probeOutput p b

prop_blockGenerator_ST :: TestBlockChain :-> (Positive Rational -> Property)
prop_blockGenerator_ST = simpleProp $ \_ (TestBlockChain chain) (Positive slotDuration) ->
    runST $ test_blockGenerator chain (Sim.VTimeDuration slotDuration)

prop_blockGenerator_IO :: TestBlockChain :-> (Positive Int -> Property)
prop_blockGenerator_IO = simpleProp $ \_ (TestBlockChain chain) (Positive slotDuration) ->
    ioProperty $ test_blockGenerator chain (slotDuration * 100)

coreToRelaySim :: ( MonadSTM m stm
                  , MonadTimer m
                  , MonadSay m
                  , MonadProbe m
                  , KnownOuroborosProtocol p
                  )
               => Bool              -- ^ two way subscription
               -> Chain (Block p)
               -> Duration (Time m) -- ^ slot duration
               -> Duration (Time m) -- ^ core transport delay
               -> Duration (Time m) -- ^ relay transport delay
               -> Probe m (NodeId, Chain (Block p))
               -> m ()
coreToRelaySim duplex chain slotDuration coreTrDelay relayTrDelay probe = do
  (coreChans, relayChans) <- if duplex
    then createTwoWaySubscriptionChannels relayTrDelay coreTrDelay
    else createOneWaySubscriptionChannels coreTrDelay relayTrDelay

  fork $ do
    cps <- coreNode (CoreId 0) slotDuration (Chain.toOldestFirst chain) coreChans
    fork $ observeChainProducerState (CoreId 0) probe cps
  fork $ void $ do
    cps <- relayNode (RelayId 0) relayChans
    fork $ observeChainProducerState (RelayId 0) probe cps

runCoreToRelaySim :: KnownOuroborosProtocol p
                  => Chain (Block p)
                  -> Sim.VTimeDuration
                  -> Sim.VTimeDuration
                  -> Sim.VTimeDuration
                  -> [(Sim.VTime, (NodeId, Chain (Block p)))]
runCoreToRelaySim chain slotDuration coreTransportDelay relayTransportDelay =
  runST $ withProbe (coreToRelaySim False chain slotDuration coreTransportDelay relayTransportDelay)

data TestNodeSim p = TestNodeSim
  { testChain               :: Chain (Block p)
  , testSlotDuration        :: Sim.VTimeDuration
  , testCoreTransportDelay  :: Sim.VTimeDuration
  , testRealyTransportDelay :: Sim.VTimeDuration
  }
  deriving (Show, Eq)

instance SingShow TestNodeSim where
  singShow s = singKnownOuroborosProtocol s $ show

instance SingArbitrary TestNodeSim where
  singArbitrary p = do
    TestBlockChain testChain <- singArbitrary p
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
prop_coreToRelay :: TestNodeSim :-> Property
prop_coreToRelay = simpleProp $ \(_ :: Sing p) (TestNodeSim chain slotDuration coreTrDelay relayTrDelay) ->
  let probes  = map snd $ runCoreToRelaySim chain slotDuration coreTrDelay relayTrDelay
      dict    :: Map NodeId [Chain (Block p)]
      dict    = partitionProbe probes
      mchain1 :: Maybe (Chain (Block p))
      mchain1 = RelayId 0 `Map.lookup` dict >>= listToMaybe
  in counterexample (show mchain1) $
    if Chain.null chain
        -- when a chain is null, the relay observer will never be triggered,
        -- since its chain never is never updated
      then property $ isNothing mchain1
      else mchain1 === Just chain

-- Node graph: c → r → r
coreToRelaySim2 :: ( KnownOuroborosProtocol p
                   , MonadSTM m stm
                   , MonadTimer m
                   , MonadSay m
                   , MonadProbe m
                   )
                => Chain (Block p)
                -> Duration (Time m)
                -- ^ slot length
                -> Duration (Time m)
                -- ^ core transport delay
                -> Duration (Time m)
                -- ^ relay transport delay
                -> Probe m (NodeId, Chain (Block p))
                -> m ()
coreToRelaySim2 chain slotDuration coreTrDelay relayTrDelay probe = do
  (cr1, r1c) <- createOneWaySubscriptionChannels coreTrDelay relayTrDelay
  (r1r2, r2r1) <- createOneWaySubscriptionChannels relayTrDelay relayTrDelay

  fork $ void $ do
    cps <- coreNode (CoreId 0) slotDuration (Chain.toOldestFirst chain) cr1
    fork $ observeChainProducerState (CoreId 0) probe cps
  fork $ void $ do
    cps <- relayNode (RelayId 1) (r1c <> r1r2)
    fork $ observeChainProducerState (RelayId 1) probe cps
  fork $ void $ do
    cps <- relayNode (RelayId 2) r2r1
    fork $ observeChainProducerState (RelayId 2) probe cps

runCoreToRelaySim2 :: KnownOuroborosProtocol p
                   => Chain (Block p)
                   -> Sim.VTimeDuration
                   -> Sim.VTimeDuration
                   -> Sim.VTimeDuration
                   -> [(Sim.VTime, (NodeId, Chain (Block p)))]
runCoreToRelaySim2 chain slotDuration coreTransportDelay relayTransportDelay = runST $ do
  probe <- newProbe
  runM $ coreToRelaySim2 chain slotDuration coreTransportDelay relayTransportDelay probe
  readProbe probe

prop_coreToRelay2 :: TestNodeSim :-> Property
prop_coreToRelay2 = simpleProp $ \_ (TestNodeSim chain slotDuration coreTrDelay relayTrDelay) ->
  let probes  = map snd $ runCoreToRelaySim2 chain slotDuration coreTrDelay relayTrDelay
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
coreToCoreViaRelaySim :: ( KnownOuroborosProtocol p
                         , MonadSTM m stm
                         , MonadTimer m
                         , MonadSay m
                         , MonadProbe m
                         )
                      => Chain (Block p)
                      -> Chain (Block p)
                      -> Duration (Time m)
                      -> Duration (Time m)
                      -> Duration (Time m)
                      -> Probe m (NodeId, Chain (Block p))
                      -> m ()
coreToCoreViaRelaySim chain1 chain2 slotDuration coreTrDelay relayTrDelay probe = do
  (c1r1, r1c1) <- createTwoWaySubscriptionChannels coreTrDelay relayTrDelay
  (r1c2, c2r1) <- createTwoWaySubscriptionChannels relayTrDelay coreTrDelay

  fork $ void $ do
    cps <- coreNode (CoreId 1) slotDuration (Chain.toOldestFirst chain1) c1r1
    fork $ observeChainProducerState (CoreId 1) probe cps
  fork $ void $ do
    cps <- relayNode (RelayId 1) (r1c1 <> r1c2)
    fork $ observeChainProducerState (RelayId 1) probe cps
  fork $ void $ do
    cps <- coreNode (CoreId 2) slotDuration (Chain.toOldestFirst chain2) c2r1
    fork $ observeChainProducerState (CoreId 2) probe cps

runCoreToCoreViaRelaySim
  :: KnownOuroborosProtocol p
  => Chain (Block p)
  -> Chain (Block p)
  -> Sim.VTimeDuration
  -> Sim.VTimeDuration
  -> Sim.VTimeDuration
  -> [(Sim.VTime, (NodeId, Chain (Block p)))]
runCoreToCoreViaRelaySim chain1 chain2 slotDuration coreTrDelay relayTrDelay = runST $ do
  probe <- newProbe
  runM $ coreToCoreViaRelaySim chain1 chain2 slotDuration coreTrDelay relayTrDelay probe
  readProbe probe

-- | This properties guarantees that all the nodes picked up the best chain from
-- all the possible chains.  In this setup there are two producers, hence there
-- are only two possible chains that each node can finish with.  Note that this
-- chain might not be the original chain that was passed from the quickcheck
-- generator: it may happen that a core node will start to build up a chain on
-- some block supplied by the other node.
--
prop_coreToCoreViaRelay :: TestChainFork :-> Property
prop_coreToCoreViaRelay = simpleProp $ \(_ :: Sing p) (TestChainFork _ chain1 chain2) ->
  let probes = map snd $ runCoreToCoreViaRelaySim chain1 chain2 (Sim.VTimeDuration 3) (Sim.VTimeDuration 1) (Sim.VTimeDuration 1)

      isValid :: Maybe (Chain (Block p)) -> Maybe (Chain (Block p)) -> Property
      isValid Nothing   Nothing   = chain1 === Genesis .&&. chain2 === Genesis
      isValid (Just _)  Nothing   = property False
      isValid Nothing   (Just _)  = property False
      isValid (Just c1) (Just c2) = compareChains c1 c2


  in
        let dict    = partitionProbe probes
            chainC1 = CoreId 1  `Map.lookup` dict >>= listToMaybe
            chainR1 = RelayId 1 `Map.lookup` dict >>= listToMaybe
            chainC2 = CoreId 2  `Map.lookup` dict >>= listToMaybe
        in
            isValid chainC1 chainR1 .&&. isValid chainC1 chainC2
  where
    compareChains :: KnownOuroborosProtocol p => Chain (Block p) -> Chain (Block p) -> Property
    compareChains c1 c2 =
        counterexample (c1_ ++ "\n\n" ++ c2_) (Chain.selectChain c1 c2 === c1)
      .&&.
        counterexample (c1_ ++ "\n\n" ++ c2_) (Chain.selectChain c2 c1 === c2)
      where
        nl  = "\n    "
        c1_ = Chain.prettyPrintChain nl show c1
        c2_ = Chain.prettyPrintChain nl show c2

data TestNetworkGraph p = TestNetworkGraph Graph [(Int, Chain (Block p))]
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

instance SingArbitrary TestNetworkGraph where
    singArbitrary p = resize 20 $ do
        TestThreadGraph g <- arbitrary
        let g' = accum (++) g (assocs $ transposeG g)
            vs = (vertices g)
        cs  <- genCoreNodes vs
        c   <- oneof (map return vs)
        let cs' = if null cs then [c] else cs
        g'' <- connectGraphG g'
        chains <- map getTestBlockChain <$> replicateM (length cs') (singArbitrary p)
        return $ TestNetworkGraph g'' (zip cs' chains)
     where
        genCoreNodes :: [Int] -> Gen [Int]
        genCoreNodes []       = return []
        genCoreNodes (x : xs) = do
            t <- frequency [(2, return True), (1, return False)]
            if t
                then (x:) <$> genCoreNodes xs
                else genCoreNodes xs

    singShrink _ (TestNetworkGraph g cs) =
        [ TestNetworkGraph g cs' | cs' <- shrinkList (:[]) cs, not (null cs') ]

instance SingShow TestNetworkGraph where
    -- TODO: we need `SingShow` instance for `Chain (Block p)`
    singShow _p (TestNetworkGraph g _cs) = "TestNetworkGraph " ++ show g

networkGraphSim :: forall p m stm .
                  ( KnownOuroborosProtocol p
                  , MonadSTM m stm
                  , MonadTimer m
                  , MonadProbe m
                  , MonadSay m
                  )
                => TestNetworkGraph p
                -> Duration (Time m) -- ^ slot duration
                -> Duration (Time m) -- ^ core transport delay
                -> Duration (Time m) -- ^ relay transport delay
                -> Probe m (NodeId, Chain (Block p))
                -> m ()
networkGraphSim (TestNetworkGraph g cs) slotDuration coreTrDelay relayTrDelay probe = do
  let vs = vertices g
      channs :: Map Vertex (NodeChannels m (MsgProducer block) MsgConsumer)
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
          relayNode (RelayId i) (channs' Map.! i)
          >>= observeChainProducerState (RelayId i) probe

runNetworkGraphSim
  :: KnownOuroborosProtocol p
  => TestNetworkGraph p
  -> Sim.VTimeDuration
  -> Sim.VTimeDuration
  -> Sim.VTimeDuration
  -> [(Sim.VTime, (NodeId, Chain (Block p)))]
runNetworkGraphSim g slotDuration coreTrDelay relayTrDelay
  = runST $ withProbe (networkGraphSim g slotDuration coreTrDelay relayTrDelay)

data NetworkTest p = NetworkTest
  { networkTestGraph        :: TestNetworkGraph p
  , networkTestSlotDuration :: Sim.VTimeDuration
  , networkTestCoreTrDelay  :: Sim.VTimeDuration
  , networkTestRelayTrDelay :: Sim.VTimeDuration
  }
  deriving Show

instance SingArbitrary NetworkTest where
  singArbitrary p = NetworkTest <$> singArbitrary p <*> duration <*> duration <*> duration
    where
      duration = Sim.VTimeDuration . getPositive <$> arbitrary

instance SingShow NetworkTest where
  singShow p (NetworkTest g slotDuration coreDelay relayDelay) =
      "NetworkTest { networkTestGraph=" ++ singShow p g
      ++ ", networkTestSlotDuration=" ++ show slotDuration
      ++ ", networkTestCoreTrDelay=" ++ show coreDelay
      ++ ", networkTestRelayTrDealy=" ++ show relayDelay ++ "}"

prop_networkGraph :: NetworkTest :-> Property
prop_networkGraph = simpleProp $ \(_ :: Sing p) (NetworkTest g@(TestNetworkGraph graph cs) slotDuration coreTrDelay relayTrDelay) ->
  let vs = vertices graph
      es = edges graph
      gs = map (\i -> removeEdge (minimum vs, maximum vs) (es !! i) es) [0..length es - 1]
      (cc :: Int) = foldl' (\x y -> if isDisconnected y then x + 1 else x) 0 gs

      probes = map snd $ runNetworkGraphSim g slotDuration coreTrDelay relayTrDelay
      dict :: Map NodeId (Chain (Block p))
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
  -- graph is disconnected if it has strictly more than one component
  isDisconnected :: Graph -> Bool
  isDisconnected gr = case components gr of
    []       -> False
    (_ : []) -> False
    _        -> True

  -- remove two edges: `a -> b` and `b -> a`
  removeEdge :: Bounds -> Edge -> [Edge] -> Graph
  removeEdge bs e es =
    let es' = filter (\e' -> e /= e' && swap e /= e') es
    in buildG bs es'

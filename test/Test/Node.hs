{-# LANGUAGE RecordWildCards #-}
module Test.Node where

import Control.Monad.ST.Lazy (runST)
import Data.Functor (void)
import Data.List (find)
import Data.Maybe (isNothing)
import Data.Semigroup ((<>))
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map

import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Block
import qualified Chain
import           Chain (Chain (..), Point)
import Node
import MonadClass
import qualified Sim

import Test.Chain (TestBlockChain (..), TestChainFork (..))

tests :: TestTree
tests =
  testGroup "Node"
  [ testGroup "fixed graph topology"
    [
    --   testProperty "core -> relay" prop_coreToRelay
    -- , testProperty "core -> relay -> relay" prop_coreToRelay2
    -- , testProperty "core <-> relay <-> core" (prop_coreToCoreViaRelay
    ]
  ]


-- note: it will reverse the order of probes!
partitionProbe :: [(NodeId, Int, Point)] -> Map NodeId [(Int, Point)]
partitionProbe
  = Map.fromListWith (++) . map (\(nid, i, p) -> (nid, [(i, p)]))

coreToRelaySim :: ( MonadSTM m stm
                  , MonadTimer m
                  , MonadSay m
                  , MonadProbe m
                  )
               => Bool              -- ^ two way subscription
               -> Chain Block
               -> Duration (Time m) -- ^ core delay (block creation)
               -> Duration (Time m) -- ^ core transport delay
               -> Duration (Time m) -- ^ relay transport delay
               -> Probe m (NodeId, Int, Point)
               -> m ()
coreToRelaySim duplex chain coreDelay coreTrDelay relayTrDelay probe = do
  (coreChans, relayChans) <- if duplex
    then createTwoWaySubscriptionChannels relayTrDelay coreTrDelay
    else createOneWaySubscriptionChannels coreTrDelay relayTrDelay

  fork $ do
    cps <- coreNode (CoreId 0) coreDelay chain coreChans
    fork $ observeChainProducerState (CoreId 0) probe cps
  fork $ void $ do
    cps <- relayNode (RelayId 0) relayChans
    fork $ observeChainProducerState (RelayId 0) probe cps

runCoreToRelaySim :: Chain Block
                  -> Sim.VTimeDuration
                  -> Sim.VTimeDuration
                  -> Sim.VTimeDuration
                  -> [(Sim.VTime, (NodeId, Int, Point))]
runCoreToRelaySim chain coreDelay coreTransportDelay relayTransportDelay =
  runST $ do
    probe <- newProbe
    runM $ coreToRelaySim False chain coreDelay coreTransportDelay relayTransportDelay probe
    readProbe probe

data TestNodeSim = TestNodeSim
  { testChain               :: Chain Block
  , testCoreDelay           :: Sim.VTimeDuration
  , testCoreTransportDelay  :: Sim.VTimeDuration
  , testRealyTransportDelay :: Sim.VTimeDuration
  }
  deriving (Show, Eq) 

instance Arbitrary TestNodeSim where
  arbitrary = do
    TestBlockChain testChain <- arbitrary
    Positive testCoreDelay <- arbitrary
    Positive testCoreTransportDelay <- arbitrary
    Positive testRelayTransportDelay <- arbitrary
    return $ TestNodeSim testChain (Sim.VTimeDuration testCoreDelay) (Sim.VTimeDuration testCoreTransportDelay) (Sim.VTimeDuration testRelayTransportDelay)

  -- TODO: shrink

-- this test relies on the property that when there is a single core node the
-- it will never have to use @'fixupBlock'@ function.
prop_coreToRelay :: TestNodeSim -> Property
prop_coreToRelay (TestNodeSim chain coreDelay coreTrDelay relayTrDelay) =
  let points  = Set.fromList $ map Chain.blockPoint $ Chain.toList chain
      probes  = map snd $ runCoreToRelaySim chain coreDelay coreTrDelay relayTrDelay
      dict    :: Map NodeId [(Int, Point)]
      dict    = partitionProbe probes
      probes1 :: Maybe [(Int, Point)]
      probes1 = RelayId 0 `Map.lookup` dict
      points  :: Set Point
      points1 = maybe Set.empty (Set.fromList . map (\(_, p) -> p)) probes1
  in counterexample (show probes1) $ 
    if Chain.null chain
        -- when a chain is null, the relay observer will never be triggered,
        -- since its chain never is never updated
      then property $ isNothing probes1
      else
        -- tip of the chain and the length agree
          (probes1 >>= find (\(_, p) -> p == Chain.headPoint chain))
          ===
          Just (Chain.length chain, Chain.headPoint chain)
        .&&.
        -- both chains consists of the same set of points
          points === points1

-- Node graph: c → r → r
coreToRelaySim2 :: ( MonadSTM m stm
                   , MonadTimer m
                   , MonadSay m
                   , MonadProbe m
                   )
                => Chain Block
                -> Duration (Time m)
                -> Duration (Time m)
                -> Duration (Time m)
                -> Probe m (NodeId, Int, Point)
                -> m ()
coreToRelaySim2 chain coreDelay coreTrDelay relayTrDelay probe = do
  (cr1, r1c) <- createOneWaySubscriptionChannels coreTrDelay relayTrDelay
  (r1r2, r2r1) <- createOneWaySubscriptionChannels relayTrDelay relayTrDelay

  fork $ void $ do
    cps <- coreNode (CoreId 0) coreDelay chain cr1
    fork $ observeChainProducerState (CoreId 0) probe cps
  fork $ void $ do
    cps <- relayNode (RelayId 1) (r1c <> r1r2)
    fork $ observeChainProducerState (RelayId 1) probe cps
  fork $ void $ do
    cps <- relayNode (RelayId 2) r2r1
    fork $ observeChainProducerState (RelayId 2) probe cps

runCoreToRelaySim2 :: Chain Block
                   -> Sim.VTimeDuration
                   -> Sim.VTimeDuration
                   -> Sim.VTimeDuration
                  -> [(Sim.VTime, (NodeId, Int, Point))]
runCoreToRelaySim2 chain coreDelay coreTransportDelay relayTransportDelay = runST $ do
  probe <- newProbe
  runM $ coreToRelaySim2 chain coreDelay coreTransportDelay relayTransportDelay probe
  readProbe probe

prop_coreToRelay2 :: TestNodeSim -> Property
prop_coreToRelay2 (TestNodeSim chain coreDelay coreTrDelay relayTrDelay) =
  let points  = Set.fromList $ map Chain.blockPoint $ Chain.toList chain
      dict    = partitionProbe probes
      probes  = map snd $ runCoreToRelaySim2 chain coreDelay coreTrDelay relayTrDelay
      probes1 = RelayId 1 `Map.lookup` dict
      points1 = maybe Set.empty (Set.fromList . map (\(_, p) -> p)) probes1
      probes2 = RelayId 2 `Map.lookup` dict
      points2 = maybe Set.empty (Set.fromList . map (\(_, p) -> p)) probes2
  in counterexample (show probes1) $ 
    if Chain.null chain
        -- when a chain is null, the relay observer will never be triggered,
        -- since its chain never is never updated
      then isNothing probes1 .&&. isNothing probes2
      else
        -- tip of the chain and the length agree
          (probes1 >>= find (\(_, p) -> p == Chain.headPoint chain))
          ===
          Just (Chain.length chain, Chain.headPoint chain)
        .&&.
        -- both chains consists of the same set of points
          points === points1
        .&&.
        -- tip of the chain and the length agree
          (probes2 >>= find (\(_, p) -> p == Chain.headPoint chain))
          ===
          Just (Chain.length chain, Chain.headPoint chain)
        .&&.
        -- both chains consists of the same set of points
          points === points2

-- Node graph: c ↔ r ↔ c
coreToCoreViaRelaySim :: ( MonadSTM m stm
                         , MonadTimer m
                         , MonadSay m
                         , MonadProbe m
                         )
                      => Chain Block
                      -> Chain Block
                      -> Duration (Time m)
                      -> Duration (Time m)
                      -> Duration (Time m)
                      -> Probe m (NodeId, Int, Point)
                      -> m ()
coreToCoreViaRelaySim chain1 chain2 coreDelay coreTrDelay relayTrDelay probe = do
  (c1r1, r1c1) <- createTwoWaySubscriptionChannels coreTrDelay relayTrDelay
  -- (r1r2, r2r1) <- createTwoWaySubscriptionChannels relayTrDelay relayTrDelay
  -- (r2c2, c2r2) <- createTwoWaySubscriptionChannels relayTrDelay coreTrDelay
  (r1c2, c2r1) <- createTwoWaySubscriptionChannels relayTrDelay coreTrDelay

  fork $ void $ do
    cps <- coreNode (CoreId 1) coreDelay chain1 c1r1
    fork $ observeChainProducerState (CoreId 1) probe cps
  {--
    - fork $ void $ do
    -   cps <- relayNode (RelayId 1) (r1c1 <> r1r2)
    -   fork $ observeChainProducerState (RelayId 1) probe cps
    - fork $ void $ do
    -   cps <- relayNode (RelayId 2) (r2r1 <> r2c2)
    -   fork $ observeChainProducerState (RelayId 2) probe cps
    - fork $ void $ do
    -   cps <- coreNode (CoreId 2) coreDelay chain2 c2r2
    -   fork $ observeChainProducerState (CoreId 2) probe cps
    --}
  fork $ void $ do
    cps <- relayNode (RelayId 1) (r1c1 <> r1c2)
    fork $ observeChainProducerState (RelayId 1) probe cps
  fork $ void $ do
    cps <- coreNode (CoreId 2) coreDelay chain2 c2r1
    fork $ observeChainProducerState (CoreId 2) probe cps

runCoreToCoreViaRelaySim
  :: Chain Block
  -> Chain Block
  -> Sim.VTimeDuration
  -> Sim.VTimeDuration
  -> Sim.VTimeDuration
  -> [(Sim.VTime, (NodeId, Int, Point))]
runCoreToCoreViaRelaySim chain1 chain2 coreDelay coreTrDelay relayTrDelay = runST $ do
  probe <- newProbe
  runM $ coreToCoreViaRelaySim chain1 chain2 coreDelay coreTrDelay relayTrDelay probe
  readProbe probe

-- TODO: investigate why this test is failing
prop_coreToCoreViaRelay :: TestChainFork -> Property
prop_coreToCoreViaRelay (TestChainFork _ chain1 chain2) =
  let probes = map snd $ runCoreToCoreViaRelaySim chain1 chain2 (Sim.VTimeDuration 1) (Sim.VTimeDuration 1) (Sim.VTimeDuration 1)
      len1 = Chain.length chain1
      len2 = Chain.length chain2
  -- TODO: why when one chain is null this fails?
  in
        let -- chain = if len1 > len2 then chain1 else chain2
            dict  = partitionProbe probes
            tipC1 = Map.lookup (CoreId 1) dict  >>= headM >>= Just . snd
            tipC2 = Map.lookup (CoreId 2) dict  >>= headM >>= Just . snd
            tipR1 = Map.lookup (RelayId 1) dict >>= headM >>= Just . snd
            -- tipR2 = Map.lookup (RelayId 2) dict >>= headM >>= Just . snd
        in
               (counterexample "C1R1" $ tipC1 === tipR1)
          -- .&&. (counterexample "R1R2" $ tipR1 === tipR2)
          -- .&&. (counterexample "R1C2" $ tipR1 === tipC2)
          .&&. (counterexample "C1C2" $ tipC1 === tipC2)
  where
    headM :: [a] -> Maybe a
    headM []      = Nothing
    headM (a : _) = Just a

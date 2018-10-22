{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Test.Node where

import Control.Monad (forever)
import Control.Monad.ST.Lazy (runST)
import Data.Functor (void)
import Data.Maybe (isNothing, listToMaybe)
import Data.Semigroup ((<>))
import           Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map

import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Block
import qualified Chain
import           Chain (Chain (..))
import           Ouroboros
import Node
import MonadClass
import qualified Sim

import Test.Chain (TestBlockChain (..), TestChainFork (..))
import Test.ArbitrarySt

tests :: TestTree
tests =
  testGroup "Node"
  [ testGroup "fixed graph topology"
    [ testProperty "core -> relay" (withBft $ prop_coreToRelay @'OuroborosBFT)
    , testProperty "core -> relay -> relay" (withBft $ prop_coreToRelay2 @'OuroborosBFT)
    , testProperty "core <-> relay <-> core" (withBft $ prop_coreToCoreViaRelay @'OuroborosBFT)
    ]
  , testProperty "blockGenerator invariant (SimM)" (withBft $ prop_blockGenerator_ST @'OuroborosBFT)
  , testProperty "blockGenerator invariant (IO)" (withBft $ prop_blockGenerator_IO @'OuroborosBFT)
  ]


-- NOTE: it reverses the order of probes
partitionProbe :: [(NodeId, a)] -> Map NodeId [a]
partitionProbe
  = Map.fromListWith (++) . map (\(nid, a) -> (nid, [a]))

-- | Block generator should generate blocks in the correct slot time.
--
test_blockGenerator
  :: forall m stm p n.
     ( MonadSTM m stm
     , MonadTimer m
     , MonadProbe m
     , MonadRunProbe m n
     )
  => Chain (Block p)
  -> Duration (Time m)
  -> n Bool
test_blockGenerator chain slotDuration = isValid <$> withProbe (experiment slotDuration)
  where
    isValid :: [(Time m, Block p)] -> Bool
    isValid = all (\(t, b) -> t == fromStart ((fromIntegral . getSlot . blockSlot $ b) `mult` slotDuration))

    experiment
      :: ( MonadSTM m stm
         , MonadTimer m
         , MonadProbe m
         )
      => Duration (Time m)
      -> Probe m (Block p)
      -> m ()
    experiment slotDur p = do
      v <- blockGenerator slotDur chain
      fork $ forever $ do
        b <- atomically $ getBlock v
        probeOutput p b
      
prop_blockGenerator_ST :: TestBlockChain p -> Positive Rational -> Bool
prop_blockGenerator_ST (TestBlockChain chain) (Positive slotDuration) = runST $ test_blockGenerator chain (Sim.VTimeDuration slotDuration)

prop_blockGenerator_IO :: TestBlockChain p -> Positive Int -> Property
prop_blockGenerator_IO (TestBlockChain chain) (Positive slotDuration) = ioProperty $ test_blockGenerator chain slotDuration


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
    cps <- coreNode (CoreId 0) slotDuration chain coreChans
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
  runST $ do
    probe <- newProbe
    runM $ coreToRelaySim False chain slotDuration coreTransportDelay relayTransportDelay probe
    readProbe probe

data TestNodeSim p = TestNodeSim
  { testChain               :: Chain (Block p)
  , testSlotDuration        :: Sim.VTimeDuration
  , testCoreTransportDelay  :: Sim.VTimeDuration
  , testRealyTransportDelay :: Sim.VTimeDuration
  } deriving (Show, Eq)

instance KnownOuroborosProtocol p => ArbitrarySt p (TestNodeSim p) where
  arbitrarySt = do
    TestBlockChain testChain <- arbitrarySt
    -- at least twice as much as testCoreDelay
    Positive slotDuration <- lift arbitrary
    Positive testCoreTransportDelay <- lift arbitrary
    Positive testRelayTransportDelay <- lift arbitrary
    return $ TestNodeSim testChain (Sim.VTimeDuration slotDuration) (Sim.VTimeDuration testCoreTransportDelay) (Sim.VTimeDuration testRelayTransportDelay)

  -- TODO: shrink

-- this test relies on the property that when there is a single core node,
-- it will never have to use @'fixupBlock'@ function (which mangles blocks
-- picked up from the generator).  This is because all the nodes start with
-- @'Genesis'@ chain, hence the core node is a single source of truth.
prop_coreToRelay :: forall p. KnownOuroborosProtocol p => TestNodeSim p -> Property
prop_coreToRelay (TestNodeSim chain slotDuration coreTrDelay relayTrDelay) =
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
coreToRelaySim2 :: ( MonadSTM m stm
                   , MonadTimer m
                   , MonadSay m
                   , MonadProbe m
                   , KnownOuroborosProtocol p
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
    cps <- coreNode (CoreId 0) slotDuration chain cr1
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

prop_coreToRelay2 :: KnownOuroborosProtocol p => TestNodeSim p -> Property
prop_coreToRelay2 (TestNodeSim chain slotDuration coreTrDelay relayTrDelay) =
  let dict    = partitionProbe probes
      probes  = map snd $ runCoreToRelaySim2 chain slotDuration coreTrDelay relayTrDelay
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
            mchain1 === Just chain

-- | Node graph: c ↔ r ↔ c
coreToCoreViaRelaySim :: ( MonadSTM m stm
                         , MonadTimer m
                         , MonadSay m
                         , MonadProbe m
                         , KnownOuroborosProtocol p
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
    cps <- coreNode (CoreId 1) slotDuration chain1 c1r1
    fork $ observeChainProducerState (CoreId 1) probe cps
  fork $ void $ do
    cps <- relayNode (RelayId 1) (r1c1 <> r1c2)
    fork $ observeChainProducerState (RelayId 1) probe cps
  fork $ void $ do
    cps <- coreNode (CoreId 2) slotDuration chain2 c2r1
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
prop_coreToCoreViaRelay :: forall p. KnownOuroborosProtocol p 
                        => TestChainFork p
                        -> Property
prop_coreToCoreViaRelay (TestChainFork _ chain1 chain2) =
  let probes = map snd $ runCoreToCoreViaRelaySim chain1 chain2 (Sim.VTimeDuration 3) (Sim.VTimeDuration 1) (Sim.VTimeDuration 1)
  in
        let dict    = partitionProbe probes
            chainC1 = CoreId 1  `Map.lookup` dict >>= listToMaybe
            chainR1 = RelayId 1 `Map.lookup` dict >>= listToMaybe
            chainC2 = CoreId 2 `Map.lookup` dict >>= listToMaybe
        in
            isValid chainC1 chainR1 .&&. isValid chainC1 chainC2
  where
    isValid :: Maybe (Chain (Block p)) -> Maybe (Chain (Block p)) -> Property
    isValid Nothing   Nothing   = chain1 === Genesis .&&. chain2 === Genesis
    isValid (Just _)  Nothing   = property False
    isValid Nothing   (Just _)  = property False
    isValid (Just c1) (Just c2) = compareChains c1 c2

    compareChains :: Chain (Block p) -> Chain (Block p) -> Property
    compareChains c1 c2 =
        counterexample (c1_ ++ "\n\n" ++ c2_) (Chain.selectChain c1 c2 === c1)
      .&&.
        counterexample (c1_ ++ "\n\n" ++ c2_) (Chain.selectChain c2 c1 === c2)
      where
        nl  = "\n    "
        c1_ = Chain.prettyPrintChain nl show c1
        c2_ = Chain.prettyPrintChain nl show c2

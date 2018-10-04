{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}

module NodeExperiment1
  ( MsgConsumer(..)
  , MsgProducer(..)
  , ConsumerHandlers
  , ProducerHandlers
  , exampleConsumer
  , consumerSideProtocol1
  , exampleProducer
  , producerSideProtocol1
  , bindProducer

  , prop_producerToConsumer
  , prop_node
  ) where

import           Control.Monad
import           Control.Monad.Free (Free (..))
import           Control.Monad.ST.Lazy (runST)
import qualified Data.List as L
import           Data.Maybe (isJust)

import           Test.QuickCheck

import           Block (HasHeader (..))
import           Chain (Chain, genesis)
import qualified Chain
import           ChainProducerState
import           MonadClass
import           Sim ( ProbeTrace, SimF, Trace
                     , flipSimChan, newProbe, readProbe, runSimMST )
import qualified Sim
import           Protocol
import           ConsumersAndProducers


-- |
-- Simulate transfering a chain from a producer to a consumer
producerToConsumerSim
    :: ( HasHeader block
       , Eq block
       , Show block
       )
    => Sim.Probe s (Chain block)
    -> Chain block
    -- ^ chain to reproduce on the consumer; ought to be non empty
    -> Chain block
    -- ^ initial chain of the consumer; ought to be non empty
    -> Free (SimF s) ()
producerToConsumerSim v pchain cchain = do
    chan <- newChan

    -- run producer in a new thread
    fork $ do
        let send = loggingSend "1" (sendMsg chan)
            recv = loggingRecv "1" (recvMsg chan)
        chainvar <- atomically $ newTVar (ChainProducerState pchain [])
        producerSideProtocol1 (exampleProducer chainvar) send recv

    chainvar <- atomically $ newTVar cchain
    fork $
        let send = loggingSend "1" (sendMsg (flipSimChan chan))
            recv = loggingRecv "1" (recvMsg (flipSimChan chan)) in
        consumerSideProtocol1 (exampleConsumer chainvar) send recv

    say "done"
    timer 1 $ do
      chain <- atomically $ readTVar chainvar
      void $ probeOutput v chain

runProducerToConsumer
  :: ( HasHeader block
     , Eq block
     , Show block
     )
  => Chain block -- ^ producer chain
  -> Chain block -- ^ consumer chain
  -> (Trace, ProbeTrace (Chain block))
runProducerToConsumer pchain cchain = runST $ do
  v <- newProbe
  trace <- runSimMST (producerToConsumerSim v pchain cchain)
  probe <- readProbe v
  return (trace, probe)

prop_producerToConsumer :: Chain.TestChainFork -> Bool
prop_producerToConsumer (Chain.TestChainFork _ pchain cchain) =
    showFail $
    -- chain was transmitted
    rchain == pchain
  where
    (_tr, pr) = runProducerToConsumer pchain cchain
    rchain    = snd (last pr) -- ^ chain transferred to the consumer
    showFail  = id
{-
    showFail = counterexample $
                     "producer chain: "     ++ show pchain
                  ++ "\nconsumer chain: " ++ show cchain
                  ++ "\nresult chain: "   ++ show rchain
                  ++ "\ntrace:\n"
                  ++ unlines (map show $ filter Sim.filterTrace tr)
-}

-- |
-- Select chain from n consumers.
selectChainM
    :: forall block m stm.
       ( HasHeader block
       , Eq block
       , MonadFork m
       , MonadSTM m stm
       )
    => Chain block
    -- ^ initial producer's chain
    -> (Chain block -> Chain block -> Chain block)
    -- ^ pure chain selection
    -> [ TVar m (Chain block) ]
    -- ^ list of consumer's mvars
    -> m (TVar m (Chain block))
selectChainM chain select ts = do
  chainVar <- atomically $ newTVar chain
  stateVar <- atomically $ newTVar Chain.genesisPoint
  fork $ forever (atomically (go stateVar chainVar))
  return chainVar
  where
  go stateVar chainVar = do
    currentPoint    <- readTVar stateVar
    currentChain    <- readTVar chainVar
    candidateChains <- mapM readTVar ts
    let chain = L.foldl' select currentChain candidateChains
        point = Chain.headPoint chain
    check (currentPoint /= point)
    writeTVar stateVar point
    writeTVar chainVar chain

bindProducer
  :: forall block m stm.
    ( HasHeader block
    , Eq block
    , MonadSTM m stm
    , MonadFork m
    )
  => TVar m (Chain block)
  -> m (TVar m (ChainProducerState block))
bindProducer v = do
  cpsVar <- atomically $ do
    c <- readTVar v
    newTVar (ChainProducerState c [])

  fork $ forever $ do
    atomically $ do
      c   <- readTVar v
      cps <- readTVar cpsVar
      if (chainState cps /= c)
        then writeTVar cpsVar (switchFork c cps)
        else retry

  return cpsVar

bindConsumersToProducerN
  :: forall block m stm.
     ( HasHeader block
     , Eq block
     , MonadFork m
     , MonadSTM m stm
     )
  => Chain block
  -> (Chain block -> Chain block -> Chain block)
  -> [TVar m (Chain block)]
  -> m (TVar m (ChainProducerState block))
bindConsumersToProducerN chain select ts =
  selectChainM chain select ts >>= bindProducer

-- |
-- Simulate a node which is subscribed to two producers and has a single
-- subscription
--
--   producer1      producer2
--      |               |
--      V               V
-- ----------------------------
-- | listener1      listener2 |
-- |       \          /       |
-- |        \        /        |
-- |         \      /         |
-- |          \    /          |
-- |         producer         |
-- ----------------------------
--               |
--               v
--           listener
nodeSim
    :: ( HasHeader block
       , Eq block
       , Show block
       )
    => Sim.Probe s (Chain block)
    -> Chain block
    -- ^ initial chain of producer 1
    -> Chain block
    -- ^ initial chain of producer 2
    -> Free (SimF s) ()
nodeSim v chain1 chain2 = do
    chan1 <- newChan -- producer1 to listener1
    chan2 <- newChan -- producer2 to listener2
    chan3 <- newChan -- producer  to listener

    -- start producer1
    fork $ do
        let send = loggingSend "1" (sendMsg chan1)
            recv = loggingRecv "1" (recvMsg chan1)
        chainvar <- atomically $ newTVar (ChainProducerState chain1 [])
        producerSideProtocol1 (exampleProducer chainvar) send recv

    -- start producer2
    fork $ do
        let send = loggingSend "2" (sendMsg chan2)
            recv = loggingRecv "2" (recvMsg chan2)
        chainvar <- atomically $ newTVar (ChainProducerState chain2 [])
        producerSideProtocol1 (exampleProducer chainvar) send recv

    -- consumer listening to producer1
    chainvar1 <- atomically $ newTVar genesis
    fork $
        let send = loggingSend "1" (sendMsg (flipSimChan chan1))
            recv = loggingRecv "1" (recvMsg (flipSimChan chan1)) in
        consumerSideProtocol1 (exampleConsumer chainvar1) send recv

    -- consumer listening to producer2
    chainvar2 <- atomically $ newTVar genesis
    fork $
        let send = loggingSend "2" (sendMsg (flipSimChan chan2))
            recv = loggingRecv "2" (recvMsg (flipSimChan chan2)) in
        consumerSideProtocol1 (exampleConsumer chainvar2) send recv

    fork $ do
        let send = loggingSend "3" (sendMsg chan3)
            recv = loggingRecv "3" (recvMsg chan3)
        chainvar <- bindConsumersToProducerN
          genesis
          Chain.selectChain
          [chainvar1, chainvar2]
        producerSideProtocol1 (exampleProducer chainvar) send recv

    -- todo: use a fork here
    chainvar3 <- atomically $ newTVar genesis
    fork $
        let send = loggingSend "3" (sendMsg (flipSimChan chan3))
            recv = loggingRecv "3" (recvMsg (flipSimChan chan3)) in
        consumerSideProtocol1 (exampleConsumer chainvar3) send recv

    timer 1 $ do
      chain <- atomically $ readTVar chainvar3
      probeOutput v chain

runNodeSim
  :: ( HasHeader block
     , Eq block
     , Show block
     )
  => Chain block
  -> Chain block
  -> (Trace, ProbeTrace (Chain block))
runNodeSim pchain1 pchain2 = runST $ do
  v <- newProbe
  trace <- runSimMST (nodeSim v pchain1 pchain2)
  probe <- readProbe v
  return (trace, probe)

prop_node :: Chain.TestChainFork -> Property
prop_node (Chain.TestChainFork _ pchain1 pchain2) =
  -- TODO: it should not fail when chains have no intersection
  isJust (pchain1 `Chain.intersectChains` pchain2) ==>
  let (tr, pr) = runNodeSim pchain1 pchain2
      rchain = snd $ last pr
      schain = Chain.selectChain pchain1 pchain2
  in counterexample
      ("producer chain1: " ++ show pchain1 ++
       "\nprocuder chain2: " ++ show pchain2 ++
       "\nresult chain: " ++ show rchain ++
       "\nselect chain: " ++ show schain ++
       "\ntrace:\n" ++ unlines (map show tr)
       )
    $ rchain == schain


return []
runTests = $quickCheckAll

{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

module ConsumerProtocol
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
  )where

import           Prelude

-- import           Data.Word
import           Control.Applicative
-- import           Control.Concurrent.STM (STM, retry)
-- import           Control.Exception (assert)
import           Control.Monad
-- import           Control.Monad.ST.Lazy
import           Control.Monad.Free (Free (..))
import           Control.Monad.Free as Free
import           Control.Monad.ST.Lazy (runST)
import           Data.FingerTree (ViewL (..))
import           Data.FingerTree as FT
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.List as L
import           Data.Maybe (isJust)
-- import           Data.STRef.Lazy
import           System.Random (mkStdGen)

import           Test.QuickCheck

import           Block (Block (..), HasHeader (..))
import           Chain (Chain (..), ChainUpdate (..), Point (..), blockPoint, 
                        findIntersection)
import qualified Chain
import           ChainProducerState
import           MonadClass
import           SimSTM (ProbeTrace, SimChan (..), SimF, SimM, Trace, failSim, flipSimChan,
                         newProbe, readProbe, runSimM, runSimMST)
import qualified SimSTM

{-# ANN module "HLint: ignore Use readTVarIO" #-}

--
-- IPC based protocol
--

-- | In this protocol the consumer always initiates things and the producer
-- replies. This is the type of messages that the consumer sends.
data MsgConsumer = MsgRequestNext
                 | MsgSetHead Point [Point]
    deriving (Show)

-- | This is the type of messages that the producer sends.
data MsgProducer block
  = MsgRollForward  block
  | MsgRollBackward Point
  | MsgAwaitReply
  | MsgIntersectImproved Point
  | MsgIntersectUnchanged
    deriving (Show)

data ConsumerHandlers block m = ConsumerHandlers {
       getChainPoints :: m (Point, [Point]),
       addBlock       :: block -> m (),
       rollbackTo     :: Point -> m ()
     }

consumerSideProtocol1
  :: forall block cid m.
     ( Show block
     , MonadSay m
     , Show cid
     )
  => ConsumerHandlers block m
  -> cid                     -- ^ consumer id
  -> (MsgConsumer -> m ())   -- ^ send
  -> (m (MsgProducer block)) -- ^ recv
  -> m ()
consumerSideProtocol1 ConsumerHandlers{..} cid send recv = do
    -- The consumer opens by sending a list of points on their chain.
    -- This includes the head block and
    (hpoint, points) <- getChainPoints
    send (MsgSetHead hpoint points)
    _msg <- recv
    requestNext
  where
    consumerId :: String
    consumerId = "consumer-" ++ show cid

    requestNext :: m ()
    requestNext = do
      send MsgRequestNext
      reply <- recv
      handleChainUpdate reply
      requestNext

    handleChainUpdate :: MsgProducer block -> m ()
    handleChainUpdate msg@MsgAwaitReply = do
      say (consumerId ++ ":handleChainUpdate: " ++ show msg)

    handleChainUpdate msg@(MsgRollForward  b) = do
      say (consumerId ++ ":handleChainUpdate: " ++ show msg)
      addBlock b

    handleChainUpdate msg@(MsgRollBackward p) = do
      say (consumerId ++ ":handleChainUpdate: " ++ show msg)
      rollbackTo p

    handleChainUpdate msg = do
        say (consumerId ++ ":handleChainUpdate: " ++ show msg)


exampleConsumer :: forall block m stm.
                   ( Eq block
                   , HasHeader block
                   , MonadSay m
                   , MonadSTM m stm
                   )
                => TVar m (Chain block)
                -> ConsumerHandlers block m
exampleConsumer chainvar = ConsumerHandlers {..}
    where
    getChainPoints :: m (Point, [Point])
    getChainPoints = atomically $ do
        chain <- readTVar chainvar
        -- TODO: bootstraping case (client has no blocks)
        -- TODO: remove `toList`
        let (p : ps) = map blockPoint $ Chain.toList $ chain
        case map blockPoint $ Chain.toList chain of
          []     -> return (Chain.genesisPoint, [])
          p : ps -> return (p, ps)

    addBlock :: block -> m ()
    addBlock b = void $ atomically $ do
        chain <- readTVar chainvar
        let !chain' = Chain.applyChainUpdate (AddBlock b) chain
        when (chain /= chain')
          $ writeTVar chainvar chain'

    rollbackTo :: Point -> m ()
    rollbackTo p = atomically $ do
        chain <- readTVar chainvar
        let !chain' = Chain.applyChainUpdate (RollBack p) chain
        when (chain /= chain')
          $ writeTVar chainvar chain'

data ProducerHandlers block m r = ProducerHandlers {
       findIntersectionRange :: Point -> [Point] -> m (Maybe Point),
       establishReaderState  :: Point -> m r,
       updateReaderState     :: r -> Point -> m (),
       tryReadChainUpdate    :: r -> m (Maybe (ChainUpdate block)),
       readChainUpdate       :: r -> m (ChainUpdate block)
     }

-- |
-- TODO:
--  * n-consumers to producer (currently 1-consumer to producer)
producerSideProtocol1
  :: forall block pid m r.
     ( Show pid
     , HasHeader block
     , Show block
     , MonadSay m
     )
  => ProducerHandlers block m r
  -> pid                         -- ^ producer id
  -> (MsgProducer block -> m ()) -- ^ send
  -> (m MsgConsumer)             -- ^ recv
  -> m ()
producerSideProtocol1 ProducerHandlers{..} pid send recv =
    awaitOpening >>= maybe (return ()) awaitOngoing
  where
    producerId :: String
    producerId = show pid

    awaitOpening = do
      -- The opening message must be this one, to establish the reader state
      say (producerId ++ ":awaitOpening")
      msg <- recv
      case msg of
        MsgSetHead hpoint points -> do
          say $ producerId ++ ":awaitOpening:recvMsg: " ++ show msg
          intersection <- findIntersectionRange hpoint points
          case intersection of
            Just pt -> do
              r <- establishReaderState pt
              let msg = MsgIntersectImproved pt
              say $ producerId ++ ":awaitOpening:sendMsg: " ++ show msg
              send msg
              return (Just r)
            Nothing -> do
              let msg :: MsgProducer block
                  msg = MsgIntersectUnchanged
              say $ producerId ++ ":awaitOpening:sendMsg: " ++ show msg
              send msg
              awaitOpening
        MsgRequestNext -> do
          -- This message is received if the consumer's chain has no intersection
          -- with the producer's chain.  The producer will receive it as an
          -- answer to `MsgIntersectUnchanged`.
          say $ producerId ++ ":awaiOpening:recvMsg: " ++ show msg
          return Nothing

    awaitOngoing r = forever $ do
      msg <- recv
      say $ producerId ++ ":awaitOngoing:recvMsg: " ++ show msg
      case msg of
        MsgRequestNext           -> handleNext r
        MsgSetHead hpoint points -> handleSetHead r hpoint points

    handleNext r = do
      mupdate <- tryReadChainUpdate r
      update  <- case mupdate of
        Just update -> return update

        -- Reader is at the head, have to wait for producer state changes.
        Nothing -> do
          let msg :: MsgProducer block
              msg = MsgAwaitReply
          say $ producerId ++ ":handleNext:sendMsg: " ++ show msg
          send msg
          readChainUpdate r
      let msg = updateMsg update
      say $ producerId ++ ":handleNext:sendMsg: " ++ show msg
      send msg

    handleSetHead r hpoint points = do
      -- TODO: guard number of points, points sorted
      -- Find the most recent point that is on our chain, and the subsequent
      -- point which is not.
      intersection <- findIntersectionRange hpoint points
      case intersection of
        Just pt -> do
          updateReaderState r pt
          let msg :: MsgProducer block
              msg = MsgIntersectImproved pt
          say $ producerId ++ ":handleSetHead:sendMsg: " ++ show msg
          send msg
        Nothing -> do
          let msg :: MsgProducer block
              msg = MsgIntersectUnchanged
          say $ producerId ++ ":handleSetHead:sendMsg: " ++ show msg
          send msg

    updateMsg (AddBlock b) = MsgRollForward b
    updateMsg (RollBack p) = MsgRollBackward p

exampleProducer
  :: forall block m stm.
     ( HasHeader block
     , Eq block
     , MonadSay m
     , MonadSTM m stm
     )
  => TVar m (ChainProducerState block)
  -> ProducerHandlers block m ReaderId
exampleProducer chainvar =
    ProducerHandlers {..}
  where
    findIntersectionRange :: Point -> [Point] -> m (Maybe Point)
    findIntersectionRange hpoint points = do
      ChainProducerState {chainState} <- atomically $ readTVar chainvar
      return $! findIntersection chainState hpoint points

    establishReaderState :: Point -> m ReaderId
    establishReaderState ipoint = atomically $ do
      cps <- readTVar chainvar
      let (cps', rid) = initReader ipoint cps
      when (cps /= cps')
        $ writeTVar chainvar cps'
      return rid

    updateReaderState :: ReaderId -> Point -> m ()
    updateReaderState rid ipoint =
      atomically $ do
        cps <- readTVar chainvar
        let !cps' = updateReader rid ipoint cps
        when (cps /= cps')
          $ writeTVar chainvar cps'

    tryReadChainUpdate :: ReaderId -> m (Maybe (ChainUpdate block))
    tryReadChainUpdate rid =
      atomically $ do
        cps <- readTVar chainvar
        case readerInstruction rid cps of
          Nothing -> return Nothing
          Just (u, cps') -> do
            writeTVar chainvar cps'
            return $ Just u

    readChainUpdate :: ReaderId -> m (ChainUpdate block)
    readChainUpdate rid =
      atomically $ do
        cps <- readTVar chainvar
        case readerInstruction rid cps of
          Nothing        -> retry
          Just (u, cps') -> do
            writeTVar chainvar cps'
            return u

-- |
-- Simulate transfering a chain from a producer to a consumer
producerToConsumerSim
    :: ( HasHeader block
       , Eq block
       , Show block
       )
    => SimSTM.Probe s (Chain block)
    -> Chain block
    -- ^ chain to reproduce on the consumer; ought to be non empty
    -> Chain block
    -- ^ initial chain of the consumer; ought to be non empty
    -> Free (SimF s) ()
producerToConsumerSim v pchain cchain = do
    chan <- newChan

    -- run producer in a new thread
    fork $ do
        chainvar <- atomically $ newTVar (ChainProducerState pchain [])
        producerSideProtocol1 (exampleProducer chainvar) 1 (sendMsg chan) (recvMsg chan)

    chainvar <- atomically $ newTVar cchain
    fork $
        consumerSideProtocol1 (exampleConsumer chainvar) 1 (sendMsg (flipSimChan chan)) (recvMsg (flipSimChan chan))

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

prop_producerToConsumer :: Chain.TestChainFork -> Property
prop_producerToConsumer (Chain.TestChainFork _ pchain cchain) =
  let (tr, pr) = runProducerToConsumer pchain cchain
      rchain = snd $ last pr -- ^ chain transferred to the consumer
  in counterexample
      ("producer chain: "     ++ show pchain
      ++ "\nconsumer chain: " ++ show cchain
      ++ "\nresult chain: "   ++ show rchain
      ++ "\ntrace:\n"
      ++ unlines (map show $ filter SimSTM.filterTrace tr))
    $ case pchain `Chain.intersectChains` rchain of
        -- chain was transmitted
        Just _  -> rchain == pchain
        -- there's not intersection, so the protocol failed
        Nothing -> rchain == cchain

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
    => SimSTM.Probe s (Chain block)
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
        chainvar <- atomically $ newTVar (ChainProducerState chain1 [])
        producerSideProtocol1 (exampleProducer chainvar) 1 (sendMsg chan1) (recvMsg chan1)

    -- start producer2
    fork $ do
        chainvar <- atomically $ newTVar (ChainProducerState chain2 [])
        producerSideProtocol1 (exampleProducer chainvar) 2 (sendMsg chan2) (recvMsg chan2)

    -- consumer listening to producer1
    chainvar1 <- atomically $ newTVar Genesis
    fork $
        consumerSideProtocol1 (exampleConsumer chainvar1) 1 (sendMsg (flipSimChan chan1)) (recvMsg (flipSimChan chan1))

    -- consumer listening to producer2
    chainvar2 <- atomically $ newTVar Genesis
    fork $
        consumerSideProtocol1 (exampleConsumer chainvar2) 2 (sendMsg (flipSimChan chan2)) (recvMsg (flipSimChan chan2))

    fork $ do
        chainvar <- bindConsumersToProducerN
          Genesis
          Chain.selectChain
          [chainvar1, chainvar2]
        producerSideProtocol1 (exampleProducer chainvar) 3 (sendMsg chan3) (recvMsg chan3)

    -- todo: use a fork here
    chainvar3 <- atomically $ newTVar Genesis
    fork
      $ consumerSideProtocol1 (exampleConsumer chainvar3) 3 (sendMsg (flipSimChan chan3)) (recvMsg (flipSimChan chan3))

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

--
-- Simulation of composition of producer and consumer
--


-- | Given two sides of a protocol, ...
--
simulateWire
  :: forall p c s .
     (SimChan s p c -> Free (SimF s) ())
  -> (SimChan s c p -> Free (SimF s) ())
  -> Free (SimF s) ()
simulateWire protocolSideA protocolSideB = do
    chan <- newChan
    fork $ protocolSideA chan
    fork $ protocolSideB (flipSimChan chan)
    return ()

return []
runTests = $quickCheckAll

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

module ConsumerProtocol where

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
import           Data.Maybe (isJust)
-- import           Data.STRef.Lazy
import           System.Random (mkStdGen)

import           Test.QuickCheck

import           Block (Block (..), HasHeader (..))
import           Chain (Chain (..), ChainUpdate (..), Point (..), absChainFragment, applyChainUpdate, blockPoint, 
                        findIntersection)
import qualified Chain
import           ChainExperiment2
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
  :: forall block m.
     ( Show block
     , MonadSendRecv m
     , MonadSay m
     )
  => ConsumerHandlers block m
  -> Int  -- ^ consumer id
  -> BiChan m MsgConsumer (MsgProducer block)
  -> m ()
consumerSideProtocol1 ConsumerHandlers{..} n chan = do
    -- The consumer opens by sending a list of points on their chain.
    -- This includes the head block and
    (hpoint, points) <- getChainPoints
    sendMsg chan (MsgSetHead hpoint points)
    _msg <- recvMsg chan
    requestNext
  where
    consumerId :: String
    consumerId = "consumer-" ++ show n

    requestNext :: m ()
    requestNext = do
      sendMsg chan MsgRequestNext
      reply <- recvMsg chan
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

exampleConsumer :: forall block m stm. (Eq block, HasHeader block, MonadSay m, MonadSTM m stm)
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
        let !chain' = applyChainUpdate (AddBlock b) chain
        when (chain /= chain')
          $ writeTVar chainvar chain'

    rollbackTo :: Point -> m ()
    rollbackTo p = atomically $ do
        chain <- readTVar chainvar
        let !chain' = applyChainUpdate (RollBack p) chain
        when (chain /= chain')
          $ writeTVar chainvar chain'

data ProducerHandlers block m r = ProducerHandlers {
       findIntersectionRange :: Point -> [Point] -> m (Maybe Point),
       establishReaderState  :: Point -> Point -> m r,
       updateReaderState     :: r -> Point -> Maybe Point -> m (),
       tryReadChainUpdate    :: r -> m (Maybe (ConsumeChain block)),
       readChainUpdate       :: r -> m (ConsumeChain block)
     }

-- |
-- TODO:
--  * n-consumers to producer (currently 1-consumer to producer)
producerSideProtocol1
  :: forall block m r.
     ( HasHeader block
     , Show block
     , MonadSendRecv m
     , MonadSay m
     )
  => ProducerHandlers block m r
  -> Int -- producer id
  -> BiChan m (MsgProducer block) MsgConsumer
  -> m ()
producerSideProtocol1 ProducerHandlers{..} n chan =
    awaitOpening >>= maybe (return ()) awaitOngoing
  where
    producerId :: String
    producerId = "producer-" ++ show n

    awaitOpening = do
      -- The opening message must be this one, to establish the reader state
      say (producerId ++ ":awaitOpening")
      msg <- recvMsg chan
      case msg of
        MsgSetHead hpoint points -> do
          say $ producerId ++ ":awaitOpening:recvMsg: " ++ show msg
          intersection <- findIntersectionRange hpoint points
          case intersection of
            Just pt -> do
              r <- establishReaderState hpoint pt
              let msg = MsgIntersectImproved pt
              say $ producerId ++ ":awaitOpening:sendMsg: " ++ show msg
              sendMsg chan msg
              return (Just r)
            Nothing -> do
              let msg :: MsgProducer block
                  msg = MsgIntersectUnchanged
              say $ producerId ++ ":awaitOpening:sendMsg: " ++ show msg
              sendMsg chan msg
              awaitOpening
        MsgRequestNext -> do
          -- This message is received if the consumer's chain has no intersection
          -- with the producer's chain.  The producer will receive it as an
          -- answer to `MsgIntersectUnchanged`.
          say $ producerId ++ ":awaiOpening:recvMsg: " ++ show msg
          return Nothing

    awaitOngoing r = forever $ do
      msg <- recvMsg chan
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
          sendMsg chan msg
          readChainUpdate r
      let msg = updateMsg update
      say $ producerId ++ ":handleNext:sendMsg: " ++ show msg
      sendMsg chan msg

    handleSetHead r hpoint points = do
      -- TODO: guard number of points, points sorted
      -- Find the most recent point that is on our chain, and the subsequent
      -- point which is not.
      intersection <- findIntersectionRange hpoint points
      case intersection of
        Just pt -> do
          updateReaderState r hpoint (Just pt)
          let msg :: MsgProducer block
              msg = MsgIntersectImproved pt
          say $ producerId ++ ":handleSetHead:sendMsg: " ++ show msg
          sendMsg chan msg
        Nothing -> do
          updateReaderState r hpoint Nothing
          let msg :: MsgProducer block
              msg = MsgIntersectUnchanged
          say $ producerId ++ ":handleSetHead:sendMsg: " ++ show msg
          sendMsg chan msg

    updateMsg (RollForward  b) = MsgRollForward b
    updateMsg (RollBackward p) = MsgRollBackward p

exampleProducer
  :: forall block m stm.
     ( HasHeader block
     , Eq block
     , Show block
     , MonadSay m
     , MonadSTM m stm
     )
  => TVar m (ChainProducerState (Chain block))
  -> ProducerHandlers block m ReaderId
exampleProducer chainvar =
    ProducerHandlers {..}
  where
    findIntersectionRange :: Point -> [Point] -> m (Maybe Point)
    findIntersectionRange hpoint points = do
      ChainProducerState {chainState} <- atomically $ readTVar chainvar
      return $! findIntersection chainState hpoint points

    establishReaderState :: Point -> Point -> m ReaderId
    establishReaderState hpoint ipoint = atomically $ do
      cps <- readTVar chainvar
      let (cps', rid) = initialiseReader hpoint ipoint cps
      when (cps /= cps')
        $ writeTVar chainvar cps'
      return rid

    updateReaderState :: ReaderId -> Point -> Maybe Point -> m ()
    updateReaderState rid hpoint mipoint = do
      cps <- atomically $ do
        cps <- readTVar chainvar
        let !ncps = updateReader rid hpoint mipoint cps
        when (cps /= ncps)
          $ writeTVar chainvar ncps
        return ncps
      say $ "updateReaderState: " ++ show cps

    tryReadChainUpdate :: ReaderId -> m (Maybe (ConsumeChain block))
    tryReadChainUpdate rid = do
      (x, cc) <- atomically $ do
        cps <- readTVar chainvar
        case readerInstruction cps rid of
          Nothing        -> return ((cps, cps), Nothing)
          Just (cps', x) -> do
            when (cps /= cps')
              $ writeTVar chainvar cps'
            return ((cps, cps'), Just x)
      say $ "tryReadStateUpdate: " ++ show x ++ " " ++ show cc
      return cc

    readChainUpdate :: ReaderId -> m (ConsumeChain block)
    readChainUpdate rid = do
      (cps, x) <- atomically $ do
        cps <- readTVar chainvar
        case readerInstruction cps rid of
          Nothing        -> retry
          Just (cps', x) -> do
            when (cps /= cps')
              $ writeTVar chainvar cps'
            return (cps', x)
      say $ "readChainUpdate: " ++ show cps ++ " " ++ show x
      return x

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
        producerSideProtocol1 (exampleProducer chainvar) 1 chan

    chainvar <- atomically $ newTVar cchain
    fork $
        consumerSideProtocol1 (exampleConsumer chainvar) 1 (flipSimChan chan)

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

prop_producerToConsumer :: Chain.ChainFork -> Property
prop_producerToConsumer (Chain.ChainFork pchain cchain) =
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
  v <- atomically $ newTVar chain
  fork $ go v ts
  return v
  where
  go v [] = return ()
  go v (m:ms) = do
    fork $ listen v m
    go v ms

  listen
    :: TVar m (Chain block)
    -> TVar m (Chain block)
    -> m ()
  listen v m = forever $
      atomically $ do
        candidateChain <- readTVar m
        currentChain   <- readTVar v
        let !newChain = select currentChain candidateChain
        if newChain /= currentChain
          then writeTVar v newChain
          else retry

bindProducer
  :: forall block m stm.
    ( HasHeader block
    , Eq block
    , MonadSTM m stm
    , MonadFork m
    )
  => TVar m (Chain block)
  -> m (TVar m (ChainProducerState (Chain block)))
bindProducer v = do
  cpsVar <- atomically $ do
    c <- readTVar v
    newTVar (ChainProducerState c [])

  fork $ forever $ do
    atomically $ do
      c   <- readTVar v
      cps <- readTVar cpsVar
      if (chainState cps /= c)
        then
          let cps' = ChainProducerState
                { chainState   = c
                , chainReaders = map (updateReader c (chainState cps)) (chainReaders cps)
                }
          in writeTVar cpsVar cps'
        else retry

  return cpsVar

  where
  updateReader :: Chain block -> Chain block -> ReaderState -> ReaderState
  updateReader new old r@ReaderState {readerIntersection} =
    case Chain.lookupBySlot new (pointSlot readerIntersection) of
      -- reader intersection is on the new chain
      Just b | blockHash b == pointHash readerIntersection
             -> r
      -- reader intersection is not on the new chain
      _      ->
        case Chain.intersectChains new old of
          -- the two chains have intersection
          Just p -> r { readerIntersection = p }
          -- the two chains do not intersect
          _      -> r

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
  -> m (TVar m (ChainProducerState (Chain block)))
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
        producerSideProtocol1 (exampleProducer chainvar) 1 chan1

    -- start producer2
    fork $ do
        chainvar <- atomically $ newTVar (ChainProducerState chain2 [])
        producerSideProtocol1 (exampleProducer chainvar) 2 chan2

    -- consumer listening to producer1
    chainvar1 <- atomically $ newTVar Genesis
    fork $
        consumerSideProtocol1 (exampleConsumer chainvar1) 1 (flipSimChan chan1)

    -- consumer listening to producer2
    chainvar2 <- atomically $ newTVar Genesis
    fork $
        consumerSideProtocol1 (exampleConsumer chainvar2) 2 (flipSimChan chan2)

    fork $ do
        chainvar <- bindConsumersToProducerN
          Genesis
          Chain.selectChain
          [chainvar1, chainvar2]
        producerSideProtocol1 (exampleProducer chainvar) 3 chan3

    -- todo: use a fork here
    chainvar3 <- atomically $ newTVar Genesis
    fork
      $ consumerSideProtocol1 (exampleConsumer chainvar3) 3 (flipSimChan chan3)

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

prop_node :: Chain.ChainFork -> Property
prop_node (Chain.ChainFork pchain1 pchain2) =
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

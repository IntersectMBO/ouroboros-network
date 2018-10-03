{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
module Node where

import Data.List
import Data.Graph
import Data.Maybe (listToMaybe, catMaybes)
import Data.Functor (($>))
import Control.Applicative
import Control.Monad

import MonadClass hiding (sendMsg, recvMsg)
import Block
import qualified Chain
import           Chain (Chain (..), Point)
import ConsumerProtocol
import ChainProducerState (ChainProducerState (..), ReaderId, initChainProducerState, switchFork)

import qualified Sim

import Test.QuickCheck


longestChainSelection :: forall block m stm. MonadSTM m stm
                      => [TVar m (Maybe (Chain block))]
                      -> TVar m (Chain block)
                      -> m ()
longestChainSelection candidateChainVars currentChainVar =
    forever (atomically updateCurrentChain)
  where
    updateCurrentChain :: stm ()
    updateCurrentChain = do
      candidateChains <- mapM readTVar candidateChainVars
      currentChain    <- readTVar currentChainVar
      case longestChain (Chain.length currentChain) (catMaybes candidateChains) of
        Nothing -> retry
        Just c  -> writeTVar currentChainVar c

    longestChain :: Int -> [(Chain block)] -> Maybe (Chain block)
    longestChain curlen
      = fmap fst
      . listToMaybe
      . sortBy (\(_, l1) (_, l2) -> compare l1 l2)
      . filter (\(_, l) -> l > curlen)
      . map (\c -> (c, Chain.length c))

-- | 
-- State-full chain selection (@'ChainProducerState'@).
longestChainSelectionS :: forall block m stm.
                          ( HasHeader block
                          , MonadSTM m stm
                          )
                      => [TVar m (Maybe (Chain block))]
                      -> TVar m (ChainProducerState block)
                      -> m ()
longestChainSelectionS candidateChainVars cpsVar =
    forever (atomically updateCurrentChain)
  where
    updateCurrentChain :: stm ()
    updateCurrentChain = do
      candidateChains <- mapM readTVar candidateChainVars
      cps@ChainProducerState{chainState} <- readTVar cpsVar
      case longestChain (Chain.length chainState) (catMaybes candidateChains) of
        Nothing -> retry
        Just c  -> writeTVar cpsVar (switchFork c cps)

    longestChain :: Int -> [(Chain block)] -> Maybe (Chain block)
    longestChain curlen
      = fmap fst
      . listToMaybe
      . sortBy (\(_, l1) (_, l2) -> compare l1 l2)
      . filter (\(_, l) -> l > curlen)
      . map (\c -> (c, Chain.length c))


chainValidation :: forall block m stm. (HasHeader block, MonadSTM m stm)
                => TVar m (Chain block)
                -> TVar m (Maybe (Chain block))
                -> m ()
chainValidation peerChainVar candidateChainVar = do
    st <- atomically (newTVar Chain.genesisPoint)
    forever (atomically (update st))
  where
    update :: TVar m Point -> stm ()
    update stateVar = do
      peerChain      <- readTVar peerChainVar
      candidatePoint <- readTVar stateVar
      check (Chain.headPoint peerChain /= candidatePoint)
      writeTVar stateVar (Chain.headPoint peerChain)
      let candidateChain | Chain.valid peerChain = Just peerChain
                         | otherwise             = Nothing
      writeTVar candidateChainVar (candidateChain)


chainTransferProtocol :: forall block m stm.
                         ( HasHeader block
                         , MonadSTM m stm
                         , MonadTimer m
                         )
                      => Duration (Time m)
                      -> TVar m (Chain block)
                      -> TVar m (Chain block)
                      -> m ()
chainTransferProtocol delay inputVar outputVar = do
    st <- atomically (newTVar Chain.genesisPoint)
    forever (update st)
  where
    update :: TVar m Point -> m ()
    update stateVar = do
      input <- atomically $ do
                input    <- readTVar inputVar
                curPoint <- readTVar stateVar
                check (Chain.headPoint input /= curPoint)
                writeTVar stateVar (Chain.headPoint input)
                return input
      timer delay $ atomically $ writeTVar outputVar input


data Chan m send recv = Chan { sendMsg :: send -> m ()
                             , recvMsg :: m recv
                             }


transferProtocol :: forall send recv m stm.
                    ( MonadSTM m stm
                    , MonadTimer m
                    )
                 => Duration (Time m)
                 -> TVar m [send]
                 -> TVar m [recv]
                 -> Chan m send recv
transferProtocol delay sendVar recvVar
  = Chan sendMsg recvMsg
  where
    sendMsg a = timer delay $ atomically $ modifyTVar' sendVar (++[a])
    recvMsg =
      atomically $ do
        xs <- readTVar recvVar
        case xs of
          x : xs' -> writeTVar recvVar xs' $> x
          []      -> retry


createCoupledChannels :: forall send recv m stm.
                         ( MonadSTM m stm
                         , MonadTimer m
                         )
                      => Duration (Time m)
                      -> Duration (Time m)
                      -> m (Chan m send recv, Chan m recv send)
createCoupledChannels delay1 delay2 = do
  sendVar <- atomically $ newTVar []
  recvVar <- atomically $ newTVar []
  let chan1 = transferProtocol delay1 sendVar recvVar
      chan2 = transferProtocol delay2 recvVar sendVar
  return (chan1, chan2)


chainGenerator :: forall block m stm.
                  ( MonadSTM m stm
                  , MonadTimer m
                  )
               => Duration (Time m)
               -> Chain block
               -> m (TVar m (Chain block))
chainGenerator offset chain = do
    outputVar <- atomically (newTVar Genesis)
    sequence_ [ timer (offset + fromIntegral n) (atomically (writeTVar outputVar v))
              | (v, n) <- zip (inits chain) [0,2..] ]
    return outputVar
  where
    inits = reverse
          . unfoldr (\c -> case c of
                              Genesis -> Nothing
                              _       -> Just (c, Chain.drop 1 c))


observeChain :: forall block m stm.
                ( HasHeader block
                , MonadSTM m stm
                , MonadSay m
                )
             => String
             -> TVar m (Chain block)
             -> m ()
observeChain labelPrefix chainVar = do
    st <- atomically (newTVar Chain.genesisPoint)
    forever (update st)
  where
    update :: TVar m Point -> m ()
    update stateVar = do
      chain <- atomically $ do
                chain    <- readTVar chainVar
                curPoint <- readTVar stateVar
                check (Chain.headPoint chain /= curPoint)
                writeTVar stateVar (Chain.headPoint chain)
                return chain
      say (labelPrefix ++ ": " ++ show (Chain.length chain, Chain.headPoint chain))


observeChainProducerState
  :: forall block m stm.
     ( HasHeader block
     , MonadSTM m stm
     , MonadSay m
     )
  => NodeId
  -> TVar m (ChainProducerState block)
  -> m ()
observeChainProducerState nid cpsVar = do
    st <- atomically (newTVar Chain.genesisPoint)
    forever (update st)
  where
    update :: TVar m Point -> m ()
    update stateVar = do
      chain <- atomically $ do
                ChainProducerState{chainState = chain} <- readTVar cpsVar
                curPoint <- readTVar stateVar
                check (Chain.headPoint chain /= curPoint)
                writeTVar stateVar (Chain.headPoint chain)
                return chain
      say (show nid ++ ": " ++ show (Chain.length chain, Chain.headPoint chain))

nodeExample1 :: forall block m stm.
                ( HasHeader block
                , MonadSTM m stm
                , MonadTimer m
                , MonadSay m
                )
             => (Chain block)
             -> (Chain block)
             -> m ()
nodeExample1 c1 c2 = do
    generatorVar1 <- chainGenerator 1 c1
    generatorVar2 <- chainGenerator 2 c2

    peerChainVar1 <- atomically $ newTVar Genesis
    peerChainVar2 <- atomically $ newTVar Genesis

    candidateChainVar1 <- atomically $ newTVar Nothing
    candidateChainVar2 <- atomically $ newTVar Nothing
    let candidateChainVars = [candidateChainVar1, candidateChainVar2]

    currentChainVar <- atomically $ newTVar Genesis

    fork $ chainTransferProtocol 1 generatorVar1 peerChainVar1
    fork $ chainTransferProtocol 2 generatorVar2 peerChainVar2

    fork $ chainValidation peerChainVar1 candidateChainVar1
    fork $ chainValidation peerChainVar2 candidateChainVar2

    fork $ longestChainSelection candidateChainVars currentChainVar

    fork $ observeChain "generatorVar1" generatorVar1
    fork $ observeChain "generatorVar2" generatorVar2
    fork $ observeChain "currentChain " currentChainVar

newtype NodeId = NodeId Int
  deriving (Eq, Ord, Show)

data ConsumerId = ConsumerId NodeId Int
  deriving (Eq, Ord, Show)

data ProducerId = ProducerId NodeId Int
  deriving (Eq, Ord, Show)

relayNode :: forall block m stm.
        ( HasHeader block
        , Eq block
        , Show block
        , MonadSTM m stm
        , MonadTimer m
        , MonadSay m
        )
     => NodeId
     -> [Chan m MsgConsumer (MsgProducer block)] -- ^ input channels
     -> [Chan m (MsgProducer block) MsgConsumer] -- ^ output channels
     -> m (TVar m (ChainProducerState block))
relayNode nid inputs outputs = do

  -- Mutable state
  -- 1. input chains
  chainVars <- zipWithM startConsumer [0..] inputs
  -- 2. candidate chains
  candidateChainVars <- replicateM (length inputs) (atomically (newTVar Nothing))
  -- 3. ChainProducerState
  cpsVar <- atomically $ newTVar (initChainProducerState Genesis)

  -- chain validation threads
  zipWithM_
    (\chain cchain -> fork $ chainValidation chain cchain)
    chainVars
    candidateChainVars
  -- chain selection thread
  fork $ longestChainSelectionS candidateChainVars cpsVar

  -- producers
  let producer = exampleProducer cpsVar
  mapM_ (uncurry $ startProducer producer) (zip [0..] outputs)

  -- chain observer
  fork $ observeChainProducerState nid cpsVar

  return cpsVar
  where
    startConsumer :: Int
                  -> Chan m MsgConsumer (MsgProducer block)
                  -> m (TVar m (Chain block))
    startConsumer cid chan = do
      chainVar <- atomically $ newTVar Genesis
      let consumer = exampleConsumer chainVar
      fork $ consumerSideProtocol1 consumer (ConsumerId nid cid) (sendMsg chan) (recvMsg chan)
      return chainVar

    startProducer :: ProducerHandlers block m ReaderId
                  -> Int
                  -> Chan m (MsgProducer block) MsgConsumer
                  -> m ()
    startProducer producer pid chan = do
      fork $ producerSideProtocol1 producer (ProducerId nid pid) (sendMsg chan) (recvMsg chan)


coreNode :: forall block m stm.
        ( HasHeader block
        , Eq block
        , Show block
        , MonadSTM m stm
        , MonadTimer m
        , MonadSay m
        )
     => NodeId
     -> Duration (Time m)
     -> Chain block
     -> [Chan m MsgConsumer (MsgProducer block)] -- ^ input channels
     -> [Chan m (MsgProducer block) MsgConsumer] -- ^ output channels
     -> m ()
coreNode nid offset chain inputs outputs = do
  cpsVar <- relayNode nid inputs outputs

  chainVar <- chainGenerator offset chain
  fork $ forever (propagate chainVar cpsVar)

  where
    propagate :: TVar m (Chain block)
              -> TVar m (ChainProducerState block)
              -> m ()
    propagate chainVar cpsVar = atomically $ do
      chain <- readTVar chainVar
      cps@ChainProducerState{chainState} <- readTVar cpsVar
      check (Chain.length chain > Chain.length chainState)
      writeTVar cpsVar (switchFork chain cps)


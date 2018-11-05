{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
module LedgerState (
    spawnLedgerStateListeners
    ) where

import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.STM (TBQueue, TVar, atomically, modifyTVar',
                     newTVar, readTVar, retry, writeTVar)
import           Control.Monad.State
import           Data.Function ((&))
import           Data.Functor (($>))

import           Ouroboros.Consensus.Util
import           Ouroboros.Network.Chain (Chain (..), HasHeader)
import           Ouroboros.Network.ChainProducerState (ChainProducerState)
import           Ouroboros.Network.ConsumersAndProducers
import           Ouroboros.Network.Node (NodeId (..))
import           Ouroboros.Network.Protocol (consumerSideProtocol1,
                     producerSideProtocol1)
import           Ouroboros.Network.ProtocolInterfaces
import           Ouroboros.Network.Serialise

import           Logging


data SomeState = SomeState { howManyRollbacks :: !Int
                           , howManyAddBlocks :: !Int
                           } deriving Show

type M = StateT SomeState IO

spawnLedgerStateListeners :: forall block. ( HasHeader block
                             , Serialise block
                             , Condense  block
                             , Condense  [block]
                             )
                          => NodeId
                          -> TBQueue LogEvent
                          -> Chain block
                          -> TVar (ChainProducerState block)
                          -> IO [Async.Async ()]
spawnLedgerStateListeners ourselves q initialChain cps = do
    chainV <- atomically $ newTVar initialChain
    let handler = Logging.LoggerHandler q chainV ourselves

    let consumer = exampleConsumer chainV & (liftConsumerHandlers lift . Logging.addSimpleLogging handler)
                                          & addPostProcessing

    consumerVar <- atomically $ newTVar []
    producerVar <- atomically $ newTVar []

    ourOwnConsumer <- Async.async $ flip evalStateT (SomeState 0 0) $
                          consumerSideProtocol1 consumer
                          (lift . sendMsg consumerVar)
                          (lift $ recvMsg producerVar)

    ourOwnProducer <- Async.async $
                          producerSideProtocol1 (exampleProducer cps)
                          (sendMsg producerVar)
                          (recvMsg consumerVar)

    return [ourOwnProducer, ourOwnConsumer]
  where
    sendMsg sendVar a = atomically $ modifyTVar' sendVar (++[a])
    recvMsg recvVar =
      atomically $ do
        xs <- readTVar recvVar
        case xs of
          x : xs' -> writeTVar recvVar xs' $> x
          []      -> retry


addPostProcessing :: ConsumerHandlers block M -> ConsumerHandlers block M
addPostProcessing c = ConsumerHandlers {
      getChainPoints = getChainPoints c

    , addBlock = \b -> do
        addBlock c b
        modify (\s -> s { howManyAddBlocks = howManyAddBlocks s + 1 })
        get >>= lift . print

    , rollbackTo = \p -> do
        rollbackTo c p
        modify (\s -> s { howManyRollbacks = howManyRollbacks s + 1 })
        get >>= lift . print
    }




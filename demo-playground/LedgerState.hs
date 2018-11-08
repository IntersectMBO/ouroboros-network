{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
module LedgerState (
    spawnLedgerStateListeners
    ) where

import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.STM (TBQueue, TVar, atomically, modifyTVar',
                     newTVar, readTVar, retry, writeTVar)
import           Data.Functor (($>))

import           Ouroboros.Consensus.Infra.Util
import           Ouroboros.Network.Chain (Chain (..), HasHeader)
import           Ouroboros.Network.ChainProducerState
import           Ouroboros.Network.ConsumersAndProducers
import           Ouroboros.Network.Node (NodeId (..))
import           Ouroboros.Network.Protocol (consumerSideProtocol1,
                     producerSideProtocol1)
import           Ouroboros.Network.Serialise

import           Logging

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

    let consumer = (Logging.addSimpleLogging handler $ exampleConsumer chainV)

    consumerVar <- atomically $ newTVar []
    producerVar <- atomically $ newTVar []

    ourOwnConsumer <- Async.async $
                          consumerSideProtocol1 consumer
                          (sendMsg consumerVar)
                          (recvMsg producerVar)

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







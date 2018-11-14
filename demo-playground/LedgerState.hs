{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}
module LedgerState (
      spawnLedgerStateListeners
    , DemoLedgerState(..)
    ) where

import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.STM (TBQueue, TVar, atomically, modifyTVar',
                     newTVar, readTVar, retry, writeTVar)
import           Control.Monad.Reader
import           Data.Function ((&))
import           Data.Functor (($>))

import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Protocol.BFT (BftLedgerView (..))
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

data DemoLedgerState b = DemoLedgerState {
    ledgerState      :: !(LedgerState b)
    -- ^ The ledger state specific for the protocol in use.
  , howManyRollbacks :: !Int
  , howManyAddBlocks :: !Int
  , numNodes         :: !Int
  }

deriving instance (Show b, Show (LedgerState b)) => Show (DemoLedgerState b)

instance BftLedgerView (DemoLedgerState b) where
  bftNumNodes = fromIntegral . numNodes

type M b = ReaderT (TVar (DemoLedgerState b)) IO

spawnLedgerStateListeners :: forall block. ( HasHeader block
                             , Serialise block
                             , Condense  block
                             , Condense  [block]
                             , UpdateLedger block
                             , Show block
                             , Show (LedgerState block)
                             )
                          => NodeId
                          -> TBQueue LogEvent
                          -> Chain block
                          -> TVar (DemoLedgerState block)
                          -> TVar (ChainProducerState block)
                          -> IO [Async.Async ()]
spawnLedgerStateListeners ourselves q initialChain ledgerVar cps = do
    chainV <- atomically $ newTVar initialChain
    let handler = Logging.LoggerHandler q chainV ourselves

    let consumer = exampleConsumer chainV & (liftConsumerHandlers lift . Logging.addSimpleLogging handler)
                                          & addPostProcessing

    consumerVar <- atomically $ newTVar []
    producerVar <- atomically $ newTVar []

    ourOwnConsumer <- Async.async $ flip runReaderT ledgerVar $
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


addPostProcessing :: (Show block, Show (LedgerState block), UpdateLedger block)
                  => ConsumerHandlers block (M block)
                  -> ConsumerHandlers block (M block)
addPostProcessing c = ConsumerHandlers {
      getChainPoints = getChainPoints c

    , addBlock = \b -> do
        addBlock c b

        modifyLedger $ \l ->
            l {
              howManyAddBlocks = howManyAddBlocks l + 1
            , ledgerState = applyLedgerState b (ledgerState l)
            }

    , rollbackTo = \p -> do
        rollbackTo c p
        modifyLedger $ \l -> l { howManyRollbacks = howManyRollbacks l + 1 }
    }

modifyLedger :: (Show b, Show (LedgerState b))
             => (DemoLedgerState b -> DemoLedgerState b)
             -> M b ()
modifyLedger updateFn = do
    ledgerVar <- ask
    newLedger <- lift $ atomically $ do
        currentLedger <- readTVar ledgerVar
        let l' = updateFn currentLedger
        writeTVar ledgerVar l'
        return l'
    lift (print newLedger)


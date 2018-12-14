{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables  #-}
module LedgerState (
      spawnLedgerStateListeners
    , DemoLedgerState(..)
    ) where

import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.STM (TBQueue, TVar, atomically, modifyTVar',
                     newTVar, readTVar, retry, writeTVar)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Functor (($>))
import           Data.Text (Text, unpack)

import           Protocol.Driver
import           Protocol.Channel

import           Ouroboros.Network.Chain (Chain (..))
import           Ouroboros.Network.ChainProducerState (ChainProducerState)
import           Ouroboros.Network.Node (NodeId (..))

import           Ouroboros.Network.Protocol.ChainSync.Codec.Id (codecChainSync)
import           Ouroboros.Network.Protocol.ChainSync.Client
import           Ouroboros.Network.Protocol.ChainSync.Server
import           Ouroboros.Network.ChainSyncExamples

import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util

import           Logging

data DemoLedgerState b = DemoLedgerState {
      extLedgerState   :: !(ExtLedgerState b)
      -- ^ The ledger state specific for the protocol in use.
    , howManyRollbacks :: !Int
    , howManyAddBlocks :: !Int
    , numNodes         :: !Int
    }
  deriving (Show)

type M b = ReaderT (TVar (DemoLedgerState b)) IO

-- | Runs a consumer and producer where the consumer has the initial
-- Chain block, the producer uses the TVar (ChainProducerState block),
-- and all rolls forward/backward observed by the consumer are logged, by
-- way of the mutable DemoLedgerState block.
--
-- Previously there was logging of all handler invokations: getting
-- chain points, adding blocks, and rolling back.
-- I (avieth) argue that this isn't particularly useful, so I've left it
-- out. 'modifyLedger' is printing the ledger at every roll forward or
-- backward; should be enough logging, no?
spawnLedgerStateListeners :: forall block.
                             ( Condense block
                             , Condense [block]
                             , ProtocolLedgerView block
                             )
                          => NodeId
                          -> NodeConfig (BlockProtocol block)
                          -> TBQueue LogEvent
                          -> Chain block
                          -> TVar (DemoLedgerState block)
                          -> TVar (ChainProducerState block)
                          -> IO [Async.Async ()]
spawnLedgerStateListeners _ourselves cfg _q initialChain ledgerVar cps = do
    chainV <- atomically $ newTVar initialChain

    queueA <- atomically $ newTVar []
    queueB <- atomically $ newTVar []

    let -- Log rolls forward/backward and update the ledger state.
        -- We eliminate the ReaderT (TVar (DemoLedgerState block)) here
        -- because, if we didn't, then the whole client would be in that
        -- ReaderT, and we'd have to give a whack of instances for the
        -- monad simulation stuff, which would rightly seem like waste of time.
        client :: forall x . Client block IO x
        client = Client
          { rollforward = \b -> flip runReaderT ledgerVar $ do
              modifyLedger $ \l ->
                case runExcept (applyExtLedgerState cfg b (extLedgerState l)) of
                  Left err ->
                    error (show err) -- TODO: Proper error handling (Alfredo)
                  Right ledgerState' ->
                    l { howManyAddBlocks = howManyAddBlocks l + 1
                      , extLedgerState   = ledgerState'
                      }
              pure (Right client)
          , rollbackward = \_to _head -> flip runReaderT ledgerVar $ do
              modifyLedger $ \l -> l { howManyRollbacks = howManyRollbacks l + 1 }
              pure (Right client)
          , points = \_ -> pure client
          }

        throwOnUnexpected :: Result Text t -> IO t
        throwOnUnexpected (Unexpected txt) = error (unpack txt)
        throwOnUnexpected (Normal t)       = pure t

        consumerPeer = chainSyncClientPeer (chainSyncClientExample chainV client)
        producerPeer = chainSyncServerPeer (chainSyncServerExample () cps)
        consumerDuplex = uniformDuplex (sendMsg queueB) (recvMsg queueA)
        producerDuplex = uniformDuplex (sendMsg queueA) (recvMsg queueB)

    ourOwnConsumer <- Async.async $
      throwOnUnexpected =<< useCodecWithDuplex consumerDuplex codecChainSync consumerPeer

    ourOwnProducer <- Async.async $
      throwOnUnexpected =<< useCodecWithDuplex producerDuplex codecChainSync producerPeer

    return [ourOwnProducer, ourOwnConsumer]
  where
    sendMsg sendVar a = atomically $ modifyTVar' sendVar (++[a])
    recvMsg recvVar =
      atomically $ do
        xs <- readTVar recvVar
        case xs of
          x : xs' -> writeTVar recvVar xs' $> Just x
          []      -> retry

modifyLedger :: ProtocolLedgerView b
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

{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Logging (
    LogEvent -- opaque
  , LoggerHandler(..)
  , loggerConsumer
  , showNetworkTraffic
  ) where

import           Control.Concurrent (MVar, readMVar)
import           Control.Concurrent.STM
import           Control.Monad
import           Data.Function ((&))
import           Data.Semigroup ((<>))
import           GHC.Stack

import           Ouroboros.Consensus.Infra.Util
import           Ouroboros.Consensus.UTxO.Mempool
import qualified Ouroboros.Consensus.UTxO.Mock as Mock
import           Ouroboros.Network.Block
import           Ouroboros.Network.Chain (Chain (..))
import qualified Ouroboros.Network.Chain as Chain
import           Ouroboros.Network.ConsumersAndProducers
import           Ouroboros.Network.Node (NodeId)
import           Ouroboros.Network.ProtocolInterfaces
import           Ouroboros.Network.Testing.ConcreteBlock

import           Mock.TxSubmission (addMempool)

data LogEvent = LogEvent {
    msg    :: String
  , sender :: NodeId
  }

showNetworkTraffic :: HasCallStack => TBQueue LogEvent -> IO ()
showNetworkTraffic q = forever $ do
    LogEvent{..} <- atomically $ readTBQueue q
    putStrLn $ "[conv_with:" <> show sender <> "] " <> msg

instance Condense BlockHeader where
    condense BlockHeader{..} =
      "{hash: " <> condense headerHash
                <> ", blockNo: "
                <> condense headerBlockNo
                <> "}"

data LoggerHandler block = LoggerHandler {
    loggingQueue :: TBQueue LogEvent
  , chainVar     :: TVar (Chain block)
  , mempoolVar   :: MVar (Mempool Mock.Tx)
  }

-- | Add logging to the example consumer
loggerConsumer :: forall block. ( Mock.HasUtxo block
                                , Condense [block]
                                , Condense block
                                , HasHeader block
                                )
               => LoggerHandler block
               -> NodeId
               -> ConsumerHandlers block IO
loggerConsumer LoggerHandler{..} ourProducer =
    exampleConsumer chainVar & addLogging
                             & addMempool mempoolVar chainVar
  where
    addLogging :: ConsumerHandlers block IO -> ConsumerHandlers block IO
    addLogging c = ConsumerHandlers {
          getChainPoints = do
            pts <- getChainPoints c
            logMsg $ "getChainPoints, sending " <> show pts <> " to " <> show ourProducer
            return pts

        , addBlock = \b -> do
            logMsg $ "Received " <> condense b <> " from " <> show ourProducer
            addBlock c b
            logMempool
            logChain

        , rollbackTo = \p -> do
            logMsg $ "Rolling back to " <> show p
            rollbackTo c p
            logChain
        }

    logChain :: IO ()
    logChain = atomically $ do
        chain <- readTVar chainVar
        let m = "Current chain candidate: " <> condense (Chain.toOldestFirst chain)
        writeTBQueue loggingQueue $ LogEvent m ourProducer

    logMempool :: IO ()
    logMempool = do
        pool <- readMVar mempoolVar
        let m = "My mempool: " <> condense pool
        atomically $ writeTBQueue loggingQueue $ LogEvent m ourProducer

    logMsg :: String -> IO ()
    logMsg m = atomically $ writeTBQueue loggingQueue $ LogEvent m ourProducer

{-------------------------------------------------------------------------------
  Orphans
-------------------------------------------------------------------------------}

deriving instance Condense BlockNo
deriving instance Condense ConcreteHeaderHash

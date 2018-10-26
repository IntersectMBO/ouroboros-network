{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Logging (
    LogEvent -- opaque
  , loggerConsumer
  , showNetworkTraffic
  ) where

import           Control.Concurrent.STM
import           Control.Monad
import           Data.Semigroup ((<>))
import           GHC.Stack

import           Block
import           Chain (Chain (..))
import qualified Chain
import           ConsumersAndProducers
import           Ouroboros (NodeId)
import           ProtocolInterfaces

data LogEvent = LogEvent {
    msg    :: String
  , sender :: NodeId
  }

showNetworkTraffic :: HasCallStack => TBQueue LogEvent -> IO ()
showNetworkTraffic q = forever $ do
    LogEvent{..} <- atomically $ readTBQueue q
    putStrLn $ "[conv_with:" <> show sender <> "] " <> msg

-- | Add logging to the example consumer
loggerConsumer :: forall p block. (Show (block p), HasHeader block)
               => TBQueue LogEvent
               -> TVar (Chain (block p))
               -> NodeId
               -> ConsumerHandlers (block p) IO
loggerConsumer q chainvar ourProducer =
    addLogging $ exampleConsumer chainvar
  where
    addLogging :: ConsumerHandlers (block p) IO -> ConsumerHandlers (block p) IO
    addLogging c = ConsumerHandlers {
          getChainPoints = do
            pts <- getChainPoints c
            logMsg $ "getChainPoints, sending " <> show pts <> " to " <> show ourProducer
            return pts

        , addBlock = \b -> do
            logMsg $ "Received " <> show b <> " from " <> show ourProducer
            addBlock c b
            logChain

        , rollbackTo = \p -> do
            logMsg $ "Rolling back to " <> show p
            rollbackTo c p
            logChain
        }

    logChain :: IO ()
    logChain = atomically $ do
        chain <- readTVar chainvar
        let m = "Current chain candidate: " <> show (Chain.toList chain)
        writeTBQueue q $ LogEvent m ourProducer

    logMsg :: String -> IO ()
    logMsg m = atomically $ writeTBQueue q $ LogEvent m ourProducer

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
  , addDetailedLogging
  , addSimpleLogging
  , loggerConsumer
  , showNetworkTraffic
  ) where

import           Control.Concurrent.STM
import           Control.Monad
import           Data.Semigroup ((<>))

import           Ouroboros.Consensus.Util
import           Ouroboros.Network.Block
import           Ouroboros.Network.Chain (Chain (..))
import qualified Ouroboros.Network.Chain as Chain
import           Ouroboros.Network.ConsumersAndProducers
import           Ouroboros.Network.Node (NodeId)
import           Ouroboros.Network.ProtocolInterfaces
import           Ouroboros.Network.Testing.ConcreteBlock

data LogEvent = LogEvent {
    msg    :: String
  , sender :: NodeId
  }

showNetworkTraffic :: TBQueue LogEvent -> IO ()
showNetworkTraffic q = forever $ do
    LogEvent{..} <- atomically $ readTBQueue q
    -- Add an extra newline to help readability.
    putStrLn $ "[conv_with:" <> show sender <> "] " <> msg <> "\n"

instance Condense BlockHeader where
    condense BlockHeader{..} =
      "{hash: " <> condense headerHash
                <> ", blockNo: "
                <> condense headerBlockNo
                <> "}"

data LoggerHandler block = LoggerHandler {
    loggingQueue :: TBQueue LogEvent
  , chainVar     :: TVar (Chain block)
  , ourProducer  :: NodeId
  }

-- | Add logging to the example consumer
loggerConsumer :: forall block. ( Condense [block]
                                , Condense block
                                , HasHeader block
                                )
               => LoggerHandler block
               -> ConsumerHandlers block IO
loggerConsumer handler =
    addDetailedLogging handler $ exampleConsumer (chainVar handler)

addDetailedLogging :: ( HasHeader block
                      , Condense block
                      , Condense [block]
                      )
                   => LoggerHandler block
                   -> ConsumerHandlers block IO
                   -> ConsumerHandlers block IO
addDetailedLogging hdl@LoggerHandler{..} c = ConsumerHandlers {
      getChainPoints = do
        pts <- getChainPoints c
        logMsg hdl $ "getChainPoints, sending " <> show pts <> " to " <> show ourProducer
        return pts

    , addBlock = \b -> do
        logMsg hdl $ "Received " <> condense b <> " from " <> show ourProducer
        addBlock c b
        logChain hdl

    , rollbackTo = \p -> do
        logMsg hdl $ "Rolling back to " <> show p
        rollbackTo c p
        logChain hdl
    }

addSimpleLogging :: ( HasHeader block
                    , Condense block
                    , Condense [block]
                    )
                 => LoggerHandler block
                 -> ConsumerHandlers block IO
                 -> ConsumerHandlers block IO
addSimpleLogging hdl@LoggerHandler{..} c = ConsumerHandlers {
      getChainPoints = do
        pts <- getChainPoints c
        logMsg hdl "GetChainPoints"
        return pts

    , addBlock = \b -> do
        logMsg hdl $ "AddBlock: " <> condense b
        addBlock c b

    , rollbackTo = \p -> do
        logMsg hdl $ "Rollback: " <> condensedPoint p
        rollbackTo c p
    }
  where
      condensedPoint :: HasHeader block => Chain.Point block -> String
      condensedPoint (Chain.Point s h) = "P["
                                      <> show (getSlot s)
                                      <> ","
                                      <> show h <> "]"

logChain :: ( Condense block
            , Condense [block]
            ) => LoggerHandler block -> IO ()
logChain LoggerHandler{..} = atomically $ do
    chain <- readTVar chainVar
    let m = "Current chain candidate: " <> condense (Chain.toOldestFirst chain)
    writeTBQueue loggingQueue $ LogEvent m ourProducer

logMsg :: LoggerHandler block -> String -> IO ()
logMsg LoggerHandler{..} m = atomically $
    writeTBQueue loggingQueue $ LogEvent m ourProducer

{-------------------------------------------------------------------------------
  Orphans
-------------------------------------------------------------------------------}

deriving instance Condense BlockNo
deriving instance Condense ConcreteHeaderHash

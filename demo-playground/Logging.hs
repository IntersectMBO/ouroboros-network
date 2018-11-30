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
import           Ouroboros.Network.Node (NodeId)
import           Ouroboros.Network.Testing.ConcreteBlock
import           Ouroboros.Network.ChainSyncExamples

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
loggerConsumer :: forall block x. ( Condense [block]
                                  , Condense block
                                  , HasHeader block
                                  )
               => LoggerHandler block
               -> Client block IO x
loggerConsumer handler =
    addDetailedLogging handler pureClient

addDetailedLogging :: ( HasHeader block
                      , Condense block
                      , Condense [block]
                      )
                   => LoggerHandler block
                   -> Client block IO x
                   -> Client block IO x
addDetailedLogging hdl@LoggerHandler{..} c = c {
      points = \pts -> do
        next <- points c pts
        logMsg hdl $ "getChainPoints, sending " <> show pts <> " to " <> show ourProducer
        return next

    , rollforward = \b -> do
        logMsg hdl $ "Received " <> condense b <> " from " <> show ourProducer
        next <- rollforward c b
        logChain hdl
        pure next

    , rollbackward = \p1 p2 -> do
        logMsg hdl $ "Rolling back to " <> show p1
        next <- rollbackward c p1 p2
        logChain hdl
        pure next
    }

addSimpleLogging :: ( HasHeader block
                    , Condense block
                    , Condense [block]
                    )
                 => LoggerHandler block
                 -> Client block IO x
                 -> Client block IO x
addSimpleLogging hdl@LoggerHandler{..} c = Client {
      points = \pts -> do
        next <- points c pts
        logMsg hdl "GetChainPoints"
        return next

    , rollforward = \b -> do
        logMsg hdl $ "AddBlock: " <> condense b
        rollforward c b

    , rollbackward = \p1 p2 -> do
        logMsg hdl $ "Rollback: " <> condensedPoint p1
        rollbackward c p1 p2
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

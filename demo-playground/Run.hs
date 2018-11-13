{-# LANGUAGE EmptyDataDecls      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Run (
      runNode
    ) where

import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.STM (TBQueue, TVar, atomically, newTBQueue,
                     newTVar)
import           Control.Monad
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Semigroup ((<>))

import           Ouroboros.Consensus.Crypto.DSIGN.Mock
import           Ouroboros.Consensus.Crypto.Hash
import qualified Ouroboros.Consensus.Ledger.Mock as Mock
import           Ouroboros.Consensus.Protocol.BFT
import           Ouroboros.Network.Chain (Chain (..))
import           Ouroboros.Network.ChainProducerState
import           Ouroboros.Network.ConsumersAndProducers
import           Ouroboros.Network.Node (NodeId (..))
import qualified Ouroboros.Network.Node as Node

import           BlockGeneration (forkCoreNode)
import           CLI
import           LedgerState
import           Logging
import           Mock.Mempool (Mempool)
import           Mock.TxSubmission
import qualified NamedPipe
import           Topology

runNode :: CLI -> IO ()
runNode CLI{..} = do
    -- If the user asked to submit a transaction, we don't have to spin up a
    -- full node, we simply transmit it and exit.
    case command of
         TxSubmitter topology tx -> handleTxSubmission topology tx
         SimpleNode t            -> handleSimpleNode t

-- The concrete block this demo will run with.
data DemoCrypto

instance Mock.SimpleBlockCrypto DemoCrypto where
  type SimpleBlockHash DemoCrypto = ShortHash

type Block = Mock.SimpleBlock (Bft BftMockCrypto) DemoCrypto

-- | Setups a simple node, which will run the chain-following protocol and,
-- if core, will also look at the mempool when trying to create a new block.
handleSimpleNode :: TopologyInfo -> IO ()
handleSimpleNode (TopologyInfo myNodeId topologyFile) = do
    let isLogger   = myNodeId == CoreId 0

    topoE <- readTopologyFile topologyFile
    case topoE of
         Left e -> error e
         Right t@(NetworkTopology nodeSetups) -> do
             let topology  = toNetworkMap t
                 nodeSetup = fromMaybe (error "node not found.") $
                                   M.lookup myNodeId topology

             putStrLn $ "**************************************"
             putStrLn $ "I am Node = " <> show myNodeId
             putStrLn $ "My consumers are " <> show (consumers nodeSetup)
             putStrLn $ "My producers are " <> show (producers nodeSetup)
             putStrLn $ "**************************************"

             -- Creates a TBQueue to be used by all the logger threads to monitor
             -- the traffic.
             loggingQueue    <- atomically $ newTBQueue 50
             terminalThread  <- (:[]) <$> spawnTerminalLogger loggingQueue

             let initialPool :: Mempool Mock.Tx
                 initialPool = mempty

             -- Each node has a mempool, regardless from its consumer
             -- and producer threads.
             nodeMempool <- atomically $ newTVar initialPool


             -- The calls to the 'Unix' functions are flipped here, as for each
             -- of my producers I want to create a consumer node and for each
             -- of my consumers I want to produce something.
             (upstream, consumerThreads) <-
               fmap unzip $ forM (producers nodeSetup) $ \pId ->
                 case isLogger of
                      True  -> spawnLogger loggingQueue pId
                      False -> spawnConsumer Genesis pId

             cps <- atomically $ newTVar (initChainProducerState Genesis)
             producerThreads <- forM (consumers nodeSetup) (spawnProducer cps)

             -- Spawn the thread which listens to the mempool.
             mempoolThread <-
                 case (role nodeSetup) of
                     CoreNode -> (:[]) <$> spawnMempoolListener myNodeId nodeMempool cps
                     _ -> mempty

             Node.forkRelayKernel upstream cps
             when (role nodeSetup == CoreNode) $ do
                 let numCoreNodes = length (filter ((== CoreNode) . role) nodeSetups)
                 let nid = case myNodeId of
                                CoreId n  -> n
                                RelayId n -> n
                 let protocolState = BftState myNodeId (SignKeyMockDSIGN nid)
                 let ledgerState = DemoLedgerState numCoreNodes

                 forkCoreNode ledgerState
                              protocolState
                              nodeMempool
                              cps
                              slotDuration

             ledgerStateThreads <- spawnLedgerStateListeners myNodeId
                                                             loggingQueue
                                                             Genesis
                                                             cps

             let allThreads = terminalThread <> producerThreads
                                             <> consumerThreads
                                             <> mempoolThread
                                             <> ledgerStateThreads
             void $ Async.waitAnyCancel allThreads

  where
      spawnTerminalLogger :: TBQueue LogEvent -> IO (Async.Async ())
      spawnTerminalLogger q = do
          Async.async $ showNetworkTraffic q

      spawnLogger :: TBQueue LogEvent
                  -> NodeId
                  -> IO (TVar (Chain Block), Async.Async ())
      spawnLogger q targetId = do
          chVar <- atomically $ newTVar Genesis
          let handler = LoggerHandler q chVar targetId
          a     <- Async.async $ NamedPipe.runConsumer myNodeId targetId $
                     loggerConsumer handler
          pure (chVar, a)

      spawnConsumer :: Chain Block
                    -> NodeId
                    -> IO (TVar (Chain Block), Async.Async ())
      spawnConsumer myChain producerNodeId = do
          chVar <- atomically $ newTVar myChain
          a     <- Async.async $ NamedPipe.runConsumer myNodeId producerNodeId $
                     exampleConsumer chVar
          pure (chVar, a)

      spawnProducer :: TVar (ChainProducerState Block)
                    -> NodeId
                    -> IO (Async.Async ())
      spawnProducer cps consumerNodeId = Async.async $
          NamedPipe.runProducer myNodeId consumerNodeId $
            exampleProducer cps

slotDuration :: Int
slotDuration = 5 * 1000000

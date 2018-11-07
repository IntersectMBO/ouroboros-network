{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

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

import           Ouroboros.Consensus.Infra.Singletons (Dict (..), withSomeSing)
import           Ouroboros.Consensus.Infra.Util
import           Ouroboros.Consensus.UTxO.Mempool (Mempool)
import qualified Ouroboros.Consensus.UTxO.Mock as Mock
import           Ouroboros.Network.Chain (Chain (..), HasHeader)
import           Ouroboros.Network.ChainProducerState
import           Ouroboros.Network.ConsumersAndProducers
import           Ouroboros.Network.MonadClass hiding (TVar, atomically, newTVar)
import           Ouroboros.Network.Node (NodeId (..))
import qualified Ouroboros.Network.Node as Node
import           Ouroboros.Network.Serialise hiding ((<>))

import           BlockGeneration (blockGenerator)
import           CLI
import           Logging
import           Mock.TxSubmission
import qualified NamedPipe
import           Payload
import           Topology

dictPayloadImplementation :: Sing (pt :: PayloadType)
                          -> Dict ( Serialise (Payload pt)
                                  , Condense  (Payload pt)
                                  , Condense  [Payload pt]
                                  , HasHeader (Payload pt)
                                  , PayloadImplementation pt
                                  , Mock.HasUtxo (Payload pt)
                                  , Mock.HasUtxo (Chain (Payload pt))
                                  )
dictPayloadImplementation SDummyPayload = Dict
dictPayloadImplementation SMockPayload  = Dict

runNode :: CLI -> IO ()
runNode CLI{..} = do
    -- If the user asked to submit a transaction, we don't have to spin up a
    -- full node, we simply transmit it and exit.
    case command of
         TxSubmitter topology tx -> handleTxSubmission topology tx
         -- Use the mock payload as default.
         SimpleNode t            -> handleSimpleNode t MockPayloadType

-- | Setups a simple node, which will run the chain-following protocol and,
-- if core, will also look at the mempool when trying to create a new block.
handleSimpleNode :: TopologyInfo -> PayloadType -> IO ()
handleSimpleNode (TopologyInfo myNodeId topologyFile) payloadType = do
    let isLogger   = myNodeId == CoreId 0

    topoE <- readTopologyFile topologyFile
    case topoE of
         Left e -> error e
         Right t -> do
             let topology      = toNetworkMap t
                 NodeSetup{..} = fromMaybe (error "node not found.") $
                                   M.lookup myNodeId topology

             putStrLn $ "**************************************"
             putStrLn $ "I am Node = " <> show myNodeId
             putStrLn $ "My consumers are " <> show consumers
             putStrLn $ "My producers are " <> show producers
             putStrLn $ "**************************************"

             -- Creates a TBQueue to be used by all the logger threads to monitor
             -- the traffic.
             loggingQueue    <- atomically $ newTBQueue 50
             terminalThread  <-
               case isLogger of
                    True  -> (:[]) <$> spawnTerminalLogger loggingQueue
                    False -> mempty

             withSomeSing payloadType $ \(sPayloadType :: Sing (pt :: PayloadType)) ->
               case dictPayloadImplementation sPayloadType of
                 Dict -> do
                   let initialChain :: Chain (Payload pt)
                       initialChain = toChain initialChainData

                       initialPool :: Mempool Mock.Tx
                       initialPool = mempty

                   -- Each node has a mempool, regardless from its consumer
                   -- and producer threads.
                   nodeMempool <- newMVar initialPool


                   -- The calls to the 'Unix' functions are flipped here, as for each
                   -- of my producers I want to create a consumer node and for each
                   -- of my consumers I want to produce something.
                   (upstream, consumerThreads) <-
                     fmap unzip $ forM producers $ \pId ->
                       case isLogger of
                            True  -> spawnLogger loggingQueue pId
                            False -> spawnConsumer initialChain pId

                   cps <- atomically $ newTVar (initChainProducerState initialChain)
                   producerThreads <- forM consumers (spawnProducer cps)

                   -- Spawn the thread which listens to the mempool.
                   mempoolThread <-
                       case role of
                           CoreNode -> (:[]) <$> spawnMempoolListener myNodeId nodeMempool cps
                           _ -> mempty

                   Node.forkRelayKernel upstream cps
                   when (role == CoreNode) $ do
                       blockVar <- blockGenerator nodeMempool
                                                  cps
                                                  slotDuration
                                                  (chainFrom initialChain 100)
                       Node.forkCoreKernel blockVar
                                           fixupBlock
                                           cps


                   let allThreads = terminalThread <> producerThreads
                                                   <> consumerThreads
                                                   <> mempoolThread
                   void $ Async.waitAnyCancel allThreads

  where
      spawnTerminalLogger :: TBQueue LogEvent -> IO (Async.Async ())
      spawnTerminalLogger q = do
          Async.async $ showNetworkTraffic q

      spawnLogger :: ( HasHeader (Payload pt)
                     , Serialise (Payload pt)
                     , Condense  (Payload pt)
                     , Condense  [Payload pt]
                     )
                  => TBQueue LogEvent
                  -> NodeId
                  -> IO (TVar (Chain (Payload pt)), Async.Async ())
      spawnLogger q targetId = do
          chVar <- atomically $ newTVar Genesis
          let handler = LoggerHandler q chVar
          a     <- Async.async $ NamedPipe.runConsumer myNodeId targetId $
                     loggerConsumer handler targetId
          pure (chVar, a)

      spawnConsumer :: ( HasHeader (Payload pt)
                       , Serialise (Payload pt)
                       )
                    => Chain (Payload pt)
                    -> NodeId
                    -> IO (TVar (Chain (Payload pt)), Async.Async ())
      spawnConsumer myChain producerNodeId = do
          chVar <- atomically $ newTVar myChain
          a     <- Async.async $ NamedPipe.runConsumer myNodeId producerNodeId $
                     exampleConsumer chVar
          pure (chVar, a)

      spawnProducer :: ( HasHeader (Payload pt)
                       , Serialise (Payload pt)
                       )
                    => TVar (ChainProducerState (Payload pt))
                    -> NodeId
                    -> IO (Async.Async ())
      spawnProducer cps consumerNodeId = Async.async $
          NamedPipe.runProducer myNodeId consumerNodeId $
            exampleProducer cps

slotDuration :: Int
slotDuration = 5 * 1000000

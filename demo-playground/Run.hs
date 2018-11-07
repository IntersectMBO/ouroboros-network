{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Run (
      runNode
    ) where

import           Control.Concurrent (newMVar)
import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.STM (TBQueue, TVar, atomically, newTBQueue,
                     newTVar)
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.ByteString as B
import           Data.Foldable
import           Data.Map (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Semigroup ((<>))
import           Data.String.Conv (toS)

import           Ouroboros.Consensus.Infra.Singletons (Dict (..), withSomeSing)
import           Ouroboros.Consensus.Infra.Util
import qualified Ouroboros.Consensus.UTxO.Mock as Mock
import           Ouroboros.Network.Chain (Chain (..), HasHeader)
import           Ouroboros.Network.ChainProducerState
import           Ouroboros.Network.ConsumersAndProducers
import           Ouroboros.Network.Node (NodeId (..))
import qualified Ouroboros.Network.Node as Node
import           Ouroboros.Network.Serialise hiding ((<>))

import           CLI
import           Logging
import qualified NamedPipe
import           Payload

data NodeRole = CoreNode
              -- ^ In our experiment, these can actually produce blocks.
              | RelayNode
              deriving (Show, Eq)

data NodeSetup = NodeSetup {
    nodeId           :: NodeId
  , producers        :: [NodeId]
  , consumers        :: [NodeId]
  , initialChainData :: [Int]
  -- ^ A very naive representation of an \"initial chain\". Essentially, given
  -- a list of integers, is up to the different payloads to use them to come
  -- up with sensible implementations for a chain.
  , role             :: NodeRole
  }

instance FromJSON NodeRole where
    parseJSON = withText "role" $ \s -> case s of
        "core"  -> pure CoreNode
        "relay" -> pure RelayNode
        _       -> fail $ "Invalid NodeRole: " <> show s

instance FromJSON NodeId where
    parseJSON v = CoreId <$> parseJSON v

deriveFromJSON defaultOptions ''NodeSetup

data NetworkTopology = NetworkTopology [NodeSetup]

deriveFromJSON defaultOptions ''NetworkTopology

type NetworkMap = Map NodeId NodeSetup

toNetworkMap :: NetworkTopology -> NetworkMap
toNetworkMap (NetworkTopology xs) =
    foldl' (\acc ns -> M.insert (nodeId ns) ns acc) mempty xs

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
runNode cli@CLI{..} = do
    -- If the user asked to submit a transaction, we don't have to spin up a
    -- full node, we simply transmit it and exit.
    case command of
         TxSubmitter tx -> handleTxSubmission tx
         SimpleNode pt  -> handleSimpleNode cli pt

handleSimpleNode :: CLI -> PayloadType -> IO ()
handleSimpleNode CLI{..} payloadType = do
    let isLogger   = myNodeId == CoreId 0

    topoE <- eitherDecode . toS <$> B.readFile topologyFile
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

                   Node.forkRelayKernel upstream cps
                   when (role == CoreNode) $ do
                       Node.forkCoreKernel slotDuration
                                           (chainFrom initialChain 100)
                                           fixupBlock
                                           cps

                   let allThreads = terminalThread <> producerThreads
                                                   <> consumerThreads
                   void $ Async.waitAnyCancel allThreads

  where
      spawnTerminalLogger :: TBQueue LogEvent -> IO (Async.Async ())
      spawnTerminalLogger q = do
          Async.async $ showNetworkTraffic q

      spawnLogger :: ( HasHeader (Payload pt)
                     , Serialise (Payload pt)
                     , Condense  (Payload pt)
                     , Condense  [Payload pt]
                     , Mock.HasUtxo (Payload pt)
                     , Mock.HasUtxo (Chain (Payload pt))
                     )
                  => TBQueue LogEvent
                  -> NodeId
                  -> IO (TVar (Chain (Payload pt)), Async.Async ())
      spawnLogger q targetId = do
          chVar <- atomically $ newTVar Genesis
          emptyPool <- newMVar mempty
          let handler = LoggerHandler q chVar emptyPool
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
slotDuration = 2 * 1000000

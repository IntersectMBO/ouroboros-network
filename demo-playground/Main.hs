{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

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
import           Options.Applicative

import           Chain (Chain (..), HasHeader)
import           ChainProducerState
import           ConsumersAndProducers
import           Infra.Util
import qualified Node
import           Ouroboros
import           Serialise hiding ((<>))
import           Util.Singletons (Dict (..), withSomeSing)

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

data CLI = CLI
  { myNodeId     :: NodeId
  , topologyFile :: FilePath
  , payloadType  :: PayloadType
  }

sample :: Parser CLI
sample = CLI
      <$> option (fmap CoreId auto)
          ( long "node-id"
         <> short 'n'
         <> metavar "NODE-ID"
         <> help "The ID for this node" )
      <*> strOption
         ( long "topology"
         <> short 't'
         <> metavar "FILEPATH"
         <> help "The path to a file describing the topology."
         )
      <*> (option (readPayloadType <$> str) (
               long "payload-type"
            <> short 'p'
            <> metavar allPayloadTypes
            <> value DummyPayloadType
            <> help "The content of the payload in the messages"
            ))

main :: IO ()
main = runNode =<< execParser opts
  where
    opts = info (sample <**> helper)
      ( fullDesc
     <> progDesc "Run a node with the chain-following protocol hooked in."
     )

dictPayloadImplementation :: Sing (pt :: PayloadType)
                          -> Dict ( Serialise (Payload pt 'OuroborosBFT)
                                  , Condense  (Payload pt 'OuroborosBFT)
                                  , HasHeader (Payload pt 'OuroborosBFT)
                                  , PayloadImplementation pt
                                  )
dictPayloadImplementation SDummyPayload = Dict
dictPayloadImplementation SMockPayload  = Dict

runNode :: CLI -> IO ()
runNode CLI{..} = do
    let isLogger   = myNodeId == CoreId 0
    topoE <- eitherDecode . toS <$> B.readFile topologyFile
    case topoE of
         Left e -> error e
         Right t -> do
             let topology      = toNetworkMap t
                 NodeSetup{..} = fromMaybe (error "note not found.") $
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
                   let initialChain :: Chain (Payload pt 'OuroborosBFT)
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

      spawnLogger :: ( HasHeader (Payload pt 'OuroborosBFT)
                     , Serialise (Payload pt 'OuroborosBFT)
                     , Condense  (Payload pt 'OuroborosBFT)
                     )
                  => TBQueue LogEvent
                  -> NodeId
                  -> IO (TVar (Chain (Payload pt 'OuroborosBFT)), Async.Async ())
      spawnLogger q targetId = do
          chVar <- atomically $ newTVar Genesis
          a     <- Async.async $ NamedPipe.runConsumer myNodeId targetId $
                     loggerConsumer q chVar targetId
          pure (chVar, a)

      spawnConsumer :: ( HasHeader (Payload pt 'OuroborosBFT)
                       , Serialise (Payload pt 'OuroborosBFT)
                       )
                    => Chain (Payload pt 'OuroborosBFT)
                    -> NodeId
                    -> IO (TVar (Chain (Payload pt 'OuroborosBFT)), Async.Async ())
      spawnConsumer myChain producerNodeId = do
          chVar <- atomically $ newTVar myChain
          a     <- Async.async $ NamedPipe.runConsumer myNodeId producerNodeId $
                     exampleConsumer chVar
          pure (chVar, a)

      spawnProducer :: ( HasHeader (Payload pt 'OuroborosBFT)
                       , Serialise (Payload pt 'OuroborosBFT)
                       )
                    => TVar (ChainProducerState (Payload pt 'OuroborosBFT))
                    -> NodeId
                    -> IO (Async.Async ())
      spawnProducer cps consumerNodeId = Async.async $
          NamedPipe.runProducer myNodeId consumerNodeId $
            exampleProducer cps

slotDuration :: Int
slotDuration = 2 * 1000000

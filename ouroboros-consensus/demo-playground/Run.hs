{-# LANGUAGE EmptyDataDecls      #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Run (
      runNode
    ) where

import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.ST (stToIO)
import           Control.Monad.Trans
import           Crypto.Random
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Semigroup ((<>))

import           Protocol.Codec (hoistCodec)

import           Ouroboros.Network.Block
import           Ouroboros.Network.Chain (Chain (..), pointHash)
import qualified Ouroboros.Network.Pipe as P
import           Ouroboros.Network.Protocol.ChainSync.Codec.Cbor

import           Ouroboros.Consensus.Crypto.DSIGN.Mock
import           Ouroboros.Consensus.Ledger.Abstract
import qualified Ouroboros.Consensus.Ledger.Mock as Mock
import           Ouroboros.Consensus.Node
import           Ouroboros.Consensus.Protocol.BFT
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Consensus.Util.Random
import           Ouroboros.Consensus.Util.STM

import           CLI
import           Logging
import           Mock.Mempool
import           Mock.TxSubmission
import           NamedPipe (DataFlow (..), NodeMapping ((:==>:)))
import qualified NamedPipe
import           Topology

runNode :: CLI -> IO ()
runNode cli@CLI{..} = do
    -- If the user asked to submit a transaction, we don't have to spin up a
    -- full node, we simply transmit it and exit.
    case command of
         TxSubmitter topology tx -> handleTxSubmission topology tx
         SimpleNode t            -> handleSimpleNode cli t

type Block = Mock.SimpleBlock (Bft BftMockCrypto) Mock.SimpleBlockMockCrypto


-- | Setups a simple node, which will run the chain-following protocol and,
-- if core, will also look at the mempool when trying to create a new block.
handleSimpleNode :: CLI -> TopologyInfo -> IO ()
handleSimpleNode CLI{systemStart, slotDuration} (TopologyInfo myNodeId topologyFile) = do
    putStrLn $ "System started at " <> show systemStart
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
             terminalThread  <- spawnTerminalLogger loggingQueue

             let initialPool :: Mempool Mock.Tx
                 initialPool = mempty

             -- Each node has a mempool, regardless from its consumer
             -- and producer threads.
             nodeMempool <- atomically $ newTVar initialPool

             let numNodes  = length nodeSetups
             let nid           = case myNodeId of
                                   CoreId  n -> n
                                   RelayId n -> n
             let protocolState = ()
             let bftConfig     = BftNodeConfig {
                                     bftNodeId   = myNodeId
                                   , bftSignKey  = SignKeyMockDSIGN nid
                                   , bftNumNodes = fromIntegral numNodes
                                   , bftVerKeys  = M.fromList [
                                         (CoreId n, VerKeyMockDSIGN n)
                                       | setup    <- nodeSetups
                                       , CoreId n <- [nodeId setup]
                                       ]
                                   }

             let initLedger :: ExtLedgerState Block
                 initLedger = ExtLedgerState (Mock.SimpleLedgerState mempty mempty) ()

             let callbacks :: NodeCallbacks IO (MonadPseudoRandomT ChaChaDRG) Block
                 callbacks = NodeCallbacks {
                     produceBlock = \proof l slot prevPoint prevBlockNo -> do
                        let curNo    = succ prevBlockNo
                            prevHash = castHash (pointHash prevPoint)

                        -- Before generating a new block, look for incoming transactions.
                        -- If there are, check if the mempool is consistent and, if it is,
                        -- grab the valid new transactions and incorporate them into a
                        -- new block.
                        mp  <- lift . lift $ readTVar nodeMempool
                        txs <- do let ts  = collect (Mock.slsUtxo . ledgerState $ l) mp
                                      mp' = mempoolRemove (M.keysSet ts) $ mp
                                  lift . lift $ writeTVar nodeMempool mp'
                                  return ts

                        Mock.forgeBlock bftConfig slot curNo prevHash txs proof
                 , adoptedNewChain = logChain loggingQueue
                 }

             -- TODO: This use of STM is actually not correct, we need to revisit
             -- this one and use a SystemDRG (which lives in IO).
             randomnessSource <- atomically $ newTVar (seedToChaCha nullSeed)
             blockchainTime <- realBlockchainTime systemStart slotDuration


             kernelHandle <-
                 nodeKernel bftConfig
                            protocolState
                            (simMonadPseudoRandomT randomnessSource)
                            blockchainTime
                            initLedger
                            Genesis
                            callbacks

             -- Spawn the thread which listens to the mempool.
             mempoolThread <-
                     spawnMempoolListener myNodeId nodeMempool kernelHandle


             forM_ (producers nodeSetup) (addUpstream kernelHandle)
             forM_ (consumers nodeSetup) (addDownstream kernelHandle)

             let allThreads = terminalThread : [mempoolThread]
             void $ Async.waitAnyCancel allThreads

  where
      spawnTerminalLogger :: TBQueue LogEvent -> IO (Async.Async ())
      spawnTerminalLogger q = do
          Async.async $ showNetworkTraffic q


      -- We need to make sure that both nodes read from the same file
      -- We therefore use the convention to distinguish between
      -- upstream and downstream from the perspective of the "lower numbered" node
      addUpstream :: NodeKernel IO NodeId Block
                  -> NodeId
                  -> IO ()
      addUpstream kernel producerNodeId = do
        let direction = Upstream (producerNodeId :==>: myNodeId)
        registerUpstream (nodeNetworkLayer kernel)
                         producerNodeId
                         (hoistCodec stToIO codecChainSync) $ \cc ->
          NamedPipe.withPipe direction $ \(hndRead, hndWrite) ->
            cc (P.pipeDuplex hndRead hndWrite)

      addDownstream :: NodeKernel IO NodeId Block
                    -> NodeId
                    -> IO ()
      addDownstream kernel consumerNodeId = do
        let direction = Downstream (myNodeId :==>: consumerNodeId)
        registerDownstream (nodeNetworkLayer kernel)
                           (hoistCodec stToIO codecChainSync)
                           $ \cc ->
          NamedPipe.withPipe direction $ \(hndRead, hndWrite) -> do
              cc (P.pipeDuplex hndRead hndWrite)

instance ProtocolLedgerView Block where
  protocolLedgerView _ _ = ()

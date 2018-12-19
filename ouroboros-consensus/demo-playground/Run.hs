{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import           Ouroboros.Network.Chain (pointHash)
import qualified Ouroboros.Network.Pipe as P
import           Ouroboros.Network.Protocol.ChainSync.Codec.Cbor

import           Ouroboros.Consensus.Demo
import           Ouroboros.Consensus.Ledger.Abstract
import qualified Ouroboros.Consensus.Ledger.Mock as Mock
import           Ouroboros.Consensus.Node
import           Ouroboros.Consensus.Util
import           Ouroboros.Consensus.Util.Orphans ()

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
      TxSubmitter topology tx ->
        handleTxSubmission topology tx
      SimpleNode  topology ->
        case protocol of
          Some p -> case demoProtocolConstraints p of
                      Dict -> handleSimpleNode p cli topology

-- | Setups a simple node, which will run the chain-following protocol and,
-- if core, will also look at the mempool when trying to create a new block.
handleSimpleNode :: forall p. DemoProtocolConstraints p
                 => DemoProtocol p -> CLI -> TopologyInfo -> IO ()
handleSimpleNode p CLI{..} (TopologyInfo myNodeId topologyFile) = do
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

             let ProtocolInfo{..} = protocolInfo
                                      p
                                      (NumCoreNodes (length nodeSetups))
                                      (CoreNodeId nid)

             -- Creates a TBQueue to be used by all the logger threads to monitor
             -- the traffic.
             loggingQueue    <- atomically $ newTBQueue 50
             terminalThread  <- spawnTerminalLogger loggingQueue

             let initialPool :: Mempool Mock.Tx
                 initialPool = mempty

             -- Each node has a mempool, regardless from its consumer
             -- and producer threads.
             nodeMempool <- atomically $ newTVar initialPool

             let callbacks :: NodeCallbacks IO (Block p)
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

                        Mock.forgeBlock pInfoConfig
                                        slot
                                        curNo
                                        prevHash
                                        txs
                                        proof
                 , produceDRG      = drgNew
                 , adoptedNewChain = logChain loggingQueue
                 }

             btime  <- realBlockchainTime systemStart slotDuration
             kernel <- nodeKernel
                         pInfoConfig
                         pInfoInitState
                         btime
                         pInfoInitLedger
                         pInfoInitChain
                         callbacks

             -- Spawn the thread which listens to the mempool.
             mempoolThread <- spawnMempoolListener myNodeId nodeMempool kernel

             forM_ (producers nodeSetup) (addUpstream'   kernel)
             forM_ (consumers nodeSetup) (addDownstream' kernel)

             let allThreads = terminalThread : [mempoolThread]
             void $ Async.waitAnyCancel allThreads

  where
      nid :: Int
      nid = case myNodeId of
              CoreId  n -> n
              RelayId _ -> error "Non-core nodes currently not supported"

      spawnTerminalLogger :: TBQueue LogEvent -> IO (Async.Async ())
      spawnTerminalLogger q = do
          Async.async $ showNetworkTraffic q

      -- We need to make sure that both nodes read from the same file
      -- We therefore use the convention to distinguish between
      -- upstream and downstream from the perspective of the "lower numbered" node
      addUpstream' :: NodeKernel IO NodeId (Block p)
                   -> NodeId
                   -> IO ()
      addUpstream' kernel producerNodeId =
          addUpstream kernel producerNodeId nodeComms
        where
          direction = Upstream (producerNodeId :==>: myNodeId)
          nodeComms = NodeComms {
              ncCodec    = hoistCodec stToIO codecChainSync
            , ncWithChan = \cc ->
                NamedPipe.withPipe direction $ \(hndRead, hndWrite) ->
                  cc (P.pipeDuplex hndRead hndWrite)
            }

      addDownstream' :: NodeKernel IO NodeId (Block p)
                     -> NodeId
                     -> IO ()
      addDownstream' kernel consumerNodeId =
          addDownstream kernel nodeComms
        where
          direction = Downstream (myNodeId :==>: consumerNodeId)
          nodeComms = NodeComms {
              ncCodec    = hoistCodec stToIO codecChainSync
            , ncWithChan = \cc ->
                NamedPipe.withPipe direction $ \(hndRead, hndWrite) ->
                 cc (P.pipeDuplex hndRead hndWrite)
            }

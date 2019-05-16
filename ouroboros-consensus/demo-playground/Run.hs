{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Run (
      runNode
    ) where

import           Codec.Serialise (encode, decode)
import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Trans
import           Control.Tracer
import           Crypto.Random
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Semigroup ((<>))

import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block
import           Ouroboros.Network.Chain (genesisPoint, pointHash)
import qualified Ouroboros.Network.Chain as Chain
import           Ouroboros.Network.Protocol.BlockFetch.Codec
import           Ouroboros.Network.Protocol.ChainSync.Codec

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.ChainSyncClient (ClockSkew (..))
import           Ouroboros.Consensus.Demo
import           Ouroboros.Consensus.Ledger.Abstract
import qualified Ouroboros.Consensus.Ledger.Mock as Mock
import           Ouroboros.Consensus.Node
import           Ouroboros.Consensus.Util
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Consensus.Util.STM
import           Ouroboros.Consensus.Util.ThreadRegistry

import           Ouroboros.Storage.ChainDB (ChainDB)
import qualified Ouroboros.Storage.ChainDB as ChainDB
import qualified Ouroboros.Storage.ChainDB.Mock as ChainDB

import           CLI
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
      SimpleNode topology protocol ->
        case protocol of
          Some p -> case demoProtocolConstraints p of
                      Dict -> handleSimpleNode p cli topology

-- | Sets up a simple node, which will run the chain sync protocol and block
-- fetch protocol, and, if core, will also look at the mempool when trying to
-- create a new block.
handleSimpleNode :: forall p. DemoProtocolConstraints p
                 => DemoProtocol p -> CLI -> TopologyInfo -> IO ()
handleSimpleNode p CLI{..} (TopologyInfo myNodeId topologyFile) = do
    putStrLn $ "System started at " <> show systemStart
    t@(NetworkTopology nodeSetups) <-
      either error id <$> readTopologyFile topologyFile
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

    withThreadRegistry $ \registry -> do

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
          }

      chainDB <- ChainDB.openDB encode pInfoConfig pInfoInitLedger Mock.simpleHeader

      btime  <- realBlockchainTime registry slotDuration systemStart
      let tracer = contramap ((show myNodeId <> " | ") <>) stdoutTracer
          nodeParams = NodeParams
            { tracer
            , threadRegistry     = registry
            , maxClockSkew       = ClockSkew 1
            , cfg                = pInfoConfig
            , initState          = pInfoInitState
            , btime
            , chainDB
            , callbacks
            , blockFetchSize     = Mock.headerBlockSize . Mock.headerPreHeader
            , blockMatchesHeader = Mock.blockMatchesHeader
            }
      kernel <- nodeKernel nodeParams

      watchChain registry tracer chainDB

      -- Spawn the thread which listens to the mempool.
      mempoolThread <- spawnMempoolListener tracer myNodeId nodeMempool kernel

      forM_ (producers nodeSetup) (addUpstream'   kernel)
      forM_ (consumers nodeSetup) (addDownstream' kernel)

      Async.wait mempoolThread
  where
      nid :: Int
      nid = case myNodeId of
              CoreId  n -> n
              RelayId _ -> error "Non-core nodes currently not supported"

      watchChain :: ThreadRegistry IO
                 -> Tracer IO String
                 -> ChainDB IO (Block p) (Header p)
                 -> IO ()
      watchChain registry tracer chainDB = onEachChange
          registry fingerprint initFingerprint
          (ChainDB.getCurrentChain chainDB) (const logFullChain)
        where
          initFingerprint  = (genesisPoint, genesisPoint)
          fingerprint frag = (AF.headPoint frag, AF.anchorPoint frag)
          logFullChain = do
            chain <- ChainDB.toChain chainDB
            traceWith tracer $
              "Updated chain: " <> condense (Chain.toOldestFirst chain)

      -- We need to make sure that both nodes read from the same file
      -- We therefore use the convention to distinguish between
      -- upstream and downstream from the perspective of the "lower numbered" node
      addUpstream' :: NodeKernel IO NodeId (Block p) (Header p)
                   -> NodeId
                   -> IO ()
      addUpstream' kernel producerNodeId =
          addUpstream kernel producerNodeId nodeCommsCS nodeCommsBF
        where
          direction = Upstream (producerNodeId :==>: myNodeId)
          nodeCommsCS = NodeComms {
              ncCodec    = codecChainSync encode encode decode decode
            , ncWithChan = NamedPipe.withPipeChannel "chain-sync" direction
            }
          nodeCommsBF = NodeComms {
              ncCodec    = codecBlockFetch encode encode decode decode
            , ncWithChan = NamedPipe.withPipeChannel "block-fetch" direction
            }


      addDownstream' :: NodeKernel IO NodeId (Block p) (Header p)
                     -> NodeId
                     -> IO ()
      addDownstream' kernel consumerNodeId =
          addDownstream kernel nodeCommsCS nodeCommsBF
        where
          direction = Downstream (myNodeId :==>: consumerNodeId)
          nodeCommsCS = NodeComms {
              ncCodec    = codecChainSync encode encode decode decode
            , ncWithChan = NamedPipe.withPipeChannel "chain-sync" direction
            }
          nodeCommsBF = NodeComms {
              ncCodec    = codecBlockFetch encode encode decode decode
            , ncWithChan = NamedPipe.withPipeChannel "block-fetch" direction
            }

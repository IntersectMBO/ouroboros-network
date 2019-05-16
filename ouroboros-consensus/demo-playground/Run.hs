{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Run (
      runNode
    ) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import           Codec.Serialise (decode, encode)
import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Trans
import           Control.Tracer
import           Crypto.Random
import           Data.Functor.Contravariant (contramap)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Semigroup ((<>))

import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block
import qualified Ouroboros.Network.Block as Block
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
      SimpleNode topology protocol -> do
        Some p <- fromProtocol protocol
        case runDemo p of
          Dict -> handleSimpleNode p cli topology

-- | Sets up a simple node, which will run the chain sync protocol and block
-- fetch protocol, and, if core, will also look at the mempool when trying to
-- create a new block.
handleSimpleNode :: forall p. RunDemo p
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

    let pInfo@ProtocolInfo{..} =
          protocolInfo p (NumCoreNodes (length nodeSetups)) (CoreNodeId nid)

    withThreadRegistry $ \registry -> do

      let initialPool :: Mempool Mock.Tx
          initialPool = mempty

      -- Each node has a mempool, regardless from its consumer
      -- and producer threads.
      nodeMempool <- atomically $ newTVar initialPool

      let callbacks :: NodeCallbacks IO (Block p)
          callbacks = NodeCallbacks {
              produceBlock = \proof _l slot prevPoint prevBlockNo -> do
                 let curNo :: BlockNo
                     curNo = succ prevBlockNo

                     prevHash :: ChainHash (Header p)
                     prevHash = castHash (pointHash prevPoint)

                 -- Before generating a new block, look for incoming transactions.
                 -- If there are, check if the mempool is consistent and, if it is,
                 -- grab the valid new transactions and incorporate them into a
                 -- new block.
                 mp  <- lift . lift $ readTVar nodeMempool
{-
                 -- TODO: In the original code this was finding the transactions
                 -- that were consistent with the ledger. Now that we abstract
                 -- over the ledger this will no longer be possible. We will
                 -- just try to include all of them; we'll come back to this
                 -- once we have a proper mempool.
                 txs <- do let ts  = collect (Mock.slsUtxo . ledgerState $ l) mp
                               mp' = mempoolRemove (M.keysSet ts) $ mp
                           lift . lift $ writeTVar nodeMempool mp'
                           return ts
-}
                 txs <- do let ts  = mempoolToMap mp
                               mp' = mempoolRemove (M.keysSet ts) $ mp
                           lift . lift $ writeTVar nodeMempool mp'
                           return ts

                 demoForgeBlock pInfoConfig
                                slot
                                curNo
                                prevHash
                                txs
                                proof

          , produceDRG      = drgNew
          }

      chainDB :: ChainDB IO (Block p) (Header p) <- ChainDB.openDB
        (demoEncodePreHeader pInfoConfig) pInfoConfig pInfoInitLedger
        demoGetHeader

      btime  <- realBlockchainTime registry slotDuration systemStart
      let tracer = contramap ((show myNodeId <> " | ") <>) stdoutTracer
      --     nodeParams = NodeParams
      --       { encoder            = demoEncodePreHeader pInfoConfig
      --       , tracer             = nullTracer
      --       , threadRegistry     = registry
      --       , maxClockSkew       = ClockSkew 1
      --       , cfg                = pInfoConfig
      --       , initState          = pInfoInitState
      --       , btime
      --       , chainDB
      --       , callbacks
      --       , blockFetchSize     = undefined -- Mock.headerBlockSize . Mock.headerPreHeader
      --       , blockMatchesHeader = undefined -- Mock.blockMatchesHeader
      --       }

      kernel :: NodeKernel IO NodeId (Block p) (Header p) <- undefined -- nodeKernel nodeParams

      watchChain registry tracer chainDB

      -- Spawn the thread which listens to the mempool.
      mempoolThread <- spawnMempoolListener tracer myNodeId nodeMempool kernel

      forM_ (producers nodeSetup) (addUpstream'   pInfo kernel)
      forM_ (consumers nodeSetup) (addDownstream' pInfo kernel)

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
      addUpstream' :: ProtocolInfo p
                   -> NodeKernel IO NodeId (Block p) (Header p)
                   -> NodeId
                   -> IO ()
      addUpstream' pInfo@ProtocolInfo{..} kernel producerNodeId =
          addUpstream kernel producerNodeId nodeCommsCS nodeCommsBF
        where
          direction = Upstream (producerNodeId :==>: myNodeId)
          nodeCommsCS = NodeComms {
              ncCodec    = codecChainSync
                             (demoEncodeHeader     pInfoConfig)
                             (encodePoint'         pInfo)
                             (demoDecodeHeader     pInfoConfig)
                             (decodePoint'         pInfo)
            , ncWithChan = NamedPipe.withPipeChannel "chain-sync" direction
            }
          nodeCommsBF = NodeComms {
              ncCodec    = codecBlockFetch
                             (demoEncodeBlock      pInfoConfig)
                             (demoEncodeHeaderHash pInfoConfig)
                             (demoDecodeBlock      pInfoConfig)
                             (demoDecodeHeaderHash pInfoConfig)
            , ncWithChan = NamedPipe.withPipeChannel "block-fetch" direction
            }

      addDownstream' :: ProtocolInfo p
                     -> NodeKernel IO NodeId (Block p) (Header p)
                     -> NodeId
                     -> IO ()
      addDownstream' pInfo@ProtocolInfo{..} kernel consumerNodeId =
          addDownstream kernel nodeCommsCS nodeCommsBF
        where
          direction = Downstream (myNodeId :==>: consumerNodeId)
          nodeCommsCS = NodeComms {
              ncCodec    = codecChainSync
                             (demoEncodeHeader     pInfoConfig)
                             (encodePoint'         pInfo)
                             (demoDecodeHeader     pInfoConfig)
                             (decodePoint'         pInfo)
            , ncWithChan = NamedPipe.withPipeChannel "chain-sync" direction
            }
          nodeCommsBF = NodeComms {
              ncCodec    = codecBlockFetch
                             (demoEncodeBlock      pInfoConfig)
                             (demoEncodeHeaderHash pInfoConfig)
                             (demoDecodeBlock      pInfoConfig)
                             (demoDecodeHeaderHash pInfoConfig)
            , ncWithChan = NamedPipe.withPipeChannel "block-fetch" direction
            }

      encodePoint' :: ProtocolInfo p -> Point (Header p) -> Encoding
      encodePoint' ProtocolInfo{..} =
          Block.encodePoint $ Block.encodeChainHash (demoEncodeHeaderHash pInfoConfig)

      decodePoint' :: forall s. ProtocolInfo p -> Decoder s (Point (Header p))
      decodePoint' ProtocolInfo{..} =
          Block.decodePoint $ Block.decodeChainHash (demoDecodeHeaderHash pInfoConfig)

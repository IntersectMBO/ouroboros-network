{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

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
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Semigroup ((<>))

import           Protocol.Codec (hoistCodec)

import           Ouroboros.Network.Block
import           Ouroboros.Network.Chain (Chain (..), pointHash)
import qualified Ouroboros.Network.Pipe as P
import           Ouroboros.Network.Protocol.ChainSync.Codec.Cbor

import           Ouroboros.Consensus.Ledger.Abstract
import qualified Ouroboros.Consensus.Ledger.Mock as Mock
import           Ouroboros.Consensus.Node
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.Praos
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Consensus.Util.Random
import           Ouroboros.Consensus.Util.STM

--- START HERE: Comment/uncomment these to use BFT.
import           Ouroboros.Consensus.Crypto.DSIGN.Mock
import           Ouroboros.Consensus.Protocol.BFT

--- START HERE: Comment/uncomment these to use PRAOS.
-- import           Data.IntMap.Strict (IntMap)
-- import           Ouroboros.Consensus.Crypto.KES (VerKeyKES)
-- import           Ouroboros.Consensus.Crypto.KES.Mock
-- import           Ouroboros.Consensus.Crypto.VRF (VerKeyVRF)
-- import           Ouroboros.Consensus.Crypto.VRF.Mock
-- import           Ouroboros.Consensus.Protocol.ExtNodeConfig

import           CLI
import           Logging
import           Mock.Mempool
import           Mock.TxSubmission
import           NamedPipe (DataFlow (..), NodeMapping ((:==>:)))
import qualified NamedPipe
import           Topology

-- This type is merely a convience one which groups all the relevant piece of
-- state to run one protocol or the other under the same umbrella.
data DesiredProtocol = DesiredProtocol {
    initLedger     :: ExtLedgerState Block
  , protocolConfig :: NodeConfig Protocol
  , nodeState      :: NodeState Protocol
  }


-- | PROTOCOL: Comment/uncomment this function to use BFT
--
desiredProtocol :: NodeId -> [NodeSetup] -> DesiredProtocol
desiredProtocol myNodeId nodeSetups = DesiredProtocol {
    initLedger     = ExtLedgerState (Mock.SimpleLedgerState mempty mempty) ()
  , nodeState      = ()
  , protocolConfig =
        let numNodes  = length nodeSetups
            nid       = case myNodeId of
                          CoreId  n -> n
                          RelayId n -> n
        in BftNodeConfig { bftNodeId   = myNodeId
                         , bftSignKey  = SignKeyMockDSIGN nid
                         , bftNumNodes = fromIntegral numNodes
                         , bftVerKeys  = M.fromList [
                               (CoreId n, VerKeyMockDSIGN n)
                             | setup    <- nodeSetups
                             , CoreId n <- [nodeId setup]
                             ]
                         }
  }

-- TODO: Most of these settings comes from the test code, and would be nice
-- to not repeat ourselves.

-- | PROTOCOL: Comment/uncomment this function to use PRAOS
--
-- desiredProtocol :: NodeId -> [NodeSetup] -> DesiredProtocol
-- desiredProtocol myNodeId nodeSetups = DesiredProtocol {
--     initLedger  = ExtLedgerState {
--                   ledgerState = Mock.SimpleLedgerState mempty mempty
--                 , ouroborosChainState = []
--                 }
--   , nodeState      = SignKeyMockKES ( fst $ verKeys IntMap.! nid
--                                     , 0
--                                     , 1 + fromIntegral numSlots
--                                     )
--   , protocolConfig = EncNodeConfig {
--       encNodeConfigP = PraosNodeConfig
--          { praosNodeId        = myNodeId
--          , praosSignKeyVRF    = SignKeyMockVRF nid
--          , praosSlotsPerEpoch = fromIntegral $ k * kPerEpoch
--          , praosInitialEta    = 0
--          , praosInitialStake  = initialStake
--          , praosLeaderF       = 0.5
--          , praosK             = fromIntegral k
--          , praosVerKeys       = verKeys
--          }
--     , encNodeConfigExt = M.fromList [
--         ("a", CoreId 0)
--        ,("b", CoreId 1)
--        ,("c", CoreId 2)
--        ]
--     }
--   }
--   where
--     nid :: Int
--     nid = case myNodeId of
--               CoreId  n -> n
--               RelayId n -> n
--
--     initialStake :: StakeDist
--     initialStake =
--         let q = recip $ fromIntegral numNodes
--         in  IntMap.fromList [(i, q) | i <- [0 .. numNodes - 1]]
--
--     verKeys :: IntMap (VerKeyKES MockKES, VerKeyVRF MockVRF)
--     verKeys = IntMap.fromList [ (nd, (VerKeyMockKES nd, VerKeyMockVRF nd))
--                               |  nd <- [0 .. numNodes - 1]]
--
--     numNodes, k, kPerEpoch, numSlots :: Int
--     numNodes  = length nodeSetups
--     k         = 5
--     kPerEpoch = 3
--     numSlots  = maxBound


-- | PROTOCOL: Comment/uncomment these lines to use PRAOS
--
-- type Protocol = ExtNodeConfig (M.Map Mock.Addr NodeId) (Praos PraosMockCrypto)
--
-- instance HasPayload (Praos PraosMockCrypto) Block where
--   blockPayload _ = encPayloadP
--                  . Mock.headerOuroboros
--                  . Mock.simpleHeader

-- instance ProtocolLedgerView Block where
--   protocolLedgerView (EncNodeConfig _nodeConfig extCfg) (Mock.SimpleLedgerState u _) =
--       _relativeStakes $ _totalStakes extCfg u


-- | PROTOCOL: Comment/uncomment these lines to use BFT
--
type Protocol = Bft BftMockCrypto
instance ProtocolLedgerView Block where
  protocolLedgerView _ _ = ()


-- Our 'Block' type stays the same.
type Block  = Mock.SimpleBlock Protocol Mock.SimpleBlockMockCrypto


runNode :: CLI -> IO ()
runNode cli@CLI{..} = do
    -- If the user asked to submit a transaction, we don't have to spin up a
    -- full node, we simply transmit it and exit.
    case command of
         TxSubmitter topology tx -> handleTxSubmission topology tx
         SimpleNode t            -> handleSimpleNode desiredProtocol cli t

-- | Setups a simple node, which will run the chain-following protocol and,
-- if core, will also look at the mempool when trying to create a new block.
handleSimpleNode :: (NodeId -> [NodeSetup] -> DesiredProtocol)
                 -> CLI -> TopologyInfo -> IO ()
handleSimpleNode mkDesiredProtocol
                 CLI{systemStart, slotDuration}
                 (TopologyInfo myNodeId topologyFile) = do
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

             let protocolInUse = mkDesiredProtocol myNodeId nodeSetups

             -- Creates a TBQueue to be used by all the logger threads to monitor
             -- the traffic.
             loggingQueue    <- atomically $ newTBQueue 50
             terminalThread  <- spawnTerminalLogger loggingQueue

             let initialPool :: Mempool Mock.Tx
                 initialPool = mempty

             -- Each node has a mempool, regardless from its consumer
             -- and producer threads.
             nodeMempool <- atomically $ newTVar initialPool

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

                        Mock.forgeBlock (protocolConfig protocolInUse)
                                        slot
                                        curNo
                                        prevHash
                                        txs
                                        proof
                 , adoptedNewChain = logChain loggingQueue
                 }

             -- TODO: This use of STM is actually not correct, we need to revisit
             -- this one and use a SystemDRG (which lives in IO).
             randomnessSource <- atomically $ newTVar (seedToChaCha nullSeed)
             blockchainTime <- realBlockchainTime systemStart slotDuration


             kernelHandle <-
                 nodeKernel (protocolConfig protocolInUse)
                            (nodeState protocolInUse)
                            (simMonadPseudoRandomT randomnessSource)
                            blockchainTime
                            (initLedger protocolInUse)
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


{-----------------------------------------------------------------------------
  Auxiliary functions
------------------------------------------------------------------------------}

_nodeStake :: M.Map Mock.Addr NodeId -> Mock.Utxo -> NodeId -> Int
_nodeStake cfg u nodeId =
    M.foldl
        (\acc (a, stake) -> if ourAddr cfg nodeId a then acc + stake else acc)
        0
        u

ourAddr :: M.Map Mock.Addr NodeId -> NodeId -> Mock.Addr -> Bool
ourAddr testAddressDistribution myNodeId address =
    fmap ((==) myNodeId) (M.lookup address testAddressDistribution)
        == Just True

_relativeStakes :: M.Map (Maybe Int) Int -> StakeDist
_relativeStakes m =
    let totalStake    = fromIntegral $ sum $ M.elems m
    in  IntMap.fromList [ (nid, fromIntegral stake / totalStake)
                        | (Just nid, stake) <- M.toList m
                        ]

_totalStakes :: M.Map Mock.Addr NodeId
             -> Mock.Utxo
             -> M.Map (Maybe Int) Int
_totalStakes addrDist = foldl f M.empty
  where
    f :: M.Map (Maybe Int) Int -> Mock.TxOut -> M.Map (Maybe Int) Int
    f m (a, stake) = case M.lookup a addrDist of
        Just (CoreId nid) -> M.insertWith (+) (Just nid) stake m
        _                 -> M.insertWith (+) Nothing stake m



{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Setup network
module Test.Dynamic.Network (
    broadcastNetwork
  ) where

import           Control.Monad
import           Control.Tracer (nullTracer)
import           Crypto.Number.Generate (generateBetween)
import           Crypto.Random (ChaChaDRG, drgNew)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.Typeable (Typeable)

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork (MonadFork)
import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer

import           Ouroboros.Network.Channel
import           Ouroboros.Network.Codec (AnyMessage, Codec)

import           Ouroboros.Network.Block
import           Ouroboros.Network.Chain
import           Ouroboros.Network.Protocol.BlockFetch.Codec
import           Ouroboros.Network.Protocol.BlockFetch.Type
import           Ouroboros.Network.Protocol.ChainSync.Codec
import           Ouroboros.Network.Protocol.ChainSync.Type

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.ChainSyncClient (ClockSkew (..))
import           Ouroboros.Consensus.Demo
import           Ouroboros.Consensus.Demo.Run
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Mock
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Node
import           Ouroboros.Consensus.Protocol.Abstract (NodeConfig)
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Consensus.Util.Random
import           Ouroboros.Consensus.Util.STM
import           Ouroboros.Consensus.Util.ThreadRegistry

import qualified Ouroboros.Storage.ChainDB.API as ChainDB
import qualified Ouroboros.Storage.ChainDB.Mock as ChainDB


-- | Setup fully-connected topology, where every node is both a producer
-- and a consumer
--
-- We run for the specified number of blocks, then return the final state of
-- each node.
broadcastNetwork :: forall m c ext.
                    ( MonadAsync m
                    , MonadFork  m
                    , MonadMask  m
                    , MonadSay   m
                    , MonadTime  m
                    , MonadTimer m
                    , MonadThrow (STM m)
                    , RunDemo (SimpleBlock c ext)
                    , SimpleCrypto c
                    , Show ext
                    , Typeable ext
                    )
                 => ThreadRegistry m
                 -> BlockchainTime m
                 -> NumCoreNodes
                 -> (CoreNodeId -> ProtocolInfo (SimpleBlock c ext))
                 -> ChaChaDRG
                 -> NumSlots
                 -> m (Map NodeId ( NodeConfig (BlockProtocol (SimpleBlock c ext))
                                  , Chain (SimpleBlock c ext)
                                  ))
broadcastNetwork registry btime numCoreNodes pInfo initRNG numSlots = do

    -- all known addresses
    let addrs :: [Addr]
        addrs = Set.toList . Set.fromList . concat
              $ [ nodeAddrs
                | node <- coreNodeIds
                , let nodeLedger = pInfoInitLedger (pInfo node)
                      nodeUtxo   = getUtxo nodeLedger
                      nodeAddrs  = map fst (Map.elems nodeUtxo)
                ]

    chans :: NodeChans m (SimpleBlock c ext) <- createCommunicationChannels

    varRNG <- atomically $ newTVar initRNG

    nodes <- forM coreNodeIds $ \coreNodeId -> do
      let us               = fromCoreNodeId coreNodeId
          ProtocolInfo{..} = pInfo coreNodeId

      let callbacks :: NodeCallbacks m (SimpleBlock c ext)
          callbacks = NodeCallbacks {
              produceBlock = \proof l slot prevPoint prevNo _txs -> do
                let curNo :: BlockNo
                    curNo = succ prevNo

                let prevHash :: ChainHash (SimpleBlock c ext)
                    prevHash = castHash (pointHash prevPoint)

                -- We ignore the transactions from the mempool (which will be
                -- empty), and instead produce some random transactions
                txs <- genTxs addrs (getUtxo l)
                demoForgeBlock pInfoConfig
                               slot
                               curNo
                               prevHash
                               (map SimpleGenTx txs)
                               proof

            , produceDRG      = atomically $ simChaChaT varRNG id $ drgNew
            }

      chainDB <- ChainDB.openDB pInfoConfig pInfoInitLedger simpleHeader

      let nodeParams = NodeParams
            { tracer             = nullTracer
            , threadRegistry     = registry
            , maxClockSkew       = ClockSkew 1
            , cfg                = pInfoConfig
            , initState          = pInfoInitState
            , btime
            , chainDB
            , callbacks
            , blockFetchSize     = fromIntegral . simpleBlockSize . simpleHeaderStd
            , blockMatchesHeader = matchesSimpleHeader
            }

      node <- nodeKernel nodeParams

      forM_ (filter (/= us) nodeIds) $ \them -> do
        let mkCommsDown :: Show bytes
                        => (NodeChan m (SimpleBlock c ext) -> Channel m bytes)
                        -> Codec ps e m bytes -> NodeComms m ps e bytes
            mkCommsDown getChan codec = NodeComms {
                ncCodec    = codec
              , ncWithChan = \cc -> cc $
                  loggingChannel (TalkingToConsumer us them) $
                    getChan (chans Map.! us Map.! them)
              }
            mkCommsUp :: Show bytes
                      => (NodeChan m (SimpleBlock c ext) -> Channel m bytes)
                      -> Codec ps e m bytes -> NodeComms m ps e bytes
            mkCommsUp getChan codec = NodeComms {
                ncCodec    = codec
              , ncWithChan = \cc -> cc $
                  loggingChannel (TalkingToProducer us them) $
                    getChan (chans Map.! them Map.! us)
              }
        addDownstream node
          (mkCommsDown chainSyncConsumer  codecChainSyncId)
          (mkCommsDown blockFetchConsumer codecBlockFetchId)
        addUpstream node them
          (mkCommsUp   chainSyncProducer  codecChainSyncId)
          (mkCommsUp   blockFetchProducer codecBlockFetchId)

      return (coreNodeId, node)

    -- STM variable to record the final chains of the nodes
    varRes <- atomically $ newTVar Nothing

    onSlot btime (finalSlot numSlots) $ do
      -- Wait a random amount of time after the final slot for the block fetch
      -- and chain sync to finish
      threadDelay 2000
      res <- fmap Map.fromList $ forM nodes $ \(cid, node) ->
        (\ch -> (fromCoreNodeId cid, (pInfoConfig (pInfo cid), ch))) <$>
        ChainDB.toChain (getChainDB node)
      atomically $ writeTVar varRes (Just res)

    atomically $ blockUntilJust (readTVar varRes)
  where
    nodeIds :: [NodeId]
    nodeIds = map fromCoreNodeId coreNodeIds

    coreNodeIds :: [CoreNodeId]
    coreNodeIds = enumCoreNodes numCoreNodes

    getUtxo :: ExtLedgerState (SimpleBlock c ext) -> Utxo
    getUtxo = mockUtxo . simpleLedgerState . ledgerState

    createCommunicationChannels :: m (NodeChans m (SimpleBlock c ext))
    createCommunicationChannels = fmap Map.fromList $ forM nodeIds $ \us ->
      fmap ((us, ) . Map.fromList) $ forM (filter (/= us) nodeIds) $ \them -> do
        (chainSyncConsumer,  chainSyncProducer)  <- createConnectedChannels
        (blockFetchConsumer, blockFetchProducer) <- createConnectedChannels
        return (them, NodeChan {..})


{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

-- | Communication channel used for the Chain Sync protocol
type ChainSyncChannel m blk = Channel m (AnyMessage (ChainSync (Header blk) (Point blk)))

-- | Communication channel used for the Block Fetch protocol
type BlockFetchChannel m blk = Channel m (AnyMessage (BlockFetch blk))

-- | The communication channels from and to each node
data NodeChan m blk = NodeChan
  { chainSyncConsumer  :: ChainSyncChannel  m blk
  , chainSyncProducer  :: ChainSyncChannel  m blk
  , blockFetchConsumer :: BlockFetchChannel m blk
  , blockFetchProducer :: BlockFetchChannel m blk
  }

-- | All connections between all nodes
type NodeChans m blk = Map NodeId (Map NodeId (NodeChan m blk))


{-------------------------------------------------------------------------------
  Internal: generating random transactions
-------------------------------------------------------------------------------}

genTxs :: MonadRandom m => [Addr] -> Utxo -> m [Tx]
genTxs addr u = do
    b <- generateBetween 0 1
    if b == 0
        then return []
        else do
            tx <- genTx addr u
            return [tx]

genTx :: MonadRandom m => [Addr] -> Utxo -> m Tx
genTx addrs u = do
    let senders = Set.toList . Set.fromList . map fst . Map.elems $ u -- people with funds
    sender    <- genElt senders
    recipient <- genElt $ filter (/= sender) addrs
    let assets  = filter (\(_, (a, _)) -> a == sender) $ Map.toList u
        fortune = sum [c | (_, (_, c)) <- assets]
        ins     = Set.fromList $ map fst assets
    amount <- fromIntegral <$> generateBetween 1 (fromIntegral fortune)
    let outRecipient = (recipient, amount)
        outs         = if amount == fortune
            then [outRecipient]
            else [outRecipient, (sender, fortune - amount)]
    return $ Tx ins outs
  where
    genElt xs = do
        m <- generateElement xs
        case m of
            Nothing -> error "expected nonempty list"
            Just x  -> return x

{-------------------------------------------------------------------------------
  Internal: logging
-------------------------------------------------------------------------------}

-- | Message sent by or to a producer
data TalkingToProducer pid = TalkingToProducer {
      producerUs   :: NodeId
    , producerThem :: pid
    }
  deriving (Show)

-- | Message sent by or to a consumer
data TalkingToConsumer cid = TalkingToConsumer {
      consumerUs   :: NodeId
    , consumerThem :: cid
    }
  deriving (Show)

instance Condense pid => Condense (TalkingToProducer pid) where
  condense TalkingToProducer{..} = condense (producerUs, producerThem)

instance Condense pid => Condense (TalkingToConsumer pid) where
  condense TalkingToConsumer{..} = condense (consumerUs, consumerThem)

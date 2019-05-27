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

import           Codec.Serialise (Serialise (encode))
import           Control.Monad
import           Control.Tracer (nullTracer)
import           Crypto.Number.Generate (generateBetween)
import           Crypto.Random (ChaChaDRG, drgNew)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

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
import qualified Ouroboros.Network.Chain as Chain
import           Ouroboros.Network.Protocol.BlockFetch.Codec
import           Ouroboros.Network.Protocol.BlockFetch.Type
import           Ouroboros.Network.Protocol.ChainSync.Codec
import           Ouroboros.Network.Protocol.ChainSync.Type

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.ChainSyncClient (ClockSkew (..))
import           Ouroboros.Consensus.Demo
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Mock
import qualified Ouroboros.Consensus.Ledger.Mock as Mock
import           Ouroboros.Consensus.Node
import           Ouroboros.Consensus.Protocol.Abstract
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
broadcastNetwork :: forall m p c.
                    ( MonadAsync m
                    , MonadFork  m
                    , MonadMask  m
                    , MonadSay   m
                    , MonadTime  m
                    , MonadTimer m
                    , MonadThrow (STM m)
                    , RunDemo p
                    , SimpleBlockCrypto c
                    , Block p ~ SimpleBlock p c
                    , SupportedBlock p (SimpleHeader p c)
                    , Serialise (Payload p (SimplePreHeader p c))
                    )
                 => ThreadRegistry m
                 -> BlockchainTime m
                 -> NumCoreNodes
                 -> (CoreNodeId -> ProtocolInfo p)
                 -> ChaChaDRG
                 -> NumSlots
                 -> m (Map NodeId (Chain (SimpleBlock p c)))
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

    chans :: NodeChans m (SimpleBlock p c) (SimpleHeader p c) <- createCommunicationChannels

    varRNG <- atomically $ newTVar initRNG

    nodes <- forM coreNodeIds $ \coreNodeId -> do
      let us               = fromCoreNodeId coreNodeId
          ProtocolInfo{..} = pInfo coreNodeId

      let callbacks :: NodeCallbacks m (SimpleBlock p c)
          callbacks = NodeCallbacks {
              produceBlock = \proof l slot prevPoint prevNo _txs -> do
                let prevHash  = castHash (Chain.pointHash prevPoint)
                    curNo     = succ prevNo

                -- We ignore the transactions from the mempool (which will be
                -- empty), and instead produce some random transactions
                txs <- genTxs addrs (getUtxo l)
                demoForgeBlock pInfoConfig
                           slot
                           curNo
                           prevHash
                           txs
                           proof

            , produceDRG      = atomically $ simChaChaT varRNG id $ drgNew
            }

      chainDB <- ChainDB.openDB encode pInfoConfig pInfoInitLedger simpleHeader

      let nodeParams = NodeParams
            { tracer             = nullTracer
            , encoder            = encode
            , threadRegistry     = registry
            , maxClockSkew       = ClockSkew 1
            , cfg                = pInfoConfig
            , initState          = pInfoInitState
            , btime
            , chainDB
            , callbacks
            , blockFetchSize     = headerBlockSize . headerPreHeader
            , blockMatchesHeader = Mock.blockMatchesHeader
            }

      node <- nodeKernel nodeParams

      forM_ (filter (/= us) nodeIds) $ \them -> do
        let mkCommsDown :: Show bytes
                        => (NodeChan m (SimpleBlock p c) (SimpleHeader p c) -> Channel m bytes)
                        -> Codec ps e m bytes -> NodeComms m ps e bytes
            mkCommsDown getChan codec = NodeComms {
                ncCodec    = codec
              , ncWithChan = \cc -> cc $
                  loggingChannel (TalkingToConsumer us them) $
                    getChan (chans Map.! us Map.! them)
              }
            mkCommsUp :: Show bytes
                      => (NodeChan m (SimpleBlock p c) (SimpleHeader p c) -> Channel m bytes)
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

      return (us, node)

    -- STM variable to record the final chains of the nodes
    varRes <- atomically $ newTVar Nothing

    onSlot btime (finalSlot numSlots) $ do
      -- Wait a random amount of time after the final slot for the block fetch
      -- and chain sync to finish
      threadDelay 2000
      res <- fmap Map.fromList $ forM nodes $ \(us, node) ->
        (us, ) <$> ChainDB.toChain (getChainDB node)
      atomically $ writeTVar varRes (Just res)

    atomically $ blockUntilJust (readTVar varRes)
  where
    nodeIds :: [NodeId]
    nodeIds = map fromCoreNodeId coreNodeIds

    coreNodeIds :: [CoreNodeId]
    coreNodeIds = enumCoreNodes numCoreNodes

    getUtxo :: ExtLedgerState (SimpleBlock p c) -> Utxo
    getUtxo = slsUtxo . ledgerState

    createCommunicationChannels :: m (NodeChans m (SimpleBlock p c) (SimpleHeader p c))
    createCommunicationChannels = fmap Map.fromList $ forM nodeIds $ \us ->
      fmap ((us, ) . Map.fromList) $ forM (filter (/= us) nodeIds) $ \them -> do
        (chainSyncConsumer,  chainSyncProducer)  <- createConnectedChannels
        (blockFetchConsumer, blockFetchProducer) <- createConnectedChannels
        return (them, NodeChan {..})


{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

-- | Communication channel used for the Chain Sync protocol
type ChainSyncChannel m hdr = Channel m (AnyMessage (ChainSync hdr (Point hdr)))

-- | Communication channel used for the Block Fetch protocol
type BlockFetchChannel m blk hdr = Channel m (AnyMessage (BlockFetch hdr blk))

-- | The communication channels from and to each node
data NodeChan m blk hdr = NodeChan
  { chainSyncConsumer  :: ChainSyncChannel  m     hdr
  , chainSyncProducer  :: ChainSyncChannel  m     hdr
  , blockFetchConsumer :: BlockFetchChannel m blk hdr
  , blockFetchProducer :: BlockFetchChannel m blk hdr
  }

-- | All connections between all nodes
type NodeChans m blk hdr = Map NodeId (Map NodeId (NodeChan m blk hdr))


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

{-# LANGUAGE GADTs                #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

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
import           Data.Functor.Identity

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork (MonadFork)
import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer

import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Codec (Codec, CodecFailure, isoCodec)

import           Ouroboros.Network.Channel
import           Ouroboros.Network.Codec (AnyMessage (..))

import           Ouroboros.Network.Block
import           Ouroboros.Network.Chain
import           Ouroboros.Network.Protocol.ChainSync.Codec
import           Ouroboros.Network.Mux.Interface
import           Ouroboros.Network.NodeToNode

import           Ouroboros.Network.Protocol.ChainSync.Type
import           Ouroboros.Network.Protocol.BlockFetch.Type
import           Ouroboros.Network.Protocol.BlockFetch.Codec

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.ChainSyncClient (ClockSkew (..))
import           Ouroboros.Consensus.Demo
import           Ouroboros.Consensus.Demo.Run
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Mock
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Node
import           Ouroboros.Consensus.NodeNetwork
import           Ouroboros.Consensus.Protocol.Abstract (NodeConfig)
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Consensus.Util.Random
import           Ouroboros.Consensus.Util.STM
import           Ouroboros.Consensus.Util.ThreadRegistry

import qualified Ouroboros.Storage.ChainDB.API as ChainDB
import qualified Ouroboros.Storage.ChainDB.Mock as ChainDB


-- | Existential wrapper which let us encode / decode using identity
-- codecs.
--
data AnyProtocolMessage blk where
     ChainSyncMsg :: Message (ChainSync (Header blk) (Point blk)) st st' -> AnyProtocolMessage blk
     BlockFetchMsg :: Message (BlockFetch blk) st st' -> AnyProtocolMessage blk

-- This instance constraint is no smaller than the instance head, and thus
-- @UndecidableInstances@ extension is required
--
instance ( Show blk
         , Show (Header blk)
         , StandardHash blk
         ) => Show (AnyProtocolMessage blk) where
    show (ChainSyncMsg msg) = show msg
    show (BlockFetchMsg msg) = show msg


codecChainSyncId_ :: forall blk m. Monad m
                  => Codec (ChainSync (Header blk) (Point blk))
                           CodecFailure m (AnyProtocolMessage blk)
codecChainSyncId_ = isoCodec fromAnyChainSyncMessage toAnyChainSyncMessage codecChainSyncId
    where
      fromAnyChainSyncMessage :: AnyMessage (ChainSync (Header blk) (Point blk)) -> AnyProtocolMessage blk
      fromAnyChainSyncMessage (AnyMessage msg) = ChainSyncMsg msg

      toAnyChainSyncMessage :: AnyProtocolMessage blk -> AnyMessage (ChainSync (Header blk) (Point blk))
      toAnyChainSyncMessage (ChainSyncMsg msg) = AnyMessage msg
      -- it means we sent a message through wrong channel
      toAnyChainSyncMessage _                  = error "toAnyChainSyncMessage: message type missmatch"


codecBlockFetchId_ :: forall blk m. Monad m
                   => Codec (BlockFetch blk)
                            CodecFailure m (AnyProtocolMessage blk)
codecBlockFetchId_ = isoCodec fromAnyBlockFetchMessage toAnyBlockFetchMessage codecBlockFetchId
    where
      fromAnyBlockFetchMessage :: AnyMessage (BlockFetch blk) -> AnyProtocolMessage blk
      fromAnyBlockFetchMessage (AnyMessage msg) = BlockFetchMsg msg

      toAnyBlockFetchMessage :: AnyProtocolMessage blk -> AnyMessage (BlockFetch blk)
      toAnyBlockFetchMessage (BlockFetchMsg msg) = AnyMessage msg
      -- it means we sent a message through wrong channel
      toAnyBlockFetchMessage _                   = error "toAnyBlockFetchMessage: message type missmatch"

-- | Interface provided by 'ouroboros-network'.  At the moment
-- 'ouroboros-network' only provides this interface in 'IO' backed by sockets,
-- we cook up here one using 'NodeChans'.
--
data NetworkInterface m up = NetworkInterface {
      -- | Like 'Ouroboros.Network.NodeToNode.nodeToNodeConnectTo'
      --
      niConnectTo :: up -> m ()

      -- | Like 'Ouroboros.Network.NodeToNode.withServerNodeToNode'
      --
    , niWithServerNode :: forall t.  (Async m () -> m t) -> m t
    }

-- | Create 'NetworkInterface' from a map of channels between nodes.
--
-- TODO: move this function to 'ouroboros-network'.
--
createNetworkInterface
    :: forall m nodeId blk.
       ( MonadAsync m
       , MonadFork  m
       , MonadMask  m
       , MonadSay   m
       , StandardHash blk
       , Show blk
       , Show (Header blk)
       , Ord nodeId
       , Show nodeId
       )
    => NodeChans m nodeId blk -- ^ map of channels between nodes
    -> [nodeId]               -- ^ list of nodes which we want to serve
    -> nodeId                 -- ^ our nodeId
    -> NetworkApplication m Identity nodeId blk (AnyProtocolMessage blk) () ()
    -> NetworkInterface m nodeId
createNetworkInterface chans nodeIds us NetworkApplication {naMuxInitiatorApp, naMuxResponderApp} = NetworkInterface
    { niConnectTo = \them -> do
        Identity app <- runSharedState $ naMuxInitiatorApp them
        case app of
          MuxInitiatorApplication f -> do
            let nodeChan = chans Map.! them Map.! us

                fn ptcl@ChainSyncWithHeadersPtcl =
                  void $ f ptcl
                       $ loggingChannel (TalkingToProducer us them)
                       $ chainSyncProducer nodeChan
                fn ptcl@BlockFetchPtcl =
                  void $ f ptcl
                       $ loggingChannel (TalkingToProducer us them)
                       $ blockFetchProducer nodeChan

            as <- traverse (async . fn) [minBound .. maxBound]
            -- wait for all the threads, if any throws an exception, cancel all
            -- of them; this is consistent with
            -- 'Ouroboros.Network.Socket.connectTo'.
            void $ waitAnyCancel as

    , niWithServerNode = \k -> do
        ts :: [Async m ()] <- fmap concat $ forM (filter (/= us) nodeIds) $ \them -> do
          let nodeChan = chans Map.! us Map.! them
              fn :: NodeToNodeProtocols -> m ()
              fn ptcl@ChainSyncWithHeadersPtcl = do
                case runIdentity naMuxResponderApp of
                  MuxResponderApplication f -> do
                    void $ f ptcl
                         $ loggingChannel (TalkingToConsumer us them)
                         $ chainSyncConsumer nodeChan
              fn ptcl@BlockFetchPtcl =
                case runIdentity naMuxResponderApp of
                  MuxResponderApplication f -> do
                    void $ f ptcl
                         $ loggingChannel (TalkingToConsumer us them)
                         $ blockFetchConsumer nodeChan
          forM [minBound .. maxBound] (async . fn)

        -- if any thread raises an exception, we kill all of them
        waitAnyCancel ts >>= k . fst
    }


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
                    , MonadST    m
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

    chans :: NodeChans m NodeId (SimpleBlock c ext) <- createCommunicationChannels

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
      let app = consensusNetworkApps
                  Identity
                  nullTracer
                  nullTracer
                  codecChainSyncId_
                  codecBlockFetchId_
                  nodeParams
                  node

          ni :: NetworkInterface m NodeId
          ni = createNetworkInterface chans nodeIds us app

      void $ forkLinked registry (niWithServerNode ni wait)
      forM_ (filter (/= us) nodeIds) $ \them -> forkLinked registry (niConnectTo ni them)

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

    createCommunicationChannels :: m (NodeChans m NodeId (SimpleBlock c ext))
    createCommunicationChannels = fmap Map.fromList $ forM nodeIds $ \us ->
      fmap ((us, ) . Map.fromList) $ forM (filter (/= us) nodeIds) $ \them -> do
        (chainSyncConsumer,  chainSyncProducer)  <- createConnectedChannels
        (blockFetchConsumer, blockFetchProducer) <- createConnectedChannels
        return (them, NodeChan {..})


{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

-- | The communication channels from and to each node
data NodeChan m blk = NodeChan
  { chainSyncConsumer  :: Channel m (AnyProtocolMessage blk)
  , chainSyncProducer  :: Channel m (AnyProtocolMessage blk)
  , blockFetchConsumer :: Channel m (AnyProtocolMessage blk)
  , blockFetchProducer :: Channel m (AnyProtocolMessage blk)
  }

-- | All connections between all nodes
type NodeChans m nodeId blk = Map nodeId (Map nodeId (NodeChan m blk))


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
data TalkingToProducer nodeId pid = TalkingToProducer {
      producerUs   :: nodeId
    , producerThem :: pid
    }
  deriving (Show)

-- | Message sent by or to a consumer
data TalkingToConsumer nodeId cid = TalkingToConsumer {
      consumerUs   :: nodeId
    , consumerThem :: cid
    }
  deriving (Show)

instance (Condense nodeId, Condense pid) => Condense (TalkingToProducer nodeId pid) where
  condense TalkingToProducer{..} = condense (producerUs, producerThem)

instance (Condense nodeId, Condense pid) => Condense (TalkingToConsumer nodeId pid) where
  condense TalkingToConsumer{..} = condense (consumerUs, consumerThem)

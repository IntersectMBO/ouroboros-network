{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

-- | Setup network
module Test.Dynamic.Network (
    broadcastNetwork
  ) where

import           Control.Monad
import           Crypto.Number.Generate (generateBetween)
import           Crypto.Random (DRG)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadTimer

import           Protocol.Channel
import           Protocol.Transition

import           Ouroboros.Network.Block
import           Ouroboros.Network.Chain
import qualified Ouroboros.Network.Chain as Chain
import           Ouroboros.Network.Protocol.ChainSync.Codec.Id
import           Ouroboros.Network.Protocol.ChainSync.Type

import           Ouroboros.Consensus.Crypto.Hash
import           Ouroboros.Consensus.Demo
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Mock
import           Ouroboros.Consensus.Node
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Consensus.Util.Random
import           Ouroboros.Consensus.Util.STM

-- | Setup fully-connected topology, where every node is both a producer
-- and a consumer
--
-- We run for the specified number of blocks, then return the final state of
-- each node.
broadcastNetwork :: forall m p gen.
                    ( MonadSTM   m
                    , MonadTimer m
                    , MonadSay   m
                    , DemoProtocolConstraints p
                    , DRG gen
                    )
                 => BlockchainTime m
                 -> NumCoreNodes
                 -> (CoreNodeId -> ProtocolInfo p)
                 -> gen
                 -> NumSlots
                 -> m (Map NodeId (Chain (Block p)))
broadcastNetwork btime numCoreNodes pInfo initRNG numSlots = do

    -- all known addresses
    let addrs :: [Addr]
        addrs = Set.toList . Set.fromList . concat
              $ [ nodeAddrs
                | node <- coreNodeIds
                , let nodeLedger = pInfoInitLedger (pInfo node)
                      nodeUtxo   = getUtxo nodeLedger
                      nodeAddrs  = map fst (Map.elems nodeUtxo)
                ]

    -- Create the communication channels
    chans :: NodeChans m (Block p)
       <- fmap Map.fromList $ forM nodeIds $ \us -> do
               fmap (us, ) $ fmap Map.fromList $ forM nodeIds $ \them ->
                 fmap (them, ) $
                   -- for no, zero delay
                   createCoupledChannels 0 0

    varRNG <- atomically $ newTVar initRNG

    nodes <- forM coreNodeIds $ \coreNodeId -> do
      let us               = fromCoreNodeId coreNodeId
          ProtocolInfo{..} = pInfo coreNodeId

      -- STM variable to record the final chain of the node
      varRes <- atomically $ newTVar Nothing

      let callbacks :: NodeCallbacks m (MonadPseudoRandomT gen) (Block p)
          callbacks = NodeCallbacks {
              produceBlock = \proof l slot prevPoint prevNo -> do
                let prevHash  = castHash (Chain.pointHash prevPoint)
                    curNo     = succ prevNo

                -- Produce some random transactions
                txs <- genTxs addrs (getUtxo l)
                forgeBlock pInfoConfig
                           slot
                           curNo
                           prevHash
                           (Map.fromList $ [(hash t, t) | t <- txs])
                           proof

            , adoptedNewChain = \_newChain -> return ()
            }

      node <- nodeKernel
                pInfoConfig
                pInfoInitState
                (simMonadPseudoRandomT varRNG)
                btime
                pInfoInitLedger
                pInfoInitChain
                callbacks

      let withLogging :: Show id
                      => id
                      -> NodeChan m (Block p)
                      -> NodeChan m (Block p)
          withLogging chId = loggingChannel chId
                                            (withSomeTransition show)
                                            (withSomeTransition show)

      forM_ (filter (/= us) nodeIds) $ \them -> do
        registerDownstream (nodeNetworkLayer node) codecChainSync $ \cc ->
          cc $ withLogging (TalkingToConsumer us them) $
                 fst (chans Map.! us Map.! them)
        registerUpstream (nodeNetworkLayer node) them codecChainSync $ \cc ->
          cc $ withLogging (TalkingToProducer us them) $
                 snd (chans Map.! them Map.! us)

      onSlot btime (finalSlot numSlots) $ atomically $ do
        chain <- getCurrentChain node
        writeTVar varRes $ Just (us, chain)

      return varRes

    atomically $ Map.fromList <$> blockUntilAllJust nodes
  where
    nodeIds :: [NodeId]
    nodeIds = map fromCoreNodeId coreNodeIds

    coreNodeIds :: [CoreNodeId]
    coreNodeIds = enumCoreNodes numCoreNodes

    getUtxo :: ExtLedgerState (Block p) -> Utxo
    getUtxo = slsUtxo . ledgerState

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

-- | Communication channel
type NodeChan  m b = Channel m (SomeTransition (ChainSyncMessage b (Point b)))

-- | All connections between all nodes
type NodeChans m b = Map NodeId (Map NodeId (NodeChan m b, NodeChan m b))

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

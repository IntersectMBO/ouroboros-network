{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}

module Test.Dynamic.General (
    prop_simple_protocol_convergence
  , BlockUnderTest
  , TestConfig (..)
  , VTime
  , allEqual
  , nodeStake
  , numNodes
  , k
  , kPerEpoch
  , numEpochs
  , numSlots
  , shortestLength
  ) where

import           Codec.Serialise.Class (Serialise)
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.ST.Lazy (runST)
import           Crypto.Number.Generate (generateBetween)
import           Crypto.Random (DRG)
import           Data.Foldable (foldl')
import           Data.Functor.Identity
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Numeric.Natural (Natural)
import           Test.QuickCheck

import           Protocol.Channel
import           Protocol.Transition

import           Ouroboros.Network.Block
import           Ouroboros.Network.Chain
import qualified Ouroboros.Network.Chain as Chain
import           Ouroboros.Network.MonadClass
import           Ouroboros.Network.Protocol.ChainSync.Codec.Id
import           Ouroboros.Network.Protocol.ChainSync.Type
import           Ouroboros.Network.Sim (VTime)

import           Ouroboros.Consensus.Crypto.Hash
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Mock
import           Ouroboros.Consensus.Node
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.ExtNodeConfig
import           Ouroboros.Consensus.Protocol.Test
import           Ouroboros.Consensus.Util (Condense (..))
import qualified Ouroboros.Consensus.Util.Chain as Chain
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Consensus.Util.Random
import           Ouroboros.Consensus.Util.STM

data TestConfig = TestConfig {
      testAddressDistribution :: Map Addr NodeId
      -- ^ Some form of mapping that allows us to partition incoming
      -- transactions in a way that we can accumulate the stake properly.
    }

ourAddr :: TestConfig -> NodeId -> Addr -> Bool
ourAddr TestConfig{..} myNodeId address =
    fmap ((==) myNodeId) (Map.lookup address testAddressDistribution)
        == Just True

nodeStake :: TestConfig -> Utxo -> NodeId -> Int
nodeStake cfg u nodeId =
    Map.foldl
        (\acc (a, stake) -> if ourAddr cfg nodeId a then acc + stake else acc)
        0
        u

numNodes, numEpochs, k, kPerEpoch, numSlots :: Int
numNodes  = 3
numEpochs = 4
k         = 5
kPerEpoch = 3
numSlots  = k * kPerEpoch * numEpochs

type ProtocolUnderTest p = ExtNodeConfig TestConfig (TestProtocol p)
type BlockUnderTest p    = SimpleBlock (ProtocolUnderTest p) SimpleBlockStandardCrypto

prop_simple_protocol_convergence :: forall p.
                                    ( ProtocolLedgerView (BlockUnderTest p), OuroborosTag p
                                    , Serialise (Payload p (SimplePreHeader (ProtocolUnderTest p) SimpleBlockStandardCrypto))
                                    , Eq (Payload p (SimplePreHeader (ProtocolUnderTest p) SimpleBlockStandardCrypto))
                                    )
                                 => (Int -> NodeConfig p)
                                 -> (Int -> NodeState (ProtocolUnderTest p))
                                 -> ChainState p
                                 -> (   [NodeId]
                                     -> [(VTime, Map NodeId (Chain (BlockUnderTest p)))]
                                     -> Property)
                                 -> Seed
                                 -> Property
prop_simple_protocol_convergence mkConfig mkState initialChainState isValid seed =
    runST $ test_simple_protocol_convergence mkConfig mkState initialChainState isValid seed

-- Run protocol on the broadcast network, and check resulting chains on all nodes.
test_simple_protocol_convergence :: forall m n p.
                                    ( MonadSTM m
                                    , MonadRunProbe m n
                                    , MonadSay m
                                    , MonadTimer m
                                    , ProtocolLedgerView (BlockUnderTest p)
                                    , OuroborosTag p
                                    , Serialise (Payload p (SimplePreHeader (ProtocolUnderTest p) SimpleBlockStandardCrypto))
                                    , Eq (Payload p (SimplePreHeader (ProtocolUnderTest p) SimpleBlockStandardCrypto))
                                    )
                                 => (Int -> NodeConfig p)
                                 -> (Int -> NodeState (ProtocolUnderTest p))
                                 -> ChainState p
                                 -> (   [NodeId]
                                     -> [(Time m, Map NodeId (Chain (BlockUnderTest p)))]
                                     -> Property)
                                 -> Seed
                                 -> n Property
test_simple_protocol_convergence mkConfig mkState initialChainState isValid seed = do
    fmap (isValid $ Map.keys nodeInit) $ withProbe $ go
  where
    go :: Probe m (Map NodeId (Chain (BlockUnderTest p))) -> m ()
    go p = do
      btime <- testBlockchainTime numSlots 100000
      finalChains <- broadcastNetwork
                       btime
                       nodeInit
                       initLedgerState
                       (seedToChaCha seed)
      probeOutput p finalChains

    -- Give everybody 1000 coins at the beginning.
    genesisTx :: Tx
    genesisTx = Tx mempty [ (a, 1000)
                          | a <- Map.keys (testAddressDistribution testConfig)
                          ]

    testConfig :: TestConfig
    testConfig = TestConfig {
          testAddressDistribution =
            Map.fromList $
              zip (map (:[]) $ take numNodes ['a' .. 'z'])
                  (map CoreId [0 .. numNodes - 1])
        }

    initLedgerState :: ExtLedgerState (BlockUnderTest p)
    initLedgerState = ExtLedgerState {
          ledgerState         =
              let Right u = runIdentity . runExceptT $ utxo genesisTx
              in SimpleLedgerState u mempty
        , ouroborosChainState = initialChainState
        }

    nodeInit :: Map NodeId ( NodeConfig (ProtocolUnderTest p)
                           , NodeState (ProtocolUnderTest p)
                           , Chain (BlockUnderTest p)
                           )
    nodeInit = Map.fromList $ [ (CoreId i, ( mkEncConfig i
                                           , mkState i
                                           , Genesis)
                                           )
                              | i <- [0 .. numNodes - 1]
                              ]
    mkEncConfig :: Int -> NodeConfig (ProtocolUnderTest p)
    mkEncConfig i = EncNodeConfig
        { encNodeConfigP   = TestNodeConfig
            { testNodeConfigP  = mkConfig i
            , testNodeConfigId = CoreId i
            }
        , encNodeConfigExt = testConfig
        }

-- | Communication channel
type NodeChan  m b = Channel m (SomeTransition (ChainSyncMessage b (Point b)))

-- | All connections between all nodes
type NodeChans m b = Map NodeId (Map NodeId (NodeChan m b, NodeChan m b))

-- | Setup fully-connected topology, where every node is both a producer
-- and a consumer
--
-- We run for the specified number of blocks, then return the final state of
-- each node.
broadcastNetwork :: forall m p c gen.
                    ( MonadSTM   m
                    , MonadTimer m
                    , MonadSay   m
                    , SimpleBlockCrypto c
                    , ProtocolLedgerView (SimpleBlock p c)
                    , Serialise (Payload p (SimplePreHeader p c))
                    , Eq (Payload p (SimplePreHeader p c))
                    , DRG gen
                    )
                 => BlockchainTime m
                 -> Map NodeId ( NodeConfig p
                               , NodeState p
                               , Chain (SimpleBlock p c)
                               )
                 -- ^ Node initial state and initial chain
                 -> ExtLedgerState (SimpleBlock p c)
                 -- ^ Initial ledger state
                 -> gen
                 -- ^ Initial random number state
                 -> m (Map NodeId (Chain (SimpleBlock p c)))
broadcastNetwork btime nodeInit initLedger initRNG = do

    -- all known addresses
    let addrs = Set.toList $ Set.fromList $ map fst $ Map.elems $ getUtxo initLedger

    -- Creates the communication channels, /including/ the one to be used
    -- to talk to ourselves. Such \"feedback loop\" is handy to be able to
    -- /actually/ apply the ledger rules.
    chans :: NodeChans m (SimpleBlock p c)
       <- fmap Map.fromList $ forM nodeIds $ \us -> do
               fmap (us, ) $ fmap Map.fromList $ forM nodeIds $ \them ->
                 fmap (them, ) $
                   createCoupledChannels
                     @(SomeTransition (ChainSyncMessage (SimpleBlock p c) (Point (SimpleBlock p c))))
                     @(SomeTransition (ChainSyncMessage (SimpleBlock p c) (Point (SimpleBlock p c))))
                     0
                     0

    varRNG <- atomically $ newTVar initRNG

    nodes <- forM (Map.toList nodeInit) $ \(us, (cfg, initSt, initChain)) -> do
      varRes <- atomically $ newTVar Nothing

      let callbacks :: NodeCallbacks m (MonadPseudoRandomT gen) (SimpleBlock p c)
          callbacks = NodeCallbacks {
              produceBlock = \proof l slot prevPoint prevNo -> do
                let prevHash  = castHash (Chain.pointHash prevPoint)
                    curNo     = succ prevNo

                -- Produce some random transactions
                txs <- genTxs addrs (getUtxo l)
                forgeBlock cfg
                           slot
                           curNo
                           prevHash
                           (Map.fromList $ [(hash t, t) | t <- txs])
                           proof

            , adoptedNewChain = \_newChain -> return ()
            }

      node <- nodeKernel
                cfg
                initSt
                (simMonadPseudoRandomT varRNG)
                btime
                initLedger
                initChain
                callbacks

      let withLogging :: Show id
                      => id
                      -> NodeChan m (SimpleBlock p c)
                      -> NodeChan m (SimpleBlock p c)
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

      onSlot btime (fromIntegral numSlots) $ atomically $ do
        chain <- getCurrentChain node
        writeTVar varRes $ Just (us, chain)

      return varRes

    atomically $ Map.fromList <$> collectAllJust nodes
  where
    nodeIds :: [NodeId]
    nodeIds = Map.keys nodeInit

    getUtxo :: ExtLedgerState (SimpleBlock p c) -> Utxo
    getUtxo l = let SimpleLedgerState u _ = ledgerState l in u

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

collectAllJust :: MonadSTM m => [TVar m (Maybe a)] -> Tr m [a]
collectAllJust = mapM collectJust

collectJust :: MonadSTM m => TVar m (Maybe a) -> Tr m a
collectJust var = do
    ma <- readTVar var
    case ma of
      Nothing -> retry
      Just a  -> return a

allEqual :: forall b. (Condense b, Eq b, HasHeader b) => [Chain b] -> Property
allEqual []             = property True
allEqual [_]            = property True
allEqual (x : xs@(_:_)) =
    let c = foldl' Chain.commonPrefix x xs
    in  foldl' (\prop d -> prop .&&. f c d) (property True) xs
  where
    f :: Chain b -> Chain b -> Property
    f c d = counterexample (g c d) $ c == d

    g :: Chain b -> Chain b -> String
    g c d = case (Chain.lastSlot c, Chain.lastSlot d) of
        (Nothing, Nothing) -> error "impossible case"
        (Nothing, Just t)  ->    "empty intersection of non-empty chains (one reaches slot "
                              <> show (getSlot t)
                              <> " and contains "
                              <> show (Chain.length d)
                              <> "blocks): "
                              <> condense d
        (Just _, Nothing)  -> error "impossible case"
        (Just s, Just t)   ->    "intersection reaches slot "
                              <> show (getSlot s)
                              <> " and has length "
                              <> show (Chain.length c)
                              <> ", but at least one chain reaches slot "
                              <> show (getSlot t)
                              <> " and has length "
                              <> show (Chain.length d)
                              <> ": "
                              <> condense c
                              <> " /= "
                              <> condense d

shortestLength :: Map NodeId (Chain b) -> Natural
shortestLength = fromIntegral . minimum . map Chain.length . Map.elems

{-------------------------------------------------------------------------------
  Generating random transactions
-------------------------------------------------------------------------------}

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

genTxs :: MonadRandom m => [Addr] -> Utxo -> m [Tx]
genTxs addr u = do
    b <- generateBetween 0 1
    if b == 0
        then return []
        else do
            tx <- genTx addr u
            return [tx]

{-------------------------------------------------------------------------------
  Logging support
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

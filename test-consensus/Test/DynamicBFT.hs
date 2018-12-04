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
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS -fno-warn-unused-binds #-}
{-# OPTIONS -fno-warn-orphans #-}

module Test.DynamicBFT (
    tests
  , TestConfig(..)
  , allEqual
  , nodeStake
  , broadcastNetwork
  ) where

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.ST.Lazy (runST)
import           Crypto.Random (DRG)
import           Data.Foldable (foldl', foldlM)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Proxy
import           Data.Set (Set)
import qualified Data.Set as Set
import           Test.QuickCheck

import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Ouroboros.Network.Block
import           Ouroboros.Network.Chain
import qualified Ouroboros.Network.Chain as Chain
import           Ouroboros.Network.MonadClass
import           Ouroboros.Network.Protocol

import           Ouroboros.Consensus.Crypto.DSIGN.Mock
import           Ouroboros.Consensus.Crypto.Hash.Class (hash)
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Mock
import           Ouroboros.Consensus.Node
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.BFT
import           Ouroboros.Consensus.Protocol.ExtNodeConfig
import           Ouroboros.Consensus.Protocol.Test
import           Ouroboros.Consensus.Util (Condense (..))
import qualified Ouroboros.Consensus.Util.Chain as Chain
import           Ouroboros.Consensus.Util.Random
import           Ouroboros.Consensus.Util.STM

tests :: TestTree
tests = testGroup "Dynamic chain generation" [
      testProperty "simple BFT convergence" prop_simple_bft_convergence
    ]

prop_simple_bft_convergence :: Seed -> Property
prop_simple_bft_convergence seed = runST $ test_simple_bft_convergence seed

-- Run BFT on the broadcast network, and check that all nodes converge to the
-- same final chain
test_simple_bft_convergence :: forall m n.
                               ( MonadSTM m
                               , MonadRunProbe m n
                               , MonadSay m
                               , MonadTimer m
                               )
                            => Seed -> n Property
test_simple_bft_convergence seed = do
    fmap isValid $ withProbe $ go
  where
    numNodes, numSlots :: Int
    numNodes = 3
    numSlots = 10

    go :: Probe m (Map NodeId (Chain BlockUnderTest)) -> m ()
    go p = do
      txMap <- genTxMap
      finalChains <- broadcastNetwork
                       numSlots
                       nodeInit
                       txMap
                       initLedgerState
                       (seedToChaCha seed)
      probeOutput p finalChains

    -- Give everybody 1000 coins at the beginning.
    genesisTx :: Tx
    genesisTx = Tx mempty [ (a, 1000)
                          | a <- Map.keys (testAddressDistribution testConfig)
                          ]

    -- TODO: We might want to have some more interesting transactions in
    -- the future here.
    genTxMap :: m (Map Slot (Set Tx))
    genTxMap = do
            -- TODO: This doesn't do anything at the moment, but ideally
            -- we would need some randomness to shuffle the TxOut, divvy the
            -- unspent output and distribute it randomly to the nodes, to
            -- create an interesting test.
        let divvy :: Int -> [TxOut] -> m [TxOut]
            divvy currentSlot xs = do
              let totalCoins = foldl (\acc (_,c) -> acc + c) 0 xs
              return $ foldl (\acc ((addr, _),nid) ->
                               if currentSlot `mod` nid == 0 then (addr, totalCoins) : acc
                                                             else (addr, 0) : acc
                             ) mempty (zip xs [1..])

        -- Move the stake around. Use the previous Tx in the accumulator a
        -- like a UTxO, as we are moving all the stake all the time, in order
        -- to make the observable state interesting.
        Map.fromList . snd <$>
            foldlM (\(prevTx@(Tx _ oldTxOut), acc) sl -> do
                    let txIn  = Set.fromList $ [ (hash prevTx, n) | n <- [0 .. numNodes - 1] ]
                    txOut <- divvy sl oldTxOut
                    let newTx = Tx txIn txOut
                    return (newTx, (Slot $ fromIntegral sl, Set.singleton newTx) : acc)
                  ) (genesisTx, [(Slot 1, Set.singleton genesisTx)]) [2 .. numSlots]

    testConfig :: TestConfig
    testConfig = TestConfig {
          testAddressDistribution =
            Map.fromList $
              zip (map (:[]) $ take numNodes ['a' .. 'z'])
                  (map CoreId [0 .. numNodes - 1])
        }

    verKeys :: Map NodeId (VerKeyDSIGN (BftDSIGN BftMockCrypto))
    verKeys = Map.fromList [ (CoreId i, VerKeyMockDSIGN i)
                           | i <- [0 .. numNodes - 1]
                           ]

    initLedgerState :: ExtLedgerState BlockUnderTest
    initLedgerState = ExtLedgerState {
          ledgerState         = SimpleLedgerState (utxo genesisTx)
        , ouroborosChainState = ()
        }

    nodeInit :: Map NodeId ( NodeConfig ProtocolUnderTest
                           , NodeState ProtocolUnderTest
                           , Chain BlockUnderTest
                           )
    nodeInit = Map.fromList $ [ (CoreId i, ( mkConfig i
                                           , mkState i
                                           , Genesis)
                                           )
                              | i <- [0 .. numNodes - 1]
                              ]
      where
        mkConfig :: Int -> NodeConfig ProtocolUnderTest
        mkConfig i = EncNodeConfig {
              encNodeConfigP = TestNodeConfig {
                  testNodeConfigP = BftNodeConfig {
                      bftNodeId   = CoreId i
                    , bftSignKey  = SignKeyMockDSIGN i
                    , bftNumNodes = fromIntegral numNodes
                    , bftVerKeys  = verKeys
                    }
                , testNodeConfigId = CoreId i
                }
            , encNodeConfigExt = testConfig
            }

        mkState :: Int -> NodeState ProtocolUnderTest
        mkState _ = ()

    isValid :: [(Time m, Map NodeId (Chain BlockUnderTest))] -> Property
    isValid trace = counterexample (show trace) $
      case trace of
        [(_, final)] -> Map.keys final == Map.keys nodeInit
                   .&&. allEqual (takeChainPrefix <$> Map.elems final)
        _otherwise   -> property False

    takeChainPrefix :: Chain BlockUnderTest -> Chain BlockUnderTest
    takeChainPrefix = id -- in BFT, chains should indeed all be equal.

{-------------------------------------------------------------------------------
  Test blocks

  TODO: We'll want to test other protocols also
-------------------------------------------------------------------------------}

type ProtocolUnderTest = ExtNodeConfig TestConfig (TestProtocol (Bft BftMockCrypto))
type BlockUnderTest    = SimpleBlock ProtocolUnderTest SimpleBlockStandardCrypto
type HeaderUnderTest   = SimpleHeader ProtocolUnderTest SimpleBlockStandardCrypto

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

instance HasPayload (Bft BftMockCrypto) BlockUnderTest where
  blockPayload _ = testPayloadP
                 . encPayloadP
                 . headerOuroboros
                 . simpleHeader

instance ProtocolLedgerView BlockUnderTest where
  protocolLedgerView (EncNodeConfig _ cfg) (SimpleLedgerState u) =
      ((), nodeStake cfg u)

{-------------------------------------------------------------------------------
  Infrastructure
-------------------------------------------------------------------------------}

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
                    , DRG gen
                    )
                 => Int
                 -- ^ Number of slots to run for
                 -> Map NodeId ( NodeConfig p
                               , NodeState p
                               , Chain (SimpleBlock p c)
                               )
                 -- ^ Node initial state and initial chain
                 -> Map Slot (Set Tx)
                 -- ^ For each slot, the transactions to be incorporated into
                 -- a block.
                 -> ExtLedgerState (SimpleBlock p c)
                 -- ^ Initial ledger state
                 -> gen
                 -- ^ Initial random number state
                 -> m (Map NodeId (Chain (SimpleBlock p c)))
broadcastNetwork numSlots nodeInit txMap initLedger initRNG = do
    -- Creates the communication channels, /including/ the one to be used
    -- to talk to ourselves. Such \"feedback loop\" is handy to be able to
    -- /actually/ apply the ledger rules.
    chans <- fmap Map.fromList $ forM nodeIds $ \us -> do
               fmap (us, ) $ fmap Map.fromList $
                 forM (filter (/= us) nodeIds) $ \them ->
                   fmap (them, ) $
                     createCoupledChannels
                       @(MsgProducer (SimpleBlock p c))
                       @(MsgConsumer (SimpleBlock p c))
                       0
                       0

    varRNG <- atomically $ newTVar initRNG

    nodes <- forM (Map.toList nodeInit) $ \(us, (cfg, initSt, initChain)) -> do
      varRes <- atomically $ newTVar Nothing
      varSt  <- atomically $ newTVar initSt
      varL   <- atomically $ newTVar initLedger

      -- TODO: New chain only to fake rollback
      let respondToUpdate :: Chain (SimpleBlock p c)
                          -> [ChainUpdate (SimpleBlock p c)]
                          -> Tr m ()
          respondToUpdate newChain (RollBack _:_) =
              modifyTVar' varL $ \_st ->
                case runExcept (chainExtLedgerState cfg newChain initLedger) of
                  Left err  -> error (show err)
                  Right st' -> st'
          respondToUpdate _ upd = do
              let newBlock :: ChainUpdate (SimpleBlock p c) -> SimpleBlock p c
                  newBlock (RollBack _) = error "newBlock: unexpected rollback"
                  newBlock (AddBlock b) = b
              modifyTVar' varL $ \st ->
                case runExcept (foldExtLedgerState cfg (map newBlock upd) st) of
                  Left err  -> error (show err)
                  Right st' -> st'

      node <- nodeKernel us cfg initLedger initChain respondToUpdate
      forM_ (filter (/= us) nodeIds) $ \them -> do
        registerDownstream node them $ fst (chans Map.! us Map.! them)
        registerUpstream   node them $ snd (chans Map.! them Map.! us)

      let runProtocol :: MonadPseudoRandomT gen (NodeStateT p (Tr m)) a
                      -> Tr m a
          runProtocol = simMonadPseudoRandomT varRNG
                      $ simOuroborosStateT varSt
                      $ id

      forM_ [1 .. numSlots] $ \slotId ->
        fork $ do
          threadDelay (slotDuration (Proxy @m) * fromIntegral slotId)
          let slot = Slot (fromIntegral slotId)

          publishBlock node slot $ \prevPoint prevNo -> do
            let prevHash = castHash (pointHash prevPoint)
                curNo    = succ prevNo
                txs      = fromMaybe mempty $ Map.lookup slot txMap

            -- NOTE: Ledger state is updated in 'respondToUpdate'
            --
            -- Since the leader proof is (partially) based on the ledger state,
            -- it is important that we get the ledger state and check if we
            -- are a leader in a single transaction.

            ExtLedgerState{..} <- readTVar varL
            mIsLeader          <- runProtocol $
                                     checkIsLeader
                                       cfg
                                       slot
                                       (protocolLedgerView cfg ledgerState)
                                       ouroborosChainState
            case mIsLeader of
              Nothing    -> return Nothing
              Just proof -> runProtocol $ Just <$>
                              forgeBlock cfg slot curNo prevHash txs proof

      fork $ do
        threadDelay (slotDuration (Proxy @m) * fromIntegral (numSlots + 10))
        atomically $ do
          chain <- getCurrentChain node
          writeTVar varRes $ Just (us, chain)

      return varRes

    atomically $ Map.fromList <$> collectAllJust nodes
  where
    nodeIds :: [NodeId]
    nodeIds = Map.keys nodeInit

    numNodes :: Int
    numNodes = Map.size nodeInit


slotDuration :: MonadTimer m => proxy m -> Duration (Time m)
slotDuration _ = 100000

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

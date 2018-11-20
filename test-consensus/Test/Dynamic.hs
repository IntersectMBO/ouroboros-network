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

module Test.Dynamic (
    tests
  ) where

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.ST.Lazy
import           Crypto.Random (DRG)
import           Data.Foldable (foldlM)
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
import           Ouroboros.Network.ChainProducerState
import           Ouroboros.Network.MonadClass
import           Ouroboros.Network.Node
import           Ouroboros.Network.Protocol

import           Ouroboros.Consensus.Crypto.DSIGN.Mock
import           Ouroboros.Consensus.Crypto.Hash.Class (hash)
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Mock
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.BFT
import           Ouroboros.Consensus.Protocol.ExtNodeConfig
import           Ouroboros.Consensus.Protocol.Test
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
                   .&&. allEqual (Map.elems final)
        _otherwise   -> property False

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
               fmap (us, ) $ fmap Map.fromList $ forM nodeIds $ \them ->
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

      let ourOwnConsumer :: Chan m (MsgConsumer (SimpleBlock p c))
                                   (MsgProducer (SimpleBlock p c))
          ourOwnConsumer =
              let loopbackRecv = snd (chans Map.! us Map.! us)
              in Chan {
                     sendMsg = sendMsg loopbackRecv
                   , recvMsg = do
                       msgToMyself <- recvMsg loopbackRecv
                       case msgToMyself of
                           MsgRollForward b -> do
                               atomically $
                                 modifyTVar' varL $ \st ->
                                   case runExcept (applyExtLedgerState cfg b st) of
                                     Left err  -> error (show err)
                                     Right st' -> st'
                           _ -> return ()
                       return msgToMyself
                  }

      varCPS <- relayNode us initChain $ NodeChannels {
          consumerChans =
              map (\them -> snd (chans Map.! them Map.! us)) (filter (/= us) nodeIds)
              <> [ourOwnConsumer]
        , producerChans =
              map (\them -> fst (chans Map.! us Map.! them)) nodeIds
        }

      let runProtocol :: MonadPseudoRandomT gen (NodeStateT p (Tr m)) a
                      -> Tr m a
          runProtocol = simMonadPseudoRandomT varRNG
                      $ simOuroborosStateT varSt
                      $ id

      forM_ [1 .. numSlots] $ \slotId ->
        fork $ do
          threadDelay (slotDuration (Proxy @m) * fromIntegral slotId)
          let slot = Slot (fromIntegral slotId)

          -- TODO: We *do not* update the ledger state here. That's done in
          -- our \"loopback\" consumer.
          ExtLedgerState{..} <- atomically $ readTVar varL

          mIsLeader <- atomically $ runProtocol $
                         checkIsLeader
                           cfg
                           slot
                           (protocolLedgerView cfg ledgerState)
                           ouroborosChainState
          case mIsLeader of
            Nothing    -> return ()
            Just proof -> atomically $ do
              cps    <- readTVar varCPS
              let chain    = chainState cps
                  prevHash = castHash (headHash chain)
                  prevNo   = headBlockNo chain
                  curNo    = succ prevNo
                  txs      = fromMaybe mempty $ Map.lookup slot txMap
              block <- runProtocol $ forgeBlock cfg slot curNo prevHash txs proof
              writeTVar varCPS cps{ chainState = chain :> block }

      fork $ do
        threadDelay (slotDuration (Proxy @m) * fromIntegral (numSlots + 10))
        atomically $ do
          cps <- readTVar varCPS
          writeTVar varRes $ Just (us, chainState cps)

      return varRes

    atomically $ Map.fromList <$> collectAllJust nodes
  where
    nodeIds :: [NodeId]
    nodeIds = Map.keys nodeInit

    numNodes :: Int
    numNodes = Map.size nodeInit


slotDuration :: MonadTimer m => proxy m -> Duration (Time m)
slotDuration _ = 1000000

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

allEqual :: Eq a => [a] -> Bool
allEqual []       = True
allEqual [_]      = True
allEqual (x:y:xs) = x == y && allEqual (y:xs)

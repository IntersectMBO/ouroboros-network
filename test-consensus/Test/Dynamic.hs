{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS -fno-warn-unused-binds #-}

module Test.Dynamic (
    tests
  ) where

import           Control.Monad
import           Control.Monad.ST.Lazy
import           Crypto.Random (DRG)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Proxy
import           Test.QuickCheck

import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Ouroboros.Network.Block
import           Ouroboros.Network.Chain
import           Ouroboros.Network.ChainProducerState
import           Ouroboros.Network.MonadClass
import           Ouroboros.Network.Node
import           Ouroboros.Network.Protocol
import           Ouroboros.Network.Serialise

import           Ouroboros.Consensus.Crypto.DSIGN.Mock
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.BFT
import           Ouroboros.Consensus.Protocol.Test
import           Ouroboros.Consensus.Test.MockLedger
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
                               , MonadTimer m
                               , MonadRunProbe m n
                               , MonadSay m
                               , MonadTimer m
                               , Show (Time m)
                               )
                            => Seed -> n Property
test_simple_bft_convergence seed =
    fmap isValid $ withProbe $ go
  where
    numNodes, numSlots :: Int
    numNodes = 3
    numSlots = 10

    go :: Probe m (Map NodeId (Chain BlockUnderTest)) -> m ()
    go p = do
      finalChains <- broadcastNetwork
                       numSlots
                       nodeInit
                       (LedgerState (fromIntegral numNodes))
                       (seedToChaCha seed)
      probeOutput p finalChains

    nodeInit :: Map NodeId (OuroborosState ProtocolUnderTest, Chain BlockUnderTest)
    nodeInit = Map.fromList $ [ (CoreId i, (mkState i, Genesis))
                              | i <- [0 .. numNodes - 1]
                              ]
      where
        mkState :: Int -> OuroborosState ProtocolUnderTest
        mkState nodeId = TestState BftState {
              bftNodeId  = CoreId nodeId
            , bftSignKey = SignKeyMockDSIGN nodeId
            }

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

type ProtocolUnderTest = TestProtocol (Bft BftMockCrypto)
type BlockUnderTest    = SimpleBlock ProtocolUnderTest SimpleBlockStandardCrypto

-- | TODO: Right now this just records the number of nodes, but we should also
-- record the stake
data LedgerState = LedgerState {
      ledgerNumNodes :: Word
    }

instance BftLedgerView LedgerState where
  bftNumNodes = ledgerNumNodes

instance TestProtocolLedgerView LedgerState where
  stakeForNode _ _ = 1234

{-------------------------------------------------------------------------------
  Infrastructure
-------------------------------------------------------------------------------}

-- | Setup fully-connected topology, where every node is both a producer
-- and a consumer
--
-- We run for the specified number of blocks, then return the final state of
-- each node.
broadcastNetwork :: forall m p c l gen.
                    ( MonadSTM   m
                    , MonadTimer m
                    , MonadSay   m
                    , Show      (OuroborosPayload p (SimplePreHeader p c))
                    , Serialise (OuroborosPayload p (SimplePreHeader p c))
                    , SimpleBlockCrypto c
                    , RunOuroboros p l
                    , DRG gen
                    )
                 => Int
                 -- ^ Number of slots to run for
                 -> Map NodeId (OuroborosState p, Chain (SimpleBlock p c))
                 -- ^ Node initial state and initial chain
                 -> l
                 -- ^ Initial ledger state
                 -> gen
                 -- ^ Initial random number state
                 -> m (Map NodeId (Chain (SimpleBlock p c)))
broadcastNetwork numSlots nodeInit initLedger initRNG = do
    chans <- fmap Map.fromList $ forM nodeIds $ \us -> do
               fmap (us, ) $ fmap Map.fromList $ forM (filter (/= us) nodeIds) $ \them ->
                 fmap (them, ) $
                   createCoupledChannels
                     @(MsgProducer (SimpleBlock p c))
                     @(MsgConsumer (SimpleBlock p c))
                     0
                     0

    varRNG <- atomically $ newTVar initRNG

    nodes <- forM (Map.toList nodeInit) $ \(us, (initSt, initChain)) -> do
      varRes <- atomically $ newTVar Nothing
      varSt  <- atomically $ newTVar initSt
      varCPS <- relayNode us initChain $ NodeChannels {
          consumerChans = map (\them -> snd (chans Map.! them Map.! us)) (filter (/= us) nodeIds)
        , producerChans = map (\them -> fst (chans Map.! us Map.! them)) (filter (/= us) nodeIds)
        }

      let runProtocol :: MonadPseudoRandomT gen (OuroborosStateT p (Tr m)) a
                      -> Tr m a
          runProtocol = simMonadPseudoRandomT varRNG
                      $ simOuroborosStateT varSt
                      $ id

      forM_ [1 .. numSlots] $ \slotId ->
        timer (slotDuration (Proxy @m) * fromIntegral slotId) $ do
          let slot = Slot (fromIntegral slotId)

          -- TODO: We should have a proper ledger here (Alfredo)
          mIsLeader <- atomically $ runProtocol $ checkIsLeader slot initLedger
          case mIsLeader of
            Nothing    -> return ()
            Just proof -> atomically $ do
              cps    <- readTVar varCPS
              let chain    = chainState cps
                  prevHash = castHash (headHash chain)
                  prevNo   = headBlockNo chain
                  curNo    = succ prevNo
              block <- runProtocol $ forgeBlock slot curNo prevHash proof
              writeTVar varCPS cps{ chainState = chain :> block }

      timer (slotDuration (Proxy @m) * fromIntegral (numSlots + 10)) $
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

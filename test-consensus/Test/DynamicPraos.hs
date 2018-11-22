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

module Test.DynamicPraos (
    tests
  ) where

import           Control.Monad.ST.Lazy
import           Data.Foldable (foldlM)
import qualified Data.IntMap.Strict as IntMap
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Test.QuickCheck

import           Test.Tasty
import           Test.Tasty.ExpectedFailure
import           Test.Tasty.QuickCheck

import           Ouroboros.Network.Block
import           Ouroboros.Network.Chain
import           Ouroboros.Network.MonadClass
import           Ouroboros.Network.Node

import           Ouroboros.Consensus.Crypto.Hash.Class (hash)
import           Ouroboros.Consensus.Crypto.KES (VerKeyKES)
import           Ouroboros.Consensus.Crypto.KES.Mock
import           Ouroboros.Consensus.Crypto.VRF.Mock
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Mock
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.ExtNodeConfig
import           Ouroboros.Consensus.Protocol.Praos
import           Ouroboros.Consensus.Protocol.Test
import           Ouroboros.Consensus.Util.Random

import           Test.DynamicBFT (TestConfig (..), allEqual, broadcastNetwork,
                     nodeStake)

tests :: TestTree
tests = expectFail $ testGroup "Dynamic chain generation" [
      testProperty "simple Praos convergence" prop_simple_praos_convergence
    ]

prop_simple_praos_convergence :: Seed -> Property
prop_simple_praos_convergence seed = runST $ test_simple_praos_convergence seed

-- Run Praos on the broadcast network, and check that all nodes converge to the
-- same final chain
test_simple_praos_convergence :: forall m n.
                                 ( MonadSTM m
                                 , MonadRunProbe m n
                                 , MonadSay m
                                 , MonadTimer m
                                 )
                              => Seed -> n Property
test_simple_praos_convergence seed = do
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

    verKeys :: Map NodeId (VerKeyKES (PraosKES PraosMockCrypto))
    verKeys = Map.fromList [ (CoreId i, VerKeyMockKES i)
                           | i <- [0 .. numNodes - 1]
                           ]

    initLedgerState :: ExtLedgerState BlockUnderTest
    initLedgerState = ExtLedgerState {
          ledgerState         = SimpleLedgerState (utxo genesisTx)
        , ouroborosChainState = []
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
                  testNodeConfigP = PraosNodeConfig {
                      praosNodeId        = CoreId i
                    , praosSignKeyVRF    = SignKeyMockVRF i
                    , praosSlotsPerEpoch = 5
                    , praosInitialEta    = 0
                    , praosInitialStake  = initialStake
                    , praosLeaderF       = 0.5
                    }
                , testNodeConfigId = CoreId i
                }
            , encNodeConfigExt = testConfig
            }

        mkState :: Int -> NodeState ProtocolUnderTest
        mkState nid = SignKeyMockKES (VerKeyMockKES nid, 0, 1 + fromIntegral numSlots)

    isValid :: [(Time m, Map NodeId (Chain BlockUnderTest))] -> Property
    isValid trace = counterexample (show trace) $
      case trace of
        [(_, final)] -> Map.keys final == Map.keys nodeInit
                   .&&. allEqual (Map.elems final)
        _otherwise   -> property False

    initialStake :: StakeDist
    initialStake =
        let q = recip $ fromIntegral numNodes
        in  IntMap.fromList [(i, q) | i <- [0 .. numNodes - 1]]

{-------------------------------------------------------------------------------
  Test blocks
-------------------------------------------------------------------------------}

type ProtocolUnderTest = ExtNodeConfig TestConfig (TestProtocol (Praos PraosMockCrypto))
type BlockUnderTest    = SimpleBlock ProtocolUnderTest SimpleBlockStandardCrypto
type HeaderUnderTest   = SimpleHeader ProtocolUnderTest SimpleBlockStandardCrypto

instance HasPayload (Praos PraosMockCrypto) BlockUnderTest where
  blockPayload _ = testPayloadP
                 . encPayloadP
                 . headerOuroboros
                 . simpleHeader

instance ProtocolLedgerView BlockUnderTest where
    protocolLedgerView (EncNodeConfig _ cfg) (SimpleLedgerState u) =
        ( relativeStakes $ totalStakes cfg u
        , nodeStake cfg u
        )

totalStakes :: TestConfig -> Utxo -> Map (Maybe Int) Int
totalStakes (TestConfig addrDist) = foldl f Map.empty
  where
    f :: Map (Maybe Int) Int -> TxOut -> Map (Maybe Int) Int
    f m (a, stake) = case Map.lookup a addrDist of
        Just (CoreId nid) -> Map.insertWith (+) (Just nid) stake m
        _                 -> Map.insertWith (+) Nothing stake m

relativeStakes :: Map (Maybe Int) Int -> StakeDist
relativeStakes m =
    let totalStake    = fromIntegral $ sum $ Map.elems m
    in  IntMap.fromList [ (nid, fromIntegral stake / totalStake)
                        | (Just nid, stake) <- Map.toList m
                        ]

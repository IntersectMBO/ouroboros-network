{-# LANGUAGE NamedFieldPuns #-}

module Test.ThreadNet.PBFT (
    tests
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Test.QuickCheck

import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Ouroboros.Network.Block (SlotNo (..), blockSlot)
import           Ouroboros.Network.MockChain.Chain (foldChain)

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Extended (ExtValidationError (..))
import           Ouroboros.Consensus.Mock.Ledger.Block
import           Ouroboros.Consensus.Mock.Ledger.Block.PBFT
import           Ouroboros.Consensus.Mock.Node ()
import           Ouroboros.Consensus.Mock.Node.PBFT (protocolInfoMockPBFT)
import           Ouroboros.Consensus.Node.ProtocolInfo (NumCoreNodes (..))
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.PBFT
import           Ouroboros.Consensus.Util.Condense (condense)

import           Test.ThreadNet.General
import           Test.ThreadNet.Network
import qualified Test.ThreadNet.Ref.PBFT as Ref
import           Test.ThreadNet.TxGen.Mock ()
import           Test.ThreadNet.Util
import           Test.ThreadNet.Util.HasCreator.Mock ()

import           Test.Util.Orphans.Arbitrary ()

tests :: TestTree
tests = testGroup "PBFT" [
      testProperty "simple convergence" $ \tc ->
        forAll (SecurityParam <$> elements [1 .. 10]) $ \k ->
        prop_simple_pbft_convergence k tc
    ]

prop_simple_pbft_convergence :: SecurityParam
                             -> TestConfig
                             -> Property
prop_simple_pbft_convergence
  k testConfig@TestConfig{numCoreNodes, numSlots, nodeJoinPlan} =
    tabulate "Ref.PBFT result" [Ref.resultConstrName refResult] $
    prop_asSimulated .&&.
    prop_general
        countSimpleGenTxs
        k
        testConfig
        (Just $ roundRobinLeaderSchedule numCoreNodes numSlots)
        -- do not use the "Test.ThreadNet.Util.Expectations" module; it doesn't
        -- consider the PBFT threshold
        (Just $ NumBlocks $ case refResult of
           Ref.Forked{} -> 1
           _            -> 0)
        (expectedBlockRejection numCoreNodes)
        testOutput
  where
    NumCoreNodes nn = numCoreNodes

    sigThd = (1.0 / fromIntegral nn) + 0.1
    params = PBftParams k numCoreNodes sigThd

    testOutput =
        runTestNetwork testConfig TestConfigBlock
            { forgeEbbEnv = Nothing
            , nodeInfo    = protocolInfoMockPBFT
                              params
                              (singletonSlotLengths pbftSlotLength)
            , rekeying    = Nothing
            }

    refResult :: Ref.Result
    refResult = Ref.simulate params nodeJoinPlan numSlots

    prop_asSimulated :: Property
    prop_asSimulated =
        counterexample ("Unexpected Nominal slots:") $
        conjoin $
        [ counterexample ("In final chain of " <> condense nid) $
          counterexample ("actual:   " <> condense actualSlots) $
          case refResult of
            Ref.Forked _ m        ->
              let expectedSlotss =
                    case Map.lookup cid m of
                      Nothing -> error "node missing from Ref.Forked"
                      Just ss -> map (:[]) $ Set.toList ss
              in
              counterexample
                ("expected: one of " <> condense expectedSlotss) $
              actualSlots `elem` expectedSlotss
            Ref.Nondeterministic  -> property True   -- TODO improve?
            Ref.Outcomes outcomes ->
              let expectedSlots =
                    [ s | (Ref.Nominal, s) <- zip outcomes [0..] ]
              in
              counterexample ("expected: " <> condense expectedSlots) $
              actualSlots == expectedSlots
        | (nid@(CoreId cid), no) <- Map.toList testOutputNodes
        , let actualSlots = actualSlotsOf no
        ]
      where
        TestOutput{testOutputNodes} = testOutput

        actualSlotsOf NodeOutput{nodeOutputFinalChain} =
            foldChain snoc id nodeOutputFinalChain [] :: [SlotNo]
          where
            snoc acc blk = acc . (blockSlot blk :)

pbftSlotLength :: SlotLength
pbftSlotLength = slotLengthFromSec 20

type Blk = SimpleBlock SimpleMockCrypto
             (SimplePBftExt SimpleMockCrypto PBftMockCrypto)

expectedBlockRejection :: NumCoreNodes -> BlockRejection Blk -> Bool
expectedBlockRejection (NumCoreNodes nn) BlockRejection
  { brBlockSlot = SlotNo s
  , brReason    = err
  , brRejector  = CoreId (CoreNodeId i)
  }
  | ownBlock               = case err of
    ExtValidationErrorHeader
      (HeaderProtocolError PBftExceededSignThreshold{}) -> True
    _                                                   -> False
  where
    -- Because of round-robin and the fact that the id divides slot, we know
    -- the node lead but rejected its own block. This is the only case we
    -- expect. (Rejecting its own block also prevents the node from propagating
    -- that block.)
    ownBlock = fromIntegral i == mod s (fromIntegral nn)
expectedBlockRejection _ _ = False

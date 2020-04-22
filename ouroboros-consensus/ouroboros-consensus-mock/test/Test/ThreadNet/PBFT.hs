{-# LANGUAGE NamedFieldPuns #-}

module Test.ThreadNet.PBFT (
    tests
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Test.QuickCheck

import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Cardano.Slotting.Slot

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
import           Ouroboros.Consensus.Util.Random (Seed (..))

import           Test.ThreadNet.General
import           Test.ThreadNet.Network
import qualified Test.ThreadNet.Ref.PBFT as Ref
import           Test.ThreadNet.TxGen.Mock ()
import           Test.ThreadNet.Util
import           Test.ThreadNet.Util.HasCreator.Mock ()
import           Test.ThreadNet.Util.NodeJoinPlan
import           Test.ThreadNet.Util.NodeRestarts
import           Test.ThreadNet.Util.NodeTopology
import           Test.ThreadNet.Util.SimpleBlock

import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Time

tests :: TestTree
tests = testGroup "PBFT" $
    [ testProperty "Issue 1505: removeTxs must not use fast path" $
      -- See (the comments of) Issue 1505.
      let ncn5 = NumCoreNodes 5 in
      prop_simple_pbft_convergence (SecurityParam 5) TestConfig
        { numCoreNodes = ncn5
        , numSlots     = NumSlots 100
        , nodeJoinPlan = NodeJoinPlan $ Map.fromList
          [ (CoreNodeId 0, SlotNo 0)   -- 0 only leads this slot
          , (CoreNodeId 1, SlotNo 6)   -- 1 only leads this slot
          , (CoreNodeId 2, SlotNo 22)  -- 2 only leads this slot
          , (CoreNodeId 3, SlotNo 24)
          , (CoreNodeId 4, SlotNo 99)  -- irrelevant, beyond affecting pbftThreshold via numCoreNodes
          ]
        , nodeRestarts = noRestarts
        , nodeTopology = meshNodeTopology ncn5
        , slotLength   = slotLengthFromSec 1
        , initSeed     = Seed (9550173506264790139,4734409083700350196,9697926137031612922,16476814117921936461,9569412668768792610)
        }
    , testProperty "simple convergence" $ \tc ->
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
    prop_general PropGeneralArgs
      { pgaBlockProperty          = prop_validSimpleBlock
      , pgaCountTxs               = countSimpleGenTxs
      , pgaExpectedBlockRejection = expectedBlockRejection numCoreNodes
      , pgaFirstBlockNo           = 0
      , pgaFixedMaxForkLength     =
          Just $ NumBlocks $ case refResult of
            Ref.Forked{} -> 1
            _            -> 0
      , pgaFixedSchedule          =
          Just $ roundRobinLeaderSchedule numCoreNodes numSlots
      , pgaSecurityParam          = k
      , pgaTestConfig             = testConfig
      }
      testOutput
  where
    NumCoreNodes nn = numCoreNodes

    sigThd = (1.0 / fromIntegral nn) + 0.1
    params = PBftParams k numCoreNodes sigThd

    testOutput =
        runTestNetwork testConfig epochSize TestConfigBlock
            { forgeEbbEnv = Nothing
            , nodeInfo    = plainTestNodeInitialization .
                            protocolInfoMockPBFT
                              params
                              (defaultSimpleBlockConfig k pbftSlotLength)
            , rekeying    = Nothing
            , txGenExtra  = ()
            }

    -- The mock ledger doesn't really care, and neither does BFT.
    -- We stick with the common @k * 10@ size for now.
    epochSize :: EpochSize
    epochSize = EpochSize (maxRollbacks k * 10)

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
    ownBlock = i == mod s nn
expectedBlockRejection _ _ = False

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Test.ThreadNet.Byron (
    tests
    -- * To support the DualByron tests
  , TestSetup (..)
  , byronPBftParams
  , expectedCannotForge
  , genTestSetup
  , noEBBs
  ) where

import           Control.Monad (join)
import qualified Data.ByteString as BS
import           Data.Coerce (coerce)
import           Data.Functor ((<&>))
import qualified Data.Map.Strict as Map
import           Data.Maybe (listToMaybe, mapMaybe)
import qualified Data.Set as Set
import           Data.Word (Word64)

import           Numeric.Search.Range (searchFromTo)
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Cardano.Crypto.Seed (mkSeedFromBytes)

import           Ouroboros.Network.MockChain.Chain (Chain)
import qualified Ouroboros.Network.MockChain.Chain as Chain

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Protocol.PBFT
import qualified Ouroboros.Consensus.Protocol.PBFT.Crypto as Crypto
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.Util.Condense (condense)

import qualified Cardano.Binary
import qualified Cardano.Chain.Block as Block
import qualified Cardano.Chain.Common as Common
import qualified Cardano.Chain.Delegation as Delegation
import qualified Cardano.Chain.Genesis as Genesis
import           Cardano.Chain.ProtocolConstants (kEpochSlots)
import           Cardano.Chain.Slotting (EpochNumber (..), unEpochSlots)
import qualified Cardano.Crypto as Crypto
import qualified Cardano.Crypto.DSIGN as Crypto

import qualified Ouroboros.Consensus.Byron.Crypto.DSIGN as Crypto
import           Ouroboros.Consensus.Byron.Ledger (ByronBlock,
                     ByronNodeToNodeVersion (..))
import qualified Ouroboros.Consensus.Byron.Ledger as Byron
import           Ouroboros.Consensus.Byron.Ledger.Conversions
import           Ouroboros.Consensus.Byron.Node
import           Ouroboros.Consensus.Byron.Protocol

import           Test.ThreadNet.General
import           Test.ThreadNet.Network (NodeOutput (..),
                     TestNodeInitialization (..))
import qualified Test.ThreadNet.Ref.PBFT as Ref
import           Test.ThreadNet.Rekeying
import           Test.ThreadNet.TxGen.Byron ()
import           Test.ThreadNet.Util
import           Test.ThreadNet.Util.NodeJoinPlan
import           Test.ThreadNet.Util.NodeRestarts
import           Test.ThreadNet.Util.NodeToNodeVersion
import           Test.ThreadNet.Util.NodeTopology
import           Test.ThreadNet.Util.Seed

import           Test.Util.HardFork.Future (singleEraFuture)
import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Slots (NumSlots (..))
import qualified Test.Util.Stream as Stream

import           Test.ThreadNet.Infra.Byron

data TestSetup = TestSetup
  { setupEBBs         :: ProduceEBBs
  , setupK            :: SecurityParam
  , setupTestConfig   :: TestConfig
  , setupNodeJoinPlan :: NodeJoinPlan
  , setupNodeRestarts :: NodeRestarts
  , setupSlotLength   :: SlotLength
  , setupVersion      :: (NodeToNodeVersion, BlockNodeToNodeVersion ByronBlock)
  }
  deriving (Show)

instance Arbitrary TestSetup where
  arbitrary = do
     -- TODO Issue #1566 will bring this to k>=0
      k <- SecurityParam <$> choose (1, 10)

      join $ genTestSetup k <$> arbitrary <*> arbitrary <*> arbitrary

  -- TODO shrink

-- | An entrypoint used by "Test.ThreadNet.DualByron"
--
-- See the @'Arbitrary' 'Test.ThreadNet.DualByron.SetupDualByron'@ instance.
genTestSetup :: SecurityParam -> NumCoreNodes -> NumSlots -> SlotLength -> Gen TestSetup
genTestSetup k numCoreNodes numSlots setupSlotLength = do
    setupEBBs    <- arbitrary
    initSeed     <- arbitrary
    nodeTopology <- genNodeTopology numCoreNodes

    let testConfig = TestConfig
          { initSeed
          , nodeTopology
          , numCoreNodes
          , numSlots
          }
    let params = byronPBftParams k numCoreNodes

    nodeJoinPlan <- genByronNodeJoinPlan params numSlots
    nodeRestarts <- genNodeRestarts nodeJoinPlan numSlots >>=
                    genNodeRekeys params nodeJoinPlan nodeTopology numSlots

    setupVersion <- genVersion (Proxy @ByronBlock)


    pure $ TestSetup
      setupEBBs
      k
      testConfig
      nodeJoinPlan
      nodeRestarts
      setupSlotLength
      setupVersion

tests :: TestTree
tests = testGroup "Byron" $
    [ testProperty "trivial join plan is considered deterministic"
        $ \TestSetup{setupK = k, setupTestConfig = TestConfig{numCoreNodes}} ->
          prop_deterministicPlan $ byronPBftParams k numCoreNodes
    , adjustOption (\(QuickCheckTests n) -> QuickCheckTests (1 `max` (div n 10))) $
      -- as of merging PR #773, this test case fails without the commit that
      -- introduces the InvalidRollForward exception
      --
      -- See a related discussion at
      -- https://github.com/input-output-hk/ouroboros-network/pull/773#issuecomment-522192097
      testProperty "addressed by InvalidRollForward exception (PR #773)" $
          once $
          let ncn = NumCoreNodes 3 in
          prop_simple_real_pbft_convergence TestSetup
            { setupEBBs       = ProduceEBBs
            , setupK          = SecurityParam 10
            , setupTestConfig = TestConfig
              { initSeed     = Seed 0
              , nodeTopology = meshNodeTopology ncn
              , numCoreNodes = ncn
              , numSlots     = NumSlots 24
              }
            , setupNodeJoinPlan = NodeJoinPlan $ Map.fromList [(CoreNodeId 0,SlotNo 0), (CoreNodeId 1,SlotNo 20), (CoreNodeId 2,SlotNo 22)]
            , setupNodeRestarts = noRestarts
            , setupSlotLength   = defaultSlotLength
            , setupVersion      = (NodeToNodeV_1, ByronNodeToNodeVersion1)
            }
    , testProperty "rewind to EBB supported as of Issue #1312, #1" $
          once $
          let ncn = NumCoreNodes 2 in
          -- When node 1 joins in slot 1, it leads with an empty chain and so
          -- forges the 0-EBB again. This causes it to report slot 0 as the
          -- found intersection point to node 0, which causes node 0 to
          -- \"rewind\" to slot 0 (even though it's already there). That rewind
          -- fails if EBBs don't affect the PBFT chain state, since its chain
          -- state is empty.
          prop_simple_real_pbft_convergence TestSetup
            { setupEBBs       = ProduceEBBs
            , setupK          = SecurityParam 10
            , setupTestConfig = TestConfig
              { numCoreNodes = ncn
              , numSlots     = NumSlots 2
              , nodeTopology = meshNodeTopology ncn
              , initSeed     = Seed 0
              }
            , setupNodeJoinPlan = NodeJoinPlan (Map.fromList [(CoreNodeId 0,SlotNo 0),(CoreNodeId 1,SlotNo 1)])
            , setupNodeRestarts = noRestarts
            , setupSlotLength   = defaultSlotLength
            , setupVersion      = (NodeToNodeV_1, ByronNodeToNodeVersion1)
            }
    , testProperty "rewind to EBB supported as of Issue #1312, #2" $
          once $
          let ncn = NumCoreNodes 2 in
          -- Same as above, except node 0 gets to forge an actual block before
          -- node 1 tells it to rewind to the EBB.
          prop_simple_real_pbft_convergence TestSetup
            { setupEBBs       = ProduceEBBs
            , setupK          = SecurityParam 10
            , setupTestConfig = TestConfig
              { numCoreNodes = ncn
              , numSlots     = NumSlots 4
              , nodeTopology = meshNodeTopology ncn
              , initSeed     = Seed 0
              }
            , setupNodeJoinPlan = NodeJoinPlan (Map.fromList [(CoreNodeId 0,SlotNo {unSlotNo = 0}),(CoreNodeId 1,SlotNo {unSlotNo = 3})])
            , setupNodeRestarts = noRestarts
            , setupSlotLength   = defaultSlotLength
            , setupVersion      = (NodeToNodeV_1, ByronNodeToNodeVersion1)
            }
    , testProperty "one testOutputTipBlockNos update per node per slot" $
          once $
          let ncn = NumCoreNodes 2 in
          -- In this example, a node was forging a new block and then
          -- restarting. Its instrumentation thread ran before and also after
          -- the restart, which caused the 'testOutputTipBlockNos' field to
          -- contain data from the middle of the slot (after the node lead)
          -- instead of only from the onset of the slot.
          prop_simple_real_pbft_convergence TestSetup
            { setupEBBs       = ProduceEBBs
            , setupK          = SecurityParam 5
            , setupTestConfig = TestConfig
              { numCoreNodes = ncn
              , numSlots     = NumSlots 7
              , nodeTopology = meshNodeTopology ncn
              , initSeed     = Seed 0
              }
            , setupNodeJoinPlan = NodeJoinPlan (Map.fromList [(CoreNodeId 0,SlotNo {unSlotNo = 0}),(CoreNodeId 1,SlotNo {unSlotNo = 0})])
            , setupNodeRestarts = NodeRestarts (Map.fromList [(SlotNo {unSlotNo = 5},Map.fromList [(CoreNodeId 1,NodeRestart)])])
            , setupSlotLength   = defaultSlotLength
            , setupVersion      = (NodeToNodeV_1, ByronNodeToNodeVersion1)
            }
    , testProperty "BlockFetch live lock due to an EBB at the ImmutableDB tip, Issue #1435" $
          once $
          let ncn = NumCoreNodes 4 in
          -- c0's ImmutableDB is T > U > V. Note that U is an EBB and U and V
          -- are both in slot 50. When its BlockFetchServer tries to stream T
          -- and U using a ChainDB.Iterator, instead of looking in the
          -- ImmutableDB, we end up looking in the VolatileDB and incorrectly
          -- return ForkTooOld. The client keeps on requesting this block range,
          -- resulting in a live lock.
          prop_simple_real_pbft_convergence TestSetup
            { setupEBBs       = ProduceEBBs
            , setupK          = SecurityParam 5
            , setupTestConfig = TestConfig
              { numCoreNodes = ncn
              , numSlots     = NumSlots 58
              , nodeTopology = meshNodeTopology ncn
              , initSeed     = Seed 0
              }
            , setupNodeJoinPlan = NodeJoinPlan $ Map.fromList [(CoreNodeId 0,SlotNo 3),(CoreNodeId 1,SlotNo 3),(CoreNodeId 2,SlotNo 5),(CoreNodeId 3,SlotNo 57)]
            , setupNodeRestarts = noRestarts
            , setupSlotLength   = defaultSlotLength
            , setupVersion      = (NodeToNodeV_1, ByronNodeToNodeVersion1)
            }
    , testProperty "ImmutableDB is leaking file handles, #1543" $
          -- The failure was: c0 leaks one ImmutableDB file handle (for path
          -- @00000.epoch@, read only, offset at 0).
          --
          -- The test case seems somewhat fragile, since the 'slotLength' value
          -- seems to matter!
          once $
          let ncn5 = NumCoreNodes 5 in
          prop_simple_real_pbft_convergence TestSetup
            { setupEBBs       = NoEBBs
            , setupK          = SecurityParam 2
            , setupTestConfig = TestConfig
              { numCoreNodes = ncn5
              -- Still fails if I increase numSlots.
              , numSlots     = NumSlots 54
              , nodeTopology = meshNodeTopology ncn5
              , initSeed     = Seed 0
              }
            , setupNodeJoinPlan = NodeJoinPlan $ Map.fromList
              [ (CoreNodeId 0, SlotNo {unSlotNo = 0})
              , (CoreNodeId 1, SlotNo {unSlotNo = 0})
              , (CoreNodeId 2, SlotNo {unSlotNo = 0})
              , (CoreNodeId 3, SlotNo {unSlotNo = 53})
              , (CoreNodeId 4, SlotNo {unSlotNo = 53})
              ]
              -- Passes if I drop either of these restarts.
            , setupNodeRestarts = NodeRestarts $ Map.fromList
              [ (SlotNo {unSlotNo = 50},Map.fromList [(CoreNodeId 0,NodeRestart)])
              , (SlotNo {unSlotNo = 53},Map.fromList [(CoreNodeId 3,NodeRestart)])
              ]
              -- Slot length of 19s passes, and 21s also fails; I haven't seen this matter before.
            , setupSlotLength = slotLengthFromSec 20
            , setupVersion    = (NodeToNodeV_1, ByronNodeToNodeVersion1)
            }
    , -- Byron runs are slow, so do 10x less of this narrow test
      adjustOption (\(QuickCheckTests i) -> QuickCheckTests $ max 1 $ i `div` 10) $
      testProperty "re-delegation via NodeRekey" $ \seed w ->
          let ncn = NumCoreNodes 5
              k :: Num a => a
              k = 5   -- small so that multiple epochs fit into a simulation
              window :: Num a => a
              window = 20   -- just for generality
              slotsPerEpoch :: Num a => a
              slotsPerEpoch = fromIntegral $ unEpochSlots $
                              kEpochSlots $ coerce (k :: Word64)
              slotsPerRekey :: Num a => a
              slotsPerRekey = 2 * k    -- delegations take effect 2k slots later
          in
          prop_simple_real_pbft_convergence TestSetup
            { setupEBBs       = ProduceEBBs
            , setupK          = SecurityParam k
            , setupTestConfig = TestConfig
              { numCoreNodes = ncn
              , numSlots     = NumSlots $ window + slotsPerEpoch + slotsPerRekey + window
              , nodeTopology = meshNodeTopology ncn
              , initSeed     = seed
              }
            , setupNodeJoinPlan = trivialNodeJoinPlan ncn
            , setupNodeRestarts = NodeRestarts $ Map.singleton (SlotNo (slotsPerEpoch + mod w window)) (Map.singleton (CoreNodeId 0) NodeRekey)
            , setupSlotLength   = defaultSlotLength
            , setupVersion      = (NodeToNodeV_1, ByronNodeToNodeVersion1)
            }
    , testProperty "exercise a corner case of mkCurrentBlockContext" $
          -- The current chain fragment is @Empty a :> B@ and we're trying to
          -- forge B'; the oddity is that B and B' have the same slot, since
          -- the node is actually leading for the /second/ time in that slot
          -- due to the 'NodeRestart'.
          --
          -- This failed with @Exception: the first block on the Byron chain
          -- must be an EBB@.
          let k   = SecurityParam 1
              ncn = NumCoreNodes 2
          in
          prop_simple_real_pbft_convergence TestSetup
            { setupEBBs       = NoEBBs
            , setupK          = k
            , setupTestConfig = TestConfig
              { numCoreNodes = ncn
              , numSlots     = NumSlots 2
              , nodeTopology = meshNodeTopology ncn
              , initSeed     = Seed 0
              }
            , setupNodeJoinPlan = trivialNodeJoinPlan ncn
            , setupNodeRestarts = NodeRestarts $ Map.singleton (SlotNo 1) (Map.singleton (CoreNodeId 1) NodeRestart)
            , setupSlotLength   = defaultSlotLength
            , setupVersion      = (NodeToNodeV_1, ByronNodeToNodeVersion1)
            }
    , testProperty "correct EpochNumber in delegation certificate 1" $
          -- Node 3 rekeys in slot 59, which is epoch 1. But Node 3 also leads
          -- that slot, and it forged and adopted a block before restarting. So
          -- the delegation transaction ends up in a block in slot 60, which is
          -- epoch 2.
          once $
          let ncn4 = NumCoreNodes 4 in
          prop_simple_real_pbft_convergence TestSetup
            { setupEBBs       = NoEBBs
            , setupK          = SecurityParam 3
            , setupTestConfig = TestConfig
              { numCoreNodes = ncn4
              , numSlots     = NumSlots 72
              , nodeTopology = meshNodeTopology ncn4
              , initSeed     = Seed 0
              }
            , setupNodeJoinPlan = trivialNodeJoinPlan ncn4
            , setupNodeRestarts = NodeRestarts (Map.fromList [(SlotNo 59,Map.fromList [(CoreNodeId 3,NodeRekey)])])
            , setupSlotLength   = defaultSlotLength
            , setupVersion      = (NodeToNodeV_1, ByronNodeToNodeVersion1)
            }
    , testProperty "correct EpochNumber in delegation certificate 2" $
          -- Revealed the incorrectness of setting the dlg cert epoch based on
          -- the slot in which the node rekeyed. It must be based on the slot
          -- in which the next block will be successfully forged; hence adding
          -- 'rekeyOracle' fixed this.
          --
          -- Node 2 joins and rekeys in slot 58, epoch 2. It also leads slot
          -- 59. So its dlg cert tx will only be included in the block in slot
          -- 60. However, since that's epoch 3, the tx is discarded as invalid
          -- before the block is forged.
          let ncn3 = NumCoreNodes 3 in
          prop_simple_real_pbft_convergence TestSetup
            { setupEBBs       = ProduceEBBs
            , setupK          = SecurityParam 2
            , setupTestConfig = TestConfig
              { numCoreNodes = ncn3
              , numSlots     = NumSlots 84
              , nodeTopology = meshNodeTopology ncn3
              , initSeed     = Seed 0
              }
            , setupNodeJoinPlan = NodeJoinPlan (Map.fromList [(CoreNodeId 0,SlotNo {unSlotNo = 1}),(CoreNodeId 1,SlotNo {unSlotNo = 1}),(CoreNodeId 2,SlotNo {unSlotNo = 58})])
            , setupNodeRestarts = NodeRestarts (Map.fromList [(SlotNo {unSlotNo = 58},Map.fromList [(CoreNodeId 2,NodeRekey)])])
            , setupSlotLength   = defaultSlotLength
            , setupVersion      = (NodeToNodeV_1, ByronNodeToNodeVersion1)
            }
    , testProperty "repeatedly add the the dlg cert tx" $
          -- Revealed the incorrectness of only adding dlg cert tx to the
          -- mempool once (since it may be essentially immediately discarded);
          -- hence adding it every time the ledger tip changes fixed this.
          --
          -- The failure was: PBftNotGenesisDelegate in Slot 95. It disappeared
          -- with the mesh topology, which usually means subtle timings are
          -- involved, unfortunately.
          --
          -- c2 rekeys in s83. c0 leads s84. But c2's dlg cert tx never reached
          -- c0. It turns out that c2 told c0 it exists but then discarded it
          -- before c0 actually requested it.
          --
          -- Excerpt of c2 trace events during s83:
          --
          -- > TraceLabelPeer (CoreId (CoreNodeId 0)) Send MsgRequestNext
          -- > TraceLabelPeer (CoreId (CoreNodeId 0)) Send MsgReplyTxIds (BlockingReply ((dlgid: certificateid: fb50aa22,202) :| []))
          -- > SwitchedToChain
          -- >         { _prevChain = AnchoredFragment {anchorPoint = BlockPoint (SlotNo 25) 26851f52, unanchorFragment = ChainFragment (SFT (fromList
          -- >            [ByronHeader {..., byronHeaderSlotNo = SlotNo {unSlotNo = 27}, byronHeaderHash = ByronHash {unByronHash = AbstractHash d50e0d2c}}
          -- >            ,ByronHeader {..., byronHeaderSlotNo = SlotNo {unSlotNo = 28}, byronHeaderHash = ByronHash {unByronHash = AbstractHash 1523de50}}
          -- >            ,ByronHeader {..., byronHeaderSlotNo = SlotNo {unSlotNo = 30}, byronHeaderHash = ByronHash {unByronHash = AbstractHash 77cb5dda}}
          -- >            ,ByronHeader {..., byronHeaderSlotNo = SlotNo {unSlotNo = 31}, byronHeaderHash = ByronHash {unByronHash = AbstractHash 7efd3ec2}}
          -- >            ,ByronHeader {..., byronHeaderSlotNo = SlotNo {unSlotNo = 33}, byronHeaderHash = ByronHash {unByronHash = AbstractHash 8903fa61}}
          -- > {-an EBB-} ,ByronHeader {..., byronHeaderSlotNo = SlotNo {unSlotNo = 40}, byronHeaderHash = ByronHash {unByronHash = AbstractHash 43f8067e}}
          -- >            ]))}
          -- >         , _newChain = AnchoredFragment {anchorPoint = BlockPoint (SlotNo 27) d50e0d2c, unanchorFragment = ChainFragment (SFT (fromList
          -- >            [ByronHeader {..., byronHeaderSlotNo = SlotNo {unSlotNo = 28}, byronHeaderHash = 1523de50}
          -- >            ,ByronHeader {..., byronHeaderSlotNo = SlotNo {unSlotNo = 30}, byronHeaderHash = 77cb5dda}
          -- >            ,ByronHeader {..., byronHeaderSlotNo = SlotNo {unSlotNo = 31}, byronHeaderHash = 7efd3ec2}
          -- >            ,ByronHeader {..., byronHeaderSlotNo = SlotNo {unSlotNo = 33}, byronHeaderHash = 8903fa61}
          -- >            ,ByronHeader {..., byronHeaderSlotNo = SlotNo {unSlotNo = 34}, byronHeaderHash = afa797b4}
          -- >            ]))}}
          --
          -- That SwitchedToChain rolled back the slot 40 EBB (epoch 1) and
          -- picked up a proper block in slot 34 (epoch 0) instead.
          --
          -- > TraceMempoolRemoveTxs (SyncWithLedger (At (SlotNo {unSlotNo = 35}))) [(dlg: Delegation.Certificate { w = #2, iVK = pub:a3219c1a, dVK = pub:1862f6a2 },MempoolDlgErr (WrongEpoch (EpochNumber {getEpochNumber = 0}) (EpochNumber {getEpochNumber = 2})))] (MempoolSize {msNumTxs = 0, msNumBytes = 0})
          -- > TraceLabelPeer (CoreId (CoreNodeId 0)) Recv MsgBatchDone
          -- > TraceLabelPeer (CoreId (CoreNodeId 0)) Recv MsgRequestTxs [dlgid: certificateid: fb50aa22]
          -- > TraceLabelPeer (CoreId (CoreNodeId 0)) Send MsgReplyTxs []
          prop_simple_real_pbft_convergence TestSetup
            { setupEBBs       = ProduceEBBs
            , setupK          = SecurityParam 4
            , setupTestConfig = TestConfig
              { numCoreNodes = NumCoreNodes 3
              , numSlots     = NumSlots 96
              , nodeTopology =    --   1 <-> 0 <-> 2
                NodeTopology $ Map.fromList [(CoreNodeId 0,Set.fromList []),(CoreNodeId 1,Set.fromList [CoreNodeId 0]),(CoreNodeId 2,Set.fromList [CoreNodeId 0])]
              , initSeed     = Seed 0
              }
            , setupNodeJoinPlan = NodeJoinPlan $ Map.fromList [(CoreNodeId 0,SlotNo 0),(CoreNodeId 1,SlotNo 0),(CoreNodeId 2,SlotNo 83)]
            , setupNodeRestarts = NodeRestarts $ Map.fromList [(SlotNo 83,Map.fromList [(CoreNodeId 2,NodeRekey)])]
            , setupSlotLength   = defaultSlotLength
            , setupVersion      = (NodeToNodeV_1, ByronNodeToNodeVersion1)
            }
    , testProperty "topology prevents timely dlg cert tx propagation" $
          -- Caught a bug in the test infrastructure. If node X rekeys in slot
          -- S and Y leads slot S+1, then either the topology must connect X
          -- and Y directly, or Y must join before slot S. Otherwise, X
          -- successfully propagates its dlg cert tx to the pre-existing nodes,
          -- but Y won't pull it from them in time to include the tx in its
          -- block for S+1. When Y joined in S, its mini protocols all failed
          -- and were delayed to restart in the next slot (S+1). They do so,
          -- but it forges its block in S+1 before the dlg cert tx arrives.
          --
          -- The expected failure is an unexpected block rejection (cf
          -- 'pgaExpectedCannotForge') (PBftNotGenesisDelegate) in Slot 49.
          -- It disappears with the mesh topology, which usually means subtle
          -- timings are involved, unfortunately.
          --
          -- c3 and c4 join in s37. c4 rekeys in s37. c3 leads in s38.
          --
          -- The dlg cert tx does not arrive at c3 in time because of the
          -- topology. When c3 and c4 join in s37, their mini protocol threads
          -- that serve {c0,c1,c2} as clients fail and are scheduled to restart
          -- at the onset of the next slot (s38). Since c3 and c4 are not
          -- directly connected, and in particular the mini protocol instances
          -- with clients in {c0,c1,c2} and server c4 are down, c4 cannot
          -- communicate its dlg cert tx to c3 in time (it arrives in s38, but
          -- after c3 has forged its block).
          let ncn5 = NumCoreNodes 5 in
          expectFailure $
          once $
          prop_simple_real_pbft_convergence TestSetup
            { setupEBBs       = ProduceEBBs
            , setupK          = SecurityParam 2
            , setupTestConfig = TestConfig
              { numCoreNodes = ncn5
              , numSlots     = NumSlots 50
              , nodeTopology = -- 3 <-> {0,1,2} <-> 4
                NodeTopology (Map.fromList [(CoreNodeId 0,Set.fromList []),(CoreNodeId 1,Set.fromList [CoreNodeId 0]),(CoreNodeId 2,Set.fromList [CoreNodeId 0, CoreNodeId 1]),(CoreNodeId 3,Set.fromList [CoreNodeId 0,CoreNodeId 1,CoreNodeId 2]),(CoreNodeId 4,Set.fromList [CoreNodeId 0,CoreNodeId 1,CoreNodeId 2])])
              , initSeed     = Seed 0
              }
            , setupNodeJoinPlan = NodeJoinPlan (Map.fromList [(CoreNodeId 0,SlotNo {unSlotNo = 0}),(CoreNodeId 1,SlotNo {unSlotNo = 0}),(CoreNodeId 2,SlotNo {unSlotNo = 0}),(CoreNodeId 3,SlotNo {unSlotNo = 37}),(CoreNodeId 4,SlotNo {unSlotNo = 37})])
            , setupNodeRestarts = NodeRestarts (Map.fromList [(SlotNo {unSlotNo = 37},Map.fromList [(CoreNodeId 4,NodeRekey)])])
            , setupSlotLength   = defaultSlotLength
            , setupVersion      = (NodeToNodeV_1, ByronNodeToNodeVersion1)
            }
    , testProperty "mkDelegationEnvironment uses currentSlot not latestSlot" $
      -- After rekeying, node 2 continues to emit its dlg cert tx. This an ugly
      -- implementation detail of rekeying, but as a nice surprise it found a
      -- bug!
      --
      -- In slot 40, node 1 forged a block that included the now-/expired/ dlg
      -- cert tx (cf @WrongEpoch@). This happened because the Byron transaction
      -- validation logic was using the slot of the latest block (i.e. 39) as
      -- the current slot (i.e. actually 40), so the transaction wasn't
      -- identified as expired until it was already inside a block.
      once $
      let ncn = NumCoreNodes 3 in
      prop_simple_real_pbft_convergence TestSetup
        { setupEBBs       = NoEBBs
        , setupK          = SecurityParam 2
        , setupTestConfig = TestConfig
          { numCoreNodes = ncn
          , numSlots     = NumSlots 41
          , nodeTopology = meshNodeTopology ncn
          , initSeed     = Seed 0
          }
        , setupNodeJoinPlan = trivialNodeJoinPlan ncn
        , setupNodeRestarts = NodeRestarts $ Map.singleton (SlotNo 30) $ Map.singleton (CoreNodeId 2) NodeRekey
        , setupSlotLength   = defaultSlotLength
            , setupVersion      = (NodeToNodeV_1, ByronNodeToNodeVersion1)
        }
    , testProperty "delayed message corner case" $
          once $
          let ncn = NumCoreNodes 2 in
          prop_simple_real_pbft_convergence TestSetup
            { setupEBBs       = NoEBBs
            , setupK          = SecurityParam 7
            , setupTestConfig = TestConfig
              { numCoreNodes = ncn
              , numSlots     = NumSlots 10
              , nodeTopology = meshNodeTopology ncn
              , initSeed     = Seed 0
              }
            , setupNodeJoinPlan = NodeJoinPlan (Map.fromList [(CoreNodeId 0,SlotNo {unSlotNo = 0}),(CoreNodeId 1,SlotNo {unSlotNo = 1})])
            , setupNodeRestarts = noRestarts
            , setupSlotLength   = defaultSlotLength
            , setupVersion      = (NodeToNodeV_1, ByronNodeToNodeVersion1)
            }
    , testProperty "mkUpdateLabels anticipates instant confirmation" $
          -- caught a bug in 'mkUpdateLabels' where it didn't anticipate that
          -- node c0 can confirm the proposal as soon as it joins when quorum
          -- == 1
          let ncn = NumCoreNodes 3 in
          prop_simple_real_pbft_convergence TestSetup
            { setupEBBs       = NoEBBs
            , setupK          = SecurityParam 9
            , setupTestConfig = TestConfig
              { numCoreNodes = ncn
              , numSlots     = NumSlots 1
              , nodeTopology = meshNodeTopology ncn
              , initSeed     = Seed 0
              }
            , setupNodeJoinPlan = trivialNodeJoinPlan ncn
            , setupNodeRestarts = noRestarts
            , setupSlotLength   = defaultSlotLength
            , setupVersion      = (NodeToNodeV_1, ByronNodeToNodeVersion1)
            }
    , testProperty "have nodes add transactions as promptly as possible, as expected by proposal tracking" $
          -- this repro requires that changes to the ledger point triggers the
          -- nearly oracular wallet to attempt to add its proposal vote again
          --
          -- Without that, node c1's own vote is not included in the block it
          -- forges in the last slot, because it attempts to add the vote
          -- before the proposal arrives from c0. With the trigger, the arrival
          -- of c0's block triggers it. In particular, the ledger *slot*
          -- doesn't change in this repro, since the new block and its
          -- predecessor both inhabit slot 0. EBBeeeeeees!
          let ncn = NumCoreNodes 4 in
          prop_simple_real_pbft_convergence TestSetup
            { setupEBBs       = NoEBBs
            , setupK          = SecurityParam 8
            , setupTestConfig = TestConfig
              { numCoreNodes = ncn
              , numSlots     = NumSlots 2
              , nodeTopology = meshNodeTopology ncn
              , initSeed     = Seed 0
              }
            , setupNodeJoinPlan = trivialNodeJoinPlan ncn
            , setupNodeRestarts = noRestarts
            , setupSlotLength   = defaultSlotLength
            , setupVersion      = (NodeToNodeV_1, ByronNodeToNodeVersion1)
            }
    , testProperty "track proposals even when c0 is not the first to lead" $
          -- requires prompt and accurate vote tracking when c0 is not the
          -- first node to lead
          --
          -- The necessary promptness trigger in this case is the arrival of
          -- the proposal transaction.
          let ncn = NumCoreNodes 4 in
          prop_simple_real_pbft_convergence TestSetup
            { setupEBBs       = NoEBBs
            , setupK          = SecurityParam 5
            , setupTestConfig = TestConfig
              { numCoreNodes = ncn
              , numSlots     = NumSlots 5
              , nodeTopology = meshNodeTopology ncn
              , initSeed     = Seed 0
              }
            , setupNodeJoinPlan = NodeJoinPlan $ Map.fromList [ (CoreNodeId 0, SlotNo 2) , (CoreNodeId 1, SlotNo 3) , (CoreNodeId 2, SlotNo 4) , (CoreNodeId 3, SlotNo 4) ]
            , setupNodeRestarts = noRestarts
            , setupSlotLength   = defaultSlotLength
            , setupVersion      = (NodeToNodeV_1, ByronNodeToNodeVersion1)
            }
    , testProperty "cardano-ledger-byron checks for proposal confirmation before it checks for expiry" $
          -- must check for quorum before checking for expiration
          let ncn = NumCoreNodes 5 in
          prop_simple_real_pbft_convergence TestSetup
            { setupEBBs       = NoEBBs
            , setupK          = SecurityParam 10
            , setupTestConfig = TestConfig
              { numCoreNodes = ncn
              , numSlots     = NumSlots 12
              , nodeTopology = meshNodeTopology ncn
              , initSeed     = Seed 0
              }
            , setupNodeJoinPlan = NodeJoinPlan $ Map.fromList [ (CoreNodeId 0, SlotNo 0) , (CoreNodeId 1, SlotNo 0) , (CoreNodeId 2, SlotNo 10) , (CoreNodeId 3, SlotNo 10) , (CoreNodeId 4, SlotNo 10) ]
            , setupNodeRestarts = noRestarts
            , setupSlotLength   = defaultSlotLength
            , setupVersion      = (NodeToNodeV_1, ByronNodeToNodeVersion1)
            }
    , testProperty "repropose an expired proposal" $
          -- the proposal expires in slot 10, but then c0 reintroduces it in
          -- slot 11 and it is eventually confirmed
          let ncn = NumCoreNodes 5 in
          prop_simple_real_pbft_convergence TestSetup
            { setupEBBs       = NoEBBs
            , setupK          = SecurityParam 10
            , setupTestConfig = TestConfig
              { numCoreNodes = ncn
              , numSlots     = NumSlots 17
              , nodeTopology = meshNodeTopology ncn
              , initSeed     = Seed 0
              }
            , setupNodeJoinPlan = NodeJoinPlan $ Map.fromList
              [(CoreNodeId 0, SlotNo 0)
              ,(CoreNodeId 1, SlotNo 10)
              ,(CoreNodeId 2, SlotNo 11)
              ,(CoreNodeId 3, SlotNo 11)
              ,(CoreNodeId 4, SlotNo 16)
              ]
            , setupNodeRestarts = noRestarts
            , setupSlotLength   = defaultSlotLength
            , setupVersion      = (NodeToNodeV_1, ByronNodeToNodeVersion1)
            }
    , testProperty "only expect EBBs if the reference simulator does" $
          -- In this repro, block in the 20th slot is wasted since c2 just
          -- joined. As a result, the final chains won't include that EBB.
          let ncn = NumCoreNodes 3 in
          prop_simple_real_pbft_convergence TestSetup
            { setupEBBs       = ProduceEBBs
            , setupK          = SecurityParam 2
            , setupTestConfig = TestConfig
              { numCoreNodes = ncn
              , numSlots     = NumSlots 21
              , nodeTopology = meshNodeTopology ncn
              , initSeed     = Seed 0
              }
            , setupNodeJoinPlan = NodeJoinPlan $ Map.fromList
              [ (CoreNodeId 0,SlotNo {unSlotNo = 0})
              , (CoreNodeId 1,SlotNo {unSlotNo = 0})
              , (CoreNodeId 2,SlotNo {unSlotNo = 20})
              ]
            , setupNodeRestarts = noRestarts
            , setupSlotLength   = defaultSlotLength
            , setupVersion      = (NodeToNodeV_1, ByronNodeToNodeVersion1)
            }
    , testProperty "only check updates for mesh topologies" $
          -- This repro exercises
          -- 'Test.ThreadNet.Byron.TrackUpdates.checkTopo'.
          --
          -- The predicted slot outcomes are
          --
          -- > leader 01234
          -- >    s0  NAAAA
          -- >    s5  NAAAA
          -- >    s10 NWN
          --
          -- The votes of c1, c3, and c4 arrive to c2 during s11 via TxSub
          -- /before/ the block containing the proposal does, so c2's mempool
          -- rejects them as invalid. When it then forges in s12, it only
          -- includes its own vote, which doesn't meet quota (3 = 5 * 0.6) and
          -- so the proposal then expires (TTL 10 slots, but only after an
          -- endorsement; see Issue 749 in cardano-ledger-byron).
          --
          -- "Test.ThreadNet.Byron.TrackUpdates" does not otherwise
          -- correctly anticipate such races, so it makes no requirement for
          -- non-mesh topologies.
          prop_simple_real_pbft_convergence TestSetup
            { setupEBBs       = NoEBBs
            , setupK          = SecurityParam 10
            , setupTestConfig = TestConfig
              { numCoreNodes = NumCoreNodes 5
              , numSlots     = NumSlots 13
              , nodeTopology = NodeTopology $ Map.fromList
                               -- mesh except for 0 <-> 2
                [ (CoreNodeId 0, Set.fromList [])
                , (CoreNodeId 1, Set.fromList [CoreNodeId 0])
                , (CoreNodeId 2, Set.fromList [CoreNodeId 1])
                , (CoreNodeId 3, Set.fromList [CoreNodeId 0, CoreNodeId 1, CoreNodeId 2])
                , (CoreNodeId 4, Set.fromList [CoreNodeId 0, CoreNodeId 1, CoreNodeId 2, CoreNodeId 3])
              ]
              , initSeed = Seed 0
            }
            , setupNodeJoinPlan = NodeJoinPlan $ Map.fromList
              [ (CoreNodeId 0, SlotNo 0)
              , (CoreNodeId 1, SlotNo 11)
              , (CoreNodeId 2, SlotNo 11)
              , (CoreNodeId 3, SlotNo 11)
              , (CoreNodeId 4, SlotNo 11)
              ]
            , setupNodeRestarts = noRestarts
            , setupSlotLength   = defaultSlotLength
            , setupVersion      = (NodeToNodeV_1, ByronNodeToNodeVersion1)
            }
    , testProperty "HeaderProtocolError prevents JIT EBB emission" $
          -- "extra" EBB generated in anticipation of a block that ends up
          -- being PBftExceededSignThreshold
          --
          -- PR 1942 reduced the input of blockProduction from ExtLedgerState
          -- to just LedgerState, but that does not provide enough information
          -- to fully anticipate the block's invalidity, since it excludes
          -- protocol-level validation
          --
          -- Remark: this particular repro involves a peculiar phenomenon
          -- apparent for k=8 n=3 in which the nodes' steady-state behavior
          -- involves a regularly occurring 'PBftExceededSignThreshold'
          once $
          prop_simple_real_pbft_convergence TestSetup
            { setupEBBs       = ProduceEBBs
            , setupK          = SecurityParam 8
            , setupTestConfig = TestConfig
              { numCoreNodes = NumCoreNodes 3
              , numSlots     = NumSlots 81
              , nodeTopology = meshNodeTopology (NumCoreNodes 3)
              , initSeed     = Seed 0
              }
            , setupNodeJoinPlan = NodeJoinPlan (Map.fromList [(CoreNodeId 0,SlotNo {unSlotNo = 2}),(CoreNodeId 1,SlotNo {unSlotNo = 6}),(CoreNodeId 2,SlotNo {unSlotNo = 9})])
            , setupNodeRestarts = noRestarts
            , setupSlotLength   = slotLengthFromSec 20
            , setupVersion      = (NodeToNodeV_1, ByronNodeToNodeVersion1)
            }
    , testProperty "WallClock must handle PastHorizon by exactly slotLength delay" $
          -- Previously, 'PastTimeHorizon' put the node to sleep for 60s. That
          -- had caused it to be offline for slots it shouldn't miss.
          once $
          prop_simple_real_pbft_convergence TestSetup
            { setupEBBs       = ProduceEBBs
            , setupK          = SecurityParam 2
            , setupTestConfig = TestConfig
              { numCoreNodes = NumCoreNodes 2
              , numSlots     = NumSlots 39
              , nodeTopology = meshNodeTopology (NumCoreNodes 2)
              , initSeed     = Seed 0
              }
            , setupNodeJoinPlan = NodeJoinPlan (Map.fromList [(CoreNodeId 0,SlotNo {unSlotNo = 0}),(CoreNodeId 1,SlotNo {unSlotNo = 33})])
            , setupNodeRestarts = noRestarts
            , setupSlotLength   = slotLengthFromSec 20
            , setupVersion      = (NodeToNodeV_1, ByronNodeToNodeVersion1)
            }
    , testProperty "systemTimeCurrent must not answer once clock is exhausted" $
          -- This test would pass (before fixing the bug) if I moved both the
          -- end of the test and also the last node's join slot ahead by 3
          -- slots. So I think to the epoch boundary may be involved, likely
          -- via its effect on the 'SafeZone'.
          --
          -- The failure is due to the following sequence. The node joins with
          -- an empty chain. The existing nodes disconnect from it. The new
          -- node blocks due to 'PastTimeHorizon', and so syncs the whole chain.
          -- Then the new node leads. But the old nodes don't get its new
          -- block. Then the test ends.
          --
          -- The new node should not have been able to create a block in that
          -- slot. The 'PastTimeHorizon' should cause the node to sleep for an
          -- entire slot duration, so it should have missed it's chance to
          -- lead.
          --
          -- This failure clarified that 'OracularClock.systemTimeCurrent' should not provide
          -- a time after the clock is exhausted.
          once $
          prop_simple_real_pbft_convergence TestSetup
            { setupEBBs       = NoEBBs
            , setupK          = SecurityParam 2
            , setupTestConfig = TestConfig
              { numCoreNodes = NumCoreNodes 3
              , numSlots     = NumSlots 21
              , nodeTopology = meshNodeTopology (NumCoreNodes 3)
              , initSeed     = Seed 0
              }
            , setupNodeJoinPlan = NodeJoinPlan (Map.fromList [(CoreNodeId 0,SlotNo {unSlotNo = 0}),(CoreNodeId 1,SlotNo {unSlotNo = 0}),(CoreNodeId 2,SlotNo {unSlotNo = 20})])
            , setupNodeRestarts = noRestarts
            , setupSlotLength   = slotLengthFromSec 20
            , setupVersion      = (NodeToNodeV_1, ByronNodeToNodeVersion1)
            }
    , testProperty "simple convergence" $ \setup ->
        prop_simple_real_pbft_convergence setup
    ]
  where
    defaultSlotLength :: SlotLength
    defaultSlotLength = slotLengthFromSec 1

prop_deterministicPlan :: PBftParams -> NumSlots -> NumCoreNodes -> Property
prop_deterministicPlan params numSlots numCoreNodes =
    property $ case Ref.simulate params njp numSlots of
      Ref.Forked{}         -> False
      Ref.Outcomes{}       -> True
      Ref.Nondeterministic -> False
  where
    njp = trivialNodeJoinPlan numCoreNodes

expectedCannotForge
  :: SecurityParam
  -> NumCoreNodes
  -> NodeRestarts
  -> SlotNo
  -> NodeId
  -> WrapCannotForge ByronBlock
  -> Bool
expectedCannotForge
  k numCoreNodes (NodeRestarts nrs)
  s (CoreId (CoreNodeId i)) (WrapCannotForge cl)
  = case cl of
    PBftCannotForgeThresholdExceeded{} ->
        -- TODO validate this against Ref implementation?
        True
    PBftCannotForgeInvalidDelegation {} ->
        -- only if it rekeyed within before a restarts latest possible
        -- maturation
        not $ null $
        [ ()
        | (restartSlot, nrs') <- Map.toList nrs
        , restartSlot <= s
            && s < latestPossibleDlgMaturation k numCoreNodes restartSlot
        , (CoreNodeId i', NodeRekey) <- Map.toList nrs'
        , i' == i
        ]
expectedCannotForge _ _ _ _ _ _ = False

-- | If we rekey in slot rekeySlot, it is in general possible that the leader
-- of rekeySlot will include our delegation transaction in its new block.
-- However, in the current test infrastructure, it will consistently have
-- already forged its new block before receiving our new transaction.
--
-- Thus the first leader to forge a valid block in one of the slots rekeySlot+1
-- through rekeySlot+N will include our new transaction in its new block. There
-- are two reasons that those leaders (excepting the last) may fail to forge a
-- valid block.
--
-- * The rekeyed node might be the scheduled leader, and so it'll immediately
--   reject its new block as invalid (since its delegation cannot have already
--   matured).
--
-- * The PBFT threshold may already be saturated for that node.
--
-- See @genNodeRekeys@ for the logic that ensures at least one of those slots'
-- leaders will be able to lead.
latestPossibleDlgMaturation
  :: SecurityParam -> NumCoreNodes -> SlotNo -> SlotNo
latestPossibleDlgMaturation
  (SecurityParam k) (NumCoreNodes n) (SlotNo rekeySlot) =
    SlotNo $ rekeySlot + n + 2 * k

prop_simple_real_pbft_convergence :: TestSetup -> Property
prop_simple_real_pbft_convergence TestSetup
  { setupEBBs         = produceEBBs
  , setupK            = k
  , setupTestConfig   = testConfig
  , setupNodeJoinPlan = nodeJoinPlan
  , setupNodeRestarts = nodeRestarts
  , setupSlotLength   = slotLength
  , setupVersion      = version
  } =
    tabulate "produce EBBs" [show produceEBBs] $
    tabulate "Ref.PBFT result" [Ref.resultConstrName refResult] $
    tabulate "proposed protocol version was adopted" [show aPvuRequired] $
    tabulate "proposed software version was adopted" [show aSvuRequired] $
    counterexample ("params: " <> show params) $
    counterexample ("Ref.PBFT result: " <> show refResult) $
    counterexample
      ("delegation certificates: " <> show [
            (,) nid $
            mapMaybe (>>= \x@(_, dlgs) -> if null dlgs then Nothing else Just x) $
            [ case Byron.byronBlockRaw blk of
                Block.ABOBBlock b    -> Just (Block.blockSlot b, Delegation.getPayload $ Block.blockDlgPayload b)
                Block.ABOBBoundary _ -> Nothing
            | blk <- Chain.chainToList ch
            ]
          | (nid, ch) <- finalChains
          ]) $
    prop_general PropGeneralArgs
      { pgaBlockProperty       = const $ property True
      , pgaCountTxs            = Byron.countByronGenTxs
      , pgaExpectedCannotForge = expectedCannotForge k numCoreNodes nodeRestarts
      , pgaFirstBlockNo        = 1
      , pgaFixedMaxForkLength  =
          Just $ NumBlocks $ case refResult of
            Ref.Forked{} -> 1
            _            -> 0
      , pgaFixedSchedule       =
          Just $ roundRobinLeaderSchedule numCoreNodes numSlots
      , pgaSecurityParam       = k
      , pgaTestConfig          = testConfig
      , pgaTestConfigB         = testConfigB
      }
      testOutput .&&.
    prop_pvu .&&.
    prop_svu .&&.
    not (all (Chain.null . snd) finalChains) .&&.
    case refResult of
      Ref.Outcomes outcomes ->
          conjoin (map (hasAllEBBs k produceEBBs outcomes) finalChains)
      _ -> property True
  where
    TestConfig
      { nodeTopology
      , numCoreNodes
      , numSlots
      , initSeed
      } = testConfig

    testConfigB = TestConfigB
      { forgeEbbEnv = case produceEBBs of
          NoEBBs      -> Nothing
          ProduceEBBs -> Just byronForgeEbbEnv
      , future       = singleEraFuture slotLength epochSize
      , messageDelay = noCalcMessageDelay
      , nodeJoinPlan
      , nodeRestarts
      , txGenExtra   = ()
      , version      = version
      }

    testOutput =
        runTestNetwork testConfig testConfigB TestConfigMB
            { nodeInfo = \nid ->
                mkProtocolByronAndHardForkTxs
                  params nid genesisConfig genesisSecrets
                  theProposedProtocolVersion
            , mkRekeyM = Just $ fromRekeyingToRekeyM Rekeying
              { rekeyOracle   = \cid s ->
                  let nominalSlots = case refResult of
                        Ref.Forked{}           -> Set.empty
                        Ref.Outcomes outcomes  ->
                          Set.fromList $
                          [ s'
                          | (Ref.Nominal, s') <- zip outcomes [0..]
                            -- ignore the 'Ref.Nominal's disrupted by the
                            -- rekey; see comment on 'refResult'
                          , cid /= Ref.mkLeaderOf params s'
                          ]
                        Ref.Nondeterministic{} -> Set.empty
                  in Set.lookupGT s nominalSlots
              , rekeyUpd      = mkRekeyUpd genesisConfig genesisSecrets
              , rekeyFreshSKs =
                  let prj  = Crypto.hashVerKey . Crypto.deriveVerKeyDSIGN
                      acc0 =   -- the VKs of the operational keys at genesis
                        Set.fromList $
                        map (Common.hashKey . Delegation.delegateVK) $
                        Map.elems $
                        Genesis.unGenesisDelegation $
                        Genesis.gdHeavyDelegation $
                        Genesis.configGenesisData genesisConfig
                      genKeyDSIGNRandom = do
                        Crypto.genKeyDSIGN . mkSeedFromBytes . BS.pack
                          <$> vectorOf 32 arbitrary
                  in
                  Stream.nubOrdBy prj acc0 $
                    runGen initSeed $   -- seems fine to reuse seed for this
                      sequence $ let ms = genKeyDSIGNRandom Stream.:< ms in ms
              }
            }

    -- Byron has a hard-coded relation between k and the size of an epoch
    epochSize :: EpochSize
    epochSize = fromByronEpochSlots $ kEpochSlots (toByronBlockCount k)

    -- NOTE: If a node is rekeying, then the 'Ref.Outcome' case will include
    -- some 'Ref.Nominal' outcomes that should actually be 'Ref.Unable'.
    refResult :: Ref.Result
    refResult = Ref.simulate params nodeJoinPlan numSlots

    finalChains :: [(NodeId, Chain ByronBlock)]
    finalChains = Map.toList $ nodeOutputFinalChain <$> testOutputNodes testOutput

    finalLedgers :: [(NodeId, Byron.LedgerState ByronBlock)]
    finalLedgers = Map.toList $ nodeOutputFinalLedger <$> testOutputNodes testOutput

    pvuLabels :: [(NodeId, ProtocolVersionUpdateLabel)]
    pvuLabels = map (fmap fst) updLabels

    svuLabels :: [(NodeId, SoftwareVersionUpdateLabel)]
    svuLabels = map (fmap snd) updLabels

    updLabels
      :: [(NodeId, (ProtocolVersionUpdateLabel, SoftwareVersionUpdateLabel))]
    updLabels =
        [ (,) cid $
          mkUpdateLabels
            params
            numSlots
            genesisConfig
            nodeJoinPlan
            nodeTopology
            refResult
            ldgr
        | (cid, ldgr) <- finalLedgers
        ]

    -- whether the proposed protocol version was required to have been adopted
    -- in one of the chains
    aPvuRequired :: Bool
    aPvuRequired =
        or
        [ Just True == pvuRequired
        | (_, ProtocolVersionUpdateLabel{pvuRequired}) <- pvuLabels
        ]

    -- whether the proposed software version was required to have been adopted in
    -- one of the chains
    aSvuRequired :: Bool
    aSvuRequired =
        or
        [ Just True == svuRequired
        | (_, SoftwareVersionUpdateLabel{svuRequired}) <- svuLabels
        ]

    -- check whether the proposed protocol version should have been and if so
    -- was adopted
    prop_pvu :: Property
    prop_pvu =
        counterexample (show pvuLabels) $
        conjoin
        [ counterexample (show (cid, pvuLabel)) $
          let ProtocolVersionUpdateLabel
                { pvuObserved
                , pvuRequired
                } = pvuLabel
          in
          property $ case pvuRequired of
            Just b  -> b == pvuObserved
            Nothing -> True
        | (cid, pvuLabel) <- pvuLabels
        ]

    -- check whether the proposed software version should have been and if so
    -- was adopted
    prop_svu :: Property
    prop_svu =
        counterexample (show svuLabels) $
        conjoin
        [ counterexample (show (cid, svuLabel)) $
          let SoftwareVersionUpdateLabel
                { svuObserved
                , svuRequired
                } = svuLabel
          in
          property $ case svuRequired of
            Just b  -> b == svuObserved
            Nothing -> True
        | (cid, svuLabel) <- svuLabels
        ]

    params :: PBftParams
    params = byronPBftParams k numCoreNodes

    genesisConfig  :: Genesis.Config
    genesisSecrets :: Genesis.GeneratedSecrets
    (genesisConfig, genesisSecrets) = generateGenesisConfig slotLength params

byronForgeEbbEnv :: ForgeEbbEnv ByronBlock
byronForgeEbbEnv = ForgeEbbEnv
    { forgeEBB = Byron.forgeEBB . configBlock
    }

-- | Whether to produce EBBs in the tests or not
--
-- TODO add a case to generate EBBs upto some epoch, like on mainnet
data ProduceEBBs
  = NoEBBs
    -- ^ No EBBs are produced in the tests. The node will still automatically
    -- produce its own genesis EBB.
  | ProduceEBBs
    -- ^ In addition to the genesis EBB the node generates itself, the tests
    -- also produce an EBB at the start of each subsequent epoch.
  deriving (Eq, Show)

-- | Exported alias for 'NoEBBs'.
--
noEBBs :: ProduceEBBs
noEBBs = NoEBBs

instance Arbitrary ProduceEBBs where
  arbitrary = elements [NoEBBs, ProduceEBBs]
  shrink NoEBBs      = []
  shrink ProduceEBBs = [NoEBBs]

hasAllEBBs :: SecurityParam
           -> ProduceEBBs
           -> [Ref.Outcome]
           -> (NodeId, Chain ByronBlock)
           -> Property
hasAllEBBs k produceEBBs outcomes (nid, c) =
    counterexample ("Missing or unexpected EBBs in " <> condense (nid, c)) $
    actual === expected
  where
    expected :: [EpochNo]
    expected = case produceEBBs of
      NoEBBs      -> [0]
      ProduceEBBs -> case reverse [ s :: SlotNo | (Ref.Nominal, s) <- zip outcomes [0..] ] of
          []  -> [0]
          s:_ -> coerce [0 .. hi]
            where
              hi :: Word64
              hi = unSlotNo s `div` denom
              denom = unEpochSlots $ kEpochSlots $ coerce k

    actual   = mapMaybe blockIsEBB $ Chain.toOldestFirst c

{-------------------------------------------------------------------------------
  Generating node join plans that ensure sufficiently dense chains
-------------------------------------------------------------------------------}

genSlot :: SlotNo -> SlotNo -> Gen SlotNo
genSlot lo hi = SlotNo <$> choose (unSlotNo lo, unSlotNo hi)

-- | As 'genNodeJoinPlan', but ensures an additional invariant
--
-- INVARIANT this 'NodeJoinPlan' ensures that -- under \"ideal circumstances\"
-- -- the chain includes at least @k@ blocks within every @2k@-slot window.
--
-- Note that there is only one chain: at any slot onset, the net's fork only
-- has one tine.
--
genByronNodeJoinPlan :: PBftParams -> NumSlots -> Gen NodeJoinPlan
genByronNodeJoinPlan params numSlots@(NumSlots t)
  | n < 0 || t < 1 = error $ "Cannot generate Byron NodeJoinPlan: "
    ++ show (params, numSlots)
  | otherwise      =
    go (NodeJoinPlan Map.empty) Ref.emptyState
      `suchThat` (\njp -> Ref.definitelyEnoughBlocks params $
                          Ref.simulate params njp numSlots)

        -- This suchThat might loop a few times, but it should always
        -- eventually succeed, since the plan where all nodes join immediately
        -- satisfies it.
        --
        -- In a run of 7000 successful Byron tests, this 'suchThat' retried:
        --
        -- 486 retried once
        -- 100 retried twice
        -- 10 retried 3 times
        -- 4 retried 4 times
        -- 4 retried 5 times
        -- 1 retried 6 times
  where
    PBftParams{pbftNumNodes} = params
    NumCoreNodes n           = pbftNumNodes

    sentinel = SlotNo t
    lastSlot = pred sentinel   -- note the t guard above

    go ::
         NodeJoinPlan
         -- ^ an /incomplete/ and /viable/ node join plan
      -> Ref.State
         -- ^ a state whose 'Ref.nextSlot' is <= the last join slot in given
         -- plan (or 0 if the plan is empty)
      -> Gen NodeJoinPlan
    go nodeJoinPlan@(NodeJoinPlan m) st
      | i == n    = pure $ NodeJoinPlan m
      | otherwise = do
            -- @True@ if this join slot for @nid@ is viable
            --
            -- /Viable/ means the desired chain density invariant remains
            -- satisfiable, at the very least the nodes after @nid@ may need to
            -- also join in this same slot.
            --
            -- Assuming @nodeJoinPlan@ is indeed viable and @st@ is indeed not
            -- ahead of it, then we should be able to find a join slot for
            -- @nid@ that is also viable: the viability of @nodeJoinPlan@ means
            -- @nid@ can at least join \"immediately\" wrt to @nodeJoinPlan@.
            --
            -- The base case is that the empty join plan and empty state are
            -- viable, which assumes that the invariant would be satisfied if
            -- all nodes join in slot 0. For uninterrupted round-robin, that
            -- merely requires @n * floor (k * t) >= k@. (TODO Does that
            -- __always__ suffice?)
        let check s' =
                Ref.viable params sentinel
                    (NodeJoinPlan (Map.insert nid s' m))
                    st
            lo = Ref.nextSlot st

            -- @check@ is downward-closed, but 'searchFromTo' requires
            -- upward-closed, so we search in dualized range
            inn = (maxBound -) . unSlotNo
            out = SlotNo . (maxBound -)
        s' <- case out <$> searchFromTo (check . out) (inn lastSlot) (inn lo) of
            Just hi -> genSlot lo hi
            Nothing -> error $
                "Cannot find viable Byron NodeJoinPlan: " ++
                show (params, numSlots, nodeJoinPlan, st)

        let m'  = Map.insert nid s' m

            -- optimization: avoid simulating from the same inputs multiple
            -- times
            --
            -- We've decided that @nid@ joins in @s'@, so advance the state to
            -- /just/ /before/ @s'@, since we might want @nid+1@ to also join
            -- in @s'@.
            --
            -- NOTE @m@ is congruent to @m'@ for all slots prior to @s'@
            st' = Ref.advanceUpTo params nodeJoinPlan st s'
        go (NodeJoinPlan m') st'
      where
        -- the next node to be added to the incomplete join plan
        nid = CoreNodeId i
        i   = case fst <$> Map.lookupMax m of
            Nothing             -> 0
            Just (CoreNodeId h) -> succ h

-- | Possibly promote some 'NodeRestart's to 'NodeRekey's
--
-- POSTCONDITION No node will rekey multiple times in a single epoch.
-- (Ouroboros allows at most one delegation per epoch, while each rekey and
-- also genesis itself all count as a delegation.)
--
-- POSTCONDITION Each rekey takes at least 2k slots, and the node can't lead
-- until it's finished. Therefore, at most one node will be rekeying at a time,
-- since otherwise its inability to lead may spoil the invariants established
-- by 'genByronNodeJoinPlan'.
--
genNodeRekeys
  :: PBftParams
  -> NodeJoinPlan
  -> NodeTopology
  -> NumSlots
  -> NodeRestarts
  -> Gen NodeRestarts
genNodeRekeys params nodeJoinPlan nodeTopology numSlots@(NumSlots t)
  nodeRestarts@(NodeRestarts nrs)
  | t <= 0    = pure nodeRestarts
  | otherwise =
    -- The necessary conditions are relatively rare, so favor adding a
    -- 'NodeRekey' when we can. But not always.
    (\x -> frequency [(2, pure nodeRestarts), (8, x)]) $
    -- TODO rekey nodes other than the last
    -- TODO rekey more than one node
    -- TODO rekey a node in a slot other than its join slot
    case Map.lookupMax njp of
      Just (cid, jslot)
            -- last node joins after first epoch, ...
          | jslot >= beginSecondEpoch
            -- ... and could instead join unproblematically at the latest time
            -- the delegation certificate would mature ...
          , latestPossibleDlgMaturation pbftSecurityParam numCoreNodes jslot
              < sentinel
          , let nodeJoinPlan' =
                  NodeJoinPlan $ Map.insert cid (jslot + twoK) njp
          , Ref.definitelyEnoughBlocks params $
            Ref.simulate params nodeJoinPlan' numSlots
            -- ... and does not join in the same slot as the leader of the next
            -- slot unless they are neighbors (otherwise the dlg cert tx might
            -- not reach it soon enough)
          , let nextLeader = Ref.mkLeaderOf params $ succ jslot
          , jslot /= coreNodeIdJoinSlot nodeJoinPlan nextLeader ||
            cid `elem` coreNodeIdNeighbors nodeTopology nextLeader
          -> pure $ NodeRestarts $
             -- We discard any 'NodeRestart's also scheduled for this slot.
             -- 'NodeRestart's are less interesting, so it's fine.
             --
             -- TODO retain those coincident node restarts as long as they
             -- don't include every other node, since that risks forgetting
             -- some relevant blocks.
             Map.insert jslot (Map.singleton cid NodeRekey) nrs
      _ -> pure nodeRestarts
  where
    PBftParams{pbftSecurityParam} = params
    k = maxRollbacks pbftSecurityParam
    sentinel = SlotNo t
    numCoreNodes = NumCoreNodes $ fromIntegral $ Map.size njp

    NodeJoinPlan njp = nodeJoinPlan

    twoK             = SlotNo $ 2 * k
    beginSecondEpoch = SlotNo $ 10 * k   -- c.f. Genesis.configEpochSlots

{-------------------------------------------------------------------------------
  Updating operational keys
-------------------------------------------------------------------------------}

-- | Overwrite the 'ProtocolInfo''s operational key, if any, and provide a
-- transaction for its new delegation certificate
--
mkRekeyUpd
  :: Monad m
  => Genesis.Config
  -> Genesis.GeneratedSecrets
  -> CoreNodeId
  -> ProtocolInfo m ByronBlock
  -> EpochNo
  -> Crypto.SignKeyDSIGN Crypto.ByronDSIGN
  -> m (Maybe (TestNodeInitialization m ByronBlock))
mkRekeyUpd genesisConfig genesisSecrets cid pInfo eno newSK =
    pInfoBlockForging pInfo <&> \blockForging ->
      case listToMaybe blockForging of
        Nothing -> Nothing
        Just _  ->
          let genSK = genesisSecretFor genesisConfig genesisSecrets cid
              creds' = updSignKey genSK bcfg cid (coerce eno) newSK
              blockForging' = byronBlockForging creds'
              pInfo' = pInfo { pInfoBlockForging = return [blockForging'] }

          in Just TestNodeInitialization
            { tniCrucialTxs = [dlgTx (blcDlgCert creds')]
            , tniProtocolInfo = pInfo'
            }
  where
    bcfg = configBlock (pInfoConfig pInfo)

-- | The secret key for a node index
--
genesisSecretFor
  :: Genesis.Config
  -> Genesis.GeneratedSecrets
  -> CoreNodeId
  -> Crypto.SignKeyDSIGN Crypto.ByronDSIGN
genesisSecretFor genesisConfig genesisSecrets cid =
    case hits of
        [sec] -> Crypto.SignKeyByronDSIGN sec
        _     -> error $ "Not exactly one genesis key " <> show (cid, hits)
  where
    hits :: [Crypto.SigningKey]
    hits =
        filter
            ((Just cid ==) . gkToIdx)
            (Genesis.gsDlgIssuersSecrets genesisSecrets)

    gkToIdx :: Crypto.SigningKey -> Maybe CoreNodeId
    gkToIdx =
        genesisKeyCoreNodeId genesisConfig
      . Crypto.VerKeyByronDSIGN . Crypto.toVerification

-- | Create new 'ByronLeaderCredentials' by generating a new delegation
-- certificate for the given new operational key.
--
updSignKey
  :: Crypto.SignKeyDSIGN Crypto.ByronDSIGN
  -> BlockConfig ByronBlock
  -> CoreNodeId
  -> EpochNumber
  -> Crypto.SignKeyDSIGN Crypto.ByronDSIGN
  -> ByronLeaderCredentials
updSignKey genSK extCfg cid eno newSK =
    ByronLeaderCredentials {
        blcSignKey    = sk'
      , blcDlgCert    = newCert
      , blcCoreNodeId = cid
      , blcLabel      = "Updated Byron credentials"
      }
  where
    newCert =
        Delegation.signCertificate
            (Byron.byronProtocolMagicId extCfg)
            (Crypto.toVerification sk')
            eno
            (Crypto.noPassSafeSigner gsk')

    Crypto.SignKeyByronDSIGN gsk' = genSK
    Crypto.SignKeyByronDSIGN sk'  = newSK

-- | Map a delegation certificate to a delegation transaction
--
dlgTx :: Delegation.Certificate -> Byron.GenTx ByronBlock
dlgTx cert =
    let ann = Cardano.Binary.serialize' (cert :: Delegation.Certificate)
        cert' = cert
          { Delegation.aEpoch     =
              Cardano.Binary.reAnnotate (Delegation.aEpoch cert)
          , Delegation.annotation = ann
          }
    in Byron.ByronDlg (Delegation.recoverCertificateId cert') cert'

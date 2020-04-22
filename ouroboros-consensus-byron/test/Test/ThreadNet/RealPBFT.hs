{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.ThreadNet.RealPBFT (
    tests
    -- * To support the DualPBFT tests
  , noEBBs
  , realPBftParams
  , genRealPBFTNodeJoinPlan
  , shrinkTestConfigSlotsOnly
  , expectedBlockRejection
  ) where

import           Control.Monad.Except (runExceptT)
import           Data.Coerce (coerce)
import qualified Data.Map.Strict as Map
import           Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import           Data.Time (Day (..), UTCTime (..))
import           Data.Word (Word64)

import           Numeric.Search.Range (searchFromTo)
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Cardano.Slotting.Slot

import           Ouroboros.Network.Block (SlotNo (..))
import           Ouroboros.Network.MockChain.Chain (Chain)
import qualified Ouroboros.Network.MockChain.Chain as Chain

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Extended (ExtValidationError (..))
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Node.Run (nodeIsEBB)
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.PBFT
import qualified Ouroboros.Consensus.Protocol.PBFT.Crypto as Crypto
import           Ouroboros.Consensus.Util.Condense (condense)
import           Ouroboros.Consensus.Util.Random

import qualified Cardano.Binary
import qualified Cardano.Chain.Block as Block
import qualified Cardano.Chain.Common as Common
import qualified Cardano.Chain.Delegation as Delegation
import qualified Cardano.Chain.Genesis as Genesis
import           Cardano.Chain.ProtocolConstants (kEpochSlots)
import           Cardano.Chain.Slotting (EpochNumber (..), unEpochSlots)
import qualified Cardano.Crypto as Crypto
import qualified Cardano.Crypto.DSIGN as Crypto
import qualified Test.Cardano.Chain.Genesis.Dummy as Dummy

import qualified Ouroboros.Consensus.Byron.Crypto.DSIGN as Crypto
import           Ouroboros.Consensus.Byron.Ledger (ByronBlock)
import qualified Ouroboros.Consensus.Byron.Ledger as Byron
import           Ouroboros.Consensus.Byron.Ledger.Conversions
import           Ouroboros.Consensus.Byron.Protocol

import           Test.ThreadNet.General
import           Test.ThreadNet.Network (NodeOutput (..),
                     TestNodeInitialization (..))
import qualified Test.ThreadNet.Ref.PBFT as Ref
import           Test.ThreadNet.TxGen.Byron ()
import           Test.ThreadNet.Util
import           Test.ThreadNet.Util.NodeJoinPlan
import           Test.ThreadNet.Util.NodeRestarts
import           Test.ThreadNet.Util.NodeTopology

import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Shrink (andId, dropId)
import qualified Test.Util.Stream as Stream
import           Test.Util.Time

import           Test.ThreadNet.RealPBFT.ProtocolInfo
import           Test.ThreadNet.RealPBFT.TrackUpdates

-- | Generate k values as small as this module is known to handle.
--
-- TODO Issue #1566 will bring this to k>=0, at which point we may be able to
-- relocate this to a more general module.
--
newtype K1 = K1 SecurityParam
  deriving (Show)

instance Arbitrary K1 where
  arbitrary                     = (K1 . SecurityParam) <$> elements [1 .. 10]
  shrink (K1 (SecurityParam k)) = (K1 . SecurityParam) <$> [ 1 .. k - 1 ]

tests :: TestTree
tests = testGroup "RealPBFT" $
    [ testProperty "trivial join plan is considered deterministic"
        $ \(K1 k) numCoreNodes ->
          prop_deterministicPlan $ realPBftParams k numCoreNodes
    , localOption (QuickCheckTests 10) $   -- each takes about 0.5 seconds!
      testProperty "check setup"
        $ \numCoreNodes ->
          forAll (elements (enumCoreNodes numCoreNodes)) $ \coreNodeId ->
          prop_setup_coreNodeId numCoreNodes coreNodeId
    , adjustOption (\(QuickCheckTests n) -> QuickCheckTests (1 `max` (div n 10))) $
      -- as of merging PR #773, this test case fails without the commit that
      -- introduces the InvalidRollForward exception
      --
      -- See a related discussion at
      -- https://github.com/input-output-hk/ouroboros-network/pull/773#issuecomment-522192097
      testProperty "addressed by InvalidRollForward exception (PR #773)" $
          once $
          let ncn = NumCoreNodes 3 in
          prop_simple_real_pbft_convergence ProduceEBBs (SecurityParam 10) TestConfig
            { numCoreNodes = ncn
            , numSlots     = NumSlots 24
            , nodeJoinPlan = NodeJoinPlan $ Map.fromList [(CoreNodeId 0,SlotNo 0), (CoreNodeId 1,SlotNo 20), (CoreNodeId 2,SlotNo 22)]
            , nodeRestarts = noRestarts
            , nodeTopology = meshNodeTopology ncn
            , slotLength   = defaultSlotLength
            , initSeed     = Seed (15069526818753326002, 9758937467355895013, 16548925776947010688, 13173070736975126721, 13719483751339084974)
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
          prop_simple_real_pbft_convergence ProduceEBBs (SecurityParam 10) TestConfig
            { numCoreNodes = ncn
            , numSlots     = NumSlots 2
            , nodeJoinPlan = NodeJoinPlan (Map.fromList [(CoreNodeId 0,SlotNo 0),(CoreNodeId 1,SlotNo 1)])
            , nodeRestarts = noRestarts
            , nodeTopology = meshNodeTopology ncn
            , slotLength   = defaultSlotLength
            , initSeed     = Seed (15069526818753326002, 9758937467355895013, 16548925776947010688, 13173070736975126721, 13719483751339084974)
            }
    , testProperty "rewind to EBB supported as of Issue #1312, #2" $
          once $
          let ncn = NumCoreNodes 2 in
          -- Same as above, except node 0 gets to forge an actual block before
          -- node 1 tells it to rewind to the EBB.
          prop_simple_real_pbft_convergence ProduceEBBs (SecurityParam 10) TestConfig
            { numCoreNodes = ncn
            , numSlots     = NumSlots 4
            , nodeJoinPlan = NodeJoinPlan (Map.fromList [(CoreNodeId 0,SlotNo {unSlotNo = 0}),(CoreNodeId 1,SlotNo {unSlotNo = 3})])
            , nodeRestarts = noRestarts
            , nodeTopology = meshNodeTopology ncn
            , slotLength   = defaultSlotLength
            , initSeed     = Seed (16817746570690588019, 3284322327197424879, 14951803542883145318, 5227823917971823767, 14093715642382269482)
            }
    , testProperty "one testOutputTipBlockNos update per node per slot" $
          once $
          let ncn = NumCoreNodes 2 in
          -- In this example, a node was forging a new block and then
          -- restarting. Its instrumentation thread ran before and also after
          -- the restart, which caused the 'testOutputTipBlockNos' field to
          -- contain data from the middle of the slot (after the node lead)
          -- instead of only from the onset of the slot.
          prop_simple_real_pbft_convergence ProduceEBBs (SecurityParam 5) TestConfig
            { numCoreNodes = ncn
            , numSlots     = NumSlots 7
            , nodeJoinPlan = NodeJoinPlan (Map.fromList [(CoreNodeId 0,SlotNo {unSlotNo = 0}),(CoreNodeId 1,SlotNo {unSlotNo = 0})])
            , nodeRestarts = NodeRestarts (Map.fromList [(SlotNo {unSlotNo = 5},Map.fromList [(CoreNodeId 1,NodeRestart)])])
            , nodeTopology = meshNodeTopology ncn
            , slotLength   = defaultSlotLength
            , initSeed     = Seed {getSeed = (17927476716858194849,11935807562313832971,15925564353519845641,3835030747036900598,2802397826914039548)}
            }
    , testProperty "BlockFetch live lock due to an EBB at the ImmutableDB tip, Issue #1435" $
          once $
          let ncn = NumCoreNodes 4 in
          -- c0's ImmDB is T > U > V. Note that U is an EBB and U and V are
          -- both in slot 50. When its BlockFetchServer tries to stream T and
          -- U using a ChainDB.Iterator, instead of looking in the
          -- ImmutableDB, we end up looking in the VolatileDB and incorrectly
          -- return ForkTooOld. The client keeps on requesting this block
          -- range, resulting in a live lock.
          prop_simple_real_pbft_convergence ProduceEBBs (SecurityParam 5) TestConfig
            { numCoreNodes = ncn
            , numSlots     = NumSlots 58
            , nodeJoinPlan = NodeJoinPlan $ Map.fromList [(CoreNodeId 0,SlotNo 3),(CoreNodeId 1,SlotNo 3),(CoreNodeId 2,SlotNo 5),(CoreNodeId 3,SlotNo 57)]
            , nodeRestarts = noRestarts
            , nodeTopology = meshNodeTopology ncn
            , slotLength   = defaultSlotLength
            , initSeed     = Seed (11044330969750026700,14522662956180538128,9026549867550077426,3049168255170604478,643621447671665184)
            }
    , testProperty "ImmutableDB is leaking file handles, #1543" $
          -- The failure was: c0 leaks one ImmDB file handle (for path
          -- @00000.epoch@, read only, offset at 0).
          --
          -- The test case seems somewhat fragile, since the 'slotLength' value
          -- seems to matter!
          once $
          let ncn5 = NumCoreNodes 5 in
          prop_simple_real_pbft_convergence NoEBBs (SecurityParam 2) TestConfig
            { numCoreNodes = ncn5
            -- Still fails if I increase numSlots.
            , numSlots     = NumSlots 54
            , nodeJoinPlan = NodeJoinPlan $ Map.fromList
              [ (CoreNodeId 0, SlotNo {unSlotNo = 0})
              , (CoreNodeId 1, SlotNo {unSlotNo = 0})
              , (CoreNodeId 2, SlotNo {unSlotNo = 0})
              , (CoreNodeId 3, SlotNo {unSlotNo = 53})
              , (CoreNodeId 4, SlotNo {unSlotNo = 53})
              ]
              -- Passes if I drop either of these restarts.
            , nodeRestarts = NodeRestarts $ Map.fromList
              [ (SlotNo {unSlotNo = 50},Map.fromList [(CoreNodeId 0,NodeRestart)])
              , (SlotNo {unSlotNo = 53},Map.fromList [(CoreNodeId 3,NodeRestart)])
              ]
            , nodeTopology = meshNodeTopology ncn5
              -- Slot length of 19s passes, and 21s also fails; I haven't seen this matter before.
            , slotLength   = slotLengthFromSec 20
            , initSeed     = Seed {getSeed = (15062108706768000853,6202101653126031470,15211681930891010376,1718914402782239589,12639712845887620121)}
            }
    , -- RealPBFT runs are slow, so do 10x less of this narrow test
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
          prop_simple_real_pbft_convergence ProduceEBBs (SecurityParam k) TestConfig
            { numCoreNodes = ncn
            , numSlots     = NumSlots $ window + slotsPerEpoch + slotsPerRekey + window
            , nodeJoinPlan = trivialNodeJoinPlan ncn
            , nodeRestarts = NodeRestarts $ Map.singleton
                (SlotNo (slotsPerEpoch + mod w window))
                (Map.singleton (CoreNodeId 0) NodeRekey)
            , nodeTopology = meshNodeTopology ncn
            , slotLength   = defaultSlotLength
            , initSeed     = seed
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
          prop_simple_real_pbft_convergence NoEBBs k TestConfig
            { numCoreNodes = ncn
            , numSlots     = NumSlots 2
            , nodeJoinPlan = trivialNodeJoinPlan ncn
            , nodeRestarts = NodeRestarts $ Map.singleton
                (SlotNo 1) (Map.singleton (CoreNodeId 1) NodeRestart)
            , nodeTopology = meshNodeTopology ncn
            , slotLength   = defaultSlotLength
            , initSeed     = Seed (4690259409304062007,9560140637825988311,3774468764133159390,14745090572658815456,7199590241247856333)
            }
    , testProperty "correct EpochNumber in delegation certificate 1" $
          -- Node 3 rekeys in slot 59, which is epoch 1. But Node 3 also leads
          -- that slot, and it forged and adopted a block before restarting. So
          -- the delegation transaction ends up in a block in slot 60, which is
          -- epoch 2.
          once $
          let ncn4 = NumCoreNodes 4 in
          prop_simple_real_pbft_convergence NoEBBs (SecurityParam 3) TestConfig
            { numCoreNodes = ncn4
            , numSlots     = NumSlots 72
            , nodeJoinPlan = trivialNodeJoinPlan ncn4
            , nodeRestarts = NodeRestarts (Map.fromList [(SlotNo 59,Map.fromList [(CoreNodeId 3,NodeRekey)])])
            , nodeTopology = meshNodeTopology ncn4
            , slotLength   = defaultSlotLength
            , initSeed     = Seed (17364222041321661634,8266509462575908621,10410472349244348261,9332246846568887555,6178891282750652496)
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
          prop_simple_real_pbft_convergence ProduceEBBs (SecurityParam 2) TestConfig
            { numCoreNodes = ncn3
            , numSlots     = NumSlots 84
            , nodeJoinPlan = NodeJoinPlan (Map.fromList [(CoreNodeId 0,SlotNo {unSlotNo = 1}),(CoreNodeId 1,SlotNo {unSlotNo = 1}),(CoreNodeId 2,SlotNo {unSlotNo = 58})])
            , nodeRestarts = NodeRestarts (Map.fromList [(SlotNo {unSlotNo = 58},Map.fromList [(CoreNodeId 2,NodeRekey)])])
            , nodeTopology = meshNodeTopology ncn3
            , slotLength   = defaultSlotLength
            , initSeed     = Seed {getSeed = (15151717355257504044,5938503171282920606,17557892055617026469,2625071732074633531,737988411488637670)}
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
          prop_simple_real_pbft_convergence ProduceEBBs (SecurityParam 4) TestConfig
            { numCoreNodes = NumCoreNodes 3
            , numSlots     = NumSlots 96
            , nodeJoinPlan = NodeJoinPlan $ Map.fromList [(CoreNodeId 0,SlotNo 0),(CoreNodeId 1,SlotNo 0),(CoreNodeId 2,SlotNo 83)]
            , nodeRestarts = NodeRestarts $ Map.fromList [(SlotNo 83,Map.fromList [(CoreNodeId 2,NodeRekey)])]
            , nodeTopology =    --   1 <-> 0 <-> 2
                NodeTopology $ Map.fromList [(CoreNodeId 0,Set.fromList []),(CoreNodeId 1,Set.fromList [CoreNodeId 0]),(CoreNodeId 2,Set.fromList [CoreNodeId 0])]
            , slotLength   = defaultSlotLength
            , initSeed     = Seed {getSeed = (6137414258840919713,13743611065535662953,11200456599001708481,15059765168210441725,7592004320108020587)}
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
          -- The failure was: PBftNotGenesisDelegate in Slot 49. It disappeared
          -- with the mesh topology, which usually means subtle timings are
          -- involved, unfortunately.
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
          prop_simple_real_pbft_convergence ProduceEBBs (SecurityParam 2) TestConfig
            { numCoreNodes = ncn5
            , numSlots     = NumSlots 50
            , nodeJoinPlan = NodeJoinPlan (Map.fromList [(CoreNodeId 0,SlotNo {unSlotNo = 0}),(CoreNodeId 1,SlotNo {unSlotNo = 0}),(CoreNodeId 2,SlotNo {unSlotNo = 0}),(CoreNodeId 3,SlotNo {unSlotNo = 37}),(CoreNodeId 4,SlotNo {unSlotNo = 37})])
            , nodeRestarts = NodeRestarts (Map.fromList [(SlotNo {unSlotNo = 37},Map.fromList [(CoreNodeId 4,NodeRekey)])])
            , nodeTopology = -- 3 <-> {0,1,2} <-> 4
                NodeTopology (Map.fromList [(CoreNodeId 0,Set.fromList []),(CoreNodeId 1,Set.fromList [CoreNodeId 0]),(CoreNodeId 2,Set.fromList [CoreNodeId 0, CoreNodeId 1]),(CoreNodeId 3,Set.fromList [CoreNodeId 0,CoreNodeId 1,CoreNodeId 2]),(CoreNodeId 4,Set.fromList [CoreNodeId 0,CoreNodeId 1,CoreNodeId 2])])
            , slotLength   = defaultSlotLength
            , initSeed     = Seed {getSeed = (13428626417421372024,5113871799759534838,13943132470772613446,18226529569527889118,4309403968134095151)}
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
      prop_simple_real_pbft_convergence
       NoEBBs
       SecurityParam {maxRollbacks = 2}
       TestConfig
         { numCoreNodes = ncn
         , numSlots     = NumSlots 41
         , nodeJoinPlan = trivialNodeJoinPlan ncn
         , nodeRestarts = NodeRestarts $ Map.singleton (SlotNo 30) $ Map.singleton (CoreNodeId 2) NodeRekey
         , nodeTopology = meshNodeTopology ncn
         , slotLength   = defaultSlotLength
         , initSeed     = Seed (368401128646137767,7989071211759985580,4921478144180472393,11759221144888418607,7602439127562955319)
         }
    , testProperty "delayed message corner case" $
          once $
          let ncn = NumCoreNodes 2 in
          prop_simple_real_pbft_convergence NoEBBs (SecurityParam 7) TestConfig
            { numCoreNodes = ncn
            , numSlots     = NumSlots 10
            , nodeJoinPlan = NodeJoinPlan (Map.fromList [(CoreNodeId 0,SlotNo {unSlotNo = 0}),(CoreNodeId 1,SlotNo {unSlotNo = 1})])
            , nodeRestarts = noRestarts
            , nodeTopology = meshNodeTopology ncn
            , slotLength   = defaultSlotLength
            , initSeed     = Seed (11954171112552902178,1213614443200450055,13600682863893184545,15433529895532611662,2464843772450023204)
            }
    , testProperty "mkUpdateLabels anticipates instant confirmation" $
          -- caught a bug in 'mkUpdateLabels' where it didn't anticipate that
          -- node c0 can confirm the proposal as soon as it joins when quorum
          -- == 1
          let ncn = NumCoreNodes 3 in
          prop_simple_real_pbft_convergence NoEBBs (SecurityParam 9) TestConfig
            { numCoreNodes = ncn
            , numSlots     = NumSlots 1
            , nodeJoinPlan = trivialNodeJoinPlan ncn
            , nodeRestarts = noRestarts
            , nodeTopology = meshNodeTopology ncn
            , slotLength   = defaultSlotLength
            , initSeed     = Seed {getSeed = (560784040296064078,562654861307142039,14390345921802859256,6074698800134646104,12960749422959162150)}
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
          prop_simple_real_pbft_convergence NoEBBs (SecurityParam 8) TestConfig
            { numCoreNodes = ncn
            , numSlots     = NumSlots 2
            , nodeJoinPlan = trivialNodeJoinPlan ncn
            , nodeRestarts = noRestarts
            , nodeTopology = meshNodeTopology ncn
            , slotLength   = defaultSlotLength
            , initSeed     = Seed {getSeed = (17661772013144211573,3458753765485439359,3510665480596920798,18073896085829422849,10200170902568172302)}
            }
    , testProperty "track proposals even when c0 is not the first to lead" $
          -- requires prompt and accurate vote tracking when c0 is not the
          -- first node to lead
          --
          -- The necessary promptness trigger in this case is the arrival of
          -- the proposal transaction.
          let ncn = NumCoreNodes 4 in
          prop_simple_real_pbft_convergence NoEBBs (SecurityParam 5) TestConfig
            { numCoreNodes = ncn
            , numSlots     = NumSlots 5
            , nodeJoinPlan = NodeJoinPlan $ Map.fromList
              [ (CoreNodeId 0, SlotNo 2)
              , (CoreNodeId 1, SlotNo 3)
              , (CoreNodeId 2, SlotNo 4)
              , (CoreNodeId 3, SlotNo 4)
              ]
            , nodeRestarts = noRestarts
            , nodeTopology = meshNodeTopology ncn
            , slotLength   = defaultSlotLength
            , initSeed     = Seed {getSeed = (7536539674426109099,5947274896735415773,14396421290275890646,8359457880945605675,13921484090802881569)}
            }
    , testProperty "cardano-ledger checks for proposal confirmation before it checks for expiry" $
          -- must check for quorum before checking for expiration
          let ncn = NumCoreNodes 5 in
          prop_simple_real_pbft_convergence NoEBBs SecurityParam {maxRollbacks = 10} TestConfig
            { numCoreNodes = ncn
            , numSlots     = NumSlots 12
            , nodeJoinPlan = NodeJoinPlan $ Map.fromList
              [ (CoreNodeId 0, SlotNo 0)
              , (CoreNodeId 1, SlotNo 0)
              , (CoreNodeId 2, SlotNo 10)
              , (CoreNodeId 3, SlotNo 10)
              , (CoreNodeId 4, SlotNo 10)
              ]
            , nodeRestarts = noRestarts
            , nodeTopology = meshNodeTopology ncn
            , slotLength   = defaultSlotLength
            , initSeed     = Seed {getSeed = (2578884099630273185,16934506387441904343,18333130054045336554,17133864958166263786,3231825379390681058)}
            }
    , testProperty "repropose an expired proposal" $
          -- the proposal expires in slot 10, but then c0 reintroduces it in
          -- slot 11 and it is eventually confirmed
          let ncn = NumCoreNodes 5 in
          prop_simple_real_pbft_convergence NoEBBs SecurityParam {maxRollbacks = 10} TestConfig
            { numCoreNodes = ncn
            , numSlots     = NumSlots 17
            , nodeJoinPlan = NodeJoinPlan $ Map.fromList
              [(CoreNodeId 0, SlotNo 0)
              ,(CoreNodeId 1, SlotNo 10)
              ,(CoreNodeId 2, SlotNo 11)
              ,(CoreNodeId 3, SlotNo 11)
              ,(CoreNodeId 4, SlotNo 16)
              ]
            , nodeRestarts = noRestarts
            , nodeTopology = meshNodeTopology ncn
            , slotLength   = defaultSlotLength
            , initSeed     = Seed {getSeed = (306806859316465898,5351335255935493133,6240542044036351784,5824248410373935607,16492982022780410836)}
            }
    , testProperty "only expect EBBs if the reference simulator does" $
          -- In this repro, block in the 20th slot is wasted since c2 just
          -- joined. As a result, the final chains won't include that EBB.
          let ncn = NumCoreNodes 3 in
          prop_simple_real_pbft_convergence ProduceEBBs SecurityParam {maxRollbacks = 2} TestConfig
            { numCoreNodes = ncn
            , numSlots     = NumSlots 21
            , nodeJoinPlan = NodeJoinPlan $ Map.fromList
              [ (CoreNodeId 0,SlotNo {unSlotNo = 0})
              , (CoreNodeId 1,SlotNo {unSlotNo = 0})
              , (CoreNodeId 2,SlotNo {unSlotNo = 20})
              ]
            , nodeRestarts = noRestarts
            , nodeTopology = meshNodeTopology ncn
            , slotLength   = defaultSlotLength
            , initSeed     = Seed {getSeed = (5875984841520223242,5307155813931649482,9880810077012492572,1841667196263253753,11730891841989901381)}
            }
    , testProperty "only check updates for mesh topologies" $
          -- This repro exercises
          -- 'Test.ThreadNet.RealPBFT.TrackUpdates.checkTopo'.
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
          -- endorsement; see Issue 749 in cardano-ledger).
          --
          -- "Test.ThreadNet.RealPBFT.TrackUpdates" does not otherwise
          -- correctly anticipate such races, so it makes no requirement for
          -- non-mesh topologies.
          prop_simple_real_pbft_convergence NoEBBs SecurityParam {maxRollbacks = 10} TestConfig
            { numCoreNodes = NumCoreNodes 5
            , numSlots     = NumSlots 13
            , nodeJoinPlan = NodeJoinPlan $ Map.fromList
              [ (CoreNodeId 0, SlotNo 0)
              , (CoreNodeId 1, SlotNo 11)
              , (CoreNodeId 2, SlotNo 11)
              , (CoreNodeId 3, SlotNo 11)
              , (CoreNodeId 4, SlotNo 11)
              ]
            , nodeRestarts = noRestarts
            , nodeTopology = NodeTopology $ Map.fromList
                -- mesh except for 0 <-> 2
              [ (CoreNodeId 0, Set.fromList [])
              , (CoreNodeId 1, Set.fromList [CoreNodeId 0])
              , (CoreNodeId 2, Set.fromList [CoreNodeId 1])
              , (CoreNodeId 3, Set.fromList [CoreNodeId 0, CoreNodeId 1, CoreNodeId 2])
              , (CoreNodeId 4, Set.fromList [CoreNodeId 0, CoreNodeId 1, CoreNodeId 2, CoreNodeId 3])
              ]
            , slotLength   = defaultSlotLength
            , initSeed     = Seed {getSeed = (8051309618816278461,2819388114162022931,16483461939305597384,11191453672390756304,8021847551866528244)}
            }
    , testProperty "simple convergence" $
          \produceEBBs ->
          -- TODO k > 1 as a workaround for Issue #1511.
          --
          forAll (SecurityParam <$> elements [2 .. 10])
            $ \k ->
          forAllShrink
              (genRealPBFTTestConfig k)
              shrinkRealPBFTTestConfig
            $ \testConfig ->
          prop_simple_real_pbft_convergence produceEBBs k testConfig
    ]
  where
    defaultSlotLength :: SlotLength
    defaultSlotLength = SlotLength 1

prop_deterministicPlan :: PBftParams -> NumSlots -> NumCoreNodes -> Property
prop_deterministicPlan params numSlots numCoreNodes =
    property $ case Ref.simulate params njp numSlots of
      Ref.Forked{}         -> False
      Ref.Outcomes{}       -> True
      Ref.Nondeterministic -> False
  where
    njp = trivialNodeJoinPlan numCoreNodes

prop_setup_coreNodeId ::
     NumCoreNodes
  -> CoreNodeId
  -> Property
prop_setup_coreNodeId numCoreNodes coreNodeId =
    case pbftIsLeader $ configConsensus $ pInfoConfig protInfo of
      PBftIsALeader isLeader ->
          coreNodeId === pbftCoreNodeId isLeader
      _ ->
          counterexample "mkProtocolRealPBFT did not use protocolInfoByron" $
          property False
  where
    protInfo :: ProtocolInfo ByronBlock
    protInfo = mkProtocolRealPBFT params coreNodeId genesisConfig genesisSecrets

    params :: PBftParams
    params = realPBftParams dummyK numCoreNodes
    dummyK = SecurityParam 10   -- not really used

    genesisConfig  :: Genesis.Config
    genesisSecrets :: Genesis.GeneratedSecrets
    (genesisConfig, genesisSecrets) = generateGenesisConfig params

expectedBlockRejection
  :: SecurityParam
  -> NumCoreNodes
  -> NodeRestarts
  -> BlockRejection ByronBlock
  -> Bool
expectedBlockRejection
  k numCoreNodes@(NumCoreNodes nn) (NodeRestarts nrs) BlockRejection
  { brBlockSlot = s
  , brReason    = err
  , brRejector  = CoreId (CoreNodeId i)
  }
  | ownBlock                   = case err of
    ExtValidationErrorHeader
      (HeaderProtocolError PBftExceededSignThreshold{}) ->
        -- TODO validate this against Ref implementation?
        True

    ExtValidationErrorHeader
      (HeaderProtocolError PBftNotGenesisDelegate{}) ->
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
    _                             -> False
  where
    -- Because of round-robin and the fact that the id divides slot, we know
    -- the node lead but rejected its own block. This is the only case we
    -- expect. (Rejecting its own block also prevents the node from propagating
    -- that block.)
    ownBlock = i == mod (unSlotNo s) nn
expectedBlockRejection _ _ _ _ = False

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

prop_simple_real_pbft_convergence :: ProduceEBBs
                                  -> SecurityParam
                                  -> TestConfig
                                  -> Property
prop_simple_real_pbft_convergence produceEBBs k
  testConfig@TestConfig
    { numCoreNodes
    , numSlots
    , nodeJoinPlan
    , nodeTopology
    , nodeRestarts
    , initSeed
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
      { pgaBlockProperty          = const $ property True
      , pgaCountTxs               = Byron.countByronGenTxs
      , pgaExpectedBlockRejection =
          expectedBlockRejection k numCoreNodes nodeRestarts
      , pgaFirstBlockNo           = 1
      , pgaFixedMaxForkLength     =
          Just $ NumBlocks $ case refResult of
            Ref.Forked{} -> 1
            _            -> 0
      , pgaFixedSchedule          =
          Just $ roundRobinLeaderSchedule numCoreNodes numSlots
      , pgaSecurityParam          = k
      , pgaTestConfig             = testConfig
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
    testOutput =
        runTestNetwork testConfig epochSize TestConfigBlock
            { forgeEbbEnv = case produceEBBs of
                NoEBBs      -> Nothing
                ProduceEBBs -> Just byronForgeEbbEnv
            , nodeInfo = \nid ->
                mkProtocolRealPBftAndHardForkTxs
                  params nid genesisConfig genesisSecrets
            , rekeying = Just Rekeying
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
                  in
                  Stream.nubOrdBy prj acc0 $
                  withSeed initSeed $   -- seems fine to reuse seed for this
                  sequence $ let ms = Crypto.genKeyDSIGN Stream.:< ms in ms
              }
            , txGenExtra = ()
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
    params = realPBftParams k numCoreNodes

    genesisConfig  :: Genesis.Config
    genesisSecrets :: Genesis.GeneratedSecrets
    (genesisConfig, genesisSecrets) = generateGenesisConfig params

byronForgeEbbEnv :: ForgeEbbEnv ByronBlock
byronForgeEbbEnv = ForgeEbbEnv
    { forgeEBB = Byron.forgeEBB
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

    actual   = mapMaybe (nodeIsEBB . getHeader) $ Chain.toOldestFirst c

{-------------------------------------------------------------------------------
  Generating the genesis configuration
-------------------------------------------------------------------------------}

realPBftParams :: SecurityParam -> NumCoreNodes -> PBftParams
realPBftParams paramK numCoreNodes = PBftParams
  { pbftNumNodes           = numCoreNodes
  , pbftSecurityParam      = paramK
  , pbftSignatureThreshold = (1 / n) + (1 / k) + epsilon
    -- crucially: @floor (k * t) >= ceil (k / n)@
  }
    where
      epsilon = 1/10000   -- avoid problematic floating point round-off

      n :: Num a => a
      n = fromIntegral x where NumCoreNodes x = numCoreNodes

      k :: Num a => a
      k = fromIntegral x where SecurityParam x = paramK

pbftSlotLength :: SlotLength
pbftSlotLength = slotLengthFromSec 20

-- Instead of using 'Dummy.dummyConfig', which hard codes the number of rich
-- men (= CoreNodes for us) to 4, we generate a dummy config with the given
-- number of rich men.
generateGenesisConfig :: PBftParams -> (Genesis.Config, Genesis.GeneratedSecrets)
generateGenesisConfig params =
    either (error . show) id $
      Crypto.deterministic "this is fake entropy for testing" $
        runExceptT $
          Genesis.generateGenesisConfigWithEntropy startTime spec
  where
    startTime = UTCTime (ModifiedJulianDay 0) 0
    PBftParams{pbftNumNodes, pbftSecurityParam} = params
    NumCoreNodes numCoreNodes = pbftNumNodes

    spec :: Genesis.GenesisSpec
    spec = Dummy.dummyGenesisSpec
      { Genesis.gsInitializer = Dummy.dummyGenesisInitializer
        { Genesis.giTestBalance =
            (Genesis.giTestBalance Dummy.dummyGenesisInitializer)
              -- The nodes are the richmen
              { Genesis.tboRichmen = fromIntegral numCoreNodes }
        }
      , Genesis.gsK = Common.BlockCount $ maxRollbacks pbftSecurityParam
      }

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
genRealPBFTNodeJoinPlan :: PBftParams -> NumSlots -> Gen NodeJoinPlan
genRealPBFTNodeJoinPlan params numSlots@(NumSlots t)
  | n < 0 || t < 1 = error $ "Cannot generate RealPBFT NodeJoinPlan: "
    ++ show (params, numSlots)
  | otherwise      =
    go (NodeJoinPlan Map.empty) Ref.emptyState
      `suchThat` (\njp -> Ref.definitelyEnoughBlocks params $
                          Ref.simulate params njp numSlots)

        -- This suchThat might loop a few times, but it should always
        -- eventually succeed, since the plan where all nodes join immediately
        -- satisfies it.
        --
        -- In a run of 7000 successful RealPBFT tests, this 'suchThat' retried:
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
                "Cannot find viable RealPBFT NodeJoinPlan: " ++
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

genRealPBFTTestConfig :: SecurityParam -> Gen TestConfig
genRealPBFTTestConfig k = do
    numCoreNodes <- arbitrary
    numSlots     <- arbitrary

    let params = realPBftParams k numCoreNodes
    nodeJoinPlan <- genRealPBFTNodeJoinPlan params numSlots
    nodeTopology <- genNodeTopology numCoreNodes
    nodeRestarts <- genNodeRestarts nodeJoinPlan numSlots >>=
                    genNodeRekeys params nodeJoinPlan nodeTopology numSlots

    initSeed <- arbitrary

    pure TestConfig
      { nodeJoinPlan
      , nodeRestarts
      , nodeTopology
      , numCoreNodes
      , numSlots
      , slotLength = pbftSlotLength
      , initSeed
      }

shrinkRealPBFTTestConfig :: TestConfig -> [TestConfig]
shrinkRealPBFTTestConfig  = shrinkTestConfigSlotsOnly
  -- NOTE 'shrink' at type 'NodePlanJoin' never increases inter-join delays
  --
  -- and we're neither shrinking the security parameter nor /increasing/ the
  -- number of slots, so the invariant established by 'genRealPBFTNodeJoinPlan'
  -- will be preserved

-- | Shrink, including the number of slots but not number of nodes
--
shrinkTestConfigSlotsOnly :: TestConfig -> [TestConfig]
shrinkTestConfigSlotsOnly TestConfig
  { numCoreNodes
  , numSlots
  , nodeJoinPlan
  , nodeRestarts
  , nodeTopology
  , slotLength
  , initSeed
  } =
    dropId $
    [ TestConfig
        { nodeJoinPlan = p'
        , nodeRestarts = r'
        , nodeTopology = top'
        , numCoreNodes
        , numSlots     = t'
        , slotLength   = len'
        , initSeed
        }
    | t'            <- andId shrink numSlots
    , let adjustedP  = truncateNodeJoinPlan nodeJoinPlan numCoreNodes (numSlots, t')
    , let adjustedR  = truncateNodeRestarts nodeRestarts t'
    , p'            <- andId shrinkNodeJoinPlan adjustedP
    , r'            <- andId shrinkNodeRestarts adjustedR
    , top'          <- andId shrinkNodeTopology nodeTopology
    , len'          <- andId shrink slotLength
    ]

-- | Possibly promote some 'NodeRestart's to 'NodeRekey's
--
-- POSTCONDITION No node will rekey multiple times in a single epoch.
-- (Ouroboros allows at most one delegation per epoch, while each rekey and
-- also genesis itself all count as a delegation.)
--
-- POSTCONDITION Each rekey takes at least 2k slots, and the node can't lead
-- until it's finished. Therefore, at most one node will be rekeying at a time,
-- since otherwise its inability to lead may spoil the invariants established
-- by 'genRealPBFTNodeJoinPlan'.
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
  :: Genesis.Config
  -> Genesis.GeneratedSecrets
  -> ProtocolInfo ByronBlock
  -> EpochNo
  -> Crypto.SignKeyDSIGN Crypto.ByronDSIGN
  -> Maybe (TestNodeInitialization ByronBlock)
mkRekeyUpd genesisConfig genesisSecrets pInfo eno newSK =
  case pbftIsLeader configConsensus of
    PBftIsNotALeader       -> Nothing
    PBftIsALeader isLeader ->
      let PBftIsLeader{pbftCoreNodeId} = isLeader
          genSK = genesisSecretFor genesisConfig genesisSecrets pbftCoreNodeId
          isLeader' = updSignKey genSK configBlock isLeader (coerce eno) newSK
          pInfo' = pInfo
            { pInfoConfig = TopLevelConfig {
                  configConsensus = configConsensus
                    { pbftIsLeader = PBftIsALeader isLeader'
                    }
                , configLedger = configLedger
                , configBlock  = configBlock
                }
            }

          PBftIsLeader{pbftDlgCert} = isLeader'
      in Just TestNodeInitialization
        { tniCrucialTxs = [dlgTx pbftDlgCert]
        , tniProtocolInfo = pInfo'
        }
  where
    ProtocolInfo{pInfoConfig = TopLevelConfig{ configConsensus
                                             , configLedger
                                             , configBlock
                                             }} = pInfo

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

-- | Overwrite the 'PBftIsLeader''s operational key and delegation certificate
--
updSignKey
  :: Crypto.SignKeyDSIGN Crypto.ByronDSIGN
  -> BlockConfig ByronBlock
  -> PBftIsLeader PBftByronCrypto
  -> EpochNumber
  -> Crypto.SignKeyDSIGN Crypto.ByronDSIGN
  -> PBftIsLeader PBftByronCrypto
updSignKey genSK extCfg isLeader eno newSK = isLeader
    { pbftDlgCert = newCert
    , pbftSignKey = newSK
    }
  where
    newCert =
        Delegation.signCertificate
            (Byron.byronProtocolMagicId extCfg)
            (Crypto.toVerification sk')
            eno
            (Crypto.noPassSafeSigner gsk')
      where
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

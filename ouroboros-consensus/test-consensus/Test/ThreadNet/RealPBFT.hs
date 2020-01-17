{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.ThreadNet.RealPBFT (
    tests
  ) where

import           Data.Coerce (coerce)
import           Data.Foldable (find)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Set as Set
import           Data.Time (Day (..), UTCTime (..))
import           Data.Word (Word64)
import           GHC.Stack (HasCallStack)

import           Numeric.Search.Range (searchFromTo)
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Ouroboros.Network.Block (SlotNo (..))
import           Ouroboros.Network.MockChain.Chain (Chain)
import qualified Ouroboros.Network.MockChain.Chain as Chain

import           Ouroboros.Consensus.Block (BlockProtocol, getHeader)
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.BlockchainTime.Mock
import qualified Ouroboros.Consensus.Crypto.DSIGN.Cardano as Crypto
import           Ouroboros.Consensus.Ledger.Byron (ByronBlock)
import qualified Ouroboros.Consensus.Ledger.Byron as Byron
import           Ouroboros.Consensus.Ledger.Extended (ExtValidationError (..))
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Node.ProtocolInfo.Byron (plcCoreNodeId)
import           Ouroboros.Consensus.Node.Run.Abstract (nodeIsEBB)
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Protocol
import qualified Ouroboros.Consensus.Protocol.PBFT.Crypto as Crypto
import           Ouroboros.Consensus.Util.Condense (condense)
import           Ouroboros.Consensus.Util.Random

import           Ouroboros.Storage.Common (EpochNo (..))

import qualified Cardano.Binary
import qualified Cardano.Chain.Common as Common
import qualified Cardano.Chain.Delegation as Delegation
import qualified Cardano.Chain.Genesis as Genesis
import           Cardano.Chain.ProtocolConstants (kEpochSlots)
import           Cardano.Chain.Slotting (EpochNumber (..), unEpochSlots)
import qualified Cardano.Chain.Update as Update
import qualified Cardano.Crypto as Crypto
import qualified Cardano.Crypto.DSIGN as Crypto
import qualified Test.Cardano.Chain.Genesis.Dummy as Dummy

import           Test.ThreadNet.General
import           Test.ThreadNet.Network (NodeOutput (..))
import qualified Test.ThreadNet.Ref.RealPBFT as Ref
import           Test.ThreadNet.Util
import           Test.ThreadNet.Util.NodeJoinPlan
import           Test.ThreadNet.Util.NodeRestarts
import           Test.ThreadNet.Util.NodeTopology

import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Shrink (andId, dropId)
import qualified Test.Util.Stream as Stream

tests :: TestTree
tests = testGroup "RealPBFT" $
    [ testProperty "trivial join plan is considered deterministic" $
          prop_deterministicPlan
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
            , slotLengths = defaultSlotLengths
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
            , slotLengths  = defaultSlotLengths
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
            , slotLengths  = defaultSlotLengths
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
            , slotLengths  = defaultSlotLengths
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
            , slotLengths  = defaultSlotLengths
            , initSeed     = Seed (11044330969750026700,14522662956180538128,9026549867550077426,3049168255170604478,643621447671665184)
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
            , slotLengths  = defaultSlotLengths
            , initSeed     = seed
            }
    , testProperty "simple convergence" $
          \produceEBBs ->
          forAll (SecurityParam <$> elements [5, 10])
            $ \k ->
          forAllShrink
              (genRealPBFTTestConfig k)
              shrinkRealPBFTTestConfig
            $ \testConfig ->
          prop_simple_real_pbft_convergence produceEBBs k testConfig
    ]
  where
    defaultSlotLengths :: SlotLengths
    defaultSlotLengths = singletonSlotLengths (SlotLength 1)

prop_deterministicPlan :: NumSlots -> NumCoreNodes -> Property
prop_deterministicPlan numSlots numCoreNodes =
  property $
  Ref.deterministicPlan numSlots (trivialNodeJoinPlan numCoreNodes)

prop_setup_coreNodeId ::
     NumCoreNodes
  -> CoreNodeId
  -> Property
prop_setup_coreNodeId numCoreNodes coreNodeId =
    case mkProtocolRealPBFT params coreNodeId genesisConfig genesisSecrets of
      ProtocolRealPBFT _cfg _th _pv _swv (Just plc) ->
          coreNodeId === plcCoreNodeId plc
      _ ->
          counterexample "mkProtocolRealPBFT did not use ProtocolRealPBFT" $
          property False
  where
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
  , brRejector  = CoreId i
  }
  | ownBlock                   = case err of
    ExtValidationErrorOuroboros
      PBftExceededSignThreshold{} -> True   -- TODO validate this against Ref
                                            -- implementation?
    ExtValidationErrorOuroboros
      PBftNotGenesisDelegate{}    ->
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
    ownBlock = fromIntegral i == mod (unSlotNo s) (fromIntegral nn)
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
    SlotNo $ rekeySlot + fromIntegral n + 2 * k

prop_simple_real_pbft_convergence :: ProduceEBBs
                                  -> SecurityParam
                                  -> TestConfig
                                  -> Property
prop_simple_real_pbft_convergence produceEBBs k
  testConfig@TestConfig{numCoreNodes, numSlots, nodeRestarts, initSeed} =
    tabulate "produce EBBs" [show produceEBBs] $
    prop_general k
        testConfig
        (Just $ roundRobinLeaderSchedule numCoreNodes numSlots)
        (expectedBlockRejection k numCoreNodes nodeRestarts)
        testOutput .&&.
    not (all Chain.null finalChains) .&&.
    conjoin (map (hasAllEBBs k numSlots produceEBBs) finalChains)
  where
    testOutput =
        runTestNetwork testConfig TestConfigBlock
            { forgeEBB = case produceEBBs of
                NoEBBs      -> Nothing
                ProduceEBBs -> Just Byron.forgeEBB
            , nodeInfo = \nid -> protocolInfo $
                mkProtocolRealPBFT params nid genesisConfig genesisSecrets
            , rekeying = Just Rekeying
              { rekeyUpd      = mkRekeyUpd genesisConfig genesisSecrets
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
            }

    finalChains :: [Chain ByronBlock]
    finalChains = Map.elems $ nodeOutputFinalChain <$> testOutputNodes testOutput

    params :: PBftParams
    params = realPBftParams k numCoreNodes

    genesisConfig  :: Genesis.Config
    genesisSecrets :: Genesis.GeneratedSecrets
    (genesisConfig, genesisSecrets) = generateGenesisConfig params

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

instance Arbitrary ProduceEBBs where
  arbitrary = elements [NoEBBs, ProduceEBBs]
  shrink NoEBBs      = []
  shrink ProduceEBBs = [NoEBBs]

hasAllEBBs :: SecurityParam
           -> NumSlots
           -> ProduceEBBs
           -> Chain ByronBlock
           -> Property
hasAllEBBs k (NumSlots t) produceEBBs c =
    counterexample ("Missing or unexpected EBBs in " <> condense c) $
    actual === expected
  where
    expected :: [EpochNo]
    expected = case produceEBBs of
      NoEBBs      -> [0]
      ProduceEBBs -> coerce [0 .. hi]
        where
          hi :: Word64
          hi = if t < 1 then 0 else fromIntegral (t - 1) `div` denom
          denom = unEpochSlots $ kEpochSlots $ coerce k

    actual   = mapMaybe (nodeIsEBB . getHeader) $ Chain.toOldestFirst c

mkProtocolRealPBFT :: HasCallStack
                   => PBftParams
                   -> CoreNodeId
                   -> Genesis.Config
                   -> Genesis.GeneratedSecrets
                   -> Protocol ByronBlock
mkProtocolRealPBFT params (CoreNodeId i)
                   genesisConfig genesisSecrets =
    ProtocolRealPBFT
      genesisConfig
      (Just $ PBftSignatureThreshold pbftSignatureThreshold)
      (Update.ProtocolVersion 1 0 0)
      (Update.SoftwareVersion (Update.ApplicationName "Cardano Test") 2)
      (Just leaderCredentials)
  where
    leaderCredentials :: PBftLeaderCredentials
    leaderCredentials = either (error . show) id $
        mkPBftLeaderCredentials
          genesisConfig
          dlgKey
          dlgCert

    PBftParams{pbftSignatureThreshold} = params

    dlgKey :: Crypto.SigningKey
    dlgKey = fromMaybe (error "dlgKey") $
       find (\sec -> Delegation.delegateVK dlgCert == Crypto.toVerification sec)
            $ Genesis.gsRichSecrets genesisSecrets

    dlgCert :: Delegation.Certificate
    dlgCert = snd $ Map.toAscList dlgMap !! i

    dlgMap :: Map Common.KeyHash Delegation.Certificate
    dlgMap = Genesis.unGenesisDelegation
           $ Genesis.gdHeavyDelegation
           $ Genesis.configGenesisData genesisConfig

{-------------------------------------------------------------------------------
  Generating the genesis configuration
-------------------------------------------------------------------------------}

realPBftParams :: SecurityParam -> NumCoreNodes -> PBftParams
realPBftParams paramK numCoreNodes = PBftParams
  { pbftNumNodes           = n
  , pbftSecurityParam      = paramK
  , pbftSignatureThreshold = (1 / n) + (1 / k) + epsilon
    -- crucially: @floor (k * t) >= ceil (k / n)@
  , pbftSlotLength         = slotLengthFromSec 20
  }
    where
      epsilon = 1/10000   -- avoid problematic floating point round-off

      n :: Num a => a
      n = fromIntegral x where NumCoreNodes x = numCoreNodes

      k :: Num a => a
      k = fromIntegral x where SecurityParam x = paramK

-- Instead of using 'Dummy.dummyConfig', which hard codes the number of rich
-- men (= CoreNodes for us) to 4, we generate a dummy config with the given
-- number of rich men.
generateGenesisConfig :: PBftParams -> (Genesis.Config, Genesis.GeneratedSecrets)
generateGenesisConfig params =
    either (error . show) id $ Genesis.generateGenesisConfig startTime spec
  where
    startTime = UTCTime (ModifiedJulianDay 0) 0
    PBftParams{pbftNumNodes, pbftSecurityParam} = params

    spec :: Genesis.GenesisSpec
    spec = Dummy.dummyGenesisSpec
      { Genesis.gsInitializer = Dummy.dummyGenesisInitializer
        { Genesis.giTestBalance =
            (Genesis.giTestBalance Dummy.dummyGenesisInitializer)
              -- The nodes are the richmen
              { Genesis.tboRichmen = fromIntegral pbftNumNodes }
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
      `suchThat` Ref.deterministicPlan numSlots
        -- This suchThat might loop a few times, but it should always
        -- eventually succeed, since the plan where all nodes join immediately
        -- satisifies it.
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
    n                        = fromIntegral pbftNumNodes

    lastSlot = SlotNo $ fromIntegral $ t - 1

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
            -- viable, which assumes that the invariant would be satisified if
            -- all nodes join in slot 0. For uninterrupted round-robin, that
            -- merely requires @n * floor (k * t) >= k@. (TODO Does that
            -- *always* suffice?)
        let check s' =
                Ref.viable params lastSlot
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
                show (nodeJoinPlan, st)

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
    nodeRestarts <- genNodeRestarts nodeJoinPlan numSlots >>=
                    genNodeRekeys params nodeJoinPlan numSlots
    nodeTopology <- genNodeTopology numCoreNodes

    initSeed <- arbitrary

    pure TestConfig
      { nodeJoinPlan
      , nodeRestarts
      , nodeTopology
      , numCoreNodes
      , numSlots
      , slotLengths = singletonSlotLengths (pbftSlotLength params)
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
  , slotLengths
  , initSeed
  } =
    dropId $
    [ TestConfig
        { nodeJoinPlan = p'
        , nodeRestarts = r'
        , nodeTopology = top'
        , numCoreNodes
        , numSlots     = t'
        , slotLengths  = ls'
        , initSeed
        }
    | t'            <- andId shrink numSlots
    , let adjustedP  = truncateNodeJoinPlan nodeJoinPlan numCoreNodes (numSlots, t')
    , let adjustedR  = truncateNodeRestarts nodeRestarts t'
    , p'            <- andId shrinkNodeJoinPlan adjustedP
    , r'            <- andId shrinkNodeRestarts adjustedR
    , top'          <- andId shrinkNodeTopology nodeTopology
    , ls'           <- andId shrink slotLengths
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
  -> NumSlots
  -> NodeRestarts
  -> Gen NodeRestarts
genNodeRekeys params (NodeJoinPlan njp) numSlots@(NumSlots t)
  nodeRestarts@(NodeRestarts nrs)
  | t <= 0    = pure nodeRestarts
  | otherwise =
    -- The necessary conditions are pretty rare, so favor adding a 'NodeRekey'
    -- when we can. But not always.
    (\x -> frequency [(2, pure nodeRestarts), (8, x)]) $
    -- TODO rekey nodes other than the last
    -- TODO rekey more than one node
    -- TODO rekey a node in a slot other than its join slot
    case Map.lookupMax njp of
      Just (nid, jslot)
            -- last node joins after first epoch, ...
          | jslot >= beginSecondEpoch
            -- ... and could instead join unproblematically at the latest time
            -- the delegation certificate would mature
          , latestPossibleDlgMaturation pbftSecurityParam numCoreNodes jslot
              <= lastSlot
          , let nodeJoinPlan' =
                  NodeJoinPlan $ Map.insert nid (jslot + twoK) njp
          , Ref.viable params lastSlot nodeJoinPlan' Ref.emptyState
          , Ref.deterministicPlan numSlots nodeJoinPlan'
          -> pure $ NodeRestarts $
             -- We discard any 'NodeRestart's also scheduled for this slot.
             -- 'NodeRestart's are less interesting, so it's fine.
             --
             -- TODO retain those coincident node restarts as long as they
             -- don't include every other node; that risks forgetting some
             -- relevant blocks.
             Map.insert jslot (Map.singleton nid NodeRekey) nrs
      _ -> pure nodeRestarts
  where
    PBftParams{pbftSecurityParam} = params
    k = maxRollbacks pbftSecurityParam
    lastSlot = SlotNo $ fromIntegral $ t - 1
    numCoreNodes = NumCoreNodes $ Map.size njp

    twoK             = SlotNo $ 2 * k
    beginSecondEpoch = SlotNo $ 10 * k   -- c.f. Genesis.configEpochSlots

{-------------------------------------------------------------------------------
  Updating operational keys
-------------------------------------------------------------------------------}

-- | Overwrite the 'ProtocolInfo''s operational key, if any, and provide a
-- transaction for its new delegation certificate
--
mkRekeyUpd
  :: (BlockProtocol b ~ PBft ByronConfig PBftCardanoCrypto)
  => Genesis.Config
  -> Genesis.GeneratedSecrets
  -> ProtocolInfo b
  -> EpochNo
  -> Crypto.SignKeyDSIGN Crypto.CardanoDSIGN
  -> Maybe (ProtocolInfo b, Byron.GenTx ByronBlock)
mkRekeyUpd genesisConfig genesisSecrets pInfo eno newSK = case pbftIsLeader of
    PBftIsNotALeader       -> Nothing
    PBftIsALeader isLeader ->
      let PBftIsLeader{pbftCoreNodeId} = isLeader
          genSK = genesisSecretFor genesisConfig genesisSecrets pbftCoreNodeId
          isLeader' = updSignKey genSK pbftExtConfig isLeader (coerce eno) newSK
          pInfo' = pInfo
            { pInfoConfig = pInfoConfig
              { pbftIsLeader = PBftIsALeader isLeader'
              }
            }

          PBftIsLeader{pbftDlgCert} = isLeader'
      in Just (pInfo', dlgTx pbftDlgCert)
  where
    ProtocolInfo{pInfoConfig}                   = pInfo
    PBftNodeConfig{pbftExtConfig, pbftIsLeader} = pInfoConfig

-- | The secret key for a node index
--
genesisSecretFor
  :: Genesis.Config
  -> Genesis.GeneratedSecrets
  -> CoreNodeId
  -> Crypto.SignKeyDSIGN Crypto.CardanoDSIGN
genesisSecretFor genesisConfig genesisSecrets cid =
    case hits of
        [sec] -> Crypto.SignKeyCardanoDSIGN sec
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
      . Crypto.VerKeyCardanoDSIGN . Crypto.toVerification

-- | Overwrite the 'PBftIsLeader''s operational key and delegation certificate
--
updSignKey
  :: Crypto.SignKeyDSIGN Crypto.CardanoDSIGN
  -> ByronConfig
  -> PBftIsLeader PBftCardanoCrypto
  -> EpochNumber
  -> Crypto.SignKeyDSIGN Crypto.CardanoDSIGN
  -> PBftIsLeader PBftCardanoCrypto
updSignKey genSK extCfg isLeader eno newSK = isLeader
    { pbftDlgCert = newCert
    , pbftSignKey = newSK
    }
  where
    newCert =
        Delegation.signCertificate
            (Byron.pbftProtocolMagicId extCfg)
            (Crypto.toVerification sk')
            eno
            (Crypto.noPassSafeSigner gsk')
      where
        Crypto.SignKeyCardanoDSIGN gsk' = genSK
        Crypto.SignKeyCardanoDSIGN sk'  = newSK

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

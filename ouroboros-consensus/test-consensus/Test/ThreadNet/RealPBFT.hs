{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
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
import           Data.Maybe (fromJust, mapMaybe)
import           Data.Time (Day (..), UTCTime (..))
import           Data.Word (Word64)

import           Numeric.Search.Range (searchFromTo)
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Ouroboros.Network.Block (SlotNo (..))
import           Ouroboros.Network.MockChain.Chain (Chain)
import qualified Ouroboros.Network.MockChain.Chain as Chain

import           Ouroboros.Consensus.Block (getHeader)
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.BlockchainTime.Mock
import           Ouroboros.Consensus.Ledger.Byron (ByronBlock)
import qualified Ouroboros.Consensus.Ledger.Byron as Byron
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Node.ProtocolInfo.Byron (plcCoreNodeId)
import           Ouroboros.Consensus.Node.Run.Abstract (nodeIsEBB)
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Protocol
import           Ouroboros.Consensus.Util.Condense (condense)
import           Ouroboros.Consensus.Util.Random

import           Ouroboros.Storage.Common (EpochNo (..))

import qualified Cardano.Chain.Common as Common
import qualified Cardano.Chain.Delegation as Delegation
import qualified Cardano.Chain.Genesis as Genesis
import           Cardano.Chain.ProtocolConstants (kEpochSlots)
import           Cardano.Chain.Slotting (unEpochSlots)
import qualified Cardano.Chain.Update as Update
import qualified Cardano.Crypto as Crypto
import qualified Test.Cardano.Chain.Genesis.Dummy as Dummy

import           Test.ThreadNet.General
import           Test.ThreadNet.Network (NodeOutput (..))
import qualified Test.ThreadNet.Ref.RealPBFT as Ref
import           Test.ThreadNet.Util
import           Test.ThreadNet.Util.NodeJoinPlan
import           Test.ThreadNet.Util.NodeTopology

import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Shrink (andId, dropId)

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
          prop_simple_real_pbft_convergence (SecurityParam 10) TestConfig
            { numCoreNodes = ncn
            , numSlots     = NumSlots 24
            , nodeJoinPlan = NodeJoinPlan $ Map.fromList [(CoreNodeId 0,SlotNo 0), (CoreNodeId 1,SlotNo 20), (CoreNodeId 2,SlotNo 22)]
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
          prop_simple_real_pbft_convergence (SecurityParam 10) TestConfig
            { numCoreNodes = ncn
            , numSlots     = NumSlots 2
            , nodeJoinPlan = NodeJoinPlan (Map.fromList [(CoreNodeId 0,SlotNo 0),(CoreNodeId 1,SlotNo 1)])
            , nodeTopology = meshNodeTopology ncn
            , slotLengths  = defaultSlotLengths
            , initSeed     = Seed (15069526818753326002, 9758937467355895013, 16548925776947010688, 13173070736975126721, 13719483751339084974)
            }
    , testProperty "rewind to EBB supported as of Issue #1312, #2" $
          once $
          let ncn = NumCoreNodes 2 in
          -- Same as above, except node 0 gets to forge an actual block before
          -- node 1 tells it to rewind to the EBB.
          prop_simple_real_pbft_convergence (SecurityParam 10) TestConfig
            { numCoreNodes = ncn
            , numSlots     = NumSlots 4
            , nodeJoinPlan = NodeJoinPlan (Map.fromList [(CoreNodeId 0,SlotNo {unSlotNo = 0}),(CoreNodeId 1,SlotNo {unSlotNo = 3})])
            , nodeTopology = meshNodeTopology ncn
            , slotLengths  = defaultSlotLengths
            , initSeed     = Seed (16817746570690588019, 3284322327197424879, 14951803542883145318, 5227823917971823767, 14093715642382269482)
            }
    , testProperty "simple convergence" $
          forAll (SecurityParam <$> elements [5, 10])
            $ \k ->
          forAllShrink
              (genRealPBFTTestConfig k)
              shrinkRealPBFTTestConfig
            $ \testConfig ->
          prop_simple_real_pbft_convergence k testConfig
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

prop_simple_real_pbft_convergence :: SecurityParam
                                  -> TestConfig
                                  -> Property
prop_simple_real_pbft_convergence
  k testConfig@TestConfig{numCoreNodes, numSlots} =
    prop_general k
        testConfig
        (Just $ roundRobinLeaderSchedule numCoreNodes numSlots)
        testOutput .&&.
    not (all Chain.null finalChains) .&&.
    conjoin (map (hasAllEBBs k numSlots) finalChains)
  where
    testOutput =
        runTestNetwork testConfig TestConfigBlock
            { forgeEBB = Just Byron.forgeEBB
            , nodeInfo = \nid -> protocolInfo $
                mkProtocolRealPBFT params nid genesisConfig genesisSecrets
            }

    finalChains :: [Chain ByronBlock]
    finalChains = Map.elems $ nodeOutputFinalChain <$> testOutputNodes testOutput

    params :: PBftParams
    params = realPBftParams k numCoreNodes

    genesisConfig  :: Genesis.Config
    genesisSecrets :: Genesis.GeneratedSecrets
    (genesisConfig, genesisSecrets) = generateGenesisConfig params

hasAllEBBs :: SecurityParam -> NumSlots -> Chain ByronBlock -> Property
hasAllEBBs k (NumSlots t) c =
    counterexample ("Missing or unexpected EBBs in " <> condense c) $
    actual === expected
  where
    expected :: [EpochNo]
    expected = coerce [0 .. hi]
      where
        hi :: Word64
        hi = if t < 1 then 0 else fromIntegral (t - 1) `div` denom
        denom = unEpochSlots $ kEpochSlots $ coerce k

    actual   = mapMaybe (nodeIsEBB . getHeader) $ Chain.toOldestFirst c

mkProtocolRealPBFT :: PBftParams
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
    dlgKey = fromJust $
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
    nodeTopology <- genNodeTopology numCoreNodes

    initSeed <- arbitrary

    pure TestConfig
      { nodeJoinPlan
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
  , nodeTopology
  , slotLengths
  , initSeed
  } =
    dropId $
    [ TestConfig
        { nodeJoinPlan = p'
        , nodeTopology = top'
        , numCoreNodes
        , numSlots     = t'
        , slotLengths  = ls'
        , initSeed
        }
    | t'            <- andId shrink numSlots
    , let adjustedP  = truncateNodeJoinPlan nodeJoinPlan numCoreNodes (numSlots, t')
    , p'            <- andId shrinkNodeJoinPlan adjustedP
    , top'          <- andId shrinkNodeTopology nodeTopology
    , ls'           <- andId shrink slotLengths
    ]

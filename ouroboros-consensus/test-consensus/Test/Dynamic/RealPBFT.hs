{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Dynamic.RealPBFT (
    tests
  ) where

import           Control.Monad (void, when)
import           Data.Foldable (find)
import           Data.Functor ((<&>))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromJust, isJust)
import           Data.Time (Day (..), UTCTime (..))

import           Numeric.Search.Range (searchFromTo)
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Ouroboros.Network.Block (ChainHash (..), blockNo)
import           Ouroboros.Network.MockChain.Chain (Chain)
import qualified Ouroboros.Network.MockChain.Chain as Chain
import           Ouroboros.Network.Point (WithOrigin (..))

import           Ouroboros.Consensus.Block (headerHash)
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Ledger.Byron (ByronBlock)
import           Ouroboros.Consensus.Ledger.Byron.Forge (forgeEBB)
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Node.ProtocolInfo.Byron (plcCoreNodeId)
import           Ouroboros.Consensus.Node.Run (nodeIsEBB)
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.NodeKernel (NodeKernel (..))
import           Ouroboros.Consensus.Protocol
import           Ouroboros.Consensus.Util.IOLike (IOLike)
import           Ouroboros.Consensus.Util.Random
import           Ouroboros.Consensus.Util.ResourceRegistry

import qualified Ouroboros.Storage.ChainDB as ChainDB
import           Ouroboros.Storage.Common (EpochNo (..), EpochSize (..))
import           Ouroboros.Storage.EpochInfo

import qualified Cardano.Chain.Common as Common
import qualified Cardano.Chain.Delegation as Delegation
import qualified Cardano.Chain.Genesis as Genesis
import qualified Cardano.Chain.ProtocolConstants as Constants
import qualified Cardano.Chain.Slotting as Slotting
import qualified Cardano.Chain.Update as Update
import qualified Cardano.Crypto as Crypto
import qualified Test.Cardano.Chain.Genesis.Dummy as Dummy

import           Test.Dynamic.General
import           Test.Dynamic.Network (NodeHook (..), NodeOutput (..))
import qualified Test.Dynamic.Ref.RealPBFT as Ref
import           Test.Dynamic.Util
import           Test.Dynamic.Util.NodeJoinPlan
import           Test.Dynamic.Util.NodeTopology

import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Shrink (andId, dropId)

tests :: TestTree
tests = testGroup "Dynamic chain generation"
    [ localOption (QuickCheckTests 10) $   -- each takes about 0.5 seconds!
      testProperty "check Real PBFT setup" $
        \numCoreNodes ->
          forAll (elements (enumCoreNodes numCoreNodes)) $ \coreNodeId ->
          prop_setup_coreNodeId numCoreNodes coreNodeId
    , adjustOption (\(QuickCheckTests n) -> QuickCheckTests (1 `max` (div n 10))) $
      -- as of merging PR #773, this test case fails without the commit that
      -- introduces the InvalidRollForward exception
      --
      -- See a related discussion at
      -- https://github.com/input-output-hk/ouroboros-network/pull/773#issuecomment-522192097
      testProperty "addressed by InvalidRollForward exception (PR #773)" $
          let ncn = NumCoreNodes 3
          in
          prop_simple_real_pbft_convergence TestConfig
            { numCoreNodes = ncn
            , numSlots = NumSlots 24
            , nodeJoinPlan = NodeJoinPlan $ Map.fromList
              [ (CoreNodeId 0,SlotNo 0)
              , (CoreNodeId 1,SlotNo 20)
              , (CoreNodeId 2,SlotNo 22)
              ]
            , nodeTopology = meshNodeTopology ncn
            }
            OnlyGenesisEBB
    , testProperty "simple Real PBFT convergence" $ \generateEBBs seed ->
        forAllShrink genRealPBFTTestConfig shrinkRealPBFTTestConfig $ \testConfig ->
        prop_simple_real_pbft_convergence testConfig generateEBBs seed
    ]

prop_setup_coreNodeId ::
     NumCoreNodes
  -> CoreNodeId
  -> Property
prop_setup_coreNodeId numCoreNodes coreNodeId =
    case mkProtocolRealPBFT numCoreNodes coreNodeId genesisConfig genesisSecrets of
      ProtocolRealPBFT _cfg _th _pv _swv (Just plc) ->
          coreNodeId === plcCoreNodeId plc
      _ ->
          counterexample "mkProtocolRealPBFT did not use ProtocolRealPBFT" $
          property False
  where
    genesisConfig  :: Genesis.Config
    genesisSecrets :: Genesis.GeneratedSecrets
    (genesisConfig, genesisSecrets) = generateGenesisConfig numCoreNodes

data GenerateEBBs
  = OnlyGenesisEBB
    -- ^ Only the node leading the first slot will produce an EBB in that slot
    -- (instead of a regular block). No other EBBs will be produced.
  | EachEpochAnEBB
    -- ^ At the start of each epoch, each node (independently) produces an EBB
    -- that fits onto its current tip.
  deriving (Eq, Show)

instance Arbitrary GenerateEBBs where
  arbitrary = elements [OnlyGenesisEBB, EachEpochAnEBB]
  shrink OnlyGenesisEBB = []
  shrink EachEpochAnEBB = [OnlyGenesisEBB]

prop_simple_real_pbft_convergence
  :: TestConfig
  -> GenerateEBBs
  -> Seed
  -> Property
prop_simple_real_pbft_convergence
  testConfig@TestConfig{numCoreNodes, numSlots} generateEBBs seed =
    tabulate "generate EBBS" [show generateEBBs] $
    prop_general k
        testConfig
        (Just $ roundRobinLeaderSchedule numCoreNodes numSlots)
        testOutput
    .&&. not (all Chain.null finalChains)
    .&&. checkEBBs
  where
    k =
      (SecurityParam . Common.unBlockCount) $
      (Genesis.gdK . Genesis.configGenesisData) $
      genesisConfig

    epochSize :: EpochSize
    epochSize =
      (EpochSize . Slotting.unEpochSlots) $
      Constants.kEpochSlots . Genesis.gdK . Genesis.configGenesisData $
      genesisConfig

    testOutput =
        runTestNetwork
            (\nid -> protocolInfo
                       (mkProtocolRealPBFT numCoreNodes nid
                                           genesisConfig genesisSecrets))
            testConfig seed
            (case generateEBBs of
               OnlyGenesisEBB -> mempty
               EachEpochAnEBB -> NodeHook ebbProducer)

    finalChains :: [Chain ByronBlock]
    finalChains = Map.elems $ nodeOutputFinalChain <$> testOutputNodes testOutput

    lastEpoch :: Chain ByronBlock -> EpochNo
    lastEpoch ch = case Chain.headSlot ch of
      Origin  -> 0
      At slot -> EpochNo (unSlotNo slot `div` unEpochSize epochSize)

    checkEBBs :: Property
    checkEBBs = case generateEBBs of
      OnlyGenesisEBB -> property True
      EachEpochAnEBB -> conjoin
        [ counterexample
            ("Expected EBBs in chain of node " <> show n <> ": " <>
             show expectedEBBs <>
             "\nActual EBBs in chain of node " <> show n <> ": " <>
             show actualEBBs)
            (expectedEBBs === actualEBBs)
        | (n, ch) <- Map.toList $
            nodeOutputFinalChain <$> testOutputNodes testOutput
          -- Count the EBBs in the chain
        , let actualEBBs = length $ filter (isJust . nodeIsEBB) $
                Chain.toNewestFirst ch
              -- An EBB for each epoch (we start counting at 0, so + 1)
              expectedEBBs = fromIntegral (unEpochNo (lastEpoch ch)) + 1
        ]

    genesisConfig  :: Genesis.Config
    genesisSecrets :: Genesis.GeneratedSecrets
    (genesisConfig, genesisSecrets) = generateGenesisConfig numCoreNodes

ebbProducer :: forall m peer. IOLike m
            => ResourceRegistry m
            -> BlockchainTime m
            -> NodeKernel m peer ByronBlock
            -> EpochInfo m
            -> m ()
ebbProducer registry btime nodeKernel epochInfo =
    void $ forkLinkedThread registry $ go 0
  where
    cfg     = getNodeConfig nodeKernel
    chainDB = getChainDB nodeKernel

    go :: EpochNo -> m ()
    go epoch = do
      -- The first slot in @epoch@
      ebbSlotNo <- epochInfoFirst epochInfo epoch
      over      <- blockUntilSlot btime ebbSlotNo
      when (over && ebbSlotNo > 0) $
        error $ "slot was already passed: " <> show ebbSlotNo
      (ebbBlockNo, prevHash) <- ChainDB.getTipHeader chainDB <&> \case
        Nothing  -> (0,           GenesisHash)
        Just hdr -> (blockNo hdr, BlockHash (headerHash hdr))
      let ebb = forgeEBB cfg ebbSlotNo ebbBlockNo prevHash
      ChainDB.addBlock chainDB ebb
      go (epoch + 1)

mkProtocolRealPBFT :: NumCoreNodes
                   -> CoreNodeId
                   -> Genesis.Config
                   -> Genesis.GeneratedSecrets
                   -> Protocol ByronBlock
mkProtocolRealPBFT numCoreNodes (CoreNodeId i)
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

    PBftParams{pbftSignatureThreshold} = realPBftParams numCoreNodes

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

realPBftParams :: NumCoreNodes -> PBftParams
realPBftParams numCoreNodes = PBftParams
  { pbftNumNodes           = n
  , pbftSecurityParam      = SecurityParam k
  , pbftSignatureThreshold = (1 / n) + (1 / k)
    -- crucially: @floor (k * t) >= ceil (k / n)@
  }
    where
      n :: Num a => a
      n = fromIntegral x where NumCoreNodes x = numCoreNodes

      k :: Num a => a
      k = 10

-- Instead of using 'Dummy.dummyConfig', which hard codes the number of rich
-- men (= CoreNodes for us) to 4, we generate a dummy config with the given
-- number of rich men.
generateGenesisConfig :: NumCoreNodes -> (Genesis.Config, Genesis.GeneratedSecrets)
generateGenesisConfig numCoreNodes =
    either (error . show) id $ Genesis.generateGenesisConfig startTime spec
  where
    startTime = UTCTime (ModifiedJulianDay 0) 0
    NumCoreNodes n = numCoreNodes
    PBftParams{pbftSecurityParam} = realPBftParams numCoreNodes

    spec :: Genesis.GenesisSpec
    spec = Dummy.dummyGenesisSpec
      { Genesis.gsInitializer = Dummy.dummyGenesisInitializer
        { Genesis.giTestBalance =
            (Genesis.giTestBalance Dummy.dummyGenesisInitializer)
              -- The nodes are the richmen
              { Genesis.tboRichmen = fromIntegral n }
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
  | otherwise      = go (NodeJoinPlan Map.empty) Ref.emptyState
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

genRealPBFTTestConfig :: Gen TestConfig
genRealPBFTTestConfig = do
    numCoreNodes <- arbitrary
    numSlots     <- arbitrary

    let params = realPBftParams numCoreNodes
    nodeJoinPlan <- genRealPBFTNodeJoinPlan params numSlots
    nodeTopology <- genNodeTopology numCoreNodes

    pure TestConfig
      { nodeJoinPlan
      , nodeTopology
      , numCoreNodes
      , numSlots
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
  } =
    dropId $
    [ TestConfig
        { nodeJoinPlan = p'
        , nodeTopology = top'
        , numCoreNodes
        , numSlots     = t'
        }
    | t'            <- andId shrink numSlots
    , let adjustedP  = truncateNodeJoinPlan nodeJoinPlan numCoreNodes (numSlots, t')
    , p'            <- andId shrinkNodeJoinPlan adjustedP
    , top'          <- andId shrinkNodeTopology nodeTopology
    ]

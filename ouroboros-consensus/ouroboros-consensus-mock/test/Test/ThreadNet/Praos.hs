{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}

module Test.ThreadNet.Praos (
    tests
  ) where

import qualified Data.ByteString as BS
import           Data.Coerce (coerce)
import           Data.List (intercalate)
import qualified Data.Map.Lazy as LazyMap
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (maybeToList)
import           Data.Semigroup (Last (..), Max (..), Min (..))
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word (Word64)
import           Numeric.Natural (Natural)

import           Test.QuickCheck

import           Test.Tasty
import           Test.Tasty.QuickCheck

import qualified Cardano.Crypto.Hash.Class as Hash
import           Cardano.Crypto.Hash.MD5 (MD5)
import           Cardano.Slotting.Slot

import           Ouroboros.Network.Block (BlockNo (..), SlotNo (..), blockHash,
                     blockNo, blockSlot)
import qualified Ouroboros.Network.Block as Network
import qualified Ouroboros.Network.MockChain.Chain as MockChain

import           Ouroboros.Consensus.Block.RealPoint (pattern RealPoint)
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Config.SecurityParam
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import           Ouroboros.Consensus.Mock.Ledger
import           Ouroboros.Consensus.Mock.Node ()
import           Ouroboros.Consensus.Mock.Node.Praos (protocolInfoPraos)
import           Ouroboros.Consensus.Mock.Protocol.Praos
import           Ouroboros.Consensus.Node.ProtocolInfo (NumCoreNodes (..),
                     enumCoreNodes)
import           Ouroboros.Consensus.NodeId

import           Test.ThreadNet.General
import           Test.ThreadNet.Network (CalcMessageDelay (..),
                     NodeOutput (..))
import           Test.ThreadNet.TxGen.Mock ()
import           Test.ThreadNet.Util
import           Test.ThreadNet.Util.HasCreator.Mock ()
import           Test.ThreadNet.Util.NodeJoinPlan
import           Test.ThreadNet.Util.NodeRestarts
import           Test.ThreadNet.Util.NodeTopology
import           Test.ThreadNet.Util.SimpleBlock

import           Test.Util.HardFork.Future (singleEraFuture)
import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Random
import           Test.Util.Slots (NumSlots (..))

data TestSetup = TestSetup
  { setupK            :: SecurityParam
  , setupTestConfig   :: TestConfig
  , setupDelaySeed    :: Seed
  , setupEpochSize    :: EpochSize
  , setupEta0         :: Natural
  , setupNodeJoinPlan :: NodeJoinPlan
  , setupPreDelta     :: Word64
  , setupSlotLength   :: SlotLength
  }
  deriving (Show)

instance Arbitrary TestSetup where
  arbitrary = do
      -- TODO k > 1 as a workaround for Issue #1511.
      k          <- SecurityParam <$> choose (2, 10)
      epochSize  <- EpochSize     <$> choose (1, 10)
      delaySeed  <- arbitrary
      eta0       <- fromIntegral <$> choose (minBound, maxBound :: Word64)
      slotLength <- arbitrary
        `suchThat` (\x -> getSlotLength x < getSlotLength (slotLengthFromSec 5))

      initSeed       <- arbitrary
      numCoreNodes   <- arbitrary
      let numSlots    = NumSlots (2 * w)
            where
              NumSlots w = stabilityNumSlots PraosParams
                { praosLeaderF       = fixedActiveSlotCoeff
                , praosSecurityParam = k
                , praosSlotsPerEpoch = unEpochSize epochSize
                }
      nodeTopology   <- genNodeTopology numCoreNodes
      let testConfig  = TestConfig
            { initSeed
            , nodeTopology
            , numCoreNodes
            , numSlots
            }

      -- crudely ensure new nodes have a relatively recent chain, since the Praos
      -- paper assumes that
      let truncForJoin (NumSlots n) = NumSlots $ n `min` maxRollbacks k
      nodeJoinPlan <- genNodeJoinPlan numCoreNodes (truncForJoin numSlots)

      pure $ TestSetup
        k
        testConfig
        delaySeed
        epochSize
        eta0
        nodeJoinPlan
        fixedPreDelta
        slotLength

  -- TODO shrink

tests :: TestTree
tests = testGroup "Praos" $
    concatMap
      (\(s, prop) ->
         -- TODO checkCoverage seems to cause QuickCheck to stop once it's
         -- confident about coverage. EG it only ran 200 tests despite
         -- --quickcheck-num 300
         [ testProperty s prop {-
         , testProperty ("cover " ++ s) (checkCoverage prop) -}
         ])
    [ (,) "simple convergence"
        $ \setup -> prop_simple_praos_convergence setup
    ]
    `asTypeOf`
    [ testProperty "simple convergence - special case (issue #131)" $
          testPraos $ Seed (49644418094676, 40315957626756, 42668365444963, 9796082466547, 32684299622558)
    , testProperty "simple convergence - special crowded case" $
          testPraos $ Seed (8871923881324151440, 881094692332313449, 3091285302407489889, 6410351877547894330, 14676014321459888687)
    ]
  where
    testPraos :: Seed -> Property
    testPraos seed =
        prop_simple_praos_convergence TestSetup
        { setupK            = k
        , setupTestConfig   = TestConfig
          { initSeed     = seed
          , nodeTopology = meshNodeTopology numCoreNodes
          , numCoreNodes
          , numSlots     =
              NumSlots $ maxRollbacks k * unEpochSize epochSize * numEpochs
          }
        , setupDelaySeed    = Seed (0, 0, 0, 0, 0)
        , setupEpochSize    = epochSize
        , setupEta0         = 0
        , setupNodeJoinPlan = trivialNodeJoinPlan numCoreNodes
        , setupPreDelta     = 1
        , setupSlotLength   = slotLengthFromSec 2
        }
      where
        numCoreNodes = NumCoreNodes 3
        k            = SecurityParam 5
        epochSize    = EpochSize 3
        numEpochs    = 3

prop_simple_praos_convergence :: TestSetup -> Property
prop_simple_praos_convergence TestSetup
  { setupK            = k
  , setupDelaySeed    = delaySeed
  , setupEta0         = eta0
  , setupTestConfig   = testConfig
  , setupEpochSize    = epochSize
  , setupNodeJoinPlan = nodeJoinPlan
  , setupPreDelta     = preDelta
  , setupSlotLength   = slotLength
  } =
  counterexample (show (eta0, testConfig)) $

    counterexample ("header adds " <> show [ (nid, s, rp, bno)
        | (nid, no)   <- Map.toList testOutputNodes
        , let NodeOutput{nodeOutputHeaderAdds} = no
        , (s, rpbnos) <- Map.toList nodeOutputHeaderAdds
        , (rp, bno)   <- rpbnos
        ]) $

    counterexample ("block selections " <> show [ (nid, s, rp, bno)
        | (nid, no)   <- Map.toList testOutputNodes
        , let NodeOutput{nodeOutputSelects} = no
        , (s, rpbnos) <- Map.toList nodeOutputSelects
        , (rp, bno)   <- rpbnos
        ]) $

    counterexample ("block forges " <> show [ (nid, blockHash blk, s, blockNo blk)
        | (nid, no) <- Map.toList testOutputNodes
        , let NodeOutput{nodeOutputForges} = no
        , (s, blk)  <- Map.toList nodeOutputForges
        ]) $

    counterexample (tracesToDot testOutputNodes) $
    classify noViolations          "NoViolations" $
    classify chainGrowthViolation  "CG Violation" $
    classify commonPrefixViolation "CP Violation" $
    tabulate "k,epochSize,numCoreNodes,numSlots,CP,CG"
      [ unwords
        [ show (maxRollbacks k)
        , show (unEpochSize epochSize)
        , show (let NumCoreNodes n = numCoreNodes in n)
        , show (let NumSlots n = numSlots testConfig in n)
        , show commonPrefixViolation
        , show chainGrowthViolation
        ]
      ] $
    tabulate "numCoreNodes, preDelta, activeSlotCoeff, k"
      [ intercalate ", "
        [ show (let NumCoreNodes n = numCoreNodes in n)
        , show preDelta
        , show (praosLeaderF params)
        , show (maxRollbacks k)
        ]
      ] $
    ( if not noViolations then id else
      tabulate "delta constant observed w/o violations"
        [show $ inferredDelta]) $
    prop_general_semisync PropGeneralArgs
      { pgaBlockProperty      = prop_validSimpleBlock
      , pgaCountTxs           = countSimpleGenTxs
      , pgaExpectedCannotLead = noExpectedCannotLeads
      , pgaFirstBlockNo       = 0
      , pgaFixedMaxForkLength = Nothing
      , pgaFixedSchedule      = Nothing
      , pgaSecurityParam      = k
      , pgaTestConfig         = testConfig
      , pgaTestConfigB        = testConfigB
      }
      testOutput .&&.
    prop_semiSyncProperties
--      .&&. (counterexample (show (onsetChains testConfig testOutput)) $ property noViolations)
  where
    testConfigB = TestConfigB
      { forgeEbbEnv  = Nothing
      , future       = singleEraFuture slotLength epochSize
      , messageDelay = mkMessageDelay fixedPreDelta delaySeed
      , nodeJoinPlan
      , nodeRestarts = noRestarts
      , txGenExtra   = ()
      }

    params = PraosParams
      { praosSecurityParam = k
      , praosSlotsPerEpoch = unEpochSize epochSize
      , praosLeaderF       = fixedActiveSlotCoeff
      }

    TestConfig{numCoreNodes} = testConfig

    testOutput =
        runTestNetwork testConfig testConfigB TestConfigMB
            { nodeInfo = \nid -> plainTestNodeInitialization $
                                    protocolInfoPraos
                                      numCoreNodes
                                      nid
                                      params
                                      (HardFork.defaultEraParams
                                        k
                                        slotLength)
                                      eta0
            , mkRekeyM = Nothing
            }

    TestOutput{testOutputNodes} = testOutput

    -- With semi-synchrony, " bad luck " can cause the net to violate Common
    -- Prefix or Chain Growth even in the absence of bugs.
    --
    -- The failure rate declines exponentially as @k@ increases, but we want
    -- small @k@ for the sake of test size and counterexample legibility.
    --
    -- We therefore check only that the failure rate is not greater than we
    -- expect. We use @QuickCheck@'s 'cover' and 'checkCoverage' for this.
    --
    -- NOTE: for the given 'stabilityNumSlots' definition, I have very very
    -- rarely seen a Chain Growth failure; it's dominated by Common Prefix.
    prop_semiSyncProperties :: Property
    prop_semiSyncProperties =
        checkCoverage $
        cover empiricallyTunedPercentage noViolations "NoViolations" $
        property True
      where
        empiricallyTunedPercentage = 70

    -- there were no violations of the papers' properties
    noViolations :: Bool
    noViolations = not $ chainGrowthViolation || commonPrefixViolation

    chainGrowthViolation :: Bool
    chainGrowthViolation = violatesChainGrowth params testOutput

    commonPrefixViolation :: Bool
    commonPrefixViolation = violatesCommonPrefix params testOutput

    inferredDelta :: Word64
    inferredDelta = inferDelta testConfig testOutput

-- | Determine Δ /a posteriori/
--
-- Finds the smallest value of Δ such that the concretized definition discussed
-- at 'fixedPreDelta' holds.
inferDelta :: TestConfig -> TestOutput Block -> Word64
inferDelta testConfig testOutput =
    foldl max 0 $
    Map.intersectionWith comb reach worst
  where
    TestConfig
      { numCoreNodes
      , numSlots
      } = testConfig
    TestOutput{testOutputNodes} = testOutput

    comb r w = if w < r then error "impossible!" else unSlotNo (w - r)

    allSlots :: [SlotNo]
    allSlots = takeWhile (< SlotNo sentinel) [0 ..]
      where
        NumSlots sentinel = numSlots

    -- the earliest slot during which a particular block number was selected by
    -- any node in the net
    --
    -- We use the block number to represent an equivalence class of blocks
    -- because all of our chain selection rules so far all only compare the
    -- block number (we have not implemented Genesis).
    reach :: Map BlockNo SlotNo
    reach =
        getAppMap coerce $
        mconcat
        [ appSingleton bno (Min s)
        | (_nid, no)  <- Map.toList testOutputNodes
        , let NodeOutput{nodeOutputSelects} = no
        , (s, rpbnos) <- Map.toList nodeOutputSelects
        , (_rp, bno)  <- rpbnos
        ]

    -- the earliest slot at the onset of which a particular block number
    -- was the worst selected in the net
    --
    -- This ignores nodes that have not yet joined, which is reasonable
    -- since they will immediately catch-up when they join.
    worst :: Map BlockNo SlotNo
    worst =
        getAppMap coerce $
        mconcat
        [ appSingleton lo $ Min (succ s)
            -- successor because we're focused on slot onsets
        | s  <- allSlots
        , lo <- maybeToList $ during s
        ]

    -- the minimum of @during1@ over all nodes in the net
    during :: SlotNo -> Maybe BlockNo
    during s =
        fmap getMin $
        mconcat
        [ Min <$> during1 (CoreId cid) s
        | cid <- enumCoreNodes numCoreNodes
        ]

    -- the maximal block number selected by that node at any point during
    -- that slot or prior to it
    --
    -- ASSUMPTION: a node's selected block number never descends.
    during1 :: NodeId -> SlotNo -> Maybe BlockNo
    during1 nid s = do
        m         <- Map.lookup nid selections
        (_s, bno) <- Map.lookupLE s m
        pure bno

    -- An entry @(nid, s, bno)@ means that node @nid@ selected @bno@ during
    -- slot @s@ and that it did not select a greater block number during
    -- slot @s@.
    selections :: NodeMap (SlotMap BlockNo)
    selections =
        getAppMap (getAppMap coerce) $
        mconcat
        [ appSingleton nid $ appSingleton s $ Max bno
        | (nid, no)  <- Map.toList testOutputNodes
        , let NodeOutput{nodeOutputSelects} = no
        , (s, rpbnos) <- Map.toList nodeOutputSelects
        , (_rp, bno)  <- rpbnos
        ]

{-------------------------------------------------------------------------------
  Test-specific constants
-------------------------------------------------------------------------------}

-- | Max chain diffusion delay /on an otherwise empty channel/
--
-- PREREQUISITE: 'fixedPreDelta > 0'
--
-- This value is very similar to /Δ/ from the Ouroboros papers, the maximum
-- chain diffusion delay (ie " message delay ") in slots.
--
-- DEFINITION: A valid and relevant block forged in @s@ will arrive at every
-- node before the onset of slot @s+Δ@.
--
-- NOTE: @Δ = 1@ corresponds to synchrony.
--
-- NOTE: Because of the word /relevant/ above and the fact that all our chain
-- selection rules currently reduce down to comparing block numbers, we can
-- concretize the definition.
--
-- DEFINITION: If a valid block with block number @bno@ is forged in slot @s@,
-- then before the onset of @s+Δ@ each node must have selected a block with a
-- block number of at least @bno@.
--
-- The crucial difference between Δ and @preDelta@ is that the filtering
-- network implemented by ChainSync/BlockFetch may cause chain diffusion delays
-- that exceed @preDelta@. A header H1 to be transfered along an edge between
-- two nodes can be " stuck behind " a later header H2 on the same edge.
-- Because H2 was forged later than H1, it is allowed by @preDelta@ to be
-- delayed for longer than H1 would be. But H1 must arrive after H2 does, so H1
-- inherits H2's delay, thereby possibly violating the tempting @Δ=preDelta@
-- interpretation.
--
-- It seems difficult and obfuscatory to enrich the test infrastructure to be
-- able to enforce an /a priori/ value of Δ, so we settle for only specifying
-- and enforcing the simpler @preDelta@. We then compute the actual Δ in
-- hindsight, once the simulation is over. In practice, for runs that do not
-- violate CP or CG, we see most realizations of Δ ranging from slightly less
-- than @preDelta@ up to @2 * preDelta@, exclusive. This doubling fits with the
-- " one header getting stuck behind another " story: each header can have a
-- delay of approximately @preDelta@, and their delays can accumulate. (I've
-- specifically seen it happen when a node forges its own block immediately
-- prior to receiving an older-but-better block; it forwards its own block
-- before switching to and forwarding the older one, so the two delays may
-- accumulate.)
--
-- We use a fixed value for @preDelta@ because otherwise it's already difficult
-- to assess how often correct code should violate the papers' expected
-- properties.
fixedPreDelta :: Word64
fixedPreDelta = 3

fixedActiveSlotCoeff :: Double
fixedActiveSlotCoeff = 0.2

{-------------------------------------------------------------------------------
  Message delay calculators
-------------------------------------------------------------------------------}

-- | Semi-synchronous 'CalcMessageDelay'
--
-- Calculates a deterministic arbitrary delay that varies with sender,
-- recipient, content, and the @delaySeed@ and is bounded above by @preDelta@.
mkMessageDelay :: Word64 -> Seed -> CalcMessageDelay Block
mkMessageDelay preDelta delaySeed = CalcMessageDelay $
    \(sender, recipient) hdr ->
    NumSlots $
    -- a delay of 0 means " during the block's forge slot "
    (`mod` preDelta) $
    (fromIntegral . BS.last . Hash.getHash) $
    Hash.hash @MD5
      ( prj sender, prj recipient
      , blockHash hdr
      , getSeed delaySeed
      )
  where
    prj (CoreNodeId i) = i

{-------------------------------------------------------------------------------
  Papers' expected properties
-------------------------------------------------------------------------------}

-- | Whether there is a Chain Growth violation
violatesChainGrowth :: PraosParams -> TestOutput Block -> Bool
{-# INLINE violatesChainGrowth #-}
violatesChainGrowth params testOutput =
    -- "at least 0 blocks in ..." is tautological
    if 0 == k then False else
    any slotViolates $ Set.fromList $
    Map.toList $ onsetSelectionUpdates testOutput
  where
    PraosParams{praosSecurityParam} = params
    k = maxRollbacks praosSecurityParam

    slotViolates :: (SlotNo, NodeMap Hash) -> Bool
    slotViolates (s, m) = any (chainViolates s) m

    chainViolates :: SlotNo -> Hash -> Bool
    chainViolates s h =
        if not (null deeps) then or $ zipWith isViolation1 deeps shallows else
        -- the selected chain @c@ has @<k@ blocks
        isViolation2
      where
        c = unfoldHash testOutput h
        c' = forget1 c
        shallows = (s:) $ map blockSlot $ MockChain.chainToList c'
        deeps    = drop (fromIntegral k) shallows

        NumSlots w = stabilityNumSlots params

        -- the @k@ blocks span more than @w@ slots
        isViolation1 (SlotNo deep) (SlotNo shallow) = deep + w < shallow

        -- there are not enough slots remaining after the tip to find the
        -- missing requisite blocks
        isViolation2 = fixedSlots + atLeastSlotsMore > maxSlots
          where
            maxSlots         = w
            fixedSlots       = unSlotNo (blockSlot (tip1 c)) + 1
            atLeastSlotsMore = k - fromIntegral (MockChain.length c')

-- | Praos's @s@ parameter for the papers' Chain Growth property specialized so
-- that @tau*s = k@
--
-- For Byron, the researchers recommend a Chain Growth property of "@k@ blocks
-- in @2k@ slots", thus @s=2k@.
--
-- For Praos, the researchers insted recommended @3k/f@ for @s@.
stabilityNumSlots :: PraosParams -> NumSlots
stabilityNumSlots params =
    NumSlots $ ceiling $ fromIntegral (3 * k) / praosLeaderF
  where
    PraosParams
      { praosSecurityParam
      , praosLeaderF
      } = params
    k = maxRollbacks praosSecurityParam

-- | Whether there is a Common Prefix violation
violatesCommonPrefix :: PraosParams -> TestOutput Block -> Bool
{-# INLINE violatesCommonPrefix #-}
violatesCommonPrefix params testOutput =
    go $
    fmap (Set.fromList . Map.elems) $
    frameRule $
    onsetSelectionUpdates testOutput
  where
    PraosParams{praosSecurityParam} = params
    k = maxRollbacks praosSecurityParam

    go :: SlotMap (Set Hash) -> Bool
    go m = case Map.minView m of
        Nothing       -> False   -- no violation
        Just (hs, m') ->
            slotViolates hs (Set.unions m') ||
            go m'

    -- compare each hash selected at one onset to each hash selected at every
    -- greater-than-or-equal onset
    slotViolates :: Set Hash -> Set Hash -> Bool
    slotViolates nowHs laterHs =
        or
        [ isViolation c1 c2
        | c1 <- f nowHs
        , c2 <- f $ Set.union nowHs laterHs
        ]
      where
        f :: Set Hash -> [Chain]
        f = map (forget1 . unfoldHash testOutput) . Set.toList

    isViolation :: Chain -> Chain -> Bool
    isViolation c1 c2 =
        not $
        MockChain.drop (fromIntegral k) c1 `MockChain.isPrefixOf` c2

-- | Apply the frame rule to a @'SlotMap' . 'NodeMap'@
--
-- Ensures that once a node is present in a slot's map, it is also present in
-- all later slots.
frameRule :: SlotMap (NodeMap a) -> SlotMap (NodeMap a)
frameRule = snd . Map.mapAccum upd acc0
  where
    acc0 = Map.empty
    upd acc m = (m', m')
      where
        m' = Map.unionWith (\_l r -> r) acc m

-- | Each node's selected block as of an onset, if it changed during the
-- preceding slot
--
-- Either @nid@ has no entry for a previous slot, or its most recent such entry
-- has a different chain hash. See 'frameRule'.
--
-- This map does not include nodes when they join (with 'MockChain.Genesis'),
-- only when they actively select a chain.
--
-- NOTE: This map includes the onset of the slot that would come after the
-- simulation's final slot, since that slot's onset's info is determined by the
-- behavior in the simulation's final slot.
onsetSelectionUpdates :: TestOutput Block -> SlotMap (NodeMap Hash)
onsetSelectionUpdates testOutput =
    bumpKeys $
    getAppMap (getAppMap coerce) $
    mconcat
    [ appSingleton s $ appSingleton nid $ Last h
    | (nid, no)   <- Map.toList testOutputNodes
    , let NodeOutput{nodeOutputSelects} = no
    , (s, rpbnos) <- Map.toList nodeOutputSelects
    , (rp, _bno)   <- rpbnos
    , let RealPoint _s' h = rp
    ]
  where
    TestOutput{testOutputNodes} = testOutput

    -- the last new selection in this slot determines the selection as of the
    -- the onset of the next slot
    bumpKeys = Map.mapKeysMonotonic succ

-- | Lookup a non-empty chain by the hash of its tip
--
-- 'error' if the hash was not forged during the 'TestOutput''s run.
unfoldHash :: TestOutput Block -> Hash -> Chain1
{-# NOINLINE unfoldHash #-}
unfoldHash testOutput = unfold
  where
    TestOutput{testOutputNodes} = testOutput

    -- note: mutual recursion
    unfold :: Hash -> Chain1
    unfold h = case Map.lookup h forges of
        Nothing -> error "impossible!"
        Just c  -> c

    -- note: mutual recursion
    forges :: Map Hash Chain1
    forges =
        LazyMap.fromList
        [ (blockHash blk, f blk :> blk)
        | (_nid, no) <- Map.toList testOutputNodes
        , let NodeOutput{nodeOutputForges} = no
        , (_s, blk)  <- Map.toList nodeOutputForges
        ]
      where
        f :: Block -> Chain
        f blk =
            case MockChain.blockPrevHash blk of
              Network.GenesisHash -> MockChain.Genesis 
              Network.BlockHash h -> forget1 (unfold h)

forget1 :: Chain1 -> Chain
forget1 (c :> b) = c MockChain.:> b

tip1 :: Chain1 -> Block
tip1 (_ :> b) = b

{-------------------------------------------------------------------------------
  Test-specific type synonyms
-------------------------------------------------------------------------------}

type Block =
  SimpleBlock SimpleMockCrypto
    (SimplePraosExt SimpleMockCrypto PraosMockCrypto)

type Hash = MockChain.HeaderHash Block

type Chain = MockChain.Chain Block

data Chain1 = Chain :> Block

{-------------------------------------------------------------------------------
  Miscellany
-------------------------------------------------------------------------------}

type SlotMap a = Map SlotNo a
type NodeMap a = Map NodeId a

-- | A wrapper of 'Map' that lifts 'Semigroup'
newtype AppMap k v = AppMap (Map k v)
  deriving (Functor)

getAppMap :: (a -> b) -> AppMap k a -> Map k b
getAppMap f (AppMap m) = fmap f m

instance (Ord k, Semigroup v) => Semigroup (AppMap k v) where
  AppMap l <> AppMap r = AppMap $ Map.unionWith (<>) l r

instance (Ord k, Semigroup v) => Monoid (AppMap k v) where
  mempty = AppMap Map.empty

appSingleton :: k -> v -> AppMap k v
appSingleton k v = AppMap $ Map.singleton k v

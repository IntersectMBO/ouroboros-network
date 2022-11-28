{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}

-- | Generate sequences of updates to model an evolving chain
module Test.Util.ChainUpdates (
    ChainUpdate (..)
  , UpdateBehavior (..)
  , behaviorValidity
  , blocksFromChainUpdates
  , classifyBehavior
  , genChainUpdates
  , toChainUpdates
    -- * Tests
  , prop_genChainUpdates
  ) where

import           Control.Monad.State.Strict
import qualified Data.Set as Set

import           Test.QuickCheck

import           Ouroboros.Network.MockChain.Chain (Chain (Genesis))
import qualified Ouroboros.Network.MockChain.Chain as Chain

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Util.Condense (Condense (..))

import           Test.Util.TestBlock

data ChainUpdate =
    AddBlock TestBlock
    -- | Roll back to the given 'Point', and then /immediately/ roll
    -- forward by the given 'TestBlock's.
  | SwitchFork (Point TestBlock) [TestBlock]
  deriving stock (Eq, Show)

instance Condense ChainUpdate where
  condense = \case
    AddBlock b -> "AddBlock " <> condense b
    SwitchFork p bs -> "SwitchFork <- " <> condense p <> " -> " <>
      unwords (map condense bs)

toChainUpdates :: [ChainUpdate] -> [Chain.ChainUpdate TestBlock TestBlock]
toChainUpdates = concatMap $ \case
    SwitchFork pt bs -> Chain.RollBack pt : map Chain.AddBlock bs
    AddBlock b       -> Chain.AddBlock b  : []

blocksFromChainUpdates :: [ChainUpdate] -> [TestBlock]
blocksFromChainUpdates = concatMap $ \case
    SwitchFork _ blks -> blks
    AddBlock blk      -> [blk]

{-------------------------------------------------------------------------------
  Generating ChainUpdates
-------------------------------------------------------------------------------}

-- | We need some state to generate @ChainUpdate@s
data ChainUpdateState = ChainUpdateState
  { cusCurrentChain :: !(Chain TestBlock)
    -- ^ The current chain, obtained by applying all the 'cusUpdates' in reverse
    -- order.
  , cusUpdates      :: ![ChainUpdate]
    -- ^ The updates that have been generated so far, in reverse order: the
    -- first update in the list is the last update to apply.
  } deriving stock (Show)

emptyUpdateState :: ChainUpdateState
emptyUpdateState = ChainUpdateState
  { cusCurrentChain = Genesis
  , cusUpdates      = []
  }

getChainUpdates :: ChainUpdateState -> [ChainUpdate]
getChainUpdates = reverse . cusUpdates

-- | Different strategies how to generate a sequence of 'ChainUpdate's.
data UpdateBehavior =
    -- | Chain updates tracking the selected chain of an honest node. In
    -- particular, this includes:
    --
    --  * All blocks involved are valid.
    --  * No 'ChainUpdate' causes the chain to regress.
    SelectedChainBehavior
  | -- | Chain updates tracking the tentative chain of an honest node (in the
    -- context of diffusion pipelining). This is similiar to
    -- 'SelectedChainBehavior', but allows for the following sequence of
    -- 'ChainUpdates':
    --
    --  1. @'AddBlock' blk@ for @blk@ invalid
    --  2. @'SwitchFork' (prevPoint blk) [blk']@ where @blk'@ is preferable to
    --     @blk@.
    TentativeChainBehavior
  | -- | Chain updates involving invalid blocks only arisable by bugs, malice or
    -- incorrect configuration.
    InvalidChainBehavior
  deriving stock (Show, Eq, Enum, Bounded)
  deriving stock
    ( -- | Note that this 'Ord' instance (and hence the order of constructors)
      -- is semantically imporant, cf. 'genChainUpdates'.
      Ord
    )

-- | Whether an 'UpdateBehavior' should cause a ChainSync and/or BlockFetch
-- client to disconnect from its peer.
behaviorValidity :: UpdateBehavior -> Validity
behaviorValidity = \case
    SelectedChainBehavior  -> Valid
    TentativeChainBehavior -> Valid
    InvalidChainBehavior   -> Invalid

-- | Generate a sequence of chain updates. The given 'UpdateBehavior' is only
-- used as an "upper bound" for what kind of updates are generated, i.e.
-- specifying 'TentativeChainBehavior' might (rarely) result in a sequence for
-- which 'classifyBehavior' returns 'SelectedChainBehavior'.
--
-- Concretely, we have the law
--
-- > classifyBehavior updates <= behavior
--
-- for all
--
-- > updates <- forAll $ genChainUpdates behavior k n
genChainUpdates
  :: UpdateBehavior
  -> SecurityParam
  -> Int
     -- ^ An indicator of how many updates to generate. The actual number of
     -- updates will be proportional with a low factor.
  -> Gen [ChainUpdate]
genChainUpdates updateBehavior securityParam n =
        getChainUpdates
    <$> genChainUpdateState updateBehavior securityParam n emptyUpdateState

genChainUpdateState ::
     UpdateBehavior
  -> SecurityParam
  -> Int
  -> ChainUpdateState
  -> Gen ChainUpdateState
genChainUpdateState behavior securityParam n =
    execStateT (replicateM_ n genChainUpdate)
  where
    -- Modify the state
    addUpdate u cus = cus { cusUpdates = u : cusUpdates cus }
    setChain  c cus = cus { cusCurrentChain = c }

    k = fromIntegral $ maxRollbacks securityParam

    genChainUpdate = frequency' $
           -- Generate two normal updates, as the other option generates two
           -- updates, in order to keep the number of updates proportional to n.
           [ (5, replicateM_ 2 genNormalUpdate) ]
        <> [ (1, genTrapTentativeBlock)
           | behavior == TentativeChainBehavior
           ]
      where
        -- Generate a single update, either AddBlock or SwitchFork
        genNormalUpdate = do
            chain <- gets cusCurrentChain
            frequency'
              [ (3, genAddBlock =<< genValidity)
              , ( if Chain.null chain then 0 else 1
                , genSwitchFork (choose (1, k))
                )
              ]

    genBlockToAdd validity = do
        ChainUpdateState { cusCurrentChain = chain } <- get
        block <- lift $ case Chain.head chain of
          Nothing      -> setValidity . firstBlock <$> genForkNo
          Just curHead -> do
            forkNo <- genForkNo
            return
              . modifyFork (const forkNo)
              . setValidity
              $ successorBlock curHead
        modify $ setChain (Chain.addBlock block chain)
        return block
      where
        setValidity b = b { tbValid = validity }
        genForkNo = case validity of
          Valid -> frequency
            [ (1, return 0)
            , (1, choose (1, 2))
            ]
          -- Blocks with equal hashes have to have equal validity, so we reserve
          -- specific ForkNos for invalid blocks to ensure this. Note that the
          -- concrete numbers chosen here don't have any real semantics.
          Invalid -> elements [3, 4]

    genAddBlock validity = do
      block <- genBlockToAdd validity
      modify $ addUpdate (AddBlock block)

    genSwitchFork genRollBackBlocks = do
      ChainUpdateState { cusCurrentChain = chain } <- get
      rollBackBlocks <- lift genRollBackBlocks
      let chain' = Chain.drop rollBackBlocks chain
      modify $ setChain chain'
      blocks <- replicateM rollBackBlocks (genBlockToAdd =<< genValidity)
      modify $ addUpdate (SwitchFork (Chain.headPoint chain') blocks)

    genTrapTentativeBlock = do
      genAddBlock Invalid
      genSwitchFork (pure 1)

    genValidity = case behavior of
      InvalidChainBehavior -> frequency' [ (4, pure Valid), (1, pure Invalid) ]
      _                    -> pure Valid

-- | Variant of 'frequency' that allows for transformers of 'Gen'
frequency' :: (MonadTrans t, Monad (t Gen)) => [(Int, t Gen a)] -> t Gen a
frequency' [] = error "frequency' used with empty list"
frequency' xs0 = lift (choose (1, tot)) >>= (`pick` xs0)
  where
    tot = sum (map fst xs0)

    pick n ((k,x):xs)
      | n <= k    = x
      | otherwise = pick (n-k) xs
    pick _ _  = error "pick used with empty list"

-- | Classify the 'UpdateBehavior' of a sequence of 'ChainUpdate's based on
-- their validities.
--
-- PRECONDITION: The updates fit on each other.
classifyBehavior :: [ChainUpdate] -> UpdateBehavior
classifyBehavior updates
    | null invalidBlocks
    = SelectedChainBehavior
    | noInvalidBlockExtended && invalidBlocksImproving
    = TentativeChainBehavior
    | otherwise
    = InvalidChainBehavior
  where
    -- The behavior is tentative iff:
    --  1. The sequence of invalid blocks is strictly improving.
    invalidBlocksImproving = strictlyIncreasing $ blockNo <$> invalidBlocks
    --  2. No block ever extends an invalid block.
    noInvalidBlockExtended = all predecessorIsValid allBlocks

    allBlocks = blocksFromChainUpdates updates
    invalidBlocks = filter ((Invalid ==) . tbValid) allBlocks

    predecessorIsValid =
        \blk -> blockPrevHash blk `Set.notMember` invalidBlockHashes
      where
        invalidBlockHashes =
          Set.fromList $ BlockHash . blockHash <$> invalidBlocks

    strictlyIncreasing :: Ord a => [a] -> Bool
    strictlyIncreasing as = and $ zipWith (<) as (tail as)

-- | Test that applying the generated updates gives us the same chain
-- as @cusCurrentChain@.
prop_genChainUpdates :: SecurityParam -> Int -> Property
prop_genChainUpdates securityParam n =
    forAll genCUS $ \cus ->
      Chain.applyChainUpdates (toChainUpdates (getChainUpdates cus)) Genesis ===
      Just (cusCurrentChain cus)
  where
    genCUS = do
      behavior <- arbitraryBoundedEnum
      genChainUpdateState behavior securityParam n emptyUpdateState

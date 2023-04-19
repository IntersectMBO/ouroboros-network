{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}

-- | Generate sequences of updates to model an evolving chain
module Test.Util.ChainUpdates (
    ChainUpdate (..)
  , UpdateBehavior (..)
  , genChainUpdates
  , toChainUpdates
    -- * Tests
  , prop_genChainUpdates
  ) where

import           Control.Monad.State.Strict
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Util.Condense (Condense (..))
import           Ouroboros.Network.Mock.Chain (Chain (Genesis))
import qualified Ouroboros.Network.Mock.Chain as Chain
import           Test.QuickCheck
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
    --  * Every 'ChainUpdate' improves the chain.
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
  deriving stock (Show, Eq, Enum, Bounded)

genChainUpdates
  :: UpdateBehavior
  -> SecurityParam
  -> Int  -- ^ The number of updates to generate
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
genChainUpdateState updateBehavior securityParam n =
    execStateT (replicateM_ n genChainUpdate)
  where
    -- Modify the state
    addUpdate u cus = cus { cusUpdates = u : cusUpdates cus }
    setChain  c cus = cus { cusCurrentChain = c }

    k = fromIntegral $ maxRollbacks securityParam

    genChainUpdate = do
      ChainUpdateState { cusCurrentChain = chain } <- get
      let genValid =
            frequency'
              [ (3, genAddBlock Valid)
              , ( if Chain.null chain then 0 else 1
                , genSwitchFork (choose (1, k))
                )
              ]
      frequency' $
        (5, replicateM_ 2 genValid) :
        [ (1, genInvalidBlock) | updateBehavior == TentativeChainBehavior ]

    genBlockToAdd validity = do
        ChainUpdateState { cusCurrentChain = chain } <- get
        block <- lift $ case Chain.head chain of
          Nothing      -> setValidity . firstBlock <$> genForkNo
          Just curHead -> do
            forkNo <- case validity of
              Valid   ->  genForkNo
              Invalid -> pure 3
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
          -- a specific ForkNo for invalid blocks to ensure this.
          Invalid -> pure 3

    genAddBlock validity = do
      block <- genBlockToAdd validity
      modify $ addUpdate (AddBlock block)

    genSwitchFork genRollBackBlocks = do
      ChainUpdateState { cusCurrentChain = chain } <- get
      rollBackBlocks <- lift genRollBackBlocks
      let chain' = Chain.drop rollBackBlocks chain
      modify $ setChain chain'
      blocks <- replicateM rollBackBlocks (genBlockToAdd Valid)
      modify $ addUpdate (SwitchFork (Chain.headPoint chain') blocks)

    genInvalidBlock = do
      genAddBlock Invalid
      genSwitchFork (pure 1)

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

-- | Test that applying the generated updates gives us the same chain
-- as @cusCurrentChain@.
prop_genChainUpdates :: SecurityParam -> Int -> Property
prop_genChainUpdates securityParam n =
    forAll genCUS $ \cus ->
      Chain.applyChainUpdates (toChainUpdates (getChainUpdates cus)) Genesis ===
      Just (cusCurrentChain cus)
  where
    genCUS = do
      behavior <- chooseEnum (minBound, maxBound)
      genChainUpdateState behavior securityParam n emptyUpdateState

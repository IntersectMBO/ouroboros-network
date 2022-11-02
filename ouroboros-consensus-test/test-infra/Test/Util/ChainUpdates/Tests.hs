{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE RecordWildCards    #-}

module Test.Util.ChainUpdates.Tests (tests) where

import qualified Data.Map.Strict as Map

import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Util (allEqual)
import           Ouroboros.Consensus.Util.Condense

import           Test.Util.ChainUpdates
import           Test.Util.QuickCheck
import           Test.Util.TestBlock

tests :: TestTree
tests = testGroup "Test.Util.ChainUpdates"
    [ testProperty "genChainUpdates" $
        prop_genChainUpdates securityParam 100
    , testProperty "consistency"
        prop_consistency
    ]

securityParam :: SecurityParam
securityParam = SecurityParam 3

data ChainUpdatesTestSetup = ChainUpdatesTestSetup {
    -- | The 'UpdateBehavior' used to generate the 'chainUpdates'.
    updateBehavior :: UpdateBehavior
  , chainUpdates   :: [ChainUpdate]
  }
  deriving stock (Show, Eq)

instance Condense ChainUpdatesTestSetup where
  condense ChainUpdatesTestSetup{..} =
         "Update behavior: " <> show updateBehavior <> "\n"
      <> "Updates: " <> condense chainUpdates

instance Arbitrary ChainUpdatesTestSetup where
  arbitrary = do
      numUpdates     <- chooseInt (5, 100)
      updateBehavior <- arbitraryBoundedEnum
      chainUpdates   <- genChainUpdates updateBehavior securityParam numUpdates
      pure ChainUpdatesTestSetup {..}

  shrink ChainUpdatesTestSetup{..} =
      [ ChainUpdatesTestSetup{chainUpdates = chainUpdates', ..}
      | _ : chainUpdates' <- [chainUpdates]
      ]

prop_consistency :: ChainUpdatesTestSetup -> Property
prop_consistency cuts@ChainUpdatesTestSetup{..} =
    counterexample (condense cuts) $
    collect updateBehavior $
    conjoin
      [ hashValidityConsistency
      , classificationConsistency
      ]
  where
    -- | Ensure that blocks with the same hash actually have the same validity
    -- (also see the comment on 'tbValid').
    hashValidityConsistency =
        counterexample "hash validity inconsistency" $
        property $ all allEqual validityByHash
      where
        allBlocks = blocksFromChainUpdates chainUpdates
        validityByHash = Map.fromListWith (<>)
          [ (blockHash blk, [tbValid blk]) | blk <- allBlocks ]

    -- | Ensure that classifying the updates generated for a specific behavior
    -- yields this or a more narrow behavior.
    classificationConsistency =
        counterexample "classification inconsistency" $
        tabulate "classified vs. requested behavior"
        [show (classifiedBehavior, updateBehavior)] $
        classifiedBehavior `le` updateBehavior
      where
        classifiedBehavior = classifyBehavior chainUpdates

{-# LANGUAGE TypeApplications #-}
-- |

module Test.Consensus.Mempool.StateMachine.TestBlock (tests) where

import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Monadic as QCM
import           Test.StateMachine.Sequential (checkCommandNames,
                     forAllCommands, prettyCommands, runCommands)
import           Test.StateMachine.Types (Reason (Ok))
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           Test.Consensus.Mempool.StateMachine (stateMachine)



tests :: TestTree
tests = testGroup "Mempool State Machine" [
      testProperty "Sequential" prop_sequential
    ]

prop_sequential :: QC.Property
prop_sequential = QC.withMaxSuccess 1000 $
    forAllCommands (stateMachine @()) Nothing $ \cmds ->
      QCM.monadicIO $ do
        (hist, _model, res) <- runCommands (stateMachine @()) cmds
        prettyCommands stateMachine hist
          $ checkCommandNames cmds
          $ res QC.=== Ok

{-# LANGUAGE TypeApplications #-}

module Test.Consensus.Byron.LedgerTables (tests) where

import           Ouroboros.Consensus.Byron.Ledger
import           Test.Consensus.Byron.Generators ()
import           Test.LedgerTables
import           Test.Tasty
import           Test.Tasty.QuickCheck

tests :: TestTree
tests = testGroup "LedgerTables"
  [ testProperty "Stowable laws" (prop_stowable_laws @ByronBlock)
  , testProperty "TableStuff laws" (prop_tablestuff_laws @ByronBlock)
  ]

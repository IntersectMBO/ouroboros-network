{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications  #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Consensus.Ledger.Mock.LedgerTables (tests) where

import           Test.Consensus.Ledger.Mock.Generators ()
import           Test.LedgerTables

import           Ouroboros.Consensus.Ledger.Basics
import           Ouroboros.Consensus.Mock.Ledger
import           Ouroboros.Consensus.Mock.Ledger.Block
import           Ouroboros.Consensus.Protocol.PBFT
import           Test.Tasty
import           Test.Tasty.QuickCheck

type Block = SimpleBlock SimpleMockCrypto (SimplePBftExt SimpleMockCrypto PBftMockCrypto)

tests :: TestTree
tests = testGroup "LedgerTables"
  [ testProperty "Stowable laws" (prop_stowable_laws @Block)
  , testProperty "TableStuff laws" (prop_tablestuff_laws @Block)
  ]

instance Arbitrary (LedgerTables (LedgerState Block) ValuesMK) where
  arbitrary = projectLedgerTables <$> arbitrary

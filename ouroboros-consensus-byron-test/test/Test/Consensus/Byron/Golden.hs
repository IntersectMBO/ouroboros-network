{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Consensus.Byron.Golden (
    tests
  ) where

import           Ouroboros.Consensus.Byron.Ledger.NetworkProtocolVersion
import           Ouroboros.Consensus.Byron.Node ()

import           Test.Tasty

import           Test.Util.Paths
import           Test.Util.Serialisation.Golden

import           Test.Consensus.Byron.Examples

tests :: TestTree
tests = goldenTest_all codecConfig $(getGoldenDir) examples

instance ToGoldenDirectory ByronNodeToNodeVersion
  -- Use defaults

instance ToGoldenDirectory ByronNodeToClientVersion
  -- Use defaults

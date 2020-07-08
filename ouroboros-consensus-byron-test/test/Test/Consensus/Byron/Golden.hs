{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Consensus.Byron.Golden (tests) where

import           System.FilePath ((</>))

import           Ouroboros.Consensus.Byron.Ledger.NetworkProtocolVersion
import           Ouroboros.Consensus.Byron.Node ()

import           Test.Tasty

import           Test.Util.Serialisation.Golden

import           Test.Consensus.Byron.Examples

tests :: TestTree
tests = goldenTest_all codecConfig goldenDir examples
  where
    goldenDir = "ouroboros-consensus-byron-test" </> "test" </> "golden"

instance ToGoldenDirectory ByronNodeToNodeVersion
  -- Use defaults

instance ToGoldenDirectory ByronNodeToClientVersion
  -- Use defaults

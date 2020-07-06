{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Consensus.Shelley.Golden (tests) where

import           System.FilePath ((</>))

import           Ouroboros.Consensus.Shelley.Ledger.NetworkProtocolVersion
import           Ouroboros.Consensus.Shelley.Node ()

import           Test.Tasty

import           Test.Util.Serialisation.Golden

import           Test.Consensus.Shelley.Examples

tests :: TestTree
tests = goldenTest_all codecConfig goldenDir examples
  where
    goldenDir = "ouroboros-consensus-shelley-test" </> "test" </> "golden"

instance ToGoldenDirectory ShelleyNodeToNodeVersion
  -- Use defaults

instance ToGoldenDirectory ShelleyNodeToClientVersion
  -- Use defaults

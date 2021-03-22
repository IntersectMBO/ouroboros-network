{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Consensus.Shelley.Golden (tests) where

import           Ouroboros.Consensus.Shelley.Ledger.NetworkProtocolVersion
import           Ouroboros.Consensus.Shelley.Node ()

import           Test.Tasty

import           Test.Util.Paths
import           Test.Util.Serialisation.Golden

import           Test.Consensus.Shelley.Examples

tests :: TestTree
tests = goldenTest_all codecConfig $(getGoldenDir) examplesShelley

instance ToGoldenDirectory ShelleyNodeToNodeVersion
  -- Use defaults

instance ToGoldenDirectory ShelleyNodeToClientVersion
  -- Use defaults

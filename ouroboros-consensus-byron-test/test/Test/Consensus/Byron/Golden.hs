{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Consensus.Byron.Golden (tests) where

import           System.FilePath ((</>))

import           Ouroboros.Consensus.Ledger.Query (QueryVersion)

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

instance ToGoldenDirectory (QueryVersion, ByronNodeToClientVersion) where
  toGoldenDirectory (queryVersion, blockVersion)
    = show queryVersion </> show blockVersion

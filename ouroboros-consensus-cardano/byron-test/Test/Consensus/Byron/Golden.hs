{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Consensus.Byron.Golden (tests) where

import           Ouroboros.Consensus.Byron.Ledger.NetworkProtocolVersion
import           Ouroboros.Consensus.Byron.Node ()
import           Ouroboros.Consensus.Ledger.Query (QueryVersion)
import           System.FilePath ((</>))
import           Test.Consensus.Byron.Examples
import           Test.Tasty
import           Test.Util.Paths
import           Test.Util.Serialisation.Golden

tests :: TestTree
tests = goldenTest_all codecConfig ($(getGoldenDir) </> "byron") examples

instance ToGoldenDirectory ByronNodeToNodeVersion
  -- Use defaults

instance ToGoldenDirectory (QueryVersion, ByronNodeToClientVersion) where
  toGoldenDirectory (queryVersion, blockVersion)
    = show queryVersion </> show blockVersion

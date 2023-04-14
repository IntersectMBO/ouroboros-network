{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Consensus.Shelley.Golden (tests) where

import           Ouroboros.Consensus.Ledger.Query (QueryVersion)
import           Ouroboros.Consensus.Shelley.Ledger.NetworkProtocolVersion
import           Ouroboros.Consensus.Shelley.Node ()
import           System.FilePath ((</>))
import           Test.Consensus.Shelley.Examples
import           Test.Tasty
import           Test.Util.Paths
import           Test.Util.Serialisation.Golden

tests :: TestTree
tests = goldenTest_all codecConfig ($(getGoldenDir) </> "shelley") examplesShelley

instance ToGoldenDirectory ShelleyNodeToNodeVersion
  -- Use defaults

instance ToGoldenDirectory (QueryVersion, ShelleyNodeToClientVersion) where
  toGoldenDirectory (queryVersion, blockVersion)
    = show queryVersion </> show blockVersion

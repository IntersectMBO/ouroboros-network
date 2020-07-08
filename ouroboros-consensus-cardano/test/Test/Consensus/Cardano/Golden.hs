{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Consensus.Cardano.Golden (tests) where

import           System.FilePath ((</>))

import           Ouroboros.Consensus.HardFork.Combinator.Serialisation

import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Cardano.Node

import           Test.Tasty

import           Test.Util.Serialisation.Golden

import           Test.Consensus.Cardano.Examples

tests :: TestTree
tests = goldenTest_all codecConfig goldenDir examples
  where
    goldenDir = "ouroboros-consensus-cardano" </> "test" </> "golden"

instance ToGoldenDirectory (HardForkNodeToNodeVersion (CardanoEras sc)) where
  toGoldenDirectory v = case v of
    CardanoNodeToNodeVersion1 -> "CardanoNodeToNodeVersion1"
    CardanoNodeToNodeVersion2 -> "CardanoNodeToNodeVersion2"
    _                         -> error $ "Unknown version: " <> show v


instance ToGoldenDirectory (HardForkNodeToClientVersion (CardanoEras sc)) where
  toGoldenDirectory v = case v of
    CardanoNodeToClientVersion1 -> "CardanoNodeToClientVersion1"
    CardanoNodeToClientVersion2 -> "CardanoNodeToClientVersion2"
    _                           -> error $ "Unknown version: " <> show v

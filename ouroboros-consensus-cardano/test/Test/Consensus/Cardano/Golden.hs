{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Consensus.Cardano.Golden (tests) where

import           Ouroboros.Consensus.HardFork.Combinator.Serialisation

import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Cardano.Node
import           Ouroboros.Consensus.Shelley.Protocol.Crypto

import           Test.Tasty

import           Test.Util.Paths
import           Test.Util.Serialisation.Golden

import           Test.Consensus.Cardano.Examples

tests :: TestTree
tests = goldenTest_all codecConfig $(getGoldenDir) examples

instance TPraosCrypto sc => ToGoldenDirectory (HardForkNodeToNodeVersion (CardanoEras sc)) where
  toGoldenDirectory v = case v of
    CardanoNodeToNodeVersion1 -> "CardanoNodeToNodeVersion1"
    CardanoNodeToNodeVersion2 -> "CardanoNodeToNodeVersion2"
    _                         -> error $ "Unknown version: " <> show v


instance TPraosCrypto sc => ToGoldenDirectory (HardForkNodeToClientVersion (CardanoEras sc)) where
  toGoldenDirectory v = case v of
    CardanoNodeToClientVersion1 -> "CardanoNodeToClientVersion1"
    CardanoNodeToClientVersion2 -> "CardanoNodeToClientVersion2"
    _                           -> error $ "Unknown version: " <> show v

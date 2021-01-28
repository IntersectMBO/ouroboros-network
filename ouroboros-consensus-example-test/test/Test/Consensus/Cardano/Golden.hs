{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Consensus.Cardano.Golden (tests) where

import           Ouroboros.Consensus.HardFork.Combinator.Serialisation

import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Cardano.Node

import           Test.Tasty

import           Test.Util.Paths
import           Test.Util.Serialisation.Golden

import           Test.Consensus.Cardano.Examples

tests :: TestTree
tests = goldenTest_all codecConfig $(getGoldenDir) examples

instance CardanoHardForkConstraints c
      => ToGoldenDirectory (HardForkNodeToNodeVersion (CardanoEras c)) where
  toGoldenDirectory v = case v of
    CardanoNodeToNodeVersion1 -> "CardanoNodeToNodeVersion1"
    CardanoNodeToNodeVersion2 -> "CardanoNodeToNodeVersion2"
    CardanoNodeToNodeVersion3 -> "CardanoNodeToNodeVersion3"
    CardanoNodeToNodeVersion4 -> "CardanoNodeToNodeVersion4"
    _                         -> error $ "Unknown version: " <> show v

instance CardanoHardForkConstraints c
      => ToGoldenDirectory (HardForkNodeToClientVersion (CardanoEras c)) where
  toGoldenDirectory v = case v of
    CardanoNodeToClientVersion1 -> "CardanoNodeToClientVersion1"
    CardanoNodeToClientVersion2 -> "CardanoNodeToClientVersion2"
    CardanoNodeToClientVersion3 -> "CardanoNodeToClientVersion3"
    CardanoNodeToClientVersion4 -> "CardanoNodeToClientVersion4"
    CardanoNodeToClientVersion5 -> "CardanoNodeToClientVersion5"
    CardanoNodeToClientVersion6 -> "CardanoNodeToClientVersion6"
    _                           -> error $ "Unknown version: " <> show v

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Consensus.Cardano.Golden (tests) where

import           System.FilePath ((</>))

import           Ouroboros.Consensus.HardFork.Combinator.Serialisation
import           Ouroboros.Consensus.Ledger.Query (QueryVersion)

import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Cardano.Node
import           Ouroboros.Consensus.Shelley.HFEras ()
import           Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()

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
    CardanoNodeToNodeVersion5 -> "CardanoNodeToNodeVersion5"
    CardanoNodeToNodeVersion6 -> "CardanoNodeToNodeVersion6"
    CardanoNodeToNodeVersion7 -> "CardanoNodeToNodeVersion7"
    _                         -> error $ "Unknown version: " <> show v

instance CardanoHardForkConstraints c
      => ToGoldenDirectory (QueryVersion, HardForkNodeToClientVersion (CardanoEras c)) where
  toGoldenDirectory (queryVersion, blockVersion) = show queryVersion </> case blockVersion of
    CardanoNodeToClientVersion1  -> "CardanoNodeToClientVersion1"
    CardanoNodeToClientVersion2  -> "CardanoNodeToClientVersion2"
    CardanoNodeToClientVersion3  -> "CardanoNodeToClientVersion3"
    CardanoNodeToClientVersion4  -> "CardanoNodeToClientVersion4"
    CardanoNodeToClientVersion5  -> "CardanoNodeToClientVersion5"
    CardanoNodeToClientVersion6  -> "CardanoNodeToClientVersion6"
    CardanoNodeToClientVersion7  -> "CardanoNodeToClientVersion7"
    CardanoNodeToClientVersion8  -> "CardanoNodeToClientVersion8"
    CardanoNodeToClientVersion9  -> "CardanoNodeToClientVersion9"
    CardanoNodeToClientVersion10 -> "CardanoNodeToClientVersion10"
    _                            -> error $ "Unknown version: " <> show blockVersion

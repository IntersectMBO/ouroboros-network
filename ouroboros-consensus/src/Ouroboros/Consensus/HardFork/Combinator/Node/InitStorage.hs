{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.HardFork.Combinator.Node.InitStorage () where

import           Data.Functor.Contravariant (contramap)
import           Data.SOP.Strict

import           Ouroboros.Consensus.Node.InitStorage

import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import           Ouroboros.Consensus.HardFork.Combinator.Basics

instance CanHardFork xs => NodeInitStorage (HardForkBlock xs) where
  -- We use the chunk info from the first era
  nodeImmutableDbChunkInfo cfg =
      case isNonEmpty (Proxy @xs) of
        ProofNonEmpty {} ->
          nodeImmutableDbChunkInfo
            (hd cfgs)
    where
      cfgs = getPerEraStorageConfig (hardForkStorageConfigPerEra cfg)

  -- Dispatch based on the era
  nodeCheckIntegrity cfg (HardForkBlock (OneEraBlock blk)) =
      case isNonEmpty (Proxy @xs) of
        ProofNonEmpty {} ->
          hcollapse $
            hczipWith (Proxy @SingleEraBlock) aux cfgs blk
    where
      cfgs = getPerEraStorageConfig (hardForkStorageConfigPerEra cfg)

      aux :: NodeInitStorage blk => StorageConfig blk -> I blk -> K Bool blk
      aux cfg' (I blk') = K $ nodeCheckIntegrity cfg' blk'

  -- Let the first era initialise the ChainDB
  nodeInitChainDB cfg initChainDB =
      case isNonEmpty (Proxy @xs) of
        ProofNonEmpty {} ->
          nodeInitChainDB
            (hd cfgs)
            (contramap (HardForkBlock . OneEraBlock . Z . I) initChainDB)
    where
      cfgs = getPerEraStorageConfig (hardForkStorageConfigPerEra cfg)

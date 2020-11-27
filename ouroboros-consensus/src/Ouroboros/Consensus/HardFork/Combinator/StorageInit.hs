{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.HardFork.Combinator.StorageInit () where

import           Data.Functor.Contravariant (contramap)
import           Data.SOP.Strict

import           Ouroboros.Consensus.Storage.Init

import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import           Ouroboros.Consensus.HardFork.Combinator.Basics

instance CanHardFork xs => StorageInit (HardForkBlock xs) where
  -- We use the chunk info from the first era
  immutableDbChunkInfo cfg =
      case isNonEmpty (Proxy @xs) of
        ProofNonEmpty {} ->
          immutableDbChunkInfo
            (hd cfgs)
    where
      cfgs = getPerEraStorageConfig (hardForkStorageConfigPerEra cfg)

  -- Dispatch based on the era
  checkIntegrity cfg (HardForkBlock (OneEraBlock blk)) =
      case isNonEmpty (Proxy @xs) of
        ProofNonEmpty {} ->
          hcollapse $
            hczipWith (Proxy @SingleEraBlock) aux cfgs blk
    where
      cfgs = getPerEraStorageConfig (hardForkStorageConfigPerEra cfg)

      aux :: StorageInit blk => StorageConfig blk -> I blk -> K Bool blk
      aux cfg' (I blk') = K $ checkIntegrity cfg' blk'

  -- Let the first era initialise the ChainDB
  initChainDB cfg initialiser =
      case isNonEmpty (Proxy @xs) of
        ProofNonEmpty {} ->
          initChainDB
            (hd cfgs)
            (contramap (HardForkBlock . OneEraBlock . Z . I) initialiser)
    where
      cfgs = getPerEraStorageConfig (hardForkStorageConfigPerEra cfg)

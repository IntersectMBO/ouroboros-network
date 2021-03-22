{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.HardFork.Combinator.Node.Metrics (

  ) where

import           Data.SOP.Strict

import           Ouroboros.Consensus.Block.SupportsMetrics
import           Ouroboros.Consensus.Util

import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import           Ouroboros.Consensus.HardFork.Combinator.Basics
import           Ouroboros.Consensus.HardFork.Combinator.Block

instance CanHardFork xs => BlockSupportsMetrics (HardForkBlock xs) where
  isSelfIssued cfg hdr =
        hcollapse
      $ hczipWith
          proxySingle
          (K .: isSelfIssued)
          (getPerEraBlockConfig $ hardForkBlockConfigPerEra cfg)
          (getOneEraHeader      $ getHardForkHeader         hdr)

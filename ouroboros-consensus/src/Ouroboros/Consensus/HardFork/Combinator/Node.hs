{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.HardFork.Combinator.Node () where

import           Data.Proxy
import           Data.SOP.Strict
import           GHC.Stack

import           Ouroboros.Consensus.Config.SupportsNode

import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import           Ouroboros.Consensus.HardFork.Combinator.Basics

{-------------------------------------------------------------------------------
  ConfigSupportsNode
-------------------------------------------------------------------------------}

instance (All ConfigSupportsNode xs, IsNonEmpty xs)
      => ConfigSupportsNode (HardForkBlock xs) where
  getSystemStart  = getSameConfigValue getSystemStart
  getNetworkMagic = getSameConfigValue getNetworkMagic

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

getSameConfigValue
  :: forall xs a.
     (All ConfigSupportsNode xs, IsNonEmpty xs, Eq a, HasCallStack)
  => (forall blk. ConfigSupportsNode blk => BlockConfig blk -> a)
  -> BlockConfig (HardForkBlock xs)
  -> a
getSameConfigValue getValue blockConfig = getSameValue values
  where
    values :: NP (K a) xs
    values =
          hcmap (Proxy @ConfigSupportsNode) (K . getValue)
        . getPerEraBlockConfig
        . hardForkBlockConfigPerEra
        $ blockConfig

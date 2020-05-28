{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.HardFork.Combinator.Node (
    -- * Type family instances
    CodecConfig(..)
  ) where

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
  newtype CodecConfig (HardForkBlock xs) = HardForkCodecConfig {
        hardForkCodecConfigPerEra :: PerEraCodecConfig xs
      }

  getCodecConfig =
        HardForkCodecConfig
      . PerEraCodecConfig
      . hcmap (Proxy @ConfigSupportsNode) getCodecConfig
      . getPerEraBlockConfig
      . hardForkBlockConfigPerEra

  getSystemStart     = getSameConfigValue getSystemStart
  getNetworkMagic    = getSameConfigValue getNetworkMagic
  getProtocolMagicId = getSameConfigValue getProtocolMagicId

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

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

import           Control.Monad.Except (throwError)
import           Data.Proxy
import           Data.SOP.Strict

import           Ouroboros.Consensus.Config.SupportsNode
import           Ouroboros.Consensus.Util (allEqual)
import           Ouroboros.Consensus.Util.Assert

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

  getSystemStart     = getSameValue "getSystemStart"     getSystemStart
  getNetworkMagic    = getSameValue "getNetworkMagic"    getNetworkMagic
  getProtocolMagicId = getSameValue "getProtocolMagicId" getProtocolMagicId

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

getSameValue
  :: forall xs a. (All ConfigSupportsNode xs, IsNonEmpty xs, Eq a)
  => String
  -> (forall blk. ConfigSupportsNode blk => BlockConfig blk -> a)
  -> BlockConfig (HardForkBlock xs)
  -> a
getSameValue label getValue blockConfig =
    case isNonEmpty (Proxy @xs) of
      ProofNonEmpty _ ->
        assertWithMsg allEqualCheck (unK (hd values))
  where
    values :: NP (K a) xs
    values =
          hcmap (Proxy @ConfigSupportsNode) (K . getValue)
        . getPerEraBlockConfig
        . hardForkBlockConfigPerEra
        $ blockConfig

    allEqualCheck :: Either String ()
    allEqualCheck
        | allEqual (hcollapse values)
        = return ()
        | otherwise
        = throwError $ "differing values for " <> label <> " across hard fork"

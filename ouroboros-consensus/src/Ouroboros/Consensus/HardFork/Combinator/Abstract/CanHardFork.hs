{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Ouroboros.Consensus.HardFork.Combinator.Abstract.CanHardFork (
    CanHardFork(..)
  ) where

import           Data.SOP.Strict
import           Data.Typeable

import           Ouroboros.Consensus.Util.SOP

import           Ouroboros.Consensus.HardFork.Combinator.Abstract.SingleEraBlock
import           Ouroboros.Consensus.HardFork.Combinator.Translation

{-------------------------------------------------------------------------------
  CanHardFork
-------------------------------------------------------------------------------}

class (All SingleEraBlock xs, Typeable xs, IsNonEmpty xs) => CanHardFork xs where
  hardForkEraTranslation :: EraTranslation xs

instance SingleEraBlock blk => CanHardFork '[blk] where
  hardForkEraTranslation = trivialEraTranslation

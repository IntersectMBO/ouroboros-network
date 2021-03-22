{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Ouroboros.Consensus.HardFork.Combinator.Abstract.CanHardFork (
    CanHardFork (..)
  ) where

import           Data.SOP.Strict
import           Data.Typeable

import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.Util.SOP

import           Ouroboros.Consensus.HardFork.Combinator.Abstract.SingleEraBlock
import           Ouroboros.Consensus.HardFork.Combinator.InjectTxs
import           Ouroboros.Consensus.HardFork.Combinator.Protocol.ChainSel
import           Ouroboros.Consensus.HardFork.Combinator.Translation
import           Ouroboros.Consensus.HardFork.Combinator.Util.InPairs (InPairs,
                     RequiringBoth)
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.InPairs as InPairs
import           Ouroboros.Consensus.HardFork.Combinator.Util.Tails (Tails)
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.Tails as Tails

{-------------------------------------------------------------------------------
  CanHardFork
-------------------------------------------------------------------------------}

class (All SingleEraBlock xs, Typeable xs, IsNonEmpty xs) => CanHardFork xs where
  hardForkEraTranslation :: EraTranslation xs
  hardForkChainSel       :: Tails AcrossEraSelection xs
  hardForkInjectTxs      :: InPairs (RequiringBoth WrapLedgerConfig InjectTx) xs

instance SingleEraBlock blk => CanHardFork '[blk] where
  hardForkEraTranslation = trivialEraTranslation
  hardForkChainSel       = Tails.mk1
  hardForkInjectTxs      = InPairs.mk1

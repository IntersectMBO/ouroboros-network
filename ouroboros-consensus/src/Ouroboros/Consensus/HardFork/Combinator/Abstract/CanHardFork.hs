{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Ouroboros.Consensus.HardFork.Combinator.Abstract.CanHardFork (CanHardFork (..)) where

import           Data.SOP.Functors (Product2)
import           Data.SOP.InPairs (InPairs, RequiringBoth)
import qualified Data.SOP.InPairs as InPairs
import           Data.SOP.NonEmpty
import           Data.SOP.Strict
import           Data.SOP.Tails (Tails)
import qualified Data.SOP.Tails as Tails
import           Data.Typeable

import           Ouroboros.Consensus.TypeFamilyWrappers

import           Ouroboros.Consensus.HardFork.Combinator.Abstract.SingleEraBlock
import           Ouroboros.Consensus.HardFork.Combinator.InjectTxs
import           Ouroboros.Consensus.HardFork.Combinator.Protocol.ChainSel
import           Ouroboros.Consensus.HardFork.Combinator.Translation
{-------------------------------------------------------------------------------
  CanHardFork
-------------------------------------------------------------------------------}

class (All SingleEraBlock xs, Typeable xs, IsNonEmpty xs) => CanHardFork xs where
  hardForkEraTranslation :: EraTranslation xs
  hardForkChainSel       :: Tails AcrossEraSelection xs
  hardForkInjectTxs      ::
    InPairs
      ( RequiringBoth
          WrapLedgerConfig
          (Product2 InjectTx InjectValidatedTx)
      )
      xs

instance SingleEraBlock blk => CanHardFork '[blk] where
  hardForkEraTranslation = trivialEraTranslation
  hardForkChainSel       = Tails.mk1
  hardForkInjectTxs      = InPairs.mk1

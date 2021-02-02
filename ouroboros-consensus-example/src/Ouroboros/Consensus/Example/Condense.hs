{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.Example.Condense () where

import           Ouroboros.Consensus.HardFork.Combinator.Condense

import           Ouroboros.Consensus.Shelley.Ledger

import           Ouroboros.Consensus.Example.Block
import           Ouroboros.Consensus.Example.CanHardFork

{-------------------------------------------------------------------------------
  Condense

  TODO where to put this?
-------------------------------------------------------------------------------}

instance ShelleyBasedEra era => CondenseConstraints (ShelleyBlock era)

instance ExampleHardForkConstraints c => CondenseConstraints (ExampleBlock c)

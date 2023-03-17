{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.Cardano.Condense () where

import           Ouroboros.Consensus.Byron.Ledger
import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Cardano.CanHardFork
import           Ouroboros.Consensus.HardFork.Combinator.Condense
import           Ouroboros.Consensus.Shelley.Ledger

{-------------------------------------------------------------------------------
  Condense

  TODO where to put this?
-------------------------------------------------------------------------------}

instance CondenseConstraints ByronBlock

instance ShelleyCompatible proto era => CondenseConstraints (ShelleyBlock proto era)

instance CardanoHardForkConstraints c1 c2 => CondenseConstraints (CardanoBlock c1 c2)

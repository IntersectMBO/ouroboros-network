module Ouroboros.Consensus.HardFork.Combinator.Abstract.NoHardForks (
    NoHardForks (..)
  , noHardForksEpochInfo
  ) where

import           Cardano.Slotting.EpochInfo
import           Data.Functor.Identity (runIdentity)
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.HardFork.Combinator.Abstract.SingleEraBlock
import           Ouroboros.Consensus.HardFork.Combinator.PartialConfig
import           Ouroboros.Consensus.HardFork.History as History
import           Ouroboros.Consensus.Ledger.Abstract

{-------------------------------------------------------------------------------
  Blocks that don't /have/ any transitions
-------------------------------------------------------------------------------}

class SingleEraBlock blk => NoHardForks blk where
  -- | Extract 'EraParams' from the top-level config
  --
  -- The HFC itself does not care about this, as it must be given the full shape
  -- across /all/ eras.
  getEraParams :: TopLevelConfig blk -> EraParams


  -- | Construct partial ledger config from full ledger config
  --
  -- See also 'toPartialConsensusConfig'
  toPartialLedgerConfig :: proxy blk
                        -> LedgerConfig blk -> PartialLedgerConfig blk

noHardForksEpochInfo :: (Monad m, NoHardForks blk)
                     => TopLevelConfig blk
                     -> EpochInfo m
noHardForksEpochInfo cfg =
      hoistEpochInfo (pure . runIdentity)
    $ fixedEpochInfo
        (History.eraEpochSize  params)
        (History.eraSlotLength params)
  where
    params :: EraParams
    params = getEraParams cfg

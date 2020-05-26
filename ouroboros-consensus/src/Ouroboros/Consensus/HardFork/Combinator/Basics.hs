{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

module Ouroboros.Consensus.HardFork.Combinator.Basics (
    -- * Hard fork protocol, block, and ledger state
    HardForkProtocol
  , HardForkBlock(..)
  , LedgerState(..)
    -- * Config
  , ConsensusConfig(..)
  , BlockConfig(..)
  , HardForkLedgerConfig(..)
    -- ** Functions on config
  , completeLedgerConfig'
  , completeConsensusConfig'
  , distribTopLevelConfig
    -- ** Convenience re-exports
  , EpochInfo
  , Identity
  ) where

import           Data.Functor.Identity
import           Data.SOP.Strict
import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks)
import           Cardano.Slotting.EpochInfo

import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Config.SecurityParam
import qualified Ouroboros.Consensus.HardFork.History as History
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Protocol.Abstract

import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import           Ouroboros.Consensus.HardFork.Combinator.PartialConfig
import           Ouroboros.Consensus.HardFork.Combinator.State.Infra

{-------------------------------------------------------------------------------
  Hard fork protocol, block, and ledger state
-------------------------------------------------------------------------------}

data HardForkProtocol (xs :: [*])

newtype HardForkBlock xs = HardForkBlock {
      getHardForkBlock :: OneEraBlock xs
    }
  deriving (Show)

type instance BlockProtocol (HardForkBlock xs) = HardForkProtocol xs

newtype instance LedgerState (HardForkBlock xs) = HardForkLedgerState {
      getHardForkLedgerState :: HardForkState LedgerState xs
    }
  deriving stock   (Show, Eq)
  deriving newtype (NoUnexpectedThunks)

{-------------------------------------------------------------------------------
  Protocol config
-------------------------------------------------------------------------------}

data instance ConsensusConfig (HardForkProtocol xs) = HardForkConsensusConfig {
      -- | The value of @k@ cannot change at hard fork boundaries
      hardForkConsensusConfigK :: !(SecurityParam)

      -- | The shape of the hard fork
      --
      -- We require this in the consensus config because consensus might need
      -- access to 'EpochInfo', and in order to compute that, we need the
      -- 'EraParams' of all eras.
    , hardForkConsensusConfigShape :: !(History.Shape xs)

      -- | Config for each era
    , hardForkConsensusConfigPerEra :: !(PerEraConsensusConfig xs)
    }
  deriving stock    (Generic)
  deriving anyclass (NoUnexpectedThunks)

{-------------------------------------------------------------------------------
  Block config
-------------------------------------------------------------------------------}

newtype instance BlockConfig (HardForkBlock xs) = HardForkBlockConfig {
      hardForkBlockConfigPerEra :: PerEraBlockConfig xs
    }
  deriving newtype (NoUnexpectedThunks)

{-------------------------------------------------------------------------------
  Ledger config
-------------------------------------------------------------------------------}

data HardForkLedgerConfig xs = HardForkLedgerConfig {
      hardForkLedgerConfigK      :: !SecurityParam
    , hardForkLedgerConfigShape  :: !(History.Shape xs)
    , hardForkLedgerConfigPerEra :: !(PerEraLedgerConfig xs)
    }
  deriving (Generic)

instance CanHardFork xs => NoUnexpectedThunks (HardForkLedgerConfig xs)

type instance LedgerCfg (LedgerState (HardForkBlock xs)) = HardForkLedgerConfig xs

{-------------------------------------------------------------------------------
  Operations on config
-------------------------------------------------------------------------------}

completeLedgerConfig' :: forall blk.
                         HasPartialLedgerConfig blk
                      => EpochInfo Identity
                      -> WrapPartialLedgerConfig blk
                      -> LedgerConfig blk
completeLedgerConfig' ei =
      completeLedgerConfig (Proxy @blk) ei
    . unwrapPartialLedgerConfig

completeConsensusConfig' :: forall blk.
                            HasPartialConsensusConfig (BlockProtocol blk)
                         => EpochInfo Identity
                         -> WrapPartialConsensusConfig blk
                         -> ConsensusConfig (BlockProtocol blk)
completeConsensusConfig' ei =
      completeConsensusConfig (Proxy @(BlockProtocol blk)) ei
    . unwrapPartialConsensusConfig

distribTopLevelConfig :: CanHardFork xs
                      => EpochInfo Identity
                      -> TopLevelConfig (HardForkBlock xs)
                      -> NP TopLevelConfig xs
distribTopLevelConfig ei TopLevelConfig{..} =
    hczipWith3 proxySingle
      (\cfgConsensus cfgLedger cfgBlock ->
           TopLevelConfig
             (completeConsensusConfig' ei cfgConsensus)
             (completeLedgerConfig'    ei cfgLedger)
             cfgBlock)
      (getPerEraConsensusConfig $
         hardForkConsensusConfigPerEra configConsensus)
      (getPerEraLedgerConfig $
         hardForkLedgerConfigPerEra configLedger)
      (getPerEraBlockConfig $
         hardForkBlockConfigPerEra configBlock)

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
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Ouroboros.Consensus.HardFork.Combinator.Basics (
    -- * Hard fork protocol, block, and ledger state
    HardForkProtocol
  , HardForkBlock(..)
  , LedgerState(..)
    -- * Config
  , ConsensusConfig(..)
  , BlockConfig(..)
  , CodecConfig(..)
  , HardForkLedgerConfig(..)
    -- ** Functions on config
  , completeLedgerConfig'
  , completeLedgerConfig''
  , completeConsensusConfig'
  , completeConsensusConfig''
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
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.SOP

import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import           Ouroboros.Consensus.HardFork.Combinator.PartialConfig
import           Ouroboros.Consensus.HardFork.Combinator.State.Instances ()
import           Ouroboros.Consensus.HardFork.Combinator.State.Types

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

deriving stock   instance CanHardFork xs => Show (LedgerState (HardForkBlock xs))
deriving stock   instance CanHardFork xs => Eq   (LedgerState (HardForkBlock xs))
deriving newtype instance CanHardFork xs => NoUnexpectedThunks (LedgerState (HardForkBlock xs))

{-------------------------------------------------------------------------------
  Chain independent state
-------------------------------------------------------------------------------}

instance CanHardFork xs => HasChainIndepState (HardForkProtocol xs) where
  type ChainIndepStateConfig (HardForkProtocol xs) = PerEraChainIndepStateConfig xs
  type ChainIndepState       (HardForkProtocol xs) = PerEraChainIndepState       xs

  -- Operations on the chain independent state

  updateChainIndepState _ cfg slot =
        fmap PerEraChainIndepState
      . hsequence'
      . hczipWith proxySingle updateOne cfgs
      . getPerEraChainIndepState
    where
      cfgs = getPerEraChainIndepStateConfig cfg

      updateOne ::
           forall m blk. (SingleEraBlock blk, IOLike m)
        => WrapChainIndepStateConfig blk
        -> WrapChainIndepState blk
        -> (m :.: WrapChainIndepState) blk
      updateOne (WrapChainIndepStateConfig cfg') (WrapChainIndepState st) =
        Comp $
          WrapChainIndepState <$>
            updateChainIndepState (Proxy @(BlockProtocol blk)) cfg' slot st

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
  Codec config
-------------------------------------------------------------------------------}

newtype instance CodecConfig (HardForkBlock xs) = HardForkCodecConfig {
      hardForkCodecConfigPerEra :: PerEraCodecConfig xs
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

completeLedgerConfig'' :: forall blk.
                          HasPartialLedgerConfig blk
                       => EpochInfo Identity
                       -> WrapPartialLedgerConfig blk
                       -> WrapLedgerConfig blk
completeLedgerConfig'' ei =
      WrapLedgerConfig
    . completeLedgerConfig (Proxy @blk) ei
    . unwrapPartialLedgerConfig

completeConsensusConfig' :: forall blk.
                            HasPartialConsensusConfig (BlockProtocol blk)
                         => EpochInfo Identity
                         -> WrapPartialConsensusConfig blk
                         -> ConsensusConfig (BlockProtocol blk)
completeConsensusConfig' ei =
      completeConsensusConfig (Proxy @(BlockProtocol blk)) ei
    . unwrapPartialConsensusConfig

completeConsensusConfig'' :: forall blk.
                             HasPartialConsensusConfig (BlockProtocol blk)
                          => EpochInfo Identity
                          -> WrapPartialConsensusConfig blk
                          -> WrapConsensusConfig blk
completeConsensusConfig'' ei =
      WrapConsensusConfig
    . completeConsensusConfig (Proxy @(BlockProtocol blk)) ei
    . unwrapPartialConsensusConfig

distribTopLevelConfig :: CanHardFork xs
                      => EpochInfo Identity
                      -> TopLevelConfig (HardForkBlock xs)
                      -> NP TopLevelConfig xs
distribTopLevelConfig ei TopLevelConfig{..} =
    hcpure proxySingle
      (fn_5 (\cfgConsensus cfgIndep cfgLedger cfgBlock cfgCodec ->
           TopLevelConfig
             (completeConsensusConfig' ei cfgConsensus)
             (unwrapChainIndepStateConfig cfgIndep)
             (completeLedgerConfig'    ei cfgLedger)
             cfgBlock
             cfgCodec))
    `hap`
      (getPerEraConsensusConfig $
         hardForkConsensusConfigPerEra configConsensus)
    `hap`
      (getPerEraChainIndepStateConfig configIndep)
    `hap`
      (getPerEraLedgerConfig $
         hardForkLedgerConfigPerEra configLedger)
    `hap`
      (getPerEraBlockConfig $
         hardForkBlockConfigPerEra configBlock)
    `hap`
      (getPerEraCodecConfig $
         hardForkCodecConfigPerEra configCodec)

{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Ouroboros.Consensus.HardFork.Combinator.Basics (
    -- * Hard fork protocol, block, and ledger state
    HardForkBlock (..)
  , HardForkProtocol
  , LedgerState (..)
    -- * Config
  , BlockConfig (..)
  , CodecConfig (..)
  , ConsensusConfig (..)
  , HardForkLedgerConfig (..)
  , StorageConfig (..)
    -- ** Functions on config
  , completeConsensusConfig'
  , completeConsensusConfig''
  , completeLedgerConfig'
  , completeLedgerConfig''
  , distribLedgerConfig
  , distribTopLevelConfig
    -- ** Convenience re-exports
  , EpochInfo
  , Except
    -- ** Overrides
  , Overrides
  , hardForkMaxTxCapacityOverrideToNP
  ) where

import           Data.Kind (Type)
import           Data.SOP.Strict
import           Data.Typeable
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)

import           Cardano.Slotting.EpochInfo

import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Block.Forging
import           Ouroboros.Consensus.Config
import qualified Ouroboros.Consensus.HardFork.History as History
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.Util (ShowProxy)
import           Ouroboros.Consensus.Util.SOP (fn_5)

import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import           Ouroboros.Consensus.HardFork.Combinator.PartialConfig
import           Ouroboros.Consensus.HardFork.Combinator.State.Instances ()
import           Ouroboros.Consensus.HardFork.Combinator.State.Types

{-------------------------------------------------------------------------------
  Hard fork protocol, block, and ledger state
-------------------------------------------------------------------------------}

data HardForkProtocol (xs :: [Type])

newtype HardForkBlock xs = HardForkBlock {
      getHardForkBlock :: OneEraBlock xs
    }
  deriving (Show)

instance Typeable xs => ShowProxy (HardForkBlock xs) where

type instance BlockProtocol (HardForkBlock xs) = HardForkProtocol xs
type instance HeaderHash    (HardForkBlock xs) = OneEraHash       xs

newtype instance LedgerState (HardForkBlock xs) = HardForkLedgerState {
      hardForkLedgerStatePerEra :: HardForkState LedgerState xs
    }

deriving stock   instance CanHardFork xs => Show (LedgerState (HardForkBlock xs))
deriving stock   instance CanHardFork xs => Eq   (LedgerState (HardForkBlock xs))
deriving newtype instance CanHardFork xs => NoThunks (LedgerState (HardForkBlock xs))

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
  deriving anyclass (NoThunks)

{-------------------------------------------------------------------------------
  Block config
-------------------------------------------------------------------------------}

newtype instance BlockConfig (HardForkBlock xs) = HardForkBlockConfig {
      hardForkBlockConfigPerEra :: PerEraBlockConfig xs
    }
  deriving newtype (NoThunks)

{-------------------------------------------------------------------------------
  Codec config
-------------------------------------------------------------------------------}

newtype instance CodecConfig (HardForkBlock xs) = HardForkCodecConfig {
      hardForkCodecConfigPerEra :: PerEraCodecConfig xs
    }
  deriving newtype (NoThunks)

{-------------------------------------------------------------------------------
  Storage config
-------------------------------------------------------------------------------}

newtype instance StorageConfig (HardForkBlock xs) = HardForkStorageConfig {
      hardForkStorageConfigPerEra :: PerEraStorageConfig xs
    }
  deriving newtype (NoThunks)

{-------------------------------------------------------------------------------
  Forging
-------------------------------------------------------------------------------}

type instance Overrides (HardForkBlock xs) = NP MaxTxCapacityOverride xs

hardForkMaxTxCapacityOverrideToNP ::
     (All Top xs)
  => MaxTxCapacityOverride (HardForkBlock xs)
  -> NP MaxTxCapacityOverride xs
hardForkMaxTxCapacityOverrideToNP = \case
  NoMaxTxCapacityOverride -> hpure NoMaxTxCapacityOverride
  MaxTxCapacityOverride v -> v

{-------------------------------------------------------------------------------
  Ledger config
-------------------------------------------------------------------------------}

data HardForkLedgerConfig xs = HardForkLedgerConfig {
      hardForkLedgerConfigShape  :: !(History.Shape xs)
    , hardForkLedgerConfigPerEra :: !(PerEraLedgerConfig xs)
    }
  deriving (Generic)

instance CanHardFork xs => NoThunks (HardForkLedgerConfig xs)

type instance LedgerCfg (LedgerState (HardForkBlock xs)) = HardForkLedgerConfig xs

{-------------------------------------------------------------------------------
  Operations on config
-------------------------------------------------------------------------------}

completeLedgerConfig' :: forall blk.
                         HasPartialLedgerConfig blk
                      => EpochInfo (Except PastHorizonException)
                      -> WrapPartialLedgerConfig blk
                      -> LedgerConfig blk
completeLedgerConfig' ei =
      completeLedgerConfig (Proxy @blk) ei
    . unwrapPartialLedgerConfig

completeLedgerConfig'' :: forall blk.
                          HasPartialLedgerConfig blk
                       => EpochInfo (Except PastHorizonException)
                       -> WrapPartialLedgerConfig blk
                       -> WrapLedgerConfig blk
completeLedgerConfig'' ei =
      WrapLedgerConfig
    . completeLedgerConfig (Proxy @blk) ei
    . unwrapPartialLedgerConfig

completeConsensusConfig' :: forall blk.
                            HasPartialConsensusConfig (BlockProtocol blk)
                         => EpochInfo (Except PastHorizonException)
                         -> WrapPartialConsensusConfig blk
                         -> ConsensusConfig (BlockProtocol blk)
completeConsensusConfig' ei =
      completeConsensusConfig (Proxy @(BlockProtocol blk)) ei
    . unwrapPartialConsensusConfig

completeConsensusConfig'' :: forall blk.
                             HasPartialConsensusConfig (BlockProtocol blk)
                          => EpochInfo (Except PastHorizonException)
                          -> WrapPartialConsensusConfig blk
                          -> WrapConsensusConfig blk
completeConsensusConfig'' ei =
      WrapConsensusConfig
    . completeConsensusConfig (Proxy @(BlockProtocol blk)) ei
    . unwrapPartialConsensusConfig

distribLedgerConfig ::
     CanHardFork xs
  => EpochInfo (Except PastHorizonException)
  -> LedgerConfig (HardForkBlock xs)
  -> NP WrapLedgerConfig xs
distribLedgerConfig ei cfg =
    hcmap
      proxySingle
      (completeLedgerConfig'' ei)
      (getPerEraLedgerConfig $ hardForkLedgerConfigPerEra cfg)

distribTopLevelConfig :: All SingleEraBlock xs
                      => EpochInfo (Except PastHorizonException)
                      -> TopLevelConfig (HardForkBlock xs)
                      -> NP TopLevelConfig xs
distribTopLevelConfig ei tlc =
    hcpure proxySingle
      (fn_5 (\cfgConsensus cfgLedger cfgBlock cfgCodec cfgStorage ->
           mkTopLevelConfig
             (completeConsensusConfig' ei cfgConsensus)
             (completeLedgerConfig'    ei cfgLedger)
             cfgBlock
             cfgCodec
             cfgStorage))
    `hap`
      (getPerEraConsensusConfig $
         hardForkConsensusConfigPerEra (configConsensus tlc))
    `hap`
      (getPerEraLedgerConfig $
         hardForkLedgerConfigPerEra (configLedger tlc))
    `hap`
      (getPerEraBlockConfig $
         hardForkBlockConfigPerEra (configBlock tlc))
    `hap`
      (getPerEraCodecConfig $
         hardForkCodecConfigPerEra (configCodec tlc))
    `hap`
      (getPerEraStorageConfig $
         hardForkStorageConfigPerEra (configStorage tlc))

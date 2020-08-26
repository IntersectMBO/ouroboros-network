{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Ouroboros.Consensus.HardFork.Combinator.PartialConfig (
    HasPartialConsensusConfig(..)
  , HasPartialLedgerConfig(..)
    -- * Newtype wrappers
  , WrapPartialLedgerConfig(..)
  , WrapPartialConsensusConfig(..)
    -- * Convenience re-exports
  , EpochInfo(..)
  , Identity(..)
  ) where

import           Data.Functor.Identity
import           Data.Kind (Type)

import           Cardano.Prelude (NoUnexpectedThunks)
import           Cardano.Slotting.EpochInfo

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Protocol.Abstract

-- | Partial consensus config
class ( ConsensusProtocol p
      , NoUnexpectedThunks (PartialConsensusConfig p)
      ) => HasPartialConsensusConfig p where
  type PartialConsensusConfig p :: Type
  type PartialConsensusConfig p = ConsensusConfig p

  -- | Construct 'ConsensusConfig' from 'PartialConsensusConfig'
  --
  -- See comments for 'completeLedgerConfig' for some details about the
  -- 'EpochInfo'.
  completeConsensusConfig :: proxy p
                          -> EpochInfo Identity
                          -> PartialConsensusConfig p -> ConsensusConfig p

  default completeConsensusConfig :: (PartialConsensusConfig p ~ ConsensusConfig p)
                                  => proxy p
                                  -> EpochInfo Identity
                                  -> PartialConsensusConfig p -> ConsensusConfig p
  completeConsensusConfig _ _ = id

  -- Some functions will not be reused from ConsensusProtocol directly:

  -- | The 'ChainSelConfig' must be derivable from the partial config
  partialChainSelConfig :: proxy p -> PartialConsensusConfig p -> ChainSelConfig p
  default partialChainSelConfig :: (PartialConsensusConfig p ~ ConsensusConfig p)
                                => proxy p
                                -> PartialConsensusConfig p -> ChainSelConfig p
  partialChainSelConfig _ = chainSelConfig

-- | Partial ledger config
class ( UpdateLedger blk
      , NoUnexpectedThunks (PartialLedgerConfig blk)
      ) => HasPartialLedgerConfig blk where
  type PartialLedgerConfig blk :: Type
  type PartialLedgerConfig blk = LedgerConfig blk

  -- | Construct 'LedgerConfig' from 'PartialLedgerCfg'
  --
  -- NOTE: The 'EpochInfo' provided will have limited range, any attempt to
  -- look past its horizon will result in a pure 'PastHorizonException'.
  -- The horizon is determined by the tip of the ledger /state/ (not view)
  -- from which the 'EpochInfo' is derived.
  --
  -- TODO: This should not be Identity;
  -- see <https://github.com/input-output-hk/ouroboros-network/issues/2126>
  completeLedgerConfig :: proxy blk
                       -> EpochInfo Identity
                       -> PartialLedgerConfig blk  -> LedgerConfig blk
  default completeLedgerConfig :: (PartialLedgerConfig blk ~ LedgerConfig blk)
                               => proxy blk
                               -> EpochInfo Identity
                               -> PartialLedgerConfig blk  -> LedgerConfig blk
  completeLedgerConfig _ _ = id

{-------------------------------------------------------------------------------
  Newtype wrappers
-------------------------------------------------------------------------------}

newtype WrapPartialLedgerConfig    blk = WrapPartialLedgerConfig    { unwrapPartialLedgerConfig    :: PartialLedgerConfig                   blk  }
newtype WrapPartialConsensusConfig blk = WrapPartialConsensusConfig { unwrapPartialConsensusConfig :: PartialConsensusConfig (BlockProtocol blk) }

deriving instance NoUnexpectedThunks (PartialLedgerConfig                   blk)  => NoUnexpectedThunks (WrapPartialLedgerConfig    blk)
deriving instance NoUnexpectedThunks (PartialConsensusConfig (BlockProtocol blk)) => NoUnexpectedThunks (WrapPartialConsensusConfig blk)

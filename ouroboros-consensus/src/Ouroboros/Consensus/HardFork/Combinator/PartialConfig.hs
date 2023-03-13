{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Ouroboros.Consensus.HardFork.Combinator.PartialConfig (
    HasPartialConsensusConfig (..)
  , HasPartialLedgerConfig (..)
    -- * Newtype wrappers
  , WrapPartialConsensusConfig (..)
  , WrapPartialLedgerConfig (..)
    -- * Convenience re-exports
  , EpochInfo (..)
  , Except
  , PastHorizonException
  ) where

import           Cardano.Slotting.EpochInfo
import           Control.Monad.Except (Except)
import           Data.Kind (Type)
import           NoThunks.Class (NoThunks)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HardFork.History.Qry (PastHorizonException)
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Protocol.Abstract

-- | Partial consensus config
class ( ConsensusProtocol p
      , NoThunks (PartialConsensusConfig p)
      ) => HasPartialConsensusConfig p where
  type PartialConsensusConfig p :: Type
  type PartialConsensusConfig p = ConsensusConfig p

  -- | Construct 'ConsensusConfig' from 'PartialConsensusConfig'
  --
  -- See comments for 'completeLedgerConfig' for some details about the
  -- 'EpochInfo'.
  completeConsensusConfig :: proxy p
                          -> EpochInfo (Except PastHorizonException)
                          -> PartialConsensusConfig p -> ConsensusConfig p

  default completeConsensusConfig :: (PartialConsensusConfig p ~ ConsensusConfig p)
                                  => proxy p
                                  -> EpochInfo (Except PastHorizonException)
                                  -> PartialConsensusConfig p -> ConsensusConfig p
  completeConsensusConfig _ _ = id

  -- | Construct partial consensus config from full consensus config
  --
  -- NOTE: This is basically just losing 'EpochInfo', but that is constant
  -- anyway when we are dealing with a single era.
  toPartialConsensusConfig :: proxy p
                           -> ConsensusConfig p
                           -> PartialConsensusConfig p
  default toPartialConsensusConfig
    :: (PartialConsensusConfig p ~ ConsensusConfig p)
    => proxy p
    -> ConsensusConfig p
    -> PartialConsensusConfig p
  toPartialConsensusConfig _ = id

-- | Partial ledger config
class ( UpdateLedger blk
      , NoThunks (PartialLedgerConfig blk)
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
  completeLedgerConfig :: proxy blk
                       -> EpochInfo (Except PastHorizonException)
                       -> PartialLedgerConfig blk  -> LedgerConfig blk
  default completeLedgerConfig :: (PartialLedgerConfig blk ~ LedgerConfig blk)
                               => proxy blk
                               -> EpochInfo (Except PastHorizonException)
                               -> PartialLedgerConfig blk  -> LedgerConfig blk
  completeLedgerConfig _ _ = id

{-------------------------------------------------------------------------------
  Newtype wrappers
-------------------------------------------------------------------------------}

newtype WrapPartialLedgerConfig    blk = WrapPartialLedgerConfig    { unwrapPartialLedgerConfig    :: PartialLedgerConfig                   blk  }
newtype WrapPartialConsensusConfig blk = WrapPartialConsensusConfig { unwrapPartialConsensusConfig :: PartialConsensusConfig (BlockProtocol blk) }

deriving instance NoThunks (PartialLedgerConfig                   blk)  => NoThunks (WrapPartialLedgerConfig    blk)
deriving instance NoThunks (PartialConsensusConfig (BlockProtocol blk)) => NoThunks (WrapPartialConsensusConfig blk)

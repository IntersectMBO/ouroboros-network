{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE DeriveAnyClass          #-}
{-# LANGUAGE DeriveGeneric           #-}
{-# LANGUAGE DerivingStrategies      #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Ouroboros.Consensus.HardFork.Combinator.Abstract (
    SingleEraBlock(..)
  , proxySingle
  , CanHardFork(..)
    -- * Re-exports
  , IsNonEmpty(..)
  , ProofNonEmpty(..)
  ) where

import           Codec.Serialise (Serialise)
import           Data.Proxy
import           Data.SOP.Strict
import           Data.Typeable

import           Cardano.Slotting.Slot

import           Ouroboros.Network.Block

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HardFork.History (EraParams)
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Mempool.API

import           Ouroboros.Consensus.HardFork.Combinator.PartialConfig
import           Ouroboros.Consensus.HardFork.Combinator.SingleEra.Info
import           Ouroboros.Consensus.HardFork.Combinator.Translation
import           Ouroboros.Consensus.HardFork.Combinator.Util.SOP

-- | Blocks from which we can assemble a hard fork
class ( LedgerSupportsProtocol blk
      , ApplyTx blk
      , HasTxId (GenTx blk)
      , QueryLedger blk
      , CanForge blk
      , HasPartialConsensusConfig (BlockProtocol blk)
      , HasPartialLedgerConfig blk
        -- Instances required to support testing
      , Show blk
      , Show (Header blk)
        -- Only needed for the PBFT hack; see 'rewindConsensusState'
      , Serialise (HeaderHash blk)
      ) => SingleEraBlock blk where
  -- | Era parameters
  --
  -- The era parameters must be static (cannot depend on the ledger state).
  --
  -- Since we ne need this to construct the 'HardForkSummary' (and hence the
  -- 'EpochInfo', this takes the /partial/ config, not the full config
  -- (or we'd end up in a catch-22).
  singleEraParams     :: proxy blk -> PartialLedgerConfig blk -> EraParams

  -- | Era transition
  --
  -- This should only report the transition point once it is stable (rollback
  -- cannot affect it anymore).
  --
  -- This takes the partial config rather than the full config for the same
  -- reason as 'singleEraParam'.
  singleEraTransition :: PartialLedgerConfig blk -> LedgerState blk -> Maybe EpochNo

  -- | Era information (for use in error messages)
  singleEraInfo       :: proxy blk -> SingleEraInfo blk

proxySingle :: Proxy SingleEraBlock
proxySingle = Proxy

class (All SingleEraBlock xs, Typeable xs, IsNonEmpty xs) => CanHardFork xs where
  hardForkEraTranslation     :: EraTranslation     xs
  hardForkEraTransitionCheck :: EraTransitionCheck xs

instance SingleEraBlock blk => CanHardFork '[blk] where
  hardForkEraTranslation     = trivialEraTranslation
  hardForkEraTransitionCheck = trivialEraTransitionCheck

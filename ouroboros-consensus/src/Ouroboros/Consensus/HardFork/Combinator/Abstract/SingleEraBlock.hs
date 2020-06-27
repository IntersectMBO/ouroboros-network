{-# LANGUAGE FlexibleContexts #-}

module Ouroboros.Consensus.HardFork.Combinator.Abstract.SingleEraBlock (
    SingleEraBlock(..)
  , singleEraTransition'
  , proxySingle
  ) where

import           Data.Proxy

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HardFork.History (Bound, EraParams)
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.CommonProtocolParams
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Storage.ChainDB.Serialisation

import           Ouroboros.Consensus.HardFork.Combinator.Info
import           Ouroboros.Consensus.HardFork.Combinator.PartialConfig

{-------------------------------------------------------------------------------
  SingleEraBlock
-------------------------------------------------------------------------------}

-- | Blocks from which we can assemble a hard fork
class ( LedgerSupportsProtocol blk
      , LedgerSupportsMempool blk
      , HasTxId (GenTx blk)
      , QueryLedger blk
      , CanForge blk
      , HasPartialConsensusConfig (BlockProtocol blk)
      , HasPartialLedgerConfig blk
      , ConvertRawHash blk
      , ReconstructNestedCtxt Header blk
      , CommonProtocolParams blk
        -- Instances required to support testing
      , Eq   (GenTx blk)
      , Eq   (ApplyTxErr blk)
      , Show blk
      , Show (Header blk)
      ) => SingleEraBlock blk where

  -- | Era transition
  --
  -- This should only report the transition point once it is stable (rollback
  -- cannot affect it anymore).
  --
  -- Since we need this to construct the 'HardForkSummary' (and hence the
  -- 'EpochInfo'), this takes the /partial/ config, not the full config
  -- (or we'd end up with a catch-22).
  singleEraTransition :: PartialLedgerConfig blk
                      -> EraParams -- ^ Current era parameters
                      -> Bound     -- ^ Start of this era
                      -> LedgerState blk
                      -> Maybe EpochNo

  -- | Era information (for use in error messages)
  singleEraInfo       :: proxy blk -> SingleEraInfo blk

proxySingle :: Proxy SingleEraBlock
proxySingle = Proxy

singleEraTransition' :: SingleEraBlock blk
                     => WrapPartialLedgerConfig blk
                     -> EraParams
                     -> Bound
                     -> LedgerState blk -> Maybe EpochNo
singleEraTransition' = singleEraTransition . unwrapPartialLedgerConfig

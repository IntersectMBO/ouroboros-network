{-# LANGUAGE FlexibleContexts #-}

module Ouroboros.Consensus.HardFork.Combinator.Abstract.SingleEraBlock (
    SingleEraBlock(..)
  , proxySingle
  ) where

import           Cardano.Slotting.Slot
import           Data.Proxy

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Abstract
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
      , HasCodecConfig blk
      , ReconstructNestedCtxt Header blk
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
  -- Implementations should be careful: if the 'LedgerConfig' contains an
  -- 'EpochInfo', the forecast range of that 'EpochInfo' will include the tip
  -- of the 'LedgerState', but may not be include the point of the actual
  -- transition. Of course, the implementation of 'singleEraTransition' is
  -- /computing/ when the transition happens, and so it assume the same
  -- 'EpochSize' until that transition, but the 'EpochInfo' it is given will
  -- not be aware of that. This means some slot/epoch computations might have
  -- to be done in the implementation of 'singleEraTransition' itself.
  singleEraTransition :: LedgerConfig blk -> LedgerState blk -> Maybe EpochNo

  -- | Era information (for use in error messages)
  singleEraInfo       :: proxy blk -> SingleEraInfo blk

proxySingle :: Proxy SingleEraBlock
proxySingle = Proxy

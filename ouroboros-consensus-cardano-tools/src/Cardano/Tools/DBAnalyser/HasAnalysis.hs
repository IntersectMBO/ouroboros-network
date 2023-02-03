{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Cardano.Tools.DBAnalyser.HasAnalysis (
    HasAnalysis (..)
  , HasProtocolInfo (..)
  , SizeInBytes
  , WithLedgerState (..)
  ) where

import           Data.Map.Strict (Map)
import           Text.Builder (Builder)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HeaderValidation (HasAnnTip (..))
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Inspect (InspectLedger)
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Ledger.Tables
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Storage.ChainDB (LgrDbSerialiseConstraints)
import           Ouroboros.Consensus.Storage.Serialisation (SizeInBytes)
import           Ouroboros.Consensus.Util.Condense (Condense)

{-------------------------------------------------------------------------------
  HasAnalysis
-------------------------------------------------------------------------------}

data WithLedgerState blk = WithLedgerState
  { wlsBlk         :: blk
  , wlsStateBefore :: LedgerState blk EmptyMK
  , wlsStateAfter  :: LedgerState blk EmptyMK
  }

class ( HasAnnTip blk
      , GetPrevHash blk
      , Condense (HeaderHash blk)
      , LedgerSupportsProtocol blk
      , LgrDbSerialiseConstraints blk
      , InspectLedger blk
      ) => HasAnalysis blk where

  countTxOutputs :: blk -> Int
  blockTxSizes   :: blk -> [SizeInBytes]
  knownEBBs      :: proxy blk -> Map (HeaderHash blk) (ChainHash blk)

  -- | Emit trace markers at points in processing.
  emitTraces     :: WithLedgerState blk -> [String]

  -- | This method was introduced for the sake of the 'BenchmarkLedgerOps' pass.
  blockStats     :: blk -> [Builder]

class HasProtocolInfo blk where
  data Args blk
  mkProtocolInfo :: Args blk -> IO (ProtocolInfo IO blk)

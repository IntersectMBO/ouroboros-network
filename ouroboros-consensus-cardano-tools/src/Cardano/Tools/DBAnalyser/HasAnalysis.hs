{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
module HasAnalysis (
    HasAnalysis (..)
  , HasProtocolInfo (..)
  , SizeInBytes
  , WithLedgerState (..)
  ) where

import           Codec.Serialise.Class
import           Data.Map.Strict (Map)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HeaderValidation (HasAnnTip (..))
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Node.ProtocolInfo
import qualified Ouroboros.Consensus.Node.Run as Node
import           Ouroboros.Consensus.Storage.Serialisation (SizeInBytes)

{-------------------------------------------------------------------------------
  HasAnalysis
-------------------------------------------------------------------------------}

data WithLedgerState blk = WithLedgerState
  { wlsBlk         :: blk
  , wlsStateBefore :: LedgerState blk EmptyMK
  , wlsStateAfter  :: LedgerState blk EmptyMK
  }

class (IsLedger (LedgerState blk), HasAnnTip blk, GetPrevHash blk, LedgerSupportsProtocol blk, Serialise (HeaderHash blk), Node.RunNode blk) => HasAnalysis blk where

  countTxOutputs :: blk -> Int
  blockTxSizes   :: blk -> [SizeInBytes]
  knownEBBs      :: proxy blk -> Map (HeaderHash blk) (ChainHash blk)

  -- | Emit trace markers at points in processing.
  emitTraces     :: WithLedgerState blk -> [String]

class HasProtocolInfo blk where
  data Args blk
  mkProtocolInfo :: Args blk -> IO (ProtocolInfo IO blk)

{-# LANGUAGE TypeFamilies #-}
module HasAnalysis (
    HasAnalysis (..)
  , HasProtocolInfo (..)
  , SizeInBytes
  ) where

import           Data.Map.Strict (Map)
import           Options.Applicative

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HeaderValidation (HasAnnTip (..))
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Storage.Serialisation (SizeInBytes)

{-------------------------------------------------------------------------------
  HasAnalysis
-------------------------------------------------------------------------------}

class (HasAnnTip blk, GetPrevHash blk) => HasAnalysis blk where
  countTxOutputs :: blk -> Int
  blockTxSizes   :: blk -> [SizeInBytes]
  knownEBBs      :: proxy blk -> Map (HeaderHash blk) (ChainHash blk)

class HasProtocolInfo blk where
  data Args blk
  argsParser     :: proxy blk -> Parser (Args blk)
  mkProtocolInfo :: Args blk -> IO (ProtocolInfo IO blk)

{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module HasAnalysis (
    HasAnalysis (..)
  ) where

import           Data.Map.Strict (Map)
import           Options.Applicative

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Storage.ChainDB.Serialisation (SizeInBytes)

{-------------------------------------------------------------------------------
  HasAnalysis
-------------------------------------------------------------------------------}

class GetPrevHash blk => HasAnalysis blk where
    data Args blk
    argsParser      :: proxy blk -> Parser (Args blk)
    mkProtocolInfo  :: Args blk -> IO (ProtocolInfo IO blk)
    countTxOutputs  :: blk -> Int
    blockHeaderSize :: blk -> SizeInBytes
    blockTxSizes    :: blk -> [SizeInBytes]
    knownEBBs       :: proxy blk -> Map (HeaderHash blk) (ChainHash blk)

{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module HasAnalysis (
    HasAnalysis (..)
  ) where

import           Data.Map.Strict (Map)
import           Options.Applicative

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Storage.ChainDB (ImmDbSerialiseConstraints)
import           Ouroboros.Consensus.Storage.ChainDB.Serialisation (SizeInBytes)

{-------------------------------------------------------------------------------
  HasAnalysis
-------------------------------------------------------------------------------}

class ( ImmDbSerialiseConstraints blk
      , LedgerSupportsProtocol blk
      ) => HasAnalysis blk where
    data Args blk
    argsParser      :: proxy blk -> Parser (Args blk)
    mkProtocolInfo  :: Args blk -> IO (ProtocolInfo IO blk)
    countTxOutputs  :: blk -> Int
    blockTxSizes    :: blk -> [SizeInBytes]
    knownEBBs       :: proxy blk -> Map (HeaderHash blk) (ChainHash blk)
    exportSnapshot  :: LedgerState blk -> IO ()

module Ouroboros.Consensus.Storage.LedgerDB.BackingStore.Impl (BackingStoreTrace (..)) where

import           Ouroboros.Consensus.Storage.LedgerDB.BackingStore.InMemory
import           Ouroboros.Consensus.Storage.LedgerDB.BackingStore.LMDB

-- | A tracing datatype that is the sum of the traces of the backing store
-- implementations
data BackingStoreTrace = LMDBTrace TraceLMDB
                       | InMemoryTrace TVarTraceEvent
                       deriving (Eq, Show)

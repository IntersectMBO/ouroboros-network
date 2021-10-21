{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

module Ouroboros.Consensus.Storage.LedgerDB.Hybrid (
    -- * Operations on keysets.
    forwardTableReadSets
  , rewindTableKeySets
    -- * Disk database.
  , DiskDb
  , readDb
  , writeDb
    -- * Ledger interface
  , getTableKeysetsForBlock
  ) where

class HasOnDiskTables state

data DbChangelog state

data TableKeySets state

data AnnTableKeySets state a

data SeqNo state

data TableReadSets state

data AnnTableReadSets state a

type KeySetSanityInfo state = Maybe (SeqNo state)
type ReadSetSanityInfo state = (KeySetSanityInfo state, SeqNo state)

data TableDiffs state

data TableSnapshots state

--------------------------------------------------------------------------------
-- Operations on keysets.
--------------------------------------------------------------------------------

rewindTableKeySets ::
     HasOnDiskTables state
  => DbChangelog state
  -> TableKeySets state
  -> AnnTableKeySets state (KeySetSanityInfo state)
rewindTableKeySets = undefined

forwardTableReadSets ::
    forall state . HasOnDiskTables state
  => DbChangelog state
  -> AnnTableReadSets state (ReadSetSanityInfo state)
  -> Maybe (TableReadSets state)
forwardTableReadSets = undefined

--------------------------------------------------------------------------------
-- Disk database.
--------------------------------------------------------------------------------

class DiskDb dbhandle state where
  readDb  :: dbhandle
          -> AnnTableKeySets state a
          -> IO (AnnTableReadSets state (a, SeqNo state))

  writeDb :: dbhandle
          -> [Either (TableDiffs state) (TableSnapshots state)]
          -> SeqNo state -- ^ The old sequence number, as a sanity check
          -> SeqNo state -- ^ The new sequence number, must be strictly greater
          -> IO ()

--------------------------------------------------------------------------------
-- Ledger interface
--------------------------------------------------------------------------------

getTableKeysetsForBlock
  ::  blk
  -> state
  -- ^ This should be a recent state. TODO: explain why.
  --
  -- TODO: what is the exact type we need here? I can't imagine we use the
  -- actual ledger state. Would the ledger require a 'DbChangelog' instead?
  -> TableKeySets state
getTableKeysetsForBlock = undefined

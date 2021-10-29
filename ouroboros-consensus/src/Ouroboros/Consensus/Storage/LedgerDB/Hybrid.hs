{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
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
    -- * Ledger interface.
  , getTableKeysetsForBlock
    -- * Ledger state hydration.
  , hydrateLedgerState2
    -- * DB Changelog extension.
  , extendDbChangelog
  ) where

class HasOnDiskTables (state :: (TableType -> * -> * -> *) -> *)

data DbChangelog (state :: (TableType -> * -> * -> *) -> *)
data TableType

data TableKeySets (state :: (TableType -> * -> * -> *) -> *)

data AnnTableKeySets (state :: (TableType -> * -> * -> *) -> *) a

data SeqNo (state :: (TableType -> * -> * -> *) -> *)

data TableReadSets (state :: (TableType -> * -> * -> *) -> *)

data AnnTableReadSets (state :: (TableType -> * -> * -> *) -> *) a

type KeySetSanityInfo state = Maybe (SeqNo state)
type ReadSetSanityInfo state = (KeySetSanityInfo state, SeqNo state)

data TableDiffs (state :: (TableType -> * -> * -> *) -> *)

data TableDiff (t :: TableType) k v

data TableSnapshots (state :: (TableType -> * -> * -> *) -> *)

data EmptyTable (t :: TableType) k v

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
      HasOnDiskTables state
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
  :: forall (state :: (TableType -> * -> * -> *) -> *) blk
  . blk
  -> state EmptyTable
  -- ^ This should be a recent state.
  --
  -- TODO: Is it correct to use EmptyTable here?
  --
  -- TODO: explain why. This might have to do with incremental computations. if
  --       the ledger thinks that no-incremental computation should take place
  --       but it should then the keys the ledger needs won't be in the
  --       resulting TableKeySets.
  --
  -- TODO: what is the exact type we need here? I can't imagine we use the
  --       actual ledger state. Would the ledger require a 'DbChangelog'
  --       instead?
  -> TableKeySets state
getTableKeysetsForBlock = undefined

-- TODO: how to reconcile 'TableKeySets state' and 'Set (TxIn (Crypto era))'.

--------------------------------------------------------------------------------
-- Ledger state hydration
--
-- These are the functions that we'll need to prepare the ledger state for block
-- application.
--------------------------------------------------------------------------------

type UnforwardedTableReadSets state = AnnTableReadSets state (ReadSetSanityInfo state)
data TrackingTable (t :: TableType) k v

hydrateLedgerState1 ::
     TableReadSets state
  -> DbChangelog state
  -> state TrackingTable
hydrateLedgerState1  = undefined

hydrateLedgerState2 ::
     HasOnDiskTables state
  => UnforwardedTableReadSets state
  -> DbChangelog state
  -> Maybe (state TrackingTable)
hydrateLedgerState2 utrs dbcl = do
  trs <- forwardTableReadSets dbcl utrs
  pure $ hydrateLedgerState1 trs dbcl

--------------------------------------------------------------------------------
-- DB Changelog extension
--------------------------------------------------------------------------------

-- | Apply a state change to the changelog.
extendDbChangelog :: forall state.
                     HasOnDiskTables state
                  => SeqNo state
                  -- ^ ???
                  -> state TableDiff
                  -- ^ ??? This be derived from the ledger state, right
                  -> Maybe (TableSnapshots state)
                  -- ^ ???
                  -> DbChangelog state
                  -> DbChangelog state
extendDbChangelog = undefined

{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE EmptyDataDeriving          #-}
-- Remove the one above this when DbChangelog gets a constructor
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}
-- |

module Ouroboros.Consensus.Storage.LedgerDB.UTxOHD where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader
import           Data.Word
import           GHC.Generics
import           NoThunks.Class

import           Cardano.Slotting.Slot

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Storage.FS.API

data Seq (a :: LedgerStateKind) s

seqLast :: Seq state (state EmptyMK) -> state EmptyMK
seqLast = undefined

seqAt :: SeqNo state -> Seq state (state EmptyMK) -> state EmptyMK
seqAt = undefined

seqAnchorAt :: SeqNo state -> Seq state (state EmptyMK) -> Seq state (state EmptyMK)
seqAnchorAt = undefined

seqLength :: Seq state (state EmptyMK) -> Int
seqLength = undefined

data DbChangelog (l :: LedgerStateKind)
  deriving (Eq, Generic, NoThunks)

newtype RewoundTableKeySets l = RewoundTableKeySets (AnnTableKeySets l ()) -- KeySetSanityInfo l

initialDbChangelog
  :: WithOrigin SlotNo -> l ValuesMK -> DbChangelog l
initialDbChangelog = undefined

initialDbChangelogWithEmptyState :: WithOrigin SlotNo -> l EmptyMK -> DbChangelog l
initialDbChangelogWithEmptyState = undefined

rewindTableKeySets'
  :: DbChangelog l -> TableKeySets l -> RewoundTableKeySets l
rewindTableKeySets' = undefined

newtype UnforwardedReadSets l = UnforwardedReadSets (AnnTableReadSets l ())

forwardTableKeySets'
  :: DbChangelog l -> UnforwardedReadSets l -> Maybe (TableReadSets l)
forwardTableKeySets' = undefined

extendDbChangelog
  :: SeqNo l
  -> l DiffMK
  -- -> Maybe (l SnapshotsMK) TOOD: We won't use this parameter in the first iteration.
  -> DbChangelog l
  -> DbChangelog l
extendDbChangelog = undefined

dbChangelogStateAnchor :: DbChangelog state -> SeqNo state
dbChangelogStateAnchor = undefined

dbChangelogStates :: DbChangelog state -> Seq state (state EmptyMK)
dbChangelogStates = undefined

dbChangelogRollBack :: WithOrigin SlotNo -> DbChangelog state -> DbChangelog state
dbChangelogRollBack = undefined

newtype SeqNo (state :: LedgerStateKind) = SeqNo { unSeqNo :: Word64 }
  deriving (Eq, Ord, Show)

class HasSeqNo (state :: LedgerStateKind) where
  stateSeqNo :: state table -> SeqNo state

instance IsLedger l => HasSeqNo l where
  stateSeqNo l =
    case getTipSlot l of
      Origin        -> SeqNo 0
      At (SlotNo n) -> SeqNo (n + 1)

class ReadsKeySets m l where

  readDb :: ReadKeySets m l

type ReadKeySets m l = RewoundTableKeySets l -> m (UnforwardedReadSets l)

newtype DbReader m l a = DbReader { runDbReader :: ReaderT (ReadKeySets m l) m a}
  deriving newtype (Functor, Applicative, Monad)

instance ReadsKeySets (DbReader m l) l where
  readDb rks = DbReader $ ReaderT $ \f -> f rks

-- TODO: this is leaking details on how we want to compose monads at the higher levels.
instance (Monad m, ReadsKeySets m l) => ReadsKeySets (ReaderT r m) l where
  readDb = lift . readDb

instance (Monad m, ReadsKeySets m l) => ReadsKeySets (ExceptT e m) l where
  readDb = lift . readDb

defaultReadKeySets :: ReadKeySets m l -> DbReader m l a -> m a
defaultReadKeySets f dbReader = runReaderT (runDbReader dbReader) f

mkOnDiskLedgerStDb :: SomeHasFS m -> m (OnDiskLedgerStDb m l blk)
mkOnDiskLedgerStDb = undefined
  -- \(SomeHasFS fs) -> do
  --   dbhandle <- hOpen fs "ledgerStateDb"
  --   ...

  --   return OnDiskLedgerStDb
  --   { ...
  --     , readKeySets = Snapshots.readDb dbhandle

  --     }

-- | On disk ledger state API.
--
--
data OnDiskLedgerStDb m l blk =
  OnDiskLedgerStDb
  { rewindTableKeySets  :: () -- TODO: move the corresponding function from
                               -- InMemory here.
  , forwardTableKeySets :: () -- TODO: ditto.

  , readKeySets         :: RewoundTableKeySets l -> m (UnforwardedReadSets l)
   -- ^ Captures the handle. Implemented by Snapshots.readDb
   --
   -- TODO: consider unifying this with defaultReadKeySets. Why? Because we are always using
   -- 'defaultReadKeySets' with readKeySets.
  , flushDb             :: RealPoint blk -> DbChangelog l -> m (DbChangelog l )
    -- ^ Flush the DbChangelog. This should push differences from the disk
    -- anchor up to the provided point, and remove the corresponding states from
    -- the list of states.
  , createRestorePoint  :: DbChangelog l -> m ()
    -- ^ Captures the DbHandle. Implemented using createRestorePoint (proposed
    -- by Douglas). We need to take the current SeqNo for the on disk state from
    -- the DbChangelog.

    {- -* other restore point ops ... -}
  , closeDb             :: m ()
    -- ^ This closes the captured handle.
  , odlsGetPt           :: m (Point blk)
    -- ^ Get the point representing the latest ledger state flushed to the disk
  , writeGenesisUTxO    :: l ValuesMK -> m ()
    -- ^ Write the initial Genesis UTxO to the disk
  }
  deriving NoThunks via OnlyCheckWhnfNamed "OnDiskLedgerStDb" (OnDiskLedgerStDb m l blk)

{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}

module Test.Util.TestLedgerState (
    LedgerTables (..)
  , SimpleLedgerState (..)
  ) where

import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)

import           Cardano.Binary (FromCBOR (..), ToCBOR (..))

import           Ouroboros.Consensus.Ledger.Basics

{-------------------------------------------------------------------------------
  Simple ledger state
-------------------------------------------------------------------------------}

newtype SimpleLedgerState k v (mk :: MapKind) = SimpleLedgerState {
    lsSimple :: mk k v
  }

deriving instance (Eq (mk k v)) => Eq (SimpleLedgerState k v mk)
deriving stock instance Show (mk k v) => Show (SimpleLedgerState k v mk)

instance (ToCBOR k, FromCBOR k, ToCBOR v, FromCBOR v)
      => SufficientSerializationForAnyBackingStore (SimpleLedgerState k v) where
  codecLedgerTables = SimpleLedgerTables $ CodecMK toCBOR toCBOR fromCBOR fromCBOR

{-------------------------------------------------------------------------------
  Simple ledger tables
-------------------------------------------------------------------------------}

instance (Ord k, Eq v, Show k, Show v) => TableStuff (SimpleLedgerState k v) where
  newtype LedgerTables (SimpleLedgerState k v) mk = SimpleLedgerTables {
      ltSimple :: mk k v
    } deriving Generic

  projectLedgerTables SimpleLedgerState{lsSimple} =
    SimpleLedgerTables lsSimple

  withLedgerTables st SimpleLedgerTables{ltSimple} =
    st { lsSimple = ltSimple }

  pureLedgerTables f =
    SimpleLedgerTables { ltSimple = f }

  mapLedgerTables f SimpleLedgerTables{ltSimple} =
    SimpleLedgerTables $ f ltSimple

  traverseLedgerTables f SimpleLedgerTables{ltSimple} =
    SimpleLedgerTables <$> f ltSimple

  zipLedgerTables f l r =
    SimpleLedgerTables (f (ltSimple l) (ltSimple r))

  zipLedgerTablesA f l r =
    SimpleLedgerTables <$> f (ltSimple l) (ltSimple r)

  zipLedgerTables2 f l m r =
    SimpleLedgerTables $ f (ltSimple l) (ltSimple m) (ltSimple r)

  zipLedgerTables2A f l c r =
    SimpleLedgerTables <$> f (ltSimple l) (ltSimple c) (ltSimple r)

  foldLedgerTables f SimpleLedgerTables{ltSimple} =
    f ltSimple

  foldLedgerTables2 f l r =
    f (ltSimple l) (ltSimple r)

  namesLedgerTables =
    SimpleLedgerTables { ltSimple = NameMK "ltSimple" }

deriving stock instance (Eq (mk k v))
               => Eq (LedgerTables (SimpleLedgerState k v) mk)

deriving stock instance (Show (mk k v))
               => Show (LedgerTables (SimpleLedgerState k v) mk)

deriving newtype instance NoThunks (mk k v)
               => NoThunks (LedgerTables (SimpleLedgerState k v) mk)

instance (Show k, Show v) => ShowLedgerState (LedgerTables (SimpleLedgerState k v)) where
  showsLedgerState _ = shows

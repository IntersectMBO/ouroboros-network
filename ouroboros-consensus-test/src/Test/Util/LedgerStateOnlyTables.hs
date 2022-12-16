{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}

-- | A simple ledger state that only holds ledger tables (and values).
--
-- This is useful when we only need a ledger state and ledger tables, but not
-- necessarily blocks with payloads (such as defined in @Test.Util.TestBlock@).
module Test.Util.LedgerStateOnlyTables (
    LedgerTables (..)
  , OTLedgerState (..)
  ) where

import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)

import           Cardano.Binary (FromCBOR (..), ToCBOR (..))

import           Ouroboros.Consensus.Ledger.Basics

{-------------------------------------------------------------------------------
  Simple ledger state
-------------------------------------------------------------------------------}

data OTLedgerState k v (mk :: MapKind) = OTLedgerState {
    otlsLedgerState  :: ValuesMK k v
  , otlsLedgerTables :: LedgerTables (OTLedgerState k v) mk
  }

deriving instance (Ord k, Eq v, Eq (mk k v))
               => Eq (OTLedgerState k v mk)
deriving stock instance (Show k, Show v, Show (mk k v))
                     => Show (OTLedgerState k v mk)

instance (ToCBOR k, FromCBOR k, ToCBOR v, FromCBOR v)
      => SufficientSerializationForAnyBackingStore (OTLedgerState k v) where
  codecLedgerTables = OTLedgerTables $ CodecMK toCBOR toCBOR fromCBOR fromCBOR

{-------------------------------------------------------------------------------
  Stowable
-------------------------------------------------------------------------------}

instance (Ord k, Eq v, Show k, Show v)
    => StowableLedgerTables (OTLedgerState k v) where
  stowLedgerTables OTLedgerState{otlsLedgerTables} =
    OTLedgerState (otltLedgerTables otlsLedgerTables) emptyLedgerTables

  unstowLedgerTables OTLedgerState{otlsLedgerState} =
    OTLedgerState
      (otltLedgerTables polyEmptyLedgerTables)
      (OTLedgerTables otlsLedgerState)

{-------------------------------------------------------------------------------
  Simple ledger tables
-------------------------------------------------------------------------------}

instance (Ord k, Eq v, Show k, Show v)
      => TableStuff (OTLedgerState k v) where
  newtype LedgerTables (OTLedgerState k v) mk = OTLedgerTables {
      otltLedgerTables :: mk k v
    } deriving Generic

  projectLedgerTables OTLedgerState{otlsLedgerTables} =
    otlsLedgerTables

  withLedgerTables st lt =
    st { otlsLedgerTables = lt }

  pureLedgerTables f =
    OTLedgerTables { otltLedgerTables = f }

  mapLedgerTables f OTLedgerTables{otltLedgerTables} =
    OTLedgerTables $ f otltLedgerTables

  traverseLedgerTables f OTLedgerTables{otltLedgerTables} =
    OTLedgerTables <$> f otltLedgerTables

  zipLedgerTables f l r =
    OTLedgerTables (f (otltLedgerTables l) (otltLedgerTables r))

  zipLedgerTablesA f l r =
    OTLedgerTables <$> f (otltLedgerTables l) (otltLedgerTables r)

  zipLedgerTables2 f l m r =
    OTLedgerTables $
      f (otltLedgerTables l) (otltLedgerTables m) (otltLedgerTables r)

  zipLedgerTables2A f l c r =
    OTLedgerTables <$>
      f (otltLedgerTables l) (otltLedgerTables c) (otltLedgerTables r)

  foldLedgerTables f OTLedgerTables{otltLedgerTables} =
    f otltLedgerTables

  foldLedgerTables2 f l r =
    f (otltLedgerTables l) (otltLedgerTables r)

  namesLedgerTables =
    OTLedgerTables { otltLedgerTables = NameMK "otltLedgerTables" }

deriving stock instance (Eq (mk k v))
               => Eq (LedgerTables (OTLedgerState k v) mk)

deriving stock instance (Show (mk k v))
               => Show (LedgerTables (OTLedgerState k v) mk)

deriving newtype instance NoThunks (mk k v)
               => NoThunks (LedgerTables (OTLedgerState k v) mk)

instance (Show k, Show v)
      => ShowLedgerState (LedgerTables (OTLedgerState k v)) where
  showsLedgerState _ = shows

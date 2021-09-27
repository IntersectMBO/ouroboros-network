{-# LANGUAGE RankNTypes #-}

-- |

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Example.Types where


import Data.Monoid
import qualified Data.Hashable as H
import GHC.Generics (Generic)
import LedgerOnDisk.KVHandle.Class
import LedgerOnDisk.Mapping.Class
import Control.Lens
import qualified Data.BTree.Primitives as Haskey
import Data.Binary
import Data.Proxy
import Data.Int
import Type.Reflection
import qualified Database.Haskey.Alloc.Concurrent as Haskey
import Control.Applicative

-- | An example of a top level state consisting of a few parts. This
-- demonstrates a top level state type with multiple mappings, and other
-- in-memory state that are not mappings, and is not kept on disk.
--
data LedgerState map =
     LedgerState {

      -- | An example of some nested state
       utxos   :: UTxOState map,

       -- | Something standing in for other state, like protocol parameters.
       pparams :: PParams
     }

-- | The content of this doesn't actually matter. It's just a place-holder.
--
newtype PParams = PParams
  { totalBalance :: Coin
  }
  deriving stock (Eq, Show)

data UTxOState map =
     UTxOState {
       -- | A simple UTxO structure.
       utxo    :: map TxIn TxOut,

       -- | An aggregation of the UTxO's coins by address.
       utxoagg :: map Addr Coin
       -- ,
       -- extra :: Bool
     }

-- add snapshots + iteration over snapshots

-- LedgerState PTMap

data TxIn  = TxIn !TxId !TxIx
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass(H.Hashable, Binary)

instance Haskey.Value TxIn where
  fixedSize _ = Haskey.fixedSize (Proxy :: Proxy (TxId, TxIx))

instance Haskey.Key TxIn where
  narrow (TxIn i1 x1)  (TxIn i2 x2) = let
    ((i3, x3), (i4, x4)) = Haskey.narrow (i1, x1) (i2, x2)
    in (TxIn i3 x3, TxIn i4 x4)

newtype TxId  = TxId Int64
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (H.Hashable, Binary)
  deriving newtype (Haskey.Value, Haskey.Key)


newtype TxIx  = TxIx Int64
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (H.Hashable, Binary)
  deriving newtype (Haskey.Value, Haskey.Key)

data TxOut = TxOut !Addr !Coin
  deriving stock (Eq, Show, Generic)
  deriving anyclass (H.Hashable, Binary)

instance Haskey.Value TxOut where
  fixedSize _ = Haskey.fixedSize (Proxy :: Proxy (Addr, Coin))

-- data Addr  = PaymentAddr Int64 | StakingAddress Int64
newtype Addr  = Addr Int64
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Binary, Haskey.Value, Haskey.Key, H.Hashable)

newtype Coin  = Coin Int64
  deriving stock (Eq, Show, Generic)
  deriving newtype (Num, Binary, Haskey.Value, H.Hashable)
  deriving (Semigroup, Monoid) via (Sum Int64)

-- class (c TxIn TxOut, c Addr Coin) => UTxOStateKVConstraint (c :: Type -> Type -> Constraint)
-- instance (c TxIn TxOut, c Addr Coin) => UTxOStateKVConstraint (c :: Type -> Type -> Constraint)

instance HasOnDiskMappings UTxOState where
  data OnDiskMappings UTxOState map = UTxOStateMappings
    { sm_utxo :: map TxIn TxOut
    , sm_utxoagg :: map Addr Coin
    } deriving stock (Generic)

  onDiskMappingsLens = lens get_it set_it where
    get_it UTxOState{..} = UTxOStateMappings {sm_utxo=utxo, sm_utxoagg=utxoagg}
    set_it s@UTxOState{} UTxOStateMappings{..} =
      s {utxo=sm_utxo, utxoagg=sm_utxoagg}

  nullMap = UTxOStateMappings { sm_utxo = NullMap, sm_utxoagg = NullMap }

instance (c TxIn TxOut, c Addr Coin) => HasConstrainedOnDiskMappings c UTxOState where
  zipMappings _ f m1@UTxOStateMappings{} m2@UTxOStateMappings{} = liftA2 go utxo utxoagg
    where
      utxo = f (sm_utxo m1) (sm_utxo m2)
      utxoagg = f (sm_utxoagg m1) (sm_utxoagg m2)
      go sm_utxo sm_utxoagg = UTxOStateMappings {..}



-- class UTxOStateKVConstraint c => LedgerStateKVConstraint (c :: Type -> Type -> Constraint)
-- instance UTxOStateKVConstraint c => LedgerStateKVConstraint (c :: Type -> Type -> Constraint)

instance HasOnDiskMappings LedgerState where
  newtype OnDiskMappings LedgerState map = LedgerStateMappings
    { sm_utxos :: OnDiskMappings UTxOState map
    } deriving stock (Generic)

  onDiskMappingsLens = lens get_it set_it where
    get_it LedgerState{..} =
      LedgerStateMappings { sm_utxos = utxos ^. onDiskMappingsLens }
    set_it ls@LedgerState{utxos} LedgerStateMappings{sm_utxos} =
      ls { utxos = utxos & onDiskMappingsLens .~ sm_utxos }

  nullMap = LedgerStateMappings { sm_utxos = nullMap }

instance (HasConstrainedOnDiskMappings c UTxOState) => HasConstrainedOnDiskMappings c LedgerState where
  zipMappings p f m1@LedgerStateMappings{} m2@LedgerStateMappings{} = go <$> zipMappings p f (sm_utxos m1) (sm_utxos m2)
    where
      go sm_utxos = LedgerStateMappings{..}



deriving stock instance (forall k v. (Show k, Show v) => Show (map k v)) => Show (OnDiskMappings UTxOState map)
deriving stock instance (forall k v. (Show k, Show v) => Show (map k v)) => Show (OnDiskMappings LedgerState map)

deriving anyclass instance (forall k v. (Binary k, Binary v) => Binary (map k v)) => Binary (OnDiskMappings UTxOState map)
deriving anyclass instance (forall k v. (Binary k, Binary v) => Binary (map k v)) => Binary (OnDiskMappings LedgerState map)

-- This should work, but it doesn't seem to
-- instance forall map. (forall k v. (Haskey.Value k, Haskey.Value v) => Haskey.Value (map k v)) => Haskey.Value (OnDiskMappings UTxOState map) where
--   fixedSize _ = Haskey.fixedSize (Proxy :: Proxy (map TxIn TxOut, map Addr Coin))
instance forall map.
  ( Typeable map
  , Typeable (OnDiskMappings UTxOState map)
  , Binary (OnDiskMappings UTxOState map)
  , Show (OnDiskMappings UTxOState map)
  ) => Haskey.Value (OnDiskMappings UTxOState map) where
  fixedSize _ = Nothing

instance forall map.
  ( Typeable map
  , Typeable (OnDiskMappings LedgerState map)
  , Binary (OnDiskMappings LedgerState map)
  , Show (OnDiskMappings LedgerState map)
  ) => Haskey.Value (OnDiskMappings LedgerState map) where
  fixedSize _ = Nothing

instance Haskey.Value (OnDiskMappings LedgerState map) => Haskey.Root (OnDiskMappings LedgerState map)

type StakePool = Int

-- | RegisterAddr Addr StakePool
-- | DeregisterAddr Addr StakePool
data Tx = Tx [TxIn] [TxOut] | LollyScramble [TxOut]
  deriving stock (Eq, Show, Generic)
  deriving anyclass (H.Hashable)

txId :: Tx -> TxId
txId = TxId . fromIntegral . H.hash


type Block = [Tx]

type LedgerMapping map = (Mapping map, MappingConstraint map TxIn, MappingConstraint map Addr)

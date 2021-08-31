{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
{-# language GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
module LedgerOnDisk.SimpleLedgerState where

import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)

import Data.Dependent.Map (DMap)
import Data.Dependent.Sum (DSum(..))
import Data.GADT.Compare
import Data.Type.Equality

import qualified Data.Dependent.Map as DMap
import Data.Proxy
import Data.Foldable

import LedgerOnDisk.Diff
import Data.Functor.Identity
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable
import Data.Monoid

import LedgerOnDisk.FromKV
import Control.Monad.Except

newtype TxIn = TxIn Int
  deriving stock (Show, Eq)
  deriving newtype (Ord, Hashable)

newtype TxOut = TxOut Int
  deriving stock (Show, Eq)
  deriving newtype (Ord)

newtype DelegateFrom = DelegateFrom Int
  deriving stock (Show, Eq)
  deriving newtype (Ord, Hashable)

newtype DelegateTo = DelegateTo Int
  deriving stock (Show, Eq)
  deriving newtype (Ord)

newtype OtherData = OtherData Int

data LedgerState = LedgerState
  { utxos :: !(HashMap TxIn TxOut)
  , stakeDelegation :: !(HashMap DelegateFrom DelegateTo)
  , other :: !OtherData
  }

data LedgerStateK a where
  LSKutxo :: TxIn -> LedgerStateK TxOut
  LSKStakeDelegation :: DelegateFrom -> LedgerStateK DelegateTo

instance GEq (LedgerStateK) where
  geq (LSKutxo {}) (LSKutxo {}) = Just Refl
  geq (LSKStakeDelegation {}) (LSKStakeDelegation {}) = Just Refl
  geq _ _ = Nothing

instance GCompare LedgerStateK where
  gcompare (LSKutxo x) (LSKutxo y) = case x `compare` y of
    LT -> GLT
    EQ -> GEQ
    GT -> GGT
  gcompare (LSKStakeDelegation x) (LSKStakeDelegation y) = case x `compare` y of
    LT -> GLT
    EQ -> GEQ
    GT -> GGT
  gcompare (LSKutxo {}) _ = GLT
  gcompare (LSKStakeDelegation {}) _ = GGT

prepareLedgerState :: HashSet TxIn -> HashSet DelegateFrom -> OtherData -> (DMap LedgerStateK Proxy, DMap LedgerStateK Identity -> LedgerState)
prepareLedgerState utxos_set delegations_set other =
  ( DMap.fromList $ utxos_dsums ++ delegations_dsums
  , make_ledger_state
  )
  where
    utxos_dsums = [ LSKutxo x :=> Proxy | x <- toList utxos_set]
    delegations_dsums = [ LSKStakeDelegation x :=> Proxy | x <- toList delegations_set ]
    make_ledger_state dm = case DMap.foldrWithKey go ([], []) dm of
      (HashMap.fromList -> utxos, HashMap.fromList -> stakeDelegation) -> LedgerState{..}
      where
        go :: forall x. LedgerStateK x -> Identity x -> ([(TxIn, TxOut)], [(DelegateFrom, DelegateTo)]) -> ([(TxIn, TxOut)], [(DelegateFrom, DelegateTo)])
        go k (Identity v) (utxos, delegations) = case k of
                LSKutxo i -> ((i, v) : utxos, delegations)
                LSKStakeDelegation i -> (utxos, (i, v) : delegations)

instance FromKV' LedgerState where
  type O LedgerState = OtherData
  type K LedgerState = LedgerStateK
  fromKV other dm = LedgerState{..}
    where
      (HashMap.fromList -> utxos, HashMap.fromList -> stakeDelegation) = DMap.foldrWithKey go mempty dm
      go :: forall x. LedgerStateK x -> Identity x -> ([(TxIn, TxOut)], [(DelegateFrom, DelegateTo)]) -> ([(TxIn, TxOut)], [(DelegateFrom, DelegateTo)])
      go k (Identity v) (utxos', delegations) = case k of
              LSKutxo i -> ((i, v) : utxos', delegations)
              LSKStakeDelegation i -> (utxos', (i, v) : delegations)
  toKV LedgerState{..} = (other, DMap.fromList $ utxo_dsums ++ delegation_dsums)
    where
      utxo_dsums = [ LSKutxo k :=> Identity v | (k, v) <- HashMap.toList utxos ]
      delegation_dsums = [ LSKStakeDelegation k :=> Identity v | (k, v) <- HashMap.toList stakeDelegation ]

  applyDiff diff_dm LedgerState{..} = LedgerState
    { utxos = new_utxos
    , stakeDelegation = new_stake_delegation
    , other
    }
    where
      (flip appEndo utxos -> new_utxos, flip appEndo stakeDelegation -> new_stake_delegation) =
        DMap.foldrWithKey go mempty diff_dm
      go :: forall x. LedgerStateK x -> D x
        -> (Endo (HashMap TxIn TxOut), Endo (HashMap DelegateFrom DelegateTo))
        -> (Endo (HashMap TxIn TxOut), Endo (HashMap DelegateFrom DelegateTo))
      go k v e = case k of
        LSKutxo i -> (Endo $ applyDforK i v, mempty) <> e
        LSKStakeDelegation i -> (mempty, Endo $ applyDforK i v) <> e

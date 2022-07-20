{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}

module Ouroboros.Consensus.Storage.LedgerDB.HD.TableTypes (
    TableDiff (..)
  , TableKeys (..)
  , TableValues (..)
    -- * Interactions between keys, values and diffs
  , castTableKeys
  , differenceTableValues
  , forwardValues
  , forwardValuesAndKeys
  , mapTableDiff
  , mapTableValues
  , restrictValues
  ) where

import           Data.Group (Group)
import qualified Data.Map.Merge.Strict as Merge
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Sequence (Seq (Empty, (:|>)))
import qualified Data.Sequence as Seq
import           Data.Set (Set)
import qualified Data.Set as Set
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)
import           NoThunks.Class (NoThunks)

import           Data.Map.Strict.Diff2 (Diff (..), DiffEntry (..),
                     DiffHistory (..))

import           Ouroboros.Consensus.Storage.LedgerDB.HD.ToStore (ToStoreKind)

{-------------------------------------------------------------------------------
  Various types of table contents
-------------------------------------------------------------------------------}

newtype TableValues (ts :: ToStoreKind) k v = TableValues (Map k v)
  deriving stock (Generic, Show, Eq)
  deriving newtype (Semigroup, Monoid)
  deriving anyclass (NoThunks)

newtype TableKeys (ts :: ToStoreKind) k v = TableKeys (Set k)
  deriving stock (Generic, Show, Eq)
  deriving newtype (Semigroup, Monoid)
  deriving anyclass (NoThunks)

newtype TableDiff (ts :: ToStoreKind) k v = TableDiff (Diff k v)
  deriving stock (Generic, Show, Eq)
  deriving newtype (Semigroup, Monoid, Group)
  deriving anyclass (NoThunks)

{-------------------------------------------------------------------------------
  Interactions between keys, values and diffs
-------------------------------------------------------------------------------}

forwardValues :: (Ord k, HasCallStack) => TableValues ts k v -> TableDiff ts k v -> TableValues ts k v
forwardValues (TableValues values) (TableDiff (Diff diffs)) =
      TableValues
    $ Merge.merge
        Merge.preserveMissing
        (Merge.mapMaybeMissing     newKeys)
        (Merge.zipWithMaybeMatched oldKeys)
        values
        diffs
  where
    newKeys :: k -> DiffHistory v -> Maybe v
    newKeys _k (DiffHistory Empty)       = error "impossible"
    newKeys _k (DiffHistory (_es :|> e)) = case e of
      Insert x  -> Just x
      Delete _x -> Nothing
      _         -> error "impossible"

    oldKeys :: k -> v -> DiffHistory v -> Maybe v
    oldKeys _k _v1 (DiffHistory Empty)       = error "impossible"
    oldKeys _k _v1 (DiffHistory (_es :|> e)) = case e of
      Insert x  -> Just x
      Delete _x -> Nothing
      _         -> error "impossible"
    {-
    newKeys :: k -> DiffEntry v -> Maybe v
    newKeys _k (DiffEntry v diffState) = case diffState of
      UedsIns       -> Just v
      UedsInsAndDel -> Nothing
      UedsDel       -> Nothing -- TODO error "impossible! delete of missing key"


    oldKeys :: k -> v -> UtxoEntryDiff v -> Maybe v
    oldKeys _k _v1 (UtxoEntryDiff _v2 diffState) = case diffState of
      UedsDel       -> Nothing
      UedsIns       -> error "impossible! duplicate insert of key"
      UedsInsAndDel -> error "impossible! duplicate insert of key"
      -}

mapTableValues :: (v -> v') -> TableValues ts k v -> TableValues ts k v'
mapTableValues f (TableValues vs) = TableValues $ Map.map f vs

differenceTableValues :: Ord k => TableValues ts k v -> TableValues ts k v -> TableDiff ts k v
differenceTableValues (TableValues m1) (TableValues m2) =
      TableDiff . Diff
    $ Merge.merge
        (Merge.mapMissing $ \_k v -> DiffHistory $ Seq.singleton (Delete v))
        (Merge.mapMissing $ \_k v -> DiffHistory $ Seq.singleton (Insert v))
        (Merge.zipWithMaybeMatched $ \ _k _v1 v2 -> Just $ DiffHistory $ Seq.singleton (Insert v2))
        m1
        m2

castTableKeys :: TableKeys ts k v -> TableKeys ts k v'
castTableKeys (TableKeys ks) = TableKeys ks

forwardValuesAndKeys ::
     (Ord k, HasCallStack)
  => TableValues ts k v
  -> TableKeys ts k v
  -> TableDiff ts k v
  -> TableValues ts k v
forwardValuesAndKeys values@(TableValues v) (TableKeys keys) (TableDiff (Diff diffs)) =
  forwardValues values (TableDiff . Diff $ diffs `Map.restrictKeys` (Map.keysSet v `Set.union` keys))

restrictValues :: Ord k => TableValues ts k v -> TableKeys ts k v -> TableValues ts k v
restrictValues (TableValues m) (TableKeys s) =
    TableValues (Map.restrictKeys m s)

mapTableDiff :: (v -> v') -> TableDiff ts k v -> TableDiff ts k v'
mapTableDiff f (TableDiff (Diff m)) = TableDiff $ Diff $ fmap g m
  where
    g (DiffHistory s) = DiffHistory $ fmap (fmap f) s

{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE InstanceSigs            #-}
{-# LANGUAGE LambdaCase              #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Test.Util.Orphans.Isomorphism (
    Isomorphism (..)
  , from
  ) where

import           Data.Foldable (toList)
import qualified Data.Sequence as Seq

import qualified Data.FingerTree.Strict as FT

import qualified Data.FingerTree.TopMeasured.Strict as TMFT
import qualified Data.Map.Diff.Strict as MapDiff
import qualified Data.Map.Diff.Strict.Internal as Internal
import qualified Ouroboros.Consensus.Block as Block
import qualified Ouroboros.Consensus.Storage.LedgerDB.HD as HD
import qualified Ouroboros.Consensus.Storage.LedgerDB.HD.DiffSeq as DS

{------------------------------------------------------------------------------
  @'Isomorphism'@ class
------------------------------------------------------------------------------}

-- FIXME(jdral): This class is based on the @IsomorphicTo@ class from REF. We
-- can not use the package at this time because the cabal update index is too
-- outdated. Once the index has been updated to a more recent one, we should
-- switch to this package.
--
-- REF: https://hackage.haskell.org/package/isomorphism-class-0.1.0.5/docs/IsomorphismClass.html
class Isomorphism b a => Isomorphism a b where
  to :: a -> b

from :: Isomorphism b a => a -> b
from = to

{------------------------------------------------------------------------------
  Orphan instances
------------------------------------------------------------------------------}

instance Isomorphism a a where
  to :: a -> a
  to = id

instance (Isomorphism a a', Isomorphism b b') => Isomorphism (a -> b) (a' -> b') where
  to f = from . f . to

instance Isomorphism a b => Isomorphism [a] [b] where
  to :: [a] -> [b]
  to = fmap to

instance (Isomorphism a c, Isomorphism b d) => Isomorphism (a, b) (c, d) where
  to :: (a, b) -> (c, d)
  to (x, y) = (to x, to y)

instance (Ord k, Eq v)
      => Isomorphism (DS.DiffSeq k v) (HD.SeqUtxoDiff k v) where
  to :: DS.DiffSeq k v -> HD.SeqUtxoDiff k v
  to (DS.DiffSeq ft) = HD.SeqUtxoDiff . FT.fromList . map to' . toList $ ft
    where
      to' (DS.Element slot d)= HD.SudElement (to slot) (to d)

instance (Ord k, Eq v)
      => Isomorphism (HD.SeqUtxoDiff k v) (DS.DiffSeq k v) where
  to :: HD.SeqUtxoDiff k v -> DS.DiffSeq k v
  to (HD.SeqUtxoDiff ft) = DS.DiffSeq . TMFT.fromList . map to' . toList $ ft
    where
      to' (HD.SudElement slot d) = DS.Element (to slot) (to d)

instance Eq v => Isomorphism (MapDiff.Diff k v) (HD.UtxoDiff k v) where
  to (Internal.Diff m) = HD.UtxoDiff (fmap to' m)
    where
      to' :: MapDiff.DiffHistory v -> HD.UtxoEntryDiff v
      to' = \case
        Internal.DiffHistory (_xs Seq.:|> x) ->
          to'' x
        _ ->
          error "A DiffHistory is isomorphic to a UtxoEntryDiff under the \
                \ assumption that diff histories contain exactly one element."
      to'' :: MapDiff.DiffEntry v -> HD.UtxoEntryDiff v
      to'' = \case
        MapDiff.Insert v -> HD.UtxoEntryDiff v HD.UedsIns
        MapDiff.Delete v -> HD.UtxoEntryDiff v HD.UedsDel

instance Eq v => Isomorphism (HD.UtxoDiff k v) (MapDiff.Diff k v) where
  to :: HD.UtxoDiff k v -> MapDiff.Diff k v
  to (HD.UtxoDiff m) = Internal.Diff $ fmap to' m
    where
      to' :: Eq v => HD.UtxoEntryDiff v -> MapDiff.DiffHistory v
      to' (HD.UtxoEntryDiff x st) = case st of
        HD.UedsIns       -> MapDiff.singletonInsert x
        HD.UedsDel       -> MapDiff.singletonDelete x
        HD.UedsInsAndDel -> MapDiff.singletonInsert x <> MapDiff.singletonDelete x

instance Isomorphism (MapDiff.Values k v) (HD.UtxoValues k v) where
  to :: MapDiff.Values k v-> HD.UtxoValues k v
  to (MapDiff.Values m) = HD.UtxoValues m

instance Isomorphism (HD.UtxoValues k v) (MapDiff.Values k v) where
  to :: HD.UtxoValues k v -> MapDiff.Values k v
  to (HD.UtxoValues m) = MapDiff.Values m

instance Isomorphism (MapDiff.Keys k v) (HD.UtxoKeys k v) where
  to :: MapDiff.Keys k v -> HD.UtxoKeys k v
  to (MapDiff.Keys m) = HD.UtxoKeys m

instance Isomorphism (HD.UtxoKeys k v) (MapDiff.Keys k v) where
  to :: HD.UtxoKeys k v -> MapDiff.Keys k v
  to (HD.UtxoKeys m) = MapDiff.Keys m

instance Isomorphism DS.SlotNo Block.SlotNo where
  to :: DS.SlotNo -> Block.SlotNo
  to (DS.SlotNo slot) = slot

instance Isomorphism Block.SlotNo DS.SlotNo where
  to :: Block.SlotNo -> DS.SlotNo
  to = DS.SlotNo

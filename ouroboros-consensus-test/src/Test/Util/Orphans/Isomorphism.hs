{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE InstanceSigs            #-}
{-# LANGUAGE LambdaCase              #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Test.Util.Orphans.Isomorphism (
    Isomorphism (..)
  , from
  , isomorphismLaw
  ) where

import           Data.Foldable (toList)
import           Data.Maybe (fromJust)
import           Data.Proxy
import           Data.Semigroupoid
import           Data.Sequence (Seq (..))
import           Data.Sequence.NonEmpty (NESeq (..))

import qualified Data.FingerTree.RootMeasured.Strict as RMFT
import qualified Data.FingerTree.Strict as FT
import qualified Data.Map.Diff.Strict as MapDiff

import qualified Ouroboros.Consensus.Block as Block
import qualified Ouroboros.Consensus.Storage.LedgerDB.HD as HD
import qualified Ouroboros.Consensus.Storage.LedgerDB.HD.DiffSeq as DS

import           Test.Tasty.QuickCheck (Property, (===))

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

isomorphismLaw ::
     forall a b. (Isomorphism a b, Show a, Eq a)
  => Proxy b
  -> a
  -> Property
isomorphismLaw _ x = to @b @a (to @a @b x) === x

{------------------------------------------------------------------------------
  Orphan instances
------------------------------------------------------------------------------}

instance (Isomorphism a a', Isomorphism b b')
      => Isomorphism (a -> b) (a' -> b') where
  to f = from . f . to

instance Isomorphism a b => Isomorphism [a] [b] where
  to = fmap to


instance (Isomorphism a c, Isomorphism b d) => Isomorphism (a, b) (c, d) where
  to (x, y) = (to x, to y)

{------------------------------------------------------------------------------
  DiffSeq is isomorphic to SeqUtxoDiff
------------------------------------------------------------------------------}

instance (Ord k, Eq v)
      => Isomorphism (DS.DiffSeq k v) (HD.SeqUtxoDiff k v) where
  to (DS.DiffSeq ft) = HD.SeqUtxoDiff . FT.fromList . map to' . toList $ ft
    where
      to' (DS.Element slot d)= HD.SudElement (to slot) (to d)

instance (Ord k, Eq v)
      => Isomorphism (HD.SeqUtxoDiff k v) (DS.DiffSeq k v) where
  to (HD.SeqUtxoDiff ft) = DS.DiffSeq . RMFT.fromList . map to' . toList $ ft
    where
      to' (HD.SudElement slot d) = DS.Element (to slot) (to d)

instance Eq v => Isomorphism (MapDiff.Diff k v) (HD.UtxoDiff k v) where
  to (MapDiff.Diff m) = HD.UtxoDiff (fmap to' m)
    where
      to' = \case
        MapDiff.NEDiffHistory (Empty :|> x :||> y) ->
          to'' x <> to'' y
        MapDiff.NEDiffHistory (Empty :||> x) ->
          to'' x
        _ -> error "A DiffHistory is isomorphic to a UtxoEntryDiff under the \
                   \ assumption that diff histories contain exactly one      \
                   \ insert, exactly one delete or exactly an insert AND a   \
                   \ delete."
      to'' = \case
        MapDiff.Insert v            -> HD.UtxoEntryDiff v HD.UedsIns
        MapDiff.Delete v            -> HD.UtxoEntryDiff v HD.UedsDel
        MapDiff.UnsafeAntiInsert _v -> error "UnsafeAntiInsert found."
        MapDiff.UnsafeAntiDelete _v -> error "UnsafeAntiDelete found."

instance Eq v => Isomorphism (HD.UtxoDiff k v) (MapDiff.Diff k v) where
  to (HD.UtxoDiff m) = MapDiff.Diff $ fmap to' m
    where
      to' (HD.UtxoEntryDiff x st) = case st of
        HD.UedsIns       -> MapDiff.singletonInsert x
        HD.UedsDel       -> MapDiff.singletonDelete x
        HD.UedsInsAndDel ->
          fromJust (MapDiff.singletonInsert x <>? MapDiff.singletonDelete x)

instance Isomorphism (MapDiff.Values k v) (HD.UtxoValues k v) where
  to (MapDiff.Values m) = HD.UtxoValues m

instance Isomorphism (HD.UtxoValues k v) (MapDiff.Values k v) where
  to (HD.UtxoValues m) = MapDiff.Values m

instance Isomorphism (MapDiff.Keys k v) (HD.UtxoKeys k v) where
  to (MapDiff.Keys m) = HD.UtxoKeys m

instance Isomorphism (HD.UtxoKeys k v) (MapDiff.Keys k v) where
  to (HD.UtxoKeys m) = MapDiff.Keys m

instance Isomorphism DS.SlotNo Block.SlotNo where
  to (DS.SlotNo slot) = slot

instance Isomorphism Block.SlotNo DS.SlotNo where
  to = DS.SlotNo

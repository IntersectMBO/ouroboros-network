{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE InstanceSigs            #-}
{-# LANGUAGE LambdaCase              #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Test.Util.Orphans.Isomorphism (
    Isomorphism (..)
  , from
  , inside
  ) where

import           Data.Foldable (toList)
import qualified Data.Sequence as Seq

import qualified Data.FingerTree.Strict as FT

import qualified Data.FingerTree.Strict.Alt as Alt
import qualified Data.Map.Strict.Diff2 as D2

import qualified Ouroboros.Consensus.Block as Block
import qualified Ouroboros.Consensus.Storage.LedgerDB.HD as HD
import qualified Ouroboros.Consensus.Storage.LedgerDB.HD.DiffSeq as DS
import qualified Ouroboros.Consensus.Storage.LedgerDB.HD.TableTypes as TT

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

inside :: (Isomorphism a b, Isomorphism c d) => (b -> c) -> a -> d
inside f = from . f . to

{------------------------------------------------------------------------------
  Orphan instances
------------------------------------------------------------------------------}

instance Isomorphism a a where
  to :: a -> a
  to = id

instance Isomorphism a b => Isomorphism [a] [b] where
  to :: [a] -> [b]
  to = fmap to

instance (Isomorphism a c, Isomorphism b d) => Isomorphism (a, b) (c, d) where
  to :: (a, b) -> (c, d)
  to (x, y) = (to x, to y)

instance (Ord k, Eq v)
      => Isomorphism (DS.DiffSeq ts k v) (HD.SeqUtxoDiff k v) where
  to :: DS.DiffSeq ts k v -> HD.SeqUtxoDiff k v
  to (DS.DiffSeq alt) = HD.SeqUtxoDiff . FT.fromList . map to' . toList $ alt
    where
      to' (DS.Element slot d)= HD.SudElement (to slot) (to d)

instance (Ord k, Eq v)
      => Isomorphism (HD.SeqUtxoDiff k v) (DS.DiffSeq ts k v) where
  to :: HD.SeqUtxoDiff k v -> DS.DiffSeq ts k v
  to (HD.SeqUtxoDiff ft) = DS.DiffSeq . Alt.fromList . map to' . toList $ ft
    where
      to' (HD.SudElement slot d) = DS.Element (to slot) (to d)

instance Isomorphism (TT.TableDiff ts k v) (HD.UtxoDiff k v) where
  to (TT.TableDiff (D2.Diff m)) = HD.UtxoDiff (fmap to' m)
    where
      to' :: D2.DiffHistory v -> HD.UtxoEntryDiff v
      to' = \case
        D2.DiffHistory (_xs Seq.:|> x) ->
          to'' x
        _ ->
          error "A DiffHistory is isomorphic to a UtxoEntryDiff under the \
                \ assumption that diff histories contain exactly one element."
      to'' :: D2.DiffEntry v -> HD.UtxoEntryDiff v
      to'' = \case
        D2.Insert v -> HD.UtxoEntryDiff v HD.UedsIns
        D2.Delete v -> HD.UtxoEntryDiff v HD.UedsDel

instance Isomorphism (HD.UtxoDiff k v) (TT.TableDiff ts k v) where
  to :: HD.UtxoDiff k v -> TT.TableDiff ts k v
  to (HD.UtxoDiff m) = TT.TableDiff . D2.Diff $ fmap to' m
    where
      to' :: HD.UtxoEntryDiff v -> D2.DiffHistory v
      to' (HD.UtxoEntryDiff v st) = case st of
        HD.UedsIns ->
          D2.DiffHistory $ Seq.singleton $ D2.Insert v
        HD.UedsDel ->
          D2.DiffHistory $ Seq.singleton $ D2.Delete v
        HD.UedsInsAndDel ->
          D2.DiffHistory $ Seq.fromList [D2.Insert v, D2.Delete v]

instance Isomorphism (TT.TableValues ts k v) (HD.UtxoValues k v) where
  to :: TT.TableValues ts k v -> HD.UtxoValues k v
  to (TT.TableValues m) = HD.UtxoValues m

instance Isomorphism (HD.UtxoValues k v) (TT.TableValues ts k v) where
  to :: HD.UtxoValues k v -> TT.TableValues ts k v
  to (HD.UtxoValues m) = TT.TableValues m

instance Isomorphism (TT.TableKeys ts k v) (HD.UtxoKeys k v) where
  to :: TT.TableKeys ts k v -> HD.UtxoKeys k v
  to (TT.TableKeys m) = HD.UtxoKeys m

instance Isomorphism (HD.UtxoKeys k v) (TT.TableKeys ts k v) where
  to :: HD.UtxoKeys k v -> TT.TableKeys ts k v
  to (HD.UtxoKeys m) = TT.TableKeys m

instance Isomorphism DS.SlotNo Block.SlotNo where
  to :: DS.SlotNo -> Block.SlotNo
  to (DS.SlotNo slot) = slot

instance Isomorphism Block.SlotNo DS.SlotNo where
  to :: Block.SlotNo -> DS.SlotNo
  to = DS.SlotNo

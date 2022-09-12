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
import           Data.Proxy
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import qualified Test.Util.MockDiffSeq as MDS

import qualified Data.FingerTree.Strict as FT

import qualified Ouroboros.Consensus.Block as Block
import qualified Ouroboros.Consensus.Storage.LedgerDB.HD as HD

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

isomorphismLaw :: forall a b. (Isomorphism a b, Eq a) => Proxy b -> a -> Bool
isomorphismLaw _ x = to @b @a (to @a @b x) == x

{------------------------------------------------------------------------------
  Orphan instances
------------------------------------------------------------------------------}

instance Isomorphism a a where
  to = id

instance (Isomorphism a a', Isomorphism b b')
      => Isomorphism (a -> b) (a' -> b') where
  to f = from . f . to

instance Isomorphism a b => Isomorphism [a] [b] where
  to = fmap to


instance (Isomorphism a c, Isomorphism b d) => Isomorphism (a, b) (c, d) where
  to (x, y) = (to x, to y)

{------------------------------------------------------------------------------
  Orphan instances
------------------------------------------------------------------------------}

instance (Ord k, Eq v)
      => Isomorphism (MDS.MockDiffSeq k v) (HD.SeqUtxoDiff k v) where
  to (MDS.MockDiffSeq sq) = HD.SeqUtxoDiff . FT.fromList . map to' . toList $ sq
    where
      to' (slot, d)= HD.SudElement (to slot) (to d)

instance (Ord k, Eq v)
      => Isomorphism (HD.SeqUtxoDiff k v) (MDS.MockDiffSeq k v) where
  to (HD.SeqUtxoDiff ft) = MDS.MockDiffSeq . FT.fromList . map to' . toList $ ft
    where
      to' (HD.SudElement slot d) = (to slot, to d)

instance Eq v => Isomorphism (MDS.MockDiff k v) (HD.UtxoDiff k v) where
  to (MDS.MockDiff m) = HD.UtxoDiff (fmap to' m)
    where
      to' :: Seq (MDS.MockDiffEntry v) -> HD.UtxoEntryDiff v
      to' = \case
        (Seq.Empty Seq.:|> x) ->
          to'' x
        (Seq.Empty Seq.:|> MDS.MockInsert v1 Seq.:|> MDS.MockDelete v2)
          | v1 == v2  -> HD.UtxoEntryDiff v1 HD.UedsInsAndDel
        _ ->
          error "A sequence of inserts/deletes is isomorphic to a           \
                \ UtxoEntryDiff under the assumption that a key can only be \
                \ inserted and deleted once"
      to'' :: MDS.MockDiffEntry v -> HD.UtxoEntryDiff v
      to'' = \case
        MDS.MockInsert v -> HD.UtxoEntryDiff v HD.UedsIns
        MDS.MockDelete v -> HD.UtxoEntryDiff v HD.UedsDel

instance Eq v => Isomorphism (HD.UtxoDiff k v) (MDS.MockDiff k v) where
  to (HD.UtxoDiff m) = MDS.MockDiff $ fmap to' m
    where
      to' :: HD.UtxoEntryDiff v -> Seq (MDS.MockDiffEntry v)
      to' (HD.UtxoEntryDiff x st) = case st of
        HD.UedsIns       -> MDS.mSingletonInsert x
        HD.UedsDel       -> MDS.mSingletonDelete x
        HD.UedsInsAndDel -> MDS.mSingletonInsert x <> MDS.mSingletonDelete x

instance Isomorphism (MDS.MockValues k v) (HD.UtxoValues k v) where
  to (MDS.MockValues m) = HD.UtxoValues m

instance Isomorphism (HD.UtxoValues k v) (MDS.MockValues k v) where
  to (HD.UtxoValues m) = MDS.MockValues m

instance Isomorphism (MDS.MockKeys k v) (HD.UtxoKeys k v) where
  to (MDS.MockKeys m) = HD.UtxoKeys m

instance Isomorphism (HD.UtxoKeys k v) (MDS.MockKeys k v) where
  to (HD.UtxoKeys m) = MDS.MockKeys m

instance Isomorphism MDS.MockSlotNo Block.SlotNo where
  to (MDS.MockSlotNo x) = fromIntegral x

instance Isomorphism Block.SlotNo MDS.MockSlotNo where
  to (Block.SlotNo x) = fromIntegral x

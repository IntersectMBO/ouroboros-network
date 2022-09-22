{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Util.Orphans.NFData () where

import           Control.DeepSeq (NFData (..))
import           Data.Foldable
import           Data.Sequence (Seq)
import           Data.Sequence.NonEmpty (NESeq)

import           Data.FingerTree.RootMeasured.Strict as RMFT
import qualified Data.FingerTree.Strict as FT
import           Data.Map.Diff.Strict (Diff (..), DiffEntry (..),
                     DiffHistory (..), Keys (..), NEDiffHistory (..),
                     UnsafeDiffHistory (..), Values (..))
import           Ouroboros.Consensus.Storage.LedgerDB.HD (SeqUtxoDiff (..),
                     SudElement (..), SudMeasure (..), UtxoDiff (..),
                     UtxoEntryDiff (..), UtxoEntryDiffState (..), UtxoKeys (..),
                     UtxoValues (..))
import           Ouroboros.Consensus.Storage.LedgerDB.HD.DiffSeq (DiffSeq (..),
                     Element (..), InternalMeasure (..), Length (..),
                     RootMeasure (..), SlotNo (..))

{------------------------------------------------------------------------------
  StrictFingerTree
------------------------------------------------------------------------------}

-- | FIXME: We should verify that this instance is sufficient.
instance (NFData a, NFData v, FT.Measured v a)
      => NFData (FT.StrictFingerTree v a) where
  rnf ft = rnf (FT.measure ft) `seq` rnf (toList ft)

{-------------------------------------------------------------------------------
  SeqUtxoDiff
-------------------------------------------------------------------------------}

deriving newtype instance (NFData k, NFData v) => NFData (UtxoValues k v)

deriving newtype instance NFData k => NFData (UtxoKeys k v)

deriving anyclass instance NFData UtxoEntryDiffState
deriving anyclass instance NFData v => NFData (UtxoEntryDiff v)
deriving newtype instance (NFData k, NFData v) => NFData (UtxoDiff k v)

deriving anyclass instance (NFData k, NFData v) => NFData (SudElement k v)
deriving anyclass instance (NFData k, NFData v) => NFData (SudMeasure k v)
deriving newtype instance (Ord k, NFData k, NFData v) => NFData (SeqUtxoDiff k v)

{-------------------------------------------------------------------------------
  DiffSeq
-------------------------------------------------------------------------------}

deriving anyclass instance ( NFData vt, NFData vi, NFData a
                           , Measured vi a
                           ) => NFData (RMFT.StrictFingerTree vt vi a)

deriving anyclass instance NFData v => NFData (DiffEntry v)
deriving newtype instance NFData v => NFData (UnsafeDiffHistory Seq v)
deriving newtype instance NFData v => NFData (UnsafeDiffHistory NESeq v)
deriving newtype instance NFData v => NFData (DiffHistory v)
deriving newtype instance NFData v => NFData (NEDiffHistory v)
deriving newtype instance (NFData k, NFData v) => NFData (Diff k v)
deriving newtype instance NFData k => NFData (Keys k v)
deriving newtype instance (NFData k, NFData v) => NFData (Values k v)

deriving newtype instance NFData Length
deriving newtype instance NFData SlotNo

deriving anyclass instance (NFData k, NFData v) => NFData (RootMeasure k v)
deriving anyclass instance (NFData k, NFData v) => NFData (InternalMeasure k v)
deriving anyclass instance (NFData k, NFData v) => NFData (Element k v)
deriving newtype instance (NFData k, NFData v) => NFData (DiffSeq k v)

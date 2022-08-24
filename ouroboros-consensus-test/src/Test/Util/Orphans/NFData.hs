{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Util.Orphans.NFData () where

import           Control.DeepSeq (NFData (..))
import           Data.Foldable

import qualified Data.FingerTree.Strict as FT
import           Data.FingerTree.TopMeasured.Strict as TMFT
import           Data.Map.Diff.Strict.Internal (Diff (..), DiffEntry (..),
                     DiffHistory (..), Keys (..), Values (..))
import           Ouroboros.Consensus.Storage.LedgerDB.HD (SeqUtxoDiff (..),
                     SudElement (..), SudMeasure (..), UtxoDiff (..),
                     UtxoEntryDiff (..), UtxoEntryDiffState (..), UtxoKeys (..),
                     UtxoValues (..))
import           Ouroboros.Consensus.Storage.LedgerDB.HD.DiffSeq (DiffSeq (..),
                     Element (..), InternalMeasure (..), Length (..),
                     SlotNo (..), TopMeasure (..))

-- | FIXME: We should verify that this instance is sufficient. Unfortunately,
-- we can not automaticaly derive the @'NFData'@ instance, since the
-- 'fingertree' package does not export all necessary constructors to
-- automatically derive the NFData instance for the underlying
-- @'FingerTree'@.
instance (NFData a, NFData v, FT.Measured v a)
      => NFData (FT.StrictFingerTree v a) where
  rnf ft = rnf (FT.measure ft) `seq` rnf (toList ft)

{-------------------------------------------------------------------------------
  Legacy diff sequences
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
  New diff sequences
-------------------------------------------------------------------------------}

deriving anyclass instance ( NFData vt, NFData vi, NFData a
                           , Measured vi a
                           ) => NFData (TMFT.StrictFingerTree vt vi a)

deriving anyclass instance NFData v => NFData (DiffEntry v)
deriving newtype instance NFData v => NFData (DiffHistory v)
deriving newtype instance (NFData k, NFData v) => NFData (Diff k v)
deriving newtype instance NFData k => NFData (Keys k v)
deriving newtype instance (NFData k, NFData v) => NFData (Values k v)

deriving newtype instance NFData Length
deriving newtype instance NFData SlotNo

deriving anyclass instance (NFData k, NFData v) => NFData (TopMeasure k v)
deriving anyclass instance (NFData k, NFData v) => NFData (InternalMeasure k v)
deriving anyclass instance (NFData k, NFData v) => NFData (Element k v)
deriving newtype instance (NFData k, NFData v) => NFData (DiffSeq k v)

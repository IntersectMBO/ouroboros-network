{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Util.Orphans.NFData () where

import           Control.DeepSeq (NFData (..))
import           Data.Foldable

import qualified Data.FingerTree.Strict as FT
import           Ouroboros.Consensus.Storage.LedgerDB.HD (SeqUtxoDiff (..),
                     SudElement (..), SudMeasure (..), UtxoDiff (..),
                     UtxoEntryDiff (..), UtxoEntryDiffState (..), UtxoKeys (..),
                     UtxoValues (..))

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


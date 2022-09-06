{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Util.Orphans.NFData () where

import           Control.DeepSeq (NFData (..))
import           Data.Foldable

import qualified Data.FingerTree.Strict as FT

{------------------------------------------------------------------------------
  StrictFingerTree
------------------------------------------------------------------------------}

-- | FIXME: We should verify that this instance is sufficient.
instance (NFData a, NFData v, FT.Measured v a)
      => NFData (FT.StrictFingerTree v a) where
  rnf ft = rnf (FT.measure ft) `seq` rnf (toList ft)


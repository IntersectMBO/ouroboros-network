{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Util.Orphans.ToExpr () where

import           Data.TreeDiff (ToExpr (..))

import           Cardano.Slotting.Slot

import           Ouroboros.Network.Block
import           Ouroboros.Network.Point

instance ToExpr SlotNo
instance ToExpr t => ToExpr (WithOrigin t)
instance ToExpr (HeaderHash blk) => ToExpr (Point blk)
instance (ToExpr slot, ToExpr hash) => ToExpr (Block slot hash)

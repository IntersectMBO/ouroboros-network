{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Util.Orphans.ToExpr () where

import           Data.TreeDiff (ToExpr (..))

import           Cardano.Slotting.Slot

import           Ouroboros.Network.Block
import           Ouroboros.Network.Point

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Protocol.Abstract

{-------------------------------------------------------------------------------
  ouroboros-network
-------------------------------------------------------------------------------}

instance ToExpr SlotNo
instance ToExpr BlockNo

instance ToExpr t => ToExpr (WithOrigin t)
instance ToExpr (HeaderHash blk) => ToExpr (Point blk)
instance ToExpr (HeaderHash blk) => ToExpr (RealPoint blk)
instance (ToExpr slot, ToExpr hash) => ToExpr (Block slot hash)

{-------------------------------------------------------------------------------
  ouroboros-consensus
-------------------------------------------------------------------------------}

instance ( ToExpr (LedgerState blk)
         , ToExpr (ChainDepState (BlockProtocol blk))
         , ToExpr (TipInfo blk)
         ) => ToExpr (ExtLedgerState blk)

instance ( ToExpr (ChainDepState (BlockProtocol blk))
         , ToExpr (TipInfo blk)
         ) => ToExpr (HeaderState blk)

instance ( ToExpr (TipInfo blk)
         ) => ToExpr (AnnTip blk)

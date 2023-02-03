{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE DeriveAnyClass           #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE QuantifiedConstraints    #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE UndecidableInstances     #-}

module Ouroboros.Consensus.Storage.LedgerDB.Query (
    ledgerDbAnchor
  , ledgerDbCurrent
  , ledgerDbIsSaturated
  , ledgerDbMaxRollback
  , ledgerDbPast
  , ledgerDbSnapshots
  , ledgerDbTip
  ) where

import           Data.Foldable (find)
import           Data.Word

import qualified Ouroboros.Network.AnchoredSeq as AS

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract

import           Ouroboros.Consensus.Storage.LedgerDB.LedgerDB

-- | The ledger state at the tip of the chain
ledgerDbCurrent :: GetTip l => LedgerDB l -> l
ledgerDbCurrent = either unCheckpoint unCheckpoint . AS.head . ledgerDbCheckpoints

-- | Information about the state of the ledger at the anchor
ledgerDbAnchor :: LedgerDB l -> l
ledgerDbAnchor = unCheckpoint . AS.anchor . ledgerDbCheckpoints

-- | All snapshots currently stored by the ledger DB (new to old)
--
-- This also includes the snapshot at the anchor. For each snapshot we also
-- return the distance from the tip.
ledgerDbSnapshots :: LedgerDB l -> [(Word64, l)]
ledgerDbSnapshots LedgerDB{..} =
    zip
      [0..]
      (map unCheckpoint (AS.toNewestFirst ledgerDbCheckpoints)
        <> [unCheckpoint (AS.anchor ledgerDbCheckpoints)])

-- | How many blocks can we currently roll back?
ledgerDbMaxRollback :: GetTip l => LedgerDB l -> Word64
ledgerDbMaxRollback LedgerDB{..} = fromIntegral (AS.length ledgerDbCheckpoints)

-- | Reference to the block at the tip of the chain
ledgerDbTip :: GetTip l => LedgerDB l -> Point l
ledgerDbTip = castPoint . getTip . ledgerDbCurrent

-- | Have we seen at least @k@ blocks?
ledgerDbIsSaturated :: GetTip l => SecurityParam -> LedgerDB l -> Bool
ledgerDbIsSaturated (SecurityParam k) db =
    ledgerDbMaxRollback db >= k

-- | Get a past ledger state
--
--  \( O(\log(\min(i,n-i)) \)
--
-- When no ledger state (or anchor) has the given 'Point', 'Nothing' is
-- returned.
ledgerDbPast ::
     (HasHeader blk, IsLedger l, HeaderHash l ~ HeaderHash blk)
  => Point blk
  -> LedgerDB l
  -> Maybe l
ledgerDbPast pt db
    | pt == castPoint (getTip (ledgerDbAnchor db))
    = Just $ ledgerDbAnchor db
    | otherwise
    = fmap unCheckpoint $
        find ((== pt) . castPoint . getTip . unCheckpoint) $
          AS.lookupByMeasure (pointSlot pt) (ledgerDbCheckpoints db)

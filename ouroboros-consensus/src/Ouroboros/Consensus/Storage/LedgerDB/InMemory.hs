{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE QuantifiedConstraints  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}
module Ouroboros.Consensus.Storage.LedgerDB.InMemory {-# DEPRECATED "Use Ouroboros.Consensus.Storage.LedgerDB instead" #-}
  ( -- * LedgerDB proper
    LDB.LedgerDbCfg (..)
  , ledgerDbWithAnchor
    -- ** opaque
  , LDB.LedgerDB
    -- ** Serialisation
  , decodeSnapshotBackwardsCompatible
  , encodeSnapshot
    -- ** Queries
  , ledgerDbAnchor
  , ledgerDbBimap
  , ledgerDbCurrent
  , ledgerDbPast
  , ledgerDbPrune
  , ledgerDbSnapshots
  , ledgerDbTip
    -- ** Running updates
  , LDB.AnnLedgerError (..)
  , LDB.Ap (..)
  , LDB.ResolveBlock
  , LDB.ResolvesBlocks (..)
  , LDB.ThrowsLedgerError (..)
  , defaultResolveBlocks
  , defaultResolveWithErrors
  , defaultThrowLedgerErrors
    -- ** Updates
  , LDB.ExceededRollback (..)
  , ledgerDbPush
  , ledgerDbSwitch
    -- * Exports for the benefit of tests
    -- ** Additional queries
  , ledgerDbIsSaturated
  , ledgerDbMaxRollback
    -- ** Pure API
  , ledgerDbPush'
  , ledgerDbPushMany'
  , ledgerDbSwitch'
  ) where

import Data.Functor.Identity

import           Codec.Serialise.Decoding (Decoder)
import           Codec.Serialise.Encoding (Encoding)
import           Control.Monad.Except hiding (ap)
import           Control.Monad.Reader hiding (ap)
import           Data.Word

import           Ouroboros.Network.AnchoredSeq (AnchoredSeq (..), Anchorable)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract

import qualified Ouroboros.Consensus.Storage.LedgerDB as LDB
import           Ouroboros.Consensus.Storage.LedgerDB.ReadsKeySets

{-------------------------------------------------------------------------------
  Local non-exported aliases
-------------------------------------------------------------------------------}
type LedgerDB l           = LDB.LedgerDB l
type ResolveBlock m blk   = LDB.ResolveBlock m blk
type AnnLedgerError l blk = LDB.AnnLedgerError l blk
type Ap m l blk c         = LDB.Ap m l blk c
type ExceededRollback     = LDB.ExceededRollback
type LedgerDbCfg l        = LDB.LedgerDbCfg l

{-------------------------------------------------------------------------------
  Deprecated functions
-------------------------------------------------------------------------------}

{-# DEPRECATED ledgerDbWithAnchor "Use Ouroboros.Consensus.Storage.LedgerDB (new)" #-}
ledgerDbWithAnchor :: (HasLedgerTables l, GetTip l) => l EmptyMK -> LedgerDB l
ledgerDbWithAnchor = LDB.new

{-# DEPRECATED defaultResolveBlocks "Use Ouroboros.Consensus.Storage.LedgerDB (defaultResolveBlocks)" #-}
defaultResolveBlocks :: ResolveBlock m blk
                     -> ReaderT (ResolveBlock m blk) m a
                     -> m a
defaultResolveBlocks = LDB.defaultResolveBlocks

{-# DEPRECATED defaultThrowLedgerErrors "Use Ouroboros.Consensus.Storage.LedgerDB (defaultThrowLedgerErrors)" #-}
defaultThrowLedgerErrors :: ExceptT (AnnLedgerError l blk) m a
                         -> m (Either (AnnLedgerError l blk) a)
defaultThrowLedgerErrors = LDB.defaultThrowLedgerErrors

{-# DEPRECATED defaultResolveWithErrors "Use Ouroboros.Consensus.Storage.LedgerDB (defaultResolveWithErrors)" #-}
defaultResolveWithErrors :: ResolveBlock m blk
                         -> ExceptT (AnnLedgerError l blk)
                                    (ReaderT (ResolveBlock m blk) m)
                                    a
                         -> m (Either (AnnLedgerError l blk) a)
defaultResolveWithErrors = LDB.defaultResolveWithErrors

{-# DEPRECATED ledgerDbCurrent "Use Ouroboros.Consensus.Storage.LedgerDB (current)" #-}
ledgerDbCurrent :: GetTip l => LedgerDB l -> l EmptyMK
ledgerDbCurrent = LDB.current

{-# DEPRECATED ledgerDbAnchor "Use Ouroboros.Consensus.Storage.LedgerDB (anchor)" #-}
ledgerDbAnchor :: LedgerDB l -> l EmptyMK
ledgerDbAnchor = LDB.anchor

{-# DEPRECATED ledgerDbSnapshots "Use Ouroboros.Consensus.Storage.LedgerDB (snapshots)" #-}
ledgerDbSnapshots :: LedgerDB l -> [(Word64, l EmptyMK)]
ledgerDbSnapshots = LDB.snapshots

{-# DEPRECATED ledgerDbMaxRollback "Use Ouroboros.Consensus.Storage.LedgerDB (maxRollback)" #-}
ledgerDbMaxRollback :: GetTip l => LedgerDB l -> Word64
ledgerDbMaxRollback = LDB.maxRollback

{-# DEPRECATED ledgerDbTip "Use Ouroboros.Consensus.Storage.LedgerDB (tip)" #-}
ledgerDbTip :: GetTip l => LedgerDB l -> Point l
ledgerDbTip = LDB.tip

{-# DEPRECATED ledgerDbIsSaturated "Use Ouroboros.Consensus.Storage.LedgerDB (isSaturated)" #-}
ledgerDbIsSaturated :: GetTip l => SecurityParam -> LedgerDB l -> Bool
ledgerDbIsSaturated = LDB.isSaturated

{-# DEPRECATED ledgerDbPast "Use Ouroboros.Consensus.Storage.LedgerDB (getPastLedgerAt)" #-}
ledgerDbPast ::
     ( HeaderHash blk ~ HeaderHash l, HasHeader blk, IsLedger l
     , StandardHash l, HasTickedLedgerTables l)
  => Point blk
  -> LedgerDB l
  -> Maybe (l EmptyMK)
ledgerDbPast = LDB.getPastLedgerAt

{-# DEPRECATED ledgerDbBimap "Use Ouroboros.Consensus.Storage.LedgerDB (volatileStatesBimap)" #-}
ledgerDbBimap ::
     (Anchorable (WithOrigin SlotNo) a b)
  => (l EmptyMK -> a)
  -> (l EmptyMK -> b)
  -> LedgerDB l
  -> AnchoredSeq (WithOrigin SlotNo) a b
ledgerDbBimap = LDB.volatileStatesBimap

{-# DEPRECATED ledgerDbPrune "Use Ouroboros.Consensus.Storage.LedgerDB (prune)" #-}
ledgerDbPrune :: (GetTip l, StandardHash l) => SecurityParam -> LedgerDB l -> LedgerDB l
ledgerDbPrune = LDB.prune

{-# DEPRECATED ledgerDbPush "Use Ouroboros.Consensus.Storage.LedgerDB (push)" #-}
ledgerDbPush :: (ApplyBlock l blk, Monad m, c, StandardHash l)
             => LedgerDbCfg l
             -> Ap m l blk c
             -> KeySetsReader m l
             -> LedgerDB l
             -> m (LedgerDB l)
ledgerDbPush = LDB.push

{-# DEPRECATED ledgerDbSwitch "Use Ouroboros.Consensus.Storage.LedgerDB (switch)" #-}
ledgerDbSwitch :: (ApplyBlock l blk, Monad m, c, StandardHash l)
               => LedgerDbCfg l
               -> Word64          -- ^ How many blocks to roll back
               -> (LDB.UpdateLedgerDbTraceEvent blk -> m ())
               -> [Ap m l blk c]  -- ^ New blocks to apply
               -> KeySetsReader m l
               -> LedgerDB l
               -> m (Either ExceededRollback (LedgerDB l))
ledgerDbSwitch = LDB.switch

{-# DEPRECATED ledgerDbPush' "Use Ouroboros.Consensus.Storage.LedgerDB (push')" #-}
ledgerDbPush' :: (ApplyBlock l blk, StandardHash l)
              => LedgerDbCfg l
              -> blk
              -> KeySetsReader Identity l
              -> LedgerDB l
              -> LedgerDB l
ledgerDbPush' = LDB.push'

{-# DEPRECATED ledgerDbPushMany' "Use Ouroboros.Consensus.Storage.LedgerDB (pushMany')" #-}
ledgerDbPushMany' :: (ApplyBlock l blk, StandardHash l)
                  => LedgerDbCfg l
                  -> [blk]
                  -> KeySetsReader Identity l
                  -> LedgerDB l
                  -> LedgerDB l
ledgerDbPushMany' = LDB.pushMany'

{-# DEPRECATED ledgerDbSwitch' "Use Ouroboros.Consensus.Storage.LedgerDB (switch')" #-}
ledgerDbSwitch' :: (ApplyBlock l blk, StandardHash l)
                => LedgerDbCfg l
                -> Word64
                -> [blk]
                -> KeySetsReader Identity l
                -> LedgerDB l
                -> Maybe (LedgerDB l)
ledgerDbSwitch' = LDB.switch'

{-# DEPRECATED encodeSnapshot "Use Ouroboros.Consensus.Storage.LedgerDB (encodeSnapshot)" #-}
encodeSnapshot :: (l -> Encoding) -> l -> Encoding
encodeSnapshot = LDB.encodeSnapshot

{-# DEPRECATED decodeSnapshotBackwardsCompatible "Use Ouroboros.Consensus.Storage.LedgerDB (decodeSnapshotBackwardsCompatible)" #-}
decodeSnapshotBackwardsCompatible ::
     Proxy blk
  -> (forall s. Decoder s l)
  -> (forall s. Decoder s (HeaderHash blk))
  -> forall s. Decoder s l
decodeSnapshotBackwardsCompatible = LDB.decodeSnapshotBackwardsCompatible

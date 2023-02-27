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

{-# DEPRECATED ledgerDbWithAnchor "Use Ouroboros.Consensus.Storage.LedgerDB (ledgerDbWithAnchor)" #-}
ledgerDbWithAnchor :: GetTip l => l -> LedgerDB l
ledgerDbWithAnchor = LDB.ledgerDbWithAnchor

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

{-# DEPRECATED ledgerDbCurrent "Use Ouroboros.Consensus.Storage.LedgerDB (ledgerDbCurrent)" #-}
ledgerDbCurrent :: GetTip l => LedgerDB l -> l
ledgerDbCurrent = LDB.ledgerDbCurrent

{-# DEPRECATED ledgerDbAnchor "Use Ouroboros.Consensus.Storage.LedgerDB (ledgerDbAnchor)" #-}
ledgerDbAnchor :: LedgerDB l -> l
ledgerDbAnchor = LDB.ledgerDbAnchor

{-# DEPRECATED ledgerDbSnapshots "Use Ouroboros.Consensus.Storage.LedgerDB (ledgerDbSnapshots)" #-}
ledgerDbSnapshots :: LedgerDB l -> [(Word64, l)]
ledgerDbSnapshots = LDB.ledgerDbSnapshots

{-# DEPRECATED ledgerDbMaxRollback "Use Ouroboros.Consensus.Storage.LedgerDB (ledgerDbMaxRollback)" #-}
ledgerDbMaxRollback :: GetTip l => LedgerDB l -> Word64
ledgerDbMaxRollback = LDB.ledgerDbMaxRollback

{-# DEPRECATED ledgerDbTip "Use Ouroboros.Consensus.Storage.LedgerDB (ledgerDbTip)" #-}
ledgerDbTip :: GetTip l => LedgerDB l -> Point l
ledgerDbTip = LDB.ledgerDbTip

{-# DEPRECATED ledgerDbIsSaturated "Use Ouroboros.Consensus.Storage.LedgerDB (ledgerDbIsSaturated)" #-}
ledgerDbIsSaturated :: GetTip l => SecurityParam -> LedgerDB l -> Bool
ledgerDbIsSaturated = LDB.ledgerDbIsSaturated

{-# DEPRECATED ledgerDbPast "Use Ouroboros.Consensus.Storage.LedgerDB (ledgerDbPast)" #-}
ledgerDbPast ::
     (HeaderHash blk ~ HeaderHash l, HasHeader blk, IsLedger l)
  => Point blk
  -> LedgerDB l
  -> Maybe l
ledgerDbPast = LDB.ledgerDbPast

{-# DEPRECATED ledgerDbBimap "Use Ouroboros.Consensus.Storage.LedgerDB (ledgerDbBimap)" #-}
ledgerDbBimap ::
     (Anchorable (WithOrigin SlotNo) a b)
  => (l -> a)
  -> (l -> b)
  -> LedgerDB l
  -> AnchoredSeq (WithOrigin SlotNo) a b
ledgerDbBimap = LDB.ledgerDbBimap

{-# DEPRECATED ledgerDbPrune "Use Ouroboros.Consensus.Storage.LedgerDB (ledgerDbPrune)" #-}
ledgerDbPrune :: GetTip l => SecurityParam -> LedgerDB l -> LedgerDB l
ledgerDbPrune = LDB.ledgerDbPrune



{-# DEPRECATED ledgerDbPush "Use Ouroboros.Consensus.Storage.LedgerDB (ledgerDbPush)" #-}
ledgerDbPush :: (ApplyBlock l blk, Monad m, c) => LedgerDbCfg l
             -> Ap m l blk c -> LedgerDB l -> m (LedgerDB l)
ledgerDbPush = LDB.ledgerDbPush

{-# DEPRECATED ledgerDbSwitch "Use Ouroboros.Consensus.Storage.LedgerDB (ledgerDbSwitch)" #-}
ledgerDbSwitch :: (ApplyBlock l blk, Monad m, c)
               => LedgerDbCfg l
               -> Word64          -- ^ How many blocks to roll back
               -> (LDB.UpdateLedgerDbTraceEvent blk -> m ())
               -> [Ap m l blk c]  -- ^ New blocks to apply
               -> LedgerDB l
               -> m (Either ExceededRollback (LedgerDB l))
ledgerDbSwitch = LDB.ledgerDbSwitch

{-# DEPRECATED ledgerDbPush' "Use Ouroboros.Consensus.Storage.LedgerDB (ledgerDbPush')" #-}
ledgerDbPush' :: ApplyBlock l blk
              => LedgerDbCfg l -> blk -> LedgerDB l -> LedgerDB l
ledgerDbPush' = LDB.ledgerDbPush'

{-# DEPRECATED ledgerDbPushMany' "Use Ouroboros.Consensus.Storage.LedgerDB (ledgerDbPushMany')" #-}
ledgerDbPushMany' :: ApplyBlock l blk
                  => LedgerDbCfg l -> [blk] -> LedgerDB l -> LedgerDB l
ledgerDbPushMany' = LDB.ledgerDbPushMany'

{-# DEPRECATED ledgerDbSwitch' "Use Ouroboros.Consensus.Storage.LedgerDB (ledgerDbSwitch')" #-}
ledgerDbSwitch' :: ApplyBlock l blk
                => LedgerDbCfg l
                -> Word64 -> [blk] -> LedgerDB l -> Maybe (LedgerDB l)
ledgerDbSwitch' = LDB.ledgerDbSwitch'

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

{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE DataKinds                #-}
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

-- | Accessors for the LedgerDB and management
--
-- This module defines the operations that can be done on a LedgerDB, as well as
-- the procedures to apply a block to a LedgerDB and pushing the resulting
-- LedgerState into the DB.
module Ouroboros.Consensus.Storage.LedgerDB.Update (
    -- * Applying blocks
    AnnLedgerError (..)
  , AnnLedgerError'
  , Ap (..)
  , ExceededRollback (..)
  , ThrowsLedgerError (..)
  , defaultThrowLedgerErrors
    -- * Block resolution
  , ResolveBlock
  , ResolvesBlocks (..)
  , defaultResolveBlocks
    -- * Updates
  , defaultResolveWithErrors
  , flush
  , prune
  , push
  , switch
  , volatileStatesBimap
    -- * Pure API
  , push'
  , pushMany'
  , switch'
    -- * Trace
  , PushGoal (..)
  , PushStart (..)
  , Pushing (..)
  , UpdateLedgerDbTraceEvent (..)
  ) where

import           Control.Monad.Except hiding (ap)
import           Control.Monad.Reader hiding (ap)
import           Data.Functor.Identity
import           Data.Kind (Constraint, Type)
import           Data.Word
import           GHC.Generics

import           Ouroboros.Network.AnchoredSeq (Anchorable (..),
                     AnchoredSeq (..))
import qualified Ouroboros.Network.AnchoredSeq as AS

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Util

import           Ouroboros.Consensus.Storage.LedgerDB.DbChangelog (DbChangelog)
import qualified Ouroboros.Consensus.Storage.LedgerDB.DbChangelog as DbChangelog
import           Ouroboros.Consensus.Storage.LedgerDB.LedgerDB
import qualified Ouroboros.Consensus.Storage.LedgerDB.Query as Query
import           Ouroboros.Consensus.Storage.LedgerDB.ReadsKeySets

{-------------------------------------------------------------------------------
  Apply blocks
-------------------------------------------------------------------------------}

-- | 'Ap' is used to pass information about blocks to ledger DB updates
--
-- The constructors serve two purposes:
--
-- * Specify the various parameters
--   a. Are we passing the block by value or by reference?
--   b. Are we applying or reapplying the block?
--
-- * Compute the constraint @c@ on the monad @m@ in order to run the query:
--   a. If we are passing a block by reference, we must be able to resolve it.
--   b. If we are applying rather than reapplying, we might have ledger errors.
type Ap :: (Type -> Type) -> LedgerStateKind -> Type -> Constraint -> Type
data Ap m l blk c where
  ReapplyVal ::           blk -> Ap m l blk ()
  ApplyVal   ::           blk -> Ap m l blk (                      ThrowsLedgerError m l blk)
  ReapplyRef :: RealPoint blk -> Ap m l blk (ResolvesBlocks m blk)
  ApplyRef   :: RealPoint blk -> Ap m l blk (ResolvesBlocks m blk, ThrowsLedgerError m l blk)

  -- | 'Weaken' increases the constraint on the monad @m@.
  --
  -- This is primarily useful when combining multiple 'Ap's in a single
  -- homogeneous structure.
  Weaken :: (c' => c) => Ap m l blk c -> Ap m l blk c'

{-------------------------------------------------------------------------------
  Internal utilities for 'Ap'
-------------------------------------------------------------------------------}

toRealPoint :: HasHeader blk => Ap m l blk c -> RealPoint blk
toRealPoint (ReapplyVal blk) = blockRealPoint blk
toRealPoint (ApplyVal blk)   = blockRealPoint blk
toRealPoint (ReapplyRef rp)  = rp
toRealPoint (ApplyRef rp)    = rp
toRealPoint (Weaken ap)      = toRealPoint ap

-- | Apply block to the current ledger state
--
-- We take in the entire 'LedgerDB' because we record that as part of errors.
applyBlock :: forall m c l blk. (ApplyBlock l blk, Monad m, c)
           => LedgerCfg l
           -> Ap m l blk c
           -> KeySetsReader m l
           -> LedgerDB l
           -> m (l DiffMK)
applyBlock cfg ap ksReader db = case ap of
    ReapplyVal b ->
       withValues b $ return . tickThenReapply cfg b
    ApplyVal b ->
      withValues b $ \l' ->
        either (throwLedgerError db (blockRealPoint b)) return $ runExcept $
          tickThenApply cfg b l'
    ReapplyRef r  -> do
      b <- doResolveBlock r
      withValues b $ return . tickThenReapply cfg b
    ApplyRef r -> do
      b <- doResolveBlock r
      withValues b $ \l' ->
        either (throwLedgerError db r) return $ runExcept $
          tickThenApply cfg b l'
    Weaken ap' ->
      applyBlock cfg ap' ksReader db
  where
    l :: l EmptyMK
    l = Query.current db

    withValues :: blk -> (l ValuesMK -> m (l DiffMK)) -> m (l DiffMK)
    withValues b f = withKeysReadSets l ksReader (ledgerDbChangelog db) (getBlockKeySets b) f

{-------------------------------------------------------------------------------
  Resolving blocks maybe from disk
-------------------------------------------------------------------------------}

-- | Resolve a block
--
-- Resolving a block reference to the actual block lives in @m@ because
-- it might need to read the block from disk (and can therefore not be
-- done inside an STM transaction).
--
-- NOTE: The ledger DB will only ask the 'ChainDB' for blocks it knows
-- must exist. If the 'ChainDB' is unable to fulfill the request, data
-- corruption must have happened and the 'ChainDB' should trigger
-- validation mode.
type ResolveBlock m blk = RealPoint blk -> m blk

-- | Monads in which we can resolve blocks
--
-- To guide type inference, we insist that we must be able to infer the type
-- of the block we are resolving from the type of the monad.
class Monad m => ResolvesBlocks m blk | m -> blk where
  doResolveBlock :: ResolveBlock m blk

instance Monad m => ResolvesBlocks (ReaderT (ResolveBlock m blk) m) blk where
  doResolveBlock r = ReaderT $ \f -> f r

defaultResolveBlocks :: ResolveBlock m blk
                     -> ReaderT (ResolveBlock m blk) m a
                     -> m a
defaultResolveBlocks = flip runReaderT

-- Quite a specific instance so we can satisfy the fundep
instance Monad m
      => ResolvesBlocks (ExceptT e (ReaderT (ResolveBlock m blk) m)) blk where
  doResolveBlock = lift . doResolveBlock

{-------------------------------------------------------------------------------
  A ledger error annotated with the LedgerDB
-------------------------------------------------------------------------------}

-- | Annotated ledger errors
data AnnLedgerError l blk = AnnLedgerError {
      -- | The ledger DB just /before/ this block was applied
      annLedgerState  :: LedgerDB l

      -- | Reference to the block that had the error
    , annLedgerErrRef :: RealPoint blk

      -- | The ledger error itself
    , annLedgerErr    :: LedgerErr l
    }

type AnnLedgerError' blk = AnnLedgerError (ExtLedgerState blk) blk

class Monad m => ThrowsLedgerError m l blk where
  throwLedgerError :: LedgerDB l -> RealPoint blk -> LedgerErr l -> m a

instance Monad m => ThrowsLedgerError (ExceptT (AnnLedgerError l blk) m) l blk where
  throwLedgerError l r e = throwError $ AnnLedgerError l r e

defaultThrowLedgerErrors :: ExceptT (AnnLedgerError l blk) m a
                         -> m (Either (AnnLedgerError l blk) a)
defaultThrowLedgerErrors = runExceptT

defaultResolveWithErrors :: ResolveBlock m blk
                         -> ExceptT (AnnLedgerError l blk)
                                    (ReaderT (ResolveBlock m blk) m)
                                    a
                         -> m (Either (AnnLedgerError l blk) a)
defaultResolveWithErrors resolve =
      defaultResolveBlocks resolve
    . defaultThrowLedgerErrors

{-------------------------------------------------------------------------------
  LedgerDB management
-------------------------------------------------------------------------------}

-- | Transform the underlying 'AnchoredSeq' using the given functions.
volatileStatesBimap ::
     Anchorable (WithOrigin SlotNo) a b
  => (l EmptyMK -> a)
  -> (l EmptyMK -> b)
  -> LedgerDB l
  -> AnchoredSeq (WithOrigin SlotNo) a b
volatileStatesBimap f g =
      AS.bimap (f . DbChangelog.unDbChangelogState) (g . DbChangelog.unDbChangelogState)
    . DbChangelog.changelogVolatileStates
    . ledgerDbChangelog

-- | Prune ledger states until at we have at most @k@ in the LedgerDB, excluding
-- the one stored at the anchor.
prune ::
     (GetTip l, StandardHash l)
  => SecurityParam -> LedgerDB l -> LedgerDB l
prune k db = db {
      ledgerDbChangelog = DbChangelog.pruneVolatilePart k (ledgerDbChangelog db)
    }

 -- NOTE: we must inline 'prune' otherwise we get unexplained thunks in
 -- 'LedgerDB' and thus a space leak. Alternatively, we could disable the
 -- @-fstrictness@ optimisation (enabled by default for -O1). See #2532.
{-# INLINE prune #-}

{-------------------------------------------------------------------------------
  Internal updates
-------------------------------------------------------------------------------}

-- | Push an updated ledger state
pushLedgerState ::
     (IsLedger l, HasLedgerTables l, StandardHash l)
  => SecurityParam
  -> l DiffMK -- ^ Updated ledger state
  -> LedgerDB l -> LedgerDB l
pushLedgerState secParam currentNew' db@LedgerDB{..}  =
    prune secParam $ db {
        ledgerDbChangelog   =
          DbChangelog.extend
            ledgerDbChangelog
            currentNew'
      }

{-------------------------------------------------------------------------------
  Internal: rolling back
-------------------------------------------------------------------------------}

-- | Rollback
--
-- Returns 'Nothing' if maximum rollback is exceeded.
rollback ::
     (GetTip l, HasLedgerTables l)
  => Word64
  -> LedgerDB l
  -> Maybe (LedgerDB l)
rollback n db@LedgerDB{..}
    | n <= Query.maxRollback db
    = Just db {
          ledgerDbChangelog   = DbChangelog.rollbackN (fromIntegral n) ledgerDbChangelog
        }
    | otherwise
    = Nothing

{-------------------------------------------------------------------------------
  Updates
-------------------------------------------------------------------------------}

-- | Exceeded maximum rollback supported by the current ledger DB state
--
-- Under normal circumstances this will not arise. It can really only happen
-- in the presence of data corruption (or when switching to a shorter fork,
-- but that is disallowed by all currently known Ouroboros protocols).
--
-- Records both the supported and the requested rollback.
data ExceededRollback = ExceededRollback {
      rollbackMaximum   :: Word64
    , rollbackRequested :: Word64
    }

push :: forall m c l blk. (ApplyBlock l blk, Monad m, StandardHash l, c)
     => LedgerDbCfg l
     -> Ap m l blk c -> KeySetsReader m l -> LedgerDB l -> m (LedgerDB l)
push cfg ap ksReader db =
    (\current' -> pushLedgerState (ledgerDbCfgSecParam cfg) current' db) <$>
      applyBlock (ledgerDbCfg cfg) ap ksReader db

-- | Push a bunch of blocks (oldest first)
pushMany ::
     forall m c l blk . (ApplyBlock l blk, Monad m, StandardHash l, c)
  => (Pushing blk -> m ())
  -> LedgerDbCfg l
  -> [Ap m l blk c] -> KeySetsReader m l -> LedgerDB l -> m (LedgerDB l)
pushMany trace cfg aps ksReader initDb = (repeatedlyM pushAndTrace) aps initDb
  where
    pushAndTrace ap db = do
      let pushing = Pushing . toRealPoint $ ap
      trace pushing
      push cfg ap ksReader db

-- | Switch to a fork
switch :: (ApplyBlock l blk, Monad m, StandardHash l, c)
       => LedgerDbCfg l
       -> Word64          -- ^ How many blocks to roll back
       -> (UpdateLedgerDbTraceEvent blk -> m ())
       -> [Ap m l blk c]  -- ^ New blocks to apply
       -> KeySetsReader m l
       -> LedgerDB l
       -> m (Either ExceededRollback (LedgerDB l))
switch cfg numRollbacks trace newBlocks ksReader db =
  case rollback numRollbacks db of
      Nothing ->
        return $ Left $ ExceededRollback {
            rollbackMaximum   = Query.maxRollback db
          , rollbackRequested = numRollbacks
          }
      Just db' -> case newBlocks of
        [] -> pure $ Right db'
        -- no blocks to apply to ledger state, return current LedgerDB
        (firstBlock:_) -> do
          let start   = PushStart . toRealPoint $ firstBlock
              goal    = PushGoal  . toRealPoint . last $ newBlocks
          Right <$> pushMany (trace . (StartedPushingBlockToTheLedgerDb start goal))
                                     cfg
                                     newBlocks
                                     ksReader
                                     db'

-- | Isolates the prefix of the changelog that should be flushed
flush ::
     (GetTip l, HasLedgerTables l)
  => DbChangelog.FlushPolicy -> LedgerDB l -> (DbChangelog l, LedgerDB l)
flush policy db = do
    (l, db { ledgerDbChangelog = r })
  where
    (l, r) = DbChangelog.flush policy (ledgerDbChangelog db)

{-------------------------------------------------------------------------------
  Trace events
-------------------------------------------------------------------------------}

newtype PushStart blk = PushStart { unPushStart :: RealPoint blk }
  deriving (Show, Eq)

newtype PushGoal blk = PushGoal { unPushGoal :: RealPoint blk }
  deriving (Show, Eq)

newtype Pushing blk = Pushing { unPushing :: RealPoint blk }
  deriving (Show, Eq)

data UpdateLedgerDbTraceEvent blk =
    -- | Event fired when we are about to push a block to the LedgerDB
      StartedPushingBlockToTheLedgerDb
        !(PushStart blk)
        -- ^ Point from which we started pushing new blocks
        (PushGoal blk)
        -- ^ Point to which we are updating the ledger, the last event
        -- StartedPushingBlockToTheLedgerDb will have Pushing and PushGoal
        -- wrapping over the same RealPoint
        !(Pushing blk)
        -- ^ Point which block we are about to push
  deriving (Show, Eq, Generic)

{-------------------------------------------------------------------------------
  Support for testing
-------------------------------------------------------------------------------}

pureBlock :: blk -> Ap m l blk ()
pureBlock = ReapplyVal

push' :: (ApplyBlock l blk, StandardHash l)
      => LedgerDbCfg l -> blk -> KeySetsReader Identity l -> LedgerDB l -> LedgerDB l
push' cfg b bk = runIdentity . push cfg (pureBlock b) bk

pushMany' :: (ApplyBlock l blk, StandardHash l)
          => LedgerDbCfg l -> [blk] -> KeySetsReader Identity l -> LedgerDB l -> LedgerDB l
pushMany' cfg bs bk =
  runIdentity . pushMany (const $ pure ()) cfg (map pureBlock bs) bk

switch' :: (ApplyBlock l blk, StandardHash l)
        => LedgerDbCfg l
        -> Word64 -> [blk] -> KeySetsReader Identity l -> LedgerDB l -> Maybe (LedgerDB l)
switch' cfg n bs bk db =
    case runIdentity $ switch cfg n (const $ pure ()) (map pureBlock bs) bk db of
      Left  ExceededRollback{} -> Nothing
      Right db'                -> Just db'

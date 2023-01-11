{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

-- TODO stylish haskell insists on deleting this pragma if placed in the group above.
{-# LANGUAGE DerivingStrategies         #-}

-- TODO: we agreed on removing the ledgerDb prefix from the operations in this
-- module. We can import the module qualified if we need to dissambiguate.
module Ouroboros.Consensus.Storage.LedgerDB.InMemory (
    -- * LedgerDB proper
    LedgerDbCfg (..)
  , ledgerDbWithAnchor
    -- ** opaque
  , LedgerDB
    -- * Ledger DB types (TODO: we might want to place this somewhere else)
  , DbChangelog
  , DbReader (..)
  , KeySetsReader
  , ReadsKeySets (..)
  , RewoundTableKeySets (..)
  , UnforwardedReadSets (..)
  , defaultReadKeySets
  , forwardTableKeySets
  , forwardTableKeySets'
  , rewindTableKeySets
    -- ** Serialisation
  , decodeSnapshotBackwardsCompatible
  , encodeSnapshot
    -- ** Queries
  , getLedgerTablesFor
  , ledgerDbAnchor
  , ledgerDbChangelog
  , ledgerDbCurrent
  , ledgerDbFlush
  , ledgerDbLastFlushedState
  , ledgerDbPast
  , ledgerDbPrefix
  , ledgerDbPrune
  , ledgerDbSnapshots
  , ledgerDbTip
  , volatileStatesBimap
    -- ** Running updates
  , AnnLedgerError (..)
  , Ap (..)
  , ResolveBlock
  , ResolvesBlocks (..)
  , ThrowsLedgerError (..)
  , defaultResolveBlocks
  , defaultResolveWithErrors
  , defaultThrowLedgerErrors
    -- ** Updates
  , ExceededRollback (..)
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
import qualified Codec.Serialise.Decoding as Dec
import           Codec.Serialise.Encoding (Encoding)
import           Control.Monad.Except hiding (ap)
import           Control.Monad.Reader hiding (ap)
import           Data.Functor.Identity
import           Data.Kind (Constraint, Type)
import           Data.Word
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)
import           NoThunks.Class (NoThunks)

import           Ouroboros.Network.AnchoredSeq (Anchorable (..),
                     AnchoredSeq (..))
import qualified Ouroboros.Network.AnchoredSeq as AS

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import qualified Ouroboros.Consensus.Ledger.Extended as Extended
import           Ouroboros.Consensus.Ledger.Tables
import           Ouroboros.Consensus.Storage.LedgerDB.HD.DiffSeq
import           Ouroboros.Consensus.Storage.LedgerDB.Types (PushGoal (..),
                     PushStart (..), Pushing (..),
                     UpdateLedgerDbTraceEvent (..))
import           Ouroboros.Consensus.Util
import           Ouroboros.Consensus.Util.CBOR (decodeWithOrigin)
import           Ouroboros.Consensus.Util.Versioned

{-------------------------------------------------------------------------------
  Ledger DB types
-------------------------------------------------------------------------------}

-- | Type alias for the new-style database
--
-- This type is a 'DbChangelog'. We illustrate its contents below, where @k = 3@
-- (for a state @Li@, the corresponding set of differences is @Di@):
--
-- > stateAnchor | diskAnchor | states                     | tableDiffs
-- > --------------------------------------------------------------------------
-- >      0      |      0     | [ L0 ]                     | [ ]
-- >      0      |      0     | [ L0, L1 ]                 | [ D1 ]
-- >      0      |      0     | [ L0, L1, L2 ]             | [ D1, D2 ]
-- >      0      |      0     | [ L0, L1, L2, L3 ]         | [ D1, D2, D3 ]
-- >      1      |      0     | [ L0, L1, L2, L3, L4 ]     | [ D1, D2, D3, D4 ]
-- >      2      |      0     | [ L0, L1, L2, L3, L4, L5 ] | [ D1, D2, D3, D4, D5 ]    (*)
-- >      2      |      2     | [ L2, L3, L4, L5 ]         | [ D3, D4, D5 ]   -- flush (**)
-- >      3      |      2     | [ L2, L3, L4, L5, L6 ]     | [ D3, D4, D5, D6 ]
--
-- The disk anchor moves when we flush data to disk, and the state anchor points
-- always to the state that represents the tip of the logical immutable
-- database. Notice that @seqNo (last states) - stateAnchor@ is usually @k@
-- except when rollbacks or data corruption take place and will be less than @k@
-- when we just loaded a snapshot. We cannot roll back more than @k@ blocks.
-- This means that after a rollback of @k@ blocks at (*), the database will look
-- something like this:
--
-- >      2      |      0     | [ L0, L1, L2 ]             | [ D1, D2 ]
--
-- And a rollback of @k@ blocks at (**) will look something like this:
--
-- >      2      |      0     | [ L2 ]                     | [ ]
--
-- Notice how the states list always contains the in-memory state of the anchor,
-- but the table differences might not contain the differences for that anchor
-- if they have been flushed to the backend.
--
-- The new-style database has to be coupled with the @BackingStore@ which
-- provides the pointers to the on-disk data.
newtype LedgerDB (l :: LedgerStateKind) = LedgerDB { ledgerDbChangelog :: DbChangelog l }
  deriving (Generic)

deriving newtype instance Eq       (DbChangelog l) => Eq       (LedgerDB l)
deriving newtype instance NoThunks (DbChangelog l) => NoThunks (LedgerDB l)

deriving instance (ShowLedgerState l, ShowLedgerState (LedgerTables l)) => Show (LedgerDB l)

{-------------------------------------------------------------------------------
  LedgerDB proper
-------------------------------------------------------------------------------}

-- | Ledger DB starting at the specified ledger state
ledgerDbWithAnchor ::
     ( TableStuff l
     , GetTip (l EmptyMK)
     )
  => l EmptyMK -> LedgerDB l
ledgerDbWithAnchor = LedgerDB . emptyDbChangeLog

{-------------------------------------------------------------------------------
  Resolve a block
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
  resolveBlock :: ResolveBlock m blk

instance Monad m => ResolvesBlocks (ReaderT (ResolveBlock m blk) m) blk where
  resolveBlock r = ReaderT $ \f -> f r

instance (Monad m, ResolvesBlocks m blk) => ResolvesBlocks (ExceptT e m) blk where
  resolveBlock = lift . resolveBlock

defaultResolveBlocks :: ResolveBlock m blk
                     -> ReaderT (ResolveBlock m blk) m a
                     -> m a
defaultResolveBlocks = flip runReaderT

{-------------------------------------------------------------------------------
  Throw a ledger error
-------------------------------------------------------------------------------}

-- | Annotated ledger errors
data AnnLedgerError (l :: LedgerStateKind) blk = AnnLedgerError {
      -- | The ledger DB just /before/ this block was applied
      annLedgerState  :: LedgerDB l

      -- | Reference to the block that had the error
    , annLedgerErrRef :: RealPoint blk

      -- | The ledger error itself
    , annLedgerErr    :: LedgerErr l
    }

-- | Monads in which we can throw ledger errors
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
  Apply blocks on top of a LedgerDB
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
data Ap :: (Type -> Type) -> LedgerStateKind -> Type -> Constraint -> Type where
  ReapplyVal ::           blk -> Ap m l blk ( ReadsKeySets m l )
  ApplyVal   ::           blk -> Ap m l blk ( ReadsKeySets m l
                                            , ThrowsLedgerError m l blk )
  ReapplyRef :: RealPoint blk -> Ap m l blk ( ResolvesBlocks m blk
                                            , ReadsKeySets m l
                                            )
  ApplyRef   :: RealPoint blk -> Ap m l blk ( ResolvesBlocks m blk
                                            , ThrowsLedgerError m l blk
                                            , ReadsKeySets m l
                                            )

  -- | 'Weaken' increases the constraint on the monad @m@.
  --
  -- This is primarily useful when combining multiple 'Ap's in a single
  -- homogeneous structure.
  Weaken :: (c' => c) => Ap m l blk c -> Ap m l blk c'

toRealPoint :: HasHeader blk => Ap m l blk c -> RealPoint blk
toRealPoint (ReapplyVal blk) = blockRealPoint blk
toRealPoint (ApplyVal blk)   = blockRealPoint blk
toRealPoint (ReapplyRef rp)  = rp
toRealPoint (ApplyRef rp)    = rp
toRealPoint (Weaken ap)      = toRealPoint ap

-- | Apply block to the current ledger state
--
-- We take in the entire 'LedgerDB' because we record that as part of errors.
applyBlock :: forall m c l blk
            . (ApplyBlock l blk, TickedTableStuff l, Monad m, c, HasCallStack)
           => LedgerCfg l
           -> Ap m l blk c
           -> LedgerDB l
           -> m (l DiffMK)
applyBlock cfg ap db = case ap of
  ReapplyVal b -> withBlockReadSets b $ \lh ->
    return $ tickThenReapply cfg b lh

  ApplyVal b -> withBlockReadSets b $ \lh ->
      either (throwLedgerError db (blockRealPoint b)) return
      $ runExcept
      $ tickThenApply cfg b lh

  ReapplyRef r  -> do
    b <- resolveBlock r -- TODO: ask: would it make sense to recursively call applyBlock using ReapplyVal?

    withBlockReadSets b $ \lh ->
      return $ tickThenReapply cfg b lh

  ApplyRef r -> do
    b <- resolveBlock r

    withBlockReadSets b $ \lh ->
      either (throwLedgerError db (blockRealPoint b)) return $ runExcept $
      tickThenApply cfg b lh
  Weaken ap' ->
    applyBlock cfg ap' db

  where
    withBlockReadSets
      :: ReadsKeySets m l
      => blk
      -> (l ValuesMK -> m (l DiffMK))
      -> m (l DiffMK)
    withBlockReadSets b f = do
      let ks = getBlockKeySets b :: LedgerTables l KeysMK
      let aks = rewindTableKeySets (ledgerDbChangelog db) ks :: RewoundTableKeySets l
      urs <- readDb aks
      case withHydratedLedgerState urs f of
        Left err ->
          -- We performed the rewind;read;forward sequence in this function. So
          -- the forward operation should not fail. If this is the case we're in
          -- the presence of a problem that we cannot deal with at this level,
          -- so we throw an error.
          --
          -- When we introduce pipelining, if the forward operation fails it
          -- could be because the DB handle was modified by a DB flush that took
          -- place when __after__ we read the unforwarded keys-set from disk.
          -- However, performing rewind;read;forward with the same __locked__
          -- changelog should always succeed.
          error $ "Changelog rewind;read;forward sequence failed, " <> show err
        Right res -> res

    withHydratedLedgerState
      :: UnforwardedReadSets l
      -> (l ValuesMK -> a)
      -> Either (WithOrigin SlotNo, WithOrigin SlotNo) a
    withHydratedLedgerState urs f =
          f
      .   withLedgerTables (ledgerDbCurrent db)
      <$> forwardTableKeySets (ledgerDbChangelog db) urs

-- | Read and forward the values up to the tip of the given ledger db. Returns
-- Left if the anchor moved.
getLedgerTablesFor ::
     (Monad m, ReadsKeySets m l, TableStuff l)
  => LedgerDB l
  -> LedgerTables l KeysMK
  -> m (Either (WithOrigin SlotNo, WithOrigin SlotNo) (LedgerTables l ValuesMK))
getLedgerTablesFor db keys = do
  let aks = rewindTableKeySets (ledgerDbChangelog db) keys
  urs <- readDb aks
  pure $ forwardTableKeySets (ledgerDbChangelog db) urs

{-------------------------------------------------------------------------------
  HD Interface that I need (Could be moved to  Ouroboros.Consensus.Ledger.Basics )
-------------------------------------------------------------------------------}

data RewoundTableKeySets l =
    RewoundTableKeySets
      !(WithOrigin SlotNo)   -- ^ the slot to which the keys were rewounded
      !(LedgerTables l KeysMK)

rewindTableKeySets ::
     DbChangelog l -> LedgerTables l KeysMK -> RewoundTableKeySets l
rewindTableKeySets dblog =
      RewoundTableKeySets
        (changelogDiffAnchor dblog)

data UnforwardedReadSets l = UnforwardedReadSets {
    -- | The Slot number of the anchor of the 'DbChangelog' that was used when
    -- rewinding and reading.
    ursSeqNo  :: !(WithOrigin SlotNo)
    -- | The values that were found in the 'BackingStore'.
  , ursValues :: !(LedgerTables l ValuesMK)
    -- | All the requested keys, being or not present in the 'BackingStore'.
  , ursKeys   :: !(LedgerTables l KeysMK)
  }

forwardTableKeySets' ::
     TableStuff l
  => WithOrigin SlotNo
  -> LedgerTables l SeqDiffMK
  -> UnforwardedReadSets l
  -> Either (WithOrigin SlotNo, WithOrigin SlotNo)
            (LedgerTables l ValuesMK)
forwardTableKeySets' seqNo chdiffs = \(UnforwardedReadSets seqNo' values keys) ->
    if seqNo /= seqNo' then Left (seqNo, seqNo') else
    Right
      $ zipLedgerTables2 forward values keys chdiffs
  where
    forward ::
         (Ord k, Eq v)
      => ApplyMapKind ValuesMK  k v
      -> ApplyMapKind KeysMK    k v
      -> ApplyMapKind SeqDiffMK k v
      -> ApplyMapKind ValuesMK  k v
    forward (ApplyValuesMK values) (ApplyKeysMK keys) (ApplySeqDiffMK diffs) =
      ApplyValuesMK $ applyDiffForKeys values keys (cumulativeDiff diffs)

forwardTableKeySets ::
     TableStuff l
  => DbChangelog l
  -> UnforwardedReadSets l
  -> Either (WithOrigin SlotNo, WithOrigin SlotNo)
            (LedgerTables l ValuesMK)
forwardTableKeySets dblog =
  forwardTableKeySets' (changelogDiffAnchor dblog) (changelogDiffs dblog)

-- | Isolates the prefix of the changelog that should be flushed
--
-- TODO take some argument to bound the size of the resulting prefix?
ledgerDbFlush ::
     (GetTip (l EmptyMK), TableStuff l)
  => DbChangelogFlushPolicy -> LedgerDB l -> (DbChangelog l, LedgerDB l)
ledgerDbFlush policy db = do
    (l, db { ledgerDbChangelog = r })
  where
    (l, r) = flushDbChangelog policy (ledgerDbChangelog db)

class ReadsKeySets m l where
  readDb :: KeySetsReader m l

type KeySetsReader m l = RewoundTableKeySets l -> m (UnforwardedReadSets l)

newtype DbReader m l a = DbReader { runDbReader :: ReaderT (KeySetsReader m l) m a}
  deriving newtype (Functor, Applicative, Monad)

instance ReadsKeySets (DbReader m l) l where
  readDb rks = DbReader $ ReaderT $ \f -> f rks

-- TODO: this is leaking details on how we want to compose monads at the higher levels.
instance (Monad m, ReadsKeySets m l) => ReadsKeySets (ReaderT r m) l where
  readDb = lift . readDb

instance (Monad m, ReadsKeySets m l) => ReadsKeySets (ExceptT e m) l where
  readDb = lift . readDb

instance
     ReadsKeySets Identity (LedgerState blk)
  => ReadsKeySets Identity (Extended.ExtLedgerState blk) where
  readDb (RewoundTableKeySets seqNo (Extended.ExtLedgerStateTables rew)) = do
      UnforwardedReadSets seqNo' values keys <- readDb (RewoundTableKeySets seqNo rew)
      pure $ UnforwardedReadSets seqNo' (Extended.ExtLedgerStateTables values) (Extended.ExtLedgerStateTables keys)

defaultReadKeySets :: KeySetsReader m l -> DbReader m l a -> m a
defaultReadKeySets f dbReader = runReaderT (runDbReader dbReader) f

{-------------------------------------------------------------------------------
  Queries
-------------------------------------------------------------------------------}

-- | The ledger state at the tip of the chain
ledgerDbCurrent :: GetTip (l EmptyMK) => LedgerDB l -> l EmptyMK
ledgerDbCurrent =
    either unDbChangelogState unDbChangelogState
  . AS.head
  . changelogVolatileStates
  . ledgerDbChangelog

-- | Information about the state of the ledger at the anchor
ledgerDbAnchor :: LedgerDB l -> l EmptyMK
ledgerDbAnchor =
    unDbChangelogState
  . AS.anchor
  . changelogVolatileStates
  . ledgerDbChangelog

-- | Get the most recently flushed ledger state
--
-- TODO: the comment below might belong somewhere else.
--
-- This is what will be serialized when snapshotting.
--
-- When we take a snapshot, we do it for both the modern and the legacy ledger.
-- Conversely, when we read a snapshot, we assume the snapshot has what we need
-- for both the legacy and modern ledger. So the read/written modern and legacy
-- ledgers must always be coherent pairs, i.e. they're the ledger state as of
-- the same block.
--
-- The legacy snapshot is always written from the immutable tip. If we flush
-- first, then the modern snapshot also is. If we don't flush first, then the
-- modern snapshot might be written from something older than the immutable tip.
-- Therefore we guard this condition with an 'assert' and this imposes the
-- following PRECONDITION:
--
-- PRECONDITION: if you are running the legacy ledger, then you must flush
-- before calling this function
ledgerDbLastFlushedState :: LedgerDB l -> l EmptyMK
ledgerDbLastFlushedState =
    unDbChangelogState
  . AS.anchor
  . changelogImmutableStates
  . ledgerDbChangelog

-- | All snapshots currently stored by the ledger DB (new to old)
--
-- This also includes the snapshot at the anchor. For each snapshot we also
-- return the distance from the tip.
--
-- TODO: this name is misleading. Probably deserves a renaming.
ledgerDbSnapshots :: LedgerDB l -> [(Word64, l EmptyMK)]
ledgerDbSnapshots =
      zip [0..]
    . map unDbChangelogState
    . AS.toNewestFirst
    . changelogVolatileStates
    . ledgerDbChangelog

-- | How many blocks can we currently roll back?
ledgerDbMaxRollback :: GetTip (l EmptyMK) => LedgerDB l -> Word64
ledgerDbMaxRollback =
    fromIntegral
  . AS.length
  . changelogVolatileStates
  . ledgerDbChangelog

-- | Reference to the block at the tip of the chain
ledgerDbTip :: IsLedger l => LedgerDB l -> Point (l EmptyMK)
ledgerDbTip = getTip . ledgerDbCurrent

-- | Have we seen at least @k@ blocks?
ledgerDbIsSaturated :: (forall mk. GetTip (l mk)) => SecurityParam -> LedgerDB l -> Bool
ledgerDbIsSaturated (SecurityParam k) db =
    ledgerDbMaxRollback db >= k

-- | Get a past ledger state
--
--  \( O(\log(\min(i,n-i)) \)
--
-- When no ledger state (or anchor) has the given 'Point', 'Nothing' is
-- returned.
ledgerDbPast ::
     ( HasHeader blk, IsLedger l, HeaderHash l ~ HeaderHash blk
     , TableStuff l, StandardHash (l EmptyMK)
     )
  => Point blk
  -> LedgerDB l
  -> Maybe (l EmptyMK)
ledgerDbPast pt db = ledgerDbCurrent <$> ledgerDbPrefix pt db

-- | Get a prefix of the LedgerDB that ends at the given point
--
--  \( O(\log(\min(i,n-i)) \)
--
-- When no ledger state (or anchor) has the given 'Point', 'Nothing' is
-- returned.
ledgerDbPrefix ::
     ( HasHeader blk, IsLedger l, HeaderHash l ~ HeaderHash blk
     , TableStuff l, StandardHash (l EmptyMK)
     )
  => Point blk
  -> LedgerDB l
  -> Maybe (LedgerDB l)
ledgerDbPrefix pt db
    | pt == castPoint (getTip (ledgerDbAnchor db))
    = Just . LedgerDB . prefixBackToAnchorDbChangelog $ ledgerDbChangelog db
    | otherwise
    = LedgerDB <$> prefixDbChangelog (castPoint pt) (ledgerDbChangelog db)

-- | Transform the underlying 'AnchoredSeq' using the given functions.
volatileStatesBimap ::
     Anchorable (WithOrigin SlotNo) a b
  => (l EmptyMK -> a)
  -> (l EmptyMK -> b)
  -> LedgerDB l
  -> AnchoredSeq (WithOrigin SlotNo) a b
volatileStatesBimap f g =
      AS.bimap (f . unDbChangelogState) (g . unDbChangelogState)
    . changelogVolatileStates
    . ledgerDbChangelog

-- | Prune snapshots until at we have at most @k@ snapshots in the LedgerDB,
-- excluding the snapshots stored at the anchor.
ledgerDbPrune ::
     GetTip (l EmptyMK)
  => SecurityParam -> LedgerDB l -> LedgerDB l
ledgerDbPrune k db = db {
      ledgerDbChangelog   = pruneVolatilePartDbChangelog k (ledgerDbChangelog db)
    }

 -- NOTE: we must inline 'ledgerDbPrune' otherwise we get unexplained thunks in
 -- 'LedgerDB' and thus a space leak. Alternatively, we could disable the
 -- @-fstrictness@ optimisation (enabled by default for -O1). See #2532.
{-# INLINE ledgerDbPrune #-}

{-------------------------------------------------------------------------------
  Internal updates
-------------------------------------------------------------------------------}

-- | Push an updated ledger state
pushLedgerState ::
     (IsLedger l, TickedTableStuff l)
  => SecurityParam
  -> l DiffMK -- ^ Updated ledger state
  -> LedgerDB l -> LedgerDB l
pushLedgerState secParam currentNew' db@LedgerDB{..}  =
    ledgerDbPrune secParam $ db {
        ledgerDbChangelog   =
          extendDbChangelog
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
     (GetTip (l EmptyMK), TableStuff l)
  => Word64
  -> LedgerDB l
  -> Maybe (LedgerDB l)
rollback n db@LedgerDB{..}
    | n <= ledgerDbMaxRollback db
    = Just db {
          ledgerDbChangelog   = rollbackDbChangelog (fromIntegral n)     ledgerDbChangelog
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

ledgerDbPush :: forall m c l blk
              . (ApplyBlock l blk, TickedTableStuff l, Monad m, c, HasCallStack)
             => LedgerDbCfg l
             -> Ap m l blk c -> LedgerDB l -> m (LedgerDB l)
ledgerDbPush cfg ap db =
    (\current' -> pushLedgerState (ledgerDbCfgSecParam cfg) current' db) <$>
      applyBlock (ledgerDbCfg cfg) ap db

-- | Push a bunch of blocks (oldest first)
ledgerDbPushMany ::
     forall m c l blk . (ApplyBlock l blk, TickedTableStuff l, Monad m, c, HasCallStack)
  => (Pushing blk -> m ())
  -> LedgerDbCfg l
  -> [Ap m l blk c] -> LedgerDB l -> m (LedgerDB l)
ledgerDbPushMany trace cfg = repeatedlyM pushAndTrace
  where
    pushAndTrace ap db = do
      let pushing = Pushing . toRealPoint $ ap
      trace pushing
      ledgerDbPush cfg ap db

-- | Switch to a fork
ledgerDbSwitch :: (ApplyBlock l blk, TickedTableStuff l, Monad m, c, HasCallStack)
               => LedgerDbCfg l
               -> Word64          -- ^ How many blocks to roll back
               -> (UpdateLedgerDbTraceEvent blk -> m ())
               -> [Ap m l blk c]  -- ^ New blocks to apply
               -> LedgerDB l
               -> m (Either ExceededRollback (LedgerDB l))
ledgerDbSwitch cfg numRollbacks trace newBlocks db =
    case rollback numRollbacks db of
      Nothing ->
        return $ Left $ ExceededRollback {
            rollbackMaximum   = ledgerDbMaxRollback db
          , rollbackRequested = numRollbacks
          }
      Just db' -> case newBlocks of
        [] -> pure $ Right db'
        -- no blocks to apply to ledger state, return current LedgerDB
        (firstBlock:_) -> do
          let start   = PushStart . toRealPoint $ firstBlock
              goal    = PushGoal  . toRealPoint . last $ newBlocks
          Right <$> ledgerDbPushMany (trace . StartedPushingBlockToTheLedgerDb start goal)
                                     cfg
                                     newBlocks
                                     db'

{-------------------------------------------------------------------------------
  LedgerDB Config
-------------------------------------------------------------------------------}

data LedgerDbCfg l = LedgerDbCfg {
      ledgerDbCfgSecParam :: !SecurityParam
    , ledgerDbCfg         :: !(LedgerCfg l)

      -- TODO: we might need to add support for specifying the flushing rate and
      -- policy in this datatype
    }
  deriving (Generic)

deriving instance NoThunks (LedgerCfg l) => NoThunks (LedgerDbCfg l)

type instance HeaderHash (LedgerDB l) = HeaderHash l

instance IsLedger l => GetTip (LedgerDB l) where
  getTip = castPoint . getTip . ledgerDbCurrent

{-------------------------------------------------------------------------------
  Support for testing
-------------------------------------------------------------------------------}

pureBlock :: blk -> Ap m l blk (ReadsKeySets m l)
pureBlock = ReapplyVal

ledgerDbPush' :: (ApplyBlock l blk, TickedTableStuff l, ReadsKeySets Identity l, HasCallStack)
              => LedgerDbCfg l -> blk -> LedgerDB l -> LedgerDB l
ledgerDbPush' cfg b = runIdentity . ledgerDbPush cfg (pureBlock b)

ledgerDbPushMany' :: (ApplyBlock l blk, TickedTableStuff l, ReadsKeySets Identity l, HasCallStack)
                  => LedgerDbCfg l -> [blk] -> LedgerDB l -> LedgerDB l
ledgerDbPushMany' cfg bs =
  runIdentity . ledgerDbPushMany (const $ pure ()) cfg (map pureBlock bs)

ledgerDbSwitch' :: forall l blk
                 . (ApplyBlock l blk, TickedTableStuff l, ReadsKeySets Identity l, HasCallStack)
                => LedgerDbCfg l
                -> Word64 -> [blk] -> LedgerDB l -> Maybe (LedgerDB l)
ledgerDbSwitch' cfg n bs db =
    case runIdentity $ ledgerDbSwitch cfg n (const $ pure ()) (map pureBlock bs) db of
      Left  ExceededRollback{} -> Nothing
      Right db'                -> Just db'

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

-- | Version 1: uses versioning ('Ouroboros.Consensus.Util.Versioned') and only
-- encodes the ledger state @l@.
snapshotEncodingVersion1 :: VersionNumber
snapshotEncodingVersion1 = 1

-- | Encoder to be used in combination with 'decodeSnapshotBackwardsCompatible'.
encodeSnapshot :: (l -> Encoding) -> l -> Encoding
encodeSnapshot encodeLedger l =
    encodeVersion snapshotEncodingVersion1 (encodeLedger l)

-- | To remain backwards compatible with existing snapshots stored on disk, we
-- must accept the old format as well as the new format.
--
-- The old format:
-- * The tip: @WithOrigin (RealPoint blk)@
-- * The chain length: @Word64@
-- * The ledger state: @l@
--
-- The new format is described by 'snapshotEncodingVersion1'.
--
-- This decoder will accept and ignore them. The encoder ('encodeSnapshot') will
-- no longer encode them.
decodeSnapshotBackwardsCompatible ::
     forall l blk.
     Proxy blk
  -> (forall s. Decoder s l)
  -> (forall s. Decoder s (HeaderHash blk))
  -> forall s. Decoder s l
decodeSnapshotBackwardsCompatible _ decodeLedger decodeHash =
    decodeVersionWithHook
      decodeOldFormat
      [(snapshotEncodingVersion1, Decode decodeVersion1)]
  where
    decodeVersion1 :: forall s. Decoder s l
    decodeVersion1 = decodeLedger

    decodeOldFormat :: Maybe Int -> forall s. Decoder s l
    decodeOldFormat (Just 3) = do
        _ <- withOriginRealPointToPoint <$>
               decodeWithOrigin (decodeRealPoint @blk decodeHash)
        _ <- Dec.decodeWord64
        decodeLedger
    decodeOldFormat mbListLen =
        fail $
          "decodeSnapshotBackwardsCompatible: invalid start " <>
          show mbListLen
